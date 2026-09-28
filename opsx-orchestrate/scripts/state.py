#!/usr/bin/env python3
"""opsx-orchestrate state tool.

The orchestrator's `.orchestrator/state.json` is written only through this script.
It owns the schema (see ../state.md), the task status transitions, the timestamps,
the derived counts, the gate evaluation, the handshake assembly and the cycle
archive. The LLM supplies judgment as short arguments; everything mechanical is here.

Every verb prints compact JSON on stdout. A refusal prints
{"ok": false, "error": "<slug>", "message": "..."} on stderr and exits 1
(exit 2 when no state file exists). Writes are load -> validate -> mutate ->
validate -> atomic replace, under a file lock.

Usage: python3 state.py [--repo PATH] <verb> ...   (state.py help lists the verbs)
"""

import argparse
import fcntl
import glob
import json
import os
import re
import shutil
import sys
import time
from datetime import datetime, timezone

SCHEMA_VERSION = "1.2"
TEXT_CAP = 280
LONG_TEXT_CAP = 600
LONG_TEXT_FIELDS = {"question", "decision_readback"}

PHASES = ["plan", "execute", "integrate"]

TASK_STATUSES = ["ready", "setup_complete", "in_progress", "completed", "needs_review",
                 "reviewed", "done", "failed", "blocked", "externalised"]
# Forward order of the main line; side states handled in TRANSITIONS.
MAIN_LINE = ["ready", "setup_complete", "in_progress", "completed", "needs_review", "reviewed", "done"]
SIDE_STATES = {"failed", "blocked", "externalised"}
# Explicit non-forward moves allowed by state.md.
BACKWARD_ALLOWED = {
    ("failed", "ready"), ("blocked", "ready"),
    ("needs_review", "in_progress"), ("reviewed", "in_progress"),
}
STOPPED = {"done", "failed", "externalised", "blocked"}
ORPHAN = {"setup_complete", "in_progress", "completed", "needs_review", "reviewed"}

TASK_CLASSES = ["feature", "test", "doc", "refactor", "bug", "contract", "infrastructure"]

TASK_FIELDS = {
    "task_name": str, "task_file": str, "task_class": str, "on_critical_path": bool,
    "worktree_path": (str, type(None)), "branch_name": (str, type(None)),
    "agent_task_id": (str, type(None)), "status": str, "merge_commit": (str, type(None)),
    "after_snapshot": (str, type(None)),
    "regression_detected": bool, "worktree_removed": bool, "review_mode": (str, type(None)),
    "findings_path": (str, type(None)), "findings_count": (int, type(None)),
    "followups_created": list, "dependents_repointed": list,
    "implementor_report_path": (str, type(None)),
    "discovered_from": (str, type(None)), "discovered_by": (str, type(None)),
    "discovered_class": (str, type(None)),
    "cites_seams": list, "blocked_by": list, "blocker_note": (str, type(None)),
    "rejections": int,
    "started_at": (str, type(None)), "completed_at": (str, type(None)), "reviewed_at": (str, type(None)),
}
TASK_DEFAULTS = {
    "on_critical_path": False, "worktree_path": None, "branch_name": None, "agent_task_id": None,
    "status": "ready", "merge_commit": None, "after_snapshot": None, "regression_detected": False,
    "worktree_removed": False,
    "review_mode": None, "findings_path": None, "findings_count": None, "followups_created": [],
    "dependents_repointed": [], "implementor_report_path": None, "discovered_from": None,
    "discovered_by": None, "discovered_class": None,
    "cites_seams": [], "blocked_by": [], "blocker_note": None, "rejections": 0,
    "started_at": None, "completed_at": None, "reviewed_at": None,
}

FINDING_FIELDS = {
    "finding_id": str, "trigger": str, "severity": str, "class": (str, type(None)), "title": str,
    "path": str, "locations": list, "why_tests_missed": (str, type(None)),
    "discovered_from": (str, type(None)), "seam": (str, type(None)), "observed": (dict, type(None)),
    "resolution": str, "blocking_merge_until_resolved": bool,
}
FINDING_DEFAULTS = {"class": None, "locations": [], "why_tests_missed": None, "discovered_from": None,
                    "seam": None, "observed": None, "resolution": "pending", "blocking_merge_until_resolved": False}
OBSERVED_REQUIRED_SEVERITIES = {"blocking", "advisory"}
TRIGGERS = ["premise-check", "conformance"]
SEVERITIES = ["blocking", "advisory", "informational", "spec-signal"]
FINDING_CLASSES = ["shape-fragmentation", "vocabulary-mismatch", "responsibility-leakage",
                   "dead-branch", "interface-drift", "mutation", "invariant-gap", "duplication"]

ASK_FIELDS = {
    "id": str, "kind": str, "raised_by": dict, "question": str, "about": (str, type(None)),
    "observed": (dict, type(None)), "options": list, "recommendation": (dict, type(None)),
    "default_if_unanswered": (str, type(None)), "blocks": list, "status": str,
    "decision": (str, type(None)), "decision_readback": (str, type(None)),
    "applied_via": (str, type(None)), "revised_from": (str, type(None)),
}
ASK_DEFAULTS = {"about": None, "observed": None, "options": [], "recommendation": None,
                "default_if_unanswered": None, "blocks": [], "status": "open", "decision": None,
                "decision_readback": None, "applied_via": None, "revised_from": None}
ASK_KINDS = ["decision", "doc-correction", "process", "environment", "confirmation"]
ASK_STATUSES = ["open", "answered", "applied", "deferred", "declined", "superseded"]
GOAL_DRIFT_OPTIONS = {"revise", "split", "abandon", "continue"}

JOURNAL_KINDS = ["discovery", "decision", "inline-fix", "deviation", "push-back", "note"]

GATE_CHECKS = {
    "plan": ["prior_integrate_consumed", "batch_composed", "briefs_cite_seams",
             "user_signed_off_goal_drift"],
    "execute": ["all_tasks_executed_or_stopped", "all_reviews_completed", "no_orphan_in_progress"],
    "integrate": ["blocking_findings_resolved", "pm_digest_produced", "user_asks_routed",
                  "open_tasks_refined_against_handshake", "handshake_artifact_written"],
}
# Checks the tool cannot compute; the orchestrator asserts them with `gate set`.
ASSERTED_CHECKS = {"batch_composed", "open_tasks_refined_against_handshake"}

HANDSHAKE_REQUIRED = ["design_diff", "pm_digest_path", "meta_discoveries",
                      "user_resolved_goal_drift", "asks_for_user_open", "asks_for_user_resolved",
                      "task_refinements"]
# design_diff element: {"seam": "<seam id>", "change": added|amended|retired, "ref": "<finding id>"}
DESIGN_DIFF_CHANGES = ["added", "amended", "retired"]

TOP_LEVEL = {
    "schema_version": str, "session_id": str, "cycle_id": str, "phase": str, "repo_root": str,
    "change_name": str, "baseline_snapshot": (str, type(None)), "baseline_status": (int, type(None)),
    "current_branch": str, "test_command": str, "history_window": int,
    "prior_handshake": (str, type(None)), "closed_at": (str, type(None)),
    "tasks": list, "architect_findings": list, "asks_for_user": list,
    "journal": list, "cycle_log": dict, "phase_gates": dict,
}
TOP_LEVEL_SETTABLE = {"baseline_snapshot", "baseline_status", "current_branch", "test_command"}
LEGACY_ALIASES = {"prior_handshake_path": "prior_handshake"}


class Refusal(Exception):
    def __init__(self, slug, message, exit_code=1, **extra):
        super().__init__(message)
        self.slug, self.message, self.exit_code, self.extra = slug, message, exit_code, extra


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


# ---------------------------------------------------------------- location

def find_repo_root(explicit=None):
    if explicit:
        return os.path.abspath(explicit)
    d = os.getcwd()
    while True:
        if os.path.exists(os.path.join(d, ".claude", "orchestrator", "config.yaml")):
            return d
        if os.path.exists(os.path.join(d, ".orchestrator", "state.json")):
            return d
        parent = os.path.dirname(d)
        if parent == d:
            return os.getcwd()
        d = parent


class Store:
    def __init__(self, repo_root):
        self.repo_root = repo_root
        self.dir = os.path.join(repo_root, ".orchestrator")
        self.path = os.path.join(self.dir, "state.json")
        self.lock_path = os.path.join(self.dir, "state.lock")

    def exists(self):
        return os.path.exists(self.path)

    def load(self):
        if not self.exists():
            raise Refusal("not_initialized", f"no state file at {self.path}; run `init`", exit_code=2)
        with open(self.path) as f:
            state = json.load(f)
        if isinstance(state, dict):  # a field added to a record schema later defaults on read
            for key, _fields, defaults, _id in RECORD_LISTS.values():
                for rec in state.get(key, []) or []:
                    if isinstance(rec, dict):
                        for k, v in defaults.items():
                            rec.setdefault(k, v)
        return state

    def write(self, state):
        os.makedirs(self.dir, exist_ok=True)
        tmp = self.path + ".tmp"
        with open(tmp, "w") as f:
            json.dump(state, f, indent=2)
            f.write("\n")
            f.flush()
            os.fsync(f.fileno())
        os.replace(tmp, self.path)

    def mutate(self, fn):
        """fn(state) -> result. Validates before and after; holds the lock throughout."""
        os.makedirs(self.dir, exist_ok=True)
        with open(self.lock_path, "w") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            try:
                state = self.load()
                require_current_schema(state)
                errors = validate(state)
                if errors:
                    raise Refusal("invalid_state", "state file fails validation; fix it before writing",
                                  errors=errors[:20])
                result = fn(state)
                errors = validate(state)
                if errors:
                    raise Refusal("invalid_mutation", "the requested change violates the schema",
                                  errors=errors[:20])
                self.write(state)
                return result
            finally:
                fcntl.flock(lock, fcntl.LOCK_UN)

    def cycle_dir(self, state):
        return os.path.join(self.dir, "cycles", state["cycle_id"])

    def handshake_path(self, cycle_id):
        return os.path.join(self.dir, f"handshake-{cycle_id}.json")


def require_current_schema(state):
    v = str(state.get("schema_version"))
    if v != SCHEMA_VERSION:
        raise Refusal("schema_version", f"state file is schema {v}, tool writes {SCHEMA_VERSION}; "
                      "run `migrate` (or `validate` to see what it would change)")


# ---------------------------------------------------------------- validation

def _check_type(value, typ):
    if typ is int:
        return isinstance(value, int) and not isinstance(value, bool)
    if isinstance(typ, tuple):
        return any(_check_type(value, t) for t in typ)
    return isinstance(value, typ)


def _tname(typ):
    if isinstance(typ, tuple):
        return " | ".join(_tname(t) for t in typ)
    return {type(None): "null"}.get(typ, typ.__name__)


def _text_caps(obj, where, errors, cap_exempt=()):
    """Every string field is capped; long fields get the larger cap."""
    if isinstance(obj, dict):
        for k, v in obj.items():
            if isinstance(v, str):
                cap = LONG_TEXT_CAP if k in LONG_TEXT_FIELDS else TEXT_CAP
                if k not in cap_exempt and len(v) > cap:
                    errors.append(f"{where}.{k}: {len(v)} chars exceeds the {cap}-char cap; "
                                  "write the text to its file and store the path")
            else:
                _text_caps(v, f"{where}.{k}", errors, cap_exempt)
    elif isinstance(obj, list):
        for i, v in enumerate(obj):
            _text_caps(v, f"{where}[{i}]", errors, cap_exempt)


def _validate_record(rec, fields, where, errors, id_field):
    if not isinstance(rec, dict):
        errors.append(f"{where}: not an object")
        return
    for k in rec:
        if k not in fields:
            errors.append(f"{where}.{k}: unknown field (allowed: {', '.join(fields)})")
    for k, typ in fields.items():
        if k not in rec:
            errors.append(f"{where}.{k}: missing")
        elif not _check_type(rec[k], typ):
            errors.append(f"{where}.{k}: expected {_tname(typ)}, got {type(rec[k]).__name__}")
    _text_caps(rec, where, errors)


def _validate_observed(obs, where, errors):
    """observed = {happened: bool, evidence: str}; None means not filled (legacy or informational)."""
    if obs is None:
        return
    if not isinstance(obs.get("happened"), bool):
        errors.append(f"{where}.happened: expected bool")
    if not isinstance(obs.get("evidence"), str) or not obs["evidence"].strip():
        errors.append(f"{where}.evidence: expected non-empty string")
    for k in obs:
        if k not in ("happened", "evidence"):
            errors.append(f"{where}.{k}: unknown field (allowed: happened, evidence)")


def _enum(value, allowed, where, errors):
    if value is not None and value not in allowed:
        errors.append(f"{where}: {value!r} not one of {allowed}")


def validate(state):
    errors = []
    if not isinstance(state, dict):
        return ["state is not an object"]
    for k in state:
        if k not in TOP_LEVEL:
            errors.append(f"{k}: unknown top-level key; use `note` for narrative, `record add` "
                          "for findings/asks, or a file")
    for k, typ in TOP_LEVEL.items():
        if k not in state:
            errors.append(f"{k}: missing")
        elif not _check_type(state[k], typ):
            errors.append(f"{k}: expected {_tname(typ)}, got {type(state[k]).__name__}")
    if errors:
        return errors
    _enum(state["phase"], PHASES, "phase", errors)
    seen = set()
    for i, t in enumerate(state["tasks"]):
        where = f"tasks[{i}]"
        _validate_record(t, TASK_FIELDS, where, errors, "task_name")
        if isinstance(t, dict):
            _enum(t.get("status"), TASK_STATUSES, f"{where}.status", errors)
            if t.get("status") == "needs_review" and not t.get("merge_commit"):
                errors.append(f"{where}: needs_review requires merge_commit")
            if t.get("task_name") in seen:
                errors.append(f"{where}: duplicate task_name {t.get('task_name')!r}")
            seen.add(t.get("task_name"))
    for i, f in enumerate(state["architect_findings"]):
        where = f"architect_findings[{i}]"
        _validate_record(f, FINDING_FIELDS, where, errors, "finding_id")
        if isinstance(f, dict):
            _enum(f.get("trigger"), TRIGGERS, f"{where}.trigger", errors)
            _enum(f.get("severity"), SEVERITIES, f"{where}.severity", errors)
            _enum(f.get("class"), FINDING_CLASSES, f"{where}.class", errors)
            _validate_observed(f.get("observed"), f"{where}.observed", errors)
    for i, a in enumerate(state["asks_for_user"]):
        where = f"asks_for_user[{i}]"
        _validate_record(a, ASK_FIELDS, where, errors, "id")
        if isinstance(a, dict):
            _enum(a.get("kind"), ASK_KINDS, f"{where}.kind", errors)
            _enum(a.get("status"), ASK_STATUSES, f"{where}.status", errors)
    for i, j in enumerate(state["journal"]):
        where = f"journal[{i}]"
        _validate_record(j, JOURNAL_FIELDS, where, errors, "at")
        if isinstance(j, dict):
            _enum(j.get("kind"), JOURNAL_KINDS, f"{where}.kind", errors)
    cl = state["cycle_log"]
    for k in cl:
        if k not in ("started_at", "phase_started_at", "history"):
            errors.append(f"cycle_log.{k}: unknown key (counts are derived; run `counts`)")
    for k in ("started_at", "phase_started_at", "history"):
        if k not in cl:
            errors.append(f"cycle_log.{k}: missing")
    pg = state["phase_gates"]
    for phase, checks in GATE_CHECKS.items():
        g = pg.get(phase)
        if not isinstance(g, dict) or "passed" not in g or "checks" not in g:
            errors.append(f"phase_gates.{phase}: needs passed and checks")
            continue
        for c in g["checks"]:
            if c not in checks:
                errors.append(f"phase_gates.{phase}.checks.{c}: unknown check")
        for k in g:
            if k not in ("passed", "checks", "passed_at"):
                errors.append(f"phase_gates.{phase}.{k}: unknown key (no prose in gates; use `note`)")
    return errors


JOURNAL_FIELDS = {"at": str, "kind": str, "by": (str, type(None)), "ref": (str, type(None)),
                  "text": str, "path": (str, type(None))}


# ---------------------------------------------------------------- helpers

def find_task(state, name):
    for t in state["tasks"]:
        if t["task_name"] == name:
            return t
    raise Refusal("no_such_task", f"no task named {name!r}",
                  tasks=[t["task_name"] for t in state["tasks"]])


def parse_value(raw):
    """field=value parsing: JSON when it parses, else the bare string."""
    if raw in ("null", "none", "None"):
        return None
    if raw in ("true", "True"):
        return True
    if raw in ("false", "False"):
        return False
    if re.fullmatch(r"-?\d+", raw):
        return int(raw)
    if raw[:1] in "[{\"":
        try:
            return json.loads(raw)
        except json.JSONDecodeError:
            pass
    return raw


def parse_assignments(items):
    out = {}
    for item in items:
        if "=" not in item:
            raise Refusal("bad_assignment", f"expected field=value, got {item!r}")
        k, v = item.split("=", 1)
        out[k] = parse_value(v)
    return out


def load_json_arg(raw):
    """A JSON literal, or @path to a JSON file."""
    if raw.startswith("@"):
        with open(raw[1:]) as f:
            return json.load(f)
    try:
        return json.loads(raw)
    except json.JSONDecodeError as e:
        raise Refusal("bad_json", f"could not parse JSON argument: {e}")


def next_seq(existing_ids, prefix):
    n = 0
    for i in existing_ids:
        m = re.fullmatch(re.escape(prefix) + r"(\d+)", i or "")
        if m:
            n = max(n, int(m.group(1)))
    return n + 1


def add_journal(state, kind, text, by=None, ref=None, path=None):
    entry = {"at": now_iso(), "kind": kind, "by": by, "ref": ref, "text": text, "path": path}
    state["journal"].append(entry)
    return entry


def gates_fresh():
    return {p: {"passed": False, "checks": {c: False for c in cs}} for p, cs in GATE_CHECKS.items()}


# ---------------------------------------------------------------- counts

def derive_counts(tasks):
    def n(pred):
        return sum(1 for t in tasks if pred(t))
    main_index = {s: i for i, s in enumerate(MAIN_LINE)}
    started = n(lambda t: t.get("started_at") or main_index.get(t["status"], 0) >= 2
               or t["status"] in ("failed",))
    completed = n(lambda t: t.get("completed_at") or main_index.get(t["status"], 0) >= 3)
    reviewed = n(lambda t: t.get("reviewed_at") or main_index.get(t["status"], 0) >= 5)
    counts = {
        "created": len(tasks),
        "started": started,
        "completed": completed,
        "reviewed": reviewed,
        "rejected": sum(t.get("rejections", 0) for t in tasks),
        "externalised": n(lambda t: t["status"] == "externalised"),
        "blocked": n(lambda t: t["status"] == "blocked"),
        "failed": n(lambda t: t["status"] == "failed"),
        "done": n(lambda t: t["status"] == "done"),
    }
    return counts


def ratios(counts):
    def div(a, b):
        return round(a / b, 2) if b else None
    return {
        "drainage": div(counts["completed"], counts["created"]),
        "review_balance": div(counts["reviewed"], counts["completed"]),
        "rejection_rate": div(counts["rejected"], counts["completed"]),
        "externalisation_pressure": div(counts["externalised"], counts["created"]),
    }


def by_status(tasks):
    out = {}
    for t in tasks:
        out[t["status"]] = out.get(t["status"], 0) + 1
    return out


def critical_path(tasks):
    on = [t for t in tasks if t.get("on_critical_path")]
    active = [t for t in tasks if t["status"] not in STOPPED]
    return {
        "on_path_total": len(on),
        "on_path_done": sum(1 for t in on if t["status"] == "done"),
        "on_path_blocked": [t["task_name"] for t in on if t["status"] == "blocked"],
        "on_path_active": sum(1 for t in on if t["status"] not in STOPPED),
        "active_total": len(active),
    }


def read_thresholds(repo_root):
    """Only the thresholds the tool uses, from the overlay's `thresholds:` block (flat YAML)."""
    out = {"drainage_trigger_ratio": 1.0, "drainage_trigger_consecutive_cycles": 3,
           "review_starvation_ratio": 1.5, "cascade_trigger_followup_count": 3}
    p = os.path.join(repo_root, ".claude", "orchestrator", "config.yaml")
    if not os.path.exists(p):
        return out
    in_block = False
    for line in open(p):
        if re.match(r"^thresholds:\s*$", line):
            in_block = True
            continue
        if in_block:
            m = re.match(r"^\s+([a-z_-]+):\s*([\d.]+)", line)
            if m:
                key = m.group(1).replace("-", "_")
                out[key] = float(m.group(2)) if "." in m.group(2) else int(m.group(2))
            elif not line.startswith(" "):
                in_block = False
    return out


def pm_signals(state, repo_root):
    counts = derive_counts(state["tasks"])
    r = ratios(counts)
    history = state["cycle_log"].get("history", [])
    th = read_thresholds(repo_root)
    fired = []
    series = [h["counts"] for h in history[-(th["drainage_trigger_consecutive_cycles"] - 1):]] + [counts]
    drains = [ratios(c)["drainage"] for c in series if c.get("created")]
    if len(drains) >= th["drainage_trigger_consecutive_cycles"] and all(
            d is not None and d < th["drainage_trigger_ratio"] for d in drains):
        fired.append({"signal": "throughput-inversion",
                      "detail": f"drainage {drains} below {th['drainage_trigger_ratio']} for "
                                f"{len(drains)} cycles"})
    st = by_status(state["tasks"])
    if st.get("in_progress") and st.get("needs_review", 0) / st["in_progress"] > th["review_starvation_ratio"]:
        fired.append({"signal": "review-starvation",
                      "detail": f"needs_review {st.get('needs_review', 0)} / in_progress {st['in_progress']}"})
    cp = critical_path(state["tasks"])
    if cp["active_total"] and cp["on_path_active"] / cp["active_total"] < 0.3 and cp["on_path_total"]:
        fired.append({"signal": "priority-inversion",
                      "detail": f"on-path active {cp['on_path_active']} of {cp['active_total']}"})
    followups = {}
    for t in state["tasks"]:
        src = t.get("discovered_from")
        if src:
            followups[src] = followups.get(src, 0) + 1
    for src, n in followups.items():
        if n > th.get("cascade_trigger_followup_count", 3):
            fired.append({"signal": "cascade", "detail": f"{src} has {n} follow-ups; spawn a conformance run scoped to the cluster"})
    classes = {}
    for t in state["tasks"]:
        c = classes.setdefault(t["task_class"], {"total": 0, "done": 0})
        c["total"] += 1
        c["done"] += t["status"] == "done"
    findings = state["architect_findings"]
    fcounts = {
        "total": len(findings),
        "reasoning_only": sum(1 for f in findings
                              if (f.get("observed") or {}).get("happened") is False),
        "reasoning_only_noted": sum(1 for f in findings
                                    if (f.get("observed") or {}).get("happened") is False
                                    and f.get("resolution") == "noted"),
    }
    return {
        "cycle_id": state["cycle_id"],
        "change_name": state["change_name"],
        "produced_at": now_iso(),
        "history_window": state["history_window"],
        "counts": counts,
        "findings": fcounts,
        "ratios": r,
        "by_status": st,
        "history": [{"cycle_id": h["cycle_id"], "counts": h["counts"], "ratios": ratios(h["counts"])}
                    for h in history],
        "critical_path": cp,
        "by_class": classes,
        "followups_by_source": followups,
        "open_asks": [{"id": a["id"], "kind": a["kind"], "blocks": a["blocks"]}
                      for a in state["asks_for_user"] if a["status"] == "open"],
        "blocked_tasks": [{"task": t["task_name"], "blocked_by": t["blocked_by"],
                           "blocker_note": t["blocker_note"]}
                          for t in state["tasks"] if t["status"] == "blocked"],
        "fired_signals": fired,
        "thresholds": th,
    }


# ---------------------------------------------------------------- gates

def _handshake_errors(path, prior=False):
    """Required fields of a handshake. `prior` is the one a plan consumes; the one this cycle
    writes is checked strictly."""
    if not os.path.exists(path):
        return [f"{path} does not exist"]
    try:
        with open(path) as f:
            h = json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        return [f"{path}: {e}"]
    # A prior handshake written by the installed checkout before schema 1.2 carries
    # `register_diff` where `design_diff` now stands; plan accepts it in that place.
    if prior and "design_diff" not in h and "register_diff" in h:
        h = {**h, "design_diff": h["register_diff"]}
    return [f"{path}: missing field {k}" for k in HANDSHAKE_REQUIRED if k not in h]


def _design_diff_errors(diff, where):
    if not isinstance(diff, list):
        return [f"{where}: expected a JSON array"]
    errors = []
    for i, e in enumerate(diff):
        w = f"{where}[{i}]"
        if not isinstance(e, dict):
            errors.append(f"{w}: not an object")
            continue
        if not isinstance(e.get("seam"), str) or not e["seam"].strip():
            errors.append(f"{w}.seam: expected non-empty string")
        if e.get("change") not in DESIGN_DIFF_CHANGES:
            errors.append(f"{w}.change: {e.get('change')!r} not one of {DESIGN_DIFF_CHANGES}")
        if not isinstance(e.get("ref"), str):
            errors.append(f"{w}.ref: expected string")
        for k in e:
            if k not in ("seam", "change", "ref"):
                errors.append(f"{w}.{k}: unknown field (allowed: seam, change, ref)")
    return errors


def _goal_drift_asks(state):
    return [a for a in state["asks_for_user"]
            if {o.get("label") for o in a.get("options", []) if isinstance(o, dict)} >= GOAL_DRIFT_OPTIONS]


def evaluate_gate(store, state, phase):
    """Returns {check: {"value": bool|None, "computed": bool, "reason": str}}."""
    tasks = state["tasks"]
    out = {}

    def put(name, value, reason):
        out[name] = {"value": value, "computed": True, "reason": reason}

    if phase == "plan":
        hp = state.get("prior_handshake")
        if hp is None:
            put("prior_integrate_consumed", True, "first cycle: no prior handshake")
        else:
            errs = _handshake_errors(os.path.join(store.repo_root, hp), prior=True)
            put("prior_integrate_consumed", not errs, "; ".join(errs) or f"{hp} has all required fields")
        missing = [t["task_name"] for t in tasks
                   if t["status"] != "externalised" and not t["cites_seams"]]
        put("briefs_cite_seams", not missing and bool(tasks),
            f"tasks without cites_seams: {missing}" if missing else
            ("no tasks yet" if not tasks else "every non-externalised task cites at least one seam"))
        gd = _goal_drift_asks(state)
        pending = [a["id"] for a in gd if a["status"] not in ("answered", "applied")]
        put("user_signed_off_goal_drift", not pending,
            f"goal-drift asks unanswered: {pending}" if pending else
            ("no goal-drift ask this cycle" if not gd else "goal-drift ask answered"))
    elif phase == "execute":
        not_stopped = [t["task_name"] for t in tasks if t["status"] not in STOPPED]
        put("all_tasks_executed_or_stopped", not not_stopped and bool(tasks),
            f"not stopped: {not_stopped}" if not_stopped else "all tasks done/failed/externalised/blocked")
        unreviewed = []
        for t in tasks:
            if t["status"] in ("reviewed", "done") and t["review_mode"] is not None:
                p = t["findings_path"]
                if not p or not os.path.exists(os.path.join(store.repo_root, p)):
                    unreviewed.append(t["task_name"])
        put("all_reviews_completed", not unreviewed,
            f"reviewed/done without a findings file: {unreviewed}" if unreviewed
            else "every reviewed task has its findings file")
        orphans = [t["task_name"] for t in tasks if t["status"] in ORPHAN]
        put("no_orphan_in_progress", not orphans,
            f"in flight: {orphans}" if orphans else "no task in flight")
    elif phase == "integrate":
        blocking = [f["finding_id"] for f in state["architect_findings"]
                    if f["severity"] == "blocking" and f["resolution"] == "pending"]
        put("blocking_findings_resolved", not blocking,
            f"pending blocking findings: {blocking}" if blocking else "no pending blocking finding")
        digest = os.path.join(store.cycle_dir(state), "pm-digest.md")
        ok, why = False, f"{os.path.relpath(digest, store.repo_root)} missing"
        if os.path.exists(digest):
            text = open(digest).read()
            m = re.search(r"^## Signals\s*\n(.*?)(?=^## |\Z)", text, re.S | re.M)
            ok = bool(m and m.group(1).strip())
            why = "signals section present" if ok else "no non-empty '## Signals' section"
        put("pm_digest_produced", ok, why)
        hpath = store.handshake_path(state["cycle_id"])
        herrs = _handshake_errors(hpath)
        put("handshake_artifact_written", not herrs,
            "; ".join(herrs) or "handshake present with all required fields")
        if herrs:
            put("user_asks_routed", False, "handshake not written yet")
        else:
            h = json.load(open(hpath))
            open_ids = {a["id"] for a in state["asks_for_user"] if a["status"] == "open"}
            in_h = {a.get("id") for a in h.get("asks_for_user_open", [])}
            asks_md = os.path.join(store.cycle_dir(state), "asks.md")
            md = open(asks_md).read() if os.path.exists(asks_md) else ""
            decisions = [a["id"] for a in state["asks_for_user"] if a["kind"] == "decision"]
            missing = sorted(open_ids - in_h)
            unlisted = [i for i in decisions if i not in md]
            problems = []
            if missing:
                problems.append(f"open asks not in handshake: {missing}")
            if unlisted:
                problems.append(f"decision asks not in asks.md: {unlisted}")
            put("user_asks_routed", not problems, "; ".join(problems) or "asks routed")
    for c in GATE_CHECKS[phase]:
        if c not in out:
            stored = state["phase_gates"][phase]["checks"].get(c, False)
            out[c] = {"value": stored, "computed": False,
                      "reason": "asserted by the orchestrator with `gate set`"}
    return out


def apply_gate_eval(state, phase, result):
    checks = state["phase_gates"][phase]["checks"]
    for c, r in result.items():
        if r["computed"]:
            checks[c] = bool(r["value"])
    for c in GATE_CHECKS[phase]:
        checks.setdefault(c, False)


# ---------------------------------------------------------------- verbs

def verb_init(store, args):
    if store.exists():
        cur = store.load()
        if not cur.get("closed_at"):
            raise Refusal("cycle_open", f"cycle {cur.get('cycle_id')} is still open (phase "
                          f"{cur.get('phase')}); run `close` (or `close --abandon`) first")
    if args.prior_handshake is None and not args.first_cycle:
        # Default: the most recent handshake on disk, if any.
        hs = sorted(glob.glob(os.path.join(store.dir, "handshake-*.json")), key=os.path.getmtime)
        args.prior_handshake = os.path.relpath(hs[-1], store.repo_root) if hs else None
        if args.prior_handshake is None:
            raise Refusal("no_prior_handshake", "no handshake-*.json found; pass --prior-handshake "
                          "or --first-cycle")
    if args.prior_handshake:
        errs = _handshake_errors(os.path.join(store.repo_root, args.prior_handshake), prior=True)
        if errs:
            raise Refusal("bad_prior_handshake", "plan refuses to start: " + "; ".join(errs))
    ts = int(time.time())
    cycle_id = args.cycle or f"cycle-{ts}"
    state = {
        "schema_version": SCHEMA_VERSION,
        "session_id": f"orch-{ts}",
        "cycle_id": cycle_id,
        "phase": "plan",
        "repo_root": store.repo_root,
        "change_name": args.change,
        "baseline_snapshot": None,
        "baseline_status": None,
        "current_branch": args.branch or _git_branch(store.repo_root),
        "test_command": args.test_command,
        "history_window": args.history_window,
        "prior_handshake": args.prior_handshake,
        "closed_at": None,
        "tasks": [],
        "architect_findings": [],
        "asks_for_user": [],
        "journal": [],
        "cycle_log": {"started_at": now_iso(), "phase_started_at": {"plan": now_iso(), "execute": None,
                                                                    "integrate": None},
                      "history": collect_history(store, args.history_window)},
        "phase_gates": gates_fresh(),
    }
    errors = validate(state)
    if errors:
        raise Refusal("invalid_state", "init produced an invalid state (bug)", errors=errors)
    store.write(state)
    return {"ok": True, "cycle_id": cycle_id, "path": store.path,
            "history_cycles": [h["cycle_id"] for h in state["cycle_log"]["history"]]}


def _git_branch(repo_root):
    head = os.path.join(repo_root, ".git", "HEAD")
    try:
        ref = open(head).read().strip()
        return ref.split("/", 2)[-1] if ref.startswith("ref:") else ref[:12]
    except OSError:
        return "main"


def collect_history(store, window):
    """Per-cycle counts from archived states, newest last, at most `window`."""
    out = []
    for p in sorted(glob.glob(os.path.join(store.dir, "cycles", "cycle-*", "state.json"))):
        try:
            s = json.load(open(p))
        except (json.JSONDecodeError, OSError):
            continue
        cid = s.get("cycle_id") or os.path.basename(os.path.dirname(p))
        if str(s.get("schema_version")) == SCHEMA_VERSION:
            counts = derive_counts(s["tasks"])
        else:
            legacy = (s.get("cycle_log") or {}).get("counts") if isinstance(s.get("cycle_log"), dict) else None
            counts = legacy if isinstance(legacy, dict) else derive_counts_legacy(s.get("tasks") or [])
        out.append({"cycle_id": cid, "counts": counts})
    return out[-window:]


def derive_counts_legacy(tasks):
    fixed = []
    for t in tasks:
        if not isinstance(t, dict):
            continue
        u = dict(TASK_DEFAULTS)
        u.update({k: v for k, v in t.items() if k in TASK_FIELDS})
        u["status"] = LEGACY_STATUS.get(t.get("status"), t.get("status"))
        if u["status"] not in TASK_STATUSES:
            u["status"] = "ready"
        fixed.append(u)
    return derive_counts(fixed)


def verb_status(store, args):
    state = store.load()
    if str(state.get("schema_version")) != SCHEMA_VERSION:
        return {"ok": True, "schema_version": state.get("schema_version"), "legacy": True,
                "cycle_id": state.get("cycle_id"), "phase": state.get("phase"),
                "message": f"schema {state.get('schema_version')} predates {SCHEMA_VERSION}; run "
                           "`migrate` before writing, `validate` to see the diff"}
    errors = validate(state)
    gates = {}
    for phase in PHASES:
        ev = evaluate_gate(store, state, phase)
        gates[phase] = {"passed": state["phase_gates"][phase]["passed"],
                        "failing": [c for c, r in ev.items() if not r["value"]]}
    out = {
        "ok": True,
        "cycle_id": state["cycle_id"],
        "change_name": state["change_name"],
        "phase": state["phase"],
        "closed_at": state["closed_at"],
        "resume": resume_hint(state),
        "gates": gates,
        "counts": derive_counts(state["tasks"]),
        "tasks": [{"name": t["task_name"], "status": t["status"],
                   "merge": (t["merge_commit"] or "")[:10] or None,
                   "blocked_by": t["blocked_by"], "blocker_note": t["blocker_note"],
                   "critical": t["on_critical_path"]} for t in state["tasks"]],
        "open_asks": [{"id": a["id"], "kind": a["kind"], "blocks": a["blocks"]}
                      for a in state["asks_for_user"] if a["status"] == "open"],
        "pending_blocking_findings": [f["finding_id"] for f in state["architect_findings"]
                                      if f["severity"] == "blocking" and f["resolution"] == "pending"],
        "journal_tail": state["journal"][-5:],
        "prior_handshake": state["prior_handshake"],
        "validation_errors": errors,
    }
    return out


def resume_hint(state):
    """SKILL.md mode detection, computed."""
    g = state["phase_gates"]
    if state["closed_at"]:
        return "cycle closed: run `init` for the next cycle"
    if g["integrate"]["passed"]:
        return "integrate passed but cycle not closed: run `close`"
    if g["execute"]["passed"] and state["phase"] != "integrate":
        return "execute passed: `phase set integrate`"
    if g["plan"]["passed"] and state["phase"] == "plan":
        return "plan passed: `phase set execute`"
    return f"resume {state['phase']}"


def verb_validate(store, args):
    state = store.load()
    if str(state.get("schema_version")) != SCHEMA_VERSION:
        plan = migration_plan(state)
        return {"ok": False, "schema_version": state.get("schema_version"), "migration": plan}
    errors = validate(state)
    return {"ok": not errors, "errors": errors}


def verb_task_add(store, args):
    def fn(state):
        if any(t["task_name"] == args.name for t in state["tasks"]):
            raise Refusal("duplicate_task", f"task {args.name!r} already exists")
        t = dict(TASK_DEFAULTS)
        t.update({"task_name": args.name, "task_file": args.file, "task_class": args.task_class})
        if args.cites:
            t["cites_seams"] = [c for c in args.cites.split(",") if c]
        if args.blocked_by:
            t["blocked_by"] = [c for c in args.blocked_by.split(",") if c]
        if args.critical:
            t["on_critical_path"] = True
        t.update(_task_field_updates(parse_assignments(args.fields), t))
        state["tasks"].append(t)
        return {"ok": True, "task": t}
    return store.mutate(fn)


def _task_field_updates(updates, task):
    for k in updates:
        if k not in TASK_FIELDS:
            raise Refusal("unknown_field", f"task field {k!r} is not in the schema; use `note` for "
                          "narrative or a file for reports",
                          allowed=sorted(TASK_FIELDS))
        if k == "status":
            raise Refusal("use_status_arg", "set status with `task set <name> <status>`")
    return updates


def verb_task_set(store, args):
    if args.status and "=" in args.status:
        # `task set <name> field=value` with no status: argparse gave the first assignment to `status`.
        args.fields.insert(0, args.status)
        args.status = None

    def fn(state):
        t = find_task(state, args.name)
        updates = _task_field_updates(parse_assignments(args.fields), t)
        old = t["status"]
        new = args.status or old
        if args.status and new not in TASK_STATUSES:
            raise Refusal("bad_status", f"{new!r} is not a task status", allowed=TASK_STATUSES,
                          hint="implemented -> completed; fix_in_progress -> reviewed; "
                               "reviewed_blocked/blocked_on_finding -> blocked with blocker_note")
        if args.status and new != old and not transition_allowed(old, new):
            if not args.force:
                raise Refusal("bad_transition", f"{old} -> {new} is not an allowed transition "
                              "(state.md field rules); pass --force --why '<reason>' to record a deviation")
            if not args.why:
                raise Refusal("why_required", "--force needs --why")
            add_journal(state, "deviation", f"task {t['task_name']}: {old} -> {new}: {args.why}",
                        by="orchestrator", ref=t["task_name"])
        t.update(updates)
        if args.status and new != old:
            t["status"] = new
            ts = now_iso()
            if new in ("in_progress", "setup_complete") and not t["started_at"]:
                t["started_at"] = ts
            if new in ("completed", "needs_review") and not t["completed_at"]:
                t["completed_at"] = ts
            if new in ("reviewed", "done") and not t["reviewed_at"]:
                t["reviewed_at"] = ts
            if new == "done" and not t["completed_at"]:
                t["completed_at"] = ts
            if (old, new) in (("needs_review", "in_progress"), ("reviewed", "in_progress")):
                t["rejections"] += 1
            if new == "needs_review" and not t["merge_commit"]:
                raise Refusal("merge_commit_required",
                              "needs_review requires merge_commit=<sha> in the same command")
        return {"ok": True, "task": t["task_name"], "status": t["status"],
                "changed": sorted(updates) + (["status"] if new != old else [])}
    return store.mutate(fn)


def transition_allowed(old, new):
    if (old, new) in BACKWARD_ALLOWED:
        return True
    if new in SIDE_STATES:
        return old != "done" or new == "externalised"
    if old in SIDE_STATES:
        return False
    return MAIN_LINE.index(new) > MAIN_LINE.index(old)


def verb_task_remove(store, args):
    def fn(state):
        t = find_task(state, args.name)
        if t["status"] not in ("ready", "externalised", "failed"):
            raise Refusal("task_in_flight", f"{args.name} is {t['status']}; only ready/failed/"
                          "externalised tasks can be removed")
        state["tasks"].remove(t)
        add_journal(state, "note", f"task {args.name} removed: {args.why}", by="orchestrator",
                    ref=args.name)
        return {"ok": True, "removed": args.name}
    return store.mutate(fn)


RECORD_LISTS = {
    "findings": ("architect_findings", FINDING_FIELDS, FINDING_DEFAULTS, "finding_id"),
    "asks": ("asks_for_user", ASK_FIELDS, ASK_DEFAULTS, "id"),
}


def verb_record_add(store, args):
    key, fields, defaults, id_field = RECORD_LISTS[args.list]
    rec_in = load_json_arg(args.json)
    if not isinstance(rec_in, dict):
        raise Refusal("bad_record", "record must be a JSON object")

    def fn(state):
        rec = dict(defaults)
        rec.update(rec_in)
        if not rec.get(id_field):
            prefix = {"finding_id": "arch-", "id": "ask-"}[id_field] + state["cycle_id"] + "-"
            rec[id_field] = prefix + str(next_seq([r.get(id_field) for r in state[key]], prefix))
        if any(r.get(id_field) == rec[id_field] for r in state[key]):
            raise Refusal("duplicate_id", f"{key} already has {rec[id_field]!r}; use `record set`")
        if (args.list == "findings" and rec.get("severity") in OBSERVED_REQUIRED_SEVERITIES
                and rec.get("observed") is None):
            raise Refusal("observed_required",
                          f"a {rec.get('severity')} finding must say whether it has happened: "
                          "observed={\"happened\": true|false, \"evidence\": \"<where | reasoning only: ...>\"}")
        if args.list == "findings" and not rec.get("path"):
            rec["path"] = os.path.relpath(os.path.join(store.cycle_dir(state), "findings",
                                                       rec["finding_id"] + ".md"), store.repo_root)
        errors = []
        _validate_record(rec, fields, f"{key}[new]", errors, id_field)
        if args.list == "findings":
            _validate_observed(rec.get("observed"), f"{key}[new].observed", errors)
        if errors:
            raise Refusal("invalid_record", "record does not fit the schema", errors=errors)
        state[key].append(rec)
        return {"ok": True, "list": key, "id": rec[id_field], "record": rec}
    return store.mutate(fn)


def verb_record_set(store, args):
    key, fields, defaults, id_field = RECORD_LISTS[args.list]
    updates = parse_assignments(args.fields)

    def fn(state):
        rec = next((r for r in state[key] if r.get(id_field) == args.id), None)
        if rec is None:
            raise Refusal("no_such_record", f"{key} has no {args.id!r}",
                          ids=[r.get(id_field) for r in state[key]])
        for k in updates:
            if k not in fields:
                raise Refusal("unknown_field", f"{k!r} is not a field of {key}", allowed=sorted(fields))
        rec.update(updates)
        return {"ok": True, "list": key, "id": args.id, "changed": sorted(updates)}
    return store.mutate(fn)


def verb_note(store, args):
    if len(args.text) > TEXT_CAP:
        raise Refusal("text_too_long", f"journal text is {len(args.text)} chars; cap is {TEXT_CAP}. "
                      "Write the long form to a file and pass --path")

    def fn(state):
        e = add_journal(state, args.kind, args.text, by=args.by, ref=args.ref, path=args.path)
        return {"ok": True, "entry": e}
    return store.mutate(fn)


def verb_set(store, args):
    updates = parse_assignments(args.fields)

    def fn(state):
        for k, v in updates.items():
            if k not in TOP_LEVEL_SETTABLE:
                raise Refusal("not_settable", f"{k!r} cannot be set directly",
                              settable=sorted(TOP_LEVEL_SETTABLE))
            state[k] = v
        return {"ok": True, "changed": sorted(updates)}
    return store.mutate(fn)


def verb_phase_set(store, args):
    def fn(state):
        target = args.phase
        cur = state["phase"]
        if target == cur:
            return {"ok": True, "phase": cur, "unchanged": True}
        idx = PHASES.index(target)
        if idx != PHASES.index(cur) + 1:
            raise Refusal("phase_order", f"{cur} -> {target}: phases advance one at a time")
        prior = PHASES[idx - 1]
        if not state["phase_gates"][prior]["passed"]:
            raise Refusal("gate_not_passed", f"{prior} gate has not passed; run `gate check {prior}`")
        state["phase"] = target
        state["cycle_log"]["phase_started_at"][target] = now_iso()
        return {"ok": True, "phase": target}
    return store.mutate(fn)


def verb_gate_check(store, args):
    def fn(state):
        ev = evaluate_gate(store, state, args.phase)
        apply_gate_eval(state, args.phase, ev)
        return {"ok": True, "phase": args.phase,
                "all_true": all(r["value"] for r in ev.values()),
                "checks": ev}
    return store.mutate(fn)


def verb_gate_set(store, args):
    updates = parse_assignments(args.fields)

    def fn(state):
        checks = state["phase_gates"][args.phase]["checks"]
        for k, v in updates.items():
            if k not in GATE_CHECKS[args.phase]:
                raise Refusal("unknown_check", f"{k!r} is not a {args.phase} check",
                              allowed=GATE_CHECKS[args.phase])
            if k not in ASSERTED_CHECKS:
                raise Refusal("computed_check", f"{k} is computed by `gate check`; it cannot be asserted")
            if not isinstance(v, bool):
                raise Refusal("bad_value", f"{k} must be true or false")
            checks[k] = v
        return {"ok": True, "phase": args.phase, "checks": checks}
    return store.mutate(fn)


def verb_gate_pass(store, args):
    def fn(state):
        ev = evaluate_gate(store, state, args.phase)
        apply_gate_eval(state, args.phase, ev)
        failing = {c: r["reason"] for c, r in ev.items() if not r["value"]}
        if failing:
            raise Refusal("gates_failed", f"{args.phase} gate cannot pass", failing=failing)
        g = state["phase_gates"][args.phase]
        g["passed"] = True
        g["passed_at"] = now_iso()
        return {"ok": True, "phase": args.phase, "passed": True}
    return store.mutate(fn)


def verb_counts(store, args):
    state = store.load()
    require_current_schema(state)
    sig = pm_signals(state, store.repo_root)
    if args.write:
        d = store.cycle_dir(state)
        os.makedirs(d, exist_ok=True)
        p = os.path.join(d, "pm-signals.json")
        with open(p, "w") as f:
            json.dump(sig, f, indent=2)
        sig["written_to"] = os.path.relpath(p, store.repo_root)
    sig["ok"] = True
    return sig


def verb_handshake(store, args):
    meta = load_json_arg(args.meta_discoveries) if args.meta_discoveries else []
    refinements = load_json_arg(args.task_refinements) if args.task_refinements else []
    if not isinstance(meta, list) or not isinstance(refinements, list):
        raise Refusal("bad_json", "meta-discoveries and task-refinements must be JSON arrays")
    flag_diff = load_json_arg(args.design_diff) if args.design_diff else None

    def fn(state):
        digest = os.path.relpath(os.path.join(store.cycle_dir(state), "pm-digest.md"), store.repo_root)
        # design_diff: the conformance step's cycles/<id>/design-diff.json when present,
        # else --design-diff, else empty.
        diff_file = os.path.join(store.cycle_dir(state), "design-diff.json")
        if os.path.exists(diff_file):
            try:
                with open(diff_file) as f:
                    diff = json.load(f)
            except (json.JSONDecodeError, OSError) as e:
                raise Refusal("bad_design_diff", f"{os.path.relpath(diff_file, store.repo_root)}: {e}")
            source = os.path.relpath(diff_file, store.repo_root)
        elif flag_diff is not None:
            diff, source = flag_diff, "--design-diff"
        else:
            diff, source = [], "default"
        derrs = _design_diff_errors(diff, "design_diff")
        if derrs:
            raise Refusal("bad_design_diff", f"design diff from {source} does not fit the shape "
                          "[{seam, change: added|amended|retired, ref}]", errors=derrs[:20])
        goal = []
        for a in _goal_drift_asks(state):
            if a["status"] in ("answered", "applied"):
                goal.append({"ask": a["id"], "decision": a["decision"],
                             "rationale": a["decision_readback"]})
        h = {
            "cycle_id": state["cycle_id"],
            "produced_at": now_iso(),
            "design_diff": diff,
            "pm_digest_path": digest,
            "meta_discoveries": meta,
            "user_resolved_goal_drift": goal,
            "asks_for_user_open": [a for a in state["asks_for_user"] if a["status"] == "open"],
            "asks_for_user_resolved": [a for a in state["asks_for_user"]
                                       if a["status"] in ("answered", "applied", "deferred")],
            "task_refinements": refinements,
            "journal": state["journal"],
        }
        p = store.handshake_path(state["cycle_id"])
        with open(p + ".tmp", "w") as f:
            json.dump(h, f, indent=2)
        os.replace(p + ".tmp", p)
        return {"ok": True, "path": os.path.relpath(p, store.repo_root),
                "design_diff": len(diff), "design_diff_source": source,
                "asks_open": len(h["asks_for_user_open"]),
                "asks_resolved": len(h["asks_for_user_resolved"]), "task_refinements": len(refinements)}
    return store.mutate(fn)


def verb_close(store, args):
    def fn(state):
        if not args.abandon and not state["phase_gates"]["integrate"]["passed"]:
            raise Refusal("gate_not_passed", "integrate gate has not passed; `gate pass integrate` "
                          "first, or `close --abandon --why '...'`")
        if args.abandon:
            if not args.why:
                raise Refusal("why_required", "--abandon needs --why")
            add_journal(state, "deviation", f"cycle abandoned: {args.why}", by="orchestrator")
        state["closed_at"] = now_iso()
        d = store.cycle_dir(state)
        os.makedirs(d, exist_ok=True)
        store.write(state)  # so the archive copy carries closed_at
        shutil.copy(store.path, os.path.join(d, "state.json"))
        copied = ["state.json"]
        hp = store.handshake_path(state["cycle_id"])
        if os.path.exists(hp):
            shutil.copy(hp, os.path.join(d, "handshake.json"))
            copied.append("handshake.json")
        return {"ok": True, "cycle_id": state["cycle_id"], "archived_to": os.path.relpath(d, store.repo_root),
                "copied": copied, "abandoned": bool(args.abandon),
                "counts": derive_counts(state["tasks"])}
    return store.mutate(fn)


# ---------------------------------------------------------------- migrate

LEGACY_STATUS = {
    "implemented": "completed", "merged": "completed", "fix_in_progress": "reviewed",
    "fix_pending": "reviewed", "reviewed_blocked": "blocked", "blocked_on_finding": "blocked",
    "in-progress": "in_progress", "needs-review": "needs_review",
}
# Task fields renamed across schema versions: old name -> current name (applied when the
# current name is absent; otherwise the old value is parked).
LEGACY_TASK_ALIASES = {"branch": "branch_name", "after_test_file": "after_snapshot",
                       "cites_register_entries": "cites_seams"}
# Task fields dropped from the schema whose null value carries nothing worth parking.
LEGACY_TASK_DROP_WHEN_NULL = {"reconciled_into"}
# Finding triggers before schema 1.2 -> current trigger; the original is kept as _legacy_trigger.
LEGACY_TRIGGERS = {"on-touch": "conformance", "end-of-cycle": "conformance",
                   "between-cycle": "conformance", "forward-mode": "premise-check"}
# The pre-1.2 register list; parked whole, its record count reported.
LEGACY_REGISTER_KEY = "register_touched"


def _legacy_task_extras(t):
    return sorted(k for k in t
                  if k not in TASK_FIELDS
                  and not (k in LEGACY_TASK_ALIASES and LEGACY_TASK_ALIASES[k] not in t)
                  and not (k in LEGACY_TASK_DROP_WHEN_NULL and t[k] is None))


def migration_plan(state):
    """What `migrate` would do to a legacy file. Pure."""
    plan = {"from": state.get("schema_version"), "to": SCHEMA_VERSION,
            "top_level_parked": [], "task_fields_parked": {}, "status_mapped": {},
            "records_parked": {}, "asks_kept": 0, "asks_parked": 0,
            "register_touched_parked": 0}
    reg = state.get(LEGACY_REGISTER_KEY)
    plan["register_touched_parked"] = len(reg) if isinstance(reg, list) else 0
    for k in state:
        kk = LEGACY_ALIASES.get(k, k)
        if kk not in TOP_LEVEL:
            plan["top_level_parked"].append(k)
    for t in state.get("tasks") or []:
        if not isinstance(t, dict):
            continue
        extra = _legacy_task_extras(t)
        if extra:
            plan["task_fields_parked"][t.get("task_name", "?")] = extra
        s = t.get("status")
        if s not in TASK_STATUSES:
            plan["status_mapped"][t.get("task_name", "?")] = f"{s} -> {LEGACY_STATUS.get(s, 'ready')}"
    for key, fields in (("architect_findings", FINDING_FIELDS),):
        n = 0
        for r in state.get(key) or []:
            if isinstance(r, dict) and any(k not in fields for k in r):
                n += 1
        if n:
            plan["records_parked"][key] = n
    asks = state.get("asks_for_user")
    if isinstance(asks, list):
        for a in asks:
            if isinstance(a, dict) and a.get("id") and a.get("question") and a.get("kind") in ASK_KINDS:
                plan["asks_kept"] += 1
            else:
                plan["asks_parked"] += 1
    return plan


def verb_migrate(store, args):
    with open(store.lock_path, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        try:
            old = store.load()
            if str(old.get("schema_version")) == SCHEMA_VERSION:
                return {"ok": True, "unchanged": True}
            parked = {"parked_at": now_iso(), "from_schema": old.get("schema_version"), "top_level": {},
                      "tasks": {}, "architect_findings": [], "asks_for_user": [], "cycle_log": {}}
            new = {}
            for k, v in old.items():
                kk = LEGACY_ALIASES.get(k, k)
                if kk in TOP_LEVEL:
                    new[kk] = v
                else:
                    parked["top_level"][k] = v
            new["schema_version"] = SCHEMA_VERSION
            new.setdefault("prior_handshake", None)
            new.setdefault("closed_at", None)
            new.setdefault("journal", [])
            new.setdefault("asks_for_user", [])
            for k in ("tasks", "architect_findings"):
                new.setdefault(k, [])
            if not isinstance(new.get("baseline_status"), (int, type(None))):
                new["baseline_status"] = None
            for k, typ in TOP_LEVEL.items():
                if k not in new or not _check_type(new[k], typ):
                    if k in new:
                        parked["top_level"][k] = new[k]
                    new[k] = {str: "", int: 5, list: [], dict: {}}.get(
                        typ if not isinstance(typ, tuple) else typ[0], None)
            # tasks
            tasks = []
            for t in old.get("tasks") or []:
                if not isinstance(t, dict):
                    continue
                u = dict(TASK_DEFAULTS)
                extra = {}
                for k, v in t.items():
                    if k in LEGACY_TASK_ALIASES and LEGACY_TASK_ALIASES[k] not in t:
                        u[LEGACY_TASK_ALIASES[k]] = v
                    elif k in LEGACY_TASK_DROP_WHEN_NULL and v is None:
                        continue
                    elif k in TASK_FIELDS:
                        u[k] = v
                    else:
                        extra[k] = v
                s = u.get("status")
                if s not in TASK_STATUSES:
                    u["status"] = LEGACY_STATUS.get(s, "ready")
                    extra["_legacy_status"] = s
                if u["status"] == "blocked" and not u.get("blocker_note"):
                    u["blocker_note"] = extra.get("_legacy_status") or "migrated"
                if u["status"] == "needs_review" and not u.get("merge_commit"):
                    u["status"] = "completed"
                for k, typ in TASK_FIELDS.items():
                    if not _check_type(u.get(k), typ):
                        extra[k] = u.get(k)
                        u[k] = TASK_DEFAULTS.get(k, "" if typ is str else None)
                for k, v in list(u.items()):
                    if isinstance(v, str) and len(v) > TEXT_CAP:
                        extra[k + "_full"], u[k] = v, v[:TEXT_CAP - 1] + "…"
                if extra:
                    parked["tasks"][u.get("task_name", "?")] = extra
                tasks.append(u)
            new["tasks"] = tasks
            # records
            parked["record_extras"] = {}

            def cap_strings(u, extra, fields):
                for k, v in list(u.items()):
                    if isinstance(v, str):
                        cap = LONG_TEXT_CAP if k in LONG_TEXT_FIELDS else TEXT_CAP
                        if len(v) > cap:
                            extra[k + "_full"] = v
                            u[k] = v[:cap - 1] + "…"

            LEGACY_ASK_STATUS = {"resolved": "answered", "answered": "answered", "applied": "applied",
                                 "open": "open", "deferred": "deferred", "declined": "declined"}
            for key, fields, defaults, id_field in (
                    ("architect_findings", FINDING_FIELDS, FINDING_DEFAULTS, "finding_id"),
                    ("asks_for_user", ASK_FIELDS, ASK_DEFAULTS, "id")):
                kept = []
                extras_here = parked["record_extras"].setdefault(key, {})
                for r in old.get(key) or []:
                    if not isinstance(r, dict):
                        parked[key].append(r)
                        continue
                    u = dict(defaults)
                    extra = {}
                    for k, v in r.items():
                        (u if k in fields else extra).__setitem__(k, v)
                    if key == "architect_findings":
                        if not u.get("path"):
                            u["path"] = extra.get("findings_path") or (
                                f".orchestrator/cycles/{old.get('cycle_id')}/findings/{u.get('finding_id')}.md")
                        if u.get("class") not in FINDING_CLASSES:
                            extra["_legacy_class"], u["class"] = u.get("class"), None
                        if u.get("severity") not in SEVERITIES:
                            extra["_legacy_severity"], u["severity"] = u.get("severity"), "advisory"
                        if u.get("trigger") not in TRIGGERS:
                            extra["_legacy_trigger"] = u.get("trigger")
                            u["trigger"] = LEGACY_TRIGGERS.get(u.get("trigger"), "conformance")
                        if not isinstance(u.get("resolution"), str):
                            u["resolution"] = "pending"
                    if key == "asks_for_user":
                        # Legacy shape (title/source/severity/resolution): every legacy ask was a
                        # question to the user, so it is a decision unless it said otherwise.
                        if u.get("kind") not in ASK_KINDS:
                            extra["_legacy_kind"], u["kind"] = u.get("kind"), "decision"
                        if not isinstance(u.get("raised_by"), dict):
                            u["raised_by"] = {"role": str(extra.get("source") or "orchestrator"), "ref": None}
                        if not isinstance(u.get("question"), str):
                            u["question"] = str(extra.get("title") or u.get("id"))
                        if u.get("status") not in ASK_STATUSES:
                            extra["_legacy_status"], u["status"] = u.get("status"), \
                                LEGACY_ASK_STATUS.get(str(u.get("status")), "open")
                        if u["status"] != "open" and not u.get("decision_readback") and extra.get("resolution"):
                            u["decision_readback"] = str(extra["resolution"])
                            u["decision"] = u.get("decision") or "see readback"
                    cap_strings(u, extra, fields)
                    errs = []
                    _validate_record(u, fields, key, errs, id_field)
                    if errs:
                        r["_migration_errors"] = errs
                        parked[key].append(r)
                        continue
                    if extra:
                        extras_here[str(u[id_field])] = extra
                    kept.append(u)
                new[key] = kept
            cl = old.get("cycle_log")
            if isinstance(cl, dict):
                new["cycle_log"] = {"started_at": cl.get("started_at") or now_iso(),
                                    "phase_started_at": cl.get("phase_started_at") or
                                    {"plan": None, "execute": None, "integrate": None},
                                    "history": [h for h in (cl.get("history") or [])
                                                if isinstance(h, dict) and "counts" in h]}
                parked["cycle_log"] = {k: v for k, v in cl.items()
                                       if k not in ("started_at", "phase_started_at", "history")}
            else:
                parked["cycle_log"] = cl
                new["cycle_log"] = {"started_at": now_iso(),
                                    "phase_started_at": {"plan": None, "execute": None, "integrate": None},
                                    "history": []}
            for p in PHASES:
                if new["cycle_log"]["phase_started_at"].get(p, "") == "":
                    new["cycle_log"]["phase_started_at"][p] = None
            gates = gates_fresh()  # current check names; a check absent from the old file is false
            for p in PHASES:
                g = (old.get("phase_gates") or {}).get(p) or {}
                gates[p]["passed"] = bool(g.get("passed"))
                old_checks = g.get("checks") if isinstance(g.get("checks"), dict) else {}
                for c in GATE_CHECKS[p]:
                    gates[p]["checks"][c] = bool(old_checks.get(c))
                extra = {k: v for k, v in g.items() if k not in ("passed", "checks")}
                dropped = {c: v for c, v in old_checks.items() if c not in GATE_CHECKS[p]}
                if dropped:
                    extra["_dropped_checks"] = dropped
                if extra:
                    parked["top_level"][f"phase_gates.{p}"] = extra
            new["phase_gates"] = gates
            if new.get("phase") not in PHASES:
                new["phase"] = "plan"
            errors = validate(new)
            if errors:
                raise Refusal("migration_failed", "migrated state still invalid", errors=errors[:30])
            d = store.cycle_dir(new)
            os.makedirs(d, exist_ok=True)
            parked_path = os.path.join(d, "legacy-state-extras.json")
            with open(parked_path, "w") as f:
                json.dump(parked, f, indent=2)
            backup = store.path + f".pre-migrate-{int(time.time())}"
            shutil.copy(store.path, backup)
            reg = parked["top_level"].get(LEGACY_REGISTER_KEY)
            reg_count = len(reg) if isinstance(reg, list) else 0
            add_journal(new, "note", f"migrated state from schema {old.get('schema_version')} to "
                        f"{SCHEMA_VERSION}; non-schema content parked", by="state.py",
                        path=os.path.relpath(parked_path, store.repo_root))
            store.write(new)
            return {"ok": True, "from": old.get("schema_version"), "to": SCHEMA_VERSION,
                    "parked": os.path.relpath(parked_path, store.repo_root),
                    "backup": os.path.relpath(backup, store.repo_root),
                    "parked_top_level_keys": sorted(parked["top_level"]),
                    "tasks_with_parked_fields": sorted(parked["tasks"]),
                    "register_touched_parked": reg_count,
                    "records": {k: {"kept": len(new[k]), "parked": len(parked[k]),
                                    "with_extras": len(parked["record_extras"].get(k, {}))}
                                for k in ("architect_findings", "asks_for_user")}}
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)


# ---------------------------------------------------------------- CLI

def build_parser():
    p = argparse.ArgumentParser(prog="state.py", description=__doc__.split("\n\n")[0])
    p.add_argument("--repo", help="repo root (default: walk up to .claude/orchestrator/config.yaml)")
    sub = p.add_subparsers(dest="verb", required=True)

    s = sub.add_parser("status", help="orientation: phase, gates, tasks, open asks (run first)")
    s.set_defaults(fn=verb_status)
    s = sub.add_parser("validate", help="schema check; on a legacy file, what migrate would do")
    s.set_defaults(fn=verb_validate)
    s = sub.add_parser("migrate", help=f"convert an older-schema file to {SCHEMA_VERSION}; "
                       "parks non-schema content in a sidecar")
    s.set_defaults(fn=verb_migrate)

    s = sub.add_parser("init", help="start a new cycle (refuses while one is open)")
    s.add_argument("--change", required=True)
    s.add_argument("--test-command", required=True)
    s.add_argument("--cycle", help="cycle id (default cycle-<unix-ts>)")
    s.add_argument("--branch")
    s.add_argument("--history-window", type=int, default=5)
    s.add_argument("--prior-handshake", help="path relative to repo root (default: newest handshake-*.json)")
    s.add_argument("--first-cycle", action="store_true", help="no prior handshake exists")
    s.set_defaults(fn=verb_init)

    s = sub.add_parser("set", help="set a top-level scalar: baseline_snapshot, baseline_status, current_branch, test_command")
    s.add_argument("fields", nargs="+", metavar="field=value")
    s.set_defaults(fn=verb_set)

    t = sub.add_parser("task", help="task add | set | remove").add_subparsers(dest="sub", required=True)
    s = t.add_parser("add")
    s.add_argument("name")
    s.add_argument("--file", required=True, help="task file path relative to repo root")
    s.add_argument("--class", dest="task_class", required=True)
    s.add_argument("--cites", help="comma-separated seam ids (seam/<name>)")
    s.add_argument("--blocked-by", help="comma-separated task names")
    s.add_argument("--critical", action="store_true")
    s.add_argument("fields", nargs="*", metavar="field=value")
    s.set_defaults(fn=verb_task_add)
    s = t.add_parser("set", help="task set <name> [status] [field=value ...]")
    s.add_argument("name")
    s.add_argument("status", nargs="?", help="new status (omit to set fields only)")
    s.add_argument("fields", nargs="*", metavar="field=value")
    s.add_argument("--force", action="store_true", help="allow a non-standard transition; records a deviation")
    s.add_argument("--why")
    s.set_defaults(fn=verb_task_set)
    s = t.add_parser("remove")
    s.add_argument("name")
    s.add_argument("--why", required=True)
    s.set_defaults(fn=verb_task_remove)

    r = sub.add_parser("record", help=f"record add | set  ({', '.join(sorted(RECORD_LISTS))})").add_subparsers(dest="sub", required=True)
    s = r.add_parser("add")
    s.add_argument("list", choices=sorted(RECORD_LISTS))
    s.add_argument("json", help="JSON object, or @file.json; ids are assigned when omitted")
    s.set_defaults(fn=verb_record_add)
    s = r.add_parser("set")
    s.add_argument("list", choices=sorted(RECORD_LISTS))
    s.add_argument("id")
    s.add_argument("fields", nargs="+", metavar="field=value")
    s.set_defaults(fn=verb_record_set)

    s = sub.add_parser("note", help="one journal line (<=280 chars); long form goes in a file via --path")
    s.add_argument("kind", choices=JOURNAL_KINDS)
    s.add_argument("text")
    s.add_argument("--ref", help="task name, finding id, ask id or seam id")
    s.add_argument("--path", help="file holding the long form")
    s.add_argument("--by", help="role that produced it")
    s.set_defaults(fn=verb_note)

    ph = sub.add_parser("phase", help="phase set <plan|execute|integrate>").add_subparsers(dest="sub", required=True)
    s = ph.add_parser("set")
    s.add_argument("phase", choices=PHASES)
    s.set_defaults(fn=verb_phase_set)

    g = sub.add_parser("gate", help="gate check | set | pass").add_subparsers(dest="sub", required=True)
    s = g.add_parser("check", help="evaluate the computable checks and store them")
    s.add_argument("phase", choices=PHASES)
    s.set_defaults(fn=verb_gate_check)
    s = g.add_parser("set", help="assert a non-computable check")
    s.add_argument("phase", choices=PHASES)
    s.add_argument("fields", nargs="+", metavar="check=true|false")
    s.set_defaults(fn=verb_gate_set)
    s = g.add_parser("pass", help="re-evaluate and mark the gate passed if every check is true")
    s.add_argument("phase", choices=PHASES)
    s.set_defaults(fn=verb_gate_pass)

    s = sub.add_parser("counts", help="the PM deterministic pass: counts, ratios, history, signals")
    s.add_argument("--write", action="store_true", help="also write cycles/<id>/pm-signals.json")
    s.set_defaults(fn=verb_counts)

    s = sub.add_parser("handshake", help="assemble and write handshake-<cycle>.json from state")
    s.add_argument("--meta-discoveries", help="JSON array or @file")
    s.add_argument("--task-refinements", help="JSON array or @file")
    s.add_argument("--design-diff", help="JSON array or @file of {seam, change, ref}; ignored when "
                   "cycles/<id>/design-diff.json exists (default [])")
    s.set_defaults(fn=verb_handshake)

    s = sub.add_parser("close", help="archive the cycle to cycles/<id>/ and mark it closed")
    s.add_argument("--abandon", action="store_true")
    s.add_argument("--why")
    s.set_defaults(fn=verb_close)
    return p


def main(argv=None):
    parser = build_parser()
    args = parser.parse_args(argv)
    store = Store(find_repo_root(args.repo))
    try:
        result = args.fn(store, args)
    except Refusal as r:
        err = {"ok": False, "error": r.slug, "message": r.message}
        err.update(r.extra)
        print(json.dumps(err, indent=2), file=sys.stderr)
        return r.exit_code
    print(json.dumps(result, indent=2 if args.verb in ("status", "counts", "validate", "migrate") else None))
    return 0 if result.get("ok", True) else 1


if __name__ == "__main__":
    sys.exit(main())
