#!/usr/bin/env python3
"""orchestrate state tool.

The orchestrator's `.orchestrator/state.json` is written only through this script.
It owns the schema (see ../state.md), the task status transitions, the timestamps,
the derived counts, the gate evaluation, the handshake assembly and the cycle
archive. The LLM supplies judgment as short arguments; everything mechanical is here.

Every verb prints compact JSON on stdout. A refusal prints
{"ok": false, "error": "<slug>", "message": "..."} on stderr and exits 1
(exit 2 when no state file exists). Writes are load -> validate -> mutate ->
validate -> atomic replace, under a file lock.

Usage: python3 state.py [--home PATH] <verb> ...   (state.py help lists the verbs)

The home is the directory holding .claude/orchestrator/config.yaml (found by walking up
from cwd); it holds .orchestrator/ and .tasks/. The code root is where worktrees, merges
and tests happen, and it holds the record (docs/specs/, docs/changes/<change>/):
config.yaml `code-root: <path>` relative to the home, default the home. `where` prints both.
"""

import argparse
import difflib
import fcntl
import glob
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import time
from datetime import datetime, timezone

SCHEMA_VERSION = "1.5"
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
    "created_at": (str, type(None)),
}
# Fields added to TASK_FIELDS within schema 1.5, after state files of that version existed. A file
# without one gets the default on read (validate fills it; the next write persists it), so no
# migration and no schema bump. The record: created_at joined the schema and every write to an open
# change's state was refused as invalid until the nine tasks were given the key by hand.
TASK_FIELDS_ADDED_IN_VERSION = {"created_at": None}

TASK_DEFAULTS = {
    "on_critical_path": False, "worktree_path": None, "branch_name": None, "agent_task_id": None,
    "status": "ready", "merge_commit": None, "after_snapshot": None, "regression_detected": False,
    "worktree_removed": False,
    "review_mode": None, "findings_path": None, "findings_count": None, "followups_created": [],
    "dependents_repointed": [], "implementor_report_path": None, "discovered_from": None,
    "discovered_by": None, "discovered_class": None,
    "cites_seams": [], "blocked_by": [], "blocker_note": None, "rejections": 0,
    "started_at": None, "completed_at": None, "reviewed_at": None, "created_at": None,
}

# A finding's labels (templates/architect-finding.md): kind and severity are the reporter's,
# decoration is the orchestrator's routing. A `thought` is a journal line, never a finding.
FINDING_FIELDS = {
    "finding_id": str, "trigger": str, "kind": str, "severity": (str, type(None)),
    "class": (str, type(None)), "title": str,
    "path": str, "locations": list, "why_tests_missed": (str, type(None)),
    "discovered_from": (str, type(None)), "seam": (str, type(None)), "evidence": (str, type(None)),
    "decoration": (str, type(None)),
    "resolution": str, "blocking_merge_until_resolved": bool,
}
FINDING_DEFAULTS = {"severity": None, "class": None, "locations": [], "why_tests_missed": None,
                    "discovered_from": None, "seam": None, "evidence": None, "decoration": None,
                    "resolution": "pending", "blocking_merge_until_resolved": False}
TRIGGERS = ["premise-check", "conformance"]
KINDS = ["issue", "question"]
SEVERITIES = ["high", "medium", "low"]
DECORATIONS = ["blocking", "hardening"]
LEGACY_SEVERITIES = ["blocking", "advisory", "informational", "spec-signal"]  # schema <= 1.4
FINDING_CLASSES = ["shape-fragmentation", "vocabulary-mismatch", "responsibility-leakage",
                   "dead-branch", "interface-drift", "mutation", "invariant-gap", "duplication"]

# An agent's setting (SKILL.md § Agent settings): one entry per spawned agent, written by its
# spawner with the id the Agent call returned; `counts` joins it to the agent's transcript for
# spend. `cycle` is the cycle the agent worked in, null before the first one. The record: no
# cycle file held a model or an effort; only the transcripts did (research agent-model-effort, E3).
AGENT_ROLES = ["phase-open", "phase-design", "phase-plan", "phase-execute", "phase-integrate",
               "phase-inline", "architect-design", "architect-premise", "architect-conformance",
               "implementor", "implementor-rework", "reviewer-task", "reviewer-whole-change", "pm"]
AGENT_MODELS = ["fable", "opus", "sonnet", "haiku"]
AGENT_EFFORTS = ["low", "medium", "high", "xhigh", "max"]
AGENT_SOURCES = ["role-default", "task-override"]
AGENT_FIELDS = {"agent_id": str, "role": str, "task": (str, type(None)), "cycle": (str, type(None)),
                "model": str, "effort": str, "source": str, "reason": str, "recorded_at": str}
AGENT_DEFAULTS = {"task": None, "cycle": None, "source": "role-default", "reason": "role default"}
# Per MTok at list price: input, output, cache read, 5-minute cache write. Haiku has a second
# price above 100k tokens of prompt. Source: research agent-model-effort, E1.
MODEL_PRICES = {"fable": (10, 50, 0.25, 12.5), "opus": (4, 20, 0.20, 5), "sonnet": (2, 10, 0.10, 2.5),
                "haiku": (0.10, 0.50, 0.01, 0.125)}
HAIKU_LONG_PROMPT_PRICES = (0.50, 2.50, 0.05, 0.625)

# An ask is one org file (templates/ask.md); state.json keeps only this index entry.
ASK_FIELDS = {"id": str, "kind": str, "status": str, "blocks": list, "path": str}
ASK_DEFAULTS = {"blocks": [], "status": "open"}
# The rest of the record lives in the file; accepted on input to `record add` / `record set`.
ASK_FILE_FIELDS = {
    "raised_by": dict, "question": str, "about": (str, type(None)),
    "observed": (dict, type(None)), "options": list, "recommendation": (dict, type(None)),
    "default_if_unanswered": (str, type(None)),
    "decision": (str, type(None)), "decision_readback": (str, type(None)),
    "applied_via": (str, type(None)), "revised_from": (str, type(None)),
}
ASK_FILE_DEFAULTS = {"about": None, "observed": None, "options": [], "recommendation": None,
                     "default_if_unanswered": None, "decision": None, "decision_readback": None,
                     "applied_via": None, "revised_from": None}
ASK_KINDS = ["decision", "doc-correction", "process", "environment", "confirmation"]
ASK_STATUSES = ["open", "answered", "applied", "deferred", "declined", "superseded"]
ASK_TERMINAL = {"applied", "declined", "superseded"}
ASK_ANSWERED = {"answered", "applied", "declined", "deferred"}  # statuses that need an answer
GOAL_DRIFT_OPTIONS = {"revise", "split", "abandon", "continue"}
ORCHESTRATOR_GITIGNORE = """# Written by state.py init. Test outputs and scratch stay out of git; every other
# orchestration artifact (inbox, cycles, state.json, handshakes) is tracked.
baseline-*.txt
after-*.txt
*.tmp
state.lock
state.json.pre-migrate-*
"""

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
                      "task_refinements", "candidate_tasks"]
# candidate_tasks element: a task the next plan drafts (execute § 5, § 8 and integrate § 7 route,
# plan § 2 writes). `critical` is the route: blocking on the main path.
CANDIDATE_FIELDS = {"name": str, "source": str, "discovered_by": str, "discovered_class": str,
                    "critical": bool, "quote": str, "evidence": str, "placement": str}
CANDIDATE_REQUIRED = ["name", "source", "discovered_by", "critical", "quote"]
CANDIDATE_PLACEMENTS = ["in-change", ".tasks"]
# design_diff element: {"seam": "<seam id>", "change": added|amended|retired, "ref": "<finding id>"}
DESIGN_DIFF_CHANGES = ["added", "amended", "retired"]

TOP_LEVEL = {
    "schema_version": str, "session_id": str, "cycle_id": str, "phase": str, "home": str,
    "code_root": str,
    "change_name": str, "baseline_snapshot": (str, type(None)), "baseline_status": (int, type(None)),
    "current_branch": str, "test_command": str, "history_window": int,
    "prior_handshake": (str, type(None)), "closed_at": (str, type(None)),
    "tasks": list, "architect_findings": list, "asks_for_user": list, "agents": list,
    "journal": list, "cycle_log": dict, "phase_gates": dict,
}
# Top-level keys added within schema 1.5, filled on read like TASK_FIELDS_ADDED_IN_VERSION.
TOP_LEVEL_ADDED_IN_VERSION = {"agents": []}
TOP_LEVEL_SETTABLE = {"baseline_snapshot", "baseline_status", "current_branch", "test_command"}
LEGACY_ALIASES = {"prior_handshake_path": "prior_handshake", "repo_root": "home"}


class Refusal(Exception):
    def __init__(self, slug, message, exit_code=1, **extra):
        super().__init__(message)
        self.slug, self.message, self.exit_code, self.extra = slug, message, exit_code, extra


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


# ---------------------------------------------------------------- location

def find_home(explicit=None):
    """(home, found). The nearest ancestor of cwd with .claude/orchestrator/config.yaml, else
    with .orchestrator/ holding a change or a legacy state.json; (cwd, False) when neither exists."""
    if explicit:
        return os.path.abspath(explicit), True
    d = os.getcwd()
    while True:
        if os.path.exists(os.path.join(d, ".claude", "orchestrator", "config.yaml")):
            return d, True
        if os.path.exists(os.path.join(d, ".orchestrator", "state.json")) \
                or glob.glob(os.path.join(d, ".orchestrator", "changes", "*", "change.json")):
            return d, True
        parent = os.path.dirname(d)
        if parent == d:
            return os.getcwd(), False
        d = parent


def config_path(home):
    return os.path.join(home, ".claude", "orchestrator", "config.yaml")


def _yaml_scalar(raw):
    raw = raw.strip()
    if len(raw) >= 2 and raw[0] in "\"'" and raw[-1] == raw[0]:
        return raw[1:-1]
    raw = re.split(r"\s+#", raw)[0].strip()
    return "" if raw.startswith("#") else raw


def parse_yaml_subset(text):
    """The overlay's YAML, without a YAML library: nested maps, scalars, lists of scalars and
    lists of one-key maps (`- <module>: <note>`), flow lists `[a, b]`, comments. Anything
    stranger is kept as the string it was. Enough for config.yaml; never for state."""
    lines = []
    for raw in text.split("\n"):
        if not raw.strip() or raw.lstrip().startswith("#"):
            continue
        lines.append((len(raw) - len(raw.lstrip(" ")), raw.strip()))
    pos = [0]

    def key_of(content):
        if content[:1] in "\"'":
            return None
        m = re.match(r"^([^:#]+?):(?:\s+(.*))?$", content)
        return (m.group(1).strip().strip("\"'"), m.group(2) or "") if m else None

    def mapping(indent):
        out = {}
        while pos[0] < len(lines):
            ind, content = lines[pos[0]]
            if ind < indent or (ind == indent and content.startswith("- ")):
                break
            if ind > indent:
                pos[0] += 1
                continue
            kv = key_of(content)
            pos[0] += 1
            if not kv:
                continue
            key, rest = kv
            val = _yaml_scalar(rest)
            if val == "":
                if pos[0] < len(lines) and lines[pos[0]][0] > indent:
                    out[key] = block(lines[pos[0]][0])
                elif pos[0] < len(lines) and lines[pos[0]][0] == indent and lines[pos[0]][1].startswith("- "):
                    out[key] = seq(indent)
                else:
                    out[key] = None
            elif val.startswith("[") and val.endswith("]"):
                out[key] = [_yaml_scalar(x) for x in val[1:-1].split(",") if x.strip()]
            else:
                out[key] = val
        return out

    def seq(indent):
        out = []
        while pos[0] < len(lines):
            ind, content = lines[pos[0]]
            if ind != indent or not content.startswith("- "):
                break
            item = content[2:].strip()
            pos[0] += 1
            kv = key_of(item)
            if kv:
                d = {kv[0]: _yaml_scalar(kv[1])}
                if pos[0] < len(lines) and lines[pos[0]][0] > indent:
                    d.update(mapping(lines[pos[0]][0]))
                out.append(d)
            else:
                out.append(_yaml_scalar(item))
        return out

    def block(indent):
        return seq(indent) if lines[pos[0]][1].startswith("- ") else mapping(indent)

    return mapping(lines[0][0]) if lines else {}


def read_config(home):
    """config.yaml as a dict; {} when the home has none."""
    p = config_path(home)
    if not os.path.exists(p):
        return {}
    return parse_yaml_subset(open(p, encoding="utf-8").read())


def read_code_root(home):
    """(raw value, absolute path) of config.yaml's top-level `code-root:`, the one-repository
    shorthand; (None, None) when absent or empty. A list is refused: several repositories go
    under `repos:` (overlay.md § Repositories)."""
    p = config_path(home)
    if not os.path.exists(p):
        return None, None
    lines = open(p, encoding="utf-8").read().split("\n")
    for i, line in enumerate(lines):
        m = re.match(r"^code-root:\s*(.*?)\s*$", line)
        if not m:
            continue
        raw = re.split(r"\s+#", m.group(1))[0].strip()
        if raw.startswith("#"):
            raw = ""
        nxt = lines[i + 1] if i + 1 < len(lines) else ""
        if raw.startswith("[") or (raw == "" and re.match(r"^\s*-\s", nxt)):
            raise Refusal("bad_code_root", "code-root takes one path, the one-repository shorthand; "
                          "several repositories are named under repos: (overlay.md § Repositories)")
        raw = raw.strip("\"'")
        if not raw:
            return None, None
        return raw, os.path.abspath(os.path.join(home, os.path.expanduser(raw)))
    return None, None


WORKTREE_PLACEHOLDERS = ("<repo>", "<branch>")


def resolve_home_config(home, cfg=None):
    """The home's shape and its repositories, from config.yaml (overlay.md § Repositories):
    {shape: workspace | code-root | repo-root, repos: {name: {path, from_config, language, build,
    test, worktree}}, worktrees: {parent, from_config, pattern}, code_root_raw, problems}.
    `repos:` names each repository; `code-root:` is the one-repository shorthand; neither means
    the home is the repository. Worktrees are placed from the home, never from a code root."""
    cfg = read_config(home) if cfg is None else cfg
    problems = []

    def absolute(raw):
        return os.path.abspath(os.path.join(home, os.path.expanduser(str(raw))))

    def merged(spec):
        out = {}
        for k in ("build", "test", "worktree"):
            base = cfg.get(k) if isinstance(cfg.get(k), dict) else {}
            own = spec.get(k) if isinstance(spec.get(k), dict) else {}
            out[k] = dict(base, **own)
        return out

    repos, code_root_raw = {}, None
    if cfg.get("repos") is not None:
        shape = "workspace"
        block = cfg.get("repos")
        if not isinstance(block, dict) or not block:
            problems.append(("bad_repos", "repos: is a map, <name>: {path: <dir>, ...} (overlay.md § Repositories)"))
        else:
            for name, spec in block.items():
                spec = spec if isinstance(spec, dict) else {}
                if not spec.get("path"):
                    problems.append(("bad_repo", f"repos.{name} needs path: <dir>, relative to the home"))
                    continue
                repos[name] = {"path": absolute(spec["path"]), "from_config": spec["path"],
                               "language": spec.get("language") or cfg.get("language"), **merged(spec)}
    else:
        code_root_raw, path = read_code_root(home)
        shape = "code-root" if path else "repo-root"
        path = path or home
        name = os.path.basename(os.path.realpath(path)) or "repo"
        repos[name] = {"path": path, "from_config": code_root_raw, "language": cfg.get("language"), **merged({})}
    wt = cfg.get("worktrees") if isinstance(cfg.get("worktrees"), dict) else {}
    parent_raw = wt.get("parent") or ".worktrees"
    pattern = wt.get("pattern") or ("<repo>/<branch>" if shape == "workspace" else "<branch>")
    if "<branch>" not in pattern:
        problems.append(("bad_worktree_pattern", f"worktrees.pattern {pattern!r} must contain <branch>"))
    if len(repos) > 1 and "<repo>" not in pattern:
        problems.append(("bad_worktree_pattern", f"worktrees.pattern {pattern!r} must contain <repo> when the "
                         "home has several repositories (two of them may both have a main)"))
    return {"shape": shape, "repos": repos, "code_root_raw": code_root_raw,
            "worktrees": {"parent": absolute(parent_raw), "from_config": parent_raw, "pattern": pattern},
            "problems": problems}


def worktree_path_for(hc, repo, branch):
    rel = hc["worktrees"]["pattern"].replace("<repo>", repo).replace("<branch>", branch)
    return os.path.normpath(os.path.join(hc["worktrees"]["parent"], rel))


def test_compare_of(store):
    """The change's repository `test.compare` command, or None."""
    repo = store.hc["repos"].get(store.repo or "") if store.hc else None
    val = (repo or {}).get("test", {}).get("compare") if repo else None
    return val or None


def read_test_compare(home):
    """The overlay's top-level `test.compare` command, or None (the one-repository shorthands)."""
    hc = resolve_home_config(home)
    for r in hc["repos"].values():
        return r["test"].get("compare") or None
    return None


def red_baseline_warnings(store, baseline_status, which="baseline_status"):
    """A red baseline with no `test.compare` leaves the regression check unable to decide
    (flows/execute.md § 4); the agent must raise the environment ask before the first merge."""
    if not baseline_status or test_compare_of(store):
        return []
    return [f"{which}={baseline_status} and the overlay has no test.compare: the exit code "
            "cannot decide a regression; set test.compare or raise the environment ask before the "
            "first merge (flows/execute.md § 4)"]


def _same_path(a, b):
    return os.path.realpath(a) == os.path.realpath(b)


# ---------------------------------------------------------------- changes

def changes_root(home):
    return os.path.join(home, ".orchestrator", "changes")


def change_json_path(home, name):
    return os.path.join(changes_root(home), name, "change.json")


def read_change(home, name):
    p = change_json_path(home, name)
    if not os.path.exists(p):
        return None
    try:
        rec = json.load(open(p, encoding="utf-8"))
    except (json.JSONDecodeError, OSError):
        return None
    return rec if isinstance(rec, dict) and rec.get("change") == name else None


def list_changes(home):
    out = []
    for p in sorted(glob.glob(os.path.join(changes_root(home), "*", "change.json"))):
        rec = read_change(home, os.path.basename(os.path.dirname(p)))
        if rec:
            out.append(rec)
    return out


def _under(path, parent):
    try:
        rp, pp = os.path.realpath(path), os.path.realpath(parent)
    except OSError:
        return False
    return rp == pp or rp.startswith(pp.rstrip(os.sep) + os.sep)


def resolve_change(home, explicit=None):
    """(name, how). The change named by --change or $ORCH_CHANGE; else the open change whose
    worktree holds the cwd; else the only open change; else (None, None)."""
    if explicit:
        return explicit, "flag"
    env = os.environ.get("ORCH_CHANGE")
    if env:
        return env, "env"
    changes = list_changes(home)
    cwd = os.getcwd()
    for rec in changes:
        if rec.get("worktree") and not _same_path(rec["worktree"], home) and _under(cwd, rec["worktree"]):
            return rec["change"], "cwd"
    if len(changes) == 1:
        return changes[0]["change"], "only"
    return None, None


class Store:
    def __init__(self, home, change=None, code_root=None, repo=None):
        self.home = home
        self.change = change
        self.repo = repo
        self.code_root = code_root or home
        self.split = not _same_path(self.code_root, self.home)
        self.home_found = True
        self.code_root_raw = None
        self.hc = None
        self.change_rec = None
        self.root = os.path.join(home, ".orchestrator")
        self.dir = os.path.join(self.root, "changes", change) if change else self.root
        self.path = os.path.join(self.dir, "state.json")
        self.lock_path = os.path.join(self.dir, "state.lock")

    def legacy_layout(self):
        """A state.json at .orchestrator/ itself: the one-change layout before changes/<change>/."""
        return os.path.exists(os.path.join(self.root, "state.json"))

    def exists(self):
        return os.path.exists(self.path)

    def load(self):
        if not self.exists():
            if self.change is None and self.legacy_layout():
                raise Refusal("legacy_layout", f"{self.root}/state.json is the one-change layout; run "
                              "`migrate` to move it under .orchestrator/changes/<change>/", exit_code=2)
            raise Refusal("not_initialized", f"no state file at {self.path}; run `init`", exit_code=2)
        with open(self.path) as f:
            state = json.load(f)
        if isinstance(state, dict):  # a field added to a record schema later defaults on read
            for k, v in TOP_LEVEL_ADDED_IN_VERSION.items():
                state.setdefault(k, list(v))
            for key, _fields, defaults, _id in RECORD_LISTS.values():
                for rec in state.get(key, []) or []:
                    if isinstance(rec, dict):
                        for k, v in defaults.items():
                            rec.setdefault(k, v)
        return state

    def write(self, state):
        os.makedirs(self.dir, exist_ok=True)
        _atomic_write(self.path, json.dumps(state, indent=2) + "\n")

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


def _atomic_write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        f.write(text)
        f.flush()
        os.fsync(f.fileno())
    os.replace(tmp, path)


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
    """An ask's observed = {happened: bool, evidence: str}; None means not filled."""
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
    for k, v in TOP_LEVEL_ADDED_IN_VERSION.items():
        state.setdefault(k, list(v))
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
        if isinstance(t, dict):
            for k, v in TASK_FIELDS_ADDED_IN_VERSION.items():
                t.setdefault(k, v)
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
            _enum(f.get("kind"), KINDS, f"{where}.kind", errors)
            _enum(f.get("severity"), SEVERITIES, f"{where}.severity", errors)
            _enum(f.get("decoration"), DECORATIONS, f"{where}.decoration", errors)
            _enum(f.get("class"), FINDING_CLASSES, f"{where}.class", errors)
    for i, a in enumerate(state["asks_for_user"]):
        where = f"asks_for_user[{i}]"
        _validate_record(a, ASK_FIELDS, where, errors, "id")
        if isinstance(a, dict):
            _enum(a.get("kind"), ASK_KINDS, f"{where}.kind", errors)
            _enum(a.get("status"), ASK_STATUSES, f"{where}.status", errors)
    for i, a in enumerate(state["agents"]):
        _validate_agent(a, f"agents[{i}]", errors)
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


def _validate_agent(a, where, errors):
    _validate_record(a, AGENT_FIELDS, where, errors, "agent_id")
    if not isinstance(a, dict):
        return
    _enum(a.get("role"), AGENT_ROLES, f"{where}.role", errors)
    _enum(a.get("model"), AGENT_MODELS, f"{where}.model", errors)
    _enum(a.get("effort"), AGENT_EFFORTS, f"{where}.effort", errors)
    _enum(a.get("source"), AGENT_SOURCES, f"{where}.source", errors)
    if isinstance(a.get("reason"), str) and not a["reason"].strip():
        errors.append(f"{where}.reason: empty; a role default's reason is `role default`")
    if a.get("source") == "task-override" and not a.get("task"):
        errors.append(f"{where}: a task override names its task")
    if a.get("model") == "haiku":
        errors.append(f"{where}: haiku runs read-only helpers only, never a role that writes or "
                      "judges code (SKILL.md § Agent settings)")


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


def _arg_path(raw_path, home=None):
    """An @file path: relative to cwd when that exists, else relative to the home."""
    if home and not os.path.isabs(raw_path) and not os.path.exists(raw_path):
        alt = os.path.join(home, raw_path)
        if os.path.exists(alt):
            return alt
    return raw_path


def load_json_arg(raw, home=None):
    """A JSON literal, or @path to a JSON file (cwd-relative, else home-relative)."""
    if raw.startswith("@"):
        path = _arg_path(raw[1:], home)
        if not os.path.exists(path):
            raise Refusal("no_such_file", f"{raw[1:]} does not exist (looked in cwd"
                          + (f" and {home}" if home else "") + ")")
        with open(path) as f:
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


# ---------------------------------------------------------------- ask files (org)
#
# One ask is one org file: a file-level :PROPERTIES: drawer (the tool's), #+TITLE: (the
# question), level-1 sections. The tool edits line by line so everything a person wrote
# survives: unknown sections, ** subheadings, re-wrapped prose, reordered sections.

ASK_SECTIONS = ["About", "What happened", "Options", "Recommendation", "If unanswered",
                "Answer", "Readback", "Discussion"]
ASK_SECTION_SYNONYMS = {"observed": "What happened", "default": "If unanswered", "decision": "Answer"}
ASK_PROPS = ["ID", "KIND", "STATUS", "RAISED_BY", "REF", "HAPPENED", "BLOCKS", "APPLIED_VIA", "REVISED_FROM"]
ASK_ANSWER_HINT = ("# An option label from above, or your own words; everything below this line "
                   "is read as your answer.")
ASK_ID_RE = re.compile(r"\bask-[A-Za-z0-9-]*\d\b")
ORG_COMMENT_RE = re.compile(r"\s*#(\s|$)")


def _canon_section(name):
    key = re.sub(r"\s+", " ", name.strip()).lower()
    for s in ASK_SECTIONS:
        if key == s.lower():
            return s
    return ASK_SECTION_SYNONYMS.get(key)


class AskDoc:
    """An ask file as lines plus indices into them."""

    def __init__(self, text=""):
        self.lines = text.lstrip("﻿").replace("\r\n", "\n").split("\n")
        self._index()

    def _index(self):
        self.drawer, self.props, self.title_line, self.sections = None, {}, None, []
        for i, l in enumerate(self.lines):
            s = l.strip()
            if s.upper() == ":PROPERTIES:":
                for j in range(i + 1, len(self.lines)):
                    sj = self.lines[j].strip()
                    if sj.upper() == ":END:":
                        self.drawer = (i, j)
                        break
                    m = re.fullmatch(r":([A-Za-z_]+):\s*(.*)", sj)
                    if m:
                        self.props[m.group(1).upper()] = j
                break
            if s.startswith("* "):
                break
        for i, l in enumerate(self.lines):
            if self.title_line is None and re.match(r"#\+title:", l, re.I):
                self.title_line = i
            if l.startswith("* "):
                self.sections.append((i, _canon_section(l[2:]), l[2:].strip()))

    def text(self):
        return "\n".join(self.lines).rstrip("\n") + "\n"

    # -- drawer
    def prop(self, key):
        i = self.props.get(key.upper())
        if i is None:
            return None
        v = re.fullmatch(r"\s*:[A-Za-z_]+:\s*(.*)", self.lines[i]).group(1).strip()
        return v or None

    def set_prop(self, key, value):
        key = key.upper()
        i = self.props.get(key)
        if value is None or value == "" or value == []:
            if i is not None and key in ("ID", "KIND", "STATUS"):
                self.lines[i] = f":{key}:"
            elif i is not None:
                del self.lines[i]
                self._index()
            return
        if isinstance(value, list):
            value = ", ".join(str(v) for v in value)
        line = f":{key}: {value}"
        if i is not None:
            self.lines[i] = line
            return
        if self.drawer is None:
            self.lines[0:0] = [":PROPERTIES:", line, ":END:"]
        else:
            order = ASK_PROPS.index(key) if key in ASK_PROPS else len(ASK_PROPS)
            pos = self.drawer[1]
            for k, li in self.props.items():
                if (ASK_PROPS.index(k) if k in ASK_PROPS else len(ASK_PROPS)) > order:
                    pos = min(pos, li)
            self.lines.insert(pos, line)
        self._index()

    # -- title
    def title(self):
        if self.title_line is None:
            return None
        return re.match(r"#\+title:\s*(.*)$", self.lines[self.title_line], re.I).group(1).strip() or None

    def set_title(self, value):
        line = f"#+TITLE: {value}"
        if self.title_line is not None:
            self.lines[self.title_line] = line
        else:
            self.lines.insert(self.drawer[1] + 1 if self.drawer else 0, line)
            self._index()

    # -- sections
    def _bounds(self, canon):
        for k, (i, c, _raw) in enumerate(self.sections):
            if c == canon:
                end = self.sections[k + 1][0] if k + 1 < len(self.sections) else len(self.lines)
                return i, end
        return None

    def section_lines(self, canon):
        b = self._bounds(canon)
        return self.lines[b[0] + 1:b[1]] if b else None

    def section_text(self, canon):
        ls = self.section_lines(canon)
        if ls is None:
            return None
        return "\n".join(l for l in ls if not ORG_COMMENT_RE.match(l)).strip() or None

    def set_section(self, canon, text):
        body = [""] + (text.split("\n") if text else []) + [""]
        b = self._bounds(canon)
        if b:
            self.lines[b[0] + 1:b[1]] = body
        else:
            order = ASK_SECTIONS.index(canon)
            pos = len(self.lines)
            for i, c, _raw in self.sections:
                if c is not None and ASK_SECTIONS.index(c) > order:
                    pos = i
                    break
            lead = [""] if pos > 0 and self.lines[pos - 1].strip() else []
            self.lines[pos:pos] = lead + [f"* {canon}"] + body
        self._index()


def _desc_items(lines):
    """`- label :: text` items; indented lines continue the previous item."""
    items = []
    for l in lines or []:
        m = re.match(r"\s*[-+]\s+(.+?)\s+::\s*(.*)$", l)
        if m:
            items.append([m.group(1).strip(), m.group(2).strip()])
        elif re.match(r"\s*[-+]\s+\S", l):
            items.append([re.sub(r"^\s*[-+]\s+", "", l).strip(), ""])
        elif items and re.match(r"\s+\S", l):
            items[-1][1] = (items[-1][1] + " " + l.strip()).strip()
    return items


def ask_to_org(rec):
    """A fresh file from a full record (index fields plus file fields)."""
    d = AskDoc("")
    d.lines = [":PROPERTIES:", f":ID: {rec['id']}", f":KIND: {rec['kind']}",
               f":STATUS: {rec.get('status') or 'open'}", ":END:"]
    d._index()
    rb = rec.get("raised_by") or {}
    d.set_prop("RAISED_BY", rb.get("role"))
    d.set_prop("REF", rb.get("ref"))
    obs = rec.get("observed")
    if isinstance(obs, dict):
        d.set_prop("HAPPENED", "yes" if obs.get("happened") else "no")
    d.set_prop("BLOCKS", rec.get("blocks") or [])
    d.set_prop("APPLIED_VIA", rec.get("applied_via"))
    d.set_prop("REVISED_FROM", rec.get("revised_from"))
    d.set_title(rec.get("question") or rec["id"])
    if rec.get("about"):
        d.set_section("About", rec["about"])
    if isinstance(obs, dict) and obs.get("evidence"):
        d.set_section("What happened", obs["evidence"])
    if rec.get("options"):
        d.set_section("Options", "\n".join(
            f"- {o.get('label')} :: {o.get('consequence') or ''}".rstrip(" :") for o in rec["options"]))
    r = rec.get("recommendation")
    if isinstance(r, dict) and r.get("option"):
        d.set_section("Recommendation", f"- {r['option']} :: {r.get('why') or ''}".rstrip(" :"))
    if rec.get("default_if_unanswered"):
        d.set_section("If unanswered", rec["default_if_unanswered"])
    d.set_section("Answer", rec["decision"] if rec.get("decision") else ASK_ANSWER_HINT)
    if rec.get("decision_readback"):
        d.set_section("Readback", rec["decision_readback"])
    return d.text()


def ask_from_org(doc, fallback_id=None):
    """The full record a file holds. Missing parts are None / empty."""
    opts = _desc_items(doc.section_lines("Options"))
    recs = _desc_items(doc.section_lines("Recommendation"))
    happened = doc.prop("HAPPENED")
    obs = None
    if happened is not None:
        obs = {"happened": happened.strip().lower() in ("yes", "true", "t", "1"),
               "evidence": doc.section_text("What happened") or ""}
    blocks = [b.strip() for b in (doc.prop("BLOCKS") or "").split(",") if b.strip()]
    return {
        "id": doc.prop("ID") or fallback_id, "kind": doc.prop("KIND"),
        "status": doc.prop("STATUS") or "open", "blocks": blocks,
        "raised_by": {"role": doc.prop("RAISED_BY") or "orchestrator", "ref": doc.prop("REF")},
        "question": doc.title(), "about": doc.section_text("About"), "observed": obs,
        "options": [{"label": l, "consequence": c} for l, c in opts],
        "recommendation": {"option": recs[0][0], "why": recs[0][1]} if recs else None,
        "default_if_unanswered": doc.section_text("If unanswered"),
        "decision": doc.section_text("Answer"), "decision_readback": doc.section_text("Readback"),
        "applied_via": doc.prop("APPLIED_VIA"), "revised_from": doc.prop("REVISED_FROM"),
    }


def _norm(s):
    return re.sub(r"\s+", " ", (s or "").casefold()).strip().rstrip(".!?;:,")


def analyse_answer(rec):
    """None when the Answer section is empty; else the text, the option it names and how surely."""
    text = rec.get("decision")
    if not text:
        return None
    labels = {_norm(o.get("label")): o.get("label") for o in rec.get("options") or [] if o.get("label")}
    nt = _norm(text)
    if nt in labels:
        return {"text": text, "label": labels[nt], "match": "exact"}
    close = difflib.get_close_matches(nt, list(labels), n=2, cutoff=0.8)
    if len(close) == 1:
        return {"text": text, "label": labels[close[0]], "match": "fuzzy"}
    return {"text": text, "label": None, "match": "none"}


def _answer_candidate(doc, rec):
    """An option label written as a line somewhere other than under * Answer."""
    labels = {_norm(o.get("label")) for o in rec.get("options") or [] if o.get("label")}
    if not labels:
        return None
    for k, (i, c, raw) in enumerate(doc.sections):
        if c in ASK_SECTIONS and c not in ("Discussion",):
            continue
        end = doc.sections[k + 1][0] if k + 1 < len(doc.sections) else len(doc.lines)
        for l in doc.lines[i + 1:end]:
            if _norm(l) in labels:
                return {"section": raw, "text": l.strip()}
    return None


def validate_ask_full(rec, where, errors):
    """A full ask record (index + file fields) as `record add` / `record set` receive it."""
    fields = {**ASK_FIELDS, **ASK_FILE_FIELDS}
    if not isinstance(rec, dict):
        errors.append(f"{where}: not an object")
        return
    for k in rec:
        if k not in fields:
            errors.append(f"{where}.{k}: unknown field (allowed: {', '.join(fields)})")
    for k, typ in fields.items():
        if k == "path":
            continue
        if k not in rec:
            errors.append(f"{where}.{k}: missing")
        elif not _check_type(rec[k], typ):
            errors.append(f"{where}.{k}: expected {_tname(typ)}, got {type(rec[k]).__name__}")
    for k in ("id", "kind", "status", "applied_via", "revised_from", "question", "decision_readback"):
        v = rec.get(k)
        cap = LONG_TEXT_CAP if k in LONG_TEXT_FIELDS else TEXT_CAP
        if isinstance(v, str) and len(v) > cap:
            errors.append(f"{where}.{k}: {len(v)} chars exceeds the {cap}-char cap")
    q = rec.get("question")
    if isinstance(q, str) and (not q.strip() or "\n" in q.strip()):
        errors.append(f"{where}.question: one non-empty line (it is the file's #+TITLE)")
    _enum(rec.get("kind"), ASK_KINDS, f"{where}.kind", errors)
    _enum(rec.get("status"), ASK_STATUSES, f"{where}.status", errors)
    if isinstance(rec.get("observed"), dict):
        _validate_observed(rec["observed"], f"{where}.observed", errors)
    opts = rec.get("options") if isinstance(rec.get("options"), list) else []
    for i, o in enumerate(opts):
        if not isinstance(o, dict) or not isinstance(o.get("label"), str) or not o["label"].strip():
            errors.append(f"{where}.options[{i}]: expected {{label, consequence}}")
    if rec.get("kind") == "decision" and rec.get("status") == "open":
        if not rec.get("about"):
            errors.append(f"{where}.about: a decision says what the thing is for (templates/ask.md)")
        if rec.get("observed") is None:
            errors.append(f"{where}.observed: a decision says whether it has happened")
        if not 2 <= len(opts) <= 4:
            errors.append(f"{where}.options: a decision offers two to four options, do-nothing last")
    if rec.get("status") in ASK_ANSWERED and not rec.get("decision"):
        errors.append(f"{where}.decision: status {rec.get('status')} needs an answer")
    if rec.get("status") == "applied" and not rec.get("applied_via"):
        errors.append(f"{where}.applied_via: status applied says what applied it")


def _cycle_of(ask_id, fallback):
    m = re.fullmatch(r"ask-(cycle-.+)-\d+", ask_id or "")
    return m.group(1) if m else fallback


def ask_path(ask_id, status, fallback_cycle, store=None):
    """Relative to the home, under the change's directory; a pure function of id and status."""
    pre = store_rel(store)
    if status in ASK_TERMINAL:
        return f"{pre}/cycles/{_cycle_of(ask_id, fallback_cycle)}/asks/{ask_id}.org"
    return f"{pre}/inbox/{ask_id}.org"


def store_rel(store):
    """The change's directory relative to the home (`.orchestrator/changes/<change>`)."""
    return os.path.relpath(store.dir, store.home) if store is not None else ".orchestrator"


def read_ask_file(store, rel):
    p = os.path.join(store.home, rel)
    if not os.path.exists(p):
        return None, None
    with open(p, encoding="utf-8") as f:
        doc = AskDoc(f.read())
    return doc, ask_from_org(doc, os.path.basename(rel)[:-4])


def _git(repo_root, *args):
    try:
        r = subprocess.run(["git", "-C", repo_root, *args], capture_output=True, text=True)
        return r.returncode, r.stdout
    except OSError:
        return None, ""


def _git_tracked(repo_root, rel_dir):
    rc, out = _git(repo_root, "ls-files", "--", rel_dir)
    return {l for l in out.split("\n") if l} if rc == 0 else set()


def _git_ignores(repo_root, rel):
    rc, _ = _git(repo_root, "check-ignore", "-q", "--", rel)
    return rc == 0


def _move_ask_file(store, old_rel, new_rel):
    src, dst = os.path.join(store.home, old_rel), os.path.join(store.home, new_rel)
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    rc, _ = _git(store.home, "ls-files", "--error-unmatch", "--", old_rel)
    if rc == 0:
        rc2, _ = _git(store.home, "mv", "--", old_rel, new_rel)
        if rc2 == 0 and os.path.exists(dst):
            return "git mv"
    os.replace(src, dst)
    return "rename"


def scan_inbox(store, state):
    """Every inbox file joined with the index, plus index entries whose file is wrong or gone."""
    index = {a["id"]: a for a in (state or {}).get("asks_for_user", [])}
    tracked = _git_tracked(store.home, ".orchestrator")
    asks, problems, seen = [], [], set()
    for p in sorted(glob.glob(os.path.join(store.dir, "inbox", "*.org"))):
        rel = os.path.relpath(p, store.home)
        fid = os.path.basename(p)[:-4]
        doc, rec = read_ask_file(store, rel)
        entry = index.get(rec["id"]) or index.get(fid)
        probs = []
        if rec["id"] != fid:
            probs.append(f"{rel}: :ID: {rec['id']} differs from the file name")
        if doc.drawer is None:
            probs.append(f"{rel}: no properties drawer (`record set` restores it from the index)")
        if entry is None:
            probs.append(f"{rel}: not in the index (`init` carries it into the next cycle; "
                         f"`record add asks @{rel}` indexes it now)")
        else:
            seen.add(entry["id"])
            if doc.drawer is not None and rec["status"] != entry["status"]:
                probs.append(f"{rel}: :STATUS: {rec['status']} but the index says {entry['status']}; "
                             f"`record set asks {entry['id']} status={rec['status']}` reconciles")
            if entry["status"] in ASK_TERMINAL:
                probs.append(f"{rel}: status {entry['status']} belongs under cycles/<id>/asks/")
            if entry["path"] != rel:
                probs.append(f"{rel}: the index points at {entry['path']}")
        if not rec["question"]:
            probs.append(f"{rel}: no #+TITLE")
        elif len(rec["question"]) > LONG_TEXT_CAP:
            probs.append(f"{rel}: #+TITLE is {len(rec['question'])} chars; cap {LONG_TEXT_CAP}")
        status = entry["status"] if entry else rec["status"]
        answer = analyse_answer(rec)
        readback = rec.get("decision_readback")
        rb_lines = [l for l in (doc.section_lines("Readback") or []) if l.strip()]
        asks.append({
            "id": entry["id"] if entry else rec["id"], "kind": rec["kind"] or (entry or {}).get("kind"),
            "status": status, "blocks": (entry or rec)["blocks"], "path": rel,
            "indexed": entry is not None, "tracked": rel in tracked,
            "question": rec["question"], "answer": answer,
            "readback": {"present": bool(readback),
                         "confirmed": any(_norm(l) in ("yes", "confirmed", "ok") for l in rb_lines)},
            "mentions": sorted({m for m in ASK_ID_RE.findall(rec.get("decision") or "")
                                if m != (entry or rec)["id"]}),
            "answer_candidate": None if answer else _answer_candidate(doc, rec),
            "problems": probs,
        })
        problems += probs
    for a in index.values():
        if a["id"] in seen:
            continue
        if not os.path.exists(os.path.join(store.home, a["path"])):
            problems.append(f"{a['id']}: file {a['path']} is missing (restore it from git)")
        elif a["status"] not in ASK_TERMINAL:
            problems.append(f"{a['id']}: status {a['status']} but its file is not in the inbox")
    return asks, problems


# ---------------------------------------------------------------- counts

def derive_counts(tasks, journal=(), earlier=frozenset()):
    """`earlier` is the task names an earlier cycle of the change held: a task carried in from the
    prior integrate is `carried`, not `created` again. The record: drainage read 0.5-0.71 and fired
    throughput inversion while the queue shrank, every carried task counted as created twice."""
    def n(pred):
        return sum(1 for t in tasks if pred(t))
    main_index = {s: i for i, s in enumerate(MAIN_LINE)}
    started = n(lambda t: t.get("started_at") or main_index.get(t["status"], 0) >= 2
               or t["status"] in ("failed",))
    completed = n(lambda t: t.get("completed_at") or main_index.get(t["status"], 0) >= 3)
    reviewed = n(lambda t: t.get("reviewed_at") or main_index.get(t["status"], 0) >= 5)
    carried = n(lambda t: t["task_name"] in earlier)
    counts = {
        "created": len(tasks) - carried,
        "carried": carried,
        "started": started,
        "completed": completed,
        "reviewed": reviewed,
        "rejected": sum(t.get("rejections", 0) for t in tasks),
        # A review finding fixed by the orchestrator never sends the task back, so `rejected` misses
        # it (the record: a rejection rate of 0.00 beside 7 and then 9 inline fixes).
        "inline_fixes": sum(1 for e in journal if isinstance(e, dict) and e.get("kind") == "inline-fix"),
        "externalised": n(lambda t: t["status"] == "externalised"),
        "blocked": n(lambda t: t["status"] == "blocked"),
        "failed": n(lambda t: t["status"] == "failed"),
        "done": n(lambda t: t["status"] == "done"),
    }
    return counts


def _archived_states(store):
    """(cycle_id, state) of each archived cycle of the change, oldest first: by the cycle's start,
    then the number its id ends in, since a glob's order puts cycle-10 before cycle-2."""
    out = []
    for p in glob.glob(os.path.join(store.dir, "cycles", "*", "state.json")):
        try:
            s = json.load(open(p))
        except (json.JSONDecodeError, OSError):
            continue
        if isinstance(s, dict):
            out.append((s.get("cycle_id") or os.path.basename(os.path.dirname(p)), s))

    def order(item):
        cid, s = item
        log = s.get("cycle_log") if isinstance(s.get("cycle_log"), dict) else {}
        m = re.search(r"(\d+)$", cid)
        return (str(log.get("started_at") or ""), int(m.group(1)) if m else 0, cid)
    return sorted(out, key=order)


def _task_names(s):
    return {t.get("task_name") for t in s.get("tasks") or [] if isinstance(t, dict)}


def cycle_counts(store, state):
    """The counts of the current cycle, with the tasks earlier cycles held and the journal."""
    earlier = set()
    for cid, s in _archived_states(store):
        if cid != state["cycle_id"]:
            earlier |= _task_names(s)
    return derive_counts(state["tasks"], state.get("journal") or [], earlier)


def hardening_entries(store):
    """Entries of the change's hardening.org (templates/hardening.md): one `* <kind> (hardening):`
    heading each, whoever routed it. The record: the count read architect findings only and showed
    0 beside six entries the reviews had filed."""
    p = os.path.join(store.dir, "hardening.org")
    if not os.path.exists(p):
        return 0
    return sum(1 for line in open(p, encoding="utf-8") if re.match(r"^\* \S+ \(hardening\):", line))


def unbatched_open_tasks(store, state):
    """Task files in tasks/open/ that no batch of this cycle holds, with their frontmatter's
    on_critical_path. Done and externalised files are left out."""
    held = {t["task_name"] for t in state["tasks"]}
    out = []
    for p in sorted(glob.glob(os.path.join(store.dir, "tasks", "open", "*.md"))):
        text = open(p, encoding="utf-8").read()
        m = re.match(r"---\n(.*?)\n---", text, re.S)
        fm = m.group(1) if m else ""

        def key(k):
            km = re.search(rf"^{k}:[ \t]*([^\s#]*)", fm, re.M)
            return km.group(1) if km else None
        name = key("task_name") or os.path.basename(p)[:-3]
        if name in held or key("status") in ("done", "externalised"):
            continue
        out.append({"task": name, "critical": key("on_critical_path") == "true"})
    return out


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


def critical_path(tasks, unbatched=()):
    """`unbatched` is the open task files no batch holds; an on-path one counts in the total (the
    record: 8 on-path tasks never batched, "so state does not count them")."""
    on = [t for t in tasks if t.get("on_critical_path")]
    active = [t for t in tasks if t["status"] not in STOPPED]
    off_batch = [u["task"] for u in unbatched if u["critical"]]
    return {
        "on_path_total": len(on) + len(off_batch),
        "on_path_unbatched": off_batch,
        "on_path_done": sum(1 for t in on if t["status"] == "done"),
        "on_path_blocked": [t["task_name"] for t in on if t["status"] == "blocked"],
        "on_path_active": sum(1 for t in on if t["status"] not in STOPPED),
        "active_total": len(active),
    }


def read_thresholds(repo_root):
    """Only the thresholds the tool uses, from the overlay's `thresholds:` block (flat YAML)."""
    out = {"drainage_trigger_ratio": 1.0, "drainage_trigger_consecutive_cycles": 3,
           "review_starvation_ratio": 1.5, "cascade_trigger_followup_count": 3,
           "phase_agent_context": 400000}
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


def _all_agents(store, state, archived):
    """Every agent entry of the change, once each: the archived cycles', this state's, and those
    still waiting in agents-pending.json (after the change's last `close`, the final integrate
    phase agent waits there for an `init` that never comes)."""
    out, seen = [], set()
    lists = [s.get("agents") or [] for _cid, s in archived]
    lists += [state["agents"], _read_pending_agents(store)]
    for recs in lists:
        for a in recs:
            if isinstance(a, dict) and a.get("agent_id") not in seen:
                seen.add(a.get("agent_id"))
                out.append(a)
    return out


def quality_signals(state, agents, archived):
    """Reviewer rejections, rework tasks and defects found after done, in total and by the agent
    setting of each task's Implementor, so a cut in spend is read beside them (research
    agent-model-effort, R2). A defect found after done is a conformance finding of kind `issue`; a
    task the whole-change review raised (`discovered_from: whole-change-<cycle-id>`); or a follow-up
    of a task already done, not raised by an Implementor and not one of a review's own follow-ups
    (`followups_created` in any cycle: one batched a cycle after its review is still the review's)."""
    done_before, own_review = set(), set()
    before = True
    for cid, s in archived:
        before = before and cid != state["cycle_id"]
        for t in s.get("tasks") or []:
            if not isinstance(t, dict):
                continue
            own_review |= set(t.get("followups_created") or [])
            if before and t.get("status") == "done":
                done_before.add(t.get("task_name"))
    tasks = {t["task_name"]: t for t in state["tasks"]}
    own_review |= {f for t in state["tasks"] for f in t.get("followups_created") or []}
    setting = {}
    for a in agents:
        if a["role"] == "implementor" and a.get("task"):
            setting.setdefault(a["task"], f"{a['model']}/{a['effort']}")
    reworked = {a["task"] for a in state.get("agents") or []
                if a.get("role") == "implementor-rework" and a.get("task")}
    rework = sorted(n for n, t in tasks.items() if t.get("rejections") or n in reworked)
    defects = []
    for f in state["architect_findings"]:
        if f.get("trigger") == "conformance" and f.get("kind") == "issue":
            src = f.get("discovered_from")
            defects.append({"source": f["finding_id"],
                            "against": src if src in tasks or src in done_before else None})
    for t in state["tasks"]:
        src = t.get("discovered_from") or ""
        if src.startswith("whole-change"):
            defects.append({"source": t["task_name"], "against": None})
        elif (t.get("discovered_by") != "implementor" and t["task_name"] not in own_review
              and (src in done_before or (src in tasks and tasks[src]["status"] == "done"))):
            defects.append({"source": t["task_name"], "against": src})
    by_setting = {}

    def bucket(key):
        return by_setting.setdefault(key, {"tasks": 0, "rejections": 0, "rework_tasks": 0,
                                           "defects_after_done": 0})
    for n, t in tasks.items():
        b = bucket(setting.get(n, "unrecorded"))
        b["tasks"] += 1
        b["rejections"] += t.get("rejections", 0)
        b["rework_tasks"] += n in rework
    for d in defects:
        bucket(setting.get(d["against"], "unrecorded") if d["against"] else "whole change")[
            "defects_after_done"] += 1
    return {"rejections": sum(t.get("rejections", 0) for t in tasks.values()),
            "rework_tasks": rework, "defects_after_done": defects, "by_implementor_setting": by_setting}


def agent_signals(store, state):
    """Spend by role for this cycle's agents, with the quality signals, and the same per cycle of
    the history window: the readout R2 is judged on against E3's numbers (templates/pm-digest.md
    § Agents). An agent counts in the cycle it worked in (`cycle`); one recorded before the first
    cycle counts under `before-first-cycle`. The archives are read once and each transcript once."""
    archived = _archived_states(store)
    agents = _all_agents(store, state, archived)
    window = [h["cycle_id"] for h in state["cycle_log"].get("history", [])]
    wanted = {state["cycle_id"], None, *window}
    spend = {a["agent_id"]: agent_spend(a["agent_id"]) for a in agents if a.get("cycle") in wanted}
    cur = spend_by_role([a for a in agents if a.get("cycle") == state["cycle_id"]], spend)
    cur["quality"] = quality_signals(state, agents, archived)
    by_cycle = dict(archived)
    history = []
    for cid in [None] + window:
        group = [a for a in agents if a.get("cycle") == cid]
        if not group:
            continue
        s = spend_by_role(group, spend)
        row = {"cycle_id": cid or "before-first-cycle",
               "by_role": {r: {k: v[k] for k in ("agents", "cost_weight", "priced_weight", "usd",
                                                  "cost_weight_per_agent")}
                           for r, v in s["by_role"].items()}}
        if str((by_cycle.get(cid) or {}).get("schema_version")) == SCHEMA_VERSION:
            q = quality_signals(by_cycle[cid], agents, archived)
            row["quality"] = {"rejections": q["rejections"], "rework_tasks": len(q["rework_tasks"]),
                              "defects_after_done": len(q["defects_after_done"])}
        history.append(row)
    cur["history"] = history
    return cur


def pm_signals(state, store):
    counts = cycle_counts(store, state)
    r = ratios(counts)
    history = state["cycle_log"].get("history", [])
    th = read_thresholds(store.home)
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
    cp = critical_path(state["tasks"], unbatched_open_tasks(store, state))
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
        "issue": sum(1 for f in findings if f.get("kind") == "issue"),
        "question": sum(1 for f in findings if f.get("kind") == "question"),
        "blocking": sum(1 for f in findings if f.get("decoration") == "blocking"),
        "hardening_list": hardening_entries(store),
        "unrouted": sum(1 for f in findings if f.get("decoration") is None
                        and f.get("resolution") == "pending"),
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
        "agents": agent_signals(store, state),
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
    # One written before candidate_tasks existed (2026-10-25): its integrate wrote the task files.
    if prior and "candidate_tasks" not in h:
        h = {**h, "candidate_tasks": []}
    return [f"{path}: missing field {k}" for k in HANDSHAKE_REQUIRED if k not in h]


def _candidate_errors(cands, where):
    if not isinstance(cands, list):
        return [f"{where}: expected a JSON array"]
    errors, names = [], set()
    for i, e in enumerate(cands):
        w = f"{where}[{i}]"
        if not isinstance(e, dict):
            errors.append(f"{w}: not an object")
            continue
        for k in CANDIDATE_REQUIRED:
            if k not in e:
                errors.append(f"{w}.{k}: missing")
        for k, v in e.items():
            if k not in CANDIDATE_FIELDS:
                errors.append(f"{w}.{k}: unknown field (allowed: {', '.join(CANDIDATE_FIELDS)})")
            elif not isinstance(v, CANDIDATE_FIELDS[k]) or (CANDIDATE_FIELDS[k] is str and not v.strip()):
                errors.append(f"{w}.{k}: expected non-empty {CANDIDATE_FIELDS[k].__name__}")
        if e.get("placement", "in-change") not in CANDIDATE_PLACEMENTS:
            errors.append(f"{w}.placement: {e['placement']!r} not one of {CANDIDATE_PLACEMENTS}")
        if e.get("name") in names:
            errors.append(f"{w}.name: {e['name']!r} named twice")
        names.add(e.get("name"))
    return errors


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


def _goal_drift_asks(store, state):
    """[(index entry, file record, answer analysis)] for every decision whose options are the four."""
    out = []
    for a in state["asks_for_user"]:
        if a["kind"] != "decision":
            continue
        _doc, rec = read_ask_file(store, a["path"])
        if rec and {_norm(o.get("label")) for o in rec["options"]} >= GOAL_DRIFT_OPTIONS:
            out.append((a, rec, analyse_answer(rec)))
    return out


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
            errs = _handshake_errors(os.path.join(store.home, hp), prior=True)
            put("prior_integrate_consumed", not errs, "; ".join(errs) or f"{hp} has all required fields")
        missing = [t["task_name"] for t in tasks
                   if t["status"] != "externalised" and not t["cites_seams"]]
        put("briefs_cite_seams", not missing and bool(tasks),
            f"tasks without cites_seams: {missing}" if missing else
            ("no tasks yet" if not tasks else "every non-externalised task cites at least one seam"))
        gd = _goal_drift_asks(store, state)
        pending, vague = [], []
        for a, _rec, ans in gd:
            if a["status"] not in ("answered", "applied"):
                pending.append(a["id"])
            elif not ans or ans["match"] != "exact" or _norm(ans["label"]) not in GOAL_DRIFT_OPTIONS:
                vague.append(a["id"])
        why = "; ".join(filter(None, [
            f"goal-drift asks unanswered: {pending}" if pending else "",
            f"answer does not name revise/split/abandon/continue: {vague}" if vague else ""]))
        put("user_signed_off_goal_drift", not pending and not vague,
            why or ("no goal-drift ask this cycle" if not gd else "goal-drift ask answered"))
    elif phase == "execute":
        not_stopped = [t["task_name"] for t in tasks if t["status"] not in STOPPED]
        put("all_tasks_executed_or_stopped", not not_stopped and bool(tasks),
            f"not stopped: {not_stopped}" if not_stopped else "all tasks done/failed/externalised/blocked")
        unreviewed = []
        for t in tasks:
            if t["status"] in ("reviewed", "done") and t["review_mode"] is not None:
                p = t["findings_path"]
                if not p or not os.path.exists(os.path.join(store.home, p)):
                    unreviewed.append(t["task_name"])
        put("all_reviews_completed", not unreviewed,
            f"reviewed/done without a findings file: {unreviewed}" if unreviewed
            else "every reviewed task has its findings file")
        orphans = [t["task_name"] for t in tasks if t["status"] in ORPHAN]
        put("no_orphan_in_progress", not orphans,
            f"in flight: {orphans}" if orphans else "no task in flight")
    elif phase == "integrate":
        blocking = [f["finding_id"] for f in state["architect_findings"]
                    if f.get("decoration") == "blocking" and f["resolution"] == "pending"]
        put("blocking_findings_resolved", not blocking,
            f"pending blocking findings: {blocking}" if blocking else "no pending blocking finding")
        digest = os.path.join(store.cycle_dir(state), "pm-digest.md")
        ok, why = False, f"{os.path.relpath(digest, store.home)} missing"
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
            entries, file_problems = scan_inbox(store, state)
            missing = sorted(open_ids - in_h)
            # An answer the orchestrator has not acted on fails the gate; one that has been read
            # back and awaits the user's yes does not (the cycle can close with it open).
            unread = [e["id"] for e in entries if e["indexed"] and e["status"] == "open"
                      and e["answer"] and not e["readback"]["present"]]
            problems = []
            if missing:
                problems.append(f"open asks not in handshake: {missing}")
            if unread:
                problems.append(f"answered in the file but not acted on (apply, or write the readback): {unread}")
            if file_problems:
                problems.append("inbox problems: " + "; ".join(file_problems[:5]))
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
    if not store.home_found:
        raise Refusal("no_home", f"no .claude/orchestrator/config.yaml found walking up from {os.getcwd()}; "
                      "create the overlay at the intended home (the repo, or the workspace above it) or "
                      "pass --home")
    _require_change(store)
    if args.change and args.change != store.change:
        raise Refusal("change_mismatch", f"--change {args.change!r} but the resolved change is {store.change!r}")
    if not args.test_command:
        args.test_command = (store.hc["repos"].get(store.repo) or {}).get("test", {}).get("command")
        if not args.test_command:
            raise Refusal("no_test_command", "pass --test-command, or set test.command in config.yaml")
    prior_baseline = None
    if store.exists():
        cur = store.load()
        prior_baseline = cur.get("baseline_status")
        if not cur.get("closed_at"):
            raise Refusal("cycle_open", f"cycle {cur.get('cycle_id')} is still open (phase "
                          f"{cur.get('phase')}); run `close` (or `close --abandon`) first")
    if args.prior_handshake is None and not args.first_cycle:
        # Default: the most recent handshake on disk, if any.
        hs = sorted(glob.glob(os.path.join(store.dir, "handshake-*.json")), key=os.path.getmtime)
        args.prior_handshake = os.path.relpath(hs[-1], store.home) if hs else None
        if args.prior_handshake is None:
            raise Refusal("no_prior_handshake", "no handshake-*.json found; pass --prior-handshake "
                          "or --first-cycle")
    if args.prior_handshake:
        errs = _handshake_errors(os.path.join(store.home, args.prior_handshake), prior=True)
        if errs:
            raise Refusal("bad_prior_handshake", "plan refuses to start: " + "; ".join(errs))
    if _git_ignores(store.home, ".orchestrator"):
        raise Refusal("orchestrator_ignored", ".orchestrator/ is ignored by this repository's "
                      ".gitignore; process artifacts are tracked (SKILL.md). Delete that line "
                      "and run `init` again (an environment ask)")
    problems = repo_problems(store.hc) + change_problems(store)
    if problems:
        slug, msg = problems[0]
        raise Refusal(slug, msg, problems=[{"error": a, "message": b} for a, b in problems])
    os.makedirs(store.dir, exist_ok=True)
    wrote_gitignore = _write_orchestrator_gitignore(store)
    ts = int(time.time())
    cycle_id = args.cycle or f"cycle-{ts}"
    carried = []
    for p in sorted(glob.glob(os.path.join(store.dir, "inbox", "*.org"))):
        rel = os.path.relpath(p, store.home)
        _doc, rec = read_ask_file(store, rel)
        if rec["kind"] not in ASK_KINDS or rec["status"] not in ASK_STATUSES:
            continue  # reported by `inbox` as a problem, not silently indexed
        carried.append(_index_entry(rec, rel))
    state = {
        "schema_version": SCHEMA_VERSION,
        "session_id": f"orch-{ts}",
        "cycle_id": cycle_id,
        "phase": "plan",
        "home": store.home,
        "code_root": store.code_root,
        "change_name": store.change,
        "baseline_snapshot": None,
        "baseline_status": None,
        "current_branch": args.branch or _git_branch(store.code_root),
        "test_command": args.test_command,
        "history_window": args.history_window,
        "prior_handshake": args.prior_handshake,
        "closed_at": None,
        "tasks": [],
        "architect_findings": [],
        "asks_for_user": carried,
        "agents": [],
        "journal": [],
        "cycle_log": {"started_at": now_iso(), "phase_started_at": {"plan": now_iso(), "execute": None,
                                                                    "integrate": None},
                      "history": collect_history(store, args.history_window)},
        "phase_gates": gates_fresh(),
    }
    # The pending agents are taken in under the lock `record add agents` writes them under, so an
    # entry recorded while init runs is in the new state or still pending, never dropped.
    with open(store.lock_path, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        try:
            pending_agents = _read_pending_agents(store)
            state["agents"] = pending_agents
            errors = validate(state)
            if errors:
                raise Refusal("invalid_state", "init produced an invalid state (bug)", errors=errors)
            store.write(state)
            if pending_agents:
                os.remove(_pending_agents_path(store))
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)
    return {"ok": True, "cycle_id": cycle_id, "path": store.path,
            "history_cycles": [h["cycle_id"] for h in state["cycle_log"]["history"]],
            "inbox_carried": [a["id"] for a in carried],
            "agents_carried": [a["agent_id"] for a in pending_agents], "gitignore_written": wrote_gitignore,
            "home": store.home, "change": store.change, "code_root": store.code_root,
            "warnings": home_warnings(store) + red_baseline_warnings(store, prior_baseline,
                                                                       "the last cycle's baseline_status")}


def _git_branch(code_root):
    """The code root's current branch (a linked worktree's .git is a file, so ask git)."""
    rc, out = _git(code_root, "rev-parse", "--abbrev-ref", "HEAD")
    if rc != 0 or not out.strip():
        return "main"
    ref = out.strip()
    if ref == "HEAD":
        rc, sha = _git(code_root, "rev-parse", "--short=12", "HEAD")
        return sha.strip() if rc == 0 and sha.strip() else "main"
    return ref


def _is_worktree_root(path):
    rc, out = _git(path, "rev-parse", "--show-toplevel")
    return rc == 0 and out.strip() and _same_path(out.strip(), path)


def repo_problems(hc):
    """[(slug, message)] about the repositories config.yaml names; empty for the home itself."""
    out = list(hc["problems"])
    slug = "code_root" if hc["shape"] == "code-root" else "repo"  # the shorthand keeps its old slugs
    for name, r in hc["repos"].items():
        if r.get("from_config") is None:
            continue  # the home is the repository; nothing to check
        key = "code-root" if hc["shape"] == "code-root" else f"repos.{name}.path"
        if not os.path.isdir(r["path"]):
            out.append((f"{slug}_missing", f"config.yaml {key}: {r['from_config']} -> {r['path']} is not a directory"))
            continue
        rc, _ = _git(r["path"], "rev-parse", "--show-toplevel")
        if rc != 0:
            out.append((f"{slug}_not_git", f"repository {name} at {r['path']} is not a git work tree"))
    return out


def change_problems(store):
    """[(slug, message)] about the resolved change's code root, its worktree; empty when it is the home."""
    if not store.split:
        return []
    cr, home = store.code_root, store.home
    if not os.path.isdir(cr):
        return [("code_root_missing", f"change {store.change}: worktree {cr} is not a directory; "
                 "`change open` again, or fix change.json")]
    rc, _ = _git(cr, "rev-parse", "--show-toplevel")
    out = []
    if rc != 0:
        out.append(("code_root_not_git", f"code root {cr} is not a git work tree (a linked worktree is fine)"))
    if os.path.exists(os.path.join(cr, ".orchestrator")):
        out.append(("misplaced_artifacts", f".orchestrator/ found in the code root {cr}; artifacts live at "
                    f"the home {home}. Move it there (a plain mv; the code repo's .gitignore usually "
                    "ignores it) and run init again"))
    if os.path.exists(config_path(cr)):
        out.append(("two_homes", f"the code root {cr} has its own .claude/orchestrator/config.yaml; one "
                    f"home per code root. Move that overlay to {config_path(home)} or drop the repository "
                    "from the home's config.yaml"))
    return out


def code_root_problems(store):
    return change_problems(store)


def home_warnings(store):
    out = []
    if os.path.isdir(os.path.join(store.home, "openspec", "specs")):
        out.append(f"openspec/specs/ is still in the home {store.home}; this skill reads the record from "
                   "docs/ in the code root (templates/spec.md). Convert it first with the openspec-convert "
                   "skill")
    hc = store.hc
    if not hc or _git(store.home, "rev-parse", "--show-toplevel")[0] != 0:
        return out
    nested = [(name, r["path"]) for name, r in hc["repos"].items() if r.get("from_config") is not None]
    if os.path.isdir(hc["worktrees"]["parent"]):
        nested.append(("worktrees", hc["worktrees"]["parent"]))
    for name, path in nested:
        rel = os.path.relpath(path, store.home)
        if rel.startswith("..") or rel == ".":
            continue
        if not _git_ignores(store.home, rel):
            what = "the worktrees folder" if name == "worktrees" else f"the repository {name}"
            out.append(f"the home repo does not ignore {what} {rel}/; add it to {store.home}/.gitignore "
                       "so no code checkout appears as an embedded repository in the home's history")
    return out


def code_root_warnings(store):
    return home_warnings(store)


def _change_summary(store, rec):
    s = Store(store.home, rec["change"])
    out = {"name": rec["change"], "repo": rec.get("repo"), "branch": rec.get("branch"),
           "worktree": rec.get("worktree"), "base": rec.get("base"), "from": rec.get("from"),
           "state": "present" if s.exists() else "absent", "cycle": None, "phase": None, "closed": None}
    if s.exists():
        try:
            st = json.load(open(s.path))
            out.update({"cycle": st.get("cycle_id"), "phase": st.get("phase"),
                        "closed": bool(st.get("closed_at"))})
        except (json.JSONDecodeError, OSError):
            out["state"] = "unreadable"
    return out


def verb_where(store, args):
    """Where am I: the home, its repositories, its changes, the resolved change's code root and
    anything wrong with them. Needs no state file."""
    cfg = config_path(store.home)
    hc = store.hc or {"shape": None, "repos": {}, "worktrees": {}, "problems": []}
    problems = repo_problems(hc) if store.hc else []
    if store.change and store.change_rec:
        problems += change_problems(store)
    elif store.change:
        problems.append(("change_not_open", f"no change {store.change!r} under {changes_root(store.home)}; "
                         "`change open <name>` opens one"))
    out = {"ok": True, "home": store.home, "home_found": store.home_found,
           "config": cfg if os.path.exists(cfg) else None,
           "shape": hc["shape"],
           "repos": {n: {"path": r["path"], "from_config": r.get("from_config"),
                         "test_command": r.get("test", {}).get("command")} for n, r in hc["repos"].items()},
           "worktrees": {"parent": hc["worktrees"].get("parent"), "pattern": hc["worktrees"].get("pattern")},
           "changes": [_change_summary(store, rec) for rec in list_changes(store.home)],
           "change": store.change if store.change_rec else None,
           "change_resolved_by": store.change_how if store.change_rec else None,
           "repo": store.repo if store.change_rec else None,
           "code_root": store.code_root if store.change_rec else None,
           "change_dir": store.dir if store.change_rec else None,
           "split": store.split if store.change_rec else None,
           "state": ("present" if store.exists() else "absent") if store.change_rec else None,
           "legacy_layout": store.legacy_layout(),
           "problems": [{"error": a, "message": b} for a, b in problems],
           "warnings": home_warnings(store) + overlay_drift(store.home, read_config(store.home))}
    if out["legacy_layout"]:
        out["warnings"].append(f"{store.root}/state.json is the one-change layout; run `migrate` to move it "
                               "under .orchestrator/changes/<change>/")
    return out


def where_sh(result, hc):
    lines = [f"export ORCH_HOME={shlex.quote(result['home'])}"]
    if hc and hc["worktrees"].get("parent"):
        lines.append(f"export ORCH_WORKTREES={shlex.quote(hc['worktrees']['parent'])}")
    if result.get("change"):
        lines += [f"export ORCH_CHANGE={shlex.quote(result['change'])}",
                  f"export ORCH_CHANGE_DIR={shlex.quote(result['change_dir'])}",
                  f"export ORCH_REPO={shlex.quote(result['repo'])}",
                  f"export CODE_ROOT={shlex.quote(result['code_root'])}"]
    return "\n".join(lines)


def verb_worktree(store, args):
    """The path a worktree of the change's repository takes for a branch (overlay.md § Worktrees)."""
    _require_change(store)
    path = worktree_path_for(store.hc, store.repo, args.branch)
    return {"ok": True, "path": path, "repo": store.repo, "branch": args.branch,
            "parent": store.hc["worktrees"]["parent"], "pattern": store.hc["worktrees"]["pattern"],
            "exists": os.path.exists(path)}


def _require_change(store):
    if not store.hc:
        raise Refusal("no_home", f"no .claude/orchestrator/config.yaml found walking up from {os.getcwd()}; "
                      "create the overlay at the intended home (the repo, or the workspace above it) or "
                      "pass --home")
    if not store.change:
        names = [c["change"] for c in list_changes(store.home)]
        raise Refusal("no_change", "which change? pass --change <name>, set ORCH_CHANGE, or run from inside "
                      f"its worktree; open ones: {names or 'none (`change open <name>` opens one)'}",
                      changes=names)
    if not store.change_rec:
        raise Refusal("change_not_open", f"no change {store.change!r} under {changes_root(store.home)}; "
                      "`change open <name>` opens one (or adopts its worktree with --worktree)")


def _git_full(repo_root, *args):
    try:
        r = subprocess.run(["git", "-C", repo_root, *args], capture_output=True, text=True)
        return r.returncode, r.stdout, r.stderr
    except OSError as e:
        return None, "", str(e)


def verb_change_open(store, args):
    """Open a change: its worktree of one repository, on a branch named after it, and its
    directory under .orchestrator/changes/<change>/ with change.json. With --worktree an existing
    checkout is adopted as it is (its branch is recorded). Refuses while the change is open."""
    if not store.hc:
        _require_change(store)
    name = args.name
    if not re.match(r"^[A-Za-z0-9][A-Za-z0-9._-]*$", name):
        raise Refusal("bad_change_name", "a change name is one path segment: letters, digits, . _ -")
    if read_change(store.home, name):
        raise Refusal("change_exists", f"change {name} is already open: {change_json_path(store.home, name)}")
    hc = store.hc
    problems = repo_problems(hc)
    if problems:
        raise Refusal(problems[0][0], problems[0][1], problems=[{"error": a, "message": b} for a, b in problems])
    repos = hc["repos"]
    repo = args.repo
    if not repo:
        if len(repos) != 1:
            raise Refusal("which_repo", f"pass --repo <name>; this home has {sorted(repos)}")
        repo = next(iter(repos))
    if repo not in repos:
        raise Refusal("unknown_repo", f"no repository {repo!r} in config.yaml; it has {sorted(repos)}")
    r = repos[repo]
    adopted, created, from_ref = False, False, None
    if args.worktree:
        path = os.path.abspath(os.path.expanduser(args.worktree))
        if not os.path.isdir(path):
            raise Refusal("worktree_missing", f"--worktree {path} is not a directory")
        adopted, from_ref = True, args.from_ref  # recorded, not acted on: the branch exists
    else:
        path = worktree_path_for(hc, repo, name)
        if os.path.exists(path):
            if not _is_worktree_root(path):
                raise Refusal("worktree_occupied", f"{path} exists and is not a git work tree root")
            if _git_branch(path) != name:
                raise Refusal("worktree_occupied", f"{path} is checked out on {_git_branch(path)!r}, not {name!r}; "
                              "adopt it with --worktree, or remove it")
            adopted, from_ref = True, args.from_ref
        else:
            from_ref = args.from_ref or "HEAD"
            os.makedirs(os.path.dirname(path), exist_ok=True)
            rc, _o, err = _git_full(r["path"], "rev-parse", "--verify", "--quiet", f"refs/heads/{name}")
            if rc == 0:
                rc, _o, err = _git_full(r["path"], "worktree", "add", path, name)
            else:
                rc, _o, err = _git_full(r["path"], "worktree", "add", "-b", name, path, from_ref)
            if rc != 0:
                raise Refusal("git_failed", f"git worktree add failed in {r['path']}: {err.strip()}")
            created = True
    if _same_path(path, store.home) and hc["shape"] != "repo-root":
        raise Refusal("worktree_is_home", "the home is not a code root unless it is the repository "
                      "(config.yaml without repos: or code-root:)")
    if not _same_path(path, store.home):
        if os.path.exists(os.path.join(path, ".orchestrator")):
            raise Refusal("misplaced_artifacts", f".orchestrator/ found in {path}; artifacts live at the home "
                          f"{store.home}. Move it there and open the change again")
        if os.path.exists(config_path(path)):
            raise Refusal("two_homes", f"{path} has its own .claude/orchestrator/config.yaml; one home per "
                          "code root. Move that overlay to the home or drop it")
    base = _change_base(path, from_ref)
    intent = os.path.join(path, "docs", "changes", name, "intent.org")
    if base and os.path.isfile(intent):
        m = re.search(r"^#\+BASE:\s*([0-9a-fA-F]{7,40})", open(intent, encoding="utf-8").read(), re.M)
        if m and not (base.startswith(m.group(1)) or m.group(1).startswith(base)):
            raise Refusal("base_mismatch", f"{intent} says #+BASE: {m.group(1)} but the branch's base is "
                          f"{base} (the merge-base with {from_ref or 'HEAD'}); correct the intent, or pass "
                          "the --from ref the change was cut from", intent_base=m.group(1), base=base)
    rec = {"change": name, "repo": repo, "branch": _git_branch(path), "worktree": path,
           "base": base, "from": from_ref, "opened_at": now_iso(), "adopted": adopted}
    os.makedirs(os.path.dirname(change_json_path(store.home, name)), exist_ok=True)
    _atomic_write(change_json_path(store.home, name), json.dumps(rec, indent=2) + "\n")
    new = Store(store.home, name, path, repo)
    new.hc, new.change_rec = hc, rec
    init_hook = _run_worktree_init(hc, r, name, store.home, path) if not getattr(args, "no_init", False) else None
    return {"ok": True, **rec, "created": created, "path": change_json_path(store.home, name),
            "init_hook": init_hook,
            "problems": [{"error": a, "message": b} for a, b in change_problems(new)],
            "warnings": home_warnings(new)}


def _change_base(path, from_ref):
    """The commit the change starts from: the merge-base of its branch with the ref it was cut from
    (an adopted branch may already carry its hand-off commit, so HEAD is not it), else HEAD. The
    record: `change open --from workspace-notes-location` wrote the hand-off commit as the base
    while intent.org held the fork point; coverage.py --since then saw 3 scenarios, not 34."""
    rc, sha = _git(path, "rev-parse", "HEAD")
    head = sha.strip() if rc == 0 and sha.strip() else None
    if head and from_ref and from_ref != "HEAD":
        rc, mb = _git(path, "merge-base", from_ref, "HEAD")
        if rc == 0 and mb.strip():
            return mb.strip()
    return head


def _run_worktree_init(hc, repo_spec, change, home, path):
    """Run the overlay's worktree.init hook inside the change's worktree (flows/execute.md § 1 runs it
    for task worktrees; the design round's probes run in this one). The hook sees ORCH_HOME,
    ORCH_CHANGE and CODE_ROOT. A failure is reported, not a refusal: the change is open."""
    hook = (repo_spec.get("worktree") or {}).get("init") or (hc.get("worktree") or {}).get("init")
    if not hook:
        return None
    env = dict(os.environ, ORCH_HOME=home, ORCH_CHANGE=change, CODE_ROOT=path)
    try:
        r = subprocess.run(hook, shell=True, cwd=path, env=env, capture_output=True, text=True)
        return {"hook": hook, "rc": r.returncode, "stderr_tail": r.stderr[-400:]}
    except OSError as e:
        return {"hook": hook, "rc": None, "stderr_tail": str(e)}


# ---------------------------------------------------------------- the overlay: bootstrap and drift

OVERLAY_VERSION = 1
# overlay.md § config.yaml schema. A key not here is drift: either removed (REMOVED_KEYS says why)
# or never part of the schema.
OVERLAY_KEYS = {
    "overlay-version": None, "project": None, "language": None,
    "repos": {"*": {"path", "language", "build", "test", "worktree"}},
    "code-root": None,
    "build": {"pre-commit"},
    "test": {"command", "parser", "compare", "pending-style"},
    "worktrees": {"parent", "pattern"},
    "worktree": {"init", "needs-submodules"},
    "artifacts": {"tasks-root", "externalised-root", "specs-root", "intent-pattern", "design-pattern"},
    "taxonomy": None,
    "thresholds": {"history-window", "drainage-trigger-ratio", "drainage-trigger-consecutive-cycles",
                   "cascade-trigger-followup-count", "stale-task-cycles", "review-starvation-ratio",
                   "externalisation-pressure-growth"},
    "critical-path": {"inference", "override-tasks", "override-labels"},
    "architect": {"drift-hotspots"},
    "reviewer": {"checks"},
    "asks": {"reader"},
    "priors": None,
}
REMOVED_KEYS = {
    "scaffolding": "scaffolding.md and the scaffolding gates were removed (2026-10-09); the design round's pending tests replaced them",
    "artifacts.register": "the interfaces register was replaced by design.org's Seams table (2026-09-28); the record lives under docs/",
    "architect.interfaces-document": "the interfaces register was replaced by design.org's Seams table (2026-09-28)",
    "architect.severity-overrides": "severity is the reporter's and routing is by the change's main path (templates/architect-finding.md, 2026-10-14)",
    "roles-overlay": "role overlays are found by name under roles/ (overlay.md § Layout); the key named nothing",
    "worktree.parent": "moved to worktrees.parent, relative to the home, with worktrees.pattern (2026-10-15)",
}
# Words in a role overlay or priors file that name a retired concept; the file is the user's and
# is reviewed by hand, so this is a warning naming the words.
RETIRED_CONCEPTS = ["interfaces register", "interfaces.org", "register entry", "register entries", "entry_tier",
                    "entry tier", "load-bearing", "scaffolding", "scaffold", "register_touched", "architecture.md",
                    "cites_register_entries"]


def overlay_drift(home, cfg):
    """Warnings about config.yaml and the role overlays against the skill's schema."""
    out = []
    if not cfg:
        return out
    v = cfg.get("overlay-version")
    if v is None:
        out.append(f"config.yaml has no overlay-version; the skill's is {OVERLAY_VERSION}. Check its keys against "
                   "overlay.md (the warnings below), then add `overlay-version: 1` (overlay.md § Drift)")
    elif str(v) != str(OVERLAY_VERSION):
        out.append(f"config.yaml is overlay-version {v}; the skill's is {OVERLAY_VERSION} (overlay.md § Drift)")

    def removed(path):
        why = REMOVED_KEYS.get(path)
        return f"config.yaml {path}: {why}; delete it" if why else None

    for key, val in cfg.items():
        if key in REMOVED_KEYS:
            out.append(removed(key))
            continue
        if key not in OVERLAY_KEYS:
            out.append(f"config.yaml {key}: not in overlay.md's schema; delete it or move it")
            continue
        sub = OVERLAY_KEYS[key]
        if not sub or not isinstance(val, dict):
            continue
        for k2, v2 in val.items():
            path = f"{key}.{k2}"
            if path in REMOVED_KEYS:
                out.append(removed(path))
            elif "*" in sub:
                if isinstance(v2, dict):
                    for k3 in v2:
                        if k3 not in sub["*"]:
                            out.append(f"config.yaml {key}.{k2}.{k3}: not in overlay.md's schema")
            elif k2 not in sub:
                out.append(f"config.yaml {path}: not in overlay.md's schema; delete it or move it")
    # a path that climbs from a code root to the home
    for key in ("build", "test", "worktree"):
        blocks = [(key, cfg.get(key))] + [(f"repos.{n}.{key}", (r or {}).get(key))
                                          for n, r in (cfg.get("repos") or {}).items()
                                          if isinstance(cfg.get("repos"), dict)]
        for name, block in blocks:
            if not isinstance(block, dict):
                continue
            for k2, v2 in block.items():
                if isinstance(v2, str) and re.search(r"(^|\s|=)\.\./", v2):
                    out.append(f"config.yaml {name}.{k2} climbs to the home with ../ ({v2!r}); the code root's depth is "
                               "the workspace's, not the skill's: write $ORCH_HOME/… (hooks see it) or ship the tool")
    # role overlays and priors naming retired concepts
    d = os.path.join(home, ".claude", "orchestrator")
    files = sorted(glob.glob(os.path.join(d, "roles", "*.md"))) + [os.path.join(d, "priors.md")]
    for f in files:
        if not os.path.exists(f):
            continue
        text = open(f, encoding="utf-8", errors="replace").read().lower()
        hits = sorted({w for w in RETIRED_CONCEPTS if w in text})
        if hits:
            out.append(f"{os.path.relpath(f, home)} names retired concepts ({', '.join(hits)}); rewrite those "
                       "parts against the current roles/*.md (overlay.md § Drift)")
    return [w for w in out if w]


def render_config(args, shape, repos):
    """config.yaml text for `home init`: the schema's required keys filled, the optional ones as comments."""
    lines = ["# Orchestrator overlay (overlay.md). Written by `state.py home init`; edited by hand.",
             "# `state.py where` warns when a key here drifts from the skill's schema.",
             f"overlay-version: {OVERLAY_VERSION}", f"project: {args.project}", f"language: {args.language}", ""]
    tc = args.test_command or "make test"
    if shape == "workspace":
        lines += ["# The repositories: a change belongs to one and is worked in its own worktree of it",
                  "# (overlay.md § Repositories). path: a checkout of the repository, relative to the home or absolute.",
                  "repos:"]
        for name, path in repos:
            lines += [f"  {name}:", f"    path: {path}"]
        lines.append("")
    elif shape == "code-root":
        lines += ["# The one repository's checkout, relative to the home (overlay.md § Repositories).",
                  f"code-root: {repos[0][1]}", ""]
    else:
        lines += ["# No repos: or code-root: — this directory is the repository and the home (overlay.md § Repositories).", ""]
    lines += ["build:",
              "  # pre-commit: ./bin/tangle-org.sh   # OPTIONAL, run in the worktree before each commit",
              "test:",
              f"  command: {tc}",
              f"  parser: {args.parser}   # emacs-results-txt | exit-code | junit-xml | tap",
              ("  compare: python3 ~/.claude/skills/orchestrate/scripts/compare.py --parser " + args.parser
               if args.parser != "exit-code" else
               "  # compare: python3 ~/.claude/skills/orchestrate/scripts/compare.py --parser <parser>   # needed when the baseline is red; exit-code cannot compare"),
              "  # pending-style: xfail-strict | xit   # how the design round marks a pending test (default by runner)",
              "",
              "# Where worktrees go, from the home (overlay.md § Worktrees). The pattern is the workspace's convention.",
              "worktrees:",
              f"  parent: {args.worktrees_parent}",
              f"  pattern: {args.worktrees_pattern}",
              "",
              "worktree:",
              "  # init: ./bin/init-worktree-runtime.sh   # OPTIONAL, run inside a new worktree",
              "  # needs-submodules: false",
              "",
              "# OPTIONAL blocks, as overlay.md gives them: artifacts, taxonomy, thresholds, critical-path,",
              "# architect (drift-hotspots), reviewer (checks), asks (reader), priors.",
              "# taxonomy:",
              "#   - feature",
              "#   - test",
              "#   - doc",
              "#   - refactor",
              "#   - bug",
              "#   - contract",
              "#   - infrastructure",
              "# architect:",
              "#   drift-hotspots:",
              "#     - <module>: <what to watch>",
              "# reviewer:",
              "#   checks:",
              "#     - <check>",
              ""]
    return "\n".join(lines)


def verb_home_init(store, args):
    """Write a new home's .claude/orchestrator/config.yaml from the skill's schema (overlay.md
    § Bootstrap). Refuses when one exists. The home is --home or the cwd."""
    home = os.path.abspath(args.home or (store.home if store.home_found else os.getcwd()))
    cfg = config_path(home)
    if os.path.exists(cfg):
        raise Refusal("overlay_exists", f"{cfg} exists; edit it, or move it away to start over")
    repos = []
    for spec in args.repo or []:
        if "=" not in spec:
            raise Refusal("bad_repo_arg", "--repo takes <name>=<path>")
        name, path = spec.split("=", 1)
        if not re.match(r"^[A-Za-z0-9][A-Za-z0-9._-]*$", name) or not path:
            raise Refusal("bad_repo_arg", f"--repo {spec!r}: <name> is one path segment, <path> non-empty")
        repos.append((name, path))
    if args.code_root and repos:
        raise Refusal("bad_repo_arg", "--code-root is the one-repository shorthand; with --repo use neither")
    if args.code_root:
        shape, repos = "code-root", [(os.path.basename(os.path.normpath(args.code_root)), args.code_root)]
    elif repos:
        shape = "workspace"
    else:
        shape = "repo-root"
    for name, path in repos:
        p = os.path.abspath(os.path.join(home, os.path.expanduser(path)))
        if not os.path.isdir(p):
            raise Refusal("repo_missing", f"repository {name}: {path} -> {p} is not a directory")
        if _git(p, "rev-parse", "--show-toplevel")[0] != 0:
            raise Refusal("repo_not_git", f"repository {name}: {p} is not a git work tree")
    if shape == "repo-root" and _git(home, "rev-parse", "--show-toplevel")[0] != 0:
        raise Refusal("home_not_git", f"{home} is not a git work tree; a repository-root home is the repository. "
                      "Name the repositories with --repo <name>=<path> for a workspace")
    if not args.worktrees_pattern:
        args.worktrees_pattern = "<repo>/<branch>" if shape == "workspace" else "<branch>"
    if not args.project:
        args.project = os.path.basename(os.path.normpath(home))
    text = render_config(args, shape, repos)
    os.makedirs(os.path.dirname(cfg), exist_ok=True)
    os.makedirs(os.path.join(os.path.dirname(cfg), "roles"), exist_ok=True)
    _atomic_write(cfg, text)
    hc = resolve_home_config(home)
    gitignore = []
    if _git(home, "rev-parse", "--show-toplevel")[0] == 0:
        for name, r in hc["repos"].items():
            rel = os.path.relpath(r["path"], home)
            if r.get("from_config") is not None and not rel.startswith("..") and not _git_ignores(home, rel):
                gitignore.append(rel + "/")
        rel = os.path.relpath(hc["worktrees"]["parent"], home)
        if not rel.startswith("..") and not _git_ignores(home, rel):
            gitignore.append(rel + "/")
    return {"ok": True, "home": home, "config": cfg, "shape": shape,
            "repos": {n: r["path"] for n, r in hc["repos"].items()},
            "worktrees": hc["worktrees"], "problems": [{"error": a, "message": b} for a, b in hc["problems"]],
            "add_to_home_gitignore": gitignore,
            "next": "review config.yaml with the user (test command, parser, pending-style, hooks, taxonomy, "
                    "drift hot spots, reviewer checks); add roles/<role>.md and priors.md as overlay.md says; "
                    "then `state.py change open <name>`"}


def _rewrite_layout_paths(obj, change):
    """`.orchestrator/<x>` -> `.orchestrator/changes/<change>/<x>` through a JSON tree, except what
    was always under changes/ (the task files) and the root .gitignore."""
    pre, keep = ".orchestrator/", ".orchestrator/changes/"
    if isinstance(obj, dict):
        return {k: _rewrite_layout_paths(v, change) for k, v in obj.items()}
    if isinstance(obj, list):
        return [_rewrite_layout_paths(v, change) for v in obj]
    if isinstance(obj, str) and obj.startswith(pre) and not obj.startswith(keep) \
            and obj != ".orchestrator/.gitignore":
        return f"{keep}{change}/{obj[len(pre):]}"
    return obj


def _move_into(home, src, dst):
    """Move a file or directory inside the home, with git mv when tracked; merge a directory into
    one that already exists."""
    if os.path.isdir(src) and os.path.isdir(dst):
        for child in sorted(os.listdir(src)):
            _move_into(home, os.path.join(src, child), os.path.join(dst, child))
        if not os.listdir(src):
            os.rmdir(src)
        return
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    rel_src, rel_dst = os.path.relpath(src, home), os.path.relpath(dst, home)
    rc, out = _git(home, "ls-files", "--", rel_src)
    if rc == 0 and out.strip():
        rc2, _ = _git(home, "mv", "-k", "--", rel_src, rel_dst)
        if rc2 == 0 and os.path.exists(dst) and not os.path.exists(src):
            return
    os.replace(src, dst)


def migrate_layout(store):
    """Move the one-change layout (.orchestrator/state.json and its siblings) under
    .orchestrator/changes/<change>/ and write that change's change.json. Returns the report."""
    root = store.root
    legacy = os.path.join(root, "state.json")
    old = json.load(open(legacy))
    name = old.get("change_name")
    if not isinstance(name, str) or not name:
        raise Refusal("legacy_no_change", f"{legacy} names no change_name; pass --change <name> to migrate")
    if read_change(store.home, name):
        raise Refusal("change_exists", f"change {name} is already open under changes/; move {legacy} by hand")
    dest = os.path.join(root, "changes", name)
    os.makedirs(dest, exist_ok=True)
    moved = []
    for entry in sorted(os.listdir(root)):
        if entry in ("changes", ".gitignore", "state.lock", "tools") or entry.startswith("."):
            continue
        if entry in ("state.json", "cycles", "inbox") or entry.startswith(("handshake-", "baseline-", "after-",
                                                                            "state.json.pre-migrate-")):
            _move_into(store.home, os.path.join(root, entry), os.path.join(dest, entry))
            moved.append(entry)
    for p in [os.path.join(dest, "state.json")] + glob.glob(os.path.join(dest, "handshake-*.json")):
        try:
            doc = json.load(open(p))
        except (json.JSONDecodeError, OSError):
            continue
        _atomic_write(p, json.dumps(_rewrite_layout_paths(doc, name), indent=2) + "\n")
    hc = store.hc or resolve_home_config(store.home)
    code_root = old.get("code_root") or old.get("repo_root") or store.home
    repo = None
    for n, r in hc["repos"].items():
        if _same_path(r["path"], code_root):
            repo = n
    if repo is None and len(hc["repos"]) == 1:
        repo = next(iter(hc["repos"]))
    rec = {"change": name, "repo": repo, "branch": old.get("current_branch") or _git_branch(code_root),
           "worktree": code_root, "base": None, "from": None, "opened_at": now_iso(), "adopted": True,
           "migrated_from_layout": now_iso()}
    _atomic_write(change_json_path(store.home, name), json.dumps(rec, indent=2) + "\n")
    return {"change": name, "moved": moved, "to": os.path.relpath(dest, store.home), "change_json": rec}


def _write_orchestrator_gitignore(store):
    gi = os.path.join(store.root, ".gitignore")
    os.makedirs(store.root, exist_ok=True)
    if os.path.exists(gi):
        return False
    _atomic_write(gi, ORCHESTRATOR_GITIGNORE)
    return True


def collect_history(store, window):
    """Per-cycle counts from archived states, newest last, at most `window`."""
    out, earlier = [], set()
    for cid, s in _archived_states(store):
        if str(s.get("schema_version")) == SCHEMA_VERSION:
            counts = derive_counts(s["tasks"], s.get("journal") or [], earlier)
        else:
            legacy = (s.get("cycle_log") or {}).get("counts") if isinstance(s.get("cycle_log"), dict) else None
            counts = legacy if isinstance(legacy, dict) else derive_counts_legacy(s.get("tasks") or [])
        earlier |= _task_names(s)
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
    if store.change is None and store.legacy_layout():
        return {"ok": True, "legacy_layout": True, "resume": "run `migrate`",
                "message": f"{store.root}/state.json is the one-change layout; run `migrate` to move it "
                           "under .orchestrator/changes/<change>/"}
    if store.hc is None and not store.change:
        raise Refusal("not_initialized", f"no state file at {store.path}; run `init`", exit_code=2)
    _require_change(store)
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
    entries, problems = scan_inbox(store, state)
    questions = {e["id"]: e["question"] for e in entries}
    answered = {e["id"]: e for e in entries if e["indexed"] and e["answer"] and not e["readback"]["present"]}
    confirming = {e["id"]: e for e in entries if e["indexed"] and e["answer"] and e["readback"]["present"]
                  and not e["readback"]["confirmed"]}
    confirmed = {e["id"]: e for e in entries if e["indexed"] and e["status"] == "open"
                 and e["readback"]["present"] and e["readback"]["confirmed"]}
    resume = resume_hint(state)
    if confirmed:
        resume = f"{len(confirmed)} readback(s) confirmed ({', '.join(confirmed)}): apply; then " + resume
    if answered:
        resume = f"{len(answered)} answer(s) waiting in the inbox ({', '.join(answered)}): run `inbox`; then " + resume
    out = {
        "ok": True,
        "cycle_id": state["cycle_id"],
        "change_name": state["change_name"],
        "phase": state["phase"],
        "closed_at": state["closed_at"],
        "paths": {"home": store.home, "change": store.change, "code_root": store.code_root},
        "resume": resume,
        "gates": gates,
        "counts": cycle_counts(store, state),
        "tasks": [{"name": t["task_name"], "status": t["status"],
                   "merge": (t["merge_commit"] or "")[:10] or None,
                   "blocked_by": t["blocked_by"], "blocker_note": t["blocker_note"],
                   "critical": t["on_critical_path"]} for t in state["tasks"]],
        "open_asks": [{"id": a["id"], "kind": a["kind"], "question": questions.get(a["id"]),
                       "blocks": a["blocks"], "path": a["path"], "answered_in_file": a["id"] in answered}
                      for a in state["asks_for_user"] if a["status"] == "open"],
        "inbox": {"open": sum(1 for a in state["asks_for_user"] if a["status"] == "open"),
                  "answered_in_file": len(answered),
                  "awaiting_confirmation": len(confirming),
                  "readback_confirmed": len(confirmed),
                  "deferred": sum(1 for a in state["asks_for_user"] if a["status"] == "deferred"),
                  "unindexed_files": sum(1 for e in entries if not e["indexed"]),
                  "problems": len(problems)},
        "pending_blocking_findings": [f["finding_id"] for f in state["architect_findings"]
                                      if f.get("decoration") == "blocking" and f["resolution"] == "pending"],
        "journal_tail": state["journal"][-5:],
        "prior_handshake": state["prior_handshake"],
        "validation_errors": errors,
    }
    return out


def verb_inbox(store, args):
    """Read-only: every inbox file with its answer analysis; works on a closed cycle too."""
    state = store.load() if store.exists() else None
    if state is not None and str(state.get("schema_version")) != SCHEMA_VERSION:
        state = None
    entries, problems = scan_inbox(store, state)
    return {"ok": True, "cycle_id": (state or {}).get("cycle_id"), "home": store.home,
            "answered": [e["id"] for e in entries if e["answer"]],
            "asks": entries, "problems": problems}


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
        plan = migration_plan(state, store)
        return {"ok": False, "schema_version": state.get("schema_version"), "migration": plan}
    errors = validate(state)
    _entries, file_problems = scan_inbox(store, state)
    return {"ok": not errors and not file_problems, "errors": errors, "file_problems": file_problems}


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
        t["created_at"] = now_iso()  # the tool stamps it; the record shows agents inventing timestamps
        _stamp_task_file(store, t)
        state["tasks"].append(t)
        return {"ok": True, "task": t}
    return store.mutate(fn)


def _stamp_task_file(store, t):
    """Write the index's created_at into the task file's frontmatter when the file exists and has the
    key (templates/task-body.md), so the two agree. The record: two task files carried created_at
    30-35 minutes after the commit that holds them."""
    if not t.get("task_file"):
        return
    path = t["task_file"] if os.path.isabs(t["task_file"]) else os.path.join(store.home, t["task_file"])
    if not os.path.isfile(path):
        return
    text = open(path, encoding="utf-8").read()
    new = re.sub(r"^(created_at:)[^\n]*$", rf"\1 {t['created_at']}", text, count=1, flags=re.M)
    if new != text:
        _atomic_write(path, new)


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
    "agents": ("agents", AGENT_FIELDS, AGENT_DEFAULTS, "agent_id"),
}


def _pending_agents_path(store):
    return os.path.join(store.dir, "agents-pending.json")


def _read_pending_agents(store):
    p = _pending_agents_path(store)
    if not os.path.exists(p):
        return []
    try:
        with open(p) as f:
            recs = json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        raise Refusal("bad_pending_agents", f"{os.path.relpath(p, store.home)}: {e}")
    if not isinstance(recs, list):
        raise Refusal("bad_pending_agents", f"{os.path.relpath(p, store.home)} is not a JSON array")
    return recs


def verb_agent_add(store, args):
    """An agent's setting, recorded by its spawner once the Agent call has returned the id. With no
    open cycle (the opening and design-round agents, an integrate phase agent recorded after
    `close`) the entry waits in agents-pending.json and the next `init` takes it in."""
    _require_change(store)
    rec_in = load_json_arg(args.json, store.home)
    if not isinstance(rec_in, dict):
        raise Refusal("bad_record", "record must be a JSON object")
    if "recorded_at" in rec_in:
        raise Refusal("bad_record", "recorded_at is the tool's")

    def build(cycle, existing):
        rec = dict(AGENT_DEFAULTS)
        rec["cycle"] = cycle
        rec.update(rec_in)
        rec["recorded_at"] = now_iso()
        if any(r.get("agent_id") == rec.get("agent_id") for r in existing):
            raise Refusal("duplicate_id", f"agent {rec.get('agent_id')!r} is already recorded; a "
                          "resumed agent keeps its first entry")
        errors = []
        _validate_agent(rec, "agents[new]", errors)
        if errors:
            raise Refusal("invalid_record", "record does not fit the schema", errors=errors)
        return rec

    # One lock for the whole choice, the one `init` takes the pending file in under: whether a
    # cycle is open is read and acted on without an `init` in between.
    os.makedirs(store.dir, exist_ok=True)
    with open(store.lock_path, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        try:
            pending = _read_pending_agents(store)
            state = store.load() if store.exists() else None
            if state is not None and not state.get("closed_at"):
                require_current_schema(state)
                errors = validate(state)
                if errors:
                    raise Refusal("invalid_state", "state file fails validation; fix it before writing",
                                  errors=errors[:20])
                rec = build(state["cycle_id"], state["agents"] + pending)
                state["agents"].append(rec)
                store.write(state)
                return {"ok": True, "list": "agents", "id": rec["agent_id"], "record": rec}
            rec = build(state.get("cycle_id") if state else None,
                        pending + ((state.get("agents") or []) if state else []))
            pending.append(rec)
            _atomic_write(_pending_agents_path(store), json.dumps(pending, indent=2) + "\n")
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)
    return {"ok": True, "list": "agents", "id": rec["agent_id"], "record": rec,
            "pending": os.path.relpath(_pending_agents_path(store), store.home)}


def _ask_input(raw, home=None):
    """A JSON object, @file.json, or @draft.org -> (record, draft AskDoc or None, draft path)."""
    if raw.startswith("@") and raw.lower().endswith(".org"):
        path = _arg_path(raw[1:], home)
        if not os.path.exists(path):
            raise Refusal("no_such_file", f"{raw[1:]} does not exist (looked in cwd"
                          + (f" and {home}" if home else "") + ")")
        with open(path, encoding="utf-8") as f:
            doc = AskDoc(f.read())
        return ask_from_org(doc, None), doc, path  # no :ID: in the draft -> the tool assigns one
    rec = load_json_arg(raw, home)
    if not isinstance(rec, dict):
        raise Refusal("bad_record", "record must be a JSON object")
    return _ask_aliases(rec), None, None


def _ask_aliases(rec):
    """Two shapes agents write in every home, read as the org form reads :RAISED_BY: and :REF:
    (ask_from_org): a top-level `ref` is `raised_by.ref`, a string `raised_by` is the role. The
    record: asks refused `invalid_record` for these in all four homes on cf2577d."""
    rec = dict(rec)
    rb = rec.get("raised_by")
    if isinstance(rb, str):
        rec["raised_by"] = rb = {"role": rb, "ref": None}
    if "ref" in rec:
        ref = rec.pop("ref")
        rb = dict(rb) if isinstance(rb, dict) else {"role": "orchestrator"}
        if rb.get("ref") not in (None, ref):
            raise Refusal("bad_record", f"ref {ref!r} and raised_by.ref {rb['ref']!r} differ; give one")
        rb["ref"] = ref
        rec["raised_by"] = rb
    return rec


def _ask_ids_on_disk(store, state):
    pats = [os.path.join(store.dir, "inbox", "*.org"),
            os.path.join(store.cycle_dir(state), "asks", "*.org")]
    return [os.path.basename(p)[:-4] for pat in pats for p in glob.glob(pat)]


def _index_entry(rec, path):
    return {"id": rec["id"], "kind": rec["kind"], "status": rec["status"],
            "blocks": list(rec.get("blocks") or []), "path": path}


def _supersede(store, state, old_id, new_id):
    old = next((a for a in state["asks_for_user"] if a["id"] == old_id), None)
    if old is None:
        raise Refusal("no_such_record", f"revised_from names {old_id!r}, which is not an ask of this cycle")
    doc, _rec = read_ask_file(store, old["path"])
    if doc is None:
        raise Refusal("ask_file_missing", f"{old['path']} is missing (restore it from git)")
    doc.set_prop("STATUS", "superseded")
    _atomic_write(os.path.join(store.home, old["path"]), doc.text())
    new_path = ask_path(old_id, "superseded", state["cycle_id"], store)
    moved = _move_ask_file(store, old["path"], new_path) if new_path != old["path"] else None
    old.update({"status": "superseded", "path": new_path})
    add_journal(state, "decision", f"{old_id} superseded by {new_id}", by="orchestrator", ref=new_id)
    return moved


def verb_ask_add(store, args):
    rec_in, draft, draft_path = _ask_input(args.json, store.home)

    def fn(state):
        rec = {**ASK_DEFAULTS, **ASK_FILE_DEFAULTS, **{k: v for k, v in rec_in.items() if k != "path"}}
        if not rec.get("id"):
            prefix = "ask-" + state["cycle_id"] + "-"
            rec["id"] = prefix + str(next_seq(
                [a["id"] for a in state["asks_for_user"]] + _ask_ids_on_disk(store, state), prefix))
        if any(a["id"] == rec["id"] for a in state["asks_for_user"]):
            raise Refusal("duplicate_id", f"asks_for_user already has {rec['id']!r}; use `record set`")
        errors = []
        validate_ask_full(rec, "asks[new]", errors)
        if errors:
            raise Refusal("invalid_record", "ask does not fit templates/ask.md", errors=errors)
        path = ask_path(rec["id"], rec["status"], state["cycle_id"], store)
        abs_path = os.path.join(store.home, path)
        in_place = draft_path and os.path.abspath(draft_path) == os.path.abspath(abs_path)
        if os.path.exists(abs_path) and not in_place:
            raise Refusal("file_exists", f"{path} already exists; `record add asks @{path}` indexes it, "
                          "or remove the stray file")
        moved = _supersede(store, state, rec["revised_from"], rec["id"]) if rec.get("revised_from") else None
        if draft is not None:
            for k in ("ID", "KIND", "STATUS"):
                draft.set_prop(k, rec[k.lower()])
            draft.set_prop("BLOCKS", rec["blocks"])
            if draft.section_lines("Answer") is None:
                draft.set_section("Answer", ASK_ANSWER_HINT)
            text = draft.text()
        else:
            text = ask_to_org(rec)
        _atomic_write(abs_path, text)
        state["asks_for_user"].append(_index_entry(rec, path))
        out = {"ok": True, "list": "asks_for_user", "id": rec["id"], "path": path, "status": rec["status"],
               "record": rec}
        if moved:
            out["superseded"] = {"id": rec["revised_from"], "moved_via": moved}
        return out
    return store.mutate(fn)


def verb_ask_set(store, args):
    updates = parse_assignments(args.fields)
    confirm = updates.pop("readback", None)  # readback=confirmed: the user's yes under * Readback
    if confirm is not None and _norm(str(confirm)) not in ("confirmed", "yes"):
        raise Refusal("bad_value", "readback takes `confirmed` (writes the user's yes under * Readback)")
    allowed = set(ASK_FILE_FIELDS) | {"kind", "status", "blocks"}
    for k in updates:
        if k not in allowed:
            raise Refusal("unknown_field" if k not in ("id", "path") else "not_settable",
                          f"{k!r} cannot be set on an ask", allowed=sorted(allowed))

    def fn(state):
        entry = next((a for a in state["asks_for_user"] if a["id"] == args.id), None)
        if entry is None:
            raise Refusal("no_such_record", f"asks_for_user has no {args.id!r}",
                          ids=[a["id"] for a in state["asks_for_user"]])
        doc, rec = read_ask_file(store, entry["path"])
        if doc is None:
            raise Refusal("ask_file_missing", f"{entry['path']} is missing (restore it from git)")
        if doc.drawer is None:  # the file wins for everything it still says; the index fills the rest
            for k in ("ID", "KIND", "STATUS"):
                doc.set_prop(k, entry[k.lower()])
            doc.set_prop("BLOCKS", entry["blocks"])
            rec.update({"id": entry["id"], "kind": entry["kind"], "status": entry["status"],
                        "blocks": list(entry["blocks"])})
        rec["id"] = entry["id"]
        new = {**rec, **updates}
        if new.get("status") in ASK_ANSWERED and not new.get("decision"):
            raise Refusal("answer_required", f"status {new['status']} needs an answer under * Answer "
                          "(or decision=<text> in this command)")
        if new.get("status") == "applied" and not new.get("applied_via"):
            raise Refusal("applied_via_required", "status applied needs applied_via=<what applied it>")
        errors = []
        validate_ask_full(new, f"asks[{args.id}]", errors)
        if errors:
            raise Refusal("invalid_record", "the change does not fit templates/ask.md", errors=errors)
        for k, v in updates.items():
            if k == "kind":
                doc.set_prop("KIND", v)
            elif k == "status":
                doc.set_prop("STATUS", v)
            elif k == "blocks":
                doc.set_prop("BLOCKS", v)
            elif k == "raised_by":
                doc.set_prop("RAISED_BY", (v or {}).get("role"))
                doc.set_prop("REF", (v or {}).get("ref"))
            elif k == "question":
                doc.set_title(v)
            elif k == "observed":
                doc.set_prop("HAPPENED", None if v is None else ("yes" if v.get("happened") else "no"))
                doc.set_section("What happened", (v or {}).get("evidence") or "")
            elif k == "options":
                doc.set_section("Options", "\n".join(
                    f"- {o.get('label')} :: {o.get('consequence') or ''}".rstrip(" :") for o in v))
            elif k == "recommendation":
                doc.set_section("Recommendation",
                                f"- {v['option']} :: {v.get('why') or ''}".rstrip(" :") if v else "")
            elif k == "about":
                doc.set_section("About", v or "")
            elif k == "default_if_unanswered":
                doc.set_section("If unanswered", v or "")
            elif k == "decision":
                doc.set_section("Answer", v or ASK_ANSWER_HINT)
            elif k == "decision_readback":
                doc.set_section("Readback", v or "")
            elif k == "applied_via":
                doc.set_prop("APPLIED_VIA", v)
            elif k == "revised_from":
                doc.set_prop("REVISED_FROM", v)
        if confirm is not None:
            rb = doc.section_text("Readback") or ""
            if not rb.strip():
                raise Refusal("no_readback", f"{entry['id']} has no * Readback to confirm; write one with "
                              "decision_readback=<text> first")
            if not any(_norm(l) in ("yes", "confirmed", "ok") for l in rb.splitlines()):
                doc.set_section("Readback", rb.rstrip() + "\n\nyes")
            updates["readback"] = "confirmed"
        old_path = entry["path"]
        _atomic_write(os.path.join(store.home, old_path), doc.text())
        final = ask_from_org(doc, entry["id"])
        final["id"] = entry["id"]
        new_path = ask_path(entry["id"], final["status"], state["cycle_id"], store)
        moved = _move_ask_file(store, old_path, new_path) if new_path != old_path else None
        entry.update(_index_entry(final, new_path))
        out = {"ok": True, "list": "asks_for_user", "id": entry["id"], "changed": sorted(updates),
               "status": entry["status"], "path": new_path}
        if moved:
            out["moved_via"] = moved
        return out
    return store.mutate(fn)


def verb_record_add(store, args):
    if args.list == "asks":
        return verb_ask_add(store, args)
    if args.list == "agents":
        return verb_agent_add(store, args)
    key, fields, defaults, id_field = RECORD_LISTS[args.list]
    rec_in = load_json_arg(args.json, store.home)
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
        if args.list == "findings" and (rec.get("kind") not in KINDS
                                        or not isinstance(rec.get("evidence"), str)
                                        or not rec["evidence"].strip()):
            raise Refusal("kind_required",
                          "a finding carries kind=issue|question and its evidence (the line, run or "
                          "output; for a question the reasoning and the reproduction to try); a "
                          "speculative need is a `note note \"thought: ...\"`, not a finding "
                          "(templates/architect-finding.md § Kind)")
        if args.list == "findings" and not rec.get("path"):
            rec["path"] = os.path.relpath(os.path.join(store.cycle_dir(state), "findings",
                                                       rec["finding_id"] + ".md"), store.home)
        errors = []
        _validate_record(rec, fields, f"{key}[new]", errors, id_field)
        if errors:
            raise Refusal("invalid_record", "record does not fit the schema", errors=errors)
        state[key].append(rec)
        return {"ok": True, "list": key, "id": rec[id_field], "record": rec}
    return store.mutate(fn)


def verb_record_set(store, args):
    if args.list == "asks":
        return verb_ask_set(store, args)
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
        out = {"ok": True, "changed": sorted(updates)}
        warnings = red_baseline_warnings(store, updates.get("baseline_status"))
        if warnings:
            out["warnings"] = warnings
        return out
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
    sig = pm_signals(state, store)
    if args.write:
        d = store.cycle_dir(state)
        os.makedirs(d, exist_ok=True)
        p = os.path.join(d, "pm-signals.json")
        with open(p, "w") as f:
            json.dump(sig, f, indent=2)
        sig["written_to"] = os.path.relpath(p, store.home)
    sig["ok"] = True
    return sig


def _agent_transcripts(agent_id):
    base = os.environ.get("CLAUDE_CONFIG_DIR") or os.path.expanduser("~/.claude")
    aid = agent_id[len("agent-"):] if agent_id.startswith("agent-") else agent_id
    found = glob.glob(os.path.join(base, "projects", "*", "*", "subagents", f"agent-{aid}*.jsonl"))
    exact = [p for p in found if os.path.basename(p) == f"agent-{aid}.jsonl"]
    return exact or found


def _model_family(model_id):
    for fam in AGENT_MODELS:
        if fam in (model_id or ""):
            return fam
    return None


def _parse_ts(ts):
    try:
        return datetime.fromisoformat(ts.replace("Z", "+00:00"))
    except (AttributeError, ValueError):
        return None


def agent_spend(agent_id):
    """One agent's spend from its transcript: each API request once (by requestId, else message
    id), its usage, the model and effort it ran at, and its wall-clock. None when the transcript is
    gone or the id is ambiguous. `cost_weight` is E3's proxy (input + 0.1 cache read + 1.25 cache
    write + 5 output), blind to the model, so it compares with E3's numbers; `priced_weight` scales
    each request's weight by its model's input price against Opus's; `usd` is list price.
    A request logged on several lines (one per content block) counts each usage field at its
    largest: the record has 153 of 11,650 requests whose output_tokens grew from line to line. A
    `<synthetic>` line (written on an API error, with zero usage and no effort) is no request."""
    paths = _agent_transcripts(agent_id)
    if len(paths) != 1:
        return None
    out = {"requests": 0, "input": 0, "cache_read": 0, "cache_create": 0, "output": 0,
           "cost_weight": 0.0, "priced_weight": 0.0, "usd": 0.0, "ran": {}, "minutes": None}
    fields = ("input_tokens", "cache_read_input_tokens", "cache_creation_input_tokens", "output_tokens")
    reqs, first, last = {}, None, None
    with open(paths[0], encoding="utf-8") as f:
        for n, line in enumerate(f):
            try:
                r = json.loads(line)
            except json.JSONDecodeError:
                continue
            at = _parse_ts(r.get("timestamp"))
            if at:
                first, last = first or at, at
            msg = r.get("message") or {}
            u = msg.get("usage") if r.get("type") == "assistant" else None
            if not u or msg.get("model") == "<synthetic>":
                continue
            req = reqs.setdefault(r.get("requestId") or msg.get("id") or n,
                                  {"model": msg.get("model"), "effort": r.get("effort"),
                                   **{k: 0 for k in fields}})
            for k in fields:
                req[k] = max(req[k], u.get(k) or 0)
    for req in reqs.values():
        i, cr, cc, o = (req[k] for k in fields)
        fam = _model_family(req["model"])
        ran = f"{fam or req['model'] or '?'}/{req['effort'] or '?'}"
        out["ran"][ran] = out["ran"].get(ran, 0) + 1
        out["requests"] += 1
        out["input"] += i
        out["cache_read"] += cr
        out["cache_create"] += cc
        out["output"] += o
        w = i + 0.1 * cr + 1.25 * cc + 5 * o
        out["cost_weight"] += w
        if fam:
            p = (HAIKU_LONG_PROMPT_PRICES if fam == "haiku" and i + cr + cc > 100000
                 else MODEL_PRICES[fam])
            out["priced_weight"] += w * p[0] / MODEL_PRICES["opus"][0]
            out["usd"] += (i * p[0] + o * p[1] + cr * p[2] + cc * p[3]) / 1e6
    if first and last:
        out["minutes"] = round((last - first).total_seconds() / 60, 1)
    for k in ("cost_weight", "priced_weight"):
        out[k] = round(out[k])
    out["usd"] = round(out["usd"], 2)
    return out


SPEND_KEYS = ("requests", "input", "cache_read", "cache_create", "output", "cost_weight",
              "priced_weight", "usd")


def spend_by_role(agents, spend):
    """Per agent and per role, from each agent's `agent_spend` in `spend`. A role's `settings`
    counts the agent settings its agents were spawned at; `mismatches` lists agents that ran at
    another one (an environment variable such as CLAUDE_CODE_EFFORT_LEVEL outranks the call, E1)."""
    rows, roles, missing, mismatches = [], {}, [], []
    for a in agents:
        s = spend.get(a["agent_id"])
        asked = f"{a['model']}/{a['effort']}"
        row = {k: a[k] for k in ("agent_id", "role", "task", "cycle", "source", "reason")}
        row["setting"] = asked
        row["spend"] = s
        rows.append(row)
        r = roles.setdefault(a["role"], {"agents": 0, "counted": 0, "settings": {}, "overrides": 0,
                                         "minutes": [], **{k: 0 for k in SPEND_KEYS}})
        r["agents"] += 1
        r["settings"][asked] = r["settings"].get(asked, 0) + 1
        r["overrides"] += a["source"] == "task-override"
        if s is None:
            missing.append(a["agent_id"])
            continue
        r["counted"] += 1
        for k in SPEND_KEYS:
            r[k] += s[k]
        if s["minutes"] is not None:
            r["minutes"].append(s["minutes"])
        if any(m != a["model"] or e not in ("?", a["effort"])
               for m, e in (k.split("/", 1) for k in s["ran"])):
            mismatches.append({"agent_id": a["agent_id"], "setting": asked, "ran": s["ran"]})
    total = sum(r["cost_weight"] for r in roles.values())
    for r in roles.values():
        m = sorted(r.pop("minutes"))
        r["median_minutes"] = m[len(m) // 2] if m else None
        r["usd"] = round(r["usd"], 2)
        r["cost_weight_share"] = round(r["cost_weight"] / total, 3) if total else None
        r["cost_weight_per_agent"] = round(r["cost_weight"] / r["counted"]) if r["counted"] else None
    return {"by_role": roles, "by_agent": rows, "missing_transcripts": missing,
            "setting_mismatches": mismatches}


def verb_context(store, args):
    """A running agent's context, from the usage of the last call in its transcript, against the
    overlay's thresholds.phase-agent-context. The dispatcher reads this line, never the transcript
    (SKILL.md § The dispatcher's loop). The record: five phase agents passed 400k (the largest
    568,815) and the users watched and swapped them by hand."""
    paths = _agent_transcripts(args.agent)
    if not paths:
        raise Refusal("no_transcript", f"no subagents/agent-{args.agent}*.jsonl under the projects "
                      "directory ($CLAUDE_CONFIG_DIR or ~/.claude); pass the id the Agent call returned")
    if len(paths) > 1:
        raise Refusal("ambiguous_agent", f"{len(paths)} transcripts match {args.agent!r}; give the full id",
                      transcripts=sorted(paths))
    used, at = None, None
    with open(paths[0], encoding="utf-8") as f:
        for line in f:
            try:
                r = json.loads(line)
            except json.JSONDecodeError:
                continue
            u = (r.get("message") or {}).get("usage") if r.get("type") == "assistant" else None
            if u:
                used = (u.get("input_tokens", 0) + u.get("cache_creation_input_tokens", 0)
                        + u.get("cache_read_input_tokens", 0))
                at = r.get("timestamp")
    limit = read_thresholds(store.home)["phase_agent_context"]
    return {"ok": True, "agent": args.agent, "context": used, "limit": limit,
            "over": used is not None and used > limit, "at": at, "transcript": paths[0]}


def verb_handshake(store, args):
    meta = load_json_arg(args.meta_discoveries, store.home) if args.meta_discoveries else []
    refinements = load_json_arg(args.task_refinements, store.home) if args.task_refinements else []
    if not isinstance(meta, list) or not isinstance(refinements, list):
        raise Refusal("bad_json", "meta-discoveries and task-refinements must be JSON arrays")
    flag_diff = load_json_arg(args.design_diff, store.home) if args.design_diff else None

    def fn(state):
        digest = os.path.relpath(os.path.join(store.cycle_dir(state), "pm-digest.md"), store.home)
        # design_diff: the conformance step's cycles/<id>/design-diff.json when present,
        # else --design-diff, else empty.
        diff_file = os.path.join(store.cycle_dir(state), "design-diff.json")
        if os.path.exists(diff_file):
            try:
                with open(diff_file) as f:
                    diff = json.load(f)
            except (json.JSONDecodeError, OSError) as e:
                raise Refusal("bad_design_diff", f"{os.path.relpath(diff_file, store.home)}: {e}")
            source = os.path.relpath(diff_file, store.home)
        elif flag_diff is not None:
            diff, source = flag_diff, "--design-diff"
        else:
            diff, source = [], "default"
        derrs = _design_diff_errors(diff, "design_diff")
        if derrs:
            raise Refusal("bad_design_diff", f"design diff from {source} does not fit the shape "
                          "[{seam, change: added|amended|retired, ref}]", errors=derrs[:20])
        cand_file = os.path.join(store.cycle_dir(state), "candidate-tasks.json")
        cands = []
        if os.path.exists(cand_file):
            try:
                with open(cand_file) as f:
                    cands = json.load(f)
            except (json.JSONDecodeError, OSError) as e:
                raise Refusal("bad_candidate_tasks", f"{os.path.relpath(cand_file, store.home)}: {e}")
        cerrs = _candidate_errors(cands, "candidate_tasks")
        named = {c.get("name") for c in cands if isinstance(c, dict)}
        cerrs += [f"finding {f['finding_id']}: resolution {f['resolution']} names no candidate"
                  for f in state["architect_findings"]
                  if f["resolution"].startswith("candidate-task-")
                  and f["resolution"][len("candidate-task-"):] not in named]
        if cerrs:
            raise Refusal("bad_candidate_tasks", "cycles/<id>/candidate-tasks.json does not fit "
                          "[{name, source, discovered_by, critical, quote, ...}] or misses a finding's "
                          "candidate", errors=cerrs[:20])
        goal = []
        for a, rec, ans in _goal_drift_asks(store, state):
            if a["status"] in ("answered", "applied"):
                goal.append({"ask": a["id"],
                             "decision": ans["label"] if ans and ans["match"] == "exact" else (ans or {}).get("text"),
                             "rationale": rec["decision_readback"]})

        def clip(s):
            return s if s is None or len(s) <= LONG_TEXT_CAP else s[:LONG_TEXT_CAP - 1] + "…"

        resolved = []
        for a in state["asks_for_user"]:
            if a["status"] in ("answered", "applied", "deferred"):
                _doc, rec = read_ask_file(store, a["path"])
                resolved.append({**a, "decision": clip((rec or {}).get("decision")),
                                 "decision_readback": clip((rec or {}).get("decision_readback")),
                                 "applied_via": (rec or {}).get("applied_via")})
        h = {
            "cycle_id": state["cycle_id"],
            "produced_at": now_iso(),
            "design_diff": diff,
            "pm_digest_path": digest,
            "meta_discoveries": meta,
            "user_resolved_goal_drift": goal,
            "asks_for_user_open": [a for a in state["asks_for_user"] if a["status"] == "open"],
            "asks_for_user_resolved": resolved,
            "task_refinements": refinements,
            "candidate_tasks": cands,
            "journal": state["journal"],
        }
        p = store.handshake_path(state["cycle_id"])
        with open(p + ".tmp", "w") as f:
            json.dump(h, f, indent=2)
        os.replace(p + ".tmp", p)
        return {"ok": True, "path": os.path.relpath(p, store.home),
                "design_diff": len(diff), "design_diff_source": source,
                "asks_open": len(h["asks_for_user_open"]),
                "asks_resolved": len(h["asks_for_user_resolved"]), "task_refinements": len(refinements),
                "candidate_tasks": len(cands)}
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
        return {"ok": True, "cycle_id": state["cycle_id"], "archived_to": os.path.relpath(d, store.home),
                "copied": copied, "abandoned": bool(args.abandon),
                "counts": cycle_counts(store, state),
                "inbox_left": {s: sum(1 for a in state["asks_for_user"] if a["status"] == s)
                               for s in ("open", "answered", "deferred")}}
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


def migration_plan(state, store=None):
    """What `migrate` would do to a legacy file. Pure but for the store's paths."""
    plan = {"from": state.get("schema_version"), "to": SCHEMA_VERSION,
            "top_level_parked": [], "task_fields_parked": {}, "status_mapped": {},
            "records_parked": {}, "asks_kept": 0, "asks_parked": 0,
            "register_touched_parked": 0}
    if store is not None:
        old_root = state.get("home", state.get("repo_root"))
        plan["rehome"] = ({"from": old_root, "to": store.home}
                          if isinstance(old_root, str) and old_root and not _same_path(old_root, store.home)
                          else None)
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
    layout = None
    if store.change is None and store.legacy_layout():
        layout = migrate_layout(store)
        new = Store(store.home, layout["change"], layout["change_json"]["worktree"], layout["change_json"]["repo"])
        new.hc, new.change_rec, new.home_found = store.hc, read_change(store.home, layout["change"]), store.home_found
        store = new
    _require_change(store)
    os.makedirs(store.dir, exist_ok=True)
    with open(store.lock_path, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        try:
            old = store.load()
            if str(old.get("schema_version")) == SCHEMA_VERSION:
                return {"ok": True, "unchanged": layout is None, "layout": layout}
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
            old_root = new.get("home")
            rehomed_from = (old_root if isinstance(old_root, str) and old_root
                            and not _same_path(old_root, store.home) else None)
            if rehomed_from:
                parked["top_level"]["repo_root"] = old_root
            new["home"], new["code_root"] = store.home, store.code_root
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

            # A legacy "resolved" ask was acted on in its cycle: it is applied history, archived
            # under its cycle's asks/, not an answer waiting in the inbox.
            LEGACY_ASK_STATUS = {"resolved": "applied", "answered": "answered", "applied": "applied",
                                 "open": "open", "deferred": "deferred", "declined": "declined",
                                 "superseded": "superseded"}
            ask_files = {"written": [], "existing": []}
            for key, fields, defaults, id_field in (
                    ("architect_findings", FINDING_FIELDS, FINDING_DEFAULTS, "finding_id"),
                    ("asks_for_user", {**ASK_FIELDS, **ASK_FILE_FIELDS},
                     {**ASK_DEFAULTS, **ASK_FILE_DEFAULTS}, "id")):
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
                                f"{store_rel(store)}/cycles/{old.get('cycle_id')}/findings/{u.get('finding_id')}.md")
                        if u.get("class") not in FINDING_CLASSES:
                            extra["_legacy_class"], u["class"] = u.get("class"), None
                        if u.get("severity") not in SEVERITIES and u.get("severity") is not None:
                            # Schema <= 1.4: severity was the routing (blocking | advisory |
                            # informational); only `blocking` carries over, as the decoration.
                            legacy = u.get("severity")
                            extra["_legacy_severity"], u["severity"] = legacy, None
                            if legacy == "blocking" and u.get("decoration") is None:
                                u["decoration"] = "blocking"
                        if u.get("kind") not in KINDS:
                            # Schema <= 1.4: observed.happened false was a suspicion (a question);
                            # true, or not filled, an issue. The old block is kept in the extras.
                            obs = extra.get("observed") if isinstance(extra.get("observed"), dict) else {}
                            if u.get("kind") is not None:
                                extra["_legacy_kind"] = u.get("kind")
                            u["kind"] = "question" if obs.get("happened") is False else "issue"
                            if not u.get("evidence"):
                                ev = obs.get("evidence")
                                if isinstance(ev, str) and ev.strip():
                                    u["evidence"] = re.sub(r"^\s*reasoning only:\s*", "", ev).strip() or ev
                                else:
                                    u["evidence"] = str(u.get("title") or "(no evidence recorded)")
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
                        if u["status"] == "applied" and not u.get("applied_via"):
                            u["applied_via"] = str(extra.get("applied_via") or "legacy: resolved in its cycle")
                        if u["status"] in ASK_ANSWERED and not u.get("decision"):
                            # The legacy `resolution` text is the only record of what was decided;
                            # it becomes the Answer. Nothing else is invented.
                            u["decision"] = (u.get("decision_readback") or
                                             (str(extra["resolution"]) if extra.get("resolution") else None)
                                             or (f"Applied without asking: {u['applied_via']}."
                                                 if u["status"] == "applied" and u["kind"] in ("doc-correction", "confirmation")
                                                 else "(no answer recorded)"))
                        if not isinstance(u.get("options"), list):
                            u["options"] = []
                        u["options"] = [o if isinstance(o, dict) and o.get("label") else {"label": str(o)}
                                        for o in u["options"]]
                        if isinstance(u.get("observed"), dict) and not isinstance(
                                u["observed"].get("happened"), bool):
                            extra["_legacy_observed"], u["observed"] = u["observed"], None
                        for k in ("about", "default_if_unanswered", "decision", "decision_readback",
                                  "applied_via", "revised_from"):
                            if u.get(k) is not None and not isinstance(u[k], str):
                                extra[k], u[k] = u[k], None
                        if not isinstance(u.get("recommendation"), (dict, type(None))):
                            extra["recommendation"], u["recommendation"] = u["recommendation"], None
                        if isinstance(u.get("question"), str):
                            u["question"] = re.sub(r"\s+", " ", u["question"]).strip() or str(u.get("id"))
                        u["blocks"] = [str(b) for b in u.get("blocks") or []] if isinstance(u.get("blocks"), list) else []
                    cap_strings(u, extra, fields)
                    errs = []
                    if key == "asks_for_user":
                        # Prose has no cap in the file; keep the full text where cap_strings clipped it.
                        for k in ("about", "default_if_unanswered", "decision"):
                            if k + "_full" in extra:
                                u[k] = extra.pop(k + "_full")
                        if u.get("kind") == "decision" and u.get("status") == "open":
                            # Legacy decisions rarely carry about/observed/options; keep them as open
                            # files for the orchestrator to complete rather than refuse the migration.
                            probe = []
                            validate_ask_full(u, key, probe)
                            errs = [e for e in probe if not any(f in e for f in (".about:", ".observed:", ".options:"))]
                        else:
                            validate_ask_full(u, key, errs)
                    else:
                        _validate_record(u, fields, key, errs, id_field)
                    if errs:
                        r["_migration_errors"] = errs
                        parked[key].append(r)
                        continue
                    if extra:
                        extras_here[str(u[id_field])] = extra
                    if key == "asks_for_user":
                        path = ask_path(u["id"], u["status"], new.get("cycle_id") or old.get("cycle_id"), store)
                        abs_path = os.path.join(store.home, path)
                        if os.path.exists(abs_path):
                            ask_files["existing"].append(path)
                        else:
                            _atomic_write(abs_path, ask_to_org(u))
                            ask_files["written"].append(path)
                        u = _index_entry(u, path)
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
                        path=os.path.relpath(parked_path, store.home))
            wrote_gitignore = _write_orchestrator_gitignore(store)
            store.write(new)
            return {"ok": True, "from": old.get("schema_version"), "to": SCHEMA_VERSION,
                    "paths": {"home": store.home, "change": store.change, "code_root": store.code_root},
                    "layout": layout,
                    "rehomed_from": rehomed_from, "gitignore_written": wrote_gitignore,
                    "parked": os.path.relpath(parked_path, store.home),
                    "backup": os.path.relpath(backup, store.home),
                    "parked_top_level_keys": sorted(parked["top_level"]),
                    "tasks_with_parked_fields": sorted(parked["tasks"]),
                    "register_touched_parked": reg_count,
                    "ask_files": ask_files,
                    "records": {k: {"kept": len(new[k]), "parked": len(parked[k]),
                                    "with_extras": len(parked["record_extras"].get(k, {}))}
                                for k in ("architect_findings", "asks_for_user")}}
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)


# ---------------------------------------------------------------- CLI

def build_parser():
    p = argparse.ArgumentParser(prog="state.py", description=__doc__.split("\n\n")[0])
    p.add_argument("--home", "--repo", dest="home",
                   help="the home: the directory holding .claude/orchestrator/config.yaml and "
                        ".orchestrator/ (default: walk up from cwd)")
    p.add_argument("--change", dest="change_sel",
                   help="the change to act on (default: $ORCH_CHANGE, the change whose worktree holds "
                        "the cwd, or the only open change)")
    sub = p.add_subparsers(dest="verb", required=True)

    s = sub.add_parser("where", help="the home, its repositories and open changes, the resolved change's "
                       "code root, and any problem with them (needs no state)")
    s.add_argument("--sh", action="store_true", help="print export ORCH_HOME=... ORCH_CHANGE=... CODE_ROOT=... for eval")
    s.set_defaults(fn=verb_where)

    h = sub.add_parser("home", help="home init: write a new home's config.yaml from the skill's schema").add_subparsers(dest="sub", required=True)
    s = h.add_parser("init", help="init [--repo <name>=<path> ...] | [--code-root <dir>] (neither: this directory is the repository)")
    s.add_argument("--repo", action="append", help="a repository: <name>=<path to a checkout>, relative to the home; repeatable")
    s.add_argument("--code-root", help="the one repository's checkout, relative to the home")
    s.add_argument("--project", help="slug (default: the home directory's name)")
    s.add_argument("--language", default="other")
    s.add_argument("--test-command", help="default: make test")
    s.add_argument("--parser", default="exit-code", choices=["emacs-results-txt", "exit-code", "junit-xml", "tap"])
    s.add_argument("--worktrees-parent", default=".worktrees")
    s.add_argument("--worktrees-pattern", help="default: <repo>/<branch> for a workspace, <branch> otherwise")
    s.set_defaults(fn=verb_home_init)

    c = sub.add_parser("change", help="change open: a change's worktree and its directory under .orchestrator/changes/").add_subparsers(dest="sub", required=True)
    s = c.add_parser("open", help="open <name> [--repo <r>] [--from <ref> | --worktree <path>]")
    s.add_argument("name")
    s.add_argument("--repo", help="the repository (default: the home's only one)")
    s.add_argument("--from", dest="from_ref", help="the ref the new branch starts from (default HEAD of the repository "
                   "checkout); the workspace integration branch when the hand-off made one. With --worktree it is recorded only")
    s.add_argument("--worktree", help="adopt this existing checkout as the change's code root")
    s.add_argument("--no-init", action="store_true", help="do not run the overlay's worktree.init hook in the worktree")
    s.set_defaults(fn=verb_change_open)

    s = sub.add_parser("worktree", help="the path a worktree of the change's repository takes for a branch")
    s.add_argument("branch")
    s.add_argument("--sh", action="store_true", help="print WORKTREE_PATH=... for eval")
    s.set_defaults(fn=verb_worktree)

    s = sub.add_parser("status", help="orientation: phase, gates, tasks, open asks and inbox answers (run first)")
    s.set_defaults(fn=verb_status)
    s = sub.add_parser("inbox", help="every .orchestrator/inbox/*.org with its answer, if any (read-only)")
    s.set_defaults(fn=verb_inbox)
    s = sub.add_parser("validate", help="schema check; on a legacy file, what migrate would do")
    s.set_defaults(fn=verb_validate)
    s = sub.add_parser("migrate", help=f"convert an older-schema file (1.0 to 1.4) to {SCHEMA_VERSION}; "
                       "parks non-schema content in a sidecar; rehomes the paths")
    s.set_defaults(fn=verb_migrate)

    s = sub.add_parser("init", help="start a new cycle of the change (refuses while one is open)")
    s.add_argument("--change", help="must match the resolved change (kept for the flows' spelling)")
    s.add_argument("--test-command", help="default: the repository's test.command in config.yaml")
    s.add_argument("--cycle", help="cycle id (default cycle-<unix-ts>)")
    s.add_argument("--branch")
    s.add_argument("--history-window", type=int, default=5)
    s.add_argument("--prior-handshake", help="path relative to the home (default: newest handshake-*.json)")
    s.add_argument("--first-cycle", action="store_true", help="no prior handshake exists")
    s.set_defaults(fn=verb_init)

    s = sub.add_parser("set", help="set a top-level scalar: baseline_snapshot, baseline_status, current_branch, test_command")
    s.add_argument("fields", nargs="+", metavar="field=value")
    s.set_defaults(fn=verb_set)

    t = sub.add_parser("task", help="task add | set | remove").add_subparsers(dest="sub", required=True)
    s = t.add_parser("add")
    s.add_argument("name")
    s.add_argument("--file", required=True, help="task file path relative to the home")
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
    s.add_argument("json", help="JSON object, @file.json, or (asks) @draft.org; ids are assigned when omitted; "
                   "an ask is written to .orchestrator/inbox/<id>.org")
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

    s = sub.add_parser("context", help="a running agent's context (tokens) against thresholds.phase-agent-context "
                       "(needs no state)")
    s.add_argument("agent", help="the agent id the Agent call returned")
    s.set_defaults(fn=verb_context)

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
    try:
        home, found = find_home(args.home)
        if args.verb == "home":
            found = os.path.exists(config_path(home))
        hc = resolve_home_config(home) if found or os.path.exists(config_path(home)) else None
        sel = args.change_sel or (getattr(args, "change", None) if args.verb == "init" else None)
        change, how = resolve_change(home, sel)
        rec = read_change(home, change) if change else None
        repo = rec.get("repo") if rec else None
        code_root = rec.get("worktree") if rec else None
        store = Store(home, change, code_root, repo)
        store.hc, store.change_rec, store.change_how = hc, rec, how
        store.home_found, store.code_root_raw = found, (hc or {}).get("code_root_raw")
        result = args.fn(store, args)
    except Refusal as r:
        err = {"ok": False, "error": r.slug, "message": r.message}
        err.update(r.extra)
        print(json.dumps(err, indent=2), file=sys.stderr)
        return r.exit_code
    if args.verb == "where" and args.sh:
        print(where_sh(result, hc))
        return 0
    if args.verb == "worktree" and args.sh:
        print(f"WORKTREE_PATH={shlex.quote(result['path'])}")
        return 0
    print(json.dumps(result, indent=2 if args.verb in ("status", "counts", "validate", "migrate", "inbox", "where") else None))
    return 0 if result.get("ok", True) else 1


if __name__ == "__main__":
    sys.exit(main())
