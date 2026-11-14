#!/usr/bin/env python3
"""orchestrate report: spend and quality by role, on demand; and a running agent's context.

Outside the state layer (docs/specs/state-layer.org § Integration Points): run by the user or
/process-improvement when reviewing cycles, never by a cycle. It writes nothing.

`spend` reads every subagent transcript under the home's project directories
($CLAUDE_CONFIG_DIR or ~/.claude, projects/<home or a path under it>/*/subagents/). An agent
counts when its spawn description (agent-<id>.meta.json) reads `<role>: <task-name | cycle-id |
change>` naming a task, a cycle or the change itself (the design round, outside any cycle); the role is that word, the setting is what the transcript
ran at. Quality comes from the record: rework briefs, conformance findings and follow-ups.

Usage: python3 report.py [--home PATH] [--change NAME] spend [--cycle ID] | context <agent-id>
"""

import argparse
import glob
import json
import os
import re
import sys
from datetime import datetime

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import state as state_layer  # noqa: E402  the change, its tasks, cycles, findings and merges

Refusal = state_layer.Refusal
DESCRIPTION_RE = re.compile(r"^(?P<role>[a-z-]+):\s*(?P<ref>\S+)")


# The roles a spawn description names (SKILL.md § Agent settings): `<role>: <task-name | cycle-id>`.
# The record: no cycle file held a model or an effort; only the transcripts did (research
# agent-model-effort, E3), so the role is read from the spawn and the setting from the transcript.
AGENT_ROLES = ["phase-open", "phase-design", "phase-plan", "phase-execute", "phase-integrate",
               "phase-inline", "architect-design", "architect-premise", "architect-conformance",
               "implementor", "implementor-rework", "reviewer-task", "reviewer-whole-change", "pm"]


AGENT_MODELS = ["fable", "opus", "sonnet", "haiku"]




# Per MTok at list price: input, output, cache read, 5-minute cache write. Haiku has a second
# price above 100k tokens of prompt. Source: research agent-model-effort, E1.
MODEL_PRICES = {"fable": (10, 50, 0.25, 12.5), "opus": (4, 20, 0.20, 5), "sonnet": (2, 10, 0.10, 2.5),
                "haiku": (0.10, 0.50, 0.01, 0.125)}


HAIKU_LONG_PROMPT_PRICES = (0.50, 2.50, 0.05, 0.625)


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

# ---------------------------------------------------------------- agents

def _projects_root():
    return os.path.join(os.environ.get("CLAUDE_CONFIG_DIR") or os.path.expanduser("~/.claude"), "projects")


def _project_key(path):
    """A project directory's name: the session's cwd with every other character than a letter
    or digit written `-`."""
    return re.sub(r"[^A-Za-z0-9]", "-", os.path.realpath(path))


def _transcript_cwd(path):
    """The cwd the transcript's first line that carries one records, or None."""
    try:
        with open(path, encoding="utf-8") as f:
            for line in f:
                try:
                    cwd = json.loads(line).get("cwd")
                except (json.JSONDecodeError, AttributeError):
                    continue
                if cwd:
                    return cwd
    except OSError:
        pass
    return None


def change_agents(ch):
    """[{agent_id, role, ref, task, cycle, transcript}] for every spawn under the home whose
    description names a role and a task or cycle of the change; plus the count of the home's
    spawns that name no role (`unattributed`)."""
    tasks, cycles = ch.tasks(), ch.cycles()
    cycle_of = {}
    for c in cycles:
        for n in ch.batch(c):
            cycle_of.setdefault(n, c)
    key = _project_key(ch.home)
    out, unattributed = [], 0
    for proj in glob.glob(os.path.join(_projects_root(), "*")):
        if not os.path.basename(proj).startswith(key):  # a cheap filter; the encoding is not one-to-one
            continue
        for meta in glob.glob(os.path.join(proj, "*", "subagents", "agent-*.meta.json")):
            cwd = _transcript_cwd(meta[:-len(".meta.json")] + ".jsonl")
            if cwd is None or not state_layer._under(cwd, ch.home):
                continue
            try:
                m = json.load(open(meta))
            except (OSError, json.JSONDecodeError):
                continue
            d = DESCRIPTION_RE.match(m.get("description") or "")
            if not d or d["role"] not in AGENT_ROLES:
                unattributed += 1
                continue
            ref = d["ref"].rstrip(".,;")
            if ref in tasks:
                task, cycle = ref, cycle_of.get(ref)
            elif ref in cycles:
                task, cycle = None, ref
            elif ref == ch.name:  # the design round and other work outside a cycle
                task, cycle = None, None
            else:
                continue
            aid = os.path.basename(meta)[len("agent-"):-len(".meta.json")]
            out.append({"agent_id": aid, "role": d["role"], "task": task, "cycle": cycle,
                        "transcript": meta[:-len(".meta.json")] + ".jsonl"})
    return sorted(out, key=lambda a: a["agent_id"]), unattributed


def _override(ch, a):
    """The task file's setting for this role (`agent: implementor: sonnet, medium`), or None."""
    if not a["task"] or a["role"] not in ("implementor", "implementor-rework", "reviewer-task"):
        return None
    block = ch.tasks()[a["task"]]["fm"].get("agent")
    key = "reviewer" if a["role"] == "reviewer-task" else "implementor"
    val = block.get(key) if isinstance(block, dict) else None
    if not val:
        return None
    parts = [x.strip() for x in str(val).split(",") if x.strip()]
    return "/".join(parts + ["?"] * (2 - len(parts))) if parts else None  # a model alone: any effort


def _differs(ran, override):
    """A `model/effort` the transcript ran at against the override's; `?` on either side matches."""
    (rm, re_), (om, oe) = (ran.split("/", 1) + ["?"])[:2], (override.split("/", 1) + ["?"])[:2]
    return rm != om or "?" not in (re_, oe) and re_ != oe


def spend_by_role(ch, agents, spend):
    rows, roles, mismatches = [], {}, []
    for a in agents:
        s = spend.get(a["agent_id"])
        ran = sorted((s or {}).get("ran", {}), key=lambda k: -s["ran"][k]) if s else []
        override = _override(ch, a)
        rows.append({**{k: a[k] for k in ("agent_id", "role", "task", "cycle")}, "override": override,
                     "ran": (s or {}).get("ran"), "spend": s})
        r = roles.setdefault(a["role"], {"agents": 0, "counted": 0, "settings": {}, "overrides": 0,
                                         "minutes": [], **{k: 0 for k in SPEND_KEYS}})
        r["agents"] += 1
        r["overrides"] += bool(override)
        if s is None:
            continue
        r["counted"] += 1
        if ran:
            r["settings"][ran[0]] = r["settings"].get(ran[0], 0) + 1
        for k in SPEND_KEYS:
            r[k] += s[k]
        if s["minutes"] is not None:
            r["minutes"].append(s["minutes"])
        if override and any(_differs(k, override) for k in s["ran"]):
            mismatches.append({"agent_id": a["agent_id"], "override": override, "ran": s["ran"]})
    total = sum(r["cost_weight"] for r in roles.values())
    for r in roles.values():
        m = sorted(r.pop("minutes"))
        r["median_minutes"] = m[len(m) // 2] if m else None
        r["usd"] = round(r["usd"], 2)
        r["cost_weight_share"] = round(r["cost_weight"] / total, 3) if total else None
        r["cost_weight_per_agent"] = round(r["cost_weight"] / r["counted"]) if r["counted"] else None
    return {"by_role": roles, "by_agent": rows, "override_mismatches": mismatches}


# ---------------------------------------------------------------- quality, from the record

def quality(ch, agents, spend, cycle=None):
    """Reviewer rejections (a `<task>-rework` brief each), rework tasks, and defects found after
    done: a conformance finding of kind `issue`, a task whose discovered_from begins
    `whole-change`, or a follow-up of a merged task that neither its Implementor nor its Reviewer
    raised. All by the setting each task's Implementor ran at (`unrecorded` without one)."""
    tasks = ch.tasks()
    scope = set(ch.batch(cycle)) if cycle else set(tasks)
    rejections = {}
    for p in glob.glob(os.path.join(ch.dir, "cycles", cycle or "*", "briefs", "*-rework.md")):
        n = os.path.basename(p)[:-len("-rework.md")]
        if not n.startswith("review-"):
            rejections[n] = rejections.get(n, 0) + 1
    setting = {}
    for a in agents:
        s = spend.get(a["agent_id"])
        if a["role"] == "implementor" and a["task"] and s and s["ran"]:
            setting.setdefault(a["task"], max(s["ran"], key=s["ran"].get))
    reworked = {a["task"] for a in agents if a["role"] == "implementor-rework" and a["task"]}
    rework = sorted(n for n in scope if rejections.get(n) or n in reworked)
    defects = []
    for fid, p in ch.findings().items():
        if cycle and f"/cycles/{cycle}/" not in p:
            continue
        fm = state_layer.read_frontmatter(p)[0] or {}
        if fm.get("trigger") == "conformance" and fm.get("kind") == "issue":
            src = fm.get("discovered_from")
            defects.append({"source": fid, "against": src if src in tasks else None})
    for n in scope:
        fm = tasks.get(n, {}).get("fm", {})
        src = str(fm.get("discovered_from") or "")
        if src.startswith("whole-change"):
            defects.append({"source": n, "against": None})
        elif fm.get("discovered_by") not in ("implementor", "reviewer") and src in tasks and ch.merges(src):
            defects.append({"source": n, "against": src})
    by_setting = {}

    def bucket(key):
        return by_setting.setdefault(key, {"tasks": 0, "rejections": 0, "rework_tasks": 0, "defects_after_done": 0})
    for n in scope:
        b = bucket(setting.get(n, "unrecorded"))
        b["tasks"] += 1
        b["rejections"] += rejections.get(n, 0)
        b["rework_tasks"] += n in rework
    for d in defects:
        bucket(setting.get(d["against"], "unrecorded") if d["against"] else "whole change")["defects_after_done"] += 1
    return {"rejections": sum(rejections.get(n, 0) for n in scope), "rework_tasks": rework,
            "defects_after_done": defects, "by_implementor_setting": by_setting}


# ---------------------------------------------------------------- verbs

def verb_spend(ch, args):
    agents, unattributed = change_agents(ch)
    spend = {a["agent_id"]: agent_spend(a["agent_id"]) for a in agents}
    picked = [a for a in agents if not args.cycle or a["cycle"] == args.cycle]
    out = {"change": ch.name, "cycle": args.cycle, **spend_by_role(ch, picked, spend),
           "unattributed_spawns": unattributed, "quality": quality(ch, agents, spend, args.cycle)}
    if not args.cycle:
        out["by_cycle"] = []
        for c in ch.cycles():
            group = [a for a in agents if a["cycle"] == c]
            s = spend_by_role(ch, group, spend)
            q = quality(ch, agents, spend, c)
            out["by_cycle"].append({
                "cycle": c,
                "by_role": {r: {k: v[k] for k in ("agents", "cost_weight", "priced_weight", "usd",
                                                   "cost_weight_per_agent")} for r, v in s["by_role"].items()},
                "quality": {"rejections": q["rejections"], "rework_tasks": len(q["rework_tasks"]),
                            "defects_after_done": len(q["defects_after_done"])}})
    return out


def phase_agent_context_limit(home):
    """thresholds.phase-agent-context from the overlay, default 400000."""
    p = os.path.join(home, ".claude", "orchestrator", "config.yaml")
    if os.path.exists(p):
        m = re.search(r"^thresholds:\s*\n(?:[ \t]+.*\n)*?[ \t]+phase-agent-context:\s*(\d+)", open(p).read(), re.M)
        if m:
            return int(m.group(1))
    return 400000


def verb_context(home, args):
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
    limit = phase_agent_context_limit(home)
    return {"ok": True, "agent": args.agent, "context": used, "limit": limit,
            "over": used is not None and used > limit, "at": at, "transcript": paths[0]}


def build_parser():
    p = argparse.ArgumentParser(prog="report.py", description=__doc__.split("\n\n")[0])
    p.add_argument("--home", help="the home (default: walk up from cwd for .claude/orchestrator/config.yaml)")
    p.add_argument("--change", help="the change (default: $ORCH_CHANGE, the worktree holding cwd, the only one)")
    sub = p.add_subparsers(dest="verb", required=True)
    s = sub.add_parser("spend", help="spend and quality by role for the change, per cycle")
    s.add_argument("--cycle", help="one cycle only")
    s = sub.add_parser("context", help="a running agent's context against thresholds.phase-agent-context")
    s.add_argument("agent", help="the id the Agent call returned (a unique prefix will do)")
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    try:
        home = state_layer.find_home(args.home)
        if args.verb == "context":
            result = verb_context(home, args)
        else:
            result = verb_spend(state_layer.Change(home, state_layer.resolve_change(home, args.change)), args)
    except Refusal as r:
        print(json.dumps({"ok": False, "error": r.slug, "message": r.message, **r.extra}, indent=2), file=sys.stderr)
        return 1
    print(json.dumps(result, indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(main())
