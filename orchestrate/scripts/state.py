#!/usr/bin/env python3
"""orchestrate state layer (docs/specs/state-layer.org).

Answers two questions about a change from its files and git history, and refuses what
would make the answers false:

1. Where does this change stand, and what comes next?   `status`
2. Is this really done?                                  `gate`, `check`, `conventions`

Files and git are the record. The one file this script writes is the change's event log,
<change-dir>/events.jsonl, for the few facts files and git do not hold (a cycle's start, an
asserted check, a finding's routing, a blocker note, a journal note, an inline fix's link to
its review). An entry is never edited or removed; a correction is a new entry naming it.

Every verb prints JSON on stdout. A refusal prints {"ok": false, "error": "<slug>",
"message": "..."} on stderr and exits 1; a gate that does not pass prints its report on
stdout and exits 1.

Usage: python3 state.py [--home PATH] [--change NAME] <verb> ...   (state.py -h lists the verbs)
"""

import argparse
import fcntl
import glob
import json
import os
import re
import subprocess
import sys
import time
from datetime import datetime, timezone

PHASES = ["plan", "execute", "integrate"]
TASK_STATUSES = ["ready", "in_progress", "needs_review", "done", "failed", "blocked", "externalised"]
STOPPED = {"done", "failed", "blocked", "externalised"}
CLOSED = {"done", "externalised"}  # a task in neither is open, for the main path
# The frontmatter keys templates/task-body.md names. Anything else is a convention break.
TASK_KEYS = {"task_name", "description", "change", "status", "task_class", "on_critical_path",
             "cites_seams", "cites_scenarios", "relations", "agent", "discovered_from", "discovered_by",
             "discovered_class"}
# Facts with one source elsewhere; a copy in a file is a break that names the source.
SECOND_COPIES = {
    "task": {"merge_commit": "a task's merge commit is read from its `Merge task <name>` commit",
             "created_at": "task times come from git", "started_at": "task times come from git",
             "completed_at": "task times come from git", "reviewed_at": "task times come from git"},
    "finding": {"decoration": "a finding's routing is in the event log (`state.py route`)",
                "resolution": "a finding's resolution is in the event log (`state.py route`)"},
}
# Gate checks no script can decide; an agent asserts them with `state.py assert`.
ASSERTED = {"plan": ["batch_composed"], "execute": [], "integrate": ["open_tasks_refined_against_handshake"]}
HANDSHAKE_REQUIRED = ["design_diff", "meta_discoveries", "asks_for_user_open", "asks_for_user_resolved",
                      "user_resolved_goal_drift", "goal_drift_recommendation", "task_refinements",
                      "candidate_tasks", "journal"]
CANDIDATE_FIELDS = {"name": str, "source": str, "discovered_by": str, "discovered_class": str,
                    "critical": bool, "quote": str, "evidence": str, "placement": str}
CANDIDATE_REQUIRED = ["name", "source", "discovered_by", "critical", "quote"]
CANDIDATE_PLACEMENTS = ["in-change", ".tasks"]
DESIGN_DIFF_CHANGES = ["added", "amended", "retired"]
GOAL_DRIFT_OPTIONS = {"revise", "split", "abandon", "continue"}
DECORATIONS = ["blocking", "hardening"]
RESOLUTIONS = ["pending", "inline-fixed", "design-amended", "reverted", "accepted-with-note", "hardening-list"]
RESOLUTION_PREFIXES = ("candidate-task-", "followup-task-")
NOTE_KINDS = ["discovery", "decision", "inline-fix", "deviation", "push-back", "note"]
TEXT_CAP = 280
LOG_NAME = "events.jsonl"
# Each kind's required fields; every entry also carries seq, at, cycle and kind.
EVENT_FIELDS = {
    "cycle-started": [], "cycle-abandoned": ["why"], "asserted": ["phase", "check", "by"],
    "finding-routed": ["finding"], "blocker-note": ["task", "note"], "note": ["note_kind", "text"],
    "inline-fix-link": ["commit", "task", "review"], "correction": ["corrects", "why"],
}

MERGE_RE = re.compile(r"^Merge task (?P<name>[A-Za-z0-9._-]+?)(?P<rework> \(rework\))?(?::\s.*)?$")
TASK_BRANCH_RE = re.compile(r"\btask-(?P<name>[A-Za-z0-9._-]+?)-\d{6,}\b")
CLOSE_RE = re.compile(r"^Orchestrator: (?P<cycle>cycle-[\w-]+) (?P<phase>plan|execute|integrate) closed$")
CLOSE_LIKE_RE = re.compile(r"^Orchestrator: (?P<cycle>cycle-[\w-]+) (?P<phase>plan|execute|integrate)\b")
INLINE_FIX_RE = re.compile(r"^Inline fix \(review (?P<task>\S+) #(?P<n>\d+)\)(?::\s.*)?$")
NON_TASK_BRIEFS = re.compile(r"^(review-.*|.*-rework|premise-check|conformance)$")


class Refusal(Exception):
    def __init__(self, slug, message, **extra):
        super().__init__(message)
        self.slug, self.message, self.extra = slug, message, extra


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def git(repo, *args):
    """stdout of a git command, or None when it fails."""
    r = subprocess.run(["git", "-C", repo, *args], capture_output=True, text=True)
    return r.stdout if r.returncode == 0 else None


# ---------------------------------------------------------------- reading files

def _yaml_scalar(raw):
    raw = raw.strip()
    if len(raw) >= 2 and raw[0] in "\"'" and raw[-1] == raw[0]:
        return raw[1:-1]
    raw = re.split(r"\s+#", raw)[0].strip()
    return "" if raw.startswith("#") else raw


def parse_yaml_subset(text):
    """Frontmatter YAML without a YAML library: nested maps, scalars, lists of scalars and of
    one-key maps (`- <task>: { status: merged }`), flow lists `[a, b]`, comments."""
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

    def flow(val):
        return [_yaml_scalar(x) for x in val[1:-1].split(",") if x.strip()]

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
                out[key] = flow(val)
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
        if lines[pos[0]][1].startswith("- "):
            return seq(indent)
        if lines[pos[0]][1] == "[]":
            pos[0] += 1
            return []
        return mapping(indent)

    return mapping(lines[0][0]) if lines else {}


def read_frontmatter(path):
    """(dict, None) for a file opening with a `---` block; (None, problem) otherwise."""
    try:
        text = open(path, encoding="utf-8").read()
    except OSError as e:
        return None, str(e)
    m = re.match(r"^---\n(.*?)\n---\s*(\n|$)", text, re.S)
    if not m:
        return None, "no frontmatter"
    return parse_yaml_subset(m.group(1)), None


def _names(value):
    """A frontmatter list of names: scalars, or one-key maps (`- a: { status: merged }`)."""
    if not isinstance(value, list):
        return [] if value in (None, "") else [str(value)]
    return [next(iter(v)) if isinstance(v, dict) else str(v) for v in value if v not in (None, "")]


def _truthy(value):
    return str(value).strip().lower() in ("true", "yes", "1")


class Ask:
    """An ask file (templates/ask.md), read only: the drawer, the title, Options, Answer, Readback."""

    def __init__(self, path):
        self.path = path
        text = open(path, encoding="utf-8").read().lstrip("﻿")
        self.props, sections, cur = {}, {}, None
        in_drawer = False
        for line in text.split("\n"):
            s = line.strip()
            if cur is None and s.upper() == ":PROPERTIES:":
                in_drawer = True
                continue
            if in_drawer:
                if s.upper() == ":END:":
                    in_drawer = False
                else:
                    m = re.fullmatch(r":([A-Za-z_]+):\s*(.*)", s)
                    if m:
                        self.props[m.group(1).upper()] = m.group(2).strip()
                continue
            if line.startswith("* "):
                cur = _canon_section(line[2:])
                sections[cur] = []
            elif cur is not None and not re.match(r"\s*#(\s|$)", line):
                sections[cur].append(line)
        self.title = (re.search(r"^#\+title:\s*(.*)$", text, re.I | re.M) or [None, None])[1]
        self.id = self.props.get("ID") or os.path.basename(path)[:-4]
        self.kind = self.props.get("KIND")
        self.status = self.props.get("STATUS") or "open"
        self.blocks = [b.strip() for b in self.props.get("BLOCKS", "").split(",") if b.strip()]
        self.options = [re.sub(r"\s+::.*$", "", re.sub(r"^\s*[-+]\s+", "", l)).strip()
                        for l in sections.get("options", []) if re.match(r"\s*[-+]\s+\S", l)]
        self.answer = "\n".join(sections.get("answer", [])).strip() or None
        self.readback = "\n".join(sections.get("readback", [])).strip() or None

    def answer_label(self):
        """The option the answer names exactly, or None."""
        norm = lambda s: re.sub(r"\s+", " ", (s or "").casefold()).strip().rstrip(".!?;:,")
        labels = {norm(o): o for o in self.options}
        return labels.get(norm(self.answer)) if self.answer else None

    def summary(self):
        return {"id": self.id, "kind": self.kind, "status": self.status, "blocks": self.blocks,
                "question": self.title, "answered": bool(self.answer), "readback": bool(self.readback),
                "path": self.path}


def _canon_section(name):
    n = name.strip().lower()
    return {"decision": "answer"}.get(n, n)


# ---------------------------------------------------------------- the change

def find_home(explicit=None):
    if explicit:
        return os.path.abspath(explicit)
    d = os.getcwd()
    while True:
        if os.path.exists(os.path.join(d, ".claude", "orchestrator", "config.yaml")):
            return d
        parent = os.path.dirname(d)
        if parent == d:
            raise Refusal("no_home", f"no .claude/orchestrator/config.yaml walking up from {os.getcwd()}; "
                                     "pass --home")
        d = parent


def _under(path, parent):
    rp, pp = os.path.realpath(path), os.path.realpath(parent)
    return rp == pp or rp.startswith(pp.rstrip(os.sep) + os.sep)


def resolve_change(home, explicit=None):
    """--change, then $ORCH_CHANGE, then the open change whose worktree holds the cwd, then the
    only open change."""
    name = explicit or os.environ.get("ORCH_CHANGE")
    if name:
        return name
    recs = []
    for p in sorted(glob.glob(os.path.join(home, ".orchestrator", "changes", "*", "change.json"))):
        try:
            recs.append(json.load(open(p)))
        except (OSError, json.JSONDecodeError):
            continue
    for r in recs:
        wt = r.get("worktree")
        if wt and os.path.realpath(wt) != os.path.realpath(home) and _under(os.getcwd(), wt):
            return r["change"]
    if len(recs) == 1:
        return recs[0]["change"]
    raise Refusal("no_change", "name the change with --change or $ORCH_CHANGE",
                  changes=[r.get("change") for r in recs])


class Change:
    """One change's record, read from its files and git; every reader is cached for the call."""

    def __init__(self, home, name):
        self.home, self.name = home, name
        self.dir = os.path.join(home, ".orchestrator", "changes", name)
        self.rec = None
        p = os.path.join(self.dir, "change.json")
        if os.path.exists(p):
            try:
                self.rec = json.load(open(p))
            except (OSError, json.JSONDecodeError) as e:
                raise Refusal("bad_change_json", f"{p}: {e}")
        self.worktree = (self.rec or {}).get("worktree")
        self.branch = (self.rec or {}).get("branch") or name
        self.base = (self.rec or {}).get("base")
        self.record_dir = os.path.join(self.worktree, "docs", "changes", name) if self.worktree else None
        self._cache = {}

    def _cached(self, key, fn):
        if key not in self._cache:
            self._cache[key] = fn()
        return self._cache[key]

    def rel(self, path):
        return os.path.relpath(path, self.home)

    # -- the event log
    @property
    def log_path(self):
        return os.path.join(self.dir, LOG_NAME)

    def raw_events(self):
        def read():
            if not os.path.exists(self.log_path):
                return []
            out = []
            for i, line in enumerate(open(self.log_path, encoding="utf-8"), 1):
                if line.strip():
                    try:
                        out.append(json.loads(line))
                    except json.JSONDecodeError:
                        raise Refusal("bad_log", f"{self.rel(self.log_path)} line {i} is not JSON")
            return out
        return self._cached("raw_events", read)

    def events(self, kind=None):
        """The log with every corrected entry left out."""
        def applied():
            raw = self.raw_events()
            corrected = {e["corrects"] for e in raw if e["kind"] == "correction"}
            return [e for e in raw if e["seq"] not in corrected and e["kind"] != "correction"]
        evs = self._cached("events", applied)
        return [e for e in evs if kind is None or e["kind"] == kind]

    # -- cycles and phases
    def cycles(self):
        return [e["cycle"] for e in self.events("cycle-started")]

    def home_subjects(self):
        def read():
            out = git(self.home, "log", "--format=%H%x1f%s")
            if out is None:
                raise Refusal("home_not_git", f"{self.home} is not a git repository with a commit; the "
                                              "state layer reads phase closes from its history")
            return [tuple(l.split("\x1f", 1)) for l in out.splitlines() if "\x1f" in l]
        return self._cached("home_subjects", read)

    def closed_phases(self, cycle):
        return {m["phase"] for _, s in self.home_subjects() if (m := CLOSE_RE.match(s)) and m["cycle"] == cycle}

    def abandoned(self):
        return {e["cycle"] for e in self.events("cycle-abandoned")}

    def open_cycle(self):
        """(cycle, phase) of the open cycle, or (None, None)."""
        cycles = self.cycles()
        if not cycles:
            return None, None
        last = cycles[-1]
        closed = self.closed_phases(last)
        if "integrate" in closed or last in self.abandoned():
            return None, None
        return last, next(p for p in PHASES if p not in closed)

    # -- the change branch
    def first_parent(self):
        """[(sha, parent count, subject)] on the change branch since its base, oldest first."""
        def read():
            if not self.worktree:
                return []
            rng = f"{self.base}..{self.branch}" if self.base else self.branch
            out = git(self.worktree, "log", "--first-parent", "--reverse", "--format=%H%x1f%P%x1f%s", rng)
            if out is None:
                raise Refusal("branch_unreadable", f"git log {rng} failed in {self.worktree}")
            rows = []
            for l in out.splitlines():
                sha, parents, subject = l.split("\x1f", 2)
                rows.append((sha, len(parents.split()), subject))
            return rows
        return self._cached("first_parent", read)

    def merges(self, task):
        """Every `Merge task <task>` commit on the change branch, oldest first."""
        return [sha for sha, n, s in self.first_parent() if n > 1 and (m := MERGE_RE.match(s)) and m["name"] == task]

    def malformed_merges(self, task=None):
        """[(sha, subject, task)] merges of a task branch whose subject is not the form."""
        out = []
        for sha, n, s in self.first_parent():
            if n < 2 or MERGE_RE.match(s):
                continue
            m = TASK_BRANCH_RE.search(s)
            if m and (task is None or m["name"] == task):
                out.append((sha, s, m["name"]))
        return out

    # -- files in the change directory
    def tasks(self):
        """{name: {"path", "fm", "problem"}} from tasks/open and tasks/closed."""
        def read():
            out = {}
            for sub in ("open", "closed"):
                for p in sorted(glob.glob(os.path.join(self.dir, "tasks", sub, "*.md"))):
                    fm, problem = read_frontmatter(p)
                    out[os.path.basename(p)[:-3]] = {"path": p, "fm": fm or {}, "problem": problem}
            return out
        return self._cached("tasks", read)

    def task(self, name):
        t = self.tasks().get(name)
        if t is None:
            raise Refusal("no_task", f"no task file {name}.md under {self.rel(self.dir)}/tasks/")
        return t

    def cycle_dir(self, cycle):
        return os.path.join(self.dir, "cycles", cycle)

    def batch(self, cycle):
        """The task names under cycles/<cycle>/briefs/, less review, rework, premise-check and
        conformance briefs."""
        names = [os.path.basename(p)[:-3] for p in glob.glob(os.path.join(self.cycle_dir(cycle), "briefs", "*.md"))]
        return sorted(n for n in names if not NON_TASK_BRIEFS.match(n))

    def reviews(self, task):
        """[(path, frontmatter)] of every review of the task, in any cycle."""
        paths = glob.glob(os.path.join(self.dir, "cycles", "*", "reviews", f"{task}.md")) + \
            glob.glob(os.path.join(self.dir, "cycles", "*", "reviews", f"{task}-rework.md"))
        return [(p, read_frontmatter(p)[0] or {}) for p in sorted(paths)]

    def findings(self):
        """{finding id: path}."""
        def read():
            out = {}
            for p in sorted(glob.glob(os.path.join(self.dir, "cycles", "*", "findings", "*.md"))):
                fm, _ = read_frontmatter(p)
                out[(fm or {}).get("finding_id") or os.path.basename(p)[:-3]] = p
            return out
        return self._cached("findings", read)

    def routing(self):
        """{finding id: {"decoration", "resolution"}}, the latest value of each from the log."""
        out = {}
        for e in self.events("finding-routed"):
            r = out.setdefault(e["finding"], {"decoration": None, "resolution": None})
            for k in ("decoration", "resolution"):
                if e.get(k):
                    r[k] = e[k]
        return out

    def asks(self, where="inbox"):
        """Ask files in the inbox, or ("all") the inbox and every cycle's asks/."""
        pats = [os.path.join(self.dir, "inbox", "*.org")]
        if where == "all":
            pats.append(os.path.join(self.dir, "cycles", "*", "asks", "*.org"))
        return [Ask(p) for pat in pats for p in sorted(glob.glob(pat))]

    def find_ask(self, ask_id):
        return next((a for a in self.asks("all") if a.id == ask_id), None)

    def handshakes(self):
        """[(cycle, path)] oldest first, by the cycle id's number."""
        out = []
        for p in glob.glob(os.path.join(self.dir, "handshake-*.json")):
            cycle = os.path.basename(p)[len("handshake-"):-len(".json")]
            out.append((cycle, p))
        key = lambda cp: [int(x) if x.isdigit() else x for x in re.split(r"(\d+)", cp[0])]
        return sorted(out, key=key)

    def candidate_names(self):
        names = set()
        for _, p in self.handshakes():
            try:
                names |= {c.get("name") for c in json.load(open(p)).get("candidate_tasks", []) if isinstance(c, dict)}
            except (OSError, json.JSONDecodeError, AttributeError):
                continue
        return names

    # -- the record in the code root
    def intent(self):
        """{"path", "exists", "approved", "status": [words]}."""
        p = os.path.join(self.record_dir, "intent.org") if self.record_dir else None
        if not p or not os.path.exists(p):
            return {"path": p, "exists": False, "approved": False, "status": []}
        text = open(p, encoding="utf-8").read()
        approved = re.search(r"^#\+APPROVED:[ \t]*(\S.*)$", text, re.M)
        status = re.search(r"^#\+STATUS:[ \t]*(.*)$", text, re.M)
        return {"path": p, "exists": True, "approved": bool(approved),
                "status": status.group(1).split() if status else []}

    def design_has_seams(self):
        p = os.path.join(self.record_dir, "design.org") if self.record_dir else None
        if not p or not os.path.exists(p):
            return False
        m = re.search(r"^\* Seams[ \t]*\n(.*?)(?=^\* |\Z)", open(p, encoding="utf-8").read(), re.S | re.M)
        if not m:
            return False
        table = [l.strip() for l in m.group(1).splitlines() if l.strip().startswith("|")]
        sep = next((i for i, l in enumerate(table) if re.match(r"^\|[-+|\s]+\|?$", l)), None)
        return sep is not None and any(not re.match(r"^\|[-+|\s]+\|?$", l) for l in table[sep + 1:])


# ---------------------------------------------------------------- the checks

def done_problems(ch, name):
    """Why the task is not done, or [] with its merge commit. A merge in another form fails the
    check rather than being read as the task's merge."""
    problems = []
    merges = ch.merges(name)
    for sha, subject, _ in ch.malformed_merges(name):
        problems.append(f"merge commit {sha[:12]} ({subject!r}) is not in the form `Merge task {name}`")
    if not merges:
        problems.append(f"no `Merge task {name}` commit on {ch.branch}")
        return problems, None
    sha = merges[-1]
    named = [p for p, fm in ch.reviews(name) if _same_sha(fm.get("merge_commit"), sha)]
    if not named:
        problems.append(f"no review file (cycles/*/reviews/{name}.md or {name}-rework.md) names its merge "
                        f"commit {sha[:12]}")
    return problems, sha


def _same_sha(a, b):
    a = str(a or "").strip()
    return len(a) >= 7 and b.startswith(a)


def start_problems(ch, name):
    t = ch.task(name)
    return [f"blocked_by {b} has no `Merge task {b}` commit on {ch.branch}"
            for b in _names((t["fm"].get("relations") or {}).get("blocked_by")) if not ch.merges(b)]


def provenance_problems(ch, name):
    """A follow-up (a handshake candidate, or a task carrying any discovered_* field) names its source."""
    fm = ch.task(name)["fm"]
    follow_up = name in ch.candidate_names() or any(fm.get(k) for k in ("discovered_from", "discovered_by",
                                                                         "discovered_class"))
    if not follow_up:
        return []
    return [f"follow-up task {name} has no {k}" for k in ("discovered_from", "discovered_by") if not fm.get(k)]


def main_path_ended(ch, intent=None):
    """No open task on the main path, at least one on it, and no mvp or done status."""
    intent = intent or ch.intent()
    if intent["status"][:1] in (["mvp"], ["done"]):
        return False
    on_path = [t for t in ch.tasks().values() if _truthy(t["fm"].get("on_critical_path"))]
    return bool(on_path) and all(t["fm"].get("status") in CLOSED for t in on_path)


def handshake_problems(path, prior=False):
    if not path or not os.path.exists(path):
        return [f"{path} does not exist"]
    try:
        h = json.load(open(path))
    except (OSError, json.JSONDecodeError) as e:
        return [f"{path}: {e}"]
    if not isinstance(h, dict):
        return [f"{path}: not a JSON object"]
    required = list(HANDSHAKE_REQUIRED)
    if prior and "pm_digest_path" in h:
        # Written by the state tool before this layer: its goal-drift recommendation is in the digest.
        required.remove("goal_drift_recommendation")
        h.setdefault("candidate_tasks", [])
    problems = [f"missing field {k}" for k in required if k not in h]
    problems += _candidate_problems(h.get("candidate_tasks", []))
    problems += _design_diff_problems(h.get("design_diff", []))
    return problems


def _candidate_problems(cands):
    if not isinstance(cands, list):
        return ["candidate_tasks: expected a JSON array"]
    out, names = [], set()
    for i, c in enumerate(cands):
        w = f"candidate_tasks[{i}]"
        if not isinstance(c, dict):
            out.append(f"{w}: not an object")
            continue
        out += [f"{w}.{k}: missing" for k in CANDIDATE_REQUIRED if k not in c]
        for k, v in c.items():
            if k not in CANDIDATE_FIELDS:
                out.append(f"{w}.{k}: unknown field")
            elif not isinstance(v, CANDIDATE_FIELDS[k]) or (CANDIDATE_FIELDS[k] is str and not v.strip()):
                out.append(f"{w}.{k}: expected non-empty {CANDIDATE_FIELDS[k].__name__}")
        if c.get("placement", "in-change") not in CANDIDATE_PLACEMENTS:
            out.append(f"{w}.placement: not one of {CANDIDATE_PLACEMENTS}")
        if c.get("name") in names:
            out.append(f"{w}.name: {c['name']!r} named twice")
        names.add(c.get("name"))
    return out


def _design_diff_problems(diff):
    if not isinstance(diff, list):
        return ["design_diff: expected a JSON array"]
    out = []
    for i, e in enumerate(diff):
        w = f"design_diff[{i}]"
        if not isinstance(e, dict):
            out.append(f"{w}: not an object")
            continue
        if not isinstance(e.get("seam"), str) or not e["seam"].strip():
            out.append(f"{w}.seam: expected non-empty string")
        if e.get("change") not in DESIGN_DIFF_CHANGES:
            out.append(f"{w}.change: not one of {DESIGN_DIFF_CHANGES}")
        if not isinstance(e.get("ref"), str):
            out.append(f"{w}.ref: expected string")
    return out


def evaluate_gate(ch, cycle, phase):
    """Every check of the phase's gate, computed afresh, with the asserted ones from the log."""
    checks = []

    def put(name, problems, fine):
        checks.append({"check": name, "kind": "computed", "ok": not problems,
                       "reason": "; ".join(problems) if problems else fine})

    batch = ch.batch(cycle)
    tasks = ch.tasks()
    if phase == "plan":
        started = next((e for e in ch.events("cycle-started") if e["cycle"] == cycle), {})
        prior = started.get("prior_handshake")
        put("prior_handshake_complete",
            [f"{prior}: {p}" for p in handshake_problems(os.path.join(ch.home, prior), prior=True)] if prior else [],
            f"{prior} has every required field" if prior else "first cycle: no prior handshake")
        missing = [n for n in batch if n not in tasks]
        put("batch_cites_seams",
            (["no batch: no task brief under " + ch.rel(os.path.join(ch.cycle_dir(cycle), "briefs"))] if not batch else [])
            + [f"no task file for brief {n}" for n in missing]
            + [f"{n} cites no seam" for n in batch if n in tasks and not _names(tasks[n]["fm"].get("cites_seams"))],
            "every task in the batch cites at least one seam")
        put("follow_ups_name_their_source",
            [p for n in batch if n in tasks for p in provenance_problems(ch, n)],
            "every follow-up in the batch names discovered_from and discovered_by")
        drift = []
        for a in ch.asks():
            if a.kind == "decision" and {o.casefold() for o in a.options} >= GOAL_DRIFT_OPTIONS:
                if a.status not in ("answered", "applied"):
                    drift.append(f"goal-drift ask {a.id} is {a.status}")
                elif (a.answer_label() or "").casefold() not in GOAL_DRIFT_OPTIONS:
                    drift.append(f"goal-drift ask {a.id}'s answer names none of revise/split/abandon/continue")
        put("goal_drift_signed_off", drift, "no goal-drift ask waits on the user")
        put("mvp_decision_not_due", ["the main path has ended: integrate's MVP decision comes first"]
            if main_path_ended(ch) else [], "an open task remains on the main path, or none is on it yet")
    elif phase == "execute":
        problems = [] if batch else ["no batch: no task brief under " + ch.rel(os.path.join(ch.cycle_dir(cycle), "briefs"))]
        for n in batch:
            st = tasks.get(n, {}).get("fm", {}).get("status")
            if n not in tasks:
                problems.append(f"no task file for brief {n}")
            elif st not in TASK_STATUSES:
                problems.append(f"{n}: status {st!r} is not in the task status set")
            elif st not in STOPPED:
                problems.append(f"{n} is {st}")
        put("batch_stopped", problems, "every task in the batch is done, failed, blocked or externalised")
        problems = []
        for n in batch:
            if tasks.get(n, {}).get("fm", {}).get("status") == "done":
                problems += [f"{n}: {p}" for p in done_problems(ch, n)[0]]
        put("done_tasks_merged_and_reviewed", problems,
            "every done task has its merge commit and a review naming it")
        order = {sha: i for i, (sha, _, _) in enumerate(ch.first_parent())}
        problems = []
        for n in batch:
            mine = ch.merges(n)
            if not mine or n not in tasks:
                continue
            for b in _names((tasks[n]["fm"].get("relations") or {}).get("blocked_by")):
                theirs = ch.merges(b)
                if not theirs or order[theirs[0]] > order[mine[0]]:
                    problems.append(f"{n} merged before its blocker {b}")
        put("blocked_by_order", problems, "every merged task merged after its blocked_by tasks")
    elif phase == "integrate":
        routing = ch.routing()
        put("blocking_findings_resolved",
            [f"finding {f} is routed blocking and {r['resolution'] or 'pending'}" for f, r in sorted(routing.items())
             if r["decoration"] == "blocking" and (r["resolution"] or "pending") == "pending"],
            "no blocking finding is pending")
        hpath = os.path.join(ch.dir, f"handshake-{cycle}.json")
        hp = handshake_problems(hpath)
        put("handshake_written", [f"{ch.rel(hpath)}: {p}" for p in hp], f"{ch.rel(hpath)} has every required field")
        if hp:
            put("asks_routed", ["the handshake is not written yet"], "")
        else:
            listed = {a.get("id") if isinstance(a, dict) else a for a in json.load(open(hpath))["asks_for_user_open"]}
            problems = []
            for a in ch.asks():
                if a.status == "open" and a.id not in listed:
                    problems.append(f"open ask {a.id} is not in the handshake's asks_for_user_open")
                if a.status == "open" and a.answer and not a.readback:
                    problems.append(f"ask {a.id} has an answer not acted on (apply it, or write the readback)")
            put("asks_routed", problems, "every open ask is in the handshake and no answer waits")
    asserted = {e["check"]: e for e in ch.events("asserted") if e["cycle"] == cycle and e["phase"] == phase}
    for name in ASSERTED[phase]:
        e = asserted.get(name)
        checks.append({"check": name, "kind": "asserted", "ok": bool(e), "by": e and e["by"], "at": e and e["at"],
                       "reason": "asserted" if e else f"not asserted (state.py assert {name} --by <role>)"})
    return {"cycle": cycle, "phase": phase, "passes": all(c["ok"] for c in checks), "checks": checks}


def convention_breaks(ch):
    """Every break of the forms in the spec's § Conventions: [{"where", "problem", "expected"}]."""
    out = []

    def brk(where, problem, expected):
        out.append({"where": where, "problem": problem, "expected": expected})

    for sha, subject, task in ch.malformed_merges():
        brk(sha, f"merge subject {subject!r}", f"Merge task {task}")
    for sha, n, s in ch.first_parent():
        if s.startswith("Inline fix") and not INLINE_FIX_RE.match(s):
            brk(sha, f"inline-fix subject {s!r}", "Inline fix (review <task> #<n>)")
    cycles = set(ch.cycles())
    for sha, s in ch.home_subjects():
        m = CLOSE_LIKE_RE.match(s)
        if m and m["cycle"] in cycles and not CLOSE_RE.match(s):
            brk(sha, f"phase subject {s!r} is not read as a close", f"Orchestrator: {m['cycle']} {m['phase']} closed")
    tasks = ch.tasks()
    for name, t in tasks.items():
        where = ch.rel(t["path"])
        if t["problem"]:
            brk(where, t["problem"], "a `---` frontmatter block (templates/task-body.md)")
            continue
        fm = t["fm"]
        if fm.get("task_name") not in (None, name):
            brk(where, f"task_name {fm.get('task_name')!r} differs from the file name", name)
        if fm.get("status") not in TASK_STATUSES:
            brk(where, f"status {fm.get('status')!r} is not in the task status set", " | ".join(TASK_STATUSES))
        for k in fm:
            if k in SECOND_COPIES["task"]:
                brk(where, f"key {k}: {SECOND_COPIES['task'][k]}", f"no {k} key")
            elif k not in TASK_KEYS:
                brk(where, f"unknown key {k}", "the keys templates/task-body.md names")
    for fid, p in ch.findings().items():
        fm, _ = read_frontmatter(p)
        for k in fm or {}:
            if k in SECOND_COPIES["finding"]:
                brk(ch.rel(p), f"key {k}: {SECOND_COPIES['finding'][k]}", f"no {k} key")
    for p in sorted(glob.glob(os.path.join(ch.dir, "cycles", "*", "reviews", "*.md"))):
        n = os.path.basename(p)[:-3]
        if n != "whole-change" and re.sub(r"-rework$", "", n) not in tasks:
            brk(ch.rel(p), "review file names no task", "reviews/<task>.md or <task>-rework.md")
    for p in sorted(glob.glob(os.path.join(ch.dir, "cycles", "*", "briefs", "*.md"))):
        n = os.path.basename(p)[:-3]
        base = re.sub(r"-rework$", "", re.sub(r"^review-", "", n))
        if n not in ("premise-check", "conformance", "review-whole-change") and base not in tasks:
            brk(ch.rel(p), "brief file names no task",
                "briefs/<task>.md, <task>-rework.md, review-<task>.md, premise-check.md or conformance.md")
    return out


# ---------------------------------------------------------------- where the change stands

def where(ch):
    """The stage, the next step and what blocks it; one answer for any session at one commit."""
    out = {"change": ch.name, "stage": None, "next": None, "cycle": None, "phase": None}
    if ch.rec is None:
        return {**out, "stage": "not open", "next": "open the change (flows/open.md)"}
    intent = ch.intent()
    out["intent"] = {"approved": intent["approved"], "status": " ".join(intent["status"]) or None}
    blocking = [a.summary() for a in ch.asks() if a.status in ("open", "answered") and a.blocks]
    out["asks"] = blocking
    status = intent["status"]
    cycle, phase = ch.open_cycle()
    if not intent["exists"]:
        out.update(stage="open, no intent", next="write the intent (flows/open.md)")
    elif status[:1] == ["done"]:
        out.update(stage="done", next="nothing")
    elif status[:1] == ["mvp"] and len(status) >= 4 and _unanswered(ch, status[3]):
        out.update(stage="MVP decision", next=f"wait for the user's answer to {status[3]}")
    elif not ch.design_has_seams():
        out.update(stage="intent, no design", next="the design round (flows/design.md)")
    elif cycle:
        gate = evaluate_gate(ch, cycle, phase)
        out.update(stage="cycle open", cycle=cycle, phase=phase, gate=gate)
        if gate["passes"]:
            after = {"plan": "execute", "execute": "integrate", "integrate": "the next plan"}[phase]
            out["next"] = (f"close {phase}: commit `Orchestrator: {cycle} {phase} closed` in the home, "
                           f"then {after}")
        else:
            out["next"] = f"resume {phase} (flows/{phase}.md)"
    elif main_path_ended(ch, intent):
        out.update(stage="main path ended",
                   next="integrate's MVP decision (flows/integrate.md § 9), not another plan")
    else:
        out.update(stage="designed, no cycle open", next="start a plan: state.py cycle start (flows/plan.md)")
    breaks = convention_breaks(ch)
    if breaks:
        out["convention_breaks"] = len(breaks)
    return out


def _unanswered(ch, ask_id):
    a = ch.find_ask(ask_id)
    return a is None or not (a.answer or a.status in ("answered", "applied"))


# ---------------------------------------------------------------- writing the log

def append_event(ch, kind, cycle, **fields):
    """Appends one entry under a lock and returns it. The only write the state layer makes."""
    missing = [k for k in EVENT_FIELDS[kind] if fields.get(k) in (None, "")]
    if missing:
        raise Refusal("bad_entry", f"a {kind} entry needs {', '.join(missing)}")
    os.makedirs(ch.dir, exist_ok=True)
    with open(ch.log_path, "a+", encoding="utf-8") as f:
        fcntl.flock(f, fcntl.LOCK_EX)
        f.seek(0)
        seq = sum(1 for line in f if line.strip()) + 1
        entry = {"seq": seq, "at": now_iso(), "cycle": cycle, "kind": kind,
                 **{k: v for k, v in fields.items() if v is not None}}
        f.seek(0, os.SEEK_END)
        f.write(json.dumps(entry, ensure_ascii=False) + "\n")
        f.flush()
        os.fsync(f.fileno())
    ch._cache.clear()
    return entry


def _require_open_change(ch):
    if ch.rec is None:
        raise Refusal("change_not_open", f"{ch.name} has no change.json; open it first (flows/open.md)")


def _require_open_cycle(ch):
    cycle, phase = ch.open_cycle()
    if not cycle:
        raise Refusal("no_open_cycle", f"{ch.name} has no open cycle")
    return cycle, phase


def _cap(text, what="text"):
    if len(text) > TEXT_CAP:
        raise Refusal("too_long", f"{what} is {len(text)} characters; the cap is {TEXT_CAP}: "
                                  "write the long form in a file and pass --path")
    return text


# ---------------------------------------------------------------- verbs

def verb_status(ch, args):
    return where(ch)


def verb_gate(ch, args):
    cycle, phase = _require_open_cycle(ch)
    want = args.phase or phase
    report = evaluate_gate(ch, cycle, want)
    return report, (0 if report["passes"] else 1)


def verb_conventions(ch, args):
    breaks = convention_breaks(ch)
    return {"ok": not breaks, "breaks": breaks}


def verb_check(ch, args):
    name = args.task
    if args.what == "done":
        problems, sha = done_problems(ch, name)
        if problems:
            raise Refusal("not_done", f"{name} is not done: " + "; ".join(problems), problems=problems)
        return {"ok": True, "task": name, "merge_commit": sha}
    if args.what == "start":
        problems = start_problems(ch, name)
        if problems:
            raise Refusal("blocked", f"{name} may not start: " + "; ".join(problems), problems=problems)
        return {"ok": True, "task": name}
    problems = provenance_problems(ch, name)
    fm = ch.task(name)["fm"]
    if fm.get("status") not in TASK_STATUSES:
        problems.append(f"status {fm.get('status')!r} is not in the task status set")
    if problems:
        raise Refusal("bad_task", f"{name}: " + "; ".join(problems), problems=problems)
    return {"ok": True, "task": name}


def verb_cycle_start(ch, args):
    _require_open_change(ch)
    cycle, _ = ch.open_cycle()
    if cycle:
        raise Refusal("cycle_open", f"{cycle} is open; close it or `state.py cycle abandon --why`")
    stage = where(ch)
    if stage["stage"] == "main path ended":
        raise Refusal("mvp_decision_due", "the main path has ended: integrate's MVP decision "
                                          "(flows/integrate.md § 9) comes before another plan")
    if stage["stage"] != "designed, no cycle open":
        raise Refusal("not_ready", f"{ch.name} is at {stage['stage']!r}: {stage['next']}")
    prior = ch.handshakes()[-1][1] if ch.handshakes() else None
    if prior:
        problems = handshake_problems(prior, prior=True)
        if problems:
            raise Refusal("bad_prior_handshake", f"{ch.rel(prior)}: " + "; ".join(problems), problems=problems)
    elif ch.cycles() or glob.glob(os.path.join(ch.dir, "cycles", "*")):
        raise Refusal("no_prior_handshake", f"{ch.name} has run a cycle and no handshake-<cycle-id>.json is in "
                                            f"{ch.rel(ch.dir)}")
    new = args.cycle or f"cycle-{int(time.time())}"
    if new in ch.cycles():
        raise Refusal("cycle_exists", f"{new} was started before")
    return append_event(ch, "cycle-started", new, prior_handshake=ch.rel(prior) if prior else None)


def verb_cycle_abandon(ch, args):
    cycle, _ = _require_open_cycle(ch)
    return append_event(ch, "cycle-abandoned", cycle, why=_cap(args.why, "--why"))


def verb_assert(ch, args):
    cycle, phase = _require_open_cycle(ch)
    owner = next((p for p, cs in ASSERTED.items() if args.check in cs), None)
    if owner is None:
        computed = {c["check"] for p in PHASES for c in evaluate_gate(ch, cycle, p)["checks"]}
        if args.check in computed:
            raise Refusal("computed_check", f"{args.check} is computed; it cannot be asserted")
        raise Refusal("unknown_check", f"{args.check} is no gate's check; asserted checks: "
                                       + ", ".join(c for cs in ASSERTED.values() for c in cs))
    if owner != phase:
        raise Refusal("wrong_phase", f"{args.check} is {owner}'s; {cycle} is in {phase}")
    return append_event(ch, "asserted", cycle, phase=owner, check=args.check, by=args.by)


def verb_route(ch, args):
    if args.finding not in ch.findings():
        raise Refusal("no_finding", f"no finding file for {args.finding} under cycles/*/findings/")
    if not (args.decoration or args.resolution):
        raise Refusal("bad_entry", "route needs --decoration, --resolution or both")
    r = args.resolution
    if r and r not in RESOLUTIONS and not r.startswith(RESOLUTION_PREFIXES):
        raise Refusal("bad_resolution", f"{r!r}: one of {', '.join(RESOLUTIONS)}, or candidate-task-<name>, "
                                        "followup-task-<name>")
    return append_event(ch, "finding-routed", ch.open_cycle()[0], finding=args.finding,
                        decoration=args.decoration, resolution=r)


def verb_blocker(ch, args):
    ch.task(args.task)
    return append_event(ch, "blocker-note", ch.open_cycle()[0], task=args.task, note=_cap(args.note, "the note"))


def verb_note(ch, args):
    return append_event(ch, "note", ch.open_cycle()[0], note_kind=args.kind, text=_cap(args.text),
                        ref=args.ref, by=args.by, path=args.path)


def verb_fix_link(ch, args):
    ch.task(args.task)
    if not any(sha.startswith(args.commit) for sha, _, _ in ch.first_parent()):
        raise Refusal("no_commit", f"{args.commit} is not on {ch.branch}'s first parent since its base")
    return append_event(ch, "inline-fix-link", ch.open_cycle()[0], commit=args.commit, task=args.task,
                        review=args.review)


def verb_correct(ch, args):
    target = next((e for e in ch.raw_events() if e["seq"] == args.seq), None)
    if target is None:
        raise Refusal("no_entry", f"no entry {args.seq} in {ch.rel(ch.log_path)}")
    if target["kind"] == "correction":
        raise Refusal("bad_entry", "a correction is not corrected; record the fact again")
    return append_event(ch, "correction", target.get("cycle"), corrects=args.seq, why=_cap(args.why, "--why"))


def verb_log(ch, args):
    evs = ch.events(args.kind)
    if args.cycle:
        evs = [e for e in evs if e["cycle"] == args.cycle]
    return {"entries": evs}


def verb_asks(ch, args):
    return {"asks": [a.summary() for a in ch.asks("all" if args.all else "inbox")]}


# ---------------------------------------------------------------- CLI

def build_parser():
    p = argparse.ArgumentParser(prog="state.py", description=__doc__.split("\n\n")[0])
    p.add_argument("--home", help="the home (default: walk up from cwd for .claude/orchestrator/config.yaml)")
    p.add_argument("--change", help="the change (default: $ORCH_CHANGE, the worktree holding cwd, the only one)")
    sub = p.add_subparsers(dest="verb", required=True)

    sub.add_parser("status", help="the stage, the next step, the open cycle's gate and the asks that block it")
    s = sub.add_parser("gate", help="the open cycle's gate, re-evaluated (exit 1 when it does not pass)")
    s.add_argument("phase", nargs="?", choices=PHASES, help="default: the open cycle's phase")
    sub.add_parser("conventions", help="every break of the record's forms, with the form expected")
    s = sub.add_parser("check", help="check done|start|new <task>: refuses what would make the record false")
    s.add_argument("what", choices=["done", "start", "new"])
    s.add_argument("task")

    c = sub.add_parser("cycle", help="cycle start | abandon").add_subparsers(dest="sub", required=True)
    s = c.add_parser("start", help="start a cycle in plan; refuses while one is open or on a bad prior handshake")
    s.add_argument("--cycle", help="the id (default cycle-<epoch>)")
    s = c.add_parser("abandon", help="end the open cycle without closing it")
    s.add_argument("--why", required=True)

    s = sub.add_parser("assert", help="assert an asserted check of the open phase")
    s.add_argument("check")
    s.add_argument("--by", required=True, help="the role asserting it")
    s = sub.add_parser("route", help="a finding's decoration and resolution")
    s.add_argument("finding")
    s.add_argument("--decoration", choices=DECORATIONS)
    s.add_argument("--resolution")
    s = sub.add_parser("blocker", help="a task's blocker note: an ask id, a finding id or free text")
    s.add_argument("task")
    s.add_argument("note")
    s = sub.add_parser("note", help="one journal line (<= 280 chars); the long form in a file via --path")
    s.add_argument("kind", choices=NOTE_KINDS)
    s.add_argument("text")
    s.add_argument("--ref")
    s.add_argument("--by")
    s.add_argument("--path")
    s = sub.add_parser("fix-link", help="link an inline-fix commit to its review when the subject does not")
    s.add_argument("commit")
    s.add_argument("--task", required=True)
    s.add_argument("--review", required=True, help="the review finding's number")
    s = sub.add_parser("correct", help="a correction naming an entry of the log")
    s.add_argument("seq", type=int)
    s.add_argument("--why", required=True)

    s = sub.add_parser("log", help="the event log with corrections applied")
    s.add_argument("--cycle")
    s.add_argument("--kind", choices=sorted(EVENT_FIELDS))
    s = sub.add_parser("asks", help="the inbox's ask files (--all: and every cycle's asks/)")
    s.add_argument("--all", action="store_true")
    return p


VERBS = {
    "status": verb_status, "gate": verb_gate, "conventions": verb_conventions, "check": verb_check,
    ("cycle", "start"): verb_cycle_start, ("cycle", "abandon"): verb_cycle_abandon, "assert": verb_assert,
    "route": verb_route, "blocker": verb_blocker, "note": verb_note, "fix-link": verb_fix_link,
    "correct": verb_correct, "log": verb_log, "asks": verb_asks,
}


def main(argv=None):
    args = build_parser().parse_args(argv)
    fn = VERBS.get((args.verb, getattr(args, "sub", None))) or VERBS[args.verb]
    try:
        home = find_home(args.home)
        ch = Change(home, resolve_change(home, args.change))
        result = fn(ch, args)
    except Refusal as r:
        print(json.dumps({"ok": False, "error": r.slug, "message": r.message, **r.extra}), file=sys.stderr)
        return 1
    code = 0
    if isinstance(result, tuple):
        result, code = result
    print(json.dumps(result, ensure_ascii=False, indent=1))
    return code


if __name__ == "__main__":
    sys.exit(main())
