#!/usr/bin/env python3
"""orchestrate ask tool: writes and moves the ask files (templates/ask.md) and reads the inbox.

Outside the state layer (docs/specs/state-layer.org § Integration Points): the ask files are
the record, with no index beside them. `state.py` reads their kind, status, blocks and answer;
this tool writes a new file, changes one (the drawer and sections), moves it when its status
crosses into or out of applied / declined / superseded, and reports every inbox file with its
answer.

Every verb prints JSON. A refusal prints {"ok": false, "error": "<slug>", "message": "..."} on
stderr and exits 1.

Usage: python3 asks.py [--home PATH] [--change NAME] <verb> ...   (asks.py -h lists the verbs)
"""

import argparse
import difflib
import glob
import json
import os
import re
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import state as state_layer  # noqa: E402  the home, the change and its open cycle


TEXT_CAP = 280


LONG_TEXT_CAP = 600


LONG_TEXT_FIELDS = {"question", "decision_readback"}


# An ask is one org file (templates/ask.md); these are the fields its drawer holds.
ASK_FIELDS = {"id": str, "kind": str, "status": str, "blocks": list, "path": str}


ASK_DEFAULTS = {"blocks": [], "status": "open"}


# The rest of the record lives in the file; accepted on input to `add` / `set`.
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


ASK_SECTIONS = ["About", "What happened", "Options", "Recommendation", "If unanswered",
                "Answer", "Readback", "Discussion"]


ASK_SECTION_SYNONYMS = {"observed": "What happened", "default": "If unanswered", "decision": "Answer"}


ASK_PROPS = ["ID", "KIND", "STATUS", "RAISED_BY", "REF", "HAPPENED", "BLOCKS", "APPLIED_VIA", "REVISED_FROM"]


ASK_ANSWER_HINT = ("# An option label from above, or your own words; everything below this line "
                   "is read as your answer.")


ASK_ID_RE = re.compile(r"\bask-[A-Za-z0-9-]*\d\b")


ORG_COMMENT_RE = re.compile(r"\s*#(\s|$)")



Refusal = state_layer.Refusal

def _atomic_write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        f.write(text)
        f.flush()
        os.fsync(f.fileno())
    os.replace(tmp, path)


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


def _enum(value, allowed, where, errors):
    if value is not None and value not in allowed:
        errors.append(f"{where}: {value!r} not one of {allowed}")


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
    """A full ask record (drawer + file fields) as `add` / `set` receive it."""
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


def _git(repo_root, *args):
    try:
        r = subprocess.run(["git", "-C", repo_root, *args], capture_output=True, text=True)
        return r.returncode, r.stdout
    except OSError:
        return None, ""


def _git_tracked(repo_root, rel_dir):
    rc, out = _git(repo_root, "ls-files", "--", rel_dir)
    return {l for l in out.split("\n") if l} if rc == 0 else set()


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

# ---------------------------------------------------------------- files

def ask_rel(ch, ask_id, status, fallback_cycle):
    """Relative to the home; a pure function of id and status."""
    pre = os.path.relpath(ch.dir, ch.home)
    if status in ASK_TERMINAL:
        return f"{pre}/cycles/{_cycle_of(ask_id, fallback_cycle)}/asks/{ask_id}.org"
    return f"{pre}/inbox/{ask_id}.org"


def ask_files(ch):
    """{id (the file's basename): path relative to the home}, inbox and every cycle's asks/."""
    out = {}
    for pat in (os.path.join(ch.dir, "inbox", "*.org"), os.path.join(ch.dir, "cycles", "*", "asks", "*.org")):
        for p in sorted(glob.glob(pat)):
            out[os.path.basename(p)[:-4]] = os.path.relpath(p, ch.home)
    return out


def read_ask_file(ch, rel):
    with open(os.path.join(ch.home, rel), encoding="utf-8") as f:
        doc = AskDoc(f.read())
    return doc, ask_from_org(doc, os.path.basename(rel)[:-4])


def _move_ask_file(home, old_rel, new_rel):
    src, dst = os.path.join(home, old_rel), os.path.join(home, new_rel)
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    rc, _ = _git(home, "ls-files", "--error-unmatch", "--", old_rel)
    if rc == 0:
        rc2, _ = _git(home, "mv", "--", old_rel, new_rel)
        if rc2 == 0 and os.path.exists(dst):
            return "git mv"
    os.replace(src, dst)
    return "rename"


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


def _cycle(ch, explicit):
    """The open cycle, else --cycle; an ask id names the cycle it was raised in."""
    open_cycle, _ = ch.open_cycle()
    cycle = open_cycle or explicit
    if not cycle:
        raise Refusal("no_cycle", f"{ch.name} has no open cycle; pass --cycle <cycle-id>")
    return cycle


def _supersede(ch, files, old_id, cycle):
    rel = files.get(old_id)
    if rel is None:
        raise Refusal("no_such_ask", f"revised_from names {old_id!r}, which has no file")
    doc, _rec = read_ask_file(ch, rel)
    doc.set_prop("STATUS", "superseded")
    _atomic_write(os.path.join(ch.home, rel), doc.text())
    new_rel = ask_rel(ch, old_id, "superseded", cycle)
    return _move_ask_file(ch.home, rel, new_rel) if new_rel != rel else None


# ---------------------------------------------------------------- verbs

def verb_add(ch, args):
    rec_in, draft, draft_path = _ask_input(args.json, ch.home)
    cycle = _cycle(ch, args.cycle)
    files = ask_files(ch)
    rec = {**ASK_DEFAULTS, **ASK_FILE_DEFAULTS, **{k: v for k, v in rec_in.items() if k != "path"}}
    if not rec.get("id"):
        prefix = f"ask-{cycle}-"
        rec["id"] = prefix + str(next_seq(list(files), prefix))
    errors = []
    validate_ask_full(rec, "ask", errors)
    if errors:
        raise Refusal("invalid_record", "ask does not fit templates/ask.md", errors=errors)
    rel = ask_rel(ch, rec["id"], rec["status"], cycle)
    abs_path = os.path.join(ch.home, rel)
    in_place = draft_path and os.path.abspath(draft_path) == os.path.abspath(abs_path)
    if rec["id"] in files and not in_place:
        raise Refusal("duplicate_id", f"{files[rec['id']]} already holds {rec['id']}; `asks.py set` changes it")
    moved = _supersede(ch, files, rec["revised_from"], cycle) if rec.get("revised_from") else None
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
    out = {"ok": True, "id": rec["id"], "path": rel, "status": rec["status"]}
    if moved:
        out["superseded"] = {"id": rec["revised_from"], "moved_via": moved}
    return out


def verb_set(ch, args):
    updates = parse_assignments(args.fields)
    confirm = updates.pop("readback", None)  # readback=confirmed: the user's yes under * Readback
    if confirm is not None and _norm(str(confirm)) not in ("confirmed", "yes"):
        raise Refusal("bad_value", "readback takes `confirmed` (writes the user's yes under * Readback)")
    allowed = set(ASK_FILE_FIELDS) | {"kind", "status", "blocks"}
    for k in updates:
        if k not in allowed:
            raise Refusal("unknown_field" if k not in ("id", "path") else "not_settable",
                          f"{k!r} cannot be set on an ask", allowed=sorted(allowed))
    rel = ask_files(ch).get(args.id)
    if rel is None:
        raise Refusal("no_such_ask", f"no file for {args.id} in the inbox or any cycle's asks/")
    doc, rec = read_ask_file(ch, rel)
    if doc.drawer is None or not doc.prop("KIND"):
        raise Refusal("no_drawer", f"{rel} has no :PROPERTIES: drawer with :ID:, :KIND: and :STATUS:; "
                                   "restore it from git or write it by hand (templates/ask.md)")
    rec["id"] = args.id
    new = {**rec, **updates}
    if new.get("status") in ASK_ANSWERED and not new.get("decision"):
        raise Refusal("answer_required", f"status {new['status']} needs an answer under * Answer "
                                         "(or decision=<text> in this command)")
    if new.get("status") == "applied" and not new.get("applied_via"):
        raise Refusal("applied_via_required", "status applied needs applied_via=<what applied it>")
    errors = []
    validate_ask_full({k: v for k, v in new.items() if k != "path"}, f"ask[{args.id}]", errors)
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
            raise Refusal("no_readback", f"{args.id} has no * Readback to confirm; write one with "
                                         "decision_readback=<text> first")
        if not any(_norm(l) in ("yes", "confirmed", "ok") for l in rb.splitlines()):
            doc.set_section("Readback", rb.rstrip() + "\n\nyes")
        updates["readback"] = "confirmed"
    _atomic_write(os.path.join(ch.home, rel), doc.text())
    final = ask_from_org(doc, args.id)
    fallback = ch.open_cycle()[0] or args.cycle
    if final["status"] in ASK_TERMINAL and _cycle_of(args.id, None) is None and not fallback:
        raise Refusal("no_cycle", f"{args.id} names no cycle and none is open: pass --cycle <cycle-id> for "
                                  "the cycles/<cycle-id>/asks/ it moves to")
    new_rel = ask_rel(ch, args.id, final["status"], fallback)
    moved = _move_ask_file(ch.home, rel, new_rel) if new_rel != rel else None
    out = {"ok": True, "id": args.id, "changed": sorted(updates), "status": final["status"], "path": new_rel}
    if moved:
        out["moved_via"] = moved
    return out


def verb_inbox(ch, args):
    """Read-only: every inbox file with its answer analysis, and every file in the wrong place."""
    tracked = _git_tracked(ch.home, os.path.relpath(ch.dir, ch.home))
    asks, problems = [], []
    for ask_id, rel in ask_files(ch).items():
        doc, rec = read_ask_file(ch, rel)
        in_inbox = f"/inbox/{ask_id}.org" in "/" + rel
        if in_inbox == (rec["status"] in ASK_TERMINAL):
            problems.append(f"{rel}: status {rec['status']} belongs in "
                            f"{'cycles/<id>/asks/' if in_inbox else 'the inbox'}")
        if not in_inbox:
            continue
        probs = []
        if rec["id"] != ask_id:
            probs.append(f"{rel}: :ID: {rec['id']} differs from the file name")
        if doc.drawer is None:
            probs.append(f"{rel}: no properties drawer")
        if not rec["question"]:
            probs.append(f"{rel}: no #+TITLE")
        elif len(rec["question"]) > LONG_TEXT_CAP:
            probs.append(f"{rel}: #+TITLE is {len(rec['question'])} chars; cap {LONG_TEXT_CAP}")
        answer = analyse_answer(rec)
        rb_lines = [l for l in (doc.section_lines("Readback") or []) if l.strip()]
        asks.append({
            "id": ask_id, "kind": rec["kind"], "status": rec["status"], "blocks": rec["blocks"], "path": rel,
            "tracked": rel in tracked, "question": rec["question"], "answer": answer,
            "readback": {"present": bool(rec.get("decision_readback")),
                         "confirmed": any(_norm(l) in ("yes", "confirmed", "ok") for l in rb_lines)},
            "mentions": sorted({m for m in ASK_ID_RE.findall(rec.get("decision") or "") if m != ask_id}),
            "answer_candidate": None if answer else _answer_candidate(doc, rec),
            "problems": probs,
        })
        problems += probs
    return {"ok": not problems, "answered": [a["id"] for a in asks if a["answer"]], "asks": asks,
            "problems": problems}


# ---------------------------------------------------------------- CLI

def build_parser():
    p = argparse.ArgumentParser(prog="asks.py", description=__doc__.split("\n\n")[0])
    p.add_argument("--home", help="the home (default: walk up from cwd for .claude/orchestrator/config.yaml)")
    p.add_argument("--change", help="the change (default: $ORCH_CHANGE, the worktree holding cwd, the only one)")
    sub = p.add_subparsers(dest="verb", required=True)
    s = sub.add_parser("add", help="write a new ask file from JSON, @file.json or @draft.org")
    s.add_argument("json")
    s.add_argument("--cycle", help="the cycle the id names when none is open")
    s.set_defaults(fn=verb_add)
    s = sub.add_parser("set", help="set <id> field=value …: edit the file, move it when its status says so")
    s.add_argument("id")
    s.add_argument("fields", nargs="+")
    s.add_argument("--cycle", help="the cycle whose asks/ a terminal ask moves to when its id names none and none is open")
    s.set_defaults(fn=verb_set)
    s = sub.add_parser("inbox", help="every inbox file with its answer (read-only)")
    s.set_defaults(fn=verb_inbox)
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    try:
        home = state_layer.find_home(args.home)
        ch = state_layer.Change(home, state_layer.resolve_change(home, args.change))
        if ch.rec is None:
            raise Refusal("change_not_open", f"{ch.name} has no change.json; open it first (home.py open)")
        result = args.fn(ch, args)
    except Refusal as r:
        print(json.dumps({"ok": False, "error": r.slug, "message": r.message, **r.extra}, indent=2), file=sys.stderr)
        return 1
    print(json.dumps(result, indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(main())
