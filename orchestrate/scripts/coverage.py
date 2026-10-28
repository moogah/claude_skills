#!/usr/bin/env python3
"""Scenario coverage, computed from the specs and the tests (roadmap 2.1).

Reads every scenario of the org specs (`docs/specs/**/*.org`: a heading `Scenario: <title>`,
its capability the file's path under the specs directory without `.org`, and its optional
`:TEST:` property) and every citation in the code root's test files (`*-spec.el`, `*-test.el`,
`test-*.el`, `test_*.py`, `*_test.py`):

  (a)  an `it` / `describe` string ending `(Scenario: <cap> § <title>[; <cap> § <title>])`
  (b)  a comment `;; Scenario: <cap> § <title>`, the title possibly finished on the next
       comment line
  (a') an `ert-deftest` docstring line `Scenario: <cap> § '<title>'`
  (p)  a pytest function's or class's docstring line `Scenario: <cap> § <title>`, the title
       possibly followed by prose or finished on the next line (the longest run of words from
       its start that names a scenario)

A citation naming a capability directory (`bash-parser`) resolves over its files.

A `:TEST:` property stands in for a citation the test cannot carry (templates/spec.md):
`none: <reason>`, or `<file>::"<start of the it string>"`, or `<file>::<ert test name>` (a
pytest test: `<file>::<function>` or `<file>::<Class>::<function>`); `:TEST+:` adds another.

Status per scenario, worst first: failed, not run (a run output was given and the test is
not in it), pending (`xit`, an `it` with no body, or PENDING in the output), cited (no run
output given), pass, none: <reason>, uncited. With `--output`, a raw test run's output, a log
or a verbose run: Buttercup lines join on the `it` string (one that a message or a newline in the
title splits over several lines is joined first), ERT lines on the test name, pytest `-v` lines
on the node id. An output whose summary counts tests of a framework but holds no line per test
(a compact report, pytest without -v) is refused, naming its `Log:` path when it gives one.

`--intent docs/changes/<change>/intent.org` selects the change's scenarios: those the diff from its
`#+BASE:` touches, and those its `* Capabilities` names (`- <cap> § <title>: stated, not met`,
templates/intent.md; a parenthesis may follow), which is how a change opened from a workspace integration branch whose spec
edits lie before its base gets its own. A `--scenario` or intent name that matches no scenario is
listed, never dropped silently.

Run from a home above the code root (no `docs/specs` at the git root and no --root), the code
root is the resolved change's, from `home.py where`.

Usage:
  coverage.py [--root CODE_ROOT] [--specs DIR] [--tests DIR ...] [--output RUN_OUTPUT]
              [--capability ID] [--scenario 'CAP § TITLE' ...] [--since COMMIT]
              [--intent INTENT_ORG] [--uncited] [--json]
  coverage.py adopt-rows DESIGN_ORG [--root …] [--specs …] [--write]
      a change's legacy `* Scenario coverage` table -> `:TEST:` properties for its `none`
      rows and for rows naming a test that carries no citation; prints them, --write
      writes them into the spec files

Exit 0 always for the report; 1 on a refusal (bad arguments, git failure, a compact output).
"""

import argparse
import ast
import json
import os
import re
import subprocess
import sys

TEST_HEADS = {"it", "xit", "describe", "xdescribe", "ert-deftest"}
STATUS_ORDER = ["failed", "not run", "pending", "cited", "pass"]
SKIP_DIRS = {"elpa", "eln-cache", "straight", "runtime", "node_modules", "venv", "site-packages"}


class Refusal(Exception):
    pass


# ---------------------------------------------------------------- specs

_ORG_HEADING = re.compile(r"^(\*+) (.*?)\s*$")
_PROP = re.compile(r"^\s*:(TEST\+?):\s*(.*?)\s*$")


class Scenario:
    def __init__(self, cap, title, path, line, end):
        self.cap, self.title, self.path, self.line, self.end = cap, title, path, line, end
        self.tests = []      # :TEST: values
        self.citations = []  # Citation


def read_specs(specs_dir):
    """[Scenario] of every org spec under SPECS_DIR."""
    out = []
    for dirpath, dirs, files in os.walk(specs_dir):
        dirs.sort()
        for f in sorted(files):
            if f.endswith(".org"):
                p = os.path.join(dirpath, f)
                cap = os.path.relpath(p, specs_dir)[:-4]
                with open(p, encoding="utf-8") as fh:
                    out.extend(scenarios_of(fh.read().split("\n"), cap, p))
    return out


def scenarios_of(lines, cap, path):
    heads, block = [], False
    for i, line in enumerate(lines):
        s = line.strip().lower()
        if s.startswith("#+begin_"):
            block = True
        elif s.startswith("#+end_"):
            block = False
        elif not block:
            m = _ORG_HEADING.match(line)
            if m:
                heads.append((i, len(m.group(1)), m.group(2)))
    out = []
    for k, (i, level, title) in enumerate(heads):
        if not title.startswith("Scenario: "):
            continue
        end = len(lines)
        for j, lv, _ in heads[k + 1:]:
            if lv <= level:
                end = j
                break
        sc = Scenario(cap, title[len("Scenario: "):], path, i + 1, end)
        if i + 1 < len(lines) and lines[i + 1].strip() == ":PROPERTIES:":
            j = i + 2
            while j < len(lines) and lines[j].strip() != ":END:":
                m = _PROP.match(lines[j])
                if m:
                    sc.tests.append(m.group(2))
                j += 1
        out.append(sc)
    return out


# ---------------------------------------------------------------- elisp lexer

class Form:
    def __init__(self, kind, line, parent):
        self.kind, self.line, self.parent = kind, line, parent
        self.name = None
        self.nargs = 0
        self.path = None  # a pytest Form's file, for its node id

    @property
    def pending(self):
        f = self
        while f:
            if f.kind in ("xit", "xdescribe"):
                return True
            f = f.parent
        return self.kind == "it" and self.nargs <= 1


_DELIM = set(" \t\n\r\f()[]\";'`,")


def scan_el(text):
    """([('string'|'comment', text, line, Form or None)], [Form]) for one elisp file."""
    events, forms, stack = [], [], []
    i, n, line = 0, len(text), 1

    def current():
        for e in reversed(stack):
            if e["form"]:
                return e["form"]
        return None

    def element():
        if stack:
            stack[-1]["n"] += 1
            return stack[-1]["n"]
        return 0

    while i < n:
        c = text[i]
        if c == "\n":
            line += 1
            i += 1
        elif c in " \t\r\f":
            i += 1
        elif c == ";":
            j = text.find("\n", i)
            j = n if j < 0 else j
            events.append(("comment", text[i:j], line, current()))
            i = j
        elif c == '"':
            j, buf, start = i + 1, [], line
            while j < n and text[j] != '"':
                if text[j] == "\\" and j + 1 < n:
                    if text[j + 1] == "\n":
                        line += 1
                    else:  # \n and \t as the runner prints them, so a title holding one matches its result
                        buf.append({"n": "\n", "t": "\t"}.get(text[j + 1], text[j + 1]))
                    j += 2
                    continue
                if text[j] == "\n":
                    line += 1
                buf.append(text[j])
                j += 1
            value = "".join(buf)
            k = element()
            top = stack[-1] if stack else None
            if top and top["form"] and k == 2 and top["form"].kind != "ert-deftest":
                top["form"].name = value
            events.append(("string", value, start, current()))
            i = j + 1
        elif c in "([":
            element()
            stack.append({"n": 0, "form": None, "line": line})
            i += 1
        elif c in ")]":
            if stack:
                e = stack.pop()
                if e["form"]:
                    e["form"].nargs = e["n"] - 1
            i += 1
        elif c in "'`":
            i += 1
        elif c == ",":
            i += 2 if i + 1 < n and text[i + 1] == "@" else 1
        elif c == "#" and i + 1 < n and text[i + 1] == "'":
            i += 2
        elif c == "?":
            i += 3 if i + 1 < n and text[i + 1] == "\\" else 2
            element()
        else:
            j = i
            while j < n and text[j] not in _DELIM:
                j += 2 if text[j] == "\\" else 1
            token = text[i:j]
            k = element()
            top = stack[-1] if stack else None
            if top and k == 1 and token in TEST_HEADS:
                top["form"] = Form(token, line, current())
                forms.append(top["form"])
            elif top and top["form"] and top["form"].kind == "ert-deftest" and k == 2:
                top["form"].name = token
            i = max(j, i + 1)
    return events, forms


# ---------------------------------------------------------------- citations

class Citation:
    def __init__(self, cap, title, path, line, form, shape, alt=None):
        self.cap, self.title, self.path, self.line, self.form, self.shape = cap, title, path, line, form, shape
        self.alt = alt or []  # other readings of a wrapped comment title, tried in order
        self.scenarios = []

    def where(self, root):
        return f"{os.path.relpath(self.path, root)}:{self.line}"


_CAP = r"[\w][\w/.-]*"
_A = re.compile(r"\(Scenarios?: (" + _CAP + r" § .+)\)\s*$", re.S)
_A_SPLIT = re.compile(r";\s+(?=" + _CAP + r" § )")
_A1 = re.compile(r"^Scenarios?: (" + _CAP + r") § '(.+)'\s*$")
_B = re.compile(r"^;+\s*Scenarios?: (" + _CAP + r") § (.+?)\s*$")


def test_files(root, dirs=None):
    found = []
    for base in dirs or [root]:
        for dirpath, ds, files in os.walk(base):
            ds[:] = sorted(d for d in ds if not d.startswith(".") and d not in SKIP_DIRS)
            for f in sorted(files):
                if f.endswith(("-spec.el", "-test.el", "_test.py")) or (f.startswith("test-") and f.endswith(".el")) \
                        or (f.startswith("test_") and f.endswith(".py")):
                    found.append(os.path.join(dirpath, f))
    return found


_P = re.compile(r"^\s*Scenarios?: (" + _CAP + r") § (.+?)\s*$")


def py_citations(path, text):
    """([Citation], [Form]) of one pytest file: docstring citations of its functions and classes.
    A Form's name is its node id after the file (`Class::function`)."""
    try:
        tree = ast.parse(text)
    except SyntaxError:
        return [], []
    out, forms = [], []

    def readings(rest, nxt):
        """The title's readings, longest first: each run of words from its start, over the line and
        the next, since prose follows the title after ` — `, `. ` or ` (` and a title wraps."""
        words = (rest + " " + (nxt or "").strip()).split()
        seen = []
        for k in range(len(words), 0, -1):
            c = " ".join(words[:k]).rstrip(" —.,;:(")
            if c and c not in seen:
                seen.append(c)
        return seen

    def walk(node, prefix, parent):
        for child in ast.iter_child_nodes(node):
            if not isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                continue
            form = Form("class" if isinstance(child, ast.ClassDef) else "def", child.lineno, parent)
            form.name, form.path = prefix + child.name, path
            forms.append(form)
            doc = ast.get_docstring(child, clean=False)
            if doc:
                lines, first = doc.split("\n"), child.body[0].lineno
                for k, text_line in enumerate(lines):
                    m = _P.match(text_line)
                    if m:
                        r = readings(m.group(2), lines[k + 1] if k + 1 < len(lines) else None)
                        out.append(Citation(m.group(1), r[0], path, first + k, form, "p", r[1:]))
            walk(child, form.name + "::", form)

    walk(tree, "", None)
    return out, forms


def citations_in(path, text=None):
    """([Citation], [Form]) of one test file."""
    if text is None:
        with open(path, encoding="utf-8", errors="replace") as fh:
            text = fh.read()
    if path.endswith(".py"):
        return py_citations(path, text)
    events, forms = scan_el(text)
    out = []
    for k, (kind, value, line, form) in enumerate(events):
        if kind == "string":
            m = _A.search(value)
            if m:
                for part in _A_SPLIT.split(m.group(1)):
                    cap, _, title = part.partition(" § ")
                    out.append(Citation(cap, title.strip(), path, line, form, "a"))
                continue
            for k2, text_line in enumerate(value.split("\n")):
                m = _A1.match(text_line)
                if m:
                    out.append(Citation(m.group(1), m.group(2), path, line + k2, form, "a'"))
        else:
            m = _B.match(value.strip())
            if not m or m.group(1).endswith(".md"):
                continue
            title = m.group(2).rstrip(".")
            alt = []
            nxt = events[k + 1] if k + 1 < len(events) else None
            if nxt and nxt[0] == "comment" and nxt[2] == line + 1:
                joined = m.group(2) + " " + re.sub(r"^;+\s*", "", nxt[1].strip())
                alt = [joined[:e.start()] for e in re.finditer(r"\.(\s|$)", joined)] + [joined.rstrip(".")]
            out.append(Citation(m.group(1), title, path, line, form, "b", alt))
    return out, forms


def resolve(citations, scenarios):
    """Attach each citation to the scenarios it names; return the unresolved ones."""
    index = {}
    for sc in scenarios:
        index.setdefault(sc.title, []).append(sc)

    def find(cap, title):
        return [sc for sc in index.get(title, []) if sc.cap == cap or sc.cap.startswith(cap + "/")]

    unresolved = []
    for c in citations:
        hits = find(c.cap, c.title)
        for t in c.alt:
            if hits:
                break
            hits = find(c.cap, t)
            if hits:
                c.title = t
        c.scenarios = hits
        for sc in hits:
            sc.citations.append(c)
        if not hits:
            unresolved.append(c)
    return unresolved


# ---------------------------------------------------------------- run output

_ANSI = re.compile(r"\x1b\[[0-9;]*m")
_ERT = re.compile(r"^\s*(passed|FAILED|failed|skipped)\s+\d+/\d+\s+(\S+)")
# Buttercup's final line for an `it`: green pass, yellow pending, red failed (the failure
# message follows in red, not the word FAILED); without colours, the words decide.
_COLOURED = re.compile(r"^\x1b\[(3[123])m\s*(.*?)\x1b\[0m.*\(\d[\d.]*(?:ms|s|m)\)\s*$", re.S)
_COLOUR = {"32": "pass", "33": "pending", "31": "failed"}
# A spec `buttercup-skip` cancelled prints `!! CANCELLED !! <reason>` before its time: pending.
_BUTTER = re.compile(r"^\s+(.*?)\s+(?:(PENDING|FAILED|SKIPPED)\s+|(!! CANCELLED !!) .*)?\((\d[\d.]*(?:ms|s|m))\)\s*$")
# A message the spec printed (a `load`'s "Loading …", a library's warning, on one line or several)
# lands after the title and pushes the time to a line of its own: the title's line is the last
# indented one before it.
_TIME_ONLY = re.compile(r"^\s*(?:(?:PENDING|FAILED|SKIPPED)\s+)?\(\d[\d.]*(?:ms|s|m)\)\s*$")
_PYTEST = re.compile(r"^(\S+?\.py)::(\S+?)(?:\[[^\]]*\])?\s+(PASSED|FAILED|ERROR|SKIPPED|XFAIL|XPASS)\b")
_SUMMARIES = {"Buttercup": re.compile(r"^Ran (\d+) (?:out of \d+ )?specs?\b", re.M),
              "ERT": re.compile(r"^Ran (\d+) tests?,", re.M),
              "pytest": re.compile(r"^=+ .*?\b(\d+) (?:passed|failed|errors?|skipped|xfailed|xpassed)\b.* in [\d.]+s", re.M)}
_SPLIT_MAX = 20  # lines a coloured title may span


class Run:
    """A run output's results: {it string: {status}}, {ert name: {status}}, {(file, node): {status}}
    and the it lines a message split ({title and message: {status}}, read by prefix)."""
    def __init__(self):
        self.it, self.ert, self.py, self.glued = {}, {}, {}, {}
        self.ran = {}  # framework -> the count its summary line gives
        self.log = None

    def tables(self):
        return {"Buttercup": self.it or self.glued, "ERT": self.ert, "pytest": self.py}


def parse_output(text):
    """Run from a raw test run's output."""
    run = Run()
    word = {"passed": "pass", "FAILED": "failed", "failed": "pending", "skipped": "pending",
            None: "pass", "PENDING": "pending", "SKIPPED": "pending",
            "PASSED": "pass", "XPASS": "pass", "ERROR": "failed", "XFAIL": "pending"}
    plain = _ANSI.sub("", text)
    for name, rx in _SUMMARIES.items():
        counts = [int(m.group(1)) for m in rx.finditer(plain)]
        if counts:
            run.ran[name] = sum(counts)
    m = re.search(r"^\s*Log: (\S.*?)\s*$", plain, re.M)
    run.log = m and m.group(1)
    lines = text.split("\n")
    prev, i = "", 0
    while i < len(lines):
        seg = lines[i].split("\r")[-1]
        i += 1
        if re.match(r"^\x1b\[3[123]m", seg) and "\x1b[0m" not in seg:  # a title holding newlines
            k = i
            while k < len(lines) and k - i < _SPLIT_MAX and "\x1b[0m" not in lines[k]:
                k += 1
            if k < len(lines) and "\x1b[0m" in lines[k]:
                seg = "\n".join([seg] + lines[i:k + 1])
                i = k + 1
        line = _ANSI.sub("", seg)
        m = _ERT.match(line)
        if m:
            run.ert.setdefault(m.group(2), set()).add(word[m.group(1)])
            continue
        m = _PYTEST.match(line)
        if m:
            run.py.setdefault((m.group(1), m.group(2)), set()).add(word[m.group(3)])
            continue
        m = _COLOURED.match(seg)
        if m:
            run.it.setdefault(m.group(2), set()).add(_COLOUR[m.group(1)])
            continue
        if _TIME_ONLY.match(line) and prev:
            m = _BUTTER.match(prev + line)
            if m:
                run.glued.setdefault(m.group(1), set()).add("pending" if m.group(3) else word[m.group(2)])
            prev = ""
            continue
        m = _BUTTER.match(line)
        if m:
            run.it.setdefault(m.group(1), set()).add("pending" if m.group(3) else word[m.group(2)])
            prev = ""
        elif re.match(r"^\s+\S", line):
            prev = line
    return run


def read_output(path):
    """Run from the file at PATH; a Refusal when a framework's summary counts tests and the file
    holds no line for any of them (a compact report: every citation would read "not run")."""
    with open(path, encoding="utf-8", errors="replace") as fh:
        run = parse_output(fh.read())
    tables = run.tables()
    bare = [name for name, n in run.ran.items() if n and not tables[name]]
    if bare:
        where = f"the run's log, {run.log}," if run.log else "the run's log"
        raise Refusal(f"{path} counts {' and '.join(bare)} tests but holds no line per test (a compact "
                      f"report, or pytest without -v); give --output {where} or a verbose run")
    return run


def worst(statuses):
    for s in STATUS_ORDER:
        if s in statuses:
            return s
    return None


def citation_status(form, run):
    if form is None:
        return "cited"
    if form.pending:
        return "pending"
    if run is None or form.kind in ("describe", "xdescribe", "class") or form.name is None:
        return "cited"
    if form.kind == "def":  # a node id from pytest's rootdir: the file matches by its tail
        found = set()
        for (f, node), st in run.py.items():
            if node == form.name and (form.path == f or form.path.endswith(os.sep + f)):
                found |= st
    elif form.kind == "ert-deftest":
        found = run.ert.get(form.name)
    else:
        found = run.it.get(form.name)
        if not found:  # a line a message split: the title, then the message, with no space between
            n = len(form.name)
            found = set().union(*[st for k, st in run.glued.items()
                                  if k.startswith(form.name) and len(k) > n and not k[n].isspace()])
    return worst(found) if found else "not run"


# ---------------------------------------------------------------- :TEST: rows

_ROW = re.compile(r'^(.+?)::(?:"(.*)"|(\S+))$')


def resolve_test_prop(value, files_by_name, forms_of):
    """A :TEST: `<file>::"<it prefix>"` or `<file>::<ert name>` -> (Form, path) or (None, why)."""
    m = _ROW.match(value)
    if not m:
        return None, "not <file>::\"<it string>\" or <file>::<ert name>"
    ref, prefix, name = m.group(1), m.group(2), m.group(3)
    if prefix is not None:
        prefix = prefix.rstrip(" …").rstrip(".")
    paths = [p for p in files_by_name.get(os.path.basename(ref), []) if p.endswith(ref)]
    if len(paths) != 1:
        return None, f"{ref}: {len(paths)} test files match"
    for f in forms_of(paths[0]):
        if prefix is not None and f.kind in ("it", "xit") and f.name and f.name.startswith(prefix):
            return f, paths[0]
        if name is not None and f.kind in ("ert-deftest", "def") and f.name == name:
            return f, paths[0]
    return None, f"no test in {ref} matches"


# ---------------------------------------------------------------- since

def touched_lines(root, specs_dir, since):
    """{abs spec path: set(line numbers)} the diff from SINCE to the working tree touches."""
    rel = os.path.relpath(specs_dir, root)
    r = subprocess.run(["git", "-C", root, "diff", "-U0", since, "--", rel], capture_output=True, text=True)
    if r.returncode:
        raise Refusal("git diff failed: " + r.stderr.strip())
    # A line counts when it carries text: an added blank line or rule at a scenario's end (a
    # requirement appended after it) does not touch it. A pure deletion touches the line before it.
    out, path, new_line = {}, None, 0
    for line in r.stdout.split("\n"):
        if line.startswith("+++ "):
            path = None if line[4:] == "/dev/null" else os.path.join(root, line[6:])
            continue
        if line.startswith("--- "):
            continue
        m = re.match(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@", line)
        if m and path:
            new_line, count = int(m.group(1)), int(m.group(2) or 1)
            if not count:
                out.setdefault(path, set()).add(new_line)
                new_line += 1
            continue
        if line.startswith("+") and path:
            if line[1:].strip() not in ("", "-----"):
                out.setdefault(path, set()).add(new_line)
            new_line += 1
    r = subprocess.run(["git", "-C", root, "ls-files", "--others", "--exclude-standard", "--", rel],
                       capture_output=True, text=True)
    for f in r.stdout.split():
        out[os.path.join(root, f)] = None  # whole file
    return out


def in_since(sc, touched):
    if sc.path not in touched:
        return False
    lines = touched[sc.path]
    return lines is None or any(sc.line <= ln <= sc.end for ln in lines)


# ---------------------------------------------------------------- intent

_NAMED = re.compile(r"^- (\S+) § (.+?): stated, not met(?: \([^()]*\))?\s*$")


def intent_scope(path):
    """(base, [(cap, title)]): the intent's #+BASE: and the scenarios its * Capabilities names as
    stated, not met. A list item's indented continuation lines are joined to it."""
    try:
        text = open(path, encoding="utf-8").read()
    except OSError as e:
        raise Refusal(f"cannot read the intent {path}: {e.strerror}")
    m = re.search(r"^#\+BASE:\s*([0-9a-fA-F]{7,40})", text, re.M)
    if not m:
        raise Refusal(f"{path} has no #+BASE: line")
    items, inside = [], False
    for line in text.split("\n"):
        if line.startswith("* "):
            inside = line.strip() == "* Capabilities"
        elif inside and line.startswith("- "):
            items.append(line)
        elif inside and items and line.startswith("  ") and line.strip():
            items[-1] += " " + line.strip()
        elif inside and not line.strip():
            items.append("")
    named = [(n.group(1), n.group(2)) for n in map(_NAMED.match, items) if n]
    return m.group(1), named


# ---------------------------------------------------------------- report

def compute(root, specs_dir, tests=None, output=None):
    scenarios = read_specs(specs_dir)
    files = test_files(root, tests)
    files_by_name, forms_cache, citations = {}, {}, []
    for p in files:
        files_by_name.setdefault(os.path.basename(p), []).append(p)
        cs, forms = citations_in(p)
        forms_cache[p] = forms
        citations.extend(cs)
    unresolved = resolve(citations, scenarios)
    run = read_output(output) if output else None
    rows, bad_props = [], []

    def forms_of(p):
        if p not in forms_cache:
            forms_cache[p] = citations_in(p)[1]
        return forms_cache[p]

    for sc in scenarios:
        statuses, cites = [], []
        for c in sc.citations:
            statuses.append(citation_status(c.form, run))
            cites.append(c.where(root))
        none = None
        for value in sc.tests:
            if value.startswith("none:"):
                none = value
                continue
            form, path = resolve_test_prop(value, files_by_name, forms_of)
            if form is None:
                bad_props.append((sc, value, path))
                continue
            statuses.append(citation_status(form, run))
            cites.append(f"{os.path.relpath(path, root)}:{form.line} (:TEST:)")
        status = worst(statuses) or none or "uncited"
        rows.append({"capability": sc.cap, "scenario": sc.title, "status": status,
                     "citations": cites, "spec": f"{os.path.relpath(sc.path, root)}:{sc.line}", "_sc": sc})
    return rows, unresolved, bad_props


def org_cell(s):
    return s.replace("|", "\\vert{}")


def print_report(rows, unresolved, bad_props, root, unmatched=()):
    counts = {}
    for r in rows:
        key = "none" if r["status"].startswith("none:") else r["status"]
        counts[key] = counts.get(key, 0) + 1
    print("| capability | scenario | status | citations |")
    print("|-")
    for r in rows:
        print(f"| {org_cell(r['capability'])} | {org_cell(r['scenario'])} | {org_cell(r['status'])} | "
              f"{org_cell(', '.join(r['citations']))} |")
    order = STATUS_ORDER + ["none", "uncited"]
    print(f"\n{len(rows)} scenarios: " + ", ".join(f"{counts[k]} {k}" for k in order if k in counts))
    if bad_props:
        print(f"\n:TEST: properties that resolve to no test ({len(bad_props)}):")
        for sc, value, why in bad_props:
            print(f"  {os.path.relpath(sc.path, root)}:{sc.line} {value}  ({why})")
    if unresolved:
        print(f"\nCitations that resolve to no scenario ({len(unresolved)}):")
        for c in unresolved:
            print(f"  {c.where(root)} ({c.shape}) {c.cap} § {c.title}")
    if unmatched:
        print(f"\nNamed scenarios that match no scenario ({len(unmatched)}):")
        for cap, title in unmatched:
            print(f"  {cap} § {title}")


# ---------------------------------------------------------------- adopt-rows

def coverage_rows(design_text):
    """[(scenario cell, kind, test, note)] of a design.org `* Scenario coverage` table."""
    rows, inside = [], False
    for line in design_text.split("\n"):
        if re.match(r"^\* ", line):
            inside = line.strip() == "* Scenario coverage"
            continue
        if inside and line.startswith("|") and not line.startswith("|-"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if len(cells) >= 4 and cells[0] != "scenario":
                rows.append(tuple(cells[:4]))
    return rows


def row_tests(cell):
    """The tests a coverage-table cell names, as :TEST: values. Two cell shapes:
    `=a.el::"x"=, =::"y"=` and `=a.el= "x"; =b.el= "y"` (an empty file part repeats the last)."""
    cell = cell.replace("\\vert{}", "|")
    tokens = re.finditer(r'(?:^|(?<=[\s,;(]))([=~])(.+?)\1(?=$|[\s,.;)])|"([^"]*)"', cell)
    out, last_file = [], None
    for m in tokens:
        if m.group(3) is not None:
            if last_file:
                out.append(f'{last_file}::"{m.group(3)}"')
            continue
        body = m.group(2)
        f, sep, rest = body.partition("::")
        if sep:
            f = f.strip() or last_file
            last_file = f
            out.append(f"{f}::{rest.strip()}")
        elif body.endswith(".el"):
            last_file = body
    return out

def adopt_rows(design_path, root, specs_dir, tests=None, write=False):
    with open(design_path, encoding="utf-8") as fh:
        rows = coverage_rows(fh.read())
    scenarios = read_specs(specs_dir)
    citations = []
    for p in test_files(root, tests):
        citations.extend(citations_in(p)[0])
    resolve(citations, scenarios)
    by_key = {}
    for sc in scenarios:
        by_key.setdefault(sc.title, []).append(sc)
    props, problems = [], []
    for cell, kind, test, note in rows:
        cap, _, title = cell.partition(" § ")
        hits = [sc for sc in by_key.get(title.replace("\\vert{}", "|"), [])
                if sc.cap == cap or sc.cap.startswith(cap + "/")]
        if len(hits) != 1:
            problems.append(f"{cell}: {len(hits)} scenarios match")
            continue
        sc = hits[0]
        if sc.citations or sc.tests:
            continue
        if kind == "none":
            props.append((sc, ["none: " + note.replace("\\vert{}", "|")]))
        elif row_tests(test):
            props.append((sc, row_tests(test)))
        else:
            problems.append(f"{cell}: kind {kind!r} with no test")
    for sc, values in props:
        print(f"{os.path.relpath(sc.path, root)}:{sc.line}  {sc.cap} § {sc.title}")
        for k, v in enumerate(values):
            print(f"    :TEST{'+' if k else ''}: {v}")
    for p in problems:
        print("PROBLEM " + p)
    print(f"{len(props)} properties" + (", written" if write else " (dry run; --write writes them)"))
    if write:
        by_file = {}
        for sc, values in props:
            by_file.setdefault(sc.path, []).append((sc.line, values))
        for path, items in by_file.items():
            with open(path, encoding="utf-8") as fh:
                lines = fh.read().split("\n")
            for line, values in sorted(items, reverse=True):
                lines[line:line] = ([":PROPERTIES:"] + [f":TEST{'+' if k else ''}: {v}" for k, v in enumerate(values)]
                                    + [":END:"])
            with open(path, "w", encoding="utf-8") as fh:
                fh.write("\n".join(lines))
    return 0


# ---------------------------------------------------------------- main

def _git_root(start):
    r = subprocess.run(["git", "-C", start, "rev-parse", "--show-toplevel"], capture_output=True, text=True)
    return r.stdout.strip() if r.returncode == 0 else os.path.abspath(start)


def _where_code_root():
    """The resolved change's code root by `home.py where` from here (a home above it), or None."""
    home = os.path.join(os.path.dirname(os.path.abspath(__file__)), "home.py")
    r = subprocess.run([sys.executable, home, "where"], capture_output=True, text=True)
    try:
        return json.loads(r.stdout).get("code_root")
    except (ValueError, AttributeError):
        return None


def main(argv=None):
    argv = list(sys.argv[1:] if argv is None else argv)
    verb = argv.pop(0) if argv and argv[0] == "adopt-rows" else None
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    if verb:
        ap.add_argument("design")
        ap.add_argument("--write", action="store_true")
    ap.add_argument("--root")
    ap.add_argument("--specs")
    ap.add_argument("--tests", action="append")
    ap.add_argument("--output")
    ap.add_argument("--capability")
    ap.add_argument("--scenario", action="append", help="'<capability> § <title>'; repeatable")
    ap.add_argument("--since")
    ap.add_argument("--intent", help="a change's intent.org: its #+BASE: and its named scenarios")
    ap.add_argument("--uncited", action="store_true")
    ap.add_argument("--json", action="store_true")
    args = ap.parse_args(argv)
    root = os.path.abspath(args.root or _git_root("."))
    from_where = None
    if not args.root and not args.specs and not os.path.isdir(os.path.join(root, "docs", "specs")):
        from_where = _where_code_root()
        root = os.path.abspath(from_where) if from_where else root
    specs = os.path.abspath(args.specs or os.path.join(root, "docs", "specs"))
    tests = [os.path.join(root, t) for t in args.tests] if args.tests else None
    try:
        if not os.path.isdir(specs):
            hint = "" if args.root or args.specs or from_where else \
                "; from a home, `home.py where` resolves no change (set ORCH_CHANGE or pass --root \"$CODE_ROOT\")"
            raise Refusal(f"no specs directory at {specs}{hint}")
        if verb:
            return adopt_rows(args.design, root, specs, tests, args.write)
        rows, unresolved, bad_props = compute(root, specs, tests, args.output)
        known = {(r["capability"], r["scenario"]) for r in rows}
        if args.capability:
            cap = args.capability
            rows = [r for r in rows if r["capability"] == cap or r["capability"].startswith(cap + "/")]
        unmatched = []
        if args.scenario:
            keys = {tuple(x.split(" § ", 1)) for x in args.scenario}
            unmatched += sorted(k for k in keys if k not in known)
            rows = [r for r in rows if (r["capability"], r["scenario"]) in keys]
        if args.since:
            touched = touched_lines(root, specs, args.since)
            rows = [r for r in rows if in_since(r["_sc"], touched)]
        if args.intent:
            intent = args.intent if os.path.isfile(args.intent) else os.path.join(root, args.intent)
            base, named = intent_scope(intent)
            names = set(named)
            unmatched += [k for k in named if k not in known]
            touched = touched_lines(root, specs, base)
            rows = [r for r in rows if in_since(r["_sc"], touched) or (r["capability"], r["scenario"]) in names]
        if args.uncited:
            rows = [r for r in rows if r["status"] == "uncited"]
        if args.json:
            print(json.dumps({"rows": [{k: v for k, v in r.items() if k != "_sc"} for r in rows],
                              "unresolved": [{"where": c.where(root), "shape": c.shape, "capability": c.cap,
                                              "title": c.title} for c in unresolved],
                              "bad_test_properties": [{"spec": f"{os.path.relpath(s.path, root)}:{s.line}",
                                                       "value": v, "why": w} for s, v, w in bad_props],
                              "unmatched_names": [f"{c} § {t}" for c, t in unmatched]},
                             separators=(",", ":")))
        else:
            print_report(rows, unresolved, bad_props, root, unmatched)
        return 0
    except Refusal as e:
        print(f"refused: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
