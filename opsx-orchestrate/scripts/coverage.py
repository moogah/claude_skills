#!/usr/bin/env python3
"""Scenario coverage, computed from the specs and the tests (roadmap 2.1).

Reads every scenario of the org specs (`docs/specs/**/*.org`: a heading `Scenario: <title>`,
its capability the file's path under the specs directory without `.org`, and its optional
`:TEST:` property) and every citation in the code root's test files (`*-spec.el`, `*-test.el`,
`test-*.el`):

  (a)  an `it` / `describe` string ending `(Scenario: <cap> § <title>[; <cap> § <title>])`
  (b)  a comment `;; Scenario: <cap> § <title>`, the title possibly finished on the next
       comment line
  (a') an `ert-deftest` docstring line `Scenario: <cap> § '<title>'`

A citation naming a capability directory (`bash-parser`) resolves over its files.

A `:TEST:` property stands in for a citation the test cannot carry (templates/spec.md):
`none: <reason>`, or `<file>::"<start of the it string>"`, or `<file>::<ert test name>`;
`:TEST+:` adds another.

Status per scenario, worst first: failed, not run (a run output was given and the test is
not in it), pending (`xit`, an `it` with no body, or PENDING in the output), cited (no run
output given), pass, none: <reason>, uncited. With `--output`, a raw `./bin/run-tests.sh`
output: Buttercup lines join on the `it` string, ERT lines on the test name.

Usage:
  coverage.py [--root CODE_ROOT] [--specs DIR] [--tests DIR ...] [--output RUN_OUTPUT]
              [--capability ID] [--scenario 'CAP § TITLE' ...] [--since COMMIT]
              [--uncited] [--json]
  coverage.py adopt-rows DESIGN_ORG [--root …] [--specs …] [--write]
      a change's legacy `* Scenario coverage` table -> `:TEST:` properties for its `none`
      rows and for rows naming a test that carries no citation; prints them, --write
      writes them into the spec files

Exit 0 always for the report; 1 on a refusal (bad arguments, git failure).
"""

import argparse
import json
import os
import re
import subprocess
import sys

TEST_HEADS = {"it", "xit", "describe", "xdescribe", "ert-deftest"}
STATUS_ORDER = ["failed", "not run", "pending", "cited", "pass"]
SKIP_DIRS = {"elpa", "eln-cache", "straight", "runtime", "node_modules"}


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
                    else:
                        buf.append(text[j + 1])
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
                if f.endswith(("-spec.el", "-test.el")) or (f.startswith("test-") and f.endswith(".el")):
                    found.append(os.path.join(dirpath, f))
    return found


def citations_in(path, text=None):
    """([Citation], [Form]) of one test file."""
    if text is None:
        with open(path, encoding="utf-8", errors="replace") as fh:
            text = fh.read()
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
_ERT = re.compile(r"^\s*(passed|FAILED|skipped)\s+\d+/\d+\s+(\S+)")
# Buttercup's final line for an `it`: green pass, yellow pending, red failed (the failure
# message follows in red, not the word FAILED); without colours, the words decide.
_COLOURED = re.compile(r"^\x1b\[(3[123])m\s*(.*?)\x1b\[0m.*\(\d[\d.]*(?:ms|s|m)\)\s*$")
_COLOUR = {"32": "pass", "33": "pending", "31": "failed"}
_BUTTER = re.compile(r"^\s+(.*?)\s+(?:(PENDING|FAILED|SKIPPED)\s+)?\((\d[\d.]*(?:ms|s|m))\)\s*$")


def parse_output(text):
    """({it string: {status}}, {ert name: {status}}) from a raw run-tests.sh output."""
    butter, ert = {}, {}
    word = {"passed": "pass", "FAILED": "failed", "skipped": "pending",
            None: "pass", "PENDING": "pending", "SKIPPED": "pending"}
    for raw in text.split("\n"):
        seg = raw.split("\r")[-1]
        line = _ANSI.sub("", seg)
        m = _ERT.match(line)
        if m:
            ert.setdefault(m.group(2), set()).add(word[m.group(1)])
            continue
        m = _COLOURED.match(seg)
        if m:
            butter.setdefault(m.group(2), set()).add(_COLOUR[m.group(1)])
            continue
        m = _BUTTER.match(line)
        if m:
            butter.setdefault(m.group(1), set()).add(word[m.group(2)])
    return butter, ert


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
    if run is None or form.kind in ("describe", "xdescribe") or form.name is None:
        return "cited"
    table = run[1] if form.kind == "ert-deftest" else run[0]
    found = table.get(form.name)
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
        if name is not None and f.kind == "ert-deftest" and f.name == name:
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
    run = None
    if output:
        with open(output, encoding="utf-8", errors="replace") as fh:
            run = parse_output(fh.read())
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


def print_report(rows, unresolved, bad_props, root):
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
    ap.add_argument("--uncited", action="store_true")
    ap.add_argument("--json", action="store_true")
    args = ap.parse_args(argv)
    root = os.path.abspath(args.root or _git_root("."))
    specs = os.path.abspath(args.specs or os.path.join(root, "docs", "specs"))
    tests = [os.path.join(root, t) for t in args.tests] if args.tests else None
    try:
        if not os.path.isdir(specs):
            raise Refusal(f"no specs directory at {specs}")
        if verb:
            return adopt_rows(args.design, root, specs, tests, args.write)
        rows, unresolved, bad_props = compute(root, specs, tests, args.output)
        if args.capability:
            cap = args.capability
            rows = [r for r in rows if r["capability"] == cap or r["capability"].startswith(cap + "/")]
        if args.scenario:
            keys = {tuple(x.split(" § ", 1)) for x in args.scenario}
            rows = [r for r in rows if (r["capability"], r["scenario"]) in keys]
        if args.since:
            touched = touched_lines(root, specs, args.since)
            rows = [r for r in rows if in_since(r["_sc"], touched)]
        if args.uncited:
            rows = [r for r in rows if r["status"] == "uncited"]
        if args.json:
            print(json.dumps({"rows": [{k: v for k, v in r.items() if k != "_sc"} for r in rows],
                              "unresolved": [{"where": c.where(root), "shape": c.shape, "capability": c.cap,
                                              "title": c.title} for c in unresolved],
                              "bad_test_properties": [{"spec": f"{os.path.relpath(s.path, root)}:{s.line}",
                                                       "value": v, "why": w} for s, v, w in bad_props]},
                             separators=(",", ":")))
        else:
            print_report(rows, unresolved, bad_props, root)
        return 0
    except Refusal as e:
        print(f"refused: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
