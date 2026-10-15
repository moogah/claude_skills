#!/usr/bin/env python3
"""Set-compare two test runs by test id: the overlay's `test.compare` for a red baseline
(flows/execute.md § 4).

    compare.py --parser <emacs-results-txt | junit-xml | tap> <baseline-file> <after-file>

Exit 0: no test fails after that did not fail before. Exit 1: a regression, and the new
failing ids are printed one per line after a summary. Exit 2: a file could not be parsed as
the named format (no run was found in it), which the caller must not read as "no regression".
`exit-code` has no ids and is refused: with that parser the flow's exit-code rule applies and
a red baseline needs an `environment` ask.

A parser returns (failing ids, ran, not_passing) from a run's output. `not_passing` counts
failures the runner counted but did not name (Buttercup's error-type failures), so a higher
count after is a regression too even when no new name appears.
"""

import argparse
import re
import sys
import xml.etree.ElementTree as ET

PARSERS = {}


def parser(name):
    def wrap(fn):
        PARSERS[name] = fn
        return fn
    return wrap


def _strip_ansi(text):
    return re.sub(r"\x1b\[[0-9;]*m", "", text)


@parser("emacs-results-txt")
def parse_emacs(text):
    """ERT and Buttercup output as ./bin/run-tests.sh prints it (either suite may be absent):
    ERT `Ran N tests, M results as expected, K unexpected` with `FAILED <name>` lines (an
    unexpected failure carries `(N sec) at FILE`; a bare name is an expected failure);
    Buttercup `Ran N (out of M) specs, K failed` with `FAILED:` lines preceded by the spec name."""
    t = _strip_ansi(text)
    ids, ran, not_passing, found = set(), 0, 0, False
    e = re.search(r"^Ran (\d+) tests?, (\d+) results? as expected, (\d+) unexpected", t, re.M)
    if e:
        found = True
        ran += int(e.group(1))
        for line in t.splitlines():
            m = re.match(r"^\s+(FAILED|failed)\s+(?:\d+/\d+\s+)?(.+?)(?:\s+\([0-9.]+ sec\))?(\s+at\s+.*)?$", line)
            if m and (" sec)" in line or " at " in line):
                ids.add(m.group(2).strip())
    b = re.search(r"^Ran (\d+) (?:out of (\d+) )?specs?, (\d+) failed", t, re.M)
    if b:
        found = True
        ran += int(b.group(1))
        not_passing += int(b.group(3))
        lines = t.splitlines()
        for i in range(1, len(lines)):
            if lines[i].startswith("FAILED:") and not lines[i - 1].startswith("FAILED:"):
                ids.add(lines[i - 1].strip())
    if not found and re.search(r"^## (ERT|Buttercup)", t, re.M):  # a hand-made baseline-failures file
        found = True
        section = None
        for line in t.splitlines():
            m = re.match(r"^## (ERT|Buttercup)", line)
            if m:
                section = m.group(1)
                continue
            if section and line.strip() and not line.startswith("#"):
                ids.add(line.split()[0] if section == "ERT" else line.strip())
        m = re.search(r"BUTTERCUP_NOT_PASSING=(\d+)", t)
        if m:
            not_passing = int(m.group(1))
    return (ids, ran, not_passing) if found else None


@parser("junit-xml")
def parse_junit(text):
    try:
        root = ET.fromstring(text)
    except ET.ParseError:
        return None
    cases = list(root.iter("testcase"))
    if not cases and root.tag not in ("testsuite", "testsuites"):
        return None
    ids = set()
    for c in cases:
        if any(ch.tag in ("failure", "error") for ch in c):
            cls = c.get("classname")
            ids.add(f"{cls}.{c.get('name')}" if cls else c.get("name") or "?")
    return ids, len(cases), 0


@parser("tap")
def parse_tap(text):
    ids, ran, found = set(), 0, False
    for line in _strip_ansi(text).splitlines():
        if re.match(r"^(TAP version \d+|1\.\.\d+)", line):
            found = True
        m = re.match(r"^(not )?ok\b\s*(\d+)?\s*-?\s*(.*?)\s*(#.*)?$", line)
        if not m:
            continue
        found = True
        ran += 1
        directive = (m.group(4) or "").lower()
        if m.group(1) and "todo" not in directive and "skip" not in directive:
            ids.add(m.group(3).strip() or m.group(2) or "?")
    return (ids, ran, 0) if found else None


def compare(parser_name, baseline_text, after_text):
    """(exit code, lines to print)."""
    if parser_name == "exit-code":
        return 2, ["exit-code gives no test ids: a red baseline cannot be compared; the flow's "
                   "exit-code rule applies and a red baseline needs an environment ask (flows/execute.md § 4)"]
    fn = PARSERS.get(parser_name)
    if fn is None:
        return 2, [f"unknown parser {parser_name!r}; one of {sorted(PARSERS)} (overlay.md test.parser)"]
    base, after = fn(baseline_text), fn(after_text)
    if base is None or after is None:
        which = "baseline" if base is None else "after"
        return 2, [f"the {which} file holds no {parser_name} run; nothing to compare"]
    b_ids, b_ran, b_np = base
    a_ids, a_ran, a_np = after
    new = sorted(a_ids - b_ids)
    fixed = sorted(b_ids - a_ids)
    count_reg = a_np > b_np and a_np - b_np > len(new)
    lines = [f"baseline: {b_ran} ran, {len(b_ids)} failing ids; after: {a_ran} ran, {len(a_ids)} failing ids; "
             f"new: {len(new)}, fixed: {len(fixed)}"]
    if a_np or b_np:
        lines.append(f"not passing (counted, named or not): {a_np} after, {b_np} baseline"
                     + (" — MORE THAN BASELINE" if a_np > b_np else ""))
    lines += [f"new failure: {i}" for i in new]
    if count_reg and not new:
        lines.append("new failure: <unnamed: the runner counted a failure it did not name>")
    reg = bool(new) or count_reg
    lines.append("REGRESSION" if reg else "NO-REGRESSION")
    return (1 if reg else 0), lines


def main(argv=None):
    p = argparse.ArgumentParser(prog="compare.py", description=__doc__.split("\n\n")[0])
    p.add_argument("--parser", required=True, help="emacs-results-txt | junit-xml | tap (exit-code is refused)")
    p.add_argument("baseline")
    p.add_argument("after")
    args = p.parse_args(argv)
    try:
        base = open(args.baseline, encoding="utf-8", errors="replace").read()
        after = open(args.after, encoding="utf-8", errors="replace").read()
    except OSError as e:
        print(f"cannot read: {e}", file=sys.stderr)
        return 2
    code, lines = compare(args.parser, base, after)
    print("\n".join(lines), file=sys.stderr if code == 2 else sys.stdout)
    return code


if __name__ == "__main__":
    sys.exit(main())
