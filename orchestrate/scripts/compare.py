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
failures the runner counted but may not name (Buttercup's error-type failures, modules that
fail to load on their own), so a higher count after is a regression too even when no new name
appears.
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
    Buttercup `Ran N (out of M) specs, K failed` with a detail block per failure: the spec's full
    description on one line, then `FAILED: <expectation>` or, when the spec signalled an error,
    `Traceback (most recent call last):`. The per-spec marker on the spec's own line
    (`<it description>  FAILED (0.54ms)`) names the failure too, by its leaf description; it is
    read when no detail block names it. The record: a traceback failure carried no `FAILED:` line,
    the parser named four of five failures in each file, and one fixed and one new failure
    cancelled to NO-REGRESSION (package-boundaries cycle-1792083023).
    ERT's lower-case `failed  N/M <name> (T sec)` line is an expected failure (`:expected-result
    :failed`) and names no failing id: read as one, it gave 2,256 false ids over three homes (maps
    24, 25, 27) and "6 spurious 'fixed'" in fast-test-runner. The lean runner's `Loaded N modules on
    their own, K failed.` counts K in not_passing: a module that fails to load alone is a failure
    whichever form, if any, names it."""
    t = _strip_ansi(text)
    ids, ran, not_passing, found = set(), 0, 0, False
    e = re.search(r"^Ran (\d+) tests?, (\d+) results? as expected, (\d+) unexpected", t, re.M)
    if e:
        found = True
        ran += int(e.group(1))
        for line in t.splitlines():
            m = re.match(r"^\s+FAILED\s+(?:\d+/\d+\s+)?(.+?)(?:\s+\([0-9.]+ sec\))?(\s+at\s+.*)?$", line)
            if m and (" sec)" in line or " at " in line):
                ids.add(m.group(1).strip())
    mods = re.search(r"^Loaded (\d+) modules? on (?:their|its) own, (\d+) failed", t, re.M)
    if mods:
        found = True
        not_passing += int(mods.group(2))
    b = re.search(r"^Ran (\d+) (?:out of (\d+) )?specs?, (\d+) failed", t, re.M)
    if b:
        found = True
        ran += int(b.group(1))
        not_passing += int(b.group(3))
        lines = t.splitlines()
        blocks, leaves = set(), set()
        for i in range(1, len(lines)):
            if (lines[i].startswith("FAILED:") or lines[i].startswith("Traceback (most recent call last)")) \
                    and not lines[i - 1].startswith(("FAILED:", "Traceback")):
                j = i - 1
                while j > 0 and not lines[j].strip():
                    j -= 1
                blocks.add(lines[j].strip())
            m = re.match(r"^\s+(.+?)\s+FAILED \([0-9.]+m?s\)\s*$", lines[i])
            if m:
                leaves.add(m.group(1).strip())
        ids |= blocks
        ids |= {leaf for leaf in leaves if not any(b.endswith(leaf) for b in blocks)}
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
