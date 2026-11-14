"""pytest suite for coverage.py (stdlib + pytest). Run: python3 -m pytest test_coverage.py  (from this directory)."""

import importlib.util
import json
import pathlib
import subprocess

import pytest

HERE = pathlib.Path(__file__).resolve().parent
FIX = HERE / "fixtures" / "coverage"
_spec = importlib.util.spec_from_file_location("coverage_tool", HERE / "coverage.py")
cov = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(cov)

SPEC = """#+TITLE: ws

* Requirements

** Requirement: R
*** Scenario: Passing one
- *WHEN* a
*** Scenario: Failing one
- *WHEN* a
*** Scenario: Pending one
- *WHEN* a
*** Scenario: Missing from the run
- *WHEN* a
*** Scenario: Wrapped title that goes
- *WHEN* a
*** Scenario: Kept by a row
:PROPERTIES:
:TEST: ws-spec.el::"answers by a row"
:TEST+: old-test.el::test-legacy
:END:
- *WHEN* a
*** Scenario: Not driveable
:PROPERTIES:
:TEST: none: needs a display
:END:
- *WHEN* a
*** Scenario: Nobody
- *WHEN* a
#+begin_example
*** Scenario: inside a block
#+end_example
"""

TESTS = r'''
;; Header: cite as (Scenario: <capability> § <title>)
(describe "ws (seam/x)"
  (it "passes (Scenario: ws § Passing one)"
    (should ?\" ?; ?\( "a \"quoted\" (paren")
    (expect t :to-be t))
  (it "fails (Scenario: ws § Failing one; ws § Missing from the run)"
    (expect t :to-be nil))
  (it "is not written yet (Scenario: ws § Pending one)")
  (it "covers the wrapped one"
    ;; Scenario: ws § Wrapped title
    ;; that goes.  More prose here.
    (expect 1 :to-be 1))
  (it "answers by a row and more"
    (expect 1 :to-be 1))
  (it "cites a gone capability (Scenario: gone § Something)"
    (expect 1 :to-be 1)))
'''

ERT = '''(ert-deftest test-legacy ()
  "Do the legacy thing.
Scenario: ws § 'Nobody else'"
  (should t))
'''

RUN = ("ws (seam/x)\n"
       "  passes (Scenario: ws § Passing one)\r\x1b[32m  passes (Scenario: ws § Passing one)\x1b[0m (1.0ms)\n"
       "  fails (Scenario: ws § Failing one; ws § Missing from the run)\r\x1b[31m  fails (Scenario: ws § Failing one; "
       "ws § Missing from the run)\x1b[0m\x1b[31m  Expected t to be nil\x1b[0m (2.0ms)\n"
       "  is not written yet (Scenario: ws § Pending one)\r\x1b[33m  is not written yet (Scenario: ws § Pending one)"
       "\x1b[0m\x1b[33m  PENDING\x1b[0m (0.1ms)\n"
       "  covers the wrapped one\r\x1b[32m  covers the wrapped one\x1b[0m (1.0ms)\n"
       "  answers by a row and more\r\x1b[32m  answers by a row and more\x1b[0m (1.0ms)\n"
       "   passed  1/1  test-legacy (0.001 sec)\n"
       "\x1b[31mws (seam/x) fails (Scenario: ws § Failing one)\x1b[0m\n"
       "Ran 5 out of 5 specs, 1 failed, in 0.1s.\n")


@pytest.fixture
def tree(tmp_path):
    (tmp_path / "docs" / "specs").mkdir(parents=True)
    (tmp_path / "docs" / "specs" / "ws.org").write_text(SPEC)
    t = tmp_path / "config" / "ws" / "test"
    t.mkdir(parents=True)
    (t / "ws-spec.el").write_text(TESTS)
    (t / "old-test.el").write_text(ERT)
    (tmp_path / "run.txt").write_text(RUN)
    return tmp_path


def rows_by_title(rows):
    return {r["scenario"]: r for r in rows}


# ---------------------------------------------------------------- lexer

def test_scan_el_forms_names_and_pending():
    events, forms = cov.scan_el(TESTS + ERT)
    names = [(f.kind, f.name, f.pending) for f in forms]
    assert names[0] == ("describe", "ws (seam/x)", False)
    assert ("it", "passes (Scenario: ws § Passing one)", False) in names
    assert ("it", "is not written yet (Scenario: ws § Pending one)", True) in names
    assert ("ert-deftest", "test-legacy", False) in names


def test_scan_el_char_literals_do_not_open_strings():
    events, _ = cov.scan_el('(it "x" (list ?\\" ?; ?\\())\n;; after\n(it "y")\n')
    assert [e[1] for e in events] == ["x", ";; after", "y"]
    assert events[1][2] == 2


def test_xit_and_xdescribe_are_pending():
    _, forms = cov.scan_el('(xdescribe "d" (it "a" (f)))\n(xit "b" (f))\n')
    assert [(f.name, f.pending) for f in forms] == [("d", True), ("a", True), ("b", True)]


# ---------------------------------------------------------------- citations

def test_citation_shapes(tree):
    cs, _ = cov.citations_in(str(tree / "config/ws/test/ws-spec.el"))
    got = [(c.shape, c.cap, c.title) for c in cs]
    assert ("a", "ws", "Failing one") in got and ("a", "ws", "Missing from the run") in got
    assert ("b", "ws", "Wrapped title") in got
    assert ("a", "gone", "Something") in got
    assert not any("<title>" in t for _, _, t in got)  # the header comment is not a citation
    ert, _ = cov.citations_in(str(tree / "config/ws/test/old-test.el"))
    assert [(c.shape, c.title, c.line) for c in ert] == [("a'", "Nobody else", 3)]


def test_spec_md_comment_form_ignored():
    cs, _ = cov.citations_in("x.el", ';; Scenario: specs/chat-mode/spec.md (delta) § "Req" ->\n;; "S"\n'
                                     ';; Scenario: spec.md § "Req" -> "S"\n')
    assert cs == []


def test_directory_capability_resolves_over_files():
    a = cov.Scenario("bash-parser/core", "T", "core.org", 1, 2)
    b = cov.Scenario("bash-parser/security", "T", "security.org", 1, 2)
    c = cov.Scenario("bash-parserx", "T", "x.org", 1, 2)
    cite = cov.Citation("bash-parser", "T", "t.el", 1, None, "a'")
    assert cov.resolve([cite], [a, b, c]) == []
    assert cite.scenarios == [a, b]


# ---------------------------------------------------------------- specs

def test_read_specs_capability_ids_and_properties(tmp_path):
    (tmp_path / "bash-parser").mkdir()
    (tmp_path / "bash-parser" / "core.org").write_text("* Requirements\n** Requirement: R\n*** Scenario: A\n")
    (tmp_path / "ws.org").write_text(SPEC)
    scs = cov.read_specs(str(tmp_path))
    assert ("bash-parser/core", "A") in [(s.cap, s.title) for s in scs]
    by = {s.title: s for s in scs if s.cap == "ws"}
    assert "inside a block" not in by
    assert by["Kept by a row"].tests == ['ws-spec.el::"answers by a row"', "old-test.el::test-legacy"]
    assert by["Passing one"].end == by["Failing one"].line - 1


# ---------------------------------------------------------------- run output

def test_parse_output_colours_and_ert():
    run = cov.parse_output(RUN)
    assert run.it["passes (Scenario: ws § Passing one)"] == {"pass"}
    assert run.it["fails (Scenario: ws § Failing one; ws § Missing from the run)"] == {"failed"}
    assert run.it["is not written yet (Scenario: ws § Pending one)"] == {"pending"}
    assert run.ert == {"test-legacy": {"pass"}}
    assert not any(k.startswith("ws (seam/x)") for k in run.it)  # the failure summary is not an it line


def test_parse_output_without_colours():
    run = cov.parse_output("  a thing  FAILED (1ms)\n  b thing (2ms)\n  c  PENDING (0ms)\n")
    assert run.it == {"a thing": {"failed"}, "b thing": {"pass"}, "c": {"pending"}}
    # ERT's lower-case `failed` is an expected failure (:expected-result :failed)
    assert cov.parse_output("   failed  12/49  test-x (0.001318 sec)\n").ert == {"test-x": {"pending"}}


def _statuses(elisp, output):
    """{it string: status} for each `it` of ELISP against the run OUTPUT."""
    run = cov.parse_output(output)
    return {f.name: cov.citation_status(f, run) for f in cov.scan_el(elisp)[1] if f.kind == "it"}


GLUED_ITS = [
    "registers the new session's file-level :ID: so org-id resolves it to the file without visiting it "
    "(seam/unindexed-id-targets) (Scenario: gptel/sessions-persistence § New session file carries an org ID)",
    "offers no menu entry and runs no teardown while magit is not installed, with no error "
    "(Scenario: workspace-git-worktrees § Absent magit means no git integration, not an error)",
    "offers the add-worktree entry and runs the teardown while magit is installed but not loaded, loading it "
    "on use (Scenario: workspace-git-worktrees § magit installed but not yet loaded yields the git integration)",
    "evaluates configuration forms in the worktree's loaded configuration "
    "(Scenario: emacs-control-tools § Eval runs in the loaded configuration)",
]


def test_lines_a_message_splits_are_joined():
    # Trimmed from fast-test-runner's after-orphaned-child-emacs-deadlock: a `load` in the spec prints
    # "Loading ..." after the title, or a library prints three lines, and the time lands on a line of
    # its own; the old reader gave 26 of the file's scenarios "not run". The last spec was cancelled.
    out = (FIX / "glued-messages.txt").read_text()
    elisp = "".join(f'(it "{t}" (f))\n' for t in GLUED_ITS)
    assert list(_statuses(elisp, out).values()) == ["pass", "pass", "pass", "pending"]
    # a title that only begins like a glued line is not taken for it
    assert _statuses('(it "offers no menu" (f))', out) == {"offers no menu": "not run"}


def test_a_title_holding_newlines_is_joined():
    # Trimmed from useful-in-buffer-completion's after-snippet-keeps-paren-closed-further-on-fix: the
    # it string's \n escapes print as line breaks inside the green result (arch-cycle-1792328155-4).
    out = (FIX / "newline-title.txt").read_text()
    elisp = ('(it "defvar then TAB in Emacs Lisp after a ( closed after whitespace alone, (| ) or (|\\n), leaves '
             'one opening parenthesis; after a comment, (| ; c\\n), keeps the typed parenthesis - seam/snippet-source '
             '(Scenario: in-buffer-completion § A snippet takes the place of the parenthesis typed before its key)" (f))\n'
             '(it "in a snippet field, def then TAB with defaultdict showing inserts the suggestion '
             '(seam/inline-suggestion)" (f))\n')
    st = _statuses(elisp, out)
    assert list(st.values()) == ["pass", "pass"] and "(|\n)" in list(st)[0]


def test_compact_output_is_refused(tree):
    # Trimmed from fast-test-runner's after-inline-fix-ncodo-review: the runner's default report counts
    # specs and names only failures, so every citation read "not run" (ask-cycle-1792275615-6: refuse it).
    (tree / "run.txt").write_text((FIX / "compact-report.txt").read_text())
    with pytest.raises(cov.Refusal, match=r"no line per test.*run-20261020-034338-67180\.log"):
        cov.compute(str(tree), str(tree / "docs/specs"), output=str(tree / "run.txt"))
    assert cov.main(["--root", str(tree), "--output", str(tree / "run.txt")]) == 1


# ---------------------------------------------------------------- end to end

def test_statuses_with_a_run(tree):
    rows, unresolved, bad = cov.compute(str(tree), str(tree / "docs/specs"), output=str(tree / "run.txt"))
    st = {t: r["status"] for t, r in rows_by_title(rows).items()}
    assert st == {"Passing one": "pass", "Failing one": "failed", "Pending one": "pending",
                  "Missing from the run": "failed", "Wrapped title that goes": "pass",
                  "Kept by a row": "pass", "Not driveable": "none: needs a display", "Nobody": "uncited"}
    assert sorted((c.cap, c.title) for c in unresolved) == [("gone", "Something"), ("ws", "Nobody else")]
    assert bad == []


def test_statuses_without_a_run(tree):
    rows, _, _ = cov.compute(str(tree), str(tree / "docs/specs"))
    st = {t: r["status"] for t, r in rows_by_title(rows).items()}
    assert st["Passing one"] == "cited" and st["Pending one"] == "pending" and st["Nobody"] == "uncited"


def test_test_not_in_run_is_not_run(tree):
    (tree / "run.txt").write_text("Ran 0 specs\n")
    rows, _, _ = cov.compute(str(tree), str(tree / "docs/specs"), output=str(tree / "run.txt"))
    assert rows_by_title(rows)["Passing one"]["status"] == "not run"


def test_bad_test_property_listed(tree):
    p = tree / "docs/specs/ws.org"
    p.write_text(p.read_text().replace('"answers by a row"', '"no such test"'))
    rows, _, bad = cov.compute(str(tree), str(tree / "docs/specs"))
    assert [(v, w) for _, v, w in bad] == [('ws-spec.el::"no such test"', "no test in ws-spec.el matches")]
    assert rows_by_title(rows)["Kept by a row"]["status"] == "cited"  # the :TEST+: still holds


def test_prefix_with_ellipsis_resolves(tree):
    p = tree / "docs/specs/ws.org"
    p.write_text(p.read_text().replace('"answers by a row"', '"answers by …"'))
    _, _, bad = cov.compute(str(tree), str(tree / "docs/specs"))
    assert bad == []


def _git(root, *args):
    subprocess.run(["git", "-C", str(root), *args], check=True, capture_output=True)


def test_since_selects_touched_scenarios(tree, capsys):
    _git(tree, "init", "-q")
    _git(tree, "-c", "user.email=t@t", "-c", "user.name=t", "add", ".")
    _git(tree, "-c", "user.email=t@t", "-c", "user.name=t", "commit", "-qm", "base")
    p = tree / "docs/specs/ws.org"
    p.write_text(p.read_text().replace("*** Scenario: Failing one\n- *WHEN* a", "*** Scenario: Failing one\n- *WHEN* b"))
    p.write_text(p.read_text().replace("- *WHEN* a\n#+begin_example", "- *WHEN* a\n\n-----\n\n#+begin_example"))  # blank and rule after Nobody
    (tree / "docs/specs/new.org").write_text("* Requirements\n** Requirement: N\n*** Scenario: Fresh\n")
    assert cov.main(["--root", str(tree), "--since", "HEAD", "--json"]) == 0
    rows = json.loads(capsys.readouterr().out)["rows"]
    assert sorted((r["capability"], r["scenario"]) for r in rows) == [("new", "Fresh"), ("ws", "Failing one")]


def test_capability_and_uncited_filters(tree, capsys):
    assert cov.main(["--root", str(tree), "--capability", "ws", "--uncited", "--json"]) == 0
    assert [r["scenario"] for r in json.loads(capsys.readouterr().out)["rows"]] == ["Nobody"]


def test_scenario_filter(tree, capsys):
    assert cov.main(["--root", str(tree), "--scenario", "ws § Nobody", "--scenario", "ws § Pending one", "--json"]) == 0
    assert sorted(r["scenario"] for r in json.loads(capsys.readouterr().out)["rows"]) == ["Nobody", "Pending one"]


def test_unmatched_scenario_names_are_listed(tree, capsys):
    assert cov.main(["--root", str(tree), "--scenario", "ws § Nobody", "--scenario", "ws § Nobdy", "--json"]) == 0
    out = json.loads(capsys.readouterr().out)
    assert [r["scenario"] for r in out["rows"]] == ["Nobody"]
    assert out["unmatched_names"] == ["ws § Nobdy"]


def test_intent_selects_touched_and_named_scenarios(tree, capsys):
    """A change cut from an integration branch whose spec edits lie before its base: its intent
    names them; the diff from #+BASE: adds what the change edits itself."""
    _git(tree, "init", "-q")
    _git(tree, "-c", "user.email=t@t", "-c", "user.name=t", "add", ".")
    _git(tree, "-c", "user.email=t@t", "-c", "user.name=t", "commit", "-qm", "hand-off")
    base = subprocess.run(["git", "-C", str(tree), "rev-parse", "HEAD"], capture_output=True, text=True).stdout.strip()
    p = tree / "docs/specs/ws.org"
    p.write_text(p.read_text().replace("*** Scenario: Failing one\n- *WHEN* a", "*** Scenario: Failing one\n- *WHEN* b"))
    d = tree / "docs/changes/c"
    d.mkdir(parents=True)
    (d / "intent.org").write_text(
        f"#+TITLE: c\n#+BASE: {base[:8]}\n\n* Capabilities\n- ws: adds R\n\n"
        "- ws § Nobody: stated, not met (changed)\n- ws § Wrapped title\n  that goes: stated, not met\n"
        "- ws § Gone away: stated, not met\n\n* Removals\n- ws § Pending one: stated, not met\n")
    assert cov.main(["--root", str(tree), "--intent", "docs/changes/c/intent.org", "--json"]) == 0
    out = json.loads(capsys.readouterr().out)
    assert sorted(r["scenario"] for r in out["rows"]) == ["Failing one", "Nobody", "Wrapped title that goes"]
    assert out["unmatched_names"] == ["ws § Gone away"]


def test_intent_without_base_is_refused(tree, capsys):
    (tree / "intent.org").write_text("#+TITLE: c\n* Capabilities\n")
    assert cov.main(["--root", str(tree), "--intent", str(tree / "intent.org")]) == 1
    assert "#+BASE:" in capsys.readouterr().err


# ---------------------------------------------------------------- pytest

PY_TESTS = '''import pytest


def test_saved_items(cli):
    """Scenario: ws § Passing one
    seam/cli"""


class TestDownload:
    def test_wrapped(self):
        """Scenario: ws § Wrapped title
        that goes. More prose here."""

    @pytest.mark.parametrize("x", [1, 2])
    def test_param(self, x):
        """Scenario: ws § Failing one — prose after the title."""

    def helper(self):
        """Scenario: ws § Pending one"""
'''

# pytest -v, as reddit-scraper's runner prints it with -v (its after-files were taken with -q)
PY_RUN = """============================= test session starts ==============================
collected 4 items

tests/test_cli.py::test_saved_items PASSED                               [ 25%]
tests/test_cli.py::TestDownload::test_wrapped PASSED                     [ 50%]
tests/test_cli.py::TestDownload::test_param[1] PASSED                    [ 75%]
tests/test_cli.py::TestDownload::test_param[2] FAILED                    [100%]

=================================== FAILURES ===================================
=========================== short test summary info ============================
FAILED tests/test_cli.py::TestDownload::test_param[2] - assert False
========================= 1 failed, 3 passed in 0.12s ==========================
"""


@pytest.fixture
def pytree(tmp_path):
    (tmp_path / "docs" / "specs").mkdir(parents=True)
    (tmp_path / "docs" / "specs" / "ws.org").write_text(SPEC)
    (tmp_path / "tests").mkdir()
    (tmp_path / "tests" / "test_cli.py").write_text(PY_TESTS)
    (tmp_path / "tests" / "conftest.py").write_text('"""Scenario: ws § Nobody"""\n')  # not a test file
    (tmp_path / "run.txt").write_text(PY_RUN)
    return tmp_path


def test_pytest_docstring_citations(pytree):
    cs, forms = cov.citations_in(str(pytree / "tests/test_cli.py"))
    assert [(c.cap, c.title, c.line, c.form.name) for c in cs] == [
        ("ws", "Passing one seam/cli", 5, "test_saved_items"),
        ("ws", "Wrapped title that goes. More prose here", 11, "TestDownload::test_wrapped"),
        ("ws", "Failing one — prose after the title", 16, "TestDownload::test_param"),
        ("ws", "Pending one", 19, "TestDownload::helper")]
    assert ("Passing one" in cs[0].alt) and ("Failing one" in cs[2].alt)


def test_pytest_statuses(pytree):
    rows, unresolved, _ = cov.compute(str(pytree), str(pytree / "docs/specs"), output=str(pytree / "run.txt"))
    st = {t: r["status"] for t, r in rows_by_title(rows).items()}
    assert (st["Passing one"], st["Wrapped title that goes"], st["Failing one"]) == ("pass", "pass", "failed")
    assert st["Pending one"] == "not run" and st["Nobody"] == "uncited" and unresolved == []
    rows, _, _ = cov.compute(str(pytree), str(pytree / "docs/specs"))
    assert rows_by_title(rows)["Passing one"]["status"] == "cited"
    # a :TEST: row names a pytest node after the file
    p = pytree / "docs/specs/ws.org"
    p.write_text(p.read_text().replace('ws-spec.el::"answers by a row"', "test_cli.py::TestDownload::test_wrapped"))
    rows, _, bad = cov.compute(str(pytree), str(pytree / "docs/specs"), output=str(pytree / "run.txt"))
    assert rows_by_title(rows)["Kept by a row"]["status"] == "pass"
    # pytest without -v names no passing test: refused
    (pytree / "run.txt").write_text("tests/test_cli.py ...F                                                  [100%]\n"
                                    "========================= 1 failed, 3 passed in 0.12s ==========================\n")
    with pytest.raises(cov.Refusal, match="pytest"):
        cov.compute(str(pytree), str(pytree / "docs/specs"), output=str(pytree / "run.txt"))


# ---------------------------------------------------------------- run from the home

def test_run_from_a_home_finds_the_code_root(tmp_path, monkeypatch, capsys):
    import home as home_py
    home, code = tmp_path / "home", tmp_path / "home" / "code"
    (home / ".claude" / "orchestrator").mkdir(parents=True)
    (home / ".claude" / "orchestrator" / "config.yaml").write_text("overlay-version: 1\nproject: scratch\ncode-root: code\n")
    (home / ".orchestrator").mkdir()
    (code / "docs" / "specs").mkdir(parents=True)
    (code / "docs" / "specs" / "ws.org").write_text(SPEC)
    _git(code, "init", "-q")
    assert home_py.main(["--home", str(home), "open", "chg", "--worktree", str(code)]) == 0
    capsys.readouterr()
    monkeypatch.delenv("ORCH_CHANGE", raising=False)
    monkeypatch.chdir(home)
    assert cov.main(["--json"]) == 0
    assert "Nobody" in [r["scenario"] for r in json.loads(capsys.readouterr().out)["rows"]]
    (home / "docs").mkdir()  # still the home's: --root wins, and with no specs there it refuses
    assert cov.main(["--root", str(home)]) == 1 and "no specs directory" in capsys.readouterr().err


# ---------------------------------------------------------------- adopt-rows

@pytest.mark.parametrize("cell,want", [
    ('=a-spec.el::"x"=', ['a-spec.el::"x"']),
    ('=a-spec.el::"x"=, =::"y"=', ['a-spec.el::"x"', 'a-spec.el::"y"']),
    ('=a-spec.el= "x"; =b-spec.el= "y z"', ['a-spec.el::"x"', 'b-spec.el::"y z"']),
    ('—', []),
])
def test_row_tests(cell, want):
    assert cov.row_tests(cell) == want


def test_adopt_rows_writes_properties(tree, capsys):
    design = tree / "design.org"
    design.write_text("* Scenario coverage\n\n| scenario | kind | test | note |\n|-+-+-+-|\n"
                      "| ws § Nobody | acceptance | =ws-spec.el::\"answers by a row\"= | |\n"
                      "| ws § Passing one | acceptance | =ws-spec.el::\"passes\"= | already cited |\n"
                      "| ws § Missing title | none | — | gone |\n\n* Next\n")
    assert cov.adopt_rows(str(design), str(tree), str(tree / "docs/specs"), write=True) == 0
    out = capsys.readouterr().out
    assert "PROBLEM ws § Missing title: 0 scenarios match" in out and "1 properties, written" in out
    text = (tree / "docs/specs/ws.org").read_text()
    assert '*** Scenario: Nobody\n:PROPERTIES:\n:TEST: ws-spec.el::"answers by a row"\n:END:\n' in text
    rows, _, bad = cov.compute(str(tree), str(tree / "docs/specs"))
    assert rows_by_title(rows)["Nobody"]["status"] == "cited" and bad == []


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q"]))
