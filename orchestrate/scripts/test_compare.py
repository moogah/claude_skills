import compare
import pathlib

FIX = pathlib.Path(__file__).resolve().parent / "fixtures" / "emacs-results"

ERT_OK = "Running 3 tests\n   passed  1/3  a-test (0.01 sec)\n   passed  2/3  b-test (0.01 sec)\n   passed  3/3  c-test (0.01 sec)\nRan 3 tests, 3 results as expected, 0 unexpected\n"
ERT_RED = ("Running 3 tests\n   passed  1/3  a-test (0.01 sec)\n   FAILED  2/3  b-test (0.01 sec) at f.el\n"
           "   passed  3/3  c-test (0.01 sec)\nRan 3 tests, 2 results as expected, 1 unexpected\n"
           "1 unexpected results:\n   FAILED  b-test\n")
BC = "Running 2 specs.\n\nsuite\n  does x (1ms)\n  does y\nFAILED: boom\n\nRan 2 specs, 1 failed, in 0.1 seconds.\n"
BC_OK = "Running 2 specs.\n\nsuite\n  does x (1ms)\n  does y (1ms)\n\nRan 2 specs, 0 failed, in 0.1 seconds.\n"


def test_emacs_parser_names_unexpected_ert_and_buttercup():
    ids, ran, np = compare.parse_emacs(ERT_RED + BC)
    assert ids == {"b-test", "does y"} and ran == 5 and np == 1
    assert compare.parse_emacs("nothing here") is None
    assert compare.parse_emacs(ERT_OK)[0] == set()


def test_emacs_compare_regression_and_fixed():
    code, lines = compare.compare("emacs-results-txt", ERT_OK + BC_OK, ERT_RED + BC)
    assert code == 1 and "new failure: b-test" in lines and "new failure: does y" in lines and lines[-1] == "REGRESSION"
    code, lines = compare.compare("emacs-results-txt", ERT_RED + BC, ERT_RED + BC)
    assert code == 0 and lines[-1] == "NO-REGRESSION"
    code, lines = compare.compare("emacs-results-txt", ERT_RED + BC, ERT_OK + BC_OK)
    assert code == 0 and "fixed: 2" in lines[0]
    # an unnamed Buttercup failure (error type) shows only in the count
    unnamed = "Ran 2 specs, 2 failed, in 0.1 seconds.\nsuite\n  does y\nFAILED: boom\n"
    code, lines = compare.compare("emacs-results-txt", BC, unnamed)
    assert code == 1 and any("unnamed" in l for l in lines)


def test_hand_made_baseline_file():
    base = "## ERT\nb-test  known\n## Buttercup\ndoes y\nBUTTERCUP_NOT_PASSING=1\n"
    code, lines = compare.compare("emacs-results-txt", base, ERT_RED + BC)
    assert code == 0, lines


def test_junit_and_tap():
    j_ok = '<testsuite tests="2"><testcase classname="m" name="a"/><testcase classname="m" name="b"/></testsuite>'
    j_red = '<testsuite tests="2"><testcase classname="m" name="a"><failure/></testcase><testcase classname="m" name="b"/></testsuite>'
    assert compare.compare("junit-xml", j_ok, j_red)[0] == 1
    assert compare.compare("junit-xml", j_red, j_red)[0] == 0
    assert compare.compare("junit-xml", "<notxml", j_red)[0] == 2
    t_ok = "TAP version 13\n1..2\nok 1 - a\nok 2 - b\n"
    t_red = "1..2\nnot ok 1 - a\nok 2 - b # SKIP\n"
    t_todo = "1..2\nok 1 - a\nnot ok 2 - b # TODO later\n"
    assert compare.compare("tap", t_ok, t_red) == (1, compare.compare("tap", t_ok, t_red)[1])
    assert "new failure: a" in compare.compare("tap", t_ok, t_red)[1]
    assert compare.compare("tap", t_ok, t_todo)[0] == 0
    assert compare.compare("tap", "", t_red)[0] == 2


def test_exit_code_and_unknown_refused():
    assert compare.compare("exit-code", "", "")[0] == 2
    assert compare.compare("nope", "", "")[0] == 2


def test_cli(tmp_path, capsys):
    b, a = tmp_path / "b.txt", tmp_path / "a.txt"
    b.write_text(ERT_OK)
    a.write_text(ERT_RED)
    assert compare.main(["--parser", "emacs-results-txt", str(b), str(a)]) == 1
    assert "new failure: b-test" in capsys.readouterr().out
    assert compare.main(["--parser", "emacs-results-txt", str(b), str(tmp_path / "missing")]) == 2


def test_buttercup_traceback_failures_are_named():
    # Trimmed from package-boundaries cycle-1792083023: the baseline's preset spec and the after-file's
    # menu-spec source guard both fail with a traceback (no FAILED: line); one is fixed, one is new,
    # and the counts cancel (5 and 5). Before the fix: NO-REGRESSION.
    base = (FIX / "baseline.txt").read_text()
    after = (FIX / "after.txt").read_text()
    b_ids, _, b_np = compare.parse_emacs(base)
    a_ids, _, a_np = compare.parse_emacs(after)
    assert b_np == 5 and a_np == 5
    assert sum(1 for i in b_ids if "registers no palette/agent tools yet" in i) == 1
    assert sum(1 for i in a_ids if "wraps the install in with-eval-after-load 'evil (source guard)" in i) == 1
    code, lines = compare.compare("emacs-results-txt", base, after)
    assert code == 1 and lines[-1] == "REGRESSION"
    assert any(l.startswith("new failure: ") and "source guard" in l for l in lines)
    assert "fixed: 1" in lines[0]
    # a runner that prints the per-spec marker and no detail block still names the failure
    marker = "Running 1 specs.\n\nsuite\n  does z  FAILED (0.10ms)\n\nRan 1 specs, 1 failed, in 0.1 seconds.\n"
    assert compare.parse_emacs(marker)[0] == {"does z"}
    # the marker does not double-count a failure its detail block already names
    both = marker + "\n========================================\nsuite does z\nFAILED: boom\n"
    assert compare.parse_emacs(both)[0] == {"suite does z"}
