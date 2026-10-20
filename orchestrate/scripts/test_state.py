"""pytest suite for state.py (stdlib + pytest). Run: python3 test_state.py  (from this directory)."""

import glob
import importlib.util
import json
import os
import pathlib
import shutil
import subprocess
import sys

import pytest

HERE = pathlib.Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location("state", HERE / "state.py")
state = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(state)

REAL_FILES = [
    "/Users/jefffarr/src/reddit_scraper/.orchestrator/cycles/cycle-1790187401/state.json",
    "/Users/jefffarr/emacs-workspaces/emacs-workspace-bookmarks/.orchestrator/cycles/cycle-1790453714/state.json",
]
FIX = HERE / "fixtures" / "asks"
# A complete decision ask as templates/ask.md requires one: about, observed, two to four options.
ASK = {"kind": "decision", "raised_by": {"role": "pm", "ref": None}, "question": "Which way?",
       "about": "A thing the user sees.", "observed": {"happened": True, "evidence": "seen in a run"},
       "options": [{"label": "Do it", "consequence": "it is done"},
                   {"label": "Do nothing", "consequence": "stays as is"}]}


def _parse(text):
    text = text.strip()
    if not text:
        return None
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return None


CH = ".orchestrator/changes/chg"  # the test change's directory, relative to the home


def open_change(home, name="chg", *extra):
    """Open (adopt) a change on the home itself, silently; returns the parsed result."""
    import contextlib, io
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        code = state.main(["--home", str(home), "change", "open", name, *extra])
    return code, _parse(out.getvalue()), _parse(err.getvalue())


@pytest.fixture
def repo(tmp_path):
    (tmp_path / ".claude" / "orchestrator").mkdir(parents=True)
    (tmp_path / ".claude" / "orchestrator" / "config.yaml").write_text("overlay-version: 1\nproject: scratch\n")
    (tmp_path / ".orchestrator").mkdir()
    code, out, err = open_change(tmp_path, "chg", "--worktree", str(tmp_path))
    assert code == 0, err
    return tmp_path


@pytest.fixture
def run(repo, capsys):
    def _run(*argv):
        try:
            code = state.main(["--repo", str(repo), *argv])
        except SystemExit as e:  # argparse errors
            code = e.code
        cap = capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)
    return _run


def read_state(repo):
    return json.loads((repo / ".orchestrator" / "changes" / "chg" / "state.json").read_text())


def init(run, cycle="cycle-1", *extra):
    return run("init", "--change", "chg", "--test-command", "make test", "--cycle", cycle, *extra)


def add_task(run, name, *fields, cites=None):
    argv = ["task", "add", name, "--file", f"tasks/{name}.md", "--class", "feature"]
    if cites:
        argv += ["--cites", cites]
    return run(*argv, *fields)


def pass_plan(run):
    add_task(run, "plan-task", cites="seam/a")
    run("gate", "set", "plan", "batch_composed=true")
    code, out, err = run("gate", "pass", "plan")
    assert code == 0, err


def handshake_fixture(diff_key="design_diff", diff=None):
    """A complete handshake; `diff_key` names the field carrying the diff."""
    h = {"cycle_id": "cycle-0", "pm_digest_path": "x.md", "meta_discoveries": [],
         "user_resolved_goal_drift": [], "asks_for_user_open": [], "asks_for_user_resolved": [],
         "task_refinements": []}
    if diff_key:
        h[diff_key] = [] if diff is None else diff
    return h


# 1
def test_init(run, repo):
    code, out, err = init(run, "cycle-1")
    assert code == 1 and err["error"] == "no_prior_handshake"
    code, out, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0 and out["ok"] and out["cycle_id"] == "cycle-1"
    assert state.validate(read_state(repo)) == []
    code, out, err = init(run, "cycle-2", "--first-cycle")
    assert code == 1 and err["error"] == "cycle_open"


# 2
def test_task_lifecycle(run, repo):
    init(run, "cycle-1", "--first-cycle")
    assert add_task(run, "t1")[0] == 0
    code, out, _ = run("task", "set", "t1", "in_progress")
    assert code == 0 and out["changed"] == ["status"]
    code, out, _ = run("task", "set", "t1", "needs_review", "merge_commit=abc123")
    assert code == 0 and out["changed"] == ["merge_commit", "status"]
    assert run("task", "set", "t1", "reviewed")[0] == 0
    assert run("task", "set", "t1", "done")[0] == 0
    t = read_state(repo)["tasks"][0]
    assert t["status"] == "done" and t["merge_commit"] == "abc123"
    assert all(t[k] and t[k].endswith("Z") for k in ("started_at", "completed_at", "reviewed_at"))


# 3
def test_needs_review_requires_merge_commit(run, repo):
    init(run, "cycle-1", "--first-cycle")
    add_task(run, "t1")
    run("task", "set", "t1", "in_progress")
    before = (repo / ".orchestrator" / "changes" / "chg" / "state.json").read_bytes()
    code, out, err = run("task", "set", "t1", "needs_review")
    assert code == 1 and err["error"] == "merge_commit_required"
    assert (repo / ".orchestrator" / "changes" / "chg" / "state.json").read_bytes() == before


# 4
def test_bad_transition_and_force(run, repo):
    init(run, "cycle-1", "--first-cycle")
    add_task(run, "t1")
    run("task", "set", "t1", "done")
    code, _, err = run("task", "set", "t1", "in_progress")
    assert code == 1 and err["error"] == "bad_transition"
    code, _, err = run("task", "set", "t1", "in_progress", "--force")
    assert code == 1 and err["error"] == "why_required"
    code, out, _ = run("task", "set", "t1", "in_progress", "--force", "--why", "review reopened")
    assert code == 0 and out["status"] == "in_progress"
    j = read_state(repo)["journal"]
    assert len(j) == 1 and j[0]["kind"] == "deviation" and "review reopened" in j[0]["text"]


# 5
def test_rejections_counted(run):
    init(run, "cycle-1", "--first-cycle")
    add_task(run, "t1")
    run("task", "set", "t1", "in_progress")
    run("task", "set", "t1", "needs_review", "merge_commit=abc")
    assert run("task", "set", "t1", "in_progress")[0] == 0
    run("task", "set", "t1", "needs_review")
    assert run("task", "set", "t1", "in_progress")[0] == 0
    code, out, _ = run("counts")
    assert code == 0 and out["counts"]["rejected"] == 2


# 6
def test_unknown_field_and_bad_status(run):
    init(run, "cycle-1", "--first-cycle")
    add_task(run, "t1")
    code, _, err = run("task", "set", "t1", "merge_order=1")
    assert code == 1 and err["error"] == "unknown_field"
    code, _, err = run("task", "set", "t1", "implemented")
    assert code == 1 and err["error"] == "bad_status" and "implemented -> completed" in err["hint"]


# 7
def test_record_findings(run):
    init(run, "cycle-1", "--first-cycle")
    rec = {"trigger": "conformance", "kind": "issue", "severity": "low", "class": "duplication",
           "title": "dup", "seam": "seam/violation-info",
           "evidence": "foo.py:10 and bar.py:22 carry the same body"}
    code, out, _ = run("record", "add", "findings", json.dumps(rec))
    assert code == 0 and out["id"] == "arch-cycle-1-1"
    assert out["record"]["path"] == ".orchestrator/changes/chg/cycles/cycle-1/findings/arch-cycle-1-1.md"
    assert out["record"]["seam"] == "seam/violation-info"
    assert out["record"]["decoration"] is None
    code, _, err = run("record", "add", "findings", json.dumps({**rec, "finding_id": "arch-cycle-1-1"}))
    assert code == 1 and err["error"] == "duplicate_id"
    code, out, _ = run("record", "set", "findings", "arch-cycle-1-1", "decoration=hardening",
                       "resolution=hardening-list")
    assert code == 0 and sorted(out["changed"]) == ["decoration", "resolution"]
    code, _, err = run("record", "set", "findings", "arch-cycle-1-1", "decoration=advisory")
    assert code == 1
    code, _, err = run("record", "set", "findings", "arch-cycle-1-1", "notes=x")
    assert code == 1 and err["error"] == "unknown_field"


# 7a
def test_finding_requires_kind_and_evidence(run):
    init(run, "cycle-1", "--first-cycle")
    base = {"trigger": "conformance", "class": "duplication", "title": "dup"}
    code, _, err = run("record", "add", "findings", json.dumps({**base, "evidence": "a.py:1"}))
    assert code == 1 and err["error"] == "kind_required" and "thought" in err["message"]
    code, _, err = run("record", "add", "findings", json.dumps({**base, "kind": "thought", "evidence": "x"}))
    assert code == 1 and err["error"] == "kind_required"
    code, _, err = run("record", "add", "findings", json.dumps({**base, "kind": "issue", "evidence": " "}))
    assert code == 1 and err["error"] == "kind_required"
    q = {**base, "kind": "question", "evidence": "a remote path may reach file-exists-p; try a /ssh: pane"}
    code, out, _ = run("record", "add", "findings", json.dumps(q))
    assert code == 0 and out["record"]["kind"] == "question" and out["record"]["severity"] is None


# 7b
def test_finding_label_values(run):
    init(run, "cycle-1", "--first-cycle")
    base = {"trigger": "premise-check", "kind": "issue", "class": "duplication", "title": "dup",
            "evidence": "a.py:1"}
    code, _, err = run("record", "add", "findings", json.dumps({**base, "severity": "advisory"}))
    assert code == 1 and any("severity" in e for e in err["errors"])
    code, _, err = run("record", "add", "findings", json.dumps({**base, "decoration": "noted"}))
    assert code == 1 and any("decoration" in e for e in err["errors"])
    code, _, err = run("record", "add", "findings", json.dumps({**base, "observed": {"happened": True}}))
    assert code == 1 and any("observed" in e for e in err["errors"])


# 7c
def test_load_fills_new_record_defaults(run, repo):
    init(run, "cycle-1", "--first-cycle")
    st = read_state(repo)
    st["architect_findings"].append({  # a record written before `decoration` and `severity` existed
        "finding_id": "arch-cycle-1-1", "trigger": "conformance", "kind": "issue", "class": None,
        "title": "old", "path": "x.md", "locations": [], "why_tests_missed": None, "evidence": "x.py:1",
        "discovered_from": None, "resolution": "pending", "blocking_merge_until_resolved": False})
    (repo / ".orchestrator" / "changes" / "chg" / "state.json").write_text(json.dumps(st))
    assert run("validate")[0] == 0
    code, out, _ = run("status")
    assert code == 0
    assert run("note", "note", "still fine")[0] == 0
    f = read_state(repo)["architect_findings"][0]
    assert f["decoration"] is None and f["severity"] is None


# 7d
def test_counts_and_gate_by_kind_and_decoration(run, repo):
    init(run, "cycle-1", "--first-cycle")
    base = {"trigger": "conformance", "severity": "medium", "class": "duplication", "title": "t"}
    run("record", "add", "findings", json.dumps({**base, "kind": "issue", "evidence": "a.py:1"}))
    run("record", "add", "findings", json.dumps({**base, "kind": "question", "evidence": "try b"}))
    code, out, _ = run("counts")
    assert code == 0 and out["findings"] == {"total": 2, "issue": 1, "question": 1, "blocking": 0,
                                             "hardening_list": 0, "unrouted": 2}
    assert run("record", "set", "findings", "arch-cycle-1-1", "decoration=blocking")[0] == 0
    assert run("record", "set", "findings", "arch-cycle-1-2", "decoration=hardening",
               "resolution=hardening-list")[0] == 0
    _, out, _ = run("counts")
    assert out["findings"]["hardening_list"] == 1 and out["findings"]["unrouted"] == 0
    (repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1").mkdir(parents=True, exist_ok=True)
    _, out, _ = run("gate", "check", "integrate")
    assert out["checks"]["blocking_findings_resolved"]["value"] is False
    assert run("record", "set", "findings", "arch-cycle-1-1", "resolution=followup-task-fix-a")[0] == 0
    _, out, _ = run("gate", "check", "integrate")
    assert out["checks"]["blocking_findings_resolved"]["value"] is True


# 7e
def test_migrate_1_4_findings(run, repo):
    init(run, "cycle-1", "--first-cycle")
    s = read_state(repo)
    s["schema_version"] = "1.4"
    old = {"trigger": "conformance", "class": "duplication", "title": "t", "path": "p.md", "locations": [],
           "why_tests_missed": None, "discovered_from": None, "seam": None,
           "blocking_merge_until_resolved": False}
    s["architect_findings"] = [
        {**old, "finding_id": "arch-1", "severity": "blocking", "resolution": "pending",
         "observed": {"happened": True, "evidence": "a.py:1"}},
        {**old, "finding_id": "arch-2", "severity": "advisory", "resolution": "noted",
         "observed": {"happened": False, "evidence": "reasoning only: a third consumer would diverge"}},
        {**old, "finding_id": "arch-3", "severity": "informational", "resolution": "pending", "observed": None},
    ]
    (repo / ".orchestrator" / "changes" / "chg" / "state.json").write_text(json.dumps(s))
    code, out, err = run("migrate")
    assert code == 0, err
    f = {x["finding_id"]: x for x in read_state(repo)["architect_findings"]}
    assert (f["arch-1"]["kind"], f["arch-1"]["decoration"], f["arch-1"]["evidence"]) == ("issue", "blocking", "a.py:1")
    assert (f["arch-2"]["kind"], f["arch-2"]["decoration"]) == ("question", None)
    assert f["arch-2"]["evidence"] == "a third consumer would diverge" and f["arch-2"]["resolution"] == "noted"
    assert (f["arch-3"]["kind"], f["arch-3"]["evidence"]) == ("issue", "t")
    assert all(x["severity"] is None and "observed" not in x for x in f.values())
    parked = json.loads((repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1" / "legacy-state-extras.json").read_text())
    ex = parked["record_extras"]["architect_findings"]
    assert ex["arch-1"]["_legacy_severity"] == "blocking" and ex["arch-2"]["observed"]["happened"] is False
    assert run("validate")[1]["ok"] is True
    _, out, _ = run("gate", "check", "integrate")
    assert "arch-1" in out["checks"]["blocking_findings_resolved"]["reason"]

# 8
def test_record_asks_and_caps(run, repo):
    init(run, "cycle-1", "--first-cycle")
    code, out, _ = run("record", "add", "asks", json.dumps(ASK))
    assert code == 0 and out["id"] == "ask-cycle-1-1" and out["path"] == ".orchestrator/changes/chg/inbox/ask-cycle-1-1.org"
    assert (repo / out["path"]).exists()
    assert read_state(repo)["asks_for_user"][0] == {"id": "ask-cycle-1-1", "kind": "decision", "status": "open",
                                                    "blocks": [], "path": out["path"]}
    code, out, _ = run("record", "add", "asks", json.dumps({**ASK, "about": "x" * 2000}))
    assert code == 0 and out["id"] == "ask-cycle-1-2"  # prose lives in the file and has no cap
    code, out, _ = run("record", "add", "asks", json.dumps({**ASK, "question": "q" * 600}))
    assert code == 0 and out["id"] == "ask-cycle-1-3"
    code, _, err = run("record", "add", "asks", json.dumps({**ASK, "question": "q" * 601}))
    assert code == 1 and err["error"] == "invalid_record" and "600-char cap" in err["errors"][0]
    code, _, err = run("record", "add", "asks", json.dumps({**ASK, "question": "two\nlines"}))
    assert code == 1 and "one non-empty line" in err["errors"][0]
    stub = {"kind": "decision", "raised_by": {"role": "pm"}, "question": "which?"}
    code, _, err = run("record", "add", "asks", json.dumps(stub))
    fields = {e.split(":")[0].rsplit(".", 1)[1] for e in err["errors"]}
    assert code == 1 and fields >= {"about", "observed", "options"}
    code, out, _ = run("record", "add", "asks", json.dumps({**stub, "kind": "process"}))
    assert code == 0 and out["id"] == "ask-cycle-1-4"  # a process note needs no options


# 9
def test_note(run, repo):
    init(run, "cycle-1", "--first-cycle")
    code, _, err = run("note", "note", "x" * 281)
    assert code == 1 and err["error"] == "text_too_long"
    code, out, _ = run("note", "note", "hello", "--by", "orchestrator")
    assert code == 0 and out["entry"]["at"].endswith("Z")
    j = read_state(repo)["journal"]
    assert len(j) == 1 and j[0]["text"] == "hello" and j[0]["at"] == out["entry"]["at"]


# 10
def test_validate_corrupted(run, repo):
    init(run, "cycle-1", "--first-cycle")
    s = read_state(repo)
    s["plan_note"] = "prose"
    (repo / ".orchestrator" / "changes" / "chg" / "state.json").write_text(json.dumps(s))
    code, out, _ = run("validate")
    assert code == 1 and out["ok"] is False
    assert any("plan_note" in e and "`note`" in e for e in out["errors"])
    code, _, err = run("note", "note", "x")
    assert code == 1 and err["error"] == "invalid_state"


# 11
def test_plan_gate(run, repo):
    init(run, "cycle-1", "--first-cycle")
    add_task(run, "t1")
    code, out, _ = run("gate", "check", "plan")
    assert code == 0 and out["checks"]["briefs_cite_seams"]["value"] is False
    assert set(out["checks"]) == set(state.GATE_CHECKS["plan"])
    code, _, err = run("gate", "set", "plan", "briefs_cite_seams=true")
    assert code == 1 and err["error"] == "computed_check"
    code, _, err = run("gate", "set", "plan", "scaffolding_generated_for_tiered_entries=true")
    assert code == 1 and err["error"] == "unknown_check"
    code, out, _ = run("gate", "set", "plan", "batch_composed=true")
    assert code == 0 and out["checks"]["batch_composed"] is True
    code, _, err = run("gate", "pass", "plan")
    assert code == 1 and err["error"] == "gates_failed" and "briefs_cite_seams" in err["failing"]
    assert "t1" in err["failing"]["briefs_cite_seams"]
    assert run("task", "set", "t1", 'cites_seams=["seam/a"]')[0] == 0
    code, out, _ = run("gate", "pass", "plan")
    assert code == 0 and out["passed"] is True
    g = read_state(repo)["phase_gates"]["plan"]
    assert g["passed"] is True and g["passed_at"].endswith("Z")


# 11a
def test_briefs_cite_seams_externalised_and_no_tasks(run, repo):
    init(run, "cycle-1", "--first-cycle")
    _, out, _ = run("gate", "check", "plan")
    c = out["checks"]["briefs_cite_seams"]
    assert c["value"] is False and c["reason"] == "no tasks yet"
    add_task(run, "t1", cites="seam/a,seam/b")
    add_task(run, "t2")  # no citation, then externalised: exempt
    run("task", "set", "t2", "externalised")
    _, out, _ = run("gate", "check", "plan")
    assert out["checks"]["briefs_cite_seams"]["value"] is True
    assert read_state(repo)["tasks"][0]["cites_seams"] == ["seam/a", "seam/b"]
    add_task(run, "t3")
    _, out, _ = run("gate", "check", "plan")
    c = out["checks"]["briefs_cite_seams"]
    assert c["value"] is False and "t3" in c["reason"] and "t2" not in c["reason"]
    code, _, err = run("task", "set", "t3", 'cites_register_entries=["e1"]')
    assert code == 1 and err["error"] == "unknown_field"
    code, _, err = run("task", "set", "t3", "reconciled_into=x")
    assert code == 1 and err["error"] == "unknown_field"


# 12
def test_phase_set(run):
    init(run, "cycle-1", "--first-cycle")
    code, _, err = run("phase", "set", "integrate")
    assert code == 1 and err["error"] == "phase_order"
    code, _, err = run("phase", "set", "execute")
    assert code == 1 and err["error"] == "gate_not_passed"
    pass_plan(run)
    code, out, _ = run("phase", "set", "execute")
    assert code == 0 and out["phase"] == "execute"


# 13
def test_execute_gate(run, repo):
    init(run, "cycle-1", "--first-cycle")
    for n in ("a", "b", "c"):
        add_task(run, n)
    run("task", "set", "a", "done")
    run("task", "set", "b", "blocked", "blocker_note=waiting")
    run("task", "set", "c", "in_progress")
    _, out, _ = run("gate", "check", "execute")
    v = {k: r["value"] for k, r in out["checks"].items()}
    assert v == {"all_tasks_executed_or_stopped": False, "all_reviews_completed": True,
                 "no_orphan_in_progress": False}
    run("task", "set", "c", "done")
    _, out, _ = run("gate", "check", "execute")
    assert out["all_true"] is True
    add_task(run, "d")
    run("task", "set", "d", "reviewed", "review_mode=author-blind", "findings_path=reviews/d.md")
    _, out, _ = run("gate", "check", "execute")
    assert out["checks"]["all_reviews_completed"]["value"] is False
    assert "d" in out["checks"]["all_reviews_completed"]["reason"]
    (repo / "reviews").mkdir()
    (repo / "reviews" / "d.md").write_text("# findings\n")
    _, out, _ = run("gate", "check", "execute")
    assert out["checks"]["all_reviews_completed"]["value"] is True


# 14
def test_counts(run, repo):
    init(run, "cycle-1", "--first-cycle")
    for n in ("a", "b", "c", "d", "e"):
        add_task(run, n)
    run("task", "set", "a", "done")
    run("task", "set", "b", "in_progress")
    run("task", "set", "d", "externalised")
    run("task", "set", "e", "blocked")
    code, out, _ = run("counts")
    assert code == 0
    assert out["counts"] == {"created": 5, "started": 2, "completed": 1, "reviewed": 1, "rejected": 0,
                             "externalised": 1, "blocked": 1, "failed": 0, "done": 1}
    assert out["ratios"]["drainage"] == 0.2
    assert out["thresholds"]["drainage_trigger_ratio"] == 1.0
    (repo / ".claude" / "orchestrator" / "config.yaml").write_text(
        "overlay-version: 1\nproject: scratch\nthresholds:\n  drainage_trigger_ratio: 0.8\n  review_starvation_ratio: 2\nother: 1\n")
    code, out, _ = run("counts", "--write")
    assert code == 0 and out["thresholds"]["drainage_trigger_ratio"] == 0.8
    assert out["thresholds"]["review_starvation_ratio"] == 2
    written = repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1" / "pm-signals.json"
    assert written.exists() and json.loads(written.read_text())["counts"]["created"] == 5


# 15
def test_handshake(run, repo, tmp_path):
    init(run, "cycle-1", "--first-cycle")
    code, _, err = run("record", "add", "register-touched", "{}")
    assert code == 2 and err is None  # argparse: not a record list any more
    run("record", "add", "asks", json.dumps(ASK))  # ask-cycle-1-1 open
    drift = {**ASK, "options": [{"label": o, "consequence": o} for o in ("revise", "split", "abandon", "continue")],
             "status": "answered", "decision": "Continue", "decision_readback": "keep going"}
    run("record", "add", "asks", json.dumps(drift))  # ask-cycle-1-2 answered in the file
    code, out, _ = run("record", "add", "asks", json.dumps({**ASK, "status": "declined", "decision": "no"}))
    assert code == 0 and out["path"] == ".orchestrator/changes/chg/cycles/cycle-1/asks/ask-cycle-1-3.org"  # terminal
    ref = tmp_path / "refinements.json"
    ref.write_text(json.dumps([{"task": "t9", "refinement": "narrow scope"}]))
    code, out, _ = run("handshake", "--task-refinements", f"@{ref}")
    assert code == 0 and out["path"] == ".orchestrator/changes/chg/handshake-cycle-1.json"
    h = json.loads((repo / out["path"]).read_text())
    assert all(k in h for k in state.HANDSHAKE_REQUIRED)
    assert h["design_diff"] == [] and "register_diff" not in h
    assert out["design_diff"] == 0 and out["design_diff_source"] == "default"
    assert [a["id"] for a in h["asks_for_user_open"]] == ["ask-cycle-1-1"]
    assert set(h["asks_for_user_open"][0]) == set(state.ASK_FIELDS)
    r = h["asks_for_user_resolved"]
    assert [a["id"] for a in r] == ["ask-cycle-1-2"] and r[0]["decision"] == "Continue"
    assert r[0]["decision_readback"] == "keep going" and r[0]["path"].endswith("ask-cycle-1-2.org")
    assert h["user_resolved_goal_drift"] == [{"ask": "ask-cycle-1-2", "decision": "continue",
                                              "rationale": "keep going"}]  # the matched option label
    assert h["task_refinements"] == [{"task": "t9", "refinement": "narrow scope"}]


# 15a
def test_handshake_design_diff_from_file_and_flag(run, repo, tmp_path):
    init(run, "cycle-1", "--first-cycle")
    diff = [{"seam": "seam/jump-advice", "change": "amended", "ref": "arch-cycle-1-1"}]
    flag = tmp_path / "diff.json"
    flag.write_text(json.dumps([{"seam": "seam/other", "change": "added", "ref": ""}]))
    code, out, _ = run("handshake", "--design-diff", f"@{flag}")
    assert code == 0 and out["design_diff"] == 1 and out["design_diff_source"] == "--design-diff"
    h = json.loads((repo / out["path"]).read_text())
    assert h["design_diff"] == [{"seam": "seam/other", "change": "added", "ref": ""}]
    code, out, _ = run("handshake", "--design-diff", json.dumps([]))
    assert code == 0 and out["design_diff"] == 0 and out["design_diff_source"] == "--design-diff"
    cyc = repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1"
    cyc.mkdir(parents=True)
    (cyc / "design-diff.json").write_text(json.dumps(diff))
    code, out, _ = run("handshake", "--design-diff", f"@{flag}")  # the file wins over the flag
    assert code == 0 and out["design_diff"] == 1
    assert out["design_diff_source"] == ".orchestrator/changes/chg/cycles/cycle-1/design-diff.json"
    h = json.loads((repo / out["path"]).read_text())
    assert h["design_diff"] == diff
    _, out, _ = run("gate", "check", "integrate")
    assert out["checks"]["handshake_artifact_written"]["value"] is True


# 15b
def test_handshake_design_diff_shape_refused(run, repo, tmp_path):
    init(run, "cycle-1", "--first-cycle")
    bad = [
        [{"seam": "", "change": "added", "ref": "r"}],
        [{"seam": "seam/x", "change": "renamed", "ref": "r"}],
        [{"seam": "seam/x", "change": "added"}],
        [{"seam": "seam/x", "change": "added", "ref": "r", "from": "a"}],
        ["seam/x"],
        {"seam": "seam/x"},
    ]
    for d in bad:
        code, _, err = run("handshake", "--design-diff", json.dumps(d))
        assert code == 1 and err["error"] == "bad_design_diff", d
        assert not (repo / ".orchestrator" / "changes" / "chg" / "handshake-cycle-1.json").exists()
    cyc = repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1"
    cyc.mkdir(parents=True)
    (cyc / "design-diff.json").write_text(json.dumps(bad[0]))
    code, _, err = run("handshake")
    assert code == 1 and err["error"] == "bad_design_diff" and "design-diff.json" in err["message"]
    (cyc / "design-diff.json").write_text("{not json")
    code, _, err = run("handshake")
    assert code == 1 and err["error"] == "bad_design_diff"


# 15c
def test_init_accepts_prior_handshake_with_register_diff(run, repo):
    """A handshake written by the installed checkout before schema 1.2 carries `register_diff`."""
    hp = repo / ".orchestrator" / "changes" / "chg" / "handshake-cycle-0.json"
    hp.write_text(json.dumps(handshake_fixture("register_diff", [{"entry_id": "e1"}])))
    code, out, _ = init(run, "cycle-1", "--prior-handshake", ".orchestrator/changes/chg/handshake-cycle-0.json")
    assert code == 0 and out["cycle_id"] == "cycle-1"
    _, out, _ = run("gate", "check", "plan")
    assert out["checks"]["prior_integrate_consumed"]["value"] is True
    # The handshake this cycle writes is checked strictly: register_diff alone does not satisfy it.
    (repo / ".orchestrator" / "changes" / "chg" / "handshake-cycle-1.json").write_text(
        json.dumps(handshake_fixture("register_diff")))
    (repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1").mkdir(parents=True)
    _, out, _ = run("gate", "check", "integrate")
    c = out["checks"]["handshake_artifact_written"]
    assert c["value"] is False and "design_diff" in c["reason"]
    # A prior handshake with neither field is still refused.
    run("close", "--abandon", "--why", "test")
    (repo / ".orchestrator" / "changes" / "chg" / "handshake-cycle-1.json").write_text(json.dumps(handshake_fixture(None)))
    code, _, err = init(run, "cycle-2", "--prior-handshake", ".orchestrator/changes/chg/handshake-cycle-1.json")
    assert code == 1 and err["error"] == "bad_prior_handshake" and "design_diff" in err["message"]


# 16
def test_integrate_gate(run, repo):
    init(run, "cycle-1", "--first-cycle")
    run("record", "add", "asks", json.dumps(ASK))
    cyc = repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1"
    cyc.mkdir(parents=True)
    _, out, _ = run("gate", "check", "integrate")
    c = out["checks"]
    assert c["pm_digest_produced"]["value"] is False and "missing" in c["pm_digest_produced"]["reason"]
    assert c["handshake_artifact_written"]["value"] is False and c["user_asks_routed"]["value"] is False
    (cyc / "pm-digest.md").write_text("# Digest\n\n## Signals\n\n## Next\nstuff\n")
    _, out, _ = run("gate", "check", "integrate")
    assert out["checks"]["pm_digest_produced"]["value"] is False
    (cyc / "pm-digest.md").write_text("# Digest\n\n## Signals\n- none fired\n\n## Next\n")
    assert run("handshake")[0] == 0
    _, out, _ = run("gate", "check", "integrate")
    c = out["checks"]
    assert c["pm_digest_produced"]["value"] is True and c["handshake_artifact_written"]["value"] is True
    assert c["user_asks_routed"]["value"] is True  # the open ask is in the handshake and in the inbox
    p = repo / ".orchestrator" / "changes" / "chg" / "inbox" / "ask-cycle-1-1.org"
    p.write_text(p.read_text().replace("* Answer\n", "* Answer\n\nDo it\n"))  # the user answers in the file
    _, out, _ = run("gate", "check", "integrate")
    r = out["checks"]["user_asks_routed"]
    assert r["value"] is False and "ask-cycle-1-1" in r["reason"] and "answered in the file" in r["reason"]
    # A readback written and awaiting the user's yes does not hold the gate (the cycle can close with it open).
    assert run("record", "set", "asks", "ask-cycle-1-1", "decision_readback=It will be done.")[0] == 0
    _, out, _ = run("gate", "check", "integrate")
    assert out["checks"]["user_asks_routed"]["value"] is True
    _, st, _ = run("status")
    assert st["inbox"]["answered_in_file"] == 0 and st["inbox"]["awaiting_confirmation"] == 1
    assert "waiting" not in st["resume"]
    p.write_text(p.read_text().replace("It will be done.\n", "It will be done.\n\nyes\n"))
    _, st, _ = run("status")
    assert st["inbox"]["readback_confirmed"] == 1 and st["resume"].startswith("1 readback(s) confirmed")
    assert run("record", "set", "asks", "ask-cycle-1-1", "status=answered")[0] == 0
    _, out, _ = run("gate", "check", "integrate")
    assert out["checks"]["user_asks_routed"]["value"] is True


# 17
def test_close_and_next_cycle(run, repo):
    init(run, "cycle-1", "--first-cycle")
    add_task(run, "t1")
    run("task", "set", "t1", "done")
    assert run("handshake")[0] == 0
    code, _, err = run("close")
    assert code == 1 and err["error"] == "gate_not_passed"
    code, out, _ = run("close", "--abandon", "--why", "scope changed")
    assert code == 0 and out["abandoned"] and out["counts"]["done"] == 1
    archived = json.loads((repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-1" / "state.json").read_text())
    assert archived["closed_at"] and archived["journal"][-1]["kind"] == "deviation"
    assert "scope changed" in archived["journal"][-1]["text"]
    code, out, _ = init(run, "cycle-2", "--prior-handshake", ".orchestrator/changes/chg/handshake-cycle-1.json")
    assert code == 0 and out["history_cycles"] == ["cycle-1"]
    s = read_state(repo)
    assert s["cycle_id"] == "cycle-2" and s["cycle_log"]["history"][0]["counts"]["done"] == 1
    _, out, _ = run("gate", "check", "plan")
    assert out["checks"]["prior_integrate_consumed"]["value"] is True


# 18
def test_atomic_write(run, repo):
    init(run, "cycle-1", "--first-cycle")
    run("note", "note", "x")
    d = repo / ".orchestrator" / "changes" / "chg"
    assert not (d / "state.json.tmp").exists() and (d / "state.lock").exists()


# 19
def test_concurrent_notes(run, repo):
    init(run, "cycle-1", "--first-cycle")
    cmd = [sys.executable, str(HERE / "state.py"), "--repo", str(repo), "note", "note", "x"]
    procs = [subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE) for _ in range(40)]
    results = [(p.wait(), p.stderr.read()) for p in procs]
    assert all(rc == 0 for rc, _ in results), [e for rc, e in results if rc]
    assert len(read_state(repo)["journal"]) == 40


# 20
def test_migrate_legacy(run, repo):
    legacy = {
        "schema_version": "1.0", "session_id": "s", "cycle_id": "cycle-9", "phase": "execute",
        "repo_root": str(repo), "change_name": "c", "baseline_snapshot": None, "baseline_status": None,
        "current_branch": "main", "test_command": "t", "history_window": 5, "prior_handshake_path": None,
        "plan_note": "free prose", "orchestrator_discoveries": ["x"],
        "tasks": [{"task_name": "a", "task_file": "f", "task_class": "feature", "status": "implemented",
                   "merge_order": 1}],
        "register_touched": [{"entry_id": "e1", "entry_tier": "shape", "load_bearing": True,
                              "status_at_plan": "speculated", "cited_by_tasks": ["a"],
                              "scaffolding_path": "tests/scaffold_e1.py"}],
        "architect_findings": [{"finding_id": "arch-1", "trigger": "on-touch", "severity": "advisory",
                                "class": "duplication", "title": "t", "resolution": "pending"}],
        "asks_for_user": [{"id": "ask-1", "title": "T", "source": "pm", "severity": "advisory",
                           "question": "q?", "status": "open"}],
        "cycle_log": {"started_at": "2026-01-01T00:00:00Z", "counts": {"created": 1}, "history": []},
        "phase_gates": {"plan": {"passed": True, "checks": {
            "prior_integrate_consumed": True, "batch_composed": True, "briefs_cite_register": True,
            "scaffolding_generated_for_tiered_entries": True, "user_signed_off_goal_drift": True}},
            "integrate": {"passed": False, "checks": {"all_touched_entries_dispositioned": False,
                                                      "blocking_findings_resolved": True}}},
    }
    (repo / ".orchestrator" / "changes" / "chg" / "state.json").write_text(json.dumps(legacy))
    code, out, _ = run("validate")
    assert code == 1 and out["migration"]["register_touched_parked"] == 1
    assert out["migration"]["task_fields_parked"] == {"a": ["merge_order"]}
    code, out, _ = run("migrate")
    assert code == 0 and out["ok"] and out["from"] == "1.0" and out["to"] == "1.5"
    assert out["rehomed_from"] is None and out["paths"] == {"home": str(repo), "change": "chg", "code_root": str(repo)}
    assert sorted(out["parked_top_level_keys"]) == ["orchestrator_discoveries", "phase_gates.integrate",
                                                    "phase_gates.plan", "plan_note", "register_touched"]
    assert out["register_touched_parked"] == 1 and sorted(out["records"]) == ["architect_findings", "asks_for_user"]
    parked = json.loads((repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-9" / "legacy-state-extras.json").read_text())
    assert parked["top_level"]["plan_note"] == "free prose" and parked["tasks"]["a"]["merge_order"] == 1
    assert parked["top_level"]["register_touched"] == legacy["register_touched"]
    assert parked["top_level"]["phase_gates.plan"] == {"_dropped_checks": {
        "briefs_cite_register": True, "scaffolding_generated_for_tiered_entries": True}}
    assert parked["top_level"]["phase_gates.integrate"] == {"_dropped_checks": {
        "all_touched_entries_dispositioned": False}}
    assert parked["cycle_log"] == {"counts": {"created": 1}} and parked["asks_for_user"] == []
    assert parked["record_extras"]["asks_for_user"]["ask-1"]["_legacy_kind"] is None
    assert glob.glob(str(repo / ".orchestrator" / "changes" / "chg" / "state.json.pre-migrate-*"))
    s = read_state(repo)
    assert "register_touched" not in s and "repo_root" not in s
    assert s["home"] == str(repo) and s["code_root"] == str(repo)
    assert s["tasks"][0]["status"] == "completed"
    ask = s["asks_for_user"][0]
    assert ask == {"id": "ask-1", "kind": "decision", "status": "open", "blocks": [],
                   "path": ".orchestrator/changes/chg/inbox/ask-1.org"}
    assert out["ask_files"] == {"written": [ask["path"]], "existing": []}
    assert out["records"]["asks_for_user"] == {"kept": 1, "parked": 0, "with_extras": 1}
    text = (repo / ask["path"]).read_text()
    assert ":RAISED_BY: pm" in text and "#+TITLE: q?" in text and "* Answer" in text and "* Readback" not in text
    assert s["architect_findings"][0]["path"].endswith("findings/arch-1.md")
    g = s["phase_gates"]
    assert g["plan"] == {"passed": True, "checks": {"prior_integrate_consumed": True, "batch_composed": True,
                                                    "briefs_cite_seams": False, "user_signed_off_goal_drift": True}}
    assert set(g["integrate"]["checks"]) == set(state.GATE_CHECKS["integrate"])
    assert g["integrate"]["checks"]["blocking_findings_resolved"] is True
    assert run("status")[0] == 0
    code, out, _ = run("validate")
    assert code == 0 and out["ok"] is True


def legacy_state(repo, **over):
    s = {"schema_version": "1.1", "session_id": "s", "cycle_id": "cycle-9", "phase": "execute",
         "repo_root": str(repo), "change_name": "c", "baseline_snapshot": None, "baseline_status": None,
         "current_branch": "main", "test_command": "t", "history_window": 5, "prior_handshake": None,
         "closed_at": None, "tasks": [], "register_touched": [], "architect_findings": [],
         "asks_for_user": [], "journal": [],
         "cycle_log": {"started_at": "2026-01-01T00:00:00Z", "phase_started_at": {}, "history": []},
         "phase_gates": {}}
    s.update(over)
    (repo / ".orchestrator" / "changes" / "chg" / "state.json").write_text(json.dumps(s))
    return s


# 20a
def test_migrate_renames_citations(run, repo):
    task = {**state.TASK_DEFAULTS, "task_name": "a", "task_file": "f", "task_class": "feature",
            "status": "done", "cites_register_entries": ["register/shape/x", "register/invariant/y"],
            "reconciled_into": None}
    del task["cites_seams"]
    task_b = {**task, "task_name": "b", "reconciled_into": "register/shape/x-v2"}
    task_c = {**task, "task_name": "c", "cites_seams": ["seam/kept"]}  # both names: the old one parks
    legacy_state(repo, tasks=[task, task_b, task_c])
    code, out, _ = run("validate")
    assert code == 1 and out["migration"]["task_fields_parked"] == {"b": ["reconciled_into"],
                                                                    "c": ["cites_register_entries"]}
    code, out, err = run("migrate")
    assert code == 0, err
    assert out["tasks_with_parked_fields"] == ["b", "c"] and out["register_touched_parked"] == 0
    s = read_state(repo)
    by = {t["task_name"]: t for t in s["tasks"]}
    assert by["a"]["cites_seams"] == ["register/shape/x", "register/invariant/y"]
    assert by["b"]["cites_seams"] == ["register/shape/x", "register/invariant/y"]
    assert by["c"]["cites_seams"] == ["seam/kept"]
    assert all("cites_register_entries" not in t and "reconciled_into" not in t for t in s["tasks"])
    parked = json.loads((repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-9" / "legacy-state-extras.json").read_text())
    assert parked["tasks"] == {"b": {"reconciled_into": "register/shape/x-v2"},
                               "c": {"cites_register_entries": ["register/shape/x", "register/invariant/y"]}}
    _, out, _ = run("gate", "check", "plan")
    assert out["checks"]["briefs_cite_seams"]["value"] is True
    assert run("validate")[1]["ok"] is True


# 20b
def test_migrate_maps_legacy_triggers(run, repo):
    def finding(i, trigger):
        return {"finding_id": f"arch-{i}", "trigger": trigger, "severity": "informational", "class": "duplication",
                "title": "t", "path": "p.md", "locations": [], "why_tests_missed": None,
                "discovered_from": None, "observed": None, "resolution": "pending",
                "blocking_merge_until_resolved": False}
    legacy_state(repo, architect_findings=[finding(1, "on-touch"), finding(2, "end-of-cycle"),
                                           finding(3, "between-cycle"), finding(4, "forward-mode"),
                                           finding(5, "conformance"), finding(6, "bogus")])
    code, out, err = run("migrate")
    assert code == 0, err
    assert out["records"]["architect_findings"] == {"kept": 6, "parked": 0, "with_extras": 6}
    s = read_state(repo)
    assert [f["trigger"] for f in s["architect_findings"]] == [
        "conformance", "conformance", "conformance", "premise-check", "conformance", "conformance"]
    parked = json.loads((repo / ".orchestrator" / "changes" / "chg" / "cycles" / "cycle-9" / "legacy-state-extras.json").read_text())
    ex = parked["record_extras"]["architect_findings"]
    assert {k: v["_legacy_trigger"] for k, v in ex.items() if "_legacy_trigger" in v} == {
        "arch-1": "on-touch", "arch-2": "end-of-cycle", "arch-3": "between-cycle",
        "arch-4": "forward-mode", "arch-6": "bogus"}
    assert run("validate")[1]["ok"] is True


# 21
@pytest.mark.parametrize("src", REAL_FILES)
def test_replay_real_data(run, repo, capsys, src):
    """Project A's archived cycle-4 (index 0) and Project B's archived cycle-2 (index 1), both schema 1.0."""
    if not os.path.exists(src):
        pytest.skip(f"{src} absent")
    shutil.copy(src, repo / ".orchestrator" / "changes" / "chg" / "state.json")
    original = json.loads(pathlib.Path(src).read_text())
    # The prior handshake, written by the installed checkout, carries register_diff.
    orch = pathlib.Path(src).parent
    while orch.name != ".orchestrator":
        orch = orch.parent
    prior = original.get("prior_handshake") or original.get("prior_handshake_path")
    assert prior and (orch.parent / prior).exists(), prior
    shutil.copy(orch.parent / prior, repo / prior)
    assert "register_diff" in json.loads((repo / prior).read_text())
    code, out, _ = run("validate")
    assert code == 1 and out["ok"] is False and out["migration"]["to"] == state.SCHEMA_VERSION
    n_reg = len(original["register_touched"])
    assert out["migration"]["register_touched_parked"] == n_reg
    code, out, err = run("migrate")
    assert code == 0, err
    parked_keys = out["parked_top_level_keys"]
    assert "register_touched" in parked_keys and out["register_touched_parked"] == n_reg
    parked = json.loads((repo / out["parked"]).read_text())
    assert parked["top_level"]["register_touched"] == original["register_touched"]
    assert out["rehomed_from"] == original["repo_root"] == parked["top_level"]["repo_root"]
    assert read_state(repo)["home"] == str(repo)
    with_scaffold = [r["entry_id"] for r in parked["top_level"]["register_touched"] if r.get("scaffolding_path")]
    is_a = "reddit_scraper" in src
    assert (n_reg, len(with_scaffold)) == ((7, 3) if is_a else (15, 0))
    # Every legacy ask becomes a file; "resolved" ones are applied history under their cycle's asks/.
    n_asks = len(original.get("asks_for_user") or [])
    assert len(out["ask_files"]["written"]) == n_asks == out["records"]["asks_for_user"]["kept"]
    assert all("/asks/" in p for p in out["ask_files"]["written"])
    assert not glob.glob(str(repo / ".orchestrator" / "changes" / "chg" / "inbox" / "*.org"))
    if n_asks:
        text = (repo / out["ask_files"]["written"][0]).read_text()
        assert ":STATUS: applied" in text and ":APPLIED_VIA:" in text and "* Answer" in text
    code, res, _ = run("validate")
    assert code == 0 and res["file_problems"] == []
    for verb in (("status",), ("counts",), ("gate", "check", read_state(repo)["phase"]),
                 ("gate", "check", "plan")):
        code, res, err = run(*verb)
        assert code == 0, (verb, err)
    assert res["checks"]["prior_integrate_consumed"]["value"] is True
    assert res["checks"]["briefs_cite_seams"]["value"] is True  # cited ids renamed, externalised exempt
    s = read_state(repo)
    assert "register_touched" not in s
    statuses = {t["task_name"]: t["status"] for t in s["tasks"]}
    assert set(statuses) == {t["task_name"] for t in original["tasks"]}
    assert {t["task_name"]: t["cites_seams"] for t in s["tasks"]} == \
        {t["task_name"]: t["cites_register_entries"] for t in original["tasks"]}
    assert not any("reconciled_into" in e for e in parked["tasks"].values())
    assert out["records"]["architect_findings"]["kept"] == len(original["architect_findings"])
    assert out["records"]["architect_findings"]["parked"] == 0
    assert out["records"]["asks_for_user"] == {"kept": len(original.get("asks_for_user", [])), "parked": 0,
                                               "with_extras": len(original.get("asks_for_user", []))}
    assert all(f["trigger"] in state.TRIGGERS for f in s["architect_findings"])
    legacy_triggers = {v["_legacy_trigger"] for v in parked["record_extras"]["architect_findings"].values()
                       if "_legacy_trigger" in v}
    assert legacy_triggers == {f["trigger"] for f in original["architect_findings"]}
    assert run("validate")[1]["ok"] is True
    with capsys.disabled():
        print(f"\n[replay] {src}\n  parked top-level keys: {len(parked_keys)} {parked_keys}"
              f"\n  register_touched parked: {out['register_touched_parked']} (with scaffold: {with_scaffold})"
              f"\n  task statuses: {statuses}\n  records: {out['records']}")


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-q", *sys.argv[1:]]))


# 22
def test_ask_org_roundtrip_matches_golden():
    rec = json.loads((FIX / "download-video.json").read_text())
    text = (FIX / "download-video.org").read_text()
    assert state.ask_to_org(rec) == text
    assert state.ask_from_org(state.AskDoc(text)) == rec


# 23
def test_record_add_asks_from_org_draft_keeps_prose(run, repo):
    init(run, "cycle-1", "--first-cycle")
    draft = repo / "draft.org"
    text = (FIX / "download-video.org").read_text()
    text = text.replace(":ID: ask-cycle-1790026644-9\n", "").replace("* About\n", "* About\n\nMy own words.\n")
    draft.write_text(text)
    code, out, err = run("record", "add", "asks", f"@{draft}")
    assert code == 0, err
    assert out["id"] == "ask-cycle-1-1" and out["path"] == ".orchestrator/changes/chg/inbox/ask-cycle-1-1.org"
    body = (repo / out["path"]).read_text()
    assert ":ID: ask-cycle-1-1" in body and "My own words." in body and body.count("* Answer") == 1
    assert read_state(repo)["asks_for_user"][0]["kind"] == "decision"


# 24
def test_record_add_asks_terminal_lands_in_cycle_dir_and_seq_skips_orphans(run, repo):
    init(run, "cycle-1", "--first-cycle")
    doc = {"kind": "doc-correction", "raised_by": {"role": "orchestrator", "ref": None}, "question": "Fix D4?",
           "status": "applied", "decision": "Applied without asking: stage numbers corrected.",
           "applied_via": "design.md D4"}
    code, out, _ = run("record", "add", "asks", json.dumps(doc))
    assert code == 0 and out["path"] == ".orchestrator/changes/chg/cycles/cycle-1/asks/ask-cycle-1-1.org"
    assert (repo / out["path"]).exists() and not (repo / ".orchestrator" / "changes" / "chg" / "inbox").exists()
    (repo / ".orchestrator" / "changes" / "chg" / "inbox").mkdir()
    (repo / ".orchestrator" / "changes" / "chg" / "inbox" / "ask-cycle-1-2.org").write_text("stray\n")
    code, out, _ = run("record", "add", "asks", json.dumps(ASK))
    assert code == 0 and out["id"] == "ask-cycle-1-3"  # an orphan file is never overwritten
    code, _, err = run("record", "add", "asks", json.dumps({**ASK, "id": "ask-cycle-1-2"}))
    assert code == 1 and err["error"] == "file_exists"


# 25
def test_record_set_asks_updates_drawer_index_and_moves(run, repo):
    init(run, "cycle-1", "--first-cycle")
    run("record", "add", "asks", json.dumps(ASK))
    code, _, err = run("record", "set", "asks", "ask-cycle-1-1", "status=answered")
    assert code == 1 and err["error"] == "answer_required"
    code, out, _ = run("record", "set", "asks", "ask-cycle-1-1", "decision=Do it", 'blocks=["t1"]')
    assert code == 0 and out["status"] == "open" and out["changed"] == ["blocks", "decision"]
    text = (repo / out["path"]).read_text()
    assert ":BLOCKS: t1" in text and "\nDo it\n" in text and "# An option label" not in text
    code, _, err = run("record", "set", "asks", "ask-cycle-1-1", "status=applied")
    assert code == 1 and err["error"] == "applied_via_required"
    code, out, _ = run("record", "set", "asks", "ask-cycle-1-1", "status=applied", "applied_via=commit abc")
    assert code == 0 and out["moved_via"] == "rename"
    assert out["path"] == ".orchestrator/changes/chg/cycles/cycle-1/asks/ask-cycle-1-1.org"
    assert not (repo / ".orchestrator" / "changes" / "chg" / "inbox" / "ask-cycle-1-1.org").exists()
    idx = read_state(repo)["asks_for_user"][0]
    assert idx["status"] == "applied" and idx["path"] == out["path"] and idx["blocks"] == ["t1"]
    assert ":APPLIED_VIA: commit abc" in (repo / out["path"]).read_text()
    code, _, err = run("record", "set", "asks", "ask-cycle-1-1", "path=x")
    assert code == 1 and err["error"] == "not_settable"


# 26
def test_record_set_asks_git_mv_when_tracked(run, repo):
    if shutil.which("git") is None:
        pytest.skip("no git")
    init(run, "cycle-1", "--first-cycle")
    run("record", "add", "asks", json.dumps(ASK))

    def g(*a):
        return subprocess.run(["git", "-C", str(repo), "-c", "user.email=t@t", "-c", "user.name=t", *a],
                              capture_output=True, text=True)
    g("init", "-q")
    g("add", ".")
    g("commit", "-qm", "x")
    assert ".orchestrator/state.lock" not in g("ls-files").stdout  # init's .gitignore keeps it out
    code, out, _ = run("record", "set", "asks", "ask-cycle-1-1", "decision=Do nothing", "status=declined")
    assert code == 0 and out["moved_via"] == "git mv"
    tracked = g("ls-files").stdout
    assert out["path"] in tracked and ".orchestrator/changes/chg/inbox/ask-cycle-1-1.org" not in tracked


# 27
def test_record_add_asks_revised_from_supersedes(run, repo):
    init(run, "cycle-1", "--first-cycle")
    run("record", "add", "asks", json.dumps(ASK))
    code, out, _ = run("record", "add", "asks", json.dumps({**ASK, "revised_from": "ask-cycle-1-1"}))
    assert code == 0 and out["id"] == "ask-cycle-1-2" and out["superseded"]["id"] == "ask-cycle-1-1"
    old = read_state(repo)["asks_for_user"][0]
    assert old["status"] == "superseded" and old["path"] == ".orchestrator/changes/chg/cycles/cycle-1/asks/ask-cycle-1-1.org"
    assert ":STATUS: superseded" in (repo / old["path"]).read_text()
    assert ":REVISED_FROM: ask-cycle-1-1" in (repo / out["path"]).read_text()
    code, _, err = run("record", "add", "asks", json.dumps({**ASK, "revised_from": "ask-cycle-1-7"}))
    assert code == 1 and err["error"] == "no_such_record"


# 28
def test_inbox_reports_answers_and_tolerates_edits(run, repo):
    init(run, "cycle-1", "--first-cycle")
    for _ in range(4):
        run("record", "add", "asks", json.dumps(ASK))
    inbox = repo / ".orchestrator" / "changes" / "chg" / "inbox"

    def answer(n, text):
        p = inbox / f"ask-cycle-1-{n}.org"
        p.write_text(p.read_text().replace("* Answer\n", f"* Answer\n\n{text}\n"))
    answer(1, "do it.")  # exact after normalisation
    answer(2, "Do nothin")  # fuzzy
    answer(3, "Half of it; see ask-cycle-1-4 for the rest")  # free text that names another ask
    # 4: lowercase title before the drawer, wrapped prose, a Discussion with a ** turn placed
    # before Answer, the answer written in the discussion, CRLF line ends
    p = inbox / "ask-cycle-1-4.org"
    t = p.read_text()
    t = t.replace("* About\n\nA thing the user sees.\n", "* About\n\nA thing\nthe user sees.\n")
    t = t.replace("* Answer\n\n# An option", "* Discussion\n\n** 2026-10-09 user\n\nDo it\n\n* Answer\n\n# An option")
    t = "#+title: Which way?\n" + t.replace("#+TITLE: Which way?\n", "")
    p.write_text(t.replace("\n", "\r\n"))
    code, out, _ = run("inbox")
    assert code == 0 and out["answered"] == ["ask-cycle-1-1", "ask-cycle-1-2", "ask-cycle-1-3"]
    by = {e["id"]: e for e in out["asks"]}
    assert by["ask-cycle-1-1"]["answer"] == {"text": "do it.", "label": "Do it", "match": "exact"}
    a2 = by["ask-cycle-1-2"]["answer"]
    assert a2["match"] == "fuzzy" and a2["label"] == "Do nothing"
    a3 = by["ask-cycle-1-3"]
    assert a3["answer"]["match"] == "none" and a3["mentions"] == ["ask-cycle-1-4"]
    e4 = by["ask-cycle-1-4"]
    assert e4["answer"] is None and e4["problems"] == [] and e4["question"] == "Which way?"
    assert e4["answer_candidate"] == {"section": "Discussion", "text": "Do it"}
    assert out["problems"] == [] and all(e["indexed"] and not e["tracked"] for e in out["asks"])
    _, st, _ = run("status")
    assert st["inbox"] == {"open": 4, "answered_in_file": 3, "awaiting_confirmation": 0, "readback_confirmed": 0,
                           "deferred": 0, "unindexed_files": 0, "problems": 0}
    assert st["resume"].startswith("3 answer(s) waiting in the inbox")
    assert [a["answered_in_file"] for a in st["open_asks"]] == [True, True, True, False]
    assert [a["question"] for a in st["open_asks"]] == [ASK["question"]] * 4  # SKILL.md: id, question, path
    # record set on the hand-edited file keeps every edit
    code, out, _ = run("record", "set", "asks", "ask-cycle-1-4", "decision=Do it", "status=answered")
    assert code == 0
    kept = (repo / out["path"]).read_text()
    assert "** 2026-10-09 user" in kept and "A thing\nthe user sees." in kept and "\r" not in kept


# 29
def test_validate_reports_index_file_mismatches_and_set_reconciles(run, repo):
    init(run, "cycle-1", "--first-cycle")
    run("record", "add", "asks", json.dumps(ASK))
    run("record", "add", "asks", json.dumps(ASK))
    inbox = repo / ".orchestrator" / "changes" / "chg" / "inbox"
    p1 = inbox / "ask-cycle-1-1.org"
    p1.write_text(p1.read_text().replace(":STATUS: open", ":STATUS: deferred").replace("* Answer\n", "* Answer\n\nlater\n"))
    (inbox / "ask-cycle-1-2.org").unlink()
    (inbox / "ask-cycle-1-9.org").write_text(
        (FIX / "download-video.org").read_text().replace("ask-cycle-1790026644-9", "ask-cycle-1-9"))
    code, out, _ = run("validate")
    assert code == 1 and out["errors"] == []
    probs = "\n".join(out["file_problems"])
    assert ":STATUS: deferred but the index says open" in probs
    assert "ask-cycle-1-2.org is missing" in probs and "ask-cycle-1-9.org: not in the index" in probs
    code, _, err = run("record", "set", "asks", "ask-cycle-1-2", "about=x")
    assert code == 1 and err["error"] == "ask_file_missing"
    code, out, _ = run("record", "set", "asks", "ask-cycle-1-1", "status=deferred")  # the file wins
    assert code == 0 and read_state(repo)["asks_for_user"][0]["status"] == "deferred"
    t = p1.read_text()
    p1.write_text(t[t.index("#+TITLE"):])  # the drawer deleted by hand
    code, out, _ = run("inbox")
    assert any("no properties drawer" in p for p in out["problems"])
    code, out, _ = run("record", "set", "asks", "ask-cycle-1-1", "about=Restored")
    assert code == 0
    t = p1.read_text()
    assert t.startswith(":PROPERTIES:\n:ID: ask-cycle-1-1\n") and ":STATUS: deferred" in t and "Restored" in t
    code, out, _ = run("record", "add", "asks", f"@{inbox / 'ask-cycle-1-9.org'}")  # index the stray file
    assert code == 0 and out["id"] == "ask-cycle-1-9"


# 30
def test_init_carries_inbox_writes_gitignore_refuses_when_ignored(run, repo):
    init(run, "cycle-1", "--first-cycle")
    gi = repo / ".orchestrator" / ".gitignore"
    assert gi.exists() and "after-*.txt" in gi.read_text() and "state.lock" in gi.read_text()
    run("record", "add", "asks", json.dumps(ASK))
    run("record", "add", "asks", json.dumps({**ASK, "status": "deferred", "decision": "later"}))
    code, out, _ = run("close", "--abandon", "--why", "test")
    assert code == 0 and out["inbox_left"] == {"open": 1, "answered": 0, "deferred": 1}
    code, out, err = init(run, "cycle-2", "--first-cycle")
    assert code == 0, err
    assert out["inbox_carried"] == ["ask-cycle-1-1", "ask-cycle-1-2"] and out["gitignore_written"] is False
    assert [a["status"] for a in read_state(repo)["asks_for_user"]] == ["open", "deferred"]
    code, out, _ = run("record", "set", "asks", "ask-cycle-1-1", "decision=Do it", "status=applied",
                       "applied_via=cycle-2 task")
    assert code == 0 and out["path"] == ".orchestrator/changes/chg/cycles/cycle-1/asks/ask-cycle-1-1.org"
    if shutil.which("git"):
        subprocess.run(["git", "-C", str(repo), "init", "-q"])
        (repo / ".gitignore").write_text(".orchestrator/\n")
        run("close", "--abandon", "--why", "test")
        code, _, err = init(run, "cycle-3", "--first-cycle")
        assert code == 1 and err["error"] == "orchestrator_ignored"


# 31
def test_migrate_1_2_asks_to_files(run, repo):
    src = json.loads((FIX / "state-1.2.json").read_text())
    src["repo_root"] = str(repo)
    (repo / ".orchestrator" / "changes" / "chg" / "state.json").write_text(json.dumps(src))
    code, out, err = run("migrate")
    assert code == 0, err
    assert out["from"] == "1.2" and out["ask_files"]["existing"] == []
    written = out["ask_files"]["written"]
    assert written[0] == ".orchestrator/changes/chg/inbox/ask-cycle-1790604160-1.org" and len(written) == 4
    assert all(w.startswith(".orchestrator/changes/chg/cycles/cycle-1790604160/asks/") for w in written[1:])
    s = read_state(repo)
    assert s["home"] == str(repo) and s["code_root"] == str(repo) and out["rehomed_from"] is None
    assert out["gitignore_written"] is True and (repo / ".orchestrator" / ".gitignore").exists()
    assert [set(a) for a in s["asks_for_user"]] == [set(state.ASK_FIELDS)] * 4
    text = (repo / written[1]).read_text()
    assert ":STATUS: applied" in text and ":APPLIED_VIA: " in text and "Applied without asking" in text
    code, out, _ = run("validate")
    assert code == 0 and out["file_problems"] == []
    code, out, _ = run("migrate")
    assert out["unchanged"] is True
    code, out, _ = run("inbox")
    assert [e["id"] for e in out["asks"]] == ["ask-cycle-1790604160-1"] and out["asks"][0]["kind"] == "process"


# ---------------------------------------------------------------- the home and the code root

def _git_ok():
    return shutil.which("git") is not None


def _g(path, *a):
    return subprocess.run(["git", "-C", str(path), "-c", "user.email=t@t", "-c", "user.name=t", *a],
                          capture_output=True, text=True)


def split_repo(repo, name="code", worktree=False, git=True):
    """Point the home's config.yaml at a nested code root, create it, and re-open the test change
    on it (the fixture opened it on the home)."""
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    cfg.write_text(cfg.read_text() + f"code-root: {name}\n")
    code = repo / name
    if worktree:
        origin = repo.parent / (repo.name + "-origin")
        origin.mkdir()
        _g(origin, "init", "-q")
        (origin / "f.txt").write_text("x\n")
        _g(origin, "add", ".")
        _g(origin, "commit", "-qm", "init")
        r = _g(origin, "worktree", "add", "-q", str(code), "-b", "feature-x")
        assert r.returncode == 0, r.stderr
        assert (code / ".git").is_file()
    else:
        code.mkdir()
        if git:
            _g(code, "init", "-q")
    (repo / ".orchestrator" / "changes" / "chg" / "change.json").unlink()
    rc, out, err = open_change(repo, "chg", "--worktree", str(code))
    assert rc == 0, err
    return code


# 32
def test_where_single_repo(run, repo):
    code, out, _ = run("where")
    assert code == 0 and out["state"] == "absent" and out["split"] is False and out["home_found"] is True
    assert out["home"] == out["code_root"] == str(repo) and out["problems"] == [] and out["config"]
    assert out["shape"] == "repo-root" and list(out["repos"].values())[0]["path"] == str(repo)
    assert out["change"] == "chg" and out["change_resolved_by"] == "only" and out["legacy_layout"] is False
    assert [c["name"] for c in out["changes"]] == ["chg"] and out["changes"][0]["worktree"] == str(repo)
    assert out["worktrees"] == {"parent": str(repo / ".worktrees"), "pattern": "<branch>"}
    init(run, "cycle-1", "--first-cycle")
    s = read_state(repo)
    assert s["home"] == s["code_root"] == str(repo) and "repo_root" not in s and s["change_name"] == "chg"
    code, out, _ = run("status")
    assert out["paths"] == {"home": str(repo), "change": "chg", "code_root": str(repo)}
    code, out, _ = run("where")
    assert out["state"] == "present"
    assert out["warnings"] == []
    (repo / "openspec" / "specs").mkdir(parents=True)  # Project A's layout: home and code root are one
    code, out, _ = run("where")
    assert len(out["warnings"]) == 1 and "openspec-convert" in out["warnings"][0]


# 33
def test_where_and_init_with_split_code_root(run, repo):
    if not _git_ok():
        pytest.skip("no git")
    code_dir = split_repo(repo)
    code, out, _ = run("where")
    assert code == 0 and out["split"] is True and out["code_root"] == str(code_dir)
    assert out["shape"] == "code-root" and out["repos"] == {"code": {"path": str(code_dir), "from_config": "code",
                                                                   "test_command": None}}
    assert out["problems"] == [] and out["repo"] == "code"
    code, out, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0, err
    assert out["home"] == str(repo) and out["code_root"] == str(code_dir) and out["warnings"] == []
    assert (repo / ".orchestrator" / "changes" / "chg" / "state.json").exists() and not (code_dir / ".orchestrator").exists()
    s = read_state(repo)
    assert s["home"] == str(repo) and s["code_root"] == str(code_dir)


# 34
def test_init_refuses_no_home(tmp_path, capsys, monkeypatch):
    empty = tmp_path / "nowhere"
    empty.mkdir()
    monkeypatch.chdir(empty)
    code = state.main(["init", "--change", "c", "--test-command", "t", "--first-cycle"])
    err = _parse(capsys.readouterr().err)
    assert code == 1 and err["error"] == "no_home" and str(empty) in err["message"]
    assert not (empty / ".orchestrator").exists()
    code = state.main(["status"])
    err = _parse(capsys.readouterr().err)
    assert code == 2 and err["error"] == "not_initialized"
    code = state.main(["where"])
    out = _parse(capsys.readouterr().out)
    assert code == 0 and out["home_found"] is False and out["home"] == str(empty)


# 35
def test_home_flag_and_repo_alias(repo, capsys):
    for flag in ("--home", "--repo"):
        code = state.main([flag, str(repo), "where"])
        out = _parse(capsys.readouterr().out)
        assert code == 0 and out["home"] == str(repo)
    code = state.main(["--home", str(repo), "where", "--sh"])
    text = capsys.readouterr().out
    assert text.splitlines() == [f"export ORCH_HOME={repo}", f"export ORCH_WORKTREES={repo / '.worktrees'}",
                                 "export ORCH_CHANGE=chg", f"export ORCH_CHANGE_DIR={repo / CH}",
                                 f"export ORCH_REPO={repo.name}", f"export CODE_ROOT={repo}"]
    code = state.main(["--home", str(repo), "worktree", "task-a-1", "--sh"])
    assert capsys.readouterr().out.strip() == f"WORKTREE_PATH={repo / '.worktrees' / 'task-a-1'}"


# 36
def test_read_code_root_parsing(repo):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    base = "overlay-version: 1\nproject: scratch\nworktree:\n  code-root: nested-not-this\n"
    cases = {
        "code-root: sub\n": ("sub", str(repo / "sub")),
        "code-root: 'quoted dir'  # comment\n": ("quoted dir", str(repo / "quoted dir")),
        'code-root: "/abs/elsewhere"\n': ("/abs/elsewhere", "/abs/elsewhere"),
        "code-root:\n": (None, None),
        "code-root: .\n": (".", str(repo)),
        "": (None, None),
    }
    for text, want in cases.items():
        cfg.write_text(base + text)
        assert state.read_code_root(str(repo)) == want, text
    assert state.Store(str(repo), str(repo)).split is False
    cfg.write_text(base + "code-root: ~/x\n")
    assert state.read_code_root(str(repo))[1] == os.path.expanduser("~/x")
    for text in ("code-root: [a, b]\n", "code-root:\n  - a\n  - b\n"):
        cfg.write_text(base + text)
        with pytest.raises(state.Refusal) as e:
            state.read_code_root(str(repo))
        assert e.value.slug == "bad_code_root"


# 37
def test_init_refuses_code_root_missing_and_not_git(run, repo):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    cfg.write_text("overlay-version: 1\nproject: scratch\ncode-root: gone\n")
    code, _, err = init(run, "cycle-1", "--first-cycle")
    assert code == 1 and err["error"] == "code_root_missing"
    code, out, _ = run("where")
    assert code == 0 and [p["error"] for p in out["problems"]] == ["code_root_missing"]
    (repo / "gone").mkdir()
    if _git_ok():
        code, _, err = init(run, "cycle-1", "--first-cycle")
        assert code == 1 and err["error"] == "code_root_not_git"
    assert not (repo / ".orchestrator" / "changes" / "chg" / "state.json").exists()


# 38
def test_init_refuses_misplaced_artifacts_then_two_homes(run, repo):
    if not _git_ok():
        pytest.skip("no git")
    code_dir = split_repo(repo)
    (code_dir / ".orchestrator").mkdir()
    (code_dir / ".claude" / "orchestrator").mkdir(parents=True)
    (code_dir / ".claude" / "orchestrator" / "config.yaml").write_text("name: old\n")
    code, _, err = init(run, "cycle-1", "--first-cycle")
    assert code == 1 and err["error"] == "misplaced_artifacts" and str(repo) in err["message"]
    assert [p["error"] for p in err["problems"]] == ["misplaced_artifacts", "two_homes"]
    (code_dir / ".orchestrator").rmdir()
    code, _, err = init(run, "cycle-1", "--first-cycle")
    assert code == 1 and err["error"] == "two_homes"
    (code_dir / ".claude" / "orchestrator" / "config.yaml").unlink()
    (code_dir / "openspec" / "specs").mkdir(parents=True)  # a record in the code root is not the warning
    (repo / "openspec" / "specs").mkdir(parents=True)
    code, out, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0, err
    assert len(out["warnings"]) == 1 and "openspec/specs/ is still in the home" in out["warnings"][0]
    assert "openspec-convert" in out["warnings"][0]


# 39
def test_init_branch_from_linked_worktree(run, repo):
    if not _git_ok():
        pytest.skip("no git")
    code_dir = split_repo(repo, worktree=True)
    code, out, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0, err
    assert read_state(repo)["current_branch"] == "feature-x"
    run("close", "--abandon", "--why", "t")
    init(run, "cycle-2", "--first-cycle", "--branch", "given")
    assert read_state(repo)["current_branch"] == "given"
    assert state._git_branch(str(repo / "no-such-dir")) == "main"
    _g(code_dir, "checkout", "-q", "--detach")
    assert len(state._git_branch(str(code_dir))) == 12


# 40
def test_orchestrator_ignored_is_checked_on_home_not_code_root(run, repo):
    if not _git_ok():
        pytest.skip("no git")
    code_dir = split_repo(repo)
    (code_dir / ".gitignore").write_text("# tasks-orchestrator ephemeral state\n.orchestrator/\n")
    code, _, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0, err
    run("close", "--abandon", "--why", "t")
    _g(repo, "init", "-q")
    (repo / ".gitignore").write_text(".orchestrator/\n")
    code, _, err = init(run, "cycle-2", "--first-cycle")
    assert code == 1 and err["error"] == "orchestrator_ignored"
    (repo / ".gitignore").write_text("")
    code, out, _ = run("where")
    assert code == 0 and len(out["warnings"]) == 1 and "does not ignore the repository code code/" in out["warnings"][0]
    (repo / ".gitignore").write_text("code/\n")
    assert run("where")[1]["warnings"] == []


# 41
def test_migrate_1_3_rehomes(run, repo):
    init(run, "cycle-1", "--first-cycle")
    add_task(run, "a")
    s = read_state(repo)
    s["schema_version"] = "1.3"
    s["repo_root"] = "/old/place"
    del s["home"], s["code_root"]
    (repo / ".orchestrator" / "changes" / "chg" / "state.json").write_text(json.dumps(s))
    (repo / ".orchestrator" / ".gitignore").unlink()
    code, out, _ = run("status")
    assert code == 0 and out["legacy"] is True
    code, out, _ = run("validate")
    assert code == 1 and out["migration"]["to"] == "1.5" and out["migration"]["top_level_parked"] == []
    assert out["migration"]["rehome"] == {"from": "/old/place", "to": str(repo)}
    code, out, err = run("migrate")
    assert code == 0, err
    assert out["rehomed_from"] == "/old/place" and out["parked_top_level_keys"] == ["repo_root"]
    assert out["gitignore_written"] is True
    s2 = read_state(repo)
    assert s2["home"] == str(repo) and s2["code_root"] == str(repo) and "repo_root" not in s2
    assert [t["task_name"] for t in s2["tasks"]] == ["a"]
    parked = json.loads((repo / out["parked"]).read_text())
    assert parked["top_level"] == {"repo_root": "/old/place"}
    code, out, _ = run("migrate")
    assert out["unchanged"] is True
    assert run("validate")[1]["ok"] is True


# 42
def test_at_file_falls_back_to_home(run, repo, tmp_path, monkeypatch):
    init(run, "cycle-1", "--first-cycle")
    draft = repo / "draft.org"
    draft.write_text(state.ask_to_org({**state.ASK_DEFAULTS, **state.ASK_FILE_DEFAULTS, **ASK,
                                       "id": "ask-cycle-1-9", "status": "open"}))
    elsewhere = tmp_path / "elsewhere"
    elsewhere.mkdir()
    monkeypatch.chdir(elsewhere)
    code, out, err = run("record", "add", "asks", "@draft.org")
    assert code == 0, err
    assert out["id"] == "ask-cycle-1-9"
    (repo / "refs.json").write_text(json.dumps([{"task": ".orchestrator/changes/c/tasks/open/a.md",
                                                 "refinement": "x", "ref": "y"}]))
    (elsewhere / "refs.json").write_text("[]")
    code, out, err = run("record", "add", "findings", "@nope.json")
    assert code == 1 and err["error"] == "no_such_file" and str(repo) in err["message"]
    assert state._arg_path("refs.json", str(repo)) == "refs.json"  # cwd wins when both exist
    (elsewhere / "refs.json").unlink()
    assert state._arg_path("refs.json", str(repo)) == str(repo / "refs.json")


# 43
def test_red_baseline_without_compare_warns_at_set_and_next_init(run, repo):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    for text, want in {"test:\n  command: ./t\n  compare: ./cmp.py  # ids\n": "./cmp.py",
                       "test:\n  command: ./t\n  # compare: ./cmp.py\n": None,
                       "test:\n  command: ./t\nother:\n  compare: ./no.py\n": None,
                       "name: x\n": None}.items():
        cfg.write_text(text)
        assert state.read_test_compare(str(repo)) == want, text
    init(run, "cycle-1", "--first-cycle")
    code, out, _ = run("set", "baseline_status=0")
    assert code == 0 and "warnings" not in out
    code, out, _ = run("set", "baseline_status=1")
    assert code == 0 and "test.compare" in out["warnings"][0]
    add_task(run, "t1")
    run("task", "set", "t1", "done")
    run("handshake")
    run("close", "--abandon", "--why", "test")
    code, out, _ = init(run, "cycle-2", "--prior-handshake", ".orchestrator/changes/chg/handshake-cycle-1.json")
    assert code == 0 and any("last cycle's baseline_status=1" in w for w in out["warnings"])
    cfg.write_text("test:\n  command: ./t\n  compare: ./cmp.py\n")
    code, out, _ = run("set", "baseline_status=1")
    assert code == 0 and "warnings" not in out


# 46
def test_parse_yaml_subset_and_resolve_repos(repo):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    cfg.write_text("""# a workspace home
project: ws
language: elisp
repos:
  emacs:
    path: ~/emacs
    build:
      pre-commit: ./bin/tangle.sh
    test:
      command: ./bin/run-tests.sh   # trailing comment
      pending-style: xit
  api:
    path: "repos/api"
    language: go
    test: {command: go test ./..., parser: exit-code}
test:
  parser: tap
  compare: python3 cmp.py
worktrees:
  parent: .worktrees
  pattern: <repo>/<branch>
architect:
  drift-hotspots:
    - module-system: "jf/resolve: contract"
    - tangle: watch it
taxonomy: [feature, test]
reviewer:
  checks:
    - cl-lib-vs-seq
asks:
  reader: "a PM"
""")
    cfg_d = state.read_config(str(repo))
    assert cfg_d["repos"]["emacs"]["test"] == {"command": "./bin/run-tests.sh", "pending-style": "xit"}
    assert cfg_d["repos"]["api"]["test"] == "{command: go test ./..., parser: exit-code}"  # flow maps stay strings
    assert cfg_d["architect"]["drift-hotspots"] == [{"module-system": "jf/resolve: contract"}, {"tangle": "watch it"}]
    assert cfg_d["taxonomy"] == ["feature", "test"] and cfg_d["reviewer"]["checks"] == ["cl-lib-vs-seq"]
    assert cfg_d["asks"]["reader"] == "a PM"
    hc = state.resolve_home_config(str(repo))
    assert hc["shape"] == "workspace" and sorted(hc["repos"]) == ["api", "emacs"]
    assert hc["repos"]["emacs"]["path"] == os.path.expanduser("~/emacs")
    assert hc["repos"]["emacs"]["test"] == {"parser": "tap", "compare": "python3 cmp.py",
                                            "command": "./bin/run-tests.sh", "pending-style": "xit"}
    assert hc["repos"]["api"]["path"] == str(repo / "repos" / "api") and hc["repos"]["api"]["language"] == "go"
    assert hc["worktrees"] == {"parent": str(repo / ".worktrees"), "from_config": ".worktrees", "pattern": "<repo>/<branch>"}
    assert state.worktree_path_for(hc, "api", "task-x-1") == str(repo / ".worktrees" / "api" / "task-x-1")
    assert hc["problems"] == []
    cfg.write_text("repos:\n  a:\n    path: a\n  b:\n    path: b\nworktrees:\n  pattern: <branch>\n")
    hc = state.resolve_home_config(str(repo))
    assert [p[0] for p in hc["problems"]] == ["bad_worktree_pattern"]
    cfg.write_text("repos:\n  a:\n    language: go\n")
    assert [p[0] for p in state.resolve_home_config(str(repo))["problems"]] == ["bad_repo"]
    cfg.write_text("repos: []\n")
    assert [p[0] for p in state.resolve_home_config(str(repo))["problems"]] == ["bad_repos"]


# 47
def test_change_open_creates_worktree_in_workspace(tmp_path, capsys):
    if not _git_ok():
        pytest.skip("no git")
    home = tmp_path / "ws"
    (home / ".claude" / "orchestrator").mkdir(parents=True)
    origin = tmp_path / "origin"
    origin.mkdir()
    _g(origin, "init", "-q", "-b", "main")
    (origin / "f.txt").write_text("x\n")
    _g(origin, "add", ".")
    _g(origin, "commit", "-qm", "init")
    _g(origin, "branch", "integration")
    (home / ".claude" / "orchestrator" / "config.yaml").write_text(
        f"project: ws\nrepos:\n  app:\n    path: {origin}\n    test:\n      command: make test\n"
        "worktrees:\n  parent: .worktrees\n  pattern: <repo>/<branch>\n")

    def run(*argv):
        code = state.main(["--home", str(home), *argv])
        cap = capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)

    code, out, _ = run("where")
    assert code == 0 and out["shape"] == "workspace" and out["changes"] == [] and out["change"] is None
    assert out["code_root"] is None and out["problems"] == []
    code, _, err = run("init", "--first-cycle")
    assert code == 1 and err["error"] == "no_change" and err["changes"] == []
    code, out, err = run("change", "open", "feat-a", "--from", "integration")
    assert code == 0, err
    wt = home / ".worktrees" / "app" / "feat-a"
    assert out["created"] is True and out["worktree"] == str(wt) and out["branch"] == "feat-a"
    assert out["repo"] == "app" and out["from"] == "integration" and len(out["base"]) == 40
    assert wt.is_dir() and (wt / ".git").is_file() and _g(wt, "branch", "--show-current").stdout.strip() == "feat-a"
    assert json.loads((home / ".orchestrator" / "changes" / "feat-a" / "change.json").read_text())["change"] == "feat-a"
    code, _, err = run("change", "open", "feat-a")
    assert code == 1 and err["error"] == "change_exists"
    code, out, err = run("change", "open", "feat-b")
    assert code == 0, err
    code, _, err = run("init", "--first-cycle")
    assert code == 1 and err["error"] == "no_change" and err["changes"] == ["feat-a", "feat-b"]
    code, out, err = run("--change", "feat-a", "init", "--first-cycle")
    assert code == 0, err
    assert out["change"] == "feat-a" and out["code_root"] == str(wt)
    s = json.loads((home / ".orchestrator" / "changes" / "feat-a" / "state.json").read_text())
    assert s["change_name"] == "feat-a" and s["test_command"] == "make test" and s["current_branch"] == "feat-a"
    assert (home / ".orchestrator" / ".gitignore").exists()
    code, out, err = run("--change", "feat-b", "init", "--first-cycle")
    assert code == 0, err  # two cycles open at once, one per change
    code, out, _ = run("where")
    assert [(c["name"], c["cycle"] is not None, c["phase"]) for c in out["changes"]] == \
        [("feat-a", True, "plan"), ("feat-b", True, "plan")]
    code, out, _ = run("--change", "feat-b", "worktree", "task-t-9")
    assert out["path"] == str(home / ".worktrees" / "app" / "task-t-9")
    code, _, err = run("--change", "feat-a", "init", "--change", "feat-b", "--first-cycle")
    assert code == 1 and err["error"] == "change_mismatch"
    os.environ["ORCH_CHANGE"] = "feat-b"
    try:
        code, out, _ = run("status")
        assert code == 0 and out["change_name"] == "feat-b"
    finally:
        del os.environ["ORCH_CHANGE"]
    cwd = os.getcwd()
    os.chdir(wt)
    try:
        code, out, _ = state.main(["where"]), None, None
        out = _parse(capsys.readouterr().out)
        assert out["home"] == str(home) and out["change"] == "feat-a" and out["change_resolved_by"] == "cwd"
    finally:
        os.chdir(cwd)
    code, out, err = run("change", "open", "feat-c", "--worktree", str(origin), "--from", "integration")
    assert code == 0 and out["adopted"] is True and out["branch"] == "main" and out["created"] is False
    assert out["from"] == "integration"
    assert [c["from"] for c in run("where")[1]["changes"]] == ["integration", "HEAD", "integration"]
    (origin / "sub").mkdir()
    code, _, err = run("change", "open", "feat-d", "--worktree", str(origin / "nope"))
    assert code == 1 and err["error"] == "worktree_missing"
    code, _, err = run("change", "open", "bad/name")
    assert code == 1 and err["error"] == "bad_change_name"
    code, _, err = run("change", "open", "feat-e", "--repo", "nope")
    assert code == 1 and err["error"] == "unknown_repo"


# 48
def test_migrate_moves_one_change_layout(repo, capsys):
    """A home laid out before changes/<change>/: state.json, cycles/, inbox/, handshakes and
    test outputs at .orchestrator/ itself, moved under the change the state names."""
    (repo / ".orchestrator" / "changes" / "chg" / "change.json").unlink()
    root = repo / ".orchestrator"
    old = {
        "schema_version": "1.5", "session_id": "s", "cycle_id": "cycle-2", "phase": "integrate",
        "home": str(repo), "code_root": str(repo), "change_name": "old-change",
        "baseline_snapshot": ".orchestrator/baseline-cycle-2.txt", "baseline_status": 0,
        "current_branch": "main", "test_command": "t", "history_window": 5,
        "prior_handshake": ".orchestrator/handshake-cycle-1.json", "closed_at": "2026-01-01T00:00:00Z",
        "tasks": [dict(state.TASK_DEFAULTS, task_name="a", task_file=".orchestrator/changes/old-change/tasks/open/a.md",
                       task_class="feature", status="done", after_snapshot=".orchestrator/after-a-1.txt",
                       implementor_report_path=".orchestrator/cycles/cycle-2/reports/a.md")],
        "architect_findings": [], "asks_for_user": [dict(state.ASK_DEFAULTS, id="ask-cycle-2-1", kind="decision",
                                                         status="open", path=".orchestrator/inbox/ask-cycle-2-1.org")],
        "journal": [], "cycle_log": {"started_at": "2026-01-01T00:00:00Z",
                                     "phase_started_at": {"plan": None, "execute": None, "integrate": None},
                                     "history": []},
        "phase_gates": state.gates_fresh(),
    }
    (root / "state.json").write_text(json.dumps(old))
    (root / "cycles" / "cycle-1").mkdir(parents=True)
    (root / "cycles" / "cycle-1" / "state.json").write_text("{}")
    (root / "inbox").mkdir()
    (root / "inbox" / "ask-cycle-2-1.org").write_text("stub\n")
    (root / "handshake-cycle-1.json").write_text(json.dumps({"pm_digest_path": ".orchestrator/cycles/cycle-1/pm-digest.md"}))
    (root / "baseline-cycle-2.txt").write_text("")
    (root / ".gitignore").write_text("x\n")

    def run(*argv):
        code = state.main(["--home", str(repo), *argv])
        cap = capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)

    code, out, _ = run("where")
    assert code == 0 and out["legacy_layout"] is True and out["changes"] == [] and any("migrate" in w for w in out["warnings"])
    code, out, _ = run("status")
    assert code == 0 and out["legacy_layout"] is True and out["resume"] == "run `migrate`"
    code, _, err = run("init", "--first-cycle")
    assert code == 1 and err["error"] == "no_change"
    code, out, err = run("migrate")
    assert code == 0, err
    assert out["unchanged"] is False and out["layout"]["change"] == "old-change"
    assert sorted(out["layout"]["moved"]) == ["baseline-cycle-2.txt", "cycles", "handshake-cycle-1.json", "inbox", "state.json"]
    new_dir = root / "changes" / "old-change"
    assert not (root / "state.json").exists() and (root / ".gitignore").read_text() == "x\n"
    assert (new_dir / "cycles" / "cycle-1" / "state.json").exists() and (new_dir / "inbox" / "ask-cycle-2-1.org").exists()
    assert (new_dir / "baseline-cycle-2.txt").exists()
    s = json.loads((new_dir / "state.json").read_text())
    assert s["baseline_snapshot"] == ".orchestrator/changes/old-change/baseline-cycle-2.txt"
    assert s["prior_handshake"] == ".orchestrator/changes/old-change/handshake-cycle-1.json"
    assert s["tasks"][0]["task_file"] == ".orchestrator/changes/old-change/tasks/open/a.md"
    assert s["tasks"][0]["after_snapshot"] == ".orchestrator/changes/old-change/after-a-1.txt"
    assert s["asks_for_user"][0]["path"] == ".orchestrator/changes/old-change/inbox/ask-cycle-2-1.org"
    hs = json.loads((new_dir / "handshake-cycle-1.json").read_text())
    assert hs["pm_digest_path"] == ".orchestrator/changes/old-change/cycles/cycle-1/pm-digest.md"
    rec = json.loads((new_dir / "change.json").read_text())
    assert rec["change"] == "old-change" and rec["worktree"] == str(repo) and rec["branch"] == "main" and rec["adopted"]
    code, out, _ = run("where")
    assert out["legacy_layout"] is False and out["change"] == "old-change" and out["changes"][0]["closed"] is True
    code, out, _ = run("migrate")
    assert code == 0 and out["unchanged"] is True
    code, _, err = run("init", "--first-cycle", "--cycle", "cycle-3")
    assert code == 1 and err["error"] == "no_test_command"
    code, out, err = run("init", "--first-cycle", "--cycle", "cycle-3", "--test-command", "t")
    assert code == 0, err


# 49
def test_home_init_writes_config_for_each_shape(tmp_path, capsys):
    if not _git_ok():
        pytest.skip("no git")

    def run(home, *argv):
        code = state.main(["--home", str(home), *argv])
        cap = capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)

    # a workspace with two repositories
    ws = tmp_path / "ws"
    ws.mkdir()
    for name in ("app", "api"):
        r = ws / "checkouts" / name
        r.mkdir(parents=True)
        _g(r, "init", "-q")
    code, out, err = run(ws, "home", "init", "--repo", "app=checkouts/app", "--repo", "api=checkouts/api",
                         "--test-command", "make check", "--parser", "tap", "--language", "go")
    assert code == 0, err
    assert out["shape"] == "workspace" and sorted(out["repos"]) == ["api", "app"] and out["problems"] == []
    assert out["worktrees"]["pattern"] == "<repo>/<branch>" and out["add_to_home_gitignore"] == []
    text = (ws / ".claude" / "orchestrator" / "config.yaml").read_text()
    assert "overlay-version: 1" in text and "project: ws" in text and "  command: make check" in text
    assert "compare: python3 ~/.claude/skills/orchestrate/scripts/compare.py --parser tap" in text
    assert (ws / ".claude" / "orchestrator" / "roles").is_dir()
    code, out, _ = run(ws, "where")
    assert out["shape"] == "workspace" and out["warnings"] == [] and out["problems"] == []
    code, _, err = run(ws, "home", "init")
    assert code == 1 and err["error"] == "overlay_exists"
    code, out, err = run(ws, "change", "open", "c1", "--repo", "app")
    assert code == 1 and err["error"] == "git_failed"  # an empty repository has no HEAD to branch from
    r = ws / "checkouts" / "app"
    (r / "f").write_text("x")
    _g(r, "add", ".")
    _g(r, "commit", "-qm", "i")
    code, out, err = run(ws, "change", "open", "c1", "--repo", "app")
    assert code == 0, err
    assert out["worktree"] == str(ws / ".worktrees" / "app" / "c1")

    # a repository root as its own home, inside a git repo
    rr = tmp_path / "rr"
    rr.mkdir()
    _g(rr, "init", "-q")
    code, out, err = run(rr, "home", "init", "--test-command", "pytest")
    assert code == 0, err
    assert out["shape"] == "repo-root" and list(out["repos"].values()) == [str(rr)]
    assert out["worktrees"]["pattern"] == "<branch>" and out["add_to_home_gitignore"] == [".worktrees/"]
    text = (rr / ".claude" / "orchestrator" / "config.yaml").read_text()
    assert "\nrepos:" not in text and "\ncode-root:" not in text and "# compare:" in text
    (rr / ".worktrees").mkdir()
    assert run(rr, "where")[1]["warnings"] == [w for w in run(rr, "where")[1]["warnings"] if "worktrees folder" in w]

    # the one-repository shorthand
    cr = tmp_path / "cr"
    (cr / "emacs").mkdir(parents=True)
    _g(cr / "emacs", "init", "-q")
    _g(cr, "init", "-q")
    code, out, err = run(cr, "home", "init", "--code-root", "emacs", "--parser", "emacs-results-txt")
    assert code == 0, err
    assert out["shape"] == "code-root" and out["repos"] == {"emacs": str(cr / "emacs")}
    assert out["add_to_home_gitignore"] == ["emacs/", ".worktrees/"]

    # refusals
    nogit = tmp_path / "nogit"
    nogit.mkdir()
    code, _, err = run(nogit, "home", "init")
    assert code == 1 and err["error"] == "home_not_git"
    code, _, err = run(nogit, "home", "init", "--repo", "x=missing")
    assert code == 1 and err["error"] == "repo_missing"
    code, _, err = run(nogit, "home", "init", "--repo", "bad")
    assert code == 1 and err["error"] == "bad_repo_arg"


# 50
def test_where_warns_on_overlay_drift(repo, capsys):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    cfg.write_text("""project: p
language: elisp
code-root: .
test:
  command: ./t
  compare: python3 ../../.orchestrator/tools/compare-baseline.py
scaffolding:
  enabled: false
artifacts:
  register: interfaces.org
  specs-root: docs/specs
architect:
  interfaces-document: interfaces.org
  drift-hotspots:
    - a: b
roles-overlay:
  implementor: roles/implementor.md
worktree:
  parent: .worktrees
  init: ./x.sh
mystery: 1
thresholds:
  history-window: 5
  nope: 2
""")
    roles = repo / ".claude" / "orchestrator" / "roles"
    roles.mkdir()
    (roles / "architect.md").write_text("Check the interfaces register and interfaces.org for load-bearing entries.\n")
    (roles / "reviewer.md").write_text("Prefer seq.el.\n")
    code = state.main(["--home", str(repo), "where"])
    out = _parse(capsys.readouterr().out)
    w = "\n".join(out["warnings"])
    for needle in ("no overlay-version", "scaffolding:", "artifacts.register:", "architect.interfaces-document:",
                   "roles-overlay:", "worktree.parent:", "mystery:", "thresholds.nope:", "test.compare climbs",
                   "roles/architect.md names retired concepts (interfaces register, interfaces.org, load-bearing)"):
        assert needle in w, needle
    assert "reviewer.md" not in w and "drift-hotspots" not in w and "worktree.init" not in w
    cfg.write_text("overlay-version: 1\nproject: p\ntest:\n  command: ./t\n")
    (roles / "architect.md").write_text("fine\n")
    code = state.main(["--home", str(repo), "where"])
    assert _parse(capsys.readouterr().out)["warnings"] == []
    cfg.write_text("overlay-version: 7\nproject: p\n")
    code = state.main(["--home", str(repo), "where"])
    assert "overlay-version 7" in _parse(capsys.readouterr().out)["warnings"][0]


# 49
def test_change_open_base_is_merge_base_and_runs_init_hook(tmp_path, capsys):
    """An adopted branch that already carries its hand-off commit has the fork point as its base;
    an intent whose #+BASE: disagrees refuses; the overlay's worktree.init runs in a new worktree."""
    if not _git_ok():
        pytest.skip("no git")
    home = tmp_path / "ws"
    (home / ".claude" / "orchestrator").mkdir(parents=True)
    origin = tmp_path / "origin"
    origin.mkdir()
    _g(origin, "init", "-q", "-b", "main")
    (origin / "f.txt").write_text("x\n")
    _g(origin, "add", ".")
    _g(origin, "commit", "-qm", "init")
    _g(origin, "branch", "integration")
    fork = _g(origin, "rev-parse", "integration").stdout.strip()
    # the hand-off commit on the change branch, carrying the intent with the fork point as its base
    wt = tmp_path / "feat-x"
    _g(origin, "worktree", "add", "-q", "-b", "feat-x", str(wt), "integration")
    (wt / "docs" / "changes" / "feat-x").mkdir(parents=True)
    (wt / "docs" / "changes" / "feat-x" / "intent.org").write_text(f"#+TITLE: feat-x\n#+BASE: {fork}\n")
    _g(wt, "add", ".")
    _g(wt, "commit", "-qm", "Record: feat-x intent")
    head = _g(wt, "rev-parse", "HEAD").stdout.strip()
    assert head != fork
    (home / ".claude" / "orchestrator" / "config.yaml").write_text(
        f"project: ws\nrepos:\n  app:\n    path: {origin}\n    test:\n      command: make test\n"
        "worktree:\n  init: printf hook > init-ran.txt\n"
        "worktrees:\n  parent: .worktrees\n  pattern: <repo>/<branch>\n")

    def run(*argv):
        code = state.main(["--home", str(home), *argv])
        cap = capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)

    code, out, err = run("change", "open", "feat-x", "--worktree", str(wt), "--from", "integration")
    assert code == 0, err
    assert out["base"] == fork and out["adopted"] is True
    assert out["init_hook"]["rc"] == 0 and (wt / "init-ran.txt").read_text() == "hook"
    assert json.loads((home / ".orchestrator" / "changes" / "feat-x" / "change.json").read_text())["base"] == fork
    # the intent disagrees with the branch: refused, both values named
    wt2 = tmp_path / "feat-y"
    _g(origin, "worktree", "add", "-q", "-b", "feat-y", str(wt2), "integration")
    (wt2 / "docs" / "changes" / "feat-y").mkdir(parents=True)
    (wt2 / "docs" / "changes" / "feat-y" / "intent.org").write_text("#+TITLE: feat-y\n#+BASE: 0123456789ab\n")
    _g(wt2, "add", ".")
    _g(wt2, "commit", "-qm", "Record: feat-y intent")
    code, _, err = run("change", "open", "feat-y", "--worktree", str(wt2), "--from", "integration")
    assert code == 1 and err["error"] == "base_mismatch" and err["base"] == fork and err["intent_base"] == "0123456789ab"
    # a new worktree from --from: base is that ref's commit; --no-init skips the hook
    code, out, err = run("change", "open", "feat-z", "--from", "integration", "--no-init")
    assert code == 0, err
    assert out["base"] == fork and out["created"] is True and out["init_hook"] is None
    assert not (home / ".worktrees" / "app" / "feat-z" / "init-ran.txt").exists()


# 50
def test_task_add_stamps_created_at(run, repo):
    code, out, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0, err
    tf = repo / ".orchestrator" / "changes" / "chg" / "tasks" / "open"
    tf.mkdir(parents=True)
    (tf / "t1.md").write_text("---\ntask_name: t1\ncreated_at: 2026-01-01T00:00:00Z\n---\n\n## Summary\n")
    code, out, err = run("task", "add", "t1", "--file", str(tf / "t1.md"), "--class", "feature")
    assert code == 0, err
    stamp = out["task"]["created_at"]
    assert stamp and stamp != "2026-01-01T00:00:00Z" and stamp.endswith("Z")
    assert f"created_at: {stamp}\n" in (tf / "t1.md").read_text()
    # no file yet: the entry is still stamped
    code, out, err = run("task", "add", "t2", "--file", "tasks/t2.md", "--class", "feature")
    assert code == 0 and out["task"]["created_at"].endswith("Z")


# 51
def test_record_set_asks_readback_confirmed(run, repo):
    code, out, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0, err
    code, out, err = run("record", "add", "asks", json.dumps(ASK))
    assert code == 0, err
    aid = out["id"]
    p = repo / ".orchestrator" / "changes" / "chg" / "inbox" / f"{aid}.org"
    p.write_text(p.read_text().replace("* Answer\n", "* Answer\n\nSomething else entirely\n"))
    code, _, err = run("record", "set", "asks", aid, "readback=confirmed")
    assert code == 1 and err["error"] == "no_readback"
    assert run("record", "set", "asks", aid, "decision_readback=It will be done.")[0] == 0
    _, st, _ = run("status")
    assert st["inbox"]["awaiting_confirmation"] == 1
    code, out, err = run("record", "set", "asks", aid, "readback=confirmed")
    assert code == 0, err
    assert "readback" in out["changed"]
    assert "It will be done.\n\nyes\n" in p.read_text()
    _, st, _ = run("status")
    assert st["inbox"]["awaiting_confirmation"] == 0 and st["inbox"]["readback_confirmed"] == 1
    code, _, err = run("record", "set", "asks", aid, "readback=maybe")
    assert code == 1 and err["error"] == "bad_value"


# 52
def test_task_field_added_in_version_is_defaulted_on_read(run, repo):
    """A state file written before created_at joined TASK_FIELDS still validates and accepts writes."""
    code, out, err = init(run, "cycle-1", "--first-cycle")
    assert code == 0, err
    assert add_task(run, "old-task")[0] == 0
    sp = repo / ".orchestrator" / "changes" / "chg" / "state.json"
    s = json.loads(sp.read_text())
    for t in s["tasks"]:
        del t["created_at"]
    sp.write_text(json.dumps(s))
    code, out, err = run("validate")
    assert code == 0, err
    code, out, err = run("task", "set", "old-task", "on_critical_path=true")
    assert code == 0, err
    s = json.loads(sp.read_text())
    assert s["tasks"][0]["created_at"] is None and s["tasks"][0]["on_critical_path"] is True
