"""pytest suite for state.py (stdlib + pytest). Run: python3 -m pytest -q  (from this directory)."""

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
    "/Users/jefffarr/emacs-workspaces/emacs-workspace-bookmarks/emacs/.orchestrator/state.json",
]


def _parse(text):
    text = text.strip()
    if not text:
        return None
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return None


@pytest.fixture
def repo(tmp_path):
    (tmp_path / ".claude" / "orchestrator").mkdir(parents=True)
    (tmp_path / ".claude" / "orchestrator" / "config.yaml").write_text("name: scratch\n")
    (tmp_path / ".orchestrator").mkdir()
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
    return json.loads((repo / ".orchestrator" / "state.json").read_text())


def init(run, cycle="cycle-1", *extra):
    return run("init", "--change", "chg", "--test-command", "make test", "--cycle", cycle, *extra)


def add_task(run, name, *fields, cites=None):
    argv = ["task", "add", name, "--file", f"tasks/{name}.md", "--class", "feature"]
    if cites:
        argv += ["--cites", cites]
    return run(*argv, *fields)


def pass_plan(run):
    add_task(run, "plan-task", cites="e1")
    run("gate", "set", "plan", "batch_composed=true", "scaffolding_generated_for_tiered_entries=true")
    code, out, err = run("gate", "pass", "plan")
    assert code == 0, err


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
    before = (repo / ".orchestrator" / "state.json").read_bytes()
    code, out, err = run("task", "set", "t1", "needs_review")
    assert code == 1 and err["error"] == "merge_commit_required"
    assert (repo / ".orchestrator" / "state.json").read_bytes() == before


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
    rec = {"trigger": "on-touch", "severity": "advisory", "class": "duplication", "title": "dup",
           "observed": {"happened": True, "evidence": "foo.py:10 and bar.py:22 carry the same body"}}
    code, out, _ = run("record", "add", "findings", json.dumps(rec))
    assert code == 0 and out["id"] == "arch-cycle-1-1"
    assert out["record"]["path"] == ".orchestrator/cycles/cycle-1/findings/arch-cycle-1-1.md"
    code, _, err = run("record", "add", "findings", json.dumps({**rec, "finding_id": "arch-cycle-1-1"}))
    assert code == 1 and err["error"] == "duplicate_id"
    code, out, _ = run("record", "set", "findings", "arch-cycle-1-1", "resolution=fixed")
    assert code == 0 and out["changed"] == ["resolution"]
    code, _, err = run("record", "set", "findings", "arch-cycle-1-1", "notes=x")
    assert code == 1 and err["error"] == "unknown_field"


# 7a
def test_finding_requires_observed(run):
    init(run, "cycle-1", "--first-cycle")
    base = {"trigger": "end-of-cycle", "class": "duplication", "title": "dup"}
    code, _, err = run("record", "add", "findings", json.dumps({**base, "severity": "advisory"}))
    assert code == 1 and err["error"] == "observed_required" and "happened" in err["message"]
    code, _, err = run("record", "add", "findings", json.dumps({**base, "severity": "blocking"}))
    assert code == 1 and err["error"] == "observed_required"
    reasoning = {"happened": False, "evidence": "reasoning only: a third consumer would diverge"}
    code, out, _ = run("record", "add", "findings", json.dumps({**base, "severity": "advisory", "observed": reasoning}))
    assert code == 0 and out["record"]["observed"] == reasoning
    code, out, _ = run("record", "add", "findings", json.dumps({**base, "severity": "informational"}))
    assert code == 0 and out["record"]["observed"] is None


# 7b
def test_observed_shape(run):
    init(run, "cycle-1", "--first-cycle")
    base = {"trigger": "end-of-cycle", "severity": "advisory", "class": "duplication", "title": "dup"}
    code, _, err = run("record", "add", "findings", json.dumps({**base, "observed": {"happened": "yes"}}))
    assert code == 1 and err["error"] == "invalid_record"
    assert any("observed.happened" in e for e in err["errors"])
    assert any("observed.evidence" in e for e in err["errors"])
    code, _, err = run("record", "add", "findings",
                       json.dumps({**base, "observed": {"happened": True, "evidence": "x", "where": "y"}}))
    assert code == 1 and any("observed.where" in e for e in err["errors"])


# 7c
def test_load_fills_new_record_defaults(run, repo):
    init(run, "cycle-1", "--first-cycle")
    st = read_state(repo)
    st["architect_findings"].append({  # a 1.1 record written before `observed` existed
        "finding_id": "arch-cycle-1-1", "trigger": "on-touch", "severity": "advisory", "class": None,
        "title": "old", "path": "x.md", "locations": [], "why_tests_missed": None,
        "discovered_from": None, "resolution": "pending", "blocking_merge_until_resolved": False})
    (repo / ".orchestrator" / "state.json").write_text(json.dumps(st))
    assert run("validate")[0] == 0
    code, out, _ = run("status")
    assert code == 0
    assert run("note", "note", "still fine")[0] == 0
    assert read_state(repo)["architect_findings"][0]["observed"] is None


# 7d
def test_counts_noted_findings(run, repo):
    init(run, "cycle-1", "--first-cycle")
    base = {"trigger": "end-of-cycle", "severity": "advisory", "class": "duplication", "title": "t"}
    run("record", "add", "findings", json.dumps({**base, "observed": {"happened": True, "evidence": "a.py:1"}}))
    run("record", "add", "findings", json.dumps({**base, "observed": {"happened": False, "evidence": "reasoning only: later"}}))
    code, out, _ = run("counts")
    assert code == 0 and out["findings"] == {"total": 2, "reasoning_only": 1, "reasoning_only_noted": 0}
    assert run("record", "set", "findings", "arch-cycle-1-2", "resolution=noted")[0] == 0
    _, out, _ = run("counts")
    assert out["findings"]["reasoning_only_noted"] == 1
    (repo / ".orchestrator" / "cycles" / "cycle-1").mkdir(parents=True, exist_ok=True)
    _, out, _ = run("gate", "check", "integrate")
    assert out["checks"]["blocking_findings_resolved"]["value"] is True

# 8
def test_record_asks_and_caps(run):
    init(run, "cycle-1", "--first-cycle")
    ask = {"kind": "decision", "raised_by": {"role": "pm"}, "question": "which?"}
    code, out, _ = run("record", "add", "asks", json.dumps(ask))
    assert code == 0 and out["id"] == "ask-cycle-1-1"
    code, out, _ = run("record", "add", "asks", json.dumps(ask))
    assert code == 0 and out["id"] == "ask-cycle-1-2"
    code, _, err = run("record", "add", "asks", json.dumps({**ask, "about": "x" * 281}))
    assert code == 1 and err["error"] == "invalid_record" and "280-char cap" in err["errors"][0]
    code, out, _ = run("record", "add", "asks", json.dumps({**ask, "question": "q" * 600}))
    assert code == 0 and out["id"] == "ask-cycle-1-3"


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
    (repo / ".orchestrator" / "state.json").write_text(json.dumps(s))
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
    assert code == 0 and out["checks"]["briefs_cite_register"]["value"] is False
    code, _, err = run("gate", "set", "plan", "briefs_cite_register=true")
    assert code == 1 and err["error"] == "computed_check"
    code, out, _ = run("gate", "set", "plan", "batch_composed=true",
                       "scaffolding_generated_for_tiered_entries=true")
    assert code == 0 and out["checks"]["batch_composed"] is True
    code, _, err = run("gate", "pass", "plan")
    assert code == 1 and err["error"] == "gates_failed" and "briefs_cite_register" in err["failing"]
    assert run("task", "set", "t1", 'cites_register_entries=["e1"]')[0] == 0
    code, out, _ = run("gate", "pass", "plan")
    assert code == 0 and out["passed"] is True
    g = read_state(repo)["phase_gates"]["plan"]
    assert g["passed"] is True and g["passed_at"].endswith("Z")


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
        "name: scratch\nthresholds:\n  drainage_trigger_ratio: 0.8\n  review_starvation_ratio: 2\nother: 1\n")
    code, out, _ = run("counts", "--write")
    assert code == 0 and out["thresholds"]["drainage_trigger_ratio"] == 0.8
    assert out["thresholds"]["review_starvation_ratio"] == 2
    written = repo / ".orchestrator" / "cycles" / "cycle-1" / "pm-signals.json"
    assert written.exists() and json.loads(written.read_text())["counts"]["created"] == 5


# 15
def test_handshake(run, repo, tmp_path):
    init(run, "cycle-1", "--first-cycle")
    entry = {"entry_id": "e1", "entry_tier": "shape", "load_bearing": True,
             "status_at_plan": "speculated", "cited_by_tasks": ["t1"]}
    assert run("record", "add", "register-touched", json.dumps(entry))[0] == 0
    assert run("record", "set", "register-touched", "e1", "status_at_integrate=confirmed")[0] == 0
    base = {"kind": "decision", "raised_by": {"role": "pm"}, "question": "q?"}
    run("record", "add", "asks", json.dumps(base))  # ask-cycle-1-1 open
    drift = {**base, "options": [{"label": o} for o in ("revise", "split", "abandon", "continue")],
             "status": "answered", "decision": "continue", "decision_readback": "keep going"}
    run("record", "add", "asks", json.dumps(drift))  # ask-cycle-1-2 answered
    run("record", "add", "asks", json.dumps({**base, "status": "declined"}))  # ask-cycle-1-3
    ref = tmp_path / "refinements.json"
    ref.write_text(json.dumps([{"task": "t9", "refinement": "narrow scope"}]))
    code, out, _ = run("handshake", "--task-refinements", f"@{ref}")
    assert code == 0 and out["path"] == ".orchestrator/handshake-cycle-1.json"
    h = json.loads((repo / out["path"]).read_text())
    assert all(k in h for k in state.HANDSHAKE_REQUIRED)
    assert h["register_diff"] == [{"entry_id": "e1", "from": "speculated", "to": "confirmed", "note_path": None}]
    assert [a["id"] for a in h["asks_for_user_open"]] == ["ask-cycle-1-1"]
    assert [a["id"] for a in h["asks_for_user_resolved"]] == ["ask-cycle-1-2"]
    assert h["user_resolved_goal_drift"] == [{"ask": "ask-cycle-1-2", "decision": "continue",
                                              "rationale": "keep going"}]
    assert h["task_refinements"] == [{"task": "t9", "refinement": "narrow scope"}]


# 16
def test_integrate_gate(run, repo):
    init(run, "cycle-1", "--first-cycle")
    run("record", "add", "asks", json.dumps({"kind": "decision", "raised_by": {"role": "pm"}, "question": "q?"}))
    cyc = repo / ".orchestrator" / "cycles" / "cycle-1"
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
    assert c["user_asks_routed"]["value"] is False and "ask-cycle-1-1" in c["user_asks_routed"]["reason"]
    (cyc / "asks.md").write_text("## ask-cycle-1-1\nq?\n")
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
    archived = json.loads((repo / ".orchestrator" / "cycles" / "cycle-1" / "state.json").read_text())
    assert archived["closed_at"] and archived["journal"][-1]["kind"] == "deviation"
    assert "scope changed" in archived["journal"][-1]["text"]
    code, out, _ = init(run, "cycle-2", "--prior-handshake", ".orchestrator/handshake-cycle-1.json")
    assert code == 0 and out["history_cycles"] == ["cycle-1"]
    s = read_state(repo)
    assert s["cycle_id"] == "cycle-2" and s["cycle_log"]["history"][0]["counts"]["done"] == 1
    _, out, _ = run("gate", "check", "plan")
    assert out["checks"]["prior_integrate_consumed"]["value"] is True


# 18
def test_atomic_write(run, repo):
    init(run, "cycle-1", "--first-cycle")
    run("note", "note", "x")
    d = repo / ".orchestrator"
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
        "register_touched": [],
        "architect_findings": [{"finding_id": "arch-1", "trigger": "on-touch", "severity": "advisory",
                                "class": "duplication", "title": "t", "resolution": "pending"}],
        "asks_for_user": [{"id": "ask-1", "title": "T", "source": "pm", "severity": "advisory",
                           "question": "q?", "status": "open"}],
        "cycle_log": {"started_at": "2026-01-01T00:00:00Z", "counts": {"created": 1}, "history": []},
        "phase_gates": {},
    }
    (repo / ".orchestrator" / "state.json").write_text(json.dumps(legacy))
    code, out, _ = run("migrate")
    assert code == 0 and out["ok"] and out["from"] == "1.0"
    assert sorted(out["parked_top_level_keys"]) == ["orchestrator_discoveries", "plan_note"]
    parked = json.loads((repo / ".orchestrator" / "cycles" / "cycle-9" / "legacy-state-extras.json").read_text())
    assert parked["top_level"]["plan_note"] == "free prose" and parked["tasks"]["a"]["merge_order"] == 1
    assert parked["cycle_log"] == {"counts": {"created": 1}} and parked["asks_for_user"] == []
    assert parked["record_extras"]["asks_for_user"]["ask-1"]["_legacy_kind"] is None
    assert glob.glob(str(repo / ".orchestrator" / "state.json.pre-migrate-*"))
    s = read_state(repo)
    assert s["tasks"][0]["status"] == "completed"
    ask = s["asks_for_user"][0]
    assert ask["kind"] == "decision" and ask["raised_by"] == {"role": "pm", "ref": None}
    assert ask["status"] == "open" and out["records"]["asks_for_user"] == {"kept": 1, "parked": 0, "with_extras": 1}
    assert s["architect_findings"][0]["path"].endswith("findings/arch-1.md")
    assert run("status")[0] == 0
    code, out, _ = run("validate")
    assert code == 0 and out["ok"] is True


# 21
@pytest.mark.parametrize("src", REAL_FILES)
def test_replay_real_data(run, repo, capsys, src):
    if not os.path.exists(src):
        pytest.skip(f"{src} absent")
    shutil.copy(src, repo / ".orchestrator" / "state.json")
    original = json.loads(pathlib.Path(src).read_text())
    code, out, _ = run("validate")
    assert code == 1 and out["ok"] is False and out["migration"]["to"] == state.SCHEMA_VERSION
    code, out, err = run("migrate")
    assert code == 0, err
    parked_keys = out["parked_top_level_keys"]
    for verb in (("status",), ("counts",), ("gate", "check", read_state(repo)["phase"])):
        code, _, err = run(*verb)
        assert code == 0, (verb, err)
    s = read_state(repo)
    statuses = {t["task_name"]: t["status"] for t in s["tasks"]}
    assert set(statuses) == {t["task_name"] for t in original["tasks"]}
    with capsys.disabled():
        print(f"\n[replay] {src}\n  parked top-level keys: {len(parked_keys)} {parked_keys}"
              f"\n  task statuses: {statuses}\n  records: {out['records']}")
