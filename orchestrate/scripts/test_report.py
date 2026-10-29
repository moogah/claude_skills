"""pytest suite for report.py (stdlib + pytest). Run: python3 -m pytest test_report.py (from this directory)."""

import importlib.util
import json
import os
import pathlib
import subprocess

import pytest

HERE = pathlib.Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location("report", HERE / "report.py")
report = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(report)

CH = ".orchestrator/changes/chg"


def _parse(text):
    text = (text or "").strip()
    try:
        return json.loads(text) if text else None
    except json.JSONDecodeError:
        return None


def _g(cwd, *args):
    return subprocess.run(["git", "-C", str(cwd), "-c", "user.email=t@t", "-c", "user.name=t", *args],
                          capture_output=True, text=True, check=True).stdout.strip()


@pytest.fixture
def home(tmp_path, monkeypatch):
    """A git home with the change `chg` (worktree: the home) and cycle-1 started, whose batch is
    tasks a and b; the Claude config directory is under tmp_path."""
    h = tmp_path / "home"
    (h / ".claude" / "orchestrator").mkdir(parents=True)
    (h / ".claude" / "orchestrator" / "config.yaml").write_text("overlay-version: 1\nproject: t\n")
    _g(h, "init", "-q", "-b", "main")
    _g(h, "commit", "-q", "--allow-empty", "-m", "init")
    base = _g(h, "rev-parse", "HEAD")
    d = h / CH
    (d / "tasks" / "open").mkdir(parents=True)
    (d / "change.json").write_text(json.dumps({"change": "chg", "branch": "main", "worktree": str(h), "base": base}))
    (d / "events.jsonl").write_text(json.dumps(
        {"seq": 1, "at": "2026-10-28T00:00:00Z", "cycle": "cycle-1", "kind": "cycle-started"}) + "\n")
    briefs = d / "cycles" / "cycle-1" / "briefs"
    briefs.mkdir(parents=True)
    for name, extra in (("a", "agent:\n  implementor: sonnet, medium\n  reason: small\n"), ("b", "")):
        (d / "tasks" / "open" / f"{name}.md").write_text(
            f"---\ntask_name: {name}\nstatus: done\non_critical_path: true\n{extra}---\n")
        (briefs / f"{name}.md").write_text("brief\n")
    monkeypatch.setenv("CLAUDE_CONFIG_DIR", str(tmp_path / "cfg"))
    return h


def _transcript(home, agent_id, description, requests, session="sess-1", cwd=None, project=None):
    """A subagent transcript and its meta.json under the home's project directory: each request is
    (request id, model, effort, usage, timestamp); a request id repeated is one API call on two lines."""
    sub = pathlib.Path(os.environ["CLAUDE_CONFIG_DIR"]) / "projects" / (project or report._project_key(home)) / session / "subagents"
    sub.mkdir(parents=True, exist_ok=True)
    lines = [json.dumps({"type": "user", "timestamp": "2026-10-23T10:00:00Z", "cwd": str(cwd or home),
                         "message": {"content": "b"}})]
    for rid, model, effort, (i, cr, cc, o), ts in requests:
        lines.append(json.dumps({"type": "assistant", "requestId": rid, "timestamp": ts, "effort": effort,
                                 "message": {"model": model, "usage": {
                                     "input_tokens": i, "cache_read_input_tokens": cr,
                                     "cache_creation_input_tokens": cc, "output_tokens": o}}}))
    (sub / f"agent-{agent_id}.jsonl").write_text("\n".join(lines) + "\n")
    (sub / f"agent-{agent_id}.meta.json").write_text(json.dumps({"agentType": "general-purpose",
                                                                 "description": description}))


def run(home, capsys, *argv):
    code = report.main(["--home", str(home), "--change", "chg", *argv])
    cap = capsys.readouterr()
    return code, _parse(cap.out), _parse(cap.err)


def test_spend_takes_each_usage_field_at_its_largest_and_skips_synthetic_lines(home):
    _transcript(home, "s1", "implementor: a", [
        ("r1", "claude-opus-5-5", "medium", (10, 1000, 0, 90), "2026-10-23T10:00:00Z"),
        ("r1", "claude-opus-5-5", "medium", (10, 1000, 0, 165), "2026-10-23T10:00:01Z"),  # output grew
        ("r2", "<synthetic>", None, (0, 0, 0, 0), "2026-10-23T10:00:02Z")])               # an API error
    s = report.agent_spend("s1")
    assert (s["requests"], s["output"], s["ran"]) == (1, 165, {"opus/medium": 1})


def test_spend_by_role_reads_the_role_from_the_spawn_description(home, capsys):
    req = lambda model, effort: [("r1", model, effort, (100, 0, 0, 10), "2026-10-23T10:05:00Z")]
    _transcript(home, "i1", "implementor: a", req("claude-opus-5-5", "medium"))
    _transcript(home, "i2", "implementor: b", req("claude-sonnet-5-5", "low"))
    _transcript(home, "p1", "phase-plan: cycle-1", req("claude-opus-5-5", "high"))
    _transcript(home, "x1", "Explore the code", req("claude-haiku-5-5", None))       # names no role
    _transcript(home, "d1", "architect-design: chg", req("claude-opus-5-5", "high"))  # the design round
    _transcript(home, "o1", "implementor: other-change-task", req("claude-opus-5-5", "medium"))
    code, out, err = run(home, capsys, "spend")
    assert code == 0, err
    assert sorted(out["by_role"]) == ["architect-design", "implementor", "phase-plan"]
    impl = out["by_role"]["implementor"]
    assert impl["agents"] == 2 and impl["settings"] == {"opus/medium": 1, "sonnet/low": 1} and impl["overrides"] == 1
    assert out["unattributed_spawns"] == 1
    assert {(a["agent_id"], a["task"], a["cycle"]) for a in out["by_agent"]} == \
        {("i1", "a", "cycle-1"), ("i2", "b", "cycle-1"), ("p1", None, "cycle-1"), ("d1", None, None)}
    # a's file asks for sonnet/medium; its Implementor ran opus/medium
    assert out["override_mismatches"] == [{"agent_id": "i1", "override": "sonnet/medium", "ran": {"opus/medium": 1}}]
    assert [c["cycle"] for c in out["by_cycle"]] == ["cycle-1"]


def test_quality_reads_rework_briefs_findings_and_follow_ups(home, capsys):
    d = home / CH
    (d / "cycles" / "cycle-1" / "briefs" / "a-rework.md").write_text("rework\n")
    (d / "cycles" / "cycle-1" / "briefs" / "review-a-rework.md").write_text("review\n")
    f = d / "cycles" / "cycle-1" / "findings"
    f.mkdir()
    (f / "arch-1.md").write_text("---\nfinding_id: arch-1\ntrigger: conformance\nkind: issue\ndiscovered_from: b\n---\n")
    (f / "arch-2.md").write_text("---\nfinding_id: arch-2\ntrigger: premise-check\nkind: issue\n---\n")
    _g(home, "commit", "-q", "--allow-empty", "-m", "x")  # b's merge: a task branch merged as `Merge task b`
    _g(home, "checkout", "-q", "-b", "task-b-1")
    _g(home, "commit", "-q", "--allow-empty", "-m", "b")
    _g(home, "checkout", "-q", "main")
    _g(home, "merge", "-q", "--no-ff", "-m", "Merge task b", "task-b-1")
    (d / "tasks" / "open" / "c.md").write_text(
        "---\ntask_name: c\nstatus: ready\ndiscovered_from: b\ndiscovered_by: architect\n---\n")
    (d / "tasks" / "open" / "w.md").write_text(
        "---\ntask_name: w\nstatus: ready\ndiscovered_from: whole-change-cycle-1\ndiscovered_by: reviewer\n---\n")
    (d / "cycles" / "cycle-1" / "briefs" / "c.md").write_text("brief\n")
    (d / "cycles" / "cycle-1" / "briefs" / "w.md").write_text("brief\n")
    _transcript(home, "i2", "implementor: b", [("r1", "claude-sonnet-5-5", "low", (1, 0, 0, 1), "2026-10-23T10:01:00Z")])
    q = run(home, capsys, "spend", "--cycle", "cycle-1")[1]["quality"]
    assert q["rejections"] == 1 and q["rework_tasks"] == ["a"]
    assert sorted((x["source"], x["against"]) for x in q["defects_after_done"]) == \
        [("arch-1", "b"), ("c", "b"), ("w", None)]
    assert q["by_implementor_setting"]["sonnet/low"]["defects_after_done"] == 2
    assert q["by_implementor_setting"]["whole change"]["defects_after_done"] == 1


def test_context_reads_the_last_call_of_an_agent_transcript(home, capsys):
    sub = pathlib.Path(os.environ["CLAUDE_CONFIG_DIR"]) / "projects" / "-home" / "sess-1" / "subagents"
    sub.mkdir(parents=True)

    def call(ctx, ts):
        return json.dumps({"type": "assistant", "timestamp": ts, "message": {"id": ts, "usage": {
            "input_tokens": 10, "cache_creation_input_tokens": 0, "cache_read_input_tokens": ctx - 10,
            "output_tokens": 5}}})
    (sub / "agent-a1b2c3.jsonl").write_text("\n".join([
        json.dumps({"type": "user", "message": {"content": "brief"}}),
        call(450000, "t1"), "not json", call(120000, "t2")]) + "\n")
    code, out, err = run(home, capsys, "context", "a1b2c3")
    assert code == 0, err
    assert (out["context"], out["limit"], out["over"], out["at"]) == (120000, 400000, False, "t2")
    (home / ".claude" / "orchestrator" / "config.yaml").write_text(
        "overlay-version: 1\nproject: scratch\nthresholds:\n  phase-agent-context: 100000\n")
    assert run(home, capsys, "context", "agent-a1b2")[1]["over"] is True  # a prefix, with or without agent-
    code, _, err = run(home, capsys, "context", "zzz")
    assert code == 1 and err["error"] == "no_transcript"
    (sub / "agent-a1b2ff.jsonl").write_text(call(1, "t") + "\n")
    code, _, err = run(home, capsys, "context", "a1b2")
    assert code == 1 and err["error"] == "ambiguous_agent"


def test_an_override_naming_only_a_model_is_compared_by_model(home, capsys):
    d = home / CH / "tasks" / "open" / "a.md"
    d.write_text(d.read_text().replace("implementor: sonnet, medium", "implementor: sonnet"))
    _transcript(home, "i1", "implementor: a", [("r1", "claude-sonnet-5-5", "high", (1, 0, 0, 1), "2026-10-23T10:01:00Z")])
    code, out, err = run(home, capsys, "spend")
    assert code == 0, err
    assert out["override_mismatches"] == [] and out["by_agent"][0]["override"] == "sonnet/?"


def test_a_sibling_directory_with_a_longer_name_is_not_the_home(home, capsys):
    sibling = home.parent / (home.name + "2")
    sibling.mkdir()
    _transcript(home, "s1", "implementor: a", [("r1", "claude-opus-5-5", "medium", (1, 0, 0, 1), "2026-10-23T10:01:00Z")],
                cwd=sibling, project=report._project_key(sibling))
    _transcript(home, "w1", "implementor: b", [("r1", "claude-opus-5-5", "medium", (1, 0, 0, 1), "2026-10-23T10:01:00Z")],
                cwd=home / ".worktrees" / "x", project=report._project_key(home / ".worktrees" / "x"))
    out = run(home, capsys, "spend")[1]
    assert [a["agent_id"] for a in out["by_agent"]] == ["w1"] and out["unattributed_spawns"] == 0
