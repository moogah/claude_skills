"""pytest suite for state.py, one test per scenario of docs/specs/state-layer.org, plus the
check verbs and the event log's writers. Run: python3 -m pytest test_state.py (from this directory).

Each test builds a home in a temporary directory: a git home holding the overlay and the
change's directory, and a separate git repository standing in for the change's worktree,
on the change branch. Merges are real `git merge --no-ff` commits; phase closes are real
empty commits in the home."""

import importlib.util
import json
import os
import pathlib
import subprocess

import pytest

HERE = pathlib.Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location("state", HERE / "state.py")
state = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(state)

FULL_HANDSHAKE = {
    "cycle_id": None, "design_diff": [], "meta_discoveries": [], "asks_for_user_open": [],
    "asks_for_user_resolved": [], "user_resolved_goal_drift": [], "goal_drift_recommendation": "continue",
    "task_refinements": [], "candidate_tasks": [], "journal": [],
}
GOAL_DRIFT = ["revise", "split", "abandon", "continue"]


def git(cwd, *args):
    env = {**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
           "GIT_COMMITTER_EMAIL": "t@t"}
    return subprocess.run(["git", "-C", str(cwd), *args], check=True, capture_output=True, text=True,
                          env=env).stdout.strip()


class Home:
    """A home with one open change, `chg`, whose worktree is a repository on branch `chg`."""

    def __init__(self, root, capsys):
        self.root, self.capsys = root, capsys
        (root / ".claude" / "orchestrator").mkdir(parents=True)
        (root / ".claude" / "orchestrator" / "config.yaml").write_text("overlay-version: 1\nproject: t\n")
        git(root, "init", "-q", "-b", "main")
        git(root, "add", "-A")
        git(root, "commit", "-q", "-m", "home")
        self.wt = root / ".worktrees" / "repo-chg"
        self.wt.mkdir(parents=True)
        git(self.wt, "init", "-q", "-b", "main")
        (self.wt / "README").write_text("x\n")
        git(self.wt, "add", "-A")
        git(self.wt, "commit", "-q", "-m", "base")
        self.base = git(self.wt, "rev-parse", "HEAD")
        git(self.wt, "checkout", "-q", "-b", "chg")
        self.dir = root / ".orchestrator" / "changes" / "chg"
        self.dir.mkdir(parents=True)
        (self.dir / "change.json").write_text(json.dumps({
            "change": "chg", "repo": "repo", "branch": "chg", "worktree": str(self.wt), "base": self.base}))
        self.record = self.wt / "docs" / "changes" / "chg"

    # -- running the tool
    def run(self, *argv):
        try:
            code = state.main(["--home", str(self.root), "--change", "chg", *argv])
        except SystemExit as e:
            code = e.code
        cap = self.capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)

    def ok(self, *argv):
        code, out, err = self.run(*argv)
        assert code == 0, err
        return out

    def gate(self, phase):
        """The gate's report, whether it passes (exit 0) or not (exit 1)."""
        code, out, err = self.run("gate", phase)
        assert out is not None, err
        assert code == (0 if out["passes"] else 1)
        return out

    # -- the record, written as agents write it
    def intent(self, status="speculated", approved=True):
        self.record.mkdir(parents=True, exist_ok=True)
        lines = ["#+TITLE: chg", f"#+STATUS: {status}", f"#+BASE: {self.base}"]
        if approved:
            lines.append("#+APPROVED: 2026-10-28 by the user")
        (self.record / "intent.org").write_text("\n".join(lines + ["", "* Goals", "- a goal", ""]))

    def design(self, seams=True):
        self.record.mkdir(parents=True, exist_ok=True)
        rows = ["| seam/a | one writer | =a.py= | cap § A | =test_a.py= |"] if seams else []
        (self.record / "design.org").write_text("\n".join([
            "#+TITLE: chg: design", "", "* Decisions", "", "* Seams", "",
            "| id | statement | owning symbols | scenarios | tests |",
            "|----+-----------+----------------+-----------+-------|", *rows, ""]))

    def designed(self):
        self.intent()
        self.design()

    def task(self, name, status="ready", critical=False, blocked_by=(), cites=("seam/a",), **extra):
        d = self.dir / "tasks" / "open"
        d.mkdir(parents=True, exist_ok=True)
        lines = ["---", f"task_name: {name}", "description: a task", "change: chg", f"status: {status}",
                 "task_class: feature", f"on_critical_path: {'true' if critical else 'false'}", "cites_seams:"]
        lines += [f"  - {c}" for c in cites] if cites else ["  []"]
        lines += ["relations:", "  blocked_by:" + ("" if blocked_by else " []")]
        lines += [f"    - {b}: {{ status: merged }}" for b in blocked_by]
        lines += [f"{k}: {v}" for k, v in extra.items()]
        lines += ["---", "", "### Summary", "x", ""]
        (d / f"{name}.md").write_text("\n".join(lines))

    def brief(self, cycle, name):
        d = self.dir / "cycles" / cycle / "briefs"
        d.mkdir(parents=True, exist_ok=True)
        (d / f"{name}.md").write_text("brief\n")

    def merge(self, name, subject=None):
        self.n = getattr(self, "n", 0) + 1
        branch = f"task-{name}-{1793000000 + self.n}"
        git(self.wt, "checkout", "-q", "-b", branch, "chg")
        (self.wt / f"{name}.txt").write_text(f"{name} {self.n}")
        git(self.wt, "add", "-A")
        git(self.wt, "commit", "-q", "-m", f"{name}: work")
        git(self.wt, "checkout", "-q", "chg")
        git(self.wt, "merge", "-q", "--no-ff", "-m", subject or f"Merge task {name}", branch)
        return git(self.wt, "rev-parse", "HEAD")

    def review(self, cycle, name, sha):
        d = self.dir / "cycles" / cycle / "reviews"
        d.mkdir(parents=True, exist_ok=True)
        (d / f"{name}.md").write_text(f"---\ntask_name: {name}\nmerge_commit: {sha}\n---\n\n## Summary\nok\n")

    def finding(self, cycle, fid, **extra):
        d = self.dir / "cycles" / cycle / "findings"
        d.mkdir(parents=True, exist_ok=True)
        body = [f"finding_id: {fid}", "trigger: conformance", "kind: issue", "severity: high",
                *[f"{k}: {v}" for k, v in extra.items()]]
        (d / f"{fid}.md").write_text("---\n" + "\n".join(body) + "\n---\n\n## Locations\n")

    def ask(self, ask_id, kind="decision", status="open", blocks=(), options=("yes", "no"), answer=None,
            readback=None):
        d = self.dir / "inbox"
        d.mkdir(parents=True, exist_ok=True)
        lines = [":PROPERTIES:", f":ID: {ask_id}", f":KIND: {kind}", f":STATUS: {status}"]
        if blocks:
            lines.append(f":BLOCKS: {', '.join(blocks)}")
        lines += [":END:", "#+TITLE: Which way?", "", "* Options"]
        lines += [f"- {o} :: what happens" for o in options]
        lines += ["", "* Answer", "# An option label from above, or your own words", answer or "", ""]
        if readback:
            lines += ["* Readback", readback, ""]
        (d / f"{ask_id}.org").write_text("\n".join(lines))

    def handshake(self, cycle, **override):
        h = {**FULL_HANDSHAKE, "cycle_id": cycle, **override}
        h = {k: v for k, v in h.items() if v is not _DROP}
        (self.dir / f"handshake-{cycle}.json").write_text(json.dumps(h))

    def close(self, cycle, phase):
        git(self.root, "commit", "-q", "--allow-empty", "-m", f"Orchestrator: {cycle} {phase} closed")

    def files(self):
        return {str(p.relative_to(self.dir)) for p in self.dir.rglob("*") if p.is_file()}


_DROP = object()


def _parse(text):
    text = (text or "").strip()
    if not text:
        return None
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return None


@pytest.fixture
def home(tmp_path, capsys):
    return Home(tmp_path, capsys)


def started(home, cycle="cycle-1"):
    """A designed change with cycle-1 started in plan."""
    home.designed()
    home.ok("cycle", "start", "--cycle", cycle)
    return cycle


def check(gate, name):
    return next(c for c in gate["checks"] if c["check"] == name)


# ---------------------------------------------------------------- Files and git are the record

def test_no_state_file_is_written(home):
    """Scenario: No state file is written."""
    cycle = started(home)
    home.task("a")
    home.brief(cycle, "a")
    home.ok("assert", "batch_composed", "--by", "orchestrator")
    home.close(cycle, "plan")
    sha = home.merge("a")
    home.task("a", status="done")
    home.review(cycle, "a", sha)
    home.close(cycle, "execute")
    home.handshake(cycle)
    home.ok("assert", "open_tasks_refined_against_handshake", "--by", "orchestrator")
    before = home.files()
    home.ok("note", "decision", "one line")
    home.ok("status")
    home.gate("integrate")
    home.close(cycle, "integrate")
    home.ok("status")
    assert not (home.dir / "state.json").exists()
    assert home.files() - before == set()
    assert "events.jsonl" in before


def test_a_fact_has_no_second_copy(home):
    """Scenario: A fact has no second copy. The merge commit is read from git, not from a
    stale task-file field; a decoration in a finding file is a break, not a source."""
    cycle = started(home)
    sha = home.merge("a")
    home.task("a", status="done", merge_commit="0" * 40)
    home.review(cycle, "a", sha)
    assert home.ok("check", "done", "a")["merge_commit"] == sha
    home.finding(cycle, "arch-1", decoration="hardening")
    home.ok("route", "arch-1", "--decoration", "blocking", "--resolution", "pending")
    gate = home.gate("integrate")
    assert "arch-1" in check(gate, "blocking_findings_resolved")["reason"]
    breaks = home.ok("conventions")["breaks"]
    assert any(b["where"].endswith("tasks/open/a.md") and "merge_commit" in b["problem"] for b in breaks)
    assert any(b["where"].endswith("findings/arch-1.md") and "decoration" in b["problem"] for b in breaks)


# ---------------------------------------------------------------- One answer to where a change stands

def test_a_change_with_no_design_gets_the_design_round(home):
    home.intent()
    home.design(seams=False)
    out = home.ok("status")
    assert out["stage"] == "intent, no design"
    assert out["next"].startswith("the design round")


def test_an_open_cycle_resumes_at_its_phase(home):
    cycle = started(home)
    home.close(cycle, "plan")
    out = home.ok("status")
    assert (out["stage"], out["cycle"], out["phase"]) == ("cycle open", cycle, "execute")
    assert out["next"].startswith("resume execute")


def test_the_main_path_end_calls_for_the_mvp_decision(home):
    home.designed()
    home.merge("a")
    home.task("a", status="done", critical=True)
    home.task("b", status="ready", critical=False)
    out = home.ok("status")
    assert out["stage"] == "main path ended"
    assert out["next"].startswith("integrate's MVP decision")
    code, _, err = home.run("cycle", "start")
    assert code == 1 and err["error"] == "mvp_decision_due"


def test_a_waiting_mvp_decision_is_a_wait(home):
    home.intent(status="mvp 2026-10-28 abc123 ask-cycle-1-4")
    home.design()
    home.ask("ask-cycle-1-4", blocks=["plan"], options=("harden", "rethink", "stop"))
    out = home.ok("status")
    assert out["stage"] == "MVP decision"
    assert out["next"] == "wait for the user's answer to ask-cycle-1-4"


def test_two_sessions_get_the_same_answer(home):
    cycle = started(home)
    home.task("a")
    home.brief(cycle, "a")
    assert home.ok("status") == home.ok("status")


def test_stages_before_the_design(home, capsys):
    code, out, err = Home.run(home, "status")
    assert out["stage"] == "open, no intent"
    other = state.main(["--home", str(home.root), "--change", "nope", "status"])
    assert other == 0 and json.loads(capsys.readouterr().out)["stage"] == "not open"


def test_done_is_nothing(home):
    home.intent(status="done 2026-10-28 abc123")
    home.design()
    out = home.ok("status")
    assert (out["stage"], out["next"]) == ("done", "nothing")


# ---------------------------------------------------------------- A task is done only on computed checks

def test_done_without_a_merge_is_refused(home):
    started(home)
    home.task("a", status="needs_review")
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and err["error"] == "not_done"
    assert "Merge task a" in err["message"]


def test_done_without_a_review_is_refused(home):
    started(home)
    home.task("a", status="needs_review")
    home.merge("a")
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and "review" in err["message"]


def test_a_review_of_an_earlier_merge_does_not_count(home):
    cycle = started(home)
    home.task("a", status="needs_review")
    first = home.merge("a")
    home.review(cycle, "a", first)
    home.merge("a", subject="Merge task a (rework)")
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and "review" in err["message"]


# ---------------------------------------------------------------- A phase passes only on its checks

def test_a_gate_with_a_failing_computed_check_does_not_pass(home):
    cycle = started(home)
    home.close(cycle, "plan")
    home.close(cycle, "execute")
    home.handshake(cycle)
    home.ok("assert", "open_tasks_refined_against_handshake", "--by", "orchestrator")
    home.finding(cycle, "arch-cycle-1-1")
    home.ok("route", "arch-cycle-1-1", "--decoration", "blocking", "--resolution", "pending")
    gate = home.gate("integrate")
    assert gate["passes"] is False
    assert "arch-cycle-1-1" in check(gate, "blocking_findings_resolved")["reason"]
    home.ok("route", "arch-cycle-1-1", "--resolution", "inline-fixed")
    assert home.gate("integrate")["passes"] is True


def _executed(home):
    cycle = started(home)
    home.task("a")
    home.brief(cycle, "a")
    home.ok("assert", "batch_composed", "--by", "orchestrator")
    home.close(cycle, "plan")
    sha = home.merge("a")
    home.task("a", status="done")
    home.review(cycle, "a", sha)
    return cycle


def test_a_passed_gate_is_re_checked(home):
    cycle = _executed(home)
    assert home.gate("execute")["passes"] is True
    home.close(cycle, "execute")
    git(home.wt, "reset", "-q", "--hard", "HEAD~1")
    gate = home.gate("execute")
    assert gate["passes"] is False
    failing = check(gate, "done_tasks_merged_and_reviewed")
    assert failing["ok"] is False and "a" in failing["reason"]


def test_an_asserted_check_shows_as_asserted(home):
    cycle = started(home)
    home.task("a")
    home.brief(cycle, "a")
    home.ok("assert", "batch_composed", "--by", "phase-plan")
    c = check(home.gate("plan"), "batch_composed")
    assert c["kind"] == "asserted" and c["by"] == "phase-plan" and c["at"]
    assert c in home.ok("status")["gate"]["checks"]


def test_a_computed_check_cannot_be_asserted(home):
    started(home)
    code, _, err = home.run("assert", "batch_cites_seams", "--by", "orchestrator")
    assert code == 1 and err["error"] == "computed_check"


# ---------------------------------------------------------------- The three rules

def test_a_follow_up_without_provenance_is_refused(home):
    home.designed()
    home.handshake("cycle-0", candidate_tasks=[
        {"name": "b", "source": "arch-cycle-0-1", "discovered_by": "architect", "critical": False, "quote": "q"}])
    home.task("b")
    code, _, err = home.run("check", "new", "b")
    assert code == 1 and "discovered_from" in err["message"]
    home.task("b", discovered_from="arch-cycle-0-1", discovered_by="architect")
    home.ok("check", "new", "b")


def test_a_task_with_an_unmerged_blocker_does_not_start(home):
    started(home)
    home.task("a")
    home.task("b", blocked_by=["a"])
    code, _, err = home.run("check", "start", "b")
    assert code == 1 and err["error"] == "blocked" and "a" in err["message"]


def test_a_task_starts_once_its_blockers_merge(home):
    started(home)
    home.task("a")
    home.task("b", blocked_by=["a"])
    home.merge("a")
    home.ok("check", "start", "b")


# ---------------------------------------------------------------- Conventions are checked

def test_a_merge_commit_in_another_form_is_reported(home):
    cycle = started(home)
    home.task("a", status="needs_review")
    sha = home.merge("a", subject="Merge branch 'task-a-1793000000' into chg")
    home.review(cycle, "a", sha)
    breaks = home.ok("conventions")["breaks"]
    assert any(b["where"] == sha and b["expected"] == "Merge task a" for b in breaks)
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and sha[:12] in err["message"]


def test_an_unknown_status_is_reported(home):
    started(home)
    home.task("a", status="completed")
    breaks = home.ok("conventions")["breaks"]
    assert any(b["where"].endswith("tasks/open/a.md") and "completed" in b["problem"] for b in breaks)


# ---------------------------------------------------------------- The event log is append-only

def test_a_typo_is_corrected_by_a_new_entry(home):
    cycle = started(home)
    home.finding(cycle, "arch-cycle-1-1")
    seq = home.ok("route", "arch-cycle-1-1", "--decoration", "blocking", "--resolution", "pending")["seq"]
    home.ok("correct", str(seq), "--why", "routed the wrong finding")
    lines = (home.dir / "events.jsonl").read_text().splitlines()
    assert len(lines) == 3  # cycle-started, the routing, its correction
    home.close(cycle, "plan")
    home.close(cycle, "execute")
    assert check(home.gate("integrate"), "blocking_findings_resolved")["ok"] is True


def test_a_cycles_start_is_in_the_log_before_any_artifact(home):
    cycle = started(home)
    out = home.ok("status")
    assert (out["stage"], out["cycle"], out["phase"]) == ("cycle open", cycle, "plan")


def test_a_cycle_does_not_start_twice(home):
    started(home)
    code, _, err = home.run("cycle", "start")
    assert code == 1 and err["error"] == "cycle_open"


def test_the_log_is_only_appended(home):
    started(home)
    first = (home.dir / "events.jsonl").read_text()
    home.task("a")
    home.ok("note", "discovery", "x", "--ref", "seam/a")
    home.ok("blocker", "a", "ask-cycle-1-1")
    after = (home.dir / "events.jsonl").read_text()
    assert after.startswith(first)
    seqs = [json.loads(l)["seq"] for l in after.splitlines()]
    assert seqs == list(range(1, len(seqs) + 1))


# ---------------------------------------------------------------- The handshake carries the plan's context

def test_integrate_closes_without_a_digest(home):
    cycle = _executed(home)
    home.close(cycle, "execute")
    home.handshake(cycle)
    home.ok("assert", "open_tasks_refined_against_handshake", "--by", "orchestrator")
    assert not (home.dir / "cycles" / cycle / "pm-digest.md").exists()
    assert home.gate("integrate")["passes"] is True


def test_plan_refuses_a_handshake_missing_a_field(home):
    cycle = _executed(home)
    home.close(cycle, "execute")
    home.handshake(cycle, meta_discoveries=_DROP)
    home.close(cycle, "integrate")
    code, _, err = home.run("cycle", "start")
    assert code == 1 and err["error"] == "bad_prior_handshake" and "meta_discoveries" in err["message"]


def test_an_open_ask_not_in_the_handshake_is_not_routed(home):
    cycle = started(home)
    home.close(cycle, "plan")
    home.close(cycle, "execute")
    home.handshake(cycle)
    home.ask("ask-cycle-1-1")
    c = check(home.gate("integrate"), "asks_routed")
    assert c["ok"] is False and "ask-cycle-1-1" in c["reason"]
