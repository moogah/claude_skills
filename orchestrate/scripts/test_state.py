"""pytest suite for state.py, one test per scenario of docs/specs/record.org, plus the
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
    "asks_for_user_resolved": [], "user_resolved_goal_drift": [],
    "goal_drift_recommendation": {"recommendation": "none", "reason": "nothing drifted"},
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

    def merge(self, name, subject=None, tests=0):
        """A task branch merged into chg; with `tests` set, the after-file execute § 4 writes for it."""
        self.n = getattr(self, "n", 0) + 1
        branch = f"task-{name}-{1793000000 + self.n}"
        git(self.wt, "checkout", "-q", "-b", branch, "chg")
        (self.wt / f"{name}.txt").write_text(f"{name} {self.n}")
        git(self.wt, "add", "-A")
        git(self.wt, "commit", "-q", "-m", f"{name}: work")
        git(self.wt, "checkout", "-q", "chg")
        git(self.wt, "merge", "-q", "--no-ff", "-m", subject or f"Merge task {name}", branch)
        sha = git(self.wt, "rev-parse", "HEAD")
        if tests is not None:
            self.test_run(name, sha, tests)
        return sha

    def test_run(self, name, sha, status, compare=None):
        stem = self.dir / f"after-{name}-{sha[:12]}"
        stem.with_suffix(".txt").write_text("ran\n")
        stem.with_suffix(".status").write_text(f"{status}\n")
        if compare is not None:
            stem.with_suffix(".compare").write_text(f"{compare}\n")

    def review(self, cycle, name, sha):
        d = self.dir / "cycles" / cycle / "reviews"
        d.mkdir(parents=True, exist_ok=True)
        (d / f"{name}.md").write_text(f"---\ntask_name: {name}\nmerge_commit: {sha}\n---\n\n## Summary\nok\n")

    def finding(self, cycle, fid, **extra):
        d = self.dir / "cycles" / cycle / "findings"
        d.mkdir(parents=True, exist_ok=True)
        fields = {"finding_id": fid, "trigger": "conformance", "kind": "issue", "severity": "high",
                  "evidence": "the run", **extra}
        body = [f"{k}: {v}" for k, v in fields.items() if v is not None]
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
    home.ok("note", "decision", "one line", "--by", "orchestrator")
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
    home.ok("assert", "batch_composed", "--by", "orchestrator")
    c = check(home.gate("plan"), "batch_composed")
    assert c["kind"] == "asserted" and c["by"] == "orchestrator" and c["at"]
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
    home.ok("note", "discovery", "x", "--ref", "seam/a", "--by", "orchestrator")
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


def test_a_candidate_routing_needs_its_candidate(home):
    cycle = started(home)
    home.close(cycle, "plan")
    home.close(cycle, "execute")
    home.finding(cycle, "arch-cycle-1-1")
    home.ok("route", "arch-cycle-1-1", "--decoration", "blocking", "--resolution", "candidate-task-fix-it")
    home.handshake(cycle)
    c = check(home.gate("integrate"), "handshake_written")
    assert c["ok"] is False and "fix-it" in c["reason"]
    home.handshake(cycle, candidate_tasks=[
        {"name": "fix-it", "source": "arch-cycle-1-1", "discovered_by": "architect", "critical": True, "quote": "q"}])
    assert check(home.gate("integrate"), "handshake_written")["ok"] is True


def test_asks_prints_what_the_handshake_carries(home):
    started(home)
    home.ask("ask-cycle-1-1", blocks=["a"], answer="yes")
    out = home.ok("asks")["asks"][0]
    assert (out["id"], out["blocks"], out["decision"], out["path"]) == \
        ("ask-cycle-1-1", ["a"], "yes", ".orchestrator/changes/chg/inbox/ask-cycle-1-1.org")
    assert home.ok("status")["inbox"] == {"open": 1, "answered": ["ask-cycle-1-1"]}


def test_status_lists_every_waiting_ask_blocking_first(home):
    started(home)
    home.ask("ask-cycle-1-1")
    home.ask("ask-cycle-1-2", blocks=["a"])
    home.ask("ask-cycle-1-3", status="applied")
    out = home.ok("status")
    assert [a["id"] for a in out["asks"]] == ["ask-cycle-1-2", "ask-cycle-1-1"]
    assert out["inbox"]["open"] == 2


# ---------------------------------------------------------------- A task's tests pass at its merge

def test_done_without_a_test_run_is_refused(home):
    cycle = started(home)
    home.task("a", status="needs_review")
    sha = home.merge("a", tests=None)
    home.review(cycle, "a", sha)
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and f"after-a-{sha[:12]}.status is missing" in err["message"]
    (home.dir / f"after-a-{sha[:12]}.status").write_text("0\n")  # the exit code is the record; the output may be absent
    home.ok("check", "done", "a")


def test_done_with_failing_tests_on_a_green_baseline_is_refused(home):
    cycle = started(home)
    home.task("a", status="needs_review")
    sha = home.merge("a", tests=1)
    home.review(cycle, "a", sha)
    (home.dir / f"baseline-{cycle}.status").write_text("0\n")
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and "tests fail" in err["message"]


def test_a_red_baseline_is_read_through_the_comparison(home):
    cycle = started(home)
    home.task("a", status="needs_review")
    home.brief(cycle, "a")
    sha = home.merge("a", tests=1)
    home.review(cycle, "a", sha)
    (home.dir / f"baseline-{cycle}.status").write_text("1\n")
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and ".compare holds no test.compare exit code" in err["message"]
    home.test_run("a", sha, 1, compare=1)
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and "regression" in err["message"]
    home.test_run("a", sha, 1, compare=0)
    home.ok("check", "done", "a")


# ---------------------------------------------------------------- the handshake's asks

def test_asks_for_a_cycle_are_those_its_git_touched(home):
    home.designed()
    home.ask("ask-cycle-0-1", status="applied", answer="yes")  # resolved before the cycle
    (home.dir / "cycles" / "cycle-0" / "asks").mkdir(parents=True)  # where a settled ask lives
    (home.dir / "events.jsonl").touch()  # an earlier cycle's log, so its directory is no lost-log evidence
    (home.dir / "inbox" / "ask-cycle-0-1.org").rename(home.dir / "cycles" / "cycle-0" / "asks" / "ask-cycle-0-1.org")
    git(home.root, "add", "-A")
    git(home.root, "commit", "-q", "-m", "earlier")
    import time
    time.sleep(1.1)  # git dates to the second
    cycle = started(home)
    home.ask("ask-cycle-1-1", status="applied", answer="no")  # resolved in it, not yet committed
    home.ask("ask-cycle-1-2")                                 # open
    out = home.ok("asks", "--cycle", cycle)
    assert [a["id"] for a in out["resolved"]] == ["ask-cycle-1-1"]
    assert [a["id"] for a in out["open"]] == ["ask-cycle-1-2"]


# ---------------------------------------------------------------- code review, 2026-10-28

def test_a_change_whose_only_cycle_was_abandoned_starts_again(home):
    started(home)
    home.ok("cycle", "abandon", "--why", "wrong batch")
    assert home.ok("status")["stage"] == "designed, no cycle open"
    assert home.ok("cycle", "start", "--cycle", "cycle-2")["cycle"] == "cycle-2"


def test_a_closed_cycle_without_its_handshake_still_refuses(home):
    cycle = started(home)
    for phase in ("plan", "execute", "integrate"):
        home.close(cycle, phase)
    code, _, err = home.run("cycle", "start")
    assert code == 1 and err["error"] == "no_prior_handshake"


def test_a_handshake_field_of_the_wrong_type_fails_the_gate(home):
    cycle = started(home)
    home.close(cycle, "plan")
    home.close(cycle, "execute")
    home.handshake(cycle, asks_for_user_open=None)
    c = check(home.gate("integrate"), "handshake_written")
    assert c["ok"] is False and "asks_for_user_open: expected a JSON array" in c["reason"]
    assert home.ok("status")["gate"]["passes"] is False


def test_a_deferred_ask_is_carried_though_untouched(home):
    home.designed()
    home.ask("ask-cycle-0-1", status="deferred", answer="later")
    git(home.root, "add", "-A")
    git(home.root, "commit", "-q", "-m", "deferred")
    import time
    time.sleep(1.1)
    cycle = started(home)
    out = home.ok("asks", "--cycle", cycle)
    assert [a["id"] for a in out["resolved"]] == ["ask-cycle-0-1"] and out["open"] == []


# ---------------------------------------------------------------- Asks with no open cycle (asks spec)

def test_the_first_cycle_takes_in_the_asks_settled_before_it(home):
    """asks § An ask settled before any cycle waits in the inbox for the first."""
    home.designed()
    home.ask("ask-chg-1", status="applied")
    home.ask("ask-chg-2")
    out = home.ok("cycle", "start", "--cycle", "cycle-1")
    assert out["asks_moved"] == [".orchestrator/changes/chg/cycles/cycle-1/asks/ask-chg-1.org"]
    assert (home.dir / "cycles" / "cycle-1" / "asks" / "ask-chg-1.org").exists()
    assert not (home.dir / "inbox" / "ask-chg-1.org").exists()
    assert (home.dir / "inbox" / "ask-chg-2.org").exists()  # open: it stays and is carried


def test_a_finding_from_the_design_round_is_routed_by_the_first_cycle(home):
    """architect § Where a defect found at the design round is reported: a finding file in the change's
    findings/, made before any cycle, routed by the first plan or integrate."""
    d = home.dir / "findings"
    d.mkdir(parents=True)
    (d / "arch-chg-1.md").write_text("---\nfinding_id: arch-chg-1\ntrigger: design-round\nkind: issue\n"
                                     "severity: high\nevidence: the run\n---\n\n## Locations\n")
    started(home)
    out = home.ok("route", "arch-chg-1", "--decoration", "hardening", "--resolution", "hardening-list")
    assert out["finding"] == "arch-chg-1"
    assert home.ok("conventions")["ok"] is True


# ---------------------------------------------------------------- A regression fixed inline (execute § 4)

def _inline_fix(home, name="a"):
    git(home.wt, "commit", "-q", "--allow-empty", "-m", f"Inline fix after merging task {name}: the clash")
    return git(home.wt, "rev-parse", "HEAD")


def test_a_task_fixed_inline_after_its_merge_is_done_on_the_run_after_the_fix(home):
    """execute § The task whose merge broke a test is reviewed after the fix; § The run after a fix is
    kept beside the run after the merge."""
    cycle = started(home)
    home.task("a", status="needs_review")
    sha = home.merge("a", tests=1)                      # the regression, left as it was
    fix = _inline_fix(home)
    stem = home.dir / f"after-a-{sha[:12]}-fix-1"
    stem.with_suffix(".txt").write_text("ran\n")
    stem.with_suffix(".status").write_text("1\n")       # the first attempt still failed
    stem2 = home.dir / f"after-a-{sha[:12]}-fix-2"
    stem2.with_suffix(".status").write_text("0\n")
    home.review(cycle, "a", fix)
    assert home.ok("check", "done", "a")["merge_commit"] == sha
    stem2.with_suffix(".status").write_text("1\n")
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and "fix-2" in err["message"]


def test_a_review_naming_a_commit_before_the_merge_does_not_count(home):
    cycle = started(home)
    home.task("a", status="needs_review")
    before = git(home.wt, "rev-parse", "HEAD")
    home.merge("a")
    home.review(cycle, "a", before)
    code, _, err = home.run("check", "done", "a")
    assert code == 1 and "review" in err["message"]


def test_an_inline_fix_subject_is_the_specs_form(home):
    """execute § An inline fix's commit names its task, its cause and its pin."""
    started(home)
    for s in ("Inline fix after merging task a: the clash", "Inline fix after review of task b: an off-by-one"):
        git(home.wt, "commit", "-q", "--allow-empty", "-m", s)
    assert home.ok("conventions")["breaks"] == []
    git(home.wt, "commit", "-q", "--allow-empty", "-m", "Inline fix (review a #1)")
    breaks = home.ok("conventions")["breaks"]
    assert [b["expected"] for b in breaks] == ["Inline fix after <merging|review of> task <task>: <what>"]


def test_a_design_whose_seams_section_is_one_line_is_designed(home):
    """design-round § A change no scenario needs a seam for skips the design with one line."""
    home.intent()
    home.record.mkdir(parents=True, exist_ok=True)
    (home.record / "design.org").write_text("#+TITLE: chg: design\n\n* Seams\n\nNo scenario needs a seam: "
                                            "the change edits the README only.\n\n* Open Questions\n")
    assert home.ok("status")["stage"] == "designed, no cycle open"
    (home.record / "design.org").write_text("#+TITLE: chg: design\n\n* Seams\n\n* Open Questions\n")
    assert home.ok("status")["stage"] == "intent, no design"


def test_a_seamless_change_passes_the_seam_check_with_tasks_citing_none(home):
    """plan § A change no scenario needs a seam for passes the gate with tasks citing none (R4-9)."""
    cycle = started(home)
    home.task("doc", cites=())
    home.brief(cycle, "doc")
    assert check(home.gate("plan"), "batch_cites_seams")["ok"] is False
    (home.record / "design.org").write_text("#+TITLE: chg: design\n\n* Seams\n\nNo scenario needs a seam: "
                                            "the change edits a docstring only.\n")
    c = check(home.gate("plan"), "batch_cites_seams")
    assert c["ok"] is True and "needs no seam" in c["reason"]


def test_rework_briefs_and_reviews_are_numbered(home):
    """execute § A re-spawned Implementor's brief names its task and the scope of its fix: each
    rework's brief is a file of its own, so a second does not overwrite the first."""
    cycle = started(home)
    home.task("a", status="needs_review")
    for n in ("a", "a-rework-1", "a-rework-2", "review-a-rework-2"):
        home.brief(cycle, n)
    assert home.ok("conventions")["breaks"] == []
    sha = home.merge("a", subject="Merge task a (rework)")
    d = home.dir / "cycles" / cycle / "reviews"
    d.mkdir(parents=True, exist_ok=True)
    (d / "a-rework-2.md").write_text(f"---\ntask_name: a\nmerge_commit: {sha}\n---\n\n## Summary\nok\n")
    home.ok("check", "done", "a")
    assert home.ok("conventions")["breaks"] == []
    assert state.Change(str(home.root), "chg").batch(cycle) == ["a"]  # rework briefs are not batch tasks


# ---------------------------------------------------------------- ported guarantees (record, 2026-11-14)

def test_no_cycle_starts_in_a_home_whose_git_ignores_the_record(home):
    home.designed()
    (home.root / ".gitignore").write_text(".orchestrator\n")
    code, _, err = home.run("cycle", "start")
    assert code == 1 and err["error"] == "orchestrator_ignored"
    assert not home.ok("log")["entries"]


def test_no_cycle_starts_while_the_code_root_holds_a_record_of_its_own(home):
    home.designed()
    (home.wt / ".orchestrator").mkdir()
    code, _, err = home.run("cycle", "start")
    assert code == 1 and err["error"] == "misplaced_artifacts"


def test_a_finding_with_no_kind_or_no_evidence_is_a_break(home):
    cycle = started(home)
    home.finding(cycle, "arch-1", kind=None)
    home.finding(cycle, "arch-2", evidence=None)
    home.finding(cycle, "arch-3", kind="thought")
    home.finding(cycle, "arch-4")
    probs = {(b["where"].split("/")[-1], b["problem"].split(" ")[0]) for b in home.ok("conventions")["breaks"]}
    assert ("arch-1.md", "kind") in probs and ("arch-2.md", "evidence") in probs and ("arch-3.md", "kind") in probs
    assert not any(w == "arch-4.md" for w, _ in probs)


def test_a_journal_entry_names_who_wrote_it_from_the_closed_set(home):
    started(home)
    code, _, err = home.run("note", "note", "x", "--by", "someone")
    assert code == 1 and err["error"] == "bad_by"
    home.ok("note", "note", "x", "--by", "reviewer")
    code, _, err = home.run("note", "note", "x")
    assert code == 1 and err["error"] == "bad_by"
    code, _, err = home.run("assert", "batch_composed", "--by", "someone")
    assert code == 1 and err["error"] == "bad_by"
    home.ok("assert", "batch_composed", "--by", "orchestrator")


# ---------------------------------------------------------------- the decisions of 2026-11-16

def test_a_findings_first_route_names_its_decoration(home):
    """IR-E1: a first route with no --decoration is refused; a later one may give the resolution alone."""
    cycle = started(home)
    home.finding(cycle, "arch-cycle-1-1")
    code, _, err = home.run("route", "arch-cycle-1-1", "--resolution", "inline-fixed")
    assert code == 1 and err["error"] == "no_decoration" and "arch-cycle-1-1" in err["message"]
    assert not home.ok("log", "--kind", "finding-routed")["entries"]
    home.ok("route", "arch-cycle-1-1", "--decoration", "hardening", "--resolution", "pending")
    home.ok("route", "arch-cycle-1-1", "--resolution", "hardening-list")
    assert home.ok("log", "--kind", "finding-routed")["entries"][-1]["resolution"] == "hardening-list"


def test_a_route_with_no_decoration_yet_names_one(home):
    """IR-E1 (round 8): an entry with a resolution and no decoration (as a converted home lays one) decorates
    nothing, so the next route still names a decoration."""
    import json as _json
    cycle = started(home)
    home.finding(cycle, "arch-cycle-1-1")
    log = home.dir / "events.jsonl"
    last = _json.loads(log.read_text().splitlines()[-1])
    entry = dict(last, seq=last["seq"] + 1, kind="finding-routed", finding="arch-cycle-1-1",
                 decoration=None, resolution="pending")
    for k in [k for k in entry if k not in ("seq", "kind", "at", "cycle", "finding", "decoration", "resolution")]:
        entry.pop(k)
    log.write_text(log.read_text() + _json.dumps(entry) + "\n")
    code, _, err = home.run("route", "arch-cycle-1-1", "--resolution", "design-amended")
    assert code == 1 and err["error"] == "no_decoration"
    home.ok("route", "arch-cycle-1-1", "--decoration", "blocking", "--resolution", "design-amended")


def test_a_corrected_first_route_leaves_the_finding_unrouted(home):
    cycle = started(home)
    home.finding(cycle, "arch-cycle-1-1")
    seq = home.ok("route", "arch-cycle-1-1", "--decoration", "blocking")["seq"]
    home.ok("correct", str(seq), "--why", "the wrong finding")
    code, _, err = home.run("route", "arch-cycle-1-1", "--resolution", "pending")
    assert code == 1 and err["error"] == "no_decoration"


def test_an_answered_ask_in_the_inbox_holds_cycle_start(home):
    """asks group 4: asks § An answered ask is handled before the next spawn, by a phase agent."""
    home.designed()
    home.ask("ask-chg-1", status="answered", answer="yes")
    home.ask("ask-chg-2", answer="no")  # open with an exact answer: answered all the same
    home.ask("ask-chg-3", answer="no, unless it is a refund", readback="Refunds only.")  # waits on the user's yes
    code, _, err = home.run("cycle", "start", "--cycle", "cycle-1")
    assert code == 1 and err["error"] == "answered_ask_waiting"
    assert err["asks"] == ["ask-chg-1", "ask-chg-2"] and "inbox/ask-chg-1.org" in err["message"]
    assert not (home.dir / "events.jsonl").exists()
    home.ask("ask-chg-1", status="deferred", answer="later")
    home.ask("ask-chg-2", status="applied", answer="no")  # settled with no cycle open: waits for cycle start
    home.ok("cycle", "start", "--cycle", "cycle-1")


def test_status_shows_the_asks_the_gates_hold_on_and_names_them_first_in_next(home):
    """asks group 4: an answer waiting in any form is the one set status, the gates and check start read."""
    cycle = started(home)
    home.ask(f"ask-{cycle}-1", status="answered", answer="yes")
    home.ask(f"ask-{cycle}-2", answer="the second, for refunds")             # free text, no readback yet
    home.ask(f"ask-{cycle}-3", answer="maybe", readback="Refunds only.")      # readback waits on the yes
    home.ask(f"ask-{cycle}-4", answer="maybe", readback="Refunds only.\n\nyes")  # confirmed, still open
    home.ask(f"ask-{cycle}-5", status="declined", answer="no")               # settled, not moved
    home.ask(f"ask-{cycle}-6", status="deferred", answer="later")            # deferred stays in the inbox
    home.ask(f"ask-{cycle}-7")                                                # unanswered
    want = [f"ask-{cycle}-{n}" for n in (1, 2, 4, 5)]
    out = home.ok("status")
    assert out["inbox"]["answered"] == want
    assert out["next"].startswith("handle the answered asks first") and all(i in out["next"] for i in want)
    assert f"ask-{cycle}-3" not in out["next"]
    c = check(home.gate("plan"), "answered_asks_applied")
    assert c["ok"] is False and all(i in c["reason"] for i in want) and f"ask-{cycle}-3" not in c["reason"]


def test_an_answered_ask_in_the_inbox_holds_a_task_start(home):
    """asks § An answered ask is handled before the next spawn, by a phase agent."""
    cycle = started(home)
    home.task("a")
    home.ask(f"ask-{cycle}-1", answer="yes")
    code, _, err = home.run("check", "start", "a")
    assert code == 1 and err["error"] == "answered_ask_waiting" and err["asks"] == [f"ask-{cycle}-1"]
    home.ask(f"ask-{cycle}-1", status="answered", answer="yes", readback="Yes means refunds too.")
    assert home.run("check", "start", "a")[0] == 1  # answered is answered, readback or not
    (home.dir / "inbox" / f"ask-{cycle}-1.org").unlink()
    home.ok("check", "start", "a")


@pytest.mark.parametrize("phase", ["plan", "execute", "integrate"])
def test_an_answered_ask_in_the_inbox_holds_every_phase_gate(home, phase):
    cycle = started(home)
    for p in ("plan", "execute", "integrate")[:("plan", "execute", "integrate").index(phase)]:
        home.close(cycle, p)
    home.ask(f"ask-{cycle}-1", status="answered", answer="yes")
    c = check(home.gate(phase), "answered_asks_applied")
    assert c["ok"] is False and f"ask-{cycle}-1" in c["reason"]
    home.ask(f"ask-{cycle}-1", status="open")
    assert check(home.gate(phase), "answered_asks_applied")["ok"] is True


def _lost_log(home):
    cycle = started(home)
    home.brief(cycle, "a")
    home.close(cycle, "plan")
    (home.dir / "cycles" / cycle / "execute-handover.md").write_text("* Merged\n* Open\n* Next\n")
    (home.dir / "events.jsonl").unlink()
    return cycle


def test_status_names_a_lost_log(home):
    """asks group 7 (R3-1): with a cycle's files and close on disk and no log, status says the log is
    missing, not that the main path has ended or no cycle is open."""
    cycle = _lost_log(home)
    home.task("a", status="done", critical=True)
    out = home.ok("status")
    assert out["stage"] == "event log missing"
    lost = out["log_missing"]
    assert lost["in_git"] is None and "environment ask" in out["next"]
    assert f".orchestrator/changes/chg/cycles/{cycle}/execute-handover.md" in lost["evidence"]
    assert any(f"Orchestrator: {cycle} plan closed" in e for e in lost["evidence"])


def test_status_points_at_the_logs_copy_in_git(home):
    cycle = started(home)
    git(home.root, "add", ".orchestrator")
    home.close(cycle, "plan")
    (home.dir / "events.jsonl").unlink()
    out = home.ok("status")
    assert out["stage"] == "event log missing" and out["log_missing"]["in_git"]
    assert "restore" in out["next"]


@pytest.mark.parametrize("argv", [
    ("note", "note", "x", "--by", "orchestrator"), ("route", "arch-1", "--decoration", "blocking"), ("blocker", "a", "ask-1"),
    ("correct", "1", "--why", "x"), ("assert", "batch_composed", "--by", "orchestrator"),
    ("cycle", "abandon", "--why", "x"), ("gate",)])
def test_no_verb_but_cycle_start_writes_a_fresh_log(home, argv):
    cycle = _lost_log(home)
    home.task("a")
    home.finding(cycle, "arch-1")
    code, _, err = home.run(*argv)
    assert code == 1 and err["error"] == "no_log", err
    assert not (home.dir / "events.jsonl").exists()
    assert "execute-handover.md" in err["message"]


def test_a_note_before_any_cycle_is_refused(home):
    home.designed()
    code, _, err = home.run("note", "note", "x", "--by", "orchestrator")
    assert code == 1 and err["error"] == "no_log"
    assert not (home.dir / "events.jsonl").exists()
    home.ok("cycle", "start", "--cycle", "cycle-1")
    home.ok("note", "note", "x", "--by", "orchestrator")


# ---------------------------------------------------------------- the done step after harden (integrate § 9 Done)

def _hardened(home, answer="harden", entries=(("TASK", "h"), ("LEFT", "ask-cycle-1-1")), h_status="done"):
    """A change at its MVP whose decision was applied with `answer`, in cycle-2's integrate after the hardening
    task `h` merged, with a whole-change review of that merge, the handshake written and the check asserted."""
    home.designed()
    home.ok("cycle", "start", "--cycle", "cycle-2")
    mvp = home.merge("a")
    home.task("a", status="done", critical=True)
    home.intent(status=f"mvp 2026-11-01 {mvp[:12]} ask-cycle-1-1")
    home.ask("ask-cycle-1-1", status="applied", blocks=["plan"], options=("harden", "rethink", "stop"), answer=answer)
    (home.dir / "cycles" / "cycle-2" / "asks").mkdir(parents=True)
    (home.dir / "inbox" / "ask-cycle-1-1.org").rename(home.dir / "cycles" / "cycle-2" / "asks" / "ask-cycle-1-1.org")
    home.task("h", status=h_status)
    home.brief("cycle-2", "h")
    sha = home.merge("h")
    (home.dir / "hardening.org").write_text("#+TITLE: Hardening list: chg\n" + "".join(
        f"\n* issue (hardening): entry {i}\n:PROPERTIES:\n:SEVERITY: low\n" + (f":{k}: {v}\n" if k else "") + ":END:\nbody\n"
        for i, (k, v) in enumerate(entries)))
    home.close("cycle-2", "plan")
    home.close("cycle-2", "execute")
    home.review("cycle-2", "whole-change", sha)
    home.handshake("cycle-2")
    home.ok("assert", "open_tasks_refined_against_handshake", "--by", "orchestrator")
    return "cycle-2"


def _done(home, merged=True, commit=True):
    """integrate § 9 Done: the done line with its summary, committed on chg, and chg merged into `ws`."""
    git(home.wt, "branch", "ws", home.base)
    rec = json.loads((home.dir / "change.json").read_text())
    (home.dir / "change.json").write_text(json.dumps({**rec, "from": "ws"}))
    text = (home.record / "intent.org").read_text()
    (home.record / "intent.org").write_text(text.replace("#+STATUS: mvp", "#+STATUS: done 2026-11-02 x #")
                                            + "\n* Status\n** done\nthe closing summary\n")
    if commit:
        git(home.wt, "add", "-A")
        git(home.wt, "commit", "-q", "-m", "Done: chg")
    if merged:
        ws = home.root / "ws-wt"
        git(home.wt, "worktree", "add", "-q", str(ws), "ws")
        git(ws, "merge", "-q", "--no-ff", "chg", "-m", "Merge chg")


def test_a_done_step_owed_and_not_made_is_advice_not_a_refusal(home):
    _hardened(home)
    g = home.gate("integrate")
    assert g["passes"] is True and check(g, "done_step_made")["ok"] is True
    assert len(g["advice"]) == 1 and "looks owed" in g["advice"][0] and "§ 9 Done" in g["advice"][0]
    out = home.ok("status")
    assert out["advice"] == g["advice"] and out["next"].startswith("close integrate") and "looks owed" in out["next"]


def test_a_done_step_made_passes_integrates_gate(home):
    _hardened(home)
    _done(home)
    g = home.gate("integrate")
    assert check(g, "done_step_made")["ok"] is True and g["passes"] is True and "advice" not in g


@pytest.mark.parametrize("merged,commit,missing", [(False, True, "not merged into ws"), (False, False, "not committed")])
def test_a_done_line_not_committed_or_not_merged_fails_the_gate(home, merged, commit, missing):
    _hardened(home)
    _done(home, merged=merged, commit=commit)
    c = check(home.gate("integrate"), "done_step_made")
    assert c["ok"] is False and missing in c["reason"]


def test_a_closed_cycle_owing_the_done_step_advises_it_in_status(home):
    cycle = _hardened(home)
    home.close(cycle, "integrate")
    out = home.ok("status")
    assert out["stage"] == "designed, no cycle open" and out["next"].startswith("start a plan")
    assert "§ 9 Done" in out["next"] and "looks owed" in out["advice"][0]
    home.ok("cycle", "start", "--cycle", "cycle-3")


def test_an_open_task_never_batched_off_the_main_path_does_not_hold_the_done_step(home):
    _hardened(home)
    home.task("left-off-path", status="ready")
    assert home.gate("integrate").get("advice")


@pytest.mark.parametrize("answer", ["stop", "rethink", ""])
def test_no_done_step_is_owed_without_a_harden_answer(home, answer):
    _hardened(home, answer=answer)
    assert _not_owed(home)


@pytest.mark.parametrize("status", ["approved", "divergent 2026-11-02 ask-cycle-1-1"])
def test_no_done_step_is_owed_off_the_mvp_line(home, status):
    _hardened(home)
    home.intent(status=status)
    assert _not_owed(home)


def test_no_done_step_is_owed_while_the_mvp_ask_is_open(home):
    _hardened(home)
    p = home.dir / "cycles" / "cycle-2" / "asks" / "ask-cycle-1-1.org"
    p.write_text(p.read_text().replace(":STATUS: applied", ":STATUS: open").replace("\nharden\n", "\n\n"))
    assert _not_owed(home)


@pytest.mark.parametrize("entries,h_status", [((("TASK", "h"), (None, None)), "done"),
                                              ((("TASK", "h"),), "failed"), ((("TASK", "other"),), "done")])
def test_no_done_step_is_owed_while_hardening_work_is_left(home, entries, h_status):
    _hardened(home, entries=entries, h_status=h_status)
    assert _not_owed(home)


def test_no_done_step_is_owed_while_a_main_path_task_is_open(home):
    _hardened(home)
    home.task("again", status="ready", critical=True)
    assert _not_owed(home)


def test_no_done_step_is_owed_while_a_candidate_or_a_blocking_finding_remains(home):
    cycle = _hardened(home)
    (home.dir / "cycles" / cycle / "candidate-tasks.json").write_text(json.dumps([{"name": "fix"}]))
    assert _not_owed(home)
    (home.dir / "cycles" / cycle / "candidate-tasks.json").write_text("[]")
    home.finding(cycle, "rev-1")
    home.ok("route", "rev-1", "--decoration", "blocking")
    assert _not_owed(home)


def test_no_done_step_is_owed_without_a_whole_change_review_after_the_mvp(home):
    cycle = _hardened(home)
    review = home.dir / "cycles" / cycle / "reviews" / "whole-change.md"
    mvp = (home.record / "intent.org").read_text().split("#+STATUS: mvp 2026-11-01 ")[1][:12]
    review.write_text(review.read_text().replace(review.read_text().split("merge_commit: ")[1].split("\n")[0], mvp))
    assert _not_owed(home)
    review.unlink()
    assert _not_owed(home)


def test_a_change_with_no_mvp_decision_passes_the_done_check(home):
    cycle = _executed(home)
    home.close(cycle, "execute")
    home.handshake(cycle)
    home.ok("assert", "open_tasks_refined_against_handshake", "--by", "orchestrator")
    assert _not_owed(home) and "advice" not in home.ok("status")


def _not_owed(home):
    g = home.gate("integrate")
    return check(g, "done_step_made")["ok"] is True and "advice" not in g
