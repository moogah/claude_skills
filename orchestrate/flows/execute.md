# Execute phase

The bridge. Design becomes diff; diff becomes discovery.

A task is "executed" only when **both** implement and review have completed on it. Implement-without-review is in-flight, not done.

## Operations

### 1. Implement (per task, in worktree)

For each task in the batch, the orchestrator:

1. Captures a baseline test snapshot if one isn't already taken for this cycle: `$TEST_CMD` (the overlay's `test.command`) run in the code root (`cd "$CODE_ROOT" && $TEST_CMD > "$ORCH_CHANGE_DIR/baseline-<cycle-id>.txt"`), then `state.py set baseline_snapshot=.orchestrator/changes/<change>/baseline-<cycle-id>.txt baseline_status=<exit code>` (paths in state are relative to the home). `$ORCH_HOME` and `$CODE_ROOT` come from `eval "$(state.py where --sh)"` (§ 3); the orchestrator's own cwd stays the home, and every git or test command that belongs in the code root says so with `-C "$CODE_ROOT"` or a `cd` in a subshell.
2. Creates a worktree on the branch `$BRANCH_NAME=task-<task-name>-<ts>` (the same `<ts>` names the after-file in § 4) at the path the overlay's worktree placement gives it, beside the change's own worktree and never inside it (`overlay.md` § Worktrees): `eval "$(state.py worktree "$BRANCH_NAME" --sh)"` sets `$WORKTREE_PATH`.
   - Uses `git -C "$CODE_ROOT" worktree add "$WORKTREE_PATH" -b "$BRANCH_NAME"`: the branch starts from the change integration branch, the code root's HEAD.
   - `-C "$CODE_ROOT"` names the repository; the path is under the home's worktrees folder.
3. Runs the overlay's `worktree.init` hook if defined.
4. Spawns an Implementor agent (`Agent` tool, `subagent_type: general-purpose`). The call returns at once and the agent runs concurrently with the orchestrator and the other agents; its completion arrives as a notification. No polling, no `sleep`: when nothing can run, the orchestrator ends its turn and is woken by the hand-back.
5. Hands the agent the assembled brief (per `roles/implementor.md`), saved as `<change-dir>/cycles/<cycle-id>/briefs/<task-name>.md` so the review and the audit trail can see what the Implementor saw. The brief names the report path, `$ORCH_CHANGE_DIR/cycles/<cycle-id>/reports/<task-name>.md` (the orchestrator creates the directory); the Implementor writes its report there and hands back one line (`SKILL.md` § Hand-back lines).
6. `state.py task set <name> in_progress agent_task_id=<id> worktree_path=<path> branch_name=<branch>` (the tool sets `started_at`).

When an agent completes, the orchestrator reads its one line and verifies at least one commit landed on the worktree branch (`git -C "$CODE_ROOT" rev-parse "$BRANCH_NAME"` against the sha in the line). No commits → `state.py task set <name> failed` (worktree retained for debugging). The report file is read at integrate; in execute the orchestrator opens it only when the line carries `deviated` or `candidates=<n>`.

While agents run, the orchestrator does the work that does not depend on the ones still running: verify each completed commit, save its report, merge whatever has landed (§ 3), spawn Reviewers (§ 7). A task whose `blocked_by` tasks have not all merged is spawned when they have.

The orchestrator never adds scope to a running Implementor by message. New work is a task of its own, drafted and premise-checked at the next plan, or an inline fix after review (§ 8). The record: scope sent to a running Implementor by `SendMessage` without a premise check was blocked by its Reviewer, cost a rework and a re-review, and then a one-task cycle of its own (24M tokens, 57 minutes).

### 2. No Architect run in execute

Nothing runs between an Implementor's commit and its merge. The Reviewer (§ 7) reads the cited seam rows and may probe the merge candidate; that is the check the old execute-time Architect run performed, and the useful catches in the record came from the probes, not the trigger. Restore an execute-time Architect run only on a demonstrated Reviewer miss that a seam-scoped run would have caught (`roles/architect.md` § Three runs).

### 3. Merge each task as it lands

Merges are not held for the batch. When an Implementor completes with a commit, its task joins the merge queue at once. The queue merges one task at a time, in the order tasks landed, while the other Implementors keep running; the chain is serial because there is one change integration branch. A task that merges and passes § 4 goes straight on through § 5-7 (its Reviewer is spawned), and the chain moves to the next queued task without waiting for that review. A task held by a blocking ask (§ 9) stays out of the queue; the tasks behind it merge. When the chain pauses (nothing queued, agents still running), the orchestrator runs `state.py inbox` and handles an answered file before it waits again (§ 9). The record shows why: a batch that waited for all seven Implementors made the first-finished task wait 10 minutes for its merge and 12 for its Reviewer, where batches merged as they landed spawned each Reviewer 0-3 minutes after its Implementor ended.

```bash
eval "$(python3 ~/.claude/skills/orchestrate/scripts/state.py where --sh)"   # ORCH_HOME, ORCH_CHANGE, ORCH_CHANGE_DIR, CODE_ROOT, …
git -C "$CODE_ROOT" merge --no-ff "$BRANCH_NAME" -m "Merge task $TASK_NAME: $DESCRIPTION"
MERGE_COMMIT=$(git -C "$CODE_ROOT" rev-parse HEAD)
```

`MERGE_COMMIT` is recorded in step 6 with the status change. **Conflicts**: `git merge --abort`, `state.py task set <name> blocked blocker_note=merge-conflict`, keep worktree, continue with next task.

### 4. Test after each merge

```bash
(cd "$CODE_ROOT" && $TEST_CMD) > "$ORCH_CHANGE_DIR/after-${TASK_NAME}-${TS}.txt" 2>&1
AFTER_STATUS=$?
```

The command runs in the code root; the after-file lives in the change's directory at the home. Record the file: `state.py task set <name> after_snapshot=.orchestrator/changes/<change>/after-<task>-<ts>.txt`. A regression is a test that passed in the baseline and fails after the merge. With a green baseline (`BASELINE_STATUS == 0`) the exit code decides: `AFTER_STATUS != 0` is a regression. With a red baseline the exit code cannot decide (it is non-zero both times), so the check is per test id: the overlay's `test.compare <baseline-file> <after-file>` (`overlay.md`; the skill ships `scripts/compare.py --parser <test.parser>`, which reads `emacs-results-txt`, `junit-xml` and `tap`), run in the code root like the test command with the two files' absolute paths, which exits non-zero and prints the ids that failed after but not before, and exits 2 when a file holds no run it can read, which is not a verdict; a project whose runner gives ids and whose overlay has no `test.compare` gets an `environment` ask at the first red baseline, since the rule below cannot fire. `state.py set baseline_status=<n>` (§ 1) prints that warning when `n` is non-zero and the key is absent, and `init` repeats it from the last cycle's baseline; the warning is the trigger. The record: the rule as text alone did not fire in a replay, and the agent ran the overlay's script by hand. The record: one project's baseline carried 13 known failures for every cycle, the exit-code rule never fired, and the set comparison the overlay improvised was patched mid-cycle. When a regression is found: `state.py task set <name> regression_detected=true`; stop further merges; keep worktrees; raise an `environment`-kind ask (`templates/ask.md`) with the after-file paths. It blocks the merge chain, so it is asked at once (§ 9). Only merging stops: Implementors still running finish and join the queue, and Reviewers already spawned carry on.

### 5. Capture orchestrator-side discoveries

After each merge, before that task's review, the orchestrator scans for discoveries no individual agent owns. Per the brainstorm and lifted from VCE's §A.7.5:

- **Latent bugs surfaced by a regression** — a test broke not because the merging task was wrong but because it perturbed a pre-existing fragile assumption (e.g. a non-stable sort coupled to insertion order).
- **Worker discoveries and observations on the merged task body** — route each `## Discoveries` entry the report lists under `candidate_tasks` by `templates/architect-finding.md` § Routing: on the change's main path, or an untrue sentence, it is fixed in the change by an in-change follow-up task, which starts with the reproduction; otherwise it is an entry on the hardening list (`templates/hardening.md`). Scan `## Observations` too: one that names a defect in code or text that exists is routed the same way, as if it were a discovery; a `thought:` line and the rest stay in the body as the record. The Reviewer does not see either (§ 7).
- **Manual conflict-resolution decisions** — when the orchestrator dropped, restructured, or regenerated code while reconciling two branches, that decision is an unreviewed structural change, journaled as a `note decision` (below). If it touched a contract or dropped a test, that is a defect in merged code: a follow-up task, routed like a finding.
- **Aborted merges where the abort reason is itself the finding** — journal the structural issue (not just the merge failure) as a `note discovery`; a defect it names is routed like a finding (`templates/architect-finding.md` § Routing), so the next cycle can address it.

Each discovery is one `state.py note discovery "<one line>" --ref <task>` (with `--path` when a report holds the detail), naming where it was routed (`fixed: <task or inline fix>`, `hardening`); a conflict-resolution decision is `note decision`; a scan that found nothing is one `note note "scanned <what>; nothing"`. The journal is where these are recorded; they do not become state keys. A journal line is never where a defect ends: it ends in a fix, a task, an ask or a hardening-list entry. A follow-up task is written by the procedure in `externalisation.md` § Who writes a task file.

### 6. Flip to `needs_review` (NOT `done`)

After successful merge + tests pass + no regression, the task is **not yet** closed:

```bash
state.py task set "$TASK_NAME" needs_review merge_commit="$MERGE_COMMIT" implementor_report_path="$REPORT"
```

where `$REPORT` is the Implementor's structured report, written by the Implementor at `<change-dir>/cycles/<cycle-id>/reports/<task-name>.md`, the path its brief named (the tool refuses `needs_review` without `merge_commit` and sets `completed_at`). Mirror `status: needs_review` and `merge_commit` into the task file's frontmatter (file stays in `tasks/open/`); mirror `status: done` the same way in § 8. The task file lives at the home (`$ORCH_HOME/.orchestrator/changes/<change>/tasks/open/<task>.md`), not in the worktree: the Implementor is given its absolute path and appends `## Observations` and `## Discoveries` there; the orchestrator commits the task file (the mirror and those sections) in the home at execute close with the other artifacts, as SKILL.md says. That holds when the home is the code root too: the Implementor writes the main checkout's copy by its absolute path, never a worktree's, and commits only code. With a split, the Reviewer's diff (`git diff <merge-base>..<merge_commit>` in the code root) cannot contain the Observations hunk, which closes the author-blind leak the record noted.

Then remove the worktree and record it:

```bash
git -C "$CODE_ROOT" worktree remove "$WORKTREE_PATH"
git -C "$CODE_ROOT" branch -D "$BRANCH_NAME"   # optional; tidies local branch list
state.py task set "$TASK_NAME" worktree_removed=true
```

### 7. Review (author-blind, per task)

Once a task is `needs_review`, the orchestrator spawns a Reviewer. **The reviewer-spawn helper enforces the author-blind constraint at the substrate level.**

#### Reviewer-spawn helper contract

The helper builds the Reviewer's input from a **fixed set of sources**, none of which are the Implementor's:

```yaml
reviewer_input:
  diff: $(git diff <merge-base>..<merge_commit>)
  task_brief: <full text of $ORCH_HOME/.orchestrator/changes/<change>/tasks/open/<task-name>.md, read from the home, EXCLUDING ## Observations and ## Discoveries sections>
  cited_seams: <the design.org Seams rows named in cites_seams, verbatim>
  cited_scenarios: <for each scenario in the task's cites_scenarios, its text from docs/specs/ at merge_commit and its coverage.py --scenario row>
  project_standards: <overlay's roles/reviewer.md, if present>
  project_priors: <overlay's priors.md, if present>
```

The helper **must not**:
- Read or pass the Implementor's structured report.
- Read or pass the `## Observations` or `## Discoveries` sections.
- Pass any indicator of the Implementor's identity.
- Allow the Reviewer's worktree to contain any file other than the clean checkout of `merge_commit`.
- Pass any other in-flight diffs.

The helper is the load-bearing piece that makes the author-blind constraint structural rather than a discipline. Modifying it to violate any of the above is a bug, not a feature request.

#### Review modes

- **Inline review**: the orchestrator (the execute phase agent) runs the review itself in its own context. Useful for small batches where the orchestrator wants to ride along closely. Default: when batch size ≤ 2.
- **Delegated review**: spawn a separate general-purpose Agent per task. Default when batch size ≥ 3, or when the orchestrator's context is bloated. The Reviewer writes its findings file and hands back one line with the counts (`SKILL.md` § Hand-back lines); the orchestrator opens the file when the count is not zero.

Either way, the input contract is identical and author-blind. The Reviewer may run the test command and its own probes read-only in a scratch directory against the `merge_commit` checkout (`roles/reviewer.md` § Probes); the probes' output goes in the findings file, never into the repository.

### 8. Handle review findings

The Reviewer's findings file lands at `<change-dir>/cycles/<cycle-id>/reviews/<task-name>.md`; the orchestrator records it with `state.py task set <name> reviewed review_mode=<inline|delegated> findings_path=<path> findings_count=<n>` (the summary stays in the file; state holds the path and the count). Then it processes:

Each finding of kind `issue` or `question` is routed by `templates/architect-finding.md` § Routing, reading the intent's goals for the main path; a `thought` under `## Thoughts` stays in the file.

| Route | Orchestrator action |
|---|---|
| `blocking` (main path, or an untrue sentence) | Reproduce, then fix. The fix starts with a test or probe that fails on the defect, then makes it pass: an inline fix (`state.py note inline-fix "<what>" --ref <task>`), a re-spawned Implementor with fix scope (`state.py task set <name> in_progress`, which counts a rejection), or a revert of the merge. The task does not advance to `done` until it is fixed. A `question` whose reproduction does not fail goes on the hardening list with what was tried; an untrue sentence needs no failing test, only the corrected text. |
| `hardening` (any other defect) | One entry on the change's hardening list (`templates/hardening.md`), with `:REF:` the review file and finding number. The task can advance to `done`. |
| `spec-signal` direction | Write an ask file at once (`templates/ask.md`; `:RAISED_BY: reviewer`, `:REF:` the review file's path, since Reviewer findings carry no id); presented inline only under § 9's rule, otherwise listed at integrate. Does not block the task itself; it signals that the design may need revision. When the finding carries a `stopgap` (an `issue`, `roles/reviewer.md` § Routing of a finding), apply it as an inline fix before the task is `done`, pinned on the surface the probe used (below), and name it in the ask file; a stopgap that would answer the ask is not applied and the ask says so |

The orchestrator's inline fixes are committed with `Co-Authored-By: <reviewer>` style attribution; follow-up tasks carry the provenance fields (`discovered_from=<task> discovered_by=reviewer discovered_class=<class>`) and are created in-change, since the defect blocks the change.

**A fix for a defect the Reviewer demonstrated on a real surface is pinned on that surface.** When the finding's `evidence` is a probe of a real process, daemon, client, `git` or file system, the regression test for its fix (inline fix, re-spawned Implementor, or follow-up task) drives that same surface and reproduces the probe: it fails before the fix and passes after. A test against a fake or stub of the surface is not the pin. Where the test tree cannot drive the surface, the fix commit and the task's `## Observations` say so; a fake is not substituted silently. The record: in a replay, a Reviewer found two defects by probing real Emacs daemons, and the rework pinned them with four specs against a fake `emacsclient`; the original run pinned the same defect with the real one, without a rule.

When all inline fixes are applied: `state.py task set <name> done`. Follow-up tasks it created go in `followups_created=[...]` on the same command.

### 9. Asks raised during execute

Every ask that arises while agents run (a `spec-signal`, an Implementor that stops to ask, an orchestrator-side discovery) is a file at once: `state.py record add asks '{...}'` per `templates/ask.md` writes `inbox/<id>.org` and returns the id; commit it in the home with a subject naming the id. The record shows a 35-minute stall on a question that was not needed until the next wave, and a 950-minute one overnight on a question that blocked nothing. So:

- The blocking call is made only under the inline rule (`templates/ask.md` § The inline rule; in execute, an ask that blocks a merge in this batch or a task not yet spawned, with the user present). The call is the dispatcher's (`SKILL.md` § Two contexts): the phase agent writes the file, first finishes the orchestrator-side work that does not depend on it (spawn Reviewers for merged tasks, report filing), and when nothing else can run hands back `stopped` naming the ask; the dispatcher asks and resumes the phase agent with the answer in the file. `environment` asks (a regression stop, a wedged runner) always take that route; when the user is absent the file is what the next session finds.
- Otherwise the file is the ask. Each task named in `blocks` gets `state.py task set <name> blocked blocker_note=<ask-id>`; the merge chain continues with the tasks behind it; the orchestrator finishes everything that does not wait on it. When nothing else can run, it does not wait for the answer: `blocked` counts as stopped, so if `gate check execute` is all true it passes the gate and goes on to integrate, where § 3a presents the ask and the blocked task is carried in the handshake for the next plan (`task set <name> ready` happens in whichever phase the answer is applied). Only when the gate cannot pass does it stop, with a one-line pointer to the file. A non-blocking ask needs no pointer; `status` lists it at the next sync point and integrate's § 3a presents it.
- An Implementor blocked on a wrong premise (`roles/implementor.md` § Premises) is not an ask: the orchestrator rewrites the task as plan § 3 would (the rewrite is the correction; a stale seam row is a record correction), journals `state.py note note "premise-wrong: <task>: <claim>"`, and spawns a fresh Implementor in the same worktree. A rewrite that needs the user's choice is a `decision` file under the rule above.
- An Implementor that stops to ask (`roles/implementor.md` § Escalation contract) leaves its task blocked (`state.py task set <name> blocked blocker_note=<ask-id>`) and its worktree retained. `blocked` counts as stopped for the exit gate, so `no_orphan_in_progress` still passes; `task set <name> ready` when the ask is applied.
- Answers arrive through the file. `state.py inbox` runs at the sync points (`templates/ask.md` § Sync points) and whenever the merge chain pauses (§ 3); an answered file is handled before anything else (`templates/ask.md` § Answer protocol), and a matched option that unblocks a task is applied and committed in the home with the same subject as the merge it releases in the code root (one commit when they are the same repo).

### Productive tension resolution

Per `roles/reviewer.md`, the Reviewer may flag a choice the Implementor had a good but invisible reason for. The orchestrator (which holds the Implementor's report and the Reviewer's findings) resolves:

- If the reasoning was wrong: act on the flag (treat as ordinary finding).
- If the reasoning was load-bearing-but-undocumented: codify it as a comment, a test, or a seam row's statement — so the next reviewer doesn't flag it again. `state.py note push-back "<one line>" --ref <task>` records that it happened.

The Reviewer never has to know which path was taken.

## Inputs (from plan)

- The composed batch and its tasks with briefs.
- The change's `design.org` (the cited seam rows) and the cited scenarios in `docs/specs/`.
- `phase_gates.plan.passed: true` (mandatory; `state.py phase set execute` enforces it).

## Exit gate

| Check | Condition |
|---|---|
| `all_tasks_executed_or_stopped` | Every task in the batch is either `done` or explicitly `failed` / `externalised` / `blocked` |
| `all_reviews_completed` | Every task that reached `reviewed` / `done` with a `review_mode` has its findings file on disk |
| `no_orphan_in_progress` | No task is stuck in `setup_complete` / `in_progress` / `completed` / `needs_review` / `reviewed` |

All three are computed: `state.py gate check execute` shows them with reasons, `state.py gate pass execute` sets the gate, and `state.py phase set integrate` advances. Integrate refuses to run otherwise.

**Commit** at execute close, in the home: `git -C "$ORCH_HOME" add .orchestrator && git -C "$ORCH_HOME" commit -m "Orchestrator: <cycle-id> execute closed"` (add `.tasks` too when the cycle touched it; `state.json`, the briefs, reports, reviews and findings, the task files' mirrored frontmatter and appended sections; the after-files are ignored by `.orchestrator/.gitignore`). Ask files were committed when created and when applied (`templates/ask.md` § Commit rule).

## What execute does **not** do

- Execute does not run the Architect. (Conformance is integrate's job; the premise check is plan's.)
- Execute does not produce the PM digest. (Integrate.)
- Execute does not amend `design.org` or `docs/specs/`. (Integrate's conformance check.)
- Execute does not modify the intent's status line. (Integrate, via goal-drift handling and § 9.)

These are all integrate-phase operations; conflating them into execute is what the brainstorm's three-phase model exists to prevent.
