# Execute phase

The bridge. Design becomes diff; diff becomes discovery.

A task is "executed" only when **both** implement and review have completed on it. Implement-without-review is in-flight, not done.

## Operations

### 1. Implement (per task, in worktree)

For each task in the batch, the orchestrator:

1. Captures a baseline test snapshot if one isn't already taken for this cycle: `$TEST_CMD` (the overlay's `test.command`, which `home.py where` prints for the change's repository) run in the code root (`(cd "$CODE_ROOT" && $TEST_CMD) > "$ORCH_CHANGE_DIR/baseline-<cycle-id>.txt" 2>&1; echo $? > "$ORCH_CHANGE_DIR/baseline-<cycle-id>.status"`); the file is the baseline and its exit code is `BASELINE_STATUS` in § 4, where `$TEST_COMPARE` is the overlay's `test.compare` for the repository. A non-zero status with no `test.compare` in the overlay is an `environment` ask before the first merge, since § 4's rule cannot fire. `$ORCH_HOME` and `$CODE_ROOT` come from `eval "$(home.py where --sh)"` (§ 3); the orchestrator's own cwd stays the home, and every git or test command that belongs in the code root says so with `-C "$CODE_ROOT"` or a `cd` in a subshell.
2. Creates a worktree on the branch `$BRANCH_NAME=task-<task-name>-<ts>` at the path the overlay's worktree placement gives it, beside the change's own worktree and never inside it (`overlay.md` § Worktrees): `eval "$(home.py worktree "$BRANCH_NAME" --sh)"` sets `$WORKTREE_PATH`.
   - Uses `git -C "$CODE_ROOT" worktree add "$WORKTREE_PATH" -b "$BRANCH_NAME"`: the branch starts from the change integration branch, the code root's HEAD.
   - `-C "$CODE_ROOT"` names the repository; the path is under the home's worktrees folder.
3. Runs the overlay's `worktree.init` hook if defined.
4. Runs `state.py check start <task-name>`, which refuses while a task in its `blocked_by` has no merge commit on the change branch, then spawns an Implementor agent (`Agent` tool, `subagent_type: general-purpose`, description `implementor: <task-name>`) at the task's agent setting: the task file's `agent.implementor` when the plan wrote a task override (`templates/task-body.md`), otherwise the role default, `model: opus`, `effort: medium`. Both are passed on every call; the description is how `report.py` knows the agent's role and task (`report.md`). The call returns at once and the agent runs concurrently with the orchestrator and the other agents; its completion arrives as a notification. No polling, no `sleep`: when nothing can run, the orchestrator ends its turn and is woken by the hand-back.
5. Hands the agent its brief, `<change-dir>/cycles/<cycle-id>/briefs/<task-name>.md`, which plan assembled and wrote (`flows/plan.md` § 5), so the review and the audit trail see what the Implementor saw. The brief names the report path, `$ORCH_CHANGE_DIR/cycles/<cycle-id>/reports/<task-name>.md` (the orchestrator creates the directory), and the task's scratch directory, `<task-name>/` under the session's scratch directory, a Reviewer's `review-<task-name>/` (`roles/implementor.md` § A machine shared with other agents); the Implementor writes its report there and hands back one line (`SKILL.md` § Hand-back lines).
6. Sets the task file's `status: in_progress`.

When an agent completes, the orchestrator reads its one line and verifies at least one commit landed on the worktree branch (`git -C "$CODE_ROOT" rev-parse "$BRANCH_NAME"` against the sha in the line). No commits → `status: failed` in the task file (worktree retained for debugging). The report file is read at integrate; in execute the orchestrator opens it only when the line carries `deviated` or `candidates=<n>`.

While agents run, the orchestrator does the work that does not depend on the ones still running: verify each completed commit, save its report, merge whatever has landed (§ 3), spawn Reviewers (§ 7). A task whose `blocked_by` tasks have not all merged is spawned when they have; `check start` says when.

The orchestrator never adds scope to a running Implementor by message. New work is a task of its own, drafted and premise-checked at the next plan, or an inline fix after review (§ 8). The record: scope sent to a running Implementor by `SendMessage` without a premise check was blocked by its Reviewer, cost a rework and a re-review, and then a one-task cycle of its own (24M tokens, 57 minutes).

### 2. No Architect run in execute

Nothing runs between an Implementor's commit and its merge. The Reviewer (§ 7) reads the cited seam rows and may probe the merge candidate; that is the check the old execute-time Architect run performed, and the useful catches in the record came from the probes, not the trigger. Restore an execute-time Architect run only on a demonstrated Reviewer miss that a seam-scoped run would have caught (`roles/architect.md` § Three runs).

### 3. Merge each task as it lands

Merges are not held for the batch. When an Implementor completes with a commit, its task joins the merge queue at once. The queue merges one task at a time, in the order tasks landed, while the other Implementors keep running; the chain is serial because there is one change integration branch. A task that merges and passes § 4 goes straight on through § 5-7 (its Reviewer is spawned), and the chain moves to the next queued task without waiting for that review. A task held by a blocking ask (§ 9) stays out of the queue; the tasks behind it merge. When the chain pauses (nothing queued, agents still running), the orchestrator runs `asks.py inbox` and handles an answered file before it waits again (§ 9). The record shows why: a batch that waited for all seven Implementors made the first-finished task wait 10 minutes for its merge and 12 for its Reviewer, where batches merged as they landed spawned each Reviewer 0-3 minutes after its Implementor ended.

```bash
eval "$(python3 ~/.claude/skills/orchestrate/scripts/home.py where --sh)"   # ORCH_HOME, ORCH_CHANGE, ORCH_CHANGE_DIR, CODE_ROOT, …
git -C "$CODE_ROOT" merge --no-ff "$BRANCH_NAME" -m "Merge task $TASK_NAME: $DESCRIPTION"
MERGE_COMMIT=$(git -C "$CODE_ROOT" rev-parse HEAD)
```

The subject is the record of the merge: `Merge task <name>` (and `Merge task <name> (rework)` for a rework, § 8), with the description after the colon; `state.py check done` and the execute gate read the task's merge from it, and a merge in another form is a convention break that fails them (`state.md` § Conventions). `MERGE_COMMIT` goes to the Reviewer (§ 7). **Conflicts**: `git merge --abort`, `status: blocked` in the task file and `state.py blocker <name> merge-conflict`, keep worktree, continue with next task.

### 4. Test after each merge

```bash
MERGE=$(git -C "$CODE_ROOT" rev-parse --short=12 HEAD)          # the `Merge task <name>` commit
AFTER="$ORCH_CHANGE_DIR/after-${TASK_NAME}-${MERGE}"
(cd "$CODE_ROOT" && $TEST_CMD) > "$AFTER.txt" 2>&1
AFTER_STATUS=$?
echo $AFTER_STATUS > "$AFTER.status"
# red baseline only: the comparison's verdict beside the run
[ "$BASELINE_STATUS" -ne 0 ] && { (cd "$CODE_ROOT" && $TEST_COMPARE "$ORCH_CHANGE_DIR/baseline-<cycle-id>.txt" "$AFTER.txt"); echo $? > "$AFTER.compare"; }
```

The command runs in the code root; the after-file lives in the change's directory at the home, named for the task and its merge commit, with the exit code beside it and, against a red baseline, `test.compare`'s exit code. These three files are the record that the tests pass at the merge: `state.py check done` reads them and refuses a task whose run is missing, fails on a green baseline, or has no comparison or a non-zero one on a red baseline (`state.md` § Verbs). A rework's merge gets its own run. A regression is a test that passed in the baseline and fails after the merge. With a green baseline (`BASELINE_STATUS == 0`) the exit code decides: `AFTER_STATUS != 0` is a regression. With a red baseline the exit code cannot decide (it is non-zero both times), so the check is per test id: the overlay's `test.compare <baseline-file> <after-file>` (`overlay.md`; the skill ships `scripts/compare.py --parser <test.parser>`, which reads `emacs-results-txt`, `junit-xml` and `tap`), run in the code root like the test command with the two files' absolute paths, which exits non-zero and prints the ids that failed after but not before, and exits 2 when a file holds no run it can read, which is not a verdict; a project whose runner gives ids and whose overlay has no `test.compare` gets an `environment` ask at the first red baseline (§ 1), since the rule below cannot fire. The record: the rule as text alone did not fire in a replay, and the agent ran the overlay's script by hand. The record: one project's baseline carried 13 known failures for every cycle, the exit-code rule never fired, and the set comparison the overlay improvised was patched mid-cycle. A count without names is no verdict either: when the tool's `not passing` count exceeds the failures it names (or one fixed and one new failure leave the count unchanged while `new: 0`), the orchestrator diffs the two files' failures by hand before trusting the line. The record: `compare.py` printed NO-REGRESSION after a merge that broke a spec whose detail block ended in a traceback and carried no `FAILED:` line; the Reviewer found it by running the suite, and the merge chain went on (package-boundaries `cycle-1792083023`; the parser now reads those blocks). When a regression is found: `state.py note discovery "regression: <task>" --ref <task> --path <the after-file>`; stop further merges; keep worktrees; raise an `environment`-kind ask (`templates/ask.md`) with the after-file paths. It blocks the merge chain, so it is asked at once (§ 9). Only merging stops: Implementors still running finish and join the queue, and Reviewers already spawned carry on.

### 5. Capture orchestrator-side discoveries

After each merge, before that task's review, the orchestrator scans for discoveries no individual agent owns. Per the brainstorm and lifted from VCE's §A.7.5:

- **Latent bugs surfaced by a regression** — a test broke not because the merging task was wrong but because it perturbed a pre-existing fragile assumption (e.g. a non-stable sort coupled to insertion order).
- **Worker discoveries and observations on the merged task body** — route each `## Discoveries` entry the report lists under `candidate_tasks` by `templates/architect-finding.md` § Routing: on the change's main path, it is fixed in the change. A simple fix (one file, no seam's owning symbols, a verification whose output is predictable: the inline path's triage, `flows/inline.md` § When to take the inline path) is an inline fix under § 8's rules: reproduced first by a test or probe that fails on the defect, pinned on the surface its evidence used, made while no after-test runs in that checkout, and journaled `state.py note inline-fix "<what>" --ref <task>`. Any other fix is a follow-up task, which starts with the reproduction: a candidate (`critical: true`, `source: <task>/discovery-<n>`, `discovered_by: implementor`) that the next plan writes. The inline fix keeps a one-line defect out of the backlog; the record: over four homes and 68 cycles, 231 inline fixes against 37 follow-up tasks; an untrue sentence in prose is a text correction, made inline; otherwise it is an entry on the hardening list (`templates/hardening.md`). Scan `## Observations` too: one that names a defect in code or text that exists is routed the same way, as if it were a discovery; a `thought:` line and the rest stay in the body as the record. The Reviewer does not see either (§ 7).
- **Manual conflict-resolution decisions** — when the orchestrator dropped, restructured, or regenerated code while reconciling two branches, that decision is an unreviewed structural change, journaled as a `note decision` (below). If it touched a contract or dropped a test, that is a defect in merged code: a follow-up candidate, routed like a finding.
- **Aborted merges where the abort reason is itself the finding** — journal the structural issue (not just the merge failure) as a `note discovery`; a defect it names is routed like a finding (`templates/architect-finding.md` § Routing), so the next cycle can address it.

Each discovery is one `state.py note discovery "<one line>" --ref <task>` (at most 280 characters, or the tool refuses `too_long`; with `--path` when a report or a file holds the detail), naming where it was routed (`fixed: <task or inline fix>`, `hardening`); a conflict-resolution decision is `note decision`; a scan that found nothing is one `note note "scanned <what>; nothing"`. The journal (the event log's `note` entries) is where these are recorded. A journal line is never where a defect ends: it ends in a fix, a task, an ask or a hardening-list entry. A follow-up is not written here: it is one element appended to `<change-dir>/cycles/<cycle-id>/candidate-tasks.json`, the source's words verbatim as its `quote` (the shape is in `flows/integrate.md` § 7), and the next plan writes the task (`externalisation.md` § Who writes a task file). The phase agent runs at a coordinating setting, not the one task text is written at (`SKILL.md` § Agent settings).

### 6. Flip to `needs_review` (NOT `done`)

After successful merge + tests pass + no regression, the task is **not yet** closed: the orchestrator sets the task file's `status: needs_review` (the file stays in `tasks/open/`). Its merge commit is the `Merge task <name>` commit (§ 3), read from git and written nowhere else; the Implementor's structured report is at `<change-dir>/cycles/<cycle-id>/reports/<task-name>.md`, the path its brief named. The task file lives at the home (`$ORCH_HOME/.orchestrator/changes/<change>/tasks/open/<task>.md`), not in the worktree: the Implementor is given its absolute path and appends `## Observations` and `## Discoveries` there; the orchestrator commits the task file (its status and those sections) in the home at execute close with the other artifacts, as SKILL.md says. That holds when the home is the code root too: the Implementor writes the main checkout's copy by its absolute path, never a worktree's, and commits only code. With a split, the Reviewer's diff (`git diff <merge-base>..<merge_commit>` in the code root) cannot contain the Observations hunk, which closes the author-blind leak the record noted.

Then remove the worktree:

```bash
git -C "$CODE_ROOT" worktree remove "$WORKTREE_PATH"
git -C "$CODE_ROOT" branch -D "$BRANCH_NAME"   # optional; tidies local branch list
```

### 7. Review (author-blind, per task)

Once a task is `needs_review`, the orchestrator spawns a Reviewer at the role default, `model: opus`, `effort: high`, or at the task file's `agent.reviewer` when the plan raised it, with the description `reviewer-task: <task-name>`. Its brief is saved as `briefs/review-<task-name>.md`. **The author-blind constraint is this section's prose, which the phase agent follows when it builds the Reviewer's input; no code checks it.**

#### Reviewer input contract

The orchestrator builds the Reviewer's input from a **fixed set of sources**, none of which are the Implementor's:

```yaml
reviewer_input:
  diff: $(git diff <merge-base>..<merge_commit>)
  task_brief: <full text of $ORCH_HOME/.orchestrator/changes/<change>/tasks/open/<task-name>.md, read from the home, EXCLUDING ## Observations and ## Discoveries sections>
  cited_seams: <the design.org Seams rows named in cites_seams, verbatim>
  cited_scenarios: <for each scenario in the task's cites_scenarios, its text from docs/specs/ at merge_commit and its coverage.py --scenario row>
  project_standards: <overlay's roles/reviewer.md, if present>
  project_priors: <overlay's priors.md, if present>
```

The input **must not**:
- Read or pass the Implementor's structured report.
- Read or pass the `## Observations` or `## Discoveries` sections.
- Pass any indicator of the Implementor's identity.
- Allow the Reviewer's worktree to contain any file other than the clean checkout of `merge_commit`.
- Pass any other in-flight diffs.

The orchestrator holds every one of these sources and builds the input from the list above and nothing else; passing any of the excluded ones is a process bug, not a judgment call.

#### Delegated review, always

Every review is delegated: a separate general-purpose Agent per task, at the Reviewer's setting above. The Reviewer writes its findings file and hands back one line with the counts (`SKILL.md` § Hand-back lines); the orchestrator opens the file when the count is not zero. The execute phase agent runs on Sonnet (`SKILL.md` § Agent settings), so a review in its own context would judge code below the Reviewer's setting; inline review, once the default for a batch of two or fewer, is retired.

The input contract is author-blind. The Reviewer may run the test command and its own probes read-only in the scratch directory its brief names (`review-<task-name>/`, § 1) against the `merge_commit` checkout (`roles/reviewer.md` § Probes); the probes' output goes in the findings file, never into the repository.

### 8. Handle review findings

The Reviewer's findings file lands at `<change-dir>/cycles/<cycle-id>/reviews/<task-name>.md`, its frontmatter naming the `merge_commit` it reviewed (`roles/reviewer.md` § Output contract); that file is the record that the task was reviewed. Then the orchestrator processes:

Each finding of kind `issue` or `question` is routed by `templates/architect-finding.md` § Routing, reading the intent's goals for the main path. The `## Thoughts` lines are read with them, each asked the question `templates/architect-finding.md` § Kind draws between `question` and `thought`: does the harm need something to change first? A thought that names a wrong result on an input the code accepts today is routed as an `issue`, with one journal line `state.py note note "rerouted-thought: <review file> <line>"`; the rest stay in the file. The record: over the 56 cycles on `cf2577d` the defects no route carried were thoughts, in all four homes (a Reviewer's probe of half-given credentials that build a read-only client, a traceback where an error line was due, a void function), because routing never read them.

| Route | Orchestrator action |
|---|---|
| `blocking` (main path, or an untrue sentence) | Reproduce, then fix. The fix starts with a test or probe that fails on the defect, then makes it pass: an inline fix (`state.py note inline-fix "<what>" --ref <task>`, committed `Inline fix (review <task> #<n>)`), a re-spawned Implementor with fix scope (the rework: its brief is `briefs/<task>-rework.md`, which is the record that the review sent the task back; `status: in_progress`; it runs at the task's setting from § 1, never a stronger one, with the description `implementor-rework: <task-name>`; its merge is `Merge task <name> (rework)` and its review `reviews/<task>-rework.md`, naming the rework's merge commit), or a revert of the merge. The task does not advance to `done` until it is fixed. A `question` whose reproduction does not fail goes on the hardening list with what was tried; an untrue sentence needs no failing test, only the corrected text. |
| `hardening` (any other defect) | One entry on the change's hardening list (`templates/hardening.md`), with `:REF:` the review file and finding number. The task can advance to `done`. |
| `spec-signal` direction | Write an ask file at once (`templates/ask.md`; `:RAISED_BY: reviewer`, `:REF:` the review file's path, since Reviewer findings carry no id); presented inline only under § 9's rule, otherwise listed at integrate. Does not block the task itself; it signals that the design may need revision. It is the ask even when the orchestrator knows the answer: a step written into a later task's body in its place is not reviewed by anyone (the record: one spec-signal became step 5 of the next task, whose Reviewer then read a brief no premise check had seen). When the finding carries a `stopgap` (an `issue`, `roles/reviewer.md` § Routing of a finding), apply it as an inline fix before the task is `done`, pinned on the surface the probe used (below), and name it in the ask file; a stopgap that would answer the ask is not applied and the ask says so |

An inline fix in the code root waits for a running after-test in that checkout to finish, or is made and tested in the review checkout; the record: a fix committed at 18:02 while an after-test ran in the same checkout from 18:01 to 18:04, so the after-file is of a mixture. To check something without its fix, the orchestrator sets work aside with a commit and never with `git stash`, whose one list every worktree of the repository shares (`roles/implementor.md` § A machine shared with other agents); the record: an inline-fix check popped another task's stash. The orchestrator's inline fixes are committed with the subject `Inline fix (review <task> #<n>)`, `<n>` the finding's number in the review file (a `: <one line>` may follow; a commit that cannot carry it is linked with `state.py fix-link <commit> --task <task> --review <n>`), and `Co-Authored-By: <reviewer>` style attribution; a follow-up is a candidate (§ 5) with `source: <review file>#F<n>`, `discovered_by: reviewer`, its class and `placement: in-change`, since the defect blocks the change.

**A fix for a defect the Reviewer demonstrated on a real surface is pinned on that surface.** When the finding's `evidence` is a probe of a real process, daemon, client, `git` or file system, the regression test for its fix (inline fix, re-spawned Implementor, or follow-up candidate) drives that same surface and reproduces the probe: it fails before the fix and passes after. A test against a fake or stub of the surface is not the pin. Where the test tree cannot drive the surface, the fix commit and the task's `## Observations` say so; a fake is not substituted silently. The record: in a replay, a Reviewer found two defects by probing real Emacs daemons, and the rework pinned them with four specs against a fake `emacsclient`; the original run pinned the same defect with the real one, without a rule.

When all inline fixes are applied: `state.py check done <name>`, which refuses unless the task's latest `Merge task <name>` commit is on the change branch and a review file names it; then `status: done` in the task file. The candidates it routed are in `candidate-tasks.json` with the review as their `source`; plan writes them.

### 9. Asks raised during execute

Every ask that arises while agents run (a `spec-signal`, an Implementor that stops to ask, an orchestrator-side discovery) is a file at once: `asks.py add '{...}'` per `templates/ask.md` writes `inbox/<id>.org` and returns the id; commit it in the home with a subject naming the id. The record shows a 35-minute stall on a question that was not needed until the next wave, and a 950-minute one overnight on a question that blocked nothing. So:

- The blocking call is made only under the inline rule (`templates/ask.md` § The inline rule; in execute, an ask that blocks a merge in this batch or a task not yet spawned, with the user present). The call is the dispatcher's (`SKILL.md` § Two contexts): the phase agent writes the file, first finishes the orchestrator-side work that does not depend on it (spawn Reviewers for merged tasks, report filing), and when nothing else can run hands back `stopped` naming the ask; the dispatcher asks and resumes the phase agent with the answer in the file. `environment` asks (a regression stop, a wedged runner) always take that route; when the user is absent the file is what the next session finds.
- Otherwise the file is the ask. Each task named in `blocks` gets `status: blocked` in its file and `state.py blocker <name> <ask-id>`; the merge chain continues with the tasks behind it; the orchestrator finishes everything that does not wait on it. When nothing else can run, it does not wait for the answer: `blocked` counts as stopped, so if `state.py gate execute` passes it closes execute and goes on to integrate, where § 3a presents the ask and the blocked task is carried in the handshake for the next plan (`status: ready` is written in whichever phase the answer is applied). Only when the gate cannot pass does it stop, with a one-line pointer to the file. A non-blocking ask needs no pointer; `status` lists it at the next sync point and integrate's § 3a presents it.
- An Implementor blocked on a wrong premise (`roles/implementor.md` § Premises) is not an ask, and execute does not rewrite the task: it journals `state.py note note "premise-wrong: <task>: <claim>" --path <the report>`, sets the task `status: blocked` with `state.py blocker <task> premise-wrong`, and leaves its worktree; integrate § 7 records the claim as the task's impact and the next plan rewrites it (`flows/plan.md` § 2), which the Architect checks (§ 3) before a fresh Implementor, described `implementor-rework: <task-name>`, runs it. A stale seam row is a record correction, made now. A rewrite that needs the user's choice is a `decision` file under the rule above.
- An Implementor that stops to ask (`roles/implementor.md` § Escalation contract) leaves its task blocked (`status: blocked`, `state.py blocker <name> <ask-id>`) and its worktree retained. `blocked` counts as stopped for the exit gate, so `batch_stopped` still passes; `status: ready` when the ask is applied.
- Answers arrive through the file. `asks.py inbox` runs at the sync points (`templates/ask.md` § Sync points) and whenever the merge chain pauses (§ 3); an answered file is handled before anything else (`templates/ask.md` § Answer protocol), and a matched option that unblocks a task is applied and committed in the home with the same subject as the merge it releases in the code root (one commit when they are the same repo).

### Productive tension resolution

Per `roles/reviewer.md`, the Reviewer may flag a choice the Implementor had a good but invisible reason for. The orchestrator (which holds the Implementor's report and the Reviewer's findings) resolves:

- If the reasoning was wrong: act on the flag (treat as ordinary finding).
- If the reasoning was load-bearing-but-undocumented: codify it as a comment, a test, or a seam row's statement — so the next reviewer doesn't flag it again. `state.py note push-back "<one line>" --ref <task>` records that it happened.

The Reviewer never has to know which path was taken.

### Handing over

The dispatcher asks a phase agent to hand over when its context passes the overlay's limit (`SKILL.md` § The dispatcher's loop). Every phase hands over this way; execute is where it happens most. The agent:

1. Starts nothing new: no Implementor, Reviewer, Architect run or merge not already under way.
2. Reaches a clean point: every worker it spawned has reported, the merge queue is empty (each landed task merged and tested, § 3-4, or held by an ask), and each task file's `status:` says where the task is.
3. Writes the handover, `<change-dir>/cycles/<cycle-id>/<phase>-handover.md` (the design round before any cycle: `<change-dir>/design-handover.md`), under three headings: **Merged** (each task and its merge commit, with its review done or not yet spawned), **Open** (each task not started, blocked or failed, and each ask it raised, by id), **Next** (the flow's steps left, in order, and anything the agent had decided that no file holds). Pointers, not prose: the task files, the event log, the reports and the reviews hold the detail.
4. Commits it in the home with the phase's other artifacts so far, and hands back `<phase> <cycle-id>: stopped, handover <path>`.

The fresh agent runs `state.py status`, reads the handover and its flow doc, and goes on from **Next**. The record: a swap run by hand in the vault (`cycle-1792295307/execute-handover.md`) wrote where things stood, the asks, what was known and not acted on, and the close; it swapped with two workers still running, their hand-backs coming to the dispatcher. Waiting for the clean point keeps each worker's report with the agent that merges it.

## Inputs (from plan)

- The composed batch: the task briefs under `cycles/<cycle-id>/briefs/` and their task files.
- The change's `design.org` (the cited seam rows) and the cited scenarios in `docs/specs/`.
- The `Orchestrator: <cycle-id> plan closed` commit in the home (mandatory; `state.py status` reads the phase from it).

## Exit gate

| Check | Condition |
|---|---|
| `batch_stopped` | Every task in the batch is `done`, `failed`, `externalised` or `blocked`; none is `ready`, `in_progress` or `needs_review` |
| `done_tasks_merged_and_reviewed` | Every `done` task in the batch passes `check done`: its `Merge task <name>` commit is on the change branch and a review file names it |
| `blocked_by_order` | Every merged task in the batch merged after each task in its `blocked_by` |

All three are computed, afresh at every report: `state.py gate execute` shows them with reasons. When it passes, execute closes with the commit below, and the cycle is in integrate.

**Commit** at execute close, in the home: `git -C "$ORCH_HOME" add .orchestrator && git -C "$ORCH_HOME" commit -m "Orchestrator: <cycle-id> execute closed"` (add `.tasks` too when the cycle touched it; the event log, the briefs, reports, reviews and findings, the task files' statuses and appended sections; the after-files are ignored by `.orchestrator/.gitignore`). Ask files were committed when created and when applied (`templates/ask.md` § Commit rule).

## What execute does **not** do

- Execute does not run the Architect. (Conformance is integrate's job; the premise check is plan's.)
- Execute does not amend `design.org` or `docs/specs/`. (Integrate's conformance check.)
- Execute does not modify the intent's status line. (Integrate, via goal-drift handling and § 9.)
- Execute does not write task files. A follow-up is a candidate (§ 5); the next plan writes it (`flows/plan.md` § 2).

The first three are integrate-phase operations; conflating them into execute is what the brainstorm's three-phase model exists to prevent.
