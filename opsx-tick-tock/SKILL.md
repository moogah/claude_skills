---
name: opsx-tick-tock
description: Implement multiple ready tasks in parallel using isolated worktrees and spawned agents (Tick), or review multiple needs-review tasks in parallel (Tock). Use when the user wants to execute ready tasks concurrently with sequential merging, or review a batch of merged tasks before closing them.
---

Orchestrate an OpenSpec change in a **tick-tock** cycle:

- **Tick (implement batch)**: spawn worker agents in isolated worktrees on
  `ready` tasks, merge sequentially with regression tests after each, flip
  task status to `needs-review` (does NOT close the task). Commit.
- **Tock (review batch)**: spawn reviewer agents (parallel, read-only) on
  `needs-review` tasks, each diffing the merge commit against the task
  spec. Findings become new `ready` follow-up tasks with
  `discovered-from:<original>`. Reviewed tasks flip to `done` and move to
  `tasks/closed/`. Commit.

Two orchestrator sessions per cycle. A task is only `done` after a tock
pass, never directly after tick.

## 0. Mode Detection

Scan `openspec/changes/<name>/tasks/open/*.md` frontmatter. Count:
- `ready_count` — tasks with `status: ready`
- `needs_review_count` — tasks with `status: needs-review`
- `blocked_unblockable_count` — `status: blocked` whose `blocked-by` deps
  are all in `tasks/closed/` (these should be flipped to `ready` during
  discovery)

If the user passed `tick` or `tock` (or `review`) as an argument, honor
it. Otherwise:

- Both queues non-empty → **offer tock first** via AskUserQuestion
  (review merged work before piling on more); user may choose tick.
- Only `needs_review_count > 0` → **tock**.
- Only `ready_count > 0` → **tick**.
- Both zero → exit (suggest `/opsx-tasks generate` or `/opsx-archive`).

---

# PART A — TICK MODE

## A.1 Discovery & Selection

After mode detection, build the ready set:
- `status: ready` tasks, plus
- `status: blocked` tasks whose `blocked-by:<dep>` relations are all in
  `tasks/closed/` — flip these to `ready` in their frontmatter before
  selection (commit the flip at the end of the session or inline)

Display overview:
```
Change: <change-name>
Mode: TICK
Ready tasks: 3
  - setup-module — Create module structure
  - write-spec-file — Draft spec.md
  - characterize-current — Capture current behaviour
Blocked: 2 (by setup-module, write-spec-file)
Needs-review (deferred to tock): 0
```

Use **AskUserQuestion**:
- 0 ready tasks: exit
- 1-3 ready: auto-select with confirmation
- 4+ ready: multiSelect, **max 5 per batch**

**Batch strategy**: default to parallel. Sequential mode spawns one agent
at a time.

## A.2 Baseline Capture

Use the project's test command (from `architecture.md`). Capture both
exit status and output.

```bash
REPO_ROOT=$(git rev-parse --show-toplevel)
TIMESTAMP=$(date +%s)
mkdir -p "$REPO_ROOT/.orchestrator"
BASELINE="$REPO_ROOT/.orchestrator/baseline-${TIMESTAMP}.txt"

TEST_CMD="<project test command>"
$TEST_CMD > "$BASELINE" 2>&1
BASELINE_STATUS=$?
```

If the baseline already has failures (non-zero exit), use AskUserQuestion
to confirm proceeding.

## A.3 Worktree & Agent Setup

For each selected task (sequential setup to avoid race conditions):

```bash
REPO_ROOT=$(git rev-parse --show-toplevel)
cd "$REPO_ROOT"

WORKTREE_NAME="task-${TASK_NAME}-$(date +%s)"
WORKTREE_PATH="${REPO_ROOT}/.worktrees/${WORKTREE_NAME}"

git worktree add "$WORKTREE_PATH" -b "$WORKTREE_NAME"
# Per-worktree setup if the project needs it (submodules, etc.)
```

**CRITICAL Worktree Requirements**:
- Always create from main repo root (`cd` to repo first)
- Use absolute paths from `$(git rev-parse --show-toplevel)`
- Never create from within worktrees (causes nesting)
- Siblings, not children: every worktree lives at `.worktrees/task-*`

Ensure `.worktrees/` is gitignored.

**State file** (`.orchestrator/state.json`):
```json
{
  "session_id": "orch-1234567890",
  "type": "tick",
  "repo_root": "<absolute path>",
  "change_name": "<change-name>",
  "baseline_snapshot": ".orchestrator/baseline-1234567890.txt",
  "baseline_status": 0,
  "current_branch": "main",
  "test_command": "<cmd>",
  "tasks": [
    {
      "task_name": "setup-module",
      "task_file": "openspec/changes/<change>/tasks/open/setup-module.md",
      "worktree_path": "<repo>/.worktrees/task-setup-module-1234567890",
      "branch_name": "task-setup-module-1234567890",
      "agent_task_id": "<Agent id>",
      "status": "setup_complete",
      "regression_detected": false,
      "needs_review": false,
      "worktree_removed": false,
      "merge_commit": null
    }
  ]
}
```

**Agent spawning** (use the Agent tool, NOT TaskCreate):

```
Agent(
  subagent_type: "general-purpose",
  description: "Implement task <task-name>",
  prompt: <see Agent Prompt Template>,
  run_in_background: false   # CRITICAL: must be false for file write access
)
```

**NEVER use `run_in_background: true`** — blocks file writes.

### Worker Agent Prompt Template (tick)

```markdown
Implement task <TASK_NAME> for change <CHANGE_NAME> in worktree
<WORKTREE_PATH> (branch: <BRANCH_NAME>).

Read the task body:
<FULL_TASK_FILE_CONTENTS>

**Testing**: Run the verification commands listed in the task. Must pass
before you commit.

**Out-of-scope findings during implementation**: append them to a
`## Observations` section in this task file rather than filing a new
task. The reviewer reads observations alongside the merge and groups
related ones in context. New tasks fragment that context — reserve
them for a user-visible bug or correctness regression that genuinely
can't wait for review.

What belongs in `## Observations`:
- departures from the task body's prescribed approach (and the
  evidence — e.g. "the recipe's claim X turned out to be empirically
  false; verified via probe Y; chose Z instead")
- latent issues you noticed in adjacent code while implementing
  (e.g. "saw an unstable sort one layer up; out of scope for this
  task but worth a follow-up")
- tests that pass but are weakly asserted, mocks that diverge
  from production, fixtures that don't reproduce real-shape inputs
- spec/design contradictions or ambiguities the implementation
  forced you to resolve

**Commit**: "Implement task <TASK_NAME>: <description from frontmatter>"
with Co-Authored-By. The commit MUST include any `## Observations`
appended to the task file. DON'T push/merge/close the task — the
orchestrator handles that.

**Report back** with:
- commit SHA
- one-paragraph summary of what changed
- last 10 lines of the test command output
- one line on observations: "appended N observations" or "none"
- if you filed a new task (rare): name it and the user-visible bug
  or correctness regression that justified bypassing `## Observations`
```

## A.4 Monitor Progress

Poll every 30s with TaskList. When each agent completes, verify at least
one commit landed on the worktree branch. No commits → mark `failed`
(keep worktree for debugging).

## A.5 Sequential Merging

Merge in completion order:

```bash
REPO_ROOT=$(git rev-parse --show-toplevel)
cd "$REPO_ROOT"
git merge --no-ff "$BRANCH_NAME" -m "Merge task $TASK_NAME: $DESCRIPTION"
MERGE_COMMIT=$(git rev-parse HEAD)
```

Record `MERGE_COMMIT` in the state file for this task.

**Conflicts**: `git merge --abort`, mark `merge_conflict`, keep worktree,
continue with next task.

## A.6 Test After Each Merge (mandatory)

```bash
TEST_CMD="<project test command>"
$TEST_CMD > "$REPO_ROOT/.orchestrator/after-${TASK_NAME}-$(date +%s).txt" 2>&1
AFTER_STATUS=$?

if [ "$AFTER_STATUS" -ne 0 ] && [ "$BASELINE_STATUS" -eq 0 ]; then
  echo "REGRESSION after merge $TASK_NAME"
  # stop further merges; keep worktrees; ask user
fi
```

## A.7 Flip to needs-review (NOT done)

After successful merge + tests pass + no regression, the task is NOT yet
closed. Flip it to `needs-review` in its frontmatter, leaving the file in
`tasks/open/`:

```yaml
---
name: <task-name>
description: ...
change: <change>
status: needs-review            # was: ready
relations:
  - blocked-by:...              # unchanged
merge_commit: <SHA>             # NEW — records what tock will diff
---
```

Then remove the worktree:

```bash
git worktree remove "$WORKTREE_PATH"
git branch -D "$BRANCH_NAME"   # optional; keeps local branch list tidy
```

**Do NOT**:
- move the file to `tasks/closed/`
- set `status: done`
- re-evaluate downstream `blocked-by` relations (those resolve only on
  `done`, which happens in tock)

**Keep worktrees if**: merge conflict, agent failed, or regression
detected.

## A.7.5 Capture orchestrator-side discoveries (mandatory)

Tick can surface findings that no individual worker agent owns:
regression chasing reveals a latent bug in unrelated code, manual
conflict resolution makes structural decisions that were not in either
agent's task body, worker reports flag deviations from the task spec
that imply the spec is wrong. **These belong in the follow-up queue,
captured before the batch closes.** Once the tick session ends, that
signal lives only in the batch commit message and the orchestrator's
volatile context — neither of which tock will mine reliably from a
cold start.

**The bar**: would a tock reviewer have to rediscover this from cold,
working only from the merged diff and the original task spec? If yes,
write it down now.

**Categories that warrant capture**:

- **Latent bugs surfaced by a regression** — a test broke not because
  the merging task was wrong but because it perturbed a pre-existing
  fragile assumption (e.g. a non-stable sort coupled to insertion
  order). Capture even if the immediate regression got resolved by
  reverting the offending merge or by a later merge in the batch
  making the collision moot — the underlying issue persists and will
  re-surface against a richer fixture.

- **Worker observations on the merged task body** — workers append
  out-of-scope findings to the merged task's `## Observations` section
  directly (per the worker prompt). The orchestrator's job is light:
  scan that section and decide whether any single observation rises
  to the bar for its own follow-up task — a user-visible bug, a
  correctness regression, or a spec contradiction big enough to
  warrant separate review. Most observations stay in the task body
  for the reviewer to read in context and group; that is the whole
  point of having them on the task file rather than as new tasks.

- **Manual conflict-resolution decisions** — when the orchestrator
  itself dropped, restructured, or regenerated code while reconciling
  two branches (because git couldn't decide), that decision is an
  unreviewed structural change. Note it in the merged task's body
  with the affected files and the rationale; if the decision warrants
  its own review (touched a contract, dropped a test, reshaped a
  fixture), open a follow-up task.

- **Aborted merges where the abort reason is itself the finding** —
  a task that couldn't merge because chasing its regression revealed
  a deeper structural issue is signal that the structural issue is
  blocking. Capture the structural issue (not just the merge failure)
  as a `ready` task so the next tick can address it.

**Where to record**:

- **New follow-up tasks**: `openspec/changes/<change>/tasks/open/<finding>.md`
  with `status: ready`, `discovered-from:<originating-task>`
  provenance (mandatory), and a Context section linking to the
  `.orchestrator/after-*.txt` or `.orchestrator/state-*.json` traces
  so the trail back to the surfacing event is preserved.
- **Annotations on merged tasks**: where the discovery is small
  enough to read alongside the reviewed task (a manual
  conflict-resolution decision, an agent deviation worth flagging
  for tock), append a `## Tick-time notes` section to the merged
  task's body before flipping its frontmatter to `needs-review`.

**Timing**: do this in the tick session, **before A.8's final
commit**. The tick batch commit should include the new task files and
any Tick-time-notes annotations, so the audit trail is one atomic
landing.

## A.8 Final Snapshot & Commit

After the last merge, run the test command again → save to
`.orchestrator/final-${SESSION_ID}.txt`. Commit the needs-review
frontmatter flips for this batch:

```
git commit -m "Tick batch <session_id>: <N> tasks flipped to needs-review"
```

## A.9 Summary Report (tick)

```
Tick Batch Complete ✓

Merged 3/3 tasks → needs-review
  - setup-module (merge a1b2c3d)
  - write-spec-file (merge e4f5g6h)
  - characterize-current (merge i7j8k9l)

Discoveries captured (new ready tasks):
  - flag-flap-on-empty-input — latent bug surfaced when chasing the
    setup-module regression (discovered-from:setup-module)
  - characterization-fixture-too-weak — characterize-current's
    agent flagged that the synthetic fixture doesn't reproduce
    real-shape inputs (discovered-from:characterize-current)

Tick-time notes appended to merged tasks: 1
  - write-spec-file — manual conflict resolution dropped its
    parser-edge-case test (deduped against characterize-current's
    coverage); flagged for tock to confirm the dedup was correct.

Aborted: 0

Next step: run a tock session to review these and close them.
  /opsx-tick-tock tock
```

If no discoveries surfaced (the batch was clean), say so explicitly
in the report — silence is ambiguous; "no discoveries this batch"
tells the next reader the orchestrator looked and found nothing.

---

# PART B — TOCK MODE

## B.1 Discovery & Selection

Scan `openspec/changes/<name>/tasks/open/*.md` for `status: needs-review`.
Each should have a `merge_commit` field in frontmatter (added by tick).
If missing, try to recover from the most recent `.orchestrator/state*.json`
by matching task_name; if still missing, ask the user.

Display overview:
```
Change: <change-name>
Mode: TOCK
Needs-review: 3
  - setup-module (merge a1b2c3d)
  - write-spec-file (merge e4f5g6h)
  - characterize-current (merge i7j8k9l)
```

Use AskUserQuestion:
- 1-3 needs-review: auto-select with confirmation
- 4+: multiSelect, **max 5 per batch**

## B.2 Review Setup

Reviewers are read-only: each examines a fixed merge commit against a
fixed task spec and writes a findings file. Reviews don't interact —
diffs are pinned to merge commits, not moving HEAD — so spawn all
reviewers at once in a single message (N parallel Agent calls) and wait
for every findings file to exist before handling any of them.

Once findings are in, make a best effort to spot overlap or conflict
between tasks across the batch: two reviews citing the same follow-up,
a finding in one review that reshapes another's scope, or two reviews
proposing inline fixes to the same file. Handle overlapping tasks
together — dedupe shared follow-ups, resolve the scope collision before
either task's handling. Non-overlapping tasks can be handled in any
order.

B.4 governs the inline-fix vs follow-up-task decision once findings
are in hand.

Per batch, the user chooses via AskUserQuestion:
- **Inline review** — orchestrator reads the diff, runs tests, and
  applies the reviewer mindset in its own context. Always sequential
  (one review at a time). Suited to 1-2 small reviews or when the user
  wants to ride along closely.
- **Delegated review** — spawn a general-purpose Agent per task with
  the reviewer prompt (below). Suited to larger diffs or batches where
  the orchestrator's context would bloat.

Default: delegated if batch ≥ 3; inline otherwise.

**State file** (`.orchestrator/state.json`):
```json
{
  "session_id": "tock-1234567890",
  "type": "tock",
  "repo_root": "<absolute path>",
  "change_name": "<change-name>",
  "current_branch": "main",
  "review_mode": "delegated",
  "tasks": [
    {
      "task_name": "setup-module",
      "task_file": "openspec/changes/<change>/tasks/open/setup-module.md",
      "merge_commit": "a1b2c3d",
      "agent_task_id": "<Agent id or null for inline>",
      "status": "review_in_progress",
      "findings_path": ".orchestrator/review-setup-module-<ts>.md",
      "findings_count": null,
      "followups_created": [],
      "dependents_repointed": [],
      "closed": false
    }
  ]
}
```

### Reviewer Mindset

The reviewer is **rigorous and substantive, not contrarian**.
Implementing agents optimise to satisfy the task as written and to make
tests pass; that is not the same as producing the right code. Passing
tests and a green regression run are necessary but not sufficient — they
tell you nothing about design drift, over-mocking, spec blind spots, or
code quality. Review exists to catch what those signals miss.

"Rigorous" means reading the actual code against the actual design and
thinking hard about what could be wrong. "Not contrarian" means not
inventing objections to prove the review was thorough. A finding must
clear this bar:

> Would a thoughtful maintainer, familiar with this codebase, raise this
> in a PR review — and would the project be meaningfully worse if it
> shipped unchanged?

If the answer is no, don't raise it. Style preferences, bikeshed names,
speculative "future-proofing," re-litigations of settled decisions, and
nits that would churn a diff without changing behaviour all fail this
bar. A clean review is a valid outcome. If you find nothing, say what
you looked for and why you ruled it out — silence is not a pass, but
padding findings is worse than finding nothing.

Look hard in three specific directions:

1. **Sub-par code from the implementing agent.** Agents gravitate toward
   solutions that look plausible and pass tests. Look for real issues:
   copy-paste, over-broad error handling, dead branches, implicit
   coupling, tests that re-state the implementation instead of verifying
   behaviour, shortcuts that cheat the verification step. Ask: "If I had
   written this from scratch, would it look like this?" If not,
   articulate the gap — but only if the gap matters functionally or to
   maintainers, not just stylistically.

2. **Implementation drift from the design.** The implementation may pass
   its verification commands while quietly diverging from architecture
   or design decisions — renamed variables that leak across boundaries,
   responsibilities that crept into the wrong module, contracts that
   were weakened to make a test pass, extension points that were
   bypassed. Re-read the change's `architecture.md` and `design.md` and
   compare against the actual code, not the task description.

3. **Specs themselves may be wrong.** Implementation is the first time
   the design meets reality. If the work revealed friction — an awkward
   abstraction, a contract that doesn't compose, a case the spec didn't
   anticipate, a decision that now looks premature — that is a **signal
   from the code**, not a problem with the implementation. "Implemented
   as specified" is not a defence if the spec is the problem. Capture
   architectural signals that the design discussion missed; they may
   warrant spec updates, new tasks, or even a pause to rethink before
   further work proceeds.

### What the review covers

- **Testing**: Is the work adequately tested? Do tests exercise real
  code paths rather than over-mock? Do tests cover the task's acceptance
  criteria? Are there paths the tests silently skip? Would a subtle
  regression actually surface, or would the assertions still pass?
- **Best practices**: Is the Go code idiomatic? Correct error handling
  (wrapped errors via `fmt.Errorf("...: %w", err)`, sentinel errors
  where appropriate, no silently-swallowed returns), appropriate use of
  `context.Context`, goroutine/channel safety, interface satisfaction,
  package layout. Any correctness, readability, or performance concerns?
  Any code that "works" but a human reviewer would reject?
- **Alignment with the change**: Does the work match the OpenSpec
  change's specs, architecture, and design? Does it stay within the
  task's stated scope? Are decisions consistent with neighbouring tasks,
  or has the task subtly broken a shared contract?
- **Signals against the spec**: Did the implementation reveal that a
  decision in `proposal.md`, `architecture.md`, `design.md`, or a spec
  file is wrong, incomplete, or worse than an alternative the agent was
  forced to invent? Call these out explicitly — they are the most
  valuable findings a review produces.

### Reviewer Agent Prompt Template (delegated)

```markdown
Review the already-merged implementation of task <TASK_NAME> for change
<CHANGE_NAME>.

You are **rigorous and substantive, not contrarian**. Raise findings
that would matter in a real PR review. Skip style nits, speculative
future-proofing, and re-litigation of settled decisions. A finding must
clear this bar:

> Would a thoughtful maintainer, familiar with this codebase, raise this
> in a PR review — and would the project be meaningfully worse if it
> shipped unchanged?

A clean review is a valid outcome. If you find nothing, say what you
looked for and why you ruled it out — silence is not a pass, but padding
is worse than nothing.

Look hard in three specific directions:
1. Sub-par code from the implementing agent (copy-paste, dead branches,
   over-broad error handling, tests that re-state the implementation,
   shortcuts that cheat verification).
2. Implementation drift from the design — re-read architecture.md and
   design.md and compare to the actual code, not just the task body.
3. Specs themselves may be wrong. If the work revealed friction, call it
   out — "implemented as specified" is not a defence if the spec is the
   problem. Spec-level findings are the most valuable output.

Task spec (what was supposed to be built):
<FULL_TASK_FILE_CONTENTS>

Reference material (pressure-test, not authority to defer to):
- openspec/changes/<CHANGE_NAME>/proposal.md
- openspec/changes/<CHANGE_NAME>/architecture.md
- openspec/changes/<CHANGE_NAME>/design.md
- openspec/changes/<CHANGE_NAME>/specs/ (if present)

Merge commit: <MERGE_COMMIT>
Diff: `git show <MERGE_COMMIT>` and
`git diff <MERGE_COMMIT>^1..<MERGE_COMMIT>` for the effective change vs
main-before-merge.

**Your job**:
1. Read the task spec and reference material.
2. Read the diff. Check for deviations and drift per directions (1)-(3).
3. Run `go test ./...` from the repo root. Record pass/fail + any
   unexpected output.
4. Assess test coverage — do the new tests exercise the task's scenarios
   and edge cases, or do they just make the CI line go green?
5. Assess code quality — idiomatic Go, error handling, concurrency.

**Do NOT modify any code or task files.** You are read-only.

**Output**: write your review to <FINDINGS_PATH> as markdown with these
sections:
  - Summary: 2-4 sentence overview (what was built, top-level
    assessment in plain English — not a label)
  - Findings: bulleted list. For each:
    - file:line or artifact reference
    - concern
    - suggested remediation
    - severity: **blocker** (shouldn't have shipped), **defect**
      (warrants a follow-up task), **inline** (trivial fix the
      orchestrator could make without its own review), or **info**
      (noteworthy, no action needed)
  - Grouping hint: which findings should cluster into a single follow-up
    task (same file/module/spec section) vs which are independent
  - Spec signals: explicit list of findings that point to problems with
    the spec (direction 3) rather than with the implementation
  - Test status: pass/fail + any interesting output
  - What I looked at: brief rundown of what you checked and ruled out,
    so a clean review is legible

DO NOT commit anything. The orchestrator handles the close, follow-up
task creation, and any dependent repointing.
```

### Inline review procedure

When running inline, the orchestrator performs the same steps itself
(Read + Bash): reads diff, runs tests, applies the mindset above, writes
findings to `<FINDINGS_PATH>` in the same format. Same signal/noise bar,
same three review directions.

## B.3 Monitor & Collect (delegated only)

Spawn all reviewers in a single message (one block, N Agent tool uses).
Poll with TaskList until every agent has completed, then read all
findings files. If a reviewer errors out or produces no findings file,
keep that task at `needs-review`, note the failure, and continue with
the successful ones.

(Inline reviews skip this section — gathering and handling interleave
in the orchestrator's own context.)

## B.4 Handling Findings

Findings split into three buckets based on remediation cost.

**Inline fix during review** — choose this when the change is trivial
and unambiguous: typos, obviously-correct one-line fixes, a missing
doc comment on an exported symbol, a clearly-wrong comparator, a missed
case in an already-covered test file. The orchestrator (not the reviewer
agent — reviewers are read-only) makes the edit directly on main,
re-runs `go test ./...`, and records the fix in the reviewed task's
Review section with the fix commit SHA. **Run regression tests after
inline fixes** — treat them like any other commit.

- If the inline fix is trivial and self-evidently correct, the task
  still flips to `done` on the same pass.
- If the inline fix is non-trivial enough that it could itself yield
  follow-up findings (touches unfamiliar code, adds new logic, changes
  a contract), leave the reviewed task at `needs-review` and re-queue
  it for another review pass. A fix that warrants its own review is
  **not** an inline fix — split it as a new task instead.

**New task (follow-up)** — for everything that isn't a trivial inline
fix. Findings go into task files under
`openspec/changes/<change>/tasks/open/`. **Group clustered findings
into a single task; split only when findings are genuinely
independent.**

- **Group** when findings share an artifact cluster (same file,
  module, spec section, or a coordinated set of `design.md` +
  `architecture.md` + adjacent task files that must move together) or
  form a coherent unit of work.
- **Split** when findings touch unrelated parts of the codebase, have
  different owners or dependencies, or would create conflicting diffs
  if batched together.

Grouping keeps the follow-up queue small, reduces the setup cost of
each fix, and produces coherent diffs instead of N small patches that
re-read the same context. One task per distinct concern, not one per
bullet point.

Use `/opsx-tasks create` or write the file directly:

```yaml
---
name: <finding-name>
description: <one-line summary>
change: <change-name>
status: ready         # or "blocked" if it has prerequisites
relations:
  - discovered-from:<reviewed-task-name>   # provenance, mandatory
---
```

The `discovered-from:` relation is mandatory — it preserves the audit
trail from review to remediation.

**Note:** `blocks:` is not a valid relation label. If the finding must
be resolved before a downstream task proceeds, **repoint the
dependent's `blocked-by:`** to the new follow-up task instead of
marking the follow-up with `blocks:`.

**Spec-level findings** (direction 3 from the reviewer mindset) are
recorded the same way, with additional handling:
- The task body names the artifact to revisit: `design.md §Decision <n>`,
  `architecture.md §<component>`, `specs/<path>` — and describes what
  the implementation revealed that the design didn't anticipate.
- If the signal invalidates downstream tasks' premises, **repoint those
  dependents' `blocked-by:`** to the finding task *and* raise it with
  the user before continuing — pausing to rework artifacts is often
  cheaper than implementing on a broken foundation.
- If the spec update is significant (not a clarification), recommend
  routing through `/opsx-continue` or explicit design-doc revisions
  rather than piecemeal task fixes.

### Closing the review

**Reviewed tasks always flip to `done`**, regardless of findings. The
only exception is the non-trivial-inline-fix case above: the task stays
at `needs-review` and goes through another review pass. Otherwise:

1. Append a **Review** section to the reviewed task's body listing:
   - Findings (one bullet each, file:line + suggested fix)
   - Any inline fixes applied during review (with the fix commit SHA)
   - Follow-up task files created
   - Dependents whose `blocked-by:` was repointed, if any
2. Flip the reviewed task's frontmatter: `status: needs-review` →
   `status: done`, and `git mv` it from `tasks/open/` to
   `tasks/closed/`.
3. If the review produced **blocking follow-ups** (findings that must
   resolve before a downstream task proceeds), **repoint the
   dependents**: edit each downstream task's frontmatter to replace
   `blocked-by:<reviewed-task>` with `blocked-by:<follow-up-1>` (and
   add entries for other blocking follow-ups). This gates dependents
   without leaving the reviewed task at `needs-review`.
4. Re-evaluate downstream `blocked-by` relations: flip `blocked` →
   `ready` for any task whose `blocked-by:` deps are all now in
   `tasks/closed/` with `status: done`.

**Why always flip to done:** Keeping the parent at `needs-review`
while waiting on follow-ups forces a re-review cascade — once the
follow-up closes, the parent has to be visited again just to flip
state, with no new code actually being reviewed. The follow-up already
gets review-equivalent scrutiny when it was opened; the parent's
second review is pure bookkeeping. Flipping to `done` immediately
preserves the audit trail (review notes live on the parent; findings
become tracked follow-up tasks with `discovered-from:` provenance)
without the cascade.

Then move to the next needs-review task in the batch.

## B.5 Commit

Single commit at the end of the batch, summarising the session:

```
git commit -m "Tock batch <session_id>: reviewed N tasks, \
  F follow-ups created, D dependents repointed"
```

Inline fixes applied during review are committed separately as they
happen (not batched into this commit).

## B.6 Summary Report (tock)

```
Tock Batch Complete ✓

Reviewed 3/3 tasks:
  ✓ setup-module → done (clean; what I looked at: …)
  ✓ write-spec-file → done
      Follow-ups: fix-schema-empty-check (ready),
                  document-canonicaljson-limitations (ready)
      Dependents repointed: none
  ✓ characterize-current → done
      Inline fix applied: 3f2a1b0 (typo in docstring)
      Follow-ups: redo-characterization-module (blocker)
      Dependents repointed: consume-characterization
                            (blocked-by repointed to
                             redo-characterization-module)

Inline fixes: 1 commit on main
New ready tasks: 2 (+1 blocker follow-up)
Newly unblocked by done flips: 4 (implement-core, wire-transient, …)
Spec signals raised: 0

Next step: run another tick session.
  /opsx-tick-tock tick
```

---

# Shared — Error Handling

- **Worker/reviewer agent failures**: mark failed, keep any worktrees
  (tick), continue with others.
- **Merge conflicts (tick)**: abort merge, skip task, continue. **The
  conflict shape itself may be a finding** — if reconciling two
  branches surfaced a structural tension (overlapping refactors, a
  shared contract neither task owned, a fixture both tasks
  regenerated incompatibly), capture that tension as a follow-up task
  per A.7.5 before the batch closes.
- **Test regressions (tick)**: stop merges, keep worktrees, offer
  revert/investigate options. **The root cause of the regression is
  usually the finding** — even if reverting/skipping resolves the
  immediate failure, the underlying issue (latent bug, fragile
  assumption, weak test) persists and must be captured per A.7.5,
  not left in commit messages and `.orchestrator/after-*.txt` traces
  alone.
- **Reviewer produced no findings file**: treat as failed review — keep
  task at `needs-review`, do not close, move to next task.
- **Inline fix during review breaks tests**: `git reset --hard HEAD~1`
  to undo the fix, convert that finding from inline to follow-up task,
  continue.
- **User interruption**: save state, report progress.

# Shared — Critical Requirements

**NEVER**:
- Use `run_in_background: true` (blocks file writes)
- Create worktrees from within other worktrees (causes nesting)
- Auto-close a task to `done` from tick (violates tick-tock)
- Skip test verification after a tick merge
- Skip test verification after an inline fix during review
- Fragment clustered findings into one task per bullet (group them)
- Pad review findings to prove the review happened — silence with
  justification is a valid outcome
- Use `blocks:` as a relation label (the validator rejects it — gate
  dependents by editing their `blocked-by:`)
- Leave a reviewed task at `needs-review` just because it produced
  blocking follow-ups (flip to done + repoint dependents instead)
- Let tick-time discoveries (latent bugs surfaced by regression,
  manual conflict-resolution decisions) live only in commit messages
  or in `.orchestrator/after-*.txt` traces. Tock cannot mine those
  reliably from cold; capture them per A.7.5 — worker observations
  go in the merged task file's `## Observations` section, while
  orchestrator-side discoveries become `ready` follow-up tasks before
  the batch commit lands.

**ALWAYS**:
- Create worktrees from main repo root
- Use absolute paths from `git rev-parse --show-toplevel`
- Test after EACH tick merge (sequential, not batched)
- Record `merge_commit` in frontmatter when flipping to needs-review
- Update state file after each phase
- Apply the signal/noise bar to findings: raise only what a thoughtful
  maintainer would flag in a PR review and that would make the project
  meaningfully worse if shipped unchanged
- Group clustered findings into one task instead of fragmenting them
  into one task per bullet
- Flip reviewed task to `done` after review, regardless of findings.
  When blocking follow-ups exist, repoint downstream dependents'
  `blocked-by:` to the follow-up tasks instead of leaving the parent at
  `needs-review`
- After a review task flips to `done`, re-evaluate dependent tasks'
  `blocked-by` status and flip newly-unblocked tasks to `ready`
- During tick, workers append out-of-scope findings directly to the
  merged task's `## Observations` section. The orchestrator scans
  those (and `.orchestrator/after-*.txt` traces for regression root
  causes) and elevates only what warrants its own task — a
  user-visible bug, a correctness regression, or a spec contradiction
  big enough for separate review. State explicitly in the A.9 summary
  whether any observations were elevated to follow-up tasks (or that
  the batch was clean) — silence is ambiguous

# Prerequisites

Check before starting:
- Change directory `openspec/changes/<name>/tasks/open/` exists
- At least one ready task (tick) or needs-review task (tock) present
- `git worktree --version` available (tick only)
- Project test command from `architecture.md` is runnable
- No existing `.orchestrator/state.json` (previous session not finalised) —
  if present and its `type` field matches the current mode and its tasks
  are still in-progress, ask whether to resume or archive and restart
