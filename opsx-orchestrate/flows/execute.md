# Execute phase

The bridge. Forward speculation becomes diff; diff becomes discovery.

A task is "executed" only when **both** implement and review have completed on it. Implement-without-review is in-flight, not done.

## Operations

### 1. Implement (per task, in worktree)

For each task in the batch, the orchestrator:

1. Captures a baseline test snapshot if one isn't already taken for this cycle: `<repo>/.orchestrator/baseline-<cycle-id>.txt`, then `state.py set baseline_snapshot=<path> baseline_status=<exit code>`.
2. Creates a worktree at `<repo>/<worktree.parent>/task-<task-name>-<ts>`.
   - Uses `git worktree add "$WORKTREE_PATH" -b "$BRANCH_NAME"`.
   - Always from main repo root; never from within an existing worktree.
3. Runs the overlay's `worktree.init` hook if defined.
4. Spawns an Implementor agent (`Agent` tool, `subagent_type: general-purpose`, `run_in_background: false`).
5. Hands the agent the assembled brief (per `roles/implementor.md`).
6. `state.py task set <name> in_progress agent_task_id=<id> worktree_path=<path> branch_name=<branch>` (the tool sets `started_at`).

When an agent completes, the orchestrator verifies at least one commit landed on the worktree branch. No commits → `state.py task set <name> failed` (worktree retained for debugging).

### 2. On-touch Architect (during implement, against load-bearing entries)

When an Implementor commits to a worktree branch and the diff modifies code cited in a `load_bearing: true` register entry, the orchestrator triggers an Architect on-touch run **scoped to that entry only**.

- Cheap; runs in parallel with the rest of the batch.
- Reads: the diff against merge-base; the register entry's full text; immediate call-graph neighbours of the touched code.
- Output: zero or more findings written to the cycle's findings dir, each indexed with `state.py record add findings '{...}'` (title, severity, class, trigger, locations; the file at `path` carries the rest).
- A `severity: blocking` finding pauses the merge of this task (`state.py task set <name> blocked blocker_note=<finding-id>`); the orchestrator routes it via the resolution channels (see "Resolution" below). An `interface-drift` finding against a stale design doc becomes an ask record (`templates/ask.md`); because it blocks this merge it is asked at once (§ 9).

The on-touch trigger is the cheap-and-narrow Architect mode (per `roles/architect.md`). It is not a substitute for the end-of-cycle audit; it catches drift while still local.

### 3. Sequential merge to integration branch

When all Implementors have completed (or failed / been stopped), the orchestrator merges in completion order:

```bash
REPO_ROOT=$(git rev-parse --show-toplevel)
cd "$REPO_ROOT"
git merge --no-ff "$BRANCH_NAME" -m "Merge task $TASK_NAME: $DESCRIPTION"
MERGE_COMMIT=$(git rev-parse HEAD)
```

`MERGE_COMMIT` is recorded in step 6 with the status change. **Conflicts**: `git merge --abort`, `state.py task set <name> blocked blocker_note=merge-conflict`, keep worktree, continue with next task.

### 4. Test after each merge

```bash
$TEST_CMD > "$REPO_ROOT/.orchestrator/after-${TASK_NAME}-${TS}.txt" 2>&1
AFTER_STATUS=$?
```

Record the file: `state.py task set <name> after_snapshot=.orchestrator/after-<task>-<ts>.txt`. If `AFTER_STATUS != 0` and `BASELINE_STATUS == 0`: regression. `state.py task set <name> regression_detected=true`; stop further merges; keep worktrees; raise an `environment`-kind ask (`templates/ask.md`) with the after-file paths. It blocks the merge chain, so it is asked at once (§ 9).

### 5. Capture orchestrator-side discoveries

Before continuing to review, the orchestrator scans for discoveries no individual agent owns. Per the brainstorm and lifted from VCE's §A.7.5:

- **Latent bugs surfaced by a regression** — a test broke not because the merging task was wrong but because it perturbed a pre-existing fragile assumption (e.g. a non-stable sort coupled to insertion order).
- **Worker observations on the merged task body** — scan `## Observations` and decide whether any single observation rises to the bar for its own follow-up task. Most stay in the body for the reviewer to read in context.
- **Manual conflict-resolution decisions** — when the orchestrator dropped, restructured, or regenerated code while reconciling two branches, that decision is an unreviewed structural change. Note it on the merged task body; if it touched a contract or dropped a test, file a follow-up.
- **Aborted merges where the abort reason is itself the finding** — capture the structural issue (not just the merge failure) as a `ready` task so the next cycle can address it.

Each discovery is one `state.py note discovery "<one line>" --ref <task>` (with `--path` when a report holds the detail); a conflict-resolution decision is `note decision`; a scan that found nothing is one `note note "scanned <what>; nothing"`. The journal is the only place for these; they do not become state keys.

### 6. Flip to `needs_review` (NOT `done`)

After successful merge + tests pass + no regression, the task is **not yet** closed:

```bash
state.py task set "$TASK_NAME" needs_review merge_commit="$MERGE_COMMIT" implementor_report_path="$REPORT"
```

where `$REPORT` is the Implementor's structured report saved by the orchestrator at `.orchestrator/cycles/<cycle-id>/reports/<task-name>.md` (the tool refuses `needs_review` without `merge_commit` and sets `completed_at`). Mirror `status: needs_review` and `merge_commit` into the task file's frontmatter (file stays in `tasks/open/`); mirror `status: done` the same way in § 8.

Then remove the worktree and record it:

```bash
git worktree remove "$WORKTREE_PATH"
git branch -D "$BRANCH_NAME"   # optional; tidies local branch list
state.py task set "$TASK_NAME" worktree_removed=true
```

### 7. Review (author-blind, per task)

Once a task is `needs_review`, the orchestrator spawns a Reviewer. **The reviewer-spawn helper enforces the author-blind constraint at the substrate level.**

#### Reviewer-spawn helper contract

The helper builds the Reviewer's input from a **fixed set of sources**, none of which are the Implementor's:

```yaml
reviewer_input:
  diff: $(git diff <merge-base>..<merge_commit>)
  task_brief: <full text of <change>/tasks/open/<task-name>.md, EXCLUDING ## Observations and ## Discoveries sections>
  cited_register_entries: <full text of each entry in cites_register_entries, with current status>
  project_standards: <overlay's roles/reviewer.md, if present>
```

The helper **must not**:
- Read or pass the Implementor's structured report.
- Read or pass the `## Observations` or `## Discoveries` sections.
- Pass any indicator of the Implementor's identity.
- Allow the Reviewer's worktree to contain any file other than the clean merge-base checkout.
- Pass any other in-flight diffs.

The helper is the load-bearing piece that makes the author-blind constraint structural rather than a discipline. Modifying it to violate any of the above is a bug, not a feature request.

#### Review modes

- **Inline review**: orchestrator runs review itself in its main context. Useful for small batches where the orchestrator wants to ride along closely. Default: when batch size ≤ 2.
- **Delegated review**: spawn a separate general-purpose Agent per task. Default when batch size ≥ 3, or when the orchestrator's context is bloated.

Either way, the input contract is identical and author-blind.

### 8. Handle review findings

The Reviewer's findings file lands at `<repo>/.orchestrator/cycles/<cycle-id>/reviews/<task-name>.md`; the orchestrator records it with `state.py task set <name> reviewed review_mode=<inline|delegated> findings_path=<path> findings_count=<n>` (the summary stays in the file; state holds the path and the count). Then it processes:

| Finding severity | Orchestrator action |
|---|---|
| `blocking` | Apply inline fix (`state.py note inline-fix "<what>" --ref <task>`) OR re-spawn Implementor with fix scope (`state.py task set <name> in_progress`, which counts a rejection) OR revert merge — task does not advance to `done` |
| `advisory` | Apply inline fix OR file follow-up task with `discovered_by: reviewer`, `discovered_class: <appropriate>` (`state.py task add … discovered_from=<task> discovered_by=reviewer discovered_class=<class>`) — task can advance to `done` |
| `spec-signal` | Record an ask (`templates/ask.md`, `raised_by.ref` the finding); presented at integrate unless its `blocks` names a merge in this batch (§ 9). Does not block the task itself; it signals that the design may need revision |

The orchestrator's inline fixes are committed with `Co-Authored-By: <reviewer>` style attribution; follow-up tasks carry the provenance fields.

When all inline fixes are applied: `state.py task set <name> done`. Follow-up tasks it created go in `followups_created=[...]` on the same command.

### 9. Asks raised during execute

An `AskUserQuestion` call blocks everything the orchestrator would otherwise do next, and the record shows a 35-minute stall on a question that was not needed until the next wave. So:

- An ask that arises while agents run (a `spec-signal`, an on-touch finding, an Implementor that stops to ask, an orchestrator-side discovery) is **recorded** with `state.py record add asks '{...}'` per `templates/ask.md` (the id comes back) and **held** until the next natural pause: the merge chain drained, the batch closed, or the user's next prompt. Integrate's § 3a presents it.
- The exception is an ask whose `blocks` names a merge in this batch or a task not yet spawned: present that one ask **alone**, now, in the template's block form, and keep doing the orchestrator-side work that does not depend on it. `environment` asks (a regression stop, a wedged runner) are always in this case.
- Never put a non-blocking ask in the same `AskUserQuestion` as a blocking one.
- An Implementor that stops to ask (`roles/implementor.md` § Escalation contract) leaves its task blocked (`state.py task set <name> blocked blocker_note=<ask-id>`) and its worktree retained. `blocked` counts as stopped for the exit gate, so `no_orphan_in_progress` still passes; `task set <name> ready` when the ask is applied.

### Productive tension resolution

Per `roles/reviewer.md`, the Reviewer may flag a choice the Implementor had a good but invisible reason for. The orchestrator (which holds the Implementor's report and the Reviewer's findings) resolves:

- If the reasoning was wrong: act on the flag (treat as ordinary finding).
- If the reasoning was load-bearing-but-undocumented: codify it as a comment, test, or register-entry invariant — so the next reviewer doesn't flag it again. `state.py note push-back "<one line>" --ref <task>` records that it happened.

The Reviewer never has to know which path was taken.

## Inputs (from plan)

- The composed batch and its tasks with briefs.
- The current state of the register (cited entries with their `status`).
- `phase_gates.plan.passed: true` (mandatory; `state.py phase set execute` enforces it).

## Exit gate

| Check | Condition |
|---|---|
| `all_tasks_executed_or_stopped` | Every task in the batch is either `done` or explicitly `failed` / `externalised` / `blocked` |
| `all_reviews_completed` | Every task that reached `reviewed` / `done` with a `review_mode` has its findings file on disk |
| `no_orphan_in_progress` | No task is stuck in `setup_complete` / `in_progress` / `completed` / `needs_review` / `reviewed` |

All three are computed: `state.py gate check execute` shows them with reasons, `state.py gate pass execute` sets the gate, and `state.py phase set integrate` advances. Integrate refuses to run otherwise.

## What execute does **not** do

- Execute does not run the end-of-cycle Architect audit. (That's integrate's job; on-touch is execute's only Architect mode.)
- Execute does not produce the PM digest. (Integrate.)
- Execute does not reconcile register entries. (Integrate.)
- Execute does not modify the proposal status header. (Integrate, via goal-drift handling.)

These are all integrate-phase operations; conflating them into execute is what the brainstorm's three-phase model exists to prevent.
