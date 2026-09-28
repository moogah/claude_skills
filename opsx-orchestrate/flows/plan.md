# Plan phase

The forward channel firing. Plan turns the design into a batch the next execute phase will implement, after checking that the batch's premises still hold at HEAD.

A plan that doesn't consume the prior cycle's integrate output isn't planning — it's queue-running. The integrate→plan handshake artifact is a hard input contract; plan **refuses to start** if the artifact is missing or any of its required fields are unset.

## Operations

### 1. Consume the prior integrate's handshake

`state.py init --change <name> --test-command <cmd>` starts the cycle and reads `<repo>/.orchestrator/handshake-<prior-cycle-id>.json` (the newest one, or `--prior-handshake <path>`; `--first-cycle` when none exists). Then read the handshake. It has:

- `design_diff` — the seam rows conformance added, amended or retired last cycle.
- `pm_digest_path` — the prior digest, for context.
- `meta_discoveries` — patterns to weigh when drafting this batch.
- `user_resolved_goal_drift` — any revise / split / abandon decisions the user made.
- `asks_for_user_open` (may be empty), `asks_for_user_resolved` (may be empty).
- `task_refinements` — which open tasks already absorbed cycle learning.
- `journal` — the prior cycle's discoveries and decisions, one line each.

On a first cycle (`--first-cycle`) there is no handshake and nothing to read. Otherwise, if the file is missing or any field is missing (not "empty array" — actually missing), `init` refuses to start. Direct the user to close the prior cycle properly (`state.py close`) or to abandon it explicitly (`state.py close --abandon --why`). This is an `environment`-kind ask (`templates/ask.md`): asked at once, since there is no state to record it in.

This is the brainstorm's loop-closure contract: each cycle's discoveries must update the next cycle's plan.

Read the overlay's `priors.md` alongside the handshake (`overlay.md` § Priors): the user's standing rules apply to batch composition and go into every brief.

### 2. Read the design and draft the candidate tasks

The change's `design.md` (`templates/design.md`) is the input: its Seams table and its Scenario coverage table. A change without them has not had its design round (`flows/design.md`); run it before this cycle, not inside it.

The orchestrator drafts the candidate task files for this cycle under `<change>/tasks/open/` (per `templates/task-body.md`) from three sources, in this order:

1. Open tasks carried over (refined by the prior integrate § 7, or user-authored on a first cycle); do not re-touch those `task_refinements` lists.
2. Coverage rows whose test is still pending, grouped by the seam they exercise: one task per seam or per closely coupled pair, never one per test.
3. The prior cycle's follow-ups and user-resolved asks with deferred implementation, which integrate § 7 already wrote as task files.

Each candidate cites the seams it implements (`cites_seams`, also carried in state) and the scenarios it makes pass (`cites_scenarios`, a task-file field only), and its Verification section lists the pending tests it promotes. The orchestrator may read HEAD while drafting, but the premise check is the authority on what holds there. The brief will quote the cited rows verbatim, so a task that cites nothing has no contract to be reviewed against.

### 3. Premise check

The Architect runs against the candidate task files (`roles/architect.md` § Premise check): for each task, a `| claim | at HEAD |` table with **Holds** or **Wrong** per claim the task body makes about the code (a function exists and has this signature; a path is reachable from this entry point; a return value has these cases; a defect of the task file's own form, such as a truncated seam quote, is a row too, flagged as such), and a one-line verdict. Output: `<repo>/.orchestrator/cycles/<cycle-id>/premise-check.md` (the orchestrator creates the directory). The check may run the suite and probes read-only in a scratch directory.

Then:

- A task with a **Wrong** premise is rewritten or split by the orchestrator before the batch freezes; the rewrite is the correction, not a stanza.
- A defect the check finds outside every task's scope is a finding (`state.py record add findings`, `trigger: premise-check`, `discovered_from: batch-<cycle-id>`) at the bar with `observed` filled, routed by `flows/integrate.md` § 7's rules when integrate runs; one with `observed.happened: false` that no prior or user request covers is `record set findings <id> resolution=noted` at once, since its fate is already known. One whose `blocks` names a task in this batch is settled now: a `blocking` finding becomes an in-batch task or a rewrite of the task it blocks; a `decision` it raises is an ask record presented in block form before `batch_composed` (`flows/execute.md` § 9's rule, one ask alone).
- A seam row the check finds stale (the code or the test tree is right, the row is not) is a `doc-correction` ask (`templates/ask.md`): the orchestrator amends the row, records the ask with `status: applied` and `applied_via: design.md <seam-id>`, and it is listed under "Applied without asking" in `asks.md` at integrate § 3a. The check itself does not edit `design.md`.

The record shows why this step exists: the plan-time review of the batch against HEAD found a three-way return value described as two-way, two contradicting log messages, and a task premise that split a task, none of it inside any task's stated scope, and every one of its outputs was used.

### 4. Batch composition and registration

Select which candidates this cycle will run. The batch composer balances:

- Critical-path coverage: ≥1 task on the critical path per cycle, unless the prior integrate explicitly deferred.
- Dependencies: `blocked_by` names the tasks a candidate must follow; execute spawns a task when its `blocked_by` tasks have merged.
- Total batch size: project-overlay-configurable; default 3–7 tasks, or fewer when the design has fewer seams left to implement.

Add each: `state.py task add <name> --file <path> --class <class> --cites <seam-id,...> [--blocked-by <task,...>] [--critical]`. A follow-up carries `discovered_from=… discovered_by=… discovered_class=…` on the same command. Then `state.py gate set plan batch_composed=true`.

### 5. Implementor brief assembly

For each task in the batch, the orchestrator assembles the brief at agent-spawn time (per `roles/implementor.md`):

- Task body.
- The cited seam rows and the coverage rows the task promotes, verbatim from `design.md`.
- Cited `design.md` decisions and `proposal.md` sections.
- Project standards (overlay's `roles/implementor.md`) and the project priors (`priors.md`).

The brief framing is fixed — the seam rows are *reference material to pressure-test, not authority to defer to*. A row that turns out wrong is a push-back in `## Discoveries`; conformance amends the row at integrate.

### 6. PM critical-path read

One LLM read of `proposal.md` to identify which tasks are on the critical path. `state.py task set <name> on_critical_path=true` for each (or `--critical` at `task add`). Cheap; runs once per plan phase, not per PM tick.

Overlay can override via `critical-path.override-tasks` (explicit task list) or `critical-path.override-labels` (task_class values that are always critical-path).

### 7. User sign-off on goal-drift recommendations

If the prior integrate's `user_resolved_goal_drift` is empty but the digest carried recommendations, plan blocks until the user signs off. This is the bridge that prevents goal-drift signals from being silently ignored. The recommendation is the `decision`-kind ask integrate raised (`templates/ask.md`; four options: revise / split / abandon / continue), presented in block form; the gate is satisfied when its `status` is `answered` or `applied`.

If the prior integrate had no goal-drift recommendations, this step is a no-op.

## Inputs

- The integrate→plan handshake artifact (mandatory).
- The change's `design.md` with its Seams and Scenario coverage sections, `proposal.md`, and the open task files.
- The prior cycle's PM digest (informational).
- The project overlay's `config.yaml` (for thresholds, taxonomy, critical-path).

## Exit gate

The cycle does not enter execute until:

| Check | Condition |
|---|---|
| `prior_integrate_consumed` | Handshake artifact present with all required fields (computed) |
| `batch_composed` | Batch task list explicit and frozen (asserted: `state.py gate set plan batch_composed=true`) |
| `briefs_cite_seams` | Every task in the batch that is not `externalised` has at least one entry in `cites_seams` (computed; it checks that a citation exists, not that the id is in `design.md`) |
| `user_signed_off_goal_drift` | If prior integrate carried goal-drift recommendations, user has dispositioned them; otherwise no-op (computed from the goal-drift ask's status) |

`state.py gate check plan` shows the computed three with reasons; `state.py gate pass plan` sets the gate when all four are true; `state.py phase set execute` advances.

Execute refuses to run if the plan gate has not passed.

## What this displaces

The older two-phase tick/tock model conflated plan with execute. Per the brainstorm:

> Tick and tock were both *per-task* operations on the batch as it forms — implement the diff, review the diff. What was missing is a phase whose unit is the *cycle itself*, with two distinct cycle-level jobs that the two-phase model could not house: *planning the next cycle's speculations* and *integrating the prior cycle's discoveries*.

Plan is the first; integrate (`flows/integrate.md`) is the second.

## Cadence variants

- **Inline tasks**: plan collapses into "is this trivial enough?" (see `flows/inline.md`). The handshake-consumption step is skipped because inline tasks don't span cycles.
- **Standard cycles**: full plan as described above.
- **Long cycles**: plan runs once at the start; execute spans many sessions. PM digest may run periodically within execute via `/loop` or `/schedule`.
- **Multi-change parallelism**: each in-flight change runs its own plan / execute / integrate cycle independently. V1 limits to single-change.

## What plan does **not** do

- Plan does not modify code. (The premise check may run the suite and probes read-only; nothing in plan writes to `src/` or the test tree.)
- Plan does not redesign. (The one edit it makes to `design.md` is a `doc-correction` the premise check surfaced: one row, applied by the orchestrator and listed.)
- Plan does not spawn Implementors. (That's execute's job.)
- Plan-time findings are routed by integrate's rules, not acted on ad hoc; only one that blocks a batch task is settled before `batch_composed`.

If a question forces plan to do any of the above, that's a sign the prior cycle wasn't properly integrated. Refuse to advance; route back through integrate.
