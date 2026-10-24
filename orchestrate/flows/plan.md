# Plan phase

The forward channel firing. Plan turns the design into a batch the next execute phase will implement, after checking that the batch's premises still hold at HEAD.

A plan that doesn't consume the prior cycle's integrate output isn't planning — it's queue-running. The integrate→plan handshake artifact is a hard input contract; plan **refuses to start** if the artifact is missing or any of its required fields are unset.

## Operations

### 1. Consume the prior integrate's handshake

First, the MVP decision (`templates/hardening.md`). When `state.py status` lists an ask with `:BLOCKS: plan` that the prior integrate raised for it, plan does not run `init` while it is open: it hands back `plan: stopped, ask <id> blocks plan; <path>`, and nothing runs on the change until the user answers. An answer waiting in the file is applied first, as that template says: `stop` and `rethink` need no cycle, so plan opens none and hands back `plan: not started, change <stopped|rethink>; <intent path>`; `harden` goes on to `init`, and § 2 drafts from the hardening list.

`state.py init --change <name>` starts the cycle (the test command comes from the overlay) and reads `<change-dir>/handshake-<prior-cycle-id>.json` (the newest one, or `--prior-handshake <path>`; `--first-cycle` when none exists). Then read the handshake. It has:

- `design_diff` — the seam rows conformance added, amended or retired last cycle.
- `pm_digest_path` — the prior digest, for context.
- `meta_discoveries` — patterns to weigh when drafting this batch.
- `user_resolved_goal_drift` — any revise / split / abandon decisions the user made.
- `asks_for_user_open` (may be empty; each entry's `path` names the file in `<change-dir>/inbox/`, read the ask there), `asks_for_user_resolved` (may be empty).
- `task_refinements` — which open tasks already absorbed cycle learning.
- `journal` — the prior cycle's discoveries and decisions, one line each.

On a first cycle (`--first-cycle`) there is no handshake and nothing to read. Otherwise, if the file is missing or any field is missing (not "empty array" — actually missing), `init` refuses to start. Direct the user to close the prior cycle properly (`state.py close`) or to abandon it explicitly (`state.py close --abandon --why`). This is an `environment`-kind ask (`templates/ask.md`): asked at once, since there is no state to record it in; the phase agent hands back `stopped` with the question and the dispatcher asks it (`SKILL.md` § Two contexts).

This is the brainstorm's loop-closure contract: each cycle's discoveries must update the next cycle's plan.

Read the overlay's `priors.md` alongside the handshake (`overlay.md` § Priors): the user's standing rules apply to batch composition and go into every brief.

`init` indexes every file left in `inbox/` into the new cycle (`inbox_carried`) and is a sync point (`templates/ask.md` § Sync points): an inbox file with an answer is handled before the batch is drafted (`templates/ask.md` § Answer protocol).

### 2. Read the design and draft the candidate tasks

The input is the change's `design.org` (`templates/design.md`), its Seams table and Decisions, and `coverage.py --intent docs/changes/<change>/intent.org` (`templates/spec.md` § Coverage), the change's scenarios with their status. A change with no Seams table, or with an `uncited` scenario among its own, has not finished its design round (`flows/design.md`); run it before this cycle, not inside it. A `design.org` whose § Open Questions has an entry with no Decision has not finished its round either, and plan drafts nothing, since a task drafted on a provisional answer is the rework the record paid for (`flows/design.md` § 5). Write each such entry as a `decision` ask file (`state.py record add asks`, `blocks: [plan]`, `ref: design.org § Open Questions`; an entry that already has an open file is not written twice), commit them in one commit, and hand back `plan <cycle-id>: stopped, asks <ids> block plan; <change-dir>/inbox/`. Resumed with the answers: write each into § Decisions and the spec it settles, in place (`templates/ask.md` § Answer protocol), apply the asks, run the scoped re-run any answer calls for, then draft.

The orchestrator drafts the candidate task files for this cycle under `<change>/tasks/open/` at the home (`$ORCH_HOME/.orchestrator/changes/<change>/tasks/open/`, per `templates/task-body.md`) from three sources, in this order:

1. Open tasks carried over (refined by the prior integrate § 7, or written by the user by hand on a first cycle); do not re-touch those `task_refinements` lists. **Plan § 2 drafts the batch; every other point that writes a task file is in `externalisation.md` § Who writes a task file.** Task files a split agent or another tool generated are not a source: plan drafts from the design and deletes them at the first plan, with one `state.py note note "deleted <n> generated task files: <names>"`. The record: in one change all 14 generated tasks were deleted; in another six were rewritten at plan (+781/−347); in a third, tasks drafted while the round still ran failed the premise check 5 of 7 times. A task named in the `:BLOCKS:` of an open ask file (`state.py inbox`) stays out of the batch until the ask is applied.
2. The change's scenarios `coverage.py` lists as `pending`, grouped by the seam whose row cites them: one task per seam or per closely coupled pair, never one per test.
3. The prior cycle's follow-ups and user-resolved asks with deferred implementation, which integrate § 7 already wrote as task files.
4. After the user answered `harden` on the MVP decision: the entries of the change's hardening list (`templates/hardening.md`) the answer covers, one candidate task per entry, or per group of entries touching the same code, with `discovered_from: <the entry's :REF:>` and the placement rule of `externalisation.md` (an entry outside the change's code may go to `.tasks/`). Each entry gets `:TASK: <task-name>`. These are not drafted from the design, so the Architect checks them (§ 3).

Every candidate is the main path's work or a defect's fix. A candidate that adds scope no intent goal needs (an option, a generalisation, a refactor for a future caller) is drafted only under the YAGNI check (`templates/architect-finding.md` § The YAGNI check).

Each candidate cites the seams it implements (`cites_seams`, also carried in state) and the scenarios it makes pass (`cites_scenarios`, a task-file field only), and its Verification section lists the pending tests it promotes. The orchestrator may read the code root's HEAD while drafting, but the premise check is the authority on what holds there. The brief will quote the cited rows verbatim, so a task that cites nothing has no contract to be reviewed against.

### 3. Premise check

Who checks a task's claims against HEAD depends on who wrote its text. Kept after one replay (roadmap 1.1, 2026-10-14).

- A task drafted at this plan from the design's seams and the pending scenarios (§ 2, source 2): no Architect check. Its Implementor checks the claims against HEAD before writing code (`roles/implementor.md` § Premises).
- Every other candidate (carried over, a follow-up from a finding, a bug report, a task the user wrote): the Architect check below. When no candidate needs it, the Architect is not spawned and the cycle has no `premise-check.md`.

The record behind the split: of 48 Wrong rows over two projects, 17 of the 27 design-wrong rows and all 12 task defects were in task text the orchestrator wrote, 14 of them in one bug task; the Implementor reads HEAD anyway, and the check's own agent cost about 6.6M tokens a run.

The Architect, spawned at its role default (`model: opus`, `effort: medium`) and recorded with `role: architect-premise`, runs against the candidate task files (`roles/architect.md` § Premise check): for each task, a `| claim | at HEAD |` table with **Holds** or **Wrong** per claim the task body makes about the code at the code root's HEAD (a function exists and has this signature; a path is reachable from this entry point; a return value has these cases; a defect of the task file's own form, such as a truncated seam quote, is a row too, flagged as such), and a one-line verdict. Output: `<change-dir>/cycles/<cycle-id>/premise-check.md` (the orchestrator creates the directory), and one hand-back line with the counts (`SKILL.md` § Hand-back lines). The check may run the suite and probes read-only in the scratch directory its brief names (`roles/architect.md` § What the Architect may and may not do).

Then:

- A task with a **Wrong** premise is rewritten or split by the orchestrator before the batch freezes; the rewrite is the correction, not a stanza.
- A defect the check finds outside every task's scope is a finding (`state.py record add findings`, every string field at most 280 characters, `trigger: premise-check`, `discovered_from: batch-<cycle-id>`) with its kind and evidence, routed now by `templates/architect-finding.md` § Routing (`record set findings <id> decoration=…`): a `hardening` one is added to the hardening list at once (`resolution=hardening-list`), a `blocking` one becomes a task when integrate § 7 runs. A speculative need the check notices is a `thought:` journal line. One whose `blocks` names a task in this batch is settled now: a `blocking` finding becomes an in-batch task or a rewrite of the task it blocks; a `decision` it raises is an ask file (`state.py record add asks`), presented before `batch_composed` only under the inline rule (`templates/ask.md` § The inline rule; the phase agent hands back `stopped` and the dispatcher asks); otherwise the task it blocks is `blocked` with `blocker_note: <ask-id>`, left out of the batch, and the plan goes on.
- A seam row the check finds stale (the code or the test tree is right, the row is not) is a record correction (`templates/design.md` § Seams): the orchestrator fixes the row, cutting rather than adding when the staleness is implementation detail, writes one `state.py note note "record-correction: <what>" --ref <seam-id>` (at most 280 characters, or the tool refuses `text_too_long`; `--path` names a file for more) and commits the row in the code root with a subject naming it. No ask file. The check itself does not edit `design.org`.

The record shows why this step exists: the plan-time review of the batch against HEAD found a three-way return value described as two-way, two contradicting log messages, and a task premise that split a task, none of it inside any task's stated scope, and every one of its outputs was used.

### 4. Batch composition and registration

Select which candidates this cycle will run. The batch composer balances:

- Critical-path coverage: ≥1 task on the critical path per cycle, unless the prior integrate explicitly deferred.
- Dependencies: `blocked_by` names the tasks a candidate must follow; execute spawns a task when its `blocked_by` tasks have merged.
- Total batch size: the plan's judgment (no overlay key); default 3–7 tasks, or fewer when the design has fewer seams left to implement.

Add each: `state.py task add <name> --file <path> --class <class> --cites <seam-id,...> [--blocked-by <task,...>] [--critical]`. A follow-up carries `discovered_from=… discovered_by=… discovered_class=…` on the same command. Then `state.py gate set plan batch_composed=true`.

### 5. Implementor brief assembly

For each task in the batch, the orchestrator assembles the brief at agent-spawn time (per `roles/implementor.md`):

- Task body.
- The cited seam rows, verbatim from `design.org`, and for each scenario in `cites_scenarios` its text from `docs/specs/` and its `coverage.py --scenario` row.
- Cited `design.org` decisions and `intent.org` sections.
- Project standards (overlay's `roles/implementor.md`) and the project priors (`priors.md`).

The brief framing is fixed — the seam rows are *reference material to pressure-test, not authority to defer to*. A row that turns out wrong is a push-back in `## Discoveries`; conformance amends the row at integrate.

**The task's agent setting.** Each task in the batch runs at the role defaults, an Implementor at `opus`, `medium` and its Reviewer at `opus`, `high`, unless the plan writes a **task override** for it: an `agent:` block in the task file's frontmatter with the new setting and a one-line reason (`templates/task-body.md`), which execute reads at the spawn (`flows/execute.md` § 1, § 7). Quality comes first, so a task is moved only on a reason its own text gives:

- **Lower** the Implementor to `sonnet` (any effort) only when the work is clearly routine: a config value, a rename across a few call sites, a doc or comment, promoting a pending test whose seam row is settled and whose steps are spelled out. A task that touches a seam's owning symbols in a way its row does not already fix, or whose steps leave a design choice open, is not routine.
- **Raise** the Implementor to `fable` (or Opus at `high` or above) only when the plan judges the task hard: several seams moving at once, concurrency or a migration of stored data, a contract the task changes for other callers. The task's Reviewer may be raised with it; a Reviewer is never lowered below its role default.
- **Not** `haiku`, for any agent of a task. **Not** on an outcome: a task carried over after it failed, was rejected or was reworked keeps the setting it had, and a reason that cites the failure is not a reason (`SKILL.md` § Agent settings: a retry runs at the same setting).

The reason is one line a reader can check against the task body, written as the reason for a task override: "agent: fable, high, because the migration touches three seams". A task with no `agent:` block needs no reason; it runs at the role defaults. Plan writes the block while drafting or at this step, before `batch_composed`; nothing changes it after.

### 6. The critical path is the main path

`on_critical_path` says the task's work is on the change's main path (`templates/architect-finding.md` § Routing), so the MVP decision waits for it (`flows/integrate.md` § 9). It is set when the task is added (`--critical` at `task add`), by where the task came from, never by a separate read:

- drafted from the design (§ 2, source 2): true when a scenario it makes pass is a use a Goal names; false for a scenario no goal names;
- a follow-up of a finding or discovery: true when it was routed `blocking` as a defect on the main path; an untrue sentence is a text correction and makes no task;
- from a user's decision (integrate § 7): true when the decision was routed to the main path;
- drafted from a hardening entry after `harden`, from a meta-discovery, or as scope under the YAGNI check: false.

A task whose source does not settle it is false, with the doubt in one `state.py note note` line; the user sees it with the MVP. After `rethink` every open task is set again against the new Goals. An `intent.org` with no `#+STATUS:` line gets one warning line (`templates/intent.md`). The record: a per-batch read of the Goals marked 143 of 168 tasks not drafted from the design as critical, in three homes, so the label said nothing about what held the MVP decision back.

Overlay can override via `critical-path.override-tasks` (explicit task list) or `critical-path.override-labels` (task_class values that are always critical-path).

### 7. User sign-off on goal-drift recommendations

If the prior integrate's `user_resolved_goal_drift` is empty but the digest carried recommendations, plan blocks until the user signs off. This is the bridge that prevents goal-drift signals from being silently ignored. The recommendation is the `decision`-kind ask file integrate wrote (`templates/ask.md`; four options: revise / split / abandon / continue). Plan reads the file's `* Answer`: an option label matched exactly satisfies the gate (`status` `answered` or `applied`); a free-text answer is read back first and the gate reports it as not signed off until the user confirms. The file is presented only under the inline rule (`templates/ask.md` § The inline rule, through the dispatcher); otherwise plan hands back `stopped` with a one-line pointer to it.

If the prior integrate had no goal-drift recommendations, this step is a no-op.

## Inputs

- The integrate→plan handshake artifact (mandatory).
- The change's `design.org`, `intent.org`, `coverage.py --intent docs/changes/<change>/intent.org`, and the open task files.
- The prior cycle's PM digest (informational).
- The project overlay's `config.yaml` (for thresholds, taxonomy, critical-path).

## Exit gate

The cycle does not enter execute until:

| Check | Condition |
|---|---|
| `prior_integrate_consumed` | Handshake artifact present with all required fields (computed) |
| `batch_composed` | Batch task list explicit and frozen (asserted: `state.py gate set plan batch_composed=true`) |
| `briefs_cite_seams` | Every task in the batch that is not `externalised` has at least one entry in `cites_seams` (computed; it checks that a citation exists, not that the id is in `design.org`) |
| `user_signed_off_goal_drift` | If prior integrate carried goal-drift recommendations, user has dispositioned them; otherwise no-op (computed from the goal-drift ask's status) |

`state.py gate check plan` shows the computed three with reasons; `state.py gate pass plan` sets the gate when all four are true; `state.py phase set execute` advances.

**Commit** at plan close, in the home: `git -C "$ORCH_HOME" add .orchestrator && git -C "$ORCH_HOME" commit -m "Orchestrator: <cycle-id> plan closed"` (add `.tasks` too when plan touched it; the batch's task files, `premise-check.md`, `state.json` with any record correction's journal line). A row a record correction amended was committed in the code root when it was made (§ 3). Ask files written during plan were committed when created (`templates/ask.md` § Commit rule).

Execute refuses to run if the plan gate has not passed.

## What this displaces

The older two-phase tick/tock model (retired on 2026-10-13; never invoked in the copied logs) conflated plan with execute. Per the brainstorm:

> Tick and tock were both *per-task* operations on the batch as it forms — implement the diff, review the diff. What was missing is a phase whose unit is the *cycle itself*, with two distinct cycle-level jobs that the two-phase model could not house: *planning the next cycle's speculations* and *integrating the prior cycle's discoveries*.

Plan is the first; integrate (`flows/integrate.md`) is the second.

## Cadence variants

- **Inline tasks**: plan collapses into "is this trivial enough?" (see `flows/inline.md`). The handshake-consumption step is skipped because inline tasks don't span cycles.
- **Standard cycles**: full plan as described above.
- **Long cycles**: plan runs once at the start; execute spans many sessions. PM digest may run periodically within execute via `/loop` or `/schedule`.
- **Several changes at once**: each open change runs its own plan / execute / integrate cycle independently, from its own worktree and state (`overlay.md` § Repositories); every `state.py` call names its change. Two changes of one repository both move its workspace integration branch, so each takes it in before integrate (`flows/integrate.md`).

## What plan does **not** do

- Plan does not modify code. (The premise check may run the suite and probes read-only; nothing in plan writes to `src/` or the test tree.)
- Plan does not redesign. (The one edit it makes to `design.org` is a record correction the premise check surfaced: one row, fixed by the orchestrator and journaled.)
- Plan does not spawn Implementors. (That's execute's job.)
- Plan-time findings are routed by integrate's rules, not acted on ad hoc; only one that blocks a batch task is settled before `batch_composed`.

If a question forces plan to do any of the above, that's a sign the prior cycle wasn't properly integrated. Refuse to advance; route back through integrate.
