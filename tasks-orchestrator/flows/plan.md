# Plan phase

The forward channel firing. Plan produces the speculations the next execute phase will probe.

A plan that doesn't consume the prior cycle's integrate output isn't planning — it's queue-running. The integrate→plan handshake artifact is a hard input contract; plan **refuses to start** if the artifact is missing or any of its required fields are unset.

## Operations

### 1. Consume the prior integrate's handshake

Read `<repo>/.orchestrator/handshake-<prior-cycle-id>.json`. Must have:

- `register_diff` — what's now `confirmed` / `divergent` / `reconciled` that was `speculated`.
- `pm_digest_path` — the prior digest, for context.
- `meta_discoveries` — patterns to update speculation priors against.
- `user_resolved_goal_drift` — any revise / split / abandon decisions the user made.
- `asks_for_user_open` (may be empty), `asks_for_user_resolved` (may be empty).

If the file is missing or any field is missing (not "empty array" — actually missing), refuse to start. Direct the user to close the prior cycle properly or to abandon it explicitly.

This is the brainstorm's loop-closure contract: each cycle's discoveries must update the next cycle's speculations.

### 2. Architect forward-mode — populate / revise speculative register entries, then generate scaffolding

The Architect runs in forward mode against the change's `proposal.md` and `design.md`:

- **At `/opsx-new` time**: populate `boundary` and `invariant` tier entries — the "what must hold" skeleton.
- **At `/opsx-tasks generate` time** (which happens during plan): populate `shape` and `vocabulary` tier entries — the "concrete contracts" fill.

The split is configured by the overlay's `forward-mode.populate-at` (default: both).

New entries land as `status: speculated`. Entries the prior integrate marked `divergent` are **re-stated**, **absorbed**, or **escalated**:

- *Re-stated*: the divergent entry was wrong; rewrite it with a fresh speculation.
- *Absorbed*: the divergent state has been resolved by an in-flight task or external event; mark `reconciled`.
- *Escalated*: the divergence is genuine and needs user disposition; carry into the plan as a goal-drift candidate.

**Scaffolding generation runs immediately after each tiered entry is populated**, in the same forward-mode invocation. For every newly populated or re-stated speculative entry whose tier is in the project's `scaffolding.tiers` (defaults: `invariant`, `vocabulary`, `boundary`; `shape` opt-in), the Architect writes a scaffolded file under `<change>/scaffolding/<tier>/<entry-id>.<ext>`:

- A failing test (per `scaffolding.failing-stub-style`) for invariants.
- A `pcase` / match scaffold listing every speculated value as an `error "TODO"` arm for vocabularies.
- A canonical mapping function with a TODO body for boundaries.
- A constructor + destructor exercise for shapes (when shape ∈ tiers).

Each file carries a `scaffolding-of: <entry-id>` header; the entry gains a `scaffolding_path` field. Scaffolded tests must fail loudly until satisfied — green-on-empty is a defect. Full contract: **[../scaffolding.md](../scaffolding.md)**.

If `scaffolding.enabled: false` in the overlay, this sub-step is a no-op and tiered entries retain their `validator` / `test_corpus` YAML fields as today.

### 3. Batch composition

Select which speculated entries this cycle will probe. **Variety is deliberate.** Probing only the safe tier (e.g. only shape entries) produces "tasks complete green; change doesn't converge" — the brainstorm's forward-fail mode.

The batch composer balances:
- A mix of register tiers (don't skip vocabulary or invariant).
- Critical-path coverage: ≥1 task on the critical path per cycle, unless the prior integrate explicitly deferred.
- Load-bearing entries get on-touch attention budget reserved.
- Total batch size: project-overlay-configurable; default 3–7 tasks.

### 4. Task generation

Project the chosen speculations into units of work via `/opsx-tasks generate` (or the overlay-configured per-project task generator):

- Each shape entry implies producer-and-consumer tasks.
- Each invariant entry implies an enforcement-mechanism task.
- Each boundary entry implies a contract-test task or load-time validator task.
- Each vocabulary entry implies a canonical-mapping-function task plus tests.

Generated tasks land in `<change>/tasks/open/<task-name>.md` with their `cites_register_entries` field populated against the entries they derive from.

### 5. Implementor brief assembly

For each generated task, the orchestrator assembles the brief at agent-spawn time (per `roles/implementor.md`):

- Task body.
- Cited register entries (full text, with `status` annotations).
- **Scaffolded files** for each cited entry that has a `scaffolding_path` (full path + revision-licence framing per `scaffolding.md`).
- Cited `design.md` / `proposal.md` sections.
- Project standards (overlay's `roles/implementor.md`).

The brief framing is fixed — register entries and scaffolding are *reference material to pressure-test, not authority to defer to*. Speculated entries and scaffolded files carry explicit licence to push back. Modifying a scaffolded file is a signal at integrate, not a transgression.

### 6. PM critical-path read

One LLM read of `proposal.md` to identify which tasks are on the critical path. Sets `on_critical_path` on every task in the batch. Cheap; runs once per plan phase, not per PM tick.

Overlay can override via `critical-path.override-tasks` (explicit task list) or `critical-path.override-labels` (task_class values that are always critical-path).

### 7. User sign-off on goal-drift recommendations

If the prior integrate's `user_resolved_goal_drift` is empty but the digest carried recommendations, plan blocks until the user signs off. This is the bridge that prevents goal-drift signals from being silently ignored.

If the prior integrate had no goal-drift recommendations, this step is a no-op.

## Inputs

- The integrate→plan handshake artifact (mandatory).
- The interfaces register at the project root (current state).
- The change's `proposal.md` and `design.md`.
- The prior cycle's PM digest (informational).
- The project overlay's `config.yaml` (for thresholds, taxonomy, critical-path, forward-mode).

## Exit gate

The cycle does not enter execute until:

| Check | Condition |
|---|---|
| `prior_integrate_consumed` | Handshake artifact read; all required fields present |
| `batch_composed` | Batch task list explicit and frozen |
| `briefs_cite_register` | Every task in batch has at least one entry in `cites_register_entries` |
| `scaffolding_generated_for_tiered_entries` | Every speculated entry whose tier is in `scaffolding.tiers` has a `scaffolding_path` set and the file exists with a valid `scaffolding-of` header. No-op when `scaffolding.enabled: false` |
| `user_signed_off_goal_drift` | If prior integrate carried goal-drift recommendations, user has dispositioned them; otherwise no-op |

All five must be `true` in `phase_gates.plan.checks`. The state file's `phase_gates.plan.passed` flips to `true` only when all pass.

Execute refuses to run if `phase_gates.plan.passed` is `false`.

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

- Plan does not modify code. (Architect forward-mode modifies the register; that's an artifact, not code.)
- Plan does not run tests or verifications.
- Plan does not spawn Implementors. (That's execute's job.)
- Plan does not produce findings. (That's execute's and integrate's jobs.)

If a question forces plan to do any of the above, that's a sign the prior cycle wasn't properly integrated. Refuse to advance; route back through integrate.
