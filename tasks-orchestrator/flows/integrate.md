# Integrate phase

The backward channel firing, plus the loop-closing operation. Integrate consumes the cycle's discoveries and produces the inputs the next plan requires.

A cycle that produces follow-up tasks but leaves speculated entries un-dispositioned has discovered without integrating — the failure the integrate phase exists to prevent. Integrate's exit gate enforces this structurally; it does not close until **every touched register entry has a disposition**.

## Operations

### 1. Register reconciliation (the load-bearing operation)

Enumerate every speculated entry the cycle touched (from the state file's `register_touched` array). For each:

- **`speculated → confirmed`** if the implementation matched. Write a terse reconciliation note (per `templates/reconciliation-note.md`) recording "speculation matched".
- **`speculated → divergent`** if the implementation pushed back AND the divergence is unresolved. Write a divergent reconciliation note with `routes_to: architect | user` and `proposed_resolution`. **Divergent entries block merge** of any task that cited them, until resolved.
- **`speculated → reconciled`** if the entry was updated to match the discovery. Write a reconciliation note with `prior_shape`, `new_shape`, and the **mandatory `why_tests_missed` line**.

The state file's `register_touched[i].status_at_integrate` flips to one of `confirmed` / `divergent` / `reconciled` for every touched entry. An entry whose `status_at_integrate` remains null is a state-file bug; the integrate gate refuses to close.

This is the gate that distinguishes a system that gets smarter from one that keeps rediscovering the same thing.

### 2. Architect end-of-cycle audit

Full signal-class run across all the cycle's diffs + the register. (Not a separate trigger; one of integrate's defining operations. See `roles/architect.md` for the eight signal classes.)

Output: zero or more findings written to `<repo>/.orchestrator/cycles/<cycle-id>/findings/`. Each finding has a severity that decides routing:

- **`blocking`** with `interface-drift` against an out-of-date design doc → routes to **the user** as an integrate-phase ask.
- **`blocking`** with any other class → produces a follow-up task in the active batch; merge of the implicated task pauses; integrate gate doesn't close until the finding's `resolution` is no longer `pending`.
- **`advisory`** → follow-up task with provenance; orchestrator decides this-batch / next-batch / `.tasks/`.
- **`informational`** → no task; PM digest's "trends to watch" section.

### 3. PM digest

Two passes, per `roles/project-manager.md`:

1. **Deterministic pass** — produces `<repo>/.orchestrator/cycles/<cycle-id>/pm-signals.json` with all counts, ratios, fired-signals list, candidate-asks list.
2. **Agent pass** — turns that into the digest prose at `<repo>/.orchestrator/cycles/<cycle-id>/pm-digest.md` (per `templates/pm-digest.md`).

If the agent pass fails, the deterministic output remains and is recoverable.

### 4. Meta-discovery surfacing

The PM's "trends to watch" + the Architect's pattern-class clusters get distilled into updated speculation priors for the next plan.

Examples:
- "We keep finding vocabulary mismatches at the bash-parser/scope boundary." → Future forward speculation in that area should probe vocabulary first.
- "Cascade follow-ups cluster around responsibility-leakage in scope-expansion.el." → That module's stated responsibility is drifting; weight its module-purpose audit higher next cycle.

Meta-discoveries land in the integrate→plan handshake artifact's `meta_discoveries` field, structured as:

```json
{
  "kind": "vocabulary-cluster | shape-fragmentation-cluster | invariant-gap-class | other",
  "scope": "<module / boundary / change name>",
  "evidence": ["<task-id>", "<task-id>", ...],
  "implication_for_next_plan": "<one sentence>"
}
```

Per-cycle meta-discoveries surface in this digest and act on the next plan. Recurring meta-discoveries across many cycles distill, via the deferred curation cycle (v2), into durable speculation priors.

### 5. Goal-drift check

Does the gap between "tasks complete" and "proposal.md outcome reachable" suggest the proposal itself is drifting? The PM's goal-drift query fires when:

- Critical-path completion ratio is stagnant or declining for K cycles.
- Non-critical-path completions continue at normal pace.
- Drainage stays positive (so it's not throughput inversion).

If the query fires:
- The PM digest carries a goal-drift recommendation: revise / split / abandon / continue.
- The proposal status header (per `templates/proposal-status-header.md`) flips to `divergent`.
- An ask routes to the user with the three structured options.

The user's decision lands in the handshake artifact's `user_resolved_goal_drift` field. Plan refuses to start the next cycle until the user has dispositioned (or explicitly chosen `continue`).

### 6. Externalisation review

`.tasks/` pressure check — has the cross-cutting backlog grown to the point where a cluster should be promoted into the active change?

The PM checks: of the externalised tasks, is there a cluster whose `discovered_class` distribution and modules-touched are coherent enough to constitute a sub-change? If yes:

- Surface as an ask: "promote cluster `vocabulary-mapping` (5 tasks, 3 cycles old) into the active change?"
- The user dispositions: promote, leave externalised, or open a new change.

### 7. Write the integrate→plan handshake artifact

The cycle's loop-closing artifact. Path: `<repo>/.orchestrator/handshake-<cycle-id>.json`.

```json
{
  "cycle_id": "<this-cycle>",
  "produced_at": "<iso-ts>",
  "register_diff": [
    { "entry_id": "...", "from": "speculated", "to": "reconciled", "note_path": "..." }
  ],
  "pm_digest_path": ".orchestrator/cycles/<cycle-id>/pm-digest.md",
  "meta_discoveries": [...],
  "user_resolved_goal_drift": [...],
  "asks_for_user_open": [],
  "asks_for_user_resolved": []
}
```

**All four required fields are mandatory.** An empty list is allowed; a missing field is not. The next plan's first operation is to read this file; if any field is missing, plan refuses to start.

This is the structural fix for the brainstorm's "learns and forgets" failure mode. Without the handshake, plan degrades into "pull from the top of the backlog" and the orchestrator becomes a queue runner.

## Inputs (from execute)

- All Implementor reports (held by orchestrator since execute, not seen by reviewers).
- All Reviewer findings.
- All Architect on-touch findings from execute.
- The set of merged diffs.
- The set of touched register entries (`state.json` `register_touched`).
- `phase_gates.execute.passed: true` (mandatory).

## Exit gate

| Check | Condition |
|---|---|
| `all_touched_entries_dispositioned` | Every `register_touched[i].status_at_integrate` is set (not null) |
| `blocking_findings_resolved` | Every `architect_findings[i]` with `severity: blocking` has `resolution != pending` |
| `pm_digest_produced` | `pm-digest.md` exists with non-empty `signals` and `asks` sections |
| `user_asks_routed` | Every entry in PM digest's `Asks for the user` section has a corresponding entry in `handshake.asks_for_user_open` (so plan picks them up next cycle) |
| `handshake_artifact_written` | `handshake-<cycle-id>.json` exists with all four required fields populated |

When all five pass, `phase_gates.integrate.passed` flips to `true`. The next plan refuses to start otherwise — that's the loop-closure contract.

## Cycle archive

When integrate closes successfully, the orchestrator archives the cycle:

```
.orchestrator/cycles/<cycle-id>/
  state.json              # frozen snapshot of the cycle's state file
  pm-digest.md
  handshake.json
  findings/<finding-id>.md
  reconciliations/<entry-id>.md
  reviews/<task-name>.md
  baseline-<ts>.txt
  after-<task>-<ts>.txt
```

The active state file at `<repo>/.orchestrator/state.json` is reset for the next cycle (with carried-over `history` window).

This archive is what the deferred curation cycle (v2) reads. It is also the audit trail the user can walk through to understand any past decision.

## Loop closure: integrate → plan

The keystone transition is integrate → plan, **not** execute → review. Each plan is *required* to consume the prior integrate's outputs. Without that as a hard input contract, plan degrades into queue-running and the orchestrator becomes a glorified task runner.

This is the loop that gives the system *compounding leverage*: each cycle's discoveries make the next cycle's speculations better. It is the operationalisation, at cycle altitude, of the two-way information flow.

## What integrate does **not** do

- Integrate does not modify code. (Reconciliation notes update register entries, which are artifacts; inline fixes from blocking findings happen in the active batch's worktrees, which is the orchestrator's responsibility but happens in execute-mode mechanics — `git merge --abort` then re-spawn — even if triggered from integrate's findings.)
- Integrate does not run new test suites — the cycle's tests already ran in execute.
- Integrate does not re-implement tasks — re-implementation is execute's job; integrate only routes findings.
