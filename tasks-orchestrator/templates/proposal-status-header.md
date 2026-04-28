# Proposal status header

The proposal is the **outermost speculation** — the change's stated outcome. Like every other speculation in the system (register entries, design decisions), it has a status, and like every other speculation it can be reconciled. Pretending the proposal is immutable once written is what produces the "ship the wrong thing on time" failure.

This header goes at the top of `proposal.md` (or `proposal.org`), under any title and tags but before the body. It mirrors the register-entry lifecycle.

## Form

```markdown
---
status: speculated | confirmed | divergent | reconciled
status_set_at: <iso-ts>
status_set_by: user | pm | architect

# When status: confirmed
confirmed_after_cycle: <cycle-id>
confirmed_basis: |
  <One paragraph — what stabilised the proposal? "Two cycles of
  forward chain executed without goal-drift signals" is a
  defensible answer.>

# When status: divergent
divergence_signal: <pm-cascade | reviewer-spec-signal | architect-interface-drift | user>
divergence_evidence: |
  <One paragraph — what the gap looks like.>
divergence_options:
  - revise: <one-line — what would the proposal say after revising?>
  - split: <one-line — what would the two proposals be?>
  - abandon: <one-line — what would unwinding cost?>
decision_pending_on: <user>

# When status: reconciled
reconciled_at: <iso-ts>
reconciled_choice: revise | split | abandon | continue-with-note
reconciled_note: |
  <One paragraph — what was the prior outcome statement, what is it
  now, why did it change. This is the proposal-level analogue of a
  register reconciliation note.>
prior_outcome: |
  <The proposal's outcome statement before reconciliation, verbatim.
  Prefix the body of proposal.md with the new outcome statement;
  this header preserves the prior version.>
---
```

## Status semantics

| Status | Meaning |
|---|---|
| `speculated` | Proposal is fresh; the forward chain has not yet stabilised against it |
| `confirmed` | The forward chain has executed cleanly for ≥2 cycles with no goal-drift signals |
| `divergent` | A goal-drift signal is open; the user has not yet chosen revise / split / abandon / continue |
| `reconciled` | The user resolved the goal-drift signal; carries `reconciled_choice` and `reconciled_note` |

## Lifecycle hooks

- **`speculated → confirmed`**: PM digest checks this transition automatically once the integrate→plan handshake has fired ≥2 cycles cleanly. Set programmatically; no user action required.
- **`speculated/confirmed → divergent`**: PM goal-drift signal fires (default: critical-path completion ratio stagnant or declining for K cycles while non-critical-path completions continue). PM writes the `divergence_signal` and `divergence_evidence` fields; user is asked in the digest.
- **`divergent → reconciled`**: User chooses revise / split / abandon / continue. The digest captures the choice in the `user_resolved_goal_drift` field of the integrate→plan handshake artifact.
- **`reconciled → speculated`**: When `reconciled_choice` is `revise` or `split`, the new proposal text re-enters the lifecycle as `speculated`; the prior reconciliation note is preserved as `prior_outcome`.

## Cost asymmetry

The brainstorm names the cost-asymmetry rule: register reconciliation costs minutes, design.md revision costs hours, proposal.md revision costs days. The system biases toward absorbing discoveries at the lowest level that can hold them, but must not *hide* the expensive option when it's warranted.

The status header is what makes goal-drift visible. Without it, goal-invalidating signals get silently absorbed as register noise until the gap is too big to ignore — the cleanup-round pattern.

## Where this lives

- **OpenSpec projects**: prepend this header to `openspec/changes/<change>/proposal.md`. The orchestrator's plan phase checks for the header on every cycle and warns if missing.
- **Non-OpenSpec projects**: out of scope for v1; the orchestrator assumes OpenSpec. (Brainstorm Q7, deferred post-v1.)

## When the header is missing

The plan phase emits a warning the first cycle. The integrate exit gate does **not** block — the header is informational at this stage; we don't yet have enough data on whether silent goal-drift is the right default to refuse it. Promote to a hard gate after VCE migration if measurement supports it.
