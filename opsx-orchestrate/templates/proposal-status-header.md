# Proposal status header

The proposal is the **outermost speculation** — the change's stated outcome. Like the seam rows and design decisions in `design.md`, it is written before implementation and checked against what implementation reveals; unlike them it carries a status, because the proposal's lifecycle is a user decision rather than a conformance check. Pretending the proposal is immutable once written is what produces the "ship the wrong thing on time" failure.

This header goes at the top of `proposal.md` (or `proposal.org`), under any title and tags but before the body. It gives the proposal a lifecycle of its own; seam rows in `design.md` have none (they hold current state and are amended in place).

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
divergence_signal: <pm-cascade | reviewer-spec-signal | architect-conformance | user>
divergence_evidence: |
  <One paragraph — what the gap looks like.>
divergence_ask: ask-<cycle-id>-<seq>   # the ask record (templates/ask.md) that carries the four
                                       # options revise / split / abandon / continue with their consequences
decision_pending_on: <user>

# When status: reconciled
reconciled_at: <iso-ts>
reconciled_choice: revise | split | abandon | continue-with-note
reconciled_note: |
  <One paragraph — what was the prior outcome statement, what is it
  now, why did it change. This is the proposal-level analogue of
  amending a seam row in design.md.>
prior_outcome: |
  <The proposal's outcome statement before `reconciled` was set, verbatim.
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
- **`speculated/confirmed → divergent`**: PM goal-drift signal fires (default: critical-path completion ratio stagnant or declining for K cycles while non-critical-path completions continue). PM writes the `divergence_signal` and `divergence_evidence` fields; integrate raises the `decision`-kind ask and presents it (`flows/integrate.md` § 3a).
- **`divergent → reconciled`**: User chooses revise / split / abandon / continue. The ask record captures the choice, and the handshake's `user_resolved_goal_drift` carries `{ask, decision, rationale}`.
- **`reconciled → speculated`**: When `reconciled_choice` is `revise` or `split`, the new proposal text re-enters the lifecycle as `speculated`; the prior outcome statement is preserved as `prior_outcome`.

## Cost asymmetry

The brainstorm names the cost-asymmetry rule: amending a seam row in `design.md` costs minutes, revising a design decision costs hours, proposal.md revision costs days. The system biases toward absorbing discoveries at the lowest level that can hold them, but must not *hide* the expensive option when it's warranted.

The status header is what makes goal-drift visible. Without it, goal-invalidating signals get silently absorbed as seam-row amendments until the gap is too big to ignore — the cleanup-round pattern.

## Where this lives

- **OpenSpec projects**: prepend this header to `openspec/changes/<change>/proposal.md`. The orchestrator's plan phase checks for the header on every cycle and warns if missing.
- **Non-OpenSpec projects**: out of scope for v1; the orchestrator assumes OpenSpec. (Brainstorm Q7, deferred post-v1.)

## When the header is missing

The plan phase emits a warning the first cycle. The integrate exit gate does **not** block — the header is informational at this stage; we don't yet have enough data on whether silent goal-drift is the right default to refuse it. Promote to a hard gate after VCE migration if measurement supports it.
