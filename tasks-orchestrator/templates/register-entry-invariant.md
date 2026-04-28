# Register entry — invariant tier

An **invariant** entry pins a non-shape rule that must hold across the system. The motivating failure: critical rules ("handlers fire exactly once per simple command", "every producer of violation-info goes through the canonical constructor", "redirection extraction is unconditional") existed only as prose comments or tribal knowledge. Tests pinned their own slices of the system but no test asserted the invariant directly; when the invariant broke, tests stayed green and the system silently misbehaved.

Invariants are the tier most often missing from existing register-style artifacts. The whole **interfaces.org → invariant** lift is what closes scope's "stage 2 is a deliberate policy gate" gap.

## Required fields

```yaml
entry_id: register/invariant/<short-name>
tier: invariant
status: speculated | confirmed | divergent | reconciled
load_bearing: true | false
title: <one-line, e.g. "handlers fire exactly once per simple command">

statement: |
  <One paragraph — the rule itself, stated as a positive assertion.
  Avoid hedges; an invariant either holds or doesn't. If you can't
  state it without "usually" or "in most cases", it's not an
  invariant — it's a guideline, and guidelines don't go in the
  register.>

why_it_matters: |
  <One paragraph — usually a past incident. "Bash-parser's chain
  decomposer mutated a shared var-context alist; a later cd in
  the chain corrupted the context already stored on earlier
  commands' plists. Tests passed in isolation but failed when run
  together — order-dependent." Concrete incidents anchor the
  invariant; abstract principles drift.>

enforcement_mechanism:
  kind: test | runtime-check | lint | structural-audit | type-system | review-discipline
  location: <file:function pair, OR "architect-audit:invariant-gap">
  notes: <optional — when the mechanism is non-obvious>

related_entries:
  - <other entry IDs this invariant constrains, e.g. shape entries>

discovered_from: <change-name or task-name>
discovered_by: architect | reviewer | implementor | user
```

## Enforcement-mechanism kinds

The Architect categorises by enforcement kind to avoid the **aspirational-protocol failure** (the rule was stated; nothing checked it):

| Kind | Use when | Example |
|---|---|---|
| `test` | The invariant is testable as a property | "Every public function returns a result with a non-nil `:status` key" → property test over public-fn list |
| `runtime-check` | The invariant must hold at runtime, not just at test time | "Stage 2 short-circuit is honoured" → assertion at the entry to stage 3 |
| `lint` | The invariant is syntactic | "No direct construction of violation-info; must go through canonical constructor" → grep + lint rule |
| `structural-audit` | Only the Architect's signal-class run can verify | "Module-purpose vs function-inventory match" → architect-audit |
| `type-system` | The invariant is encoded in types / contracts | Rust traits, Elm union types, etc. |
| `review-discipline` | The invariant is human-judgement only | Last resort. Carries higher drift risk; flag in PM digest as a class to consolidate |

`review-discipline` is allowed but discouraged. An invariant whose only enforcement is human review is fragile; it should migrate to a stronger mechanism as soon as one is feasible.

## Status-specific fields

When `status: reconciled`, add:

```yaml
why_tests_missed: <one sentence — typically "tests checked behaviour at sites; no test asserted the invariant directly">
reconciliation_note_path: register/notes/<entry-id>.md
prior_statement: |
  <The earlier wording, so the lineage is traceable>
```

When `status: divergent`, add:

```yaml
divergence_evidence: <where the invariant currently fails>
escalation: architect | user
```

## When to create one

The Architect creates invariant entries during plan-phase forward-mode whenever:

- A `design.md` or `proposal.md` contains a "must" or "always" or "never" or "exactly once" statement.
- An incident analysis (handoff doc, post-mortem) names a rule that, in retrospect, "should have been enforced".
- An interface-document drift scan finds prose describing a system property that has no structured representation.
- A reviewer's third-direction finding ("the spec is wrong") names a rule the design assumed but didn't enforce.

## When to mark `load_bearing: true`

Set `load_bearing: true` when:

- Violating the invariant has produced an incident in this project's history.
- The invariant's enforcement mechanism is `runtime-check` or `structural-audit` (i.e. it is enforced *in production*, not just in tests).
- The invariant constrains a security decision or a data-integrity guarantee.

Load-bearing invariants get on-touch Architect review whenever code citing them changes; end-of-cycle Architect runs an explicit **invariant-gap** audit against the full register.

## Invariants that are really shapes / vocabularies / boundaries in disguise

Before creating an invariant entry, check whether the rule can be expressed structurally:

- "Always uses the canonical constructor" → really a shape-entry with a single allowed producer.
- "Op codes are exactly these N values" → really a vocabulary entry.
- "Stage 2 short-circuits" → really a boundary entry with `short_circuit_policy: deliberate`.

If the rule fits a structural tier, use that tier; the structural tiers carry their own enforcement. Only fall back to the invariant tier when the rule genuinely can't be pinned to a shape, vocabulary, or stage. The fourth tier exists as a safety net, not the default.
