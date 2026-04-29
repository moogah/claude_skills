# Register entry — boundary tier

A **boundary** entry describes a multi-stage pipeline or call chain with a contract about what's passed at each stage and what's allowed to fail where. Boundaries differ from shapes in that they carry **stage semantics**: short-circuit policies, error-propagation rules, deliberate-policy gates, idempotency requirements.

The canonical example: scope's "Bash Validation Pipeline" — five stages, each with its own input/output contract, with stage 2 designed as a hard short-circuit that consumers must not bypass. That short-circuit-is-deliberate fact is currently a prose comment; it should be a structured invariant on a boundary entry.

## Required fields

```yaml
entry_id: register/boundary/<short-name>
tier: boundary
status: speculated | confirmed | divergent | reconciled
load_bearing: true | false
title: <one-line, e.g. "bash validation pipeline">
purpose: <one paragraph — what flows through this boundary, why it's staged>

stages:
  - n: 1
    name: parse
    input: <shape-entry-id or inline description>
    output: <shape-entry-id or inline description>
    can_fail_with: [parse-error, syntax-error]
    notes: <stage-specific contract notes>
  - n: 2
    name: policy-gate
    input: parsed-ast
    output: violation-info-list | allow-pass
    can_fail_with: [policy-violation]
    short_circuit_policy: deliberate    # see below
    notes: |
      Stage 2 is a deliberate hard short-circuit. If it returns
      violations, the pipeline halts; downstream stages must not
      run. This is a security policy, not an optimisation; bypassing
      it (e.g. by running stages 3+ even on violation) re-introduces
      the bug `Stage 2 short-circuit fix` was written to close.

cross_stage_invariants:
  - <invariant-entry-id>      # invariant entries can be cited from boundaries

producers:
  - file: scope-validation.el
    function: scope/validate-bash
    enters_at_stage: 1

consumers:
  - file: scope-shell-tools.el
    function: scope/run-bash
    reads_output_of_stage: 5

discovered_from: <change-name or task-name>
discovered_by: architect
```

## Stage fields

Each stage entry describes:

- **`input`** / **`output`** — usually IDs of shape entries; inline descriptions for trivial values.
- **`can_fail_with`** — vocabulary-entry-id or inline list of error codes the stage can produce.
- **`short_circuit_policy`** — `none`, `optional`, `deliberate`. `deliberate` means consumers must honour the short-circuit; bypassing is a contract violation.
- **`notes`** — stage-local prose. Keep terse; long discussions belong in `design.md`.

## Status-specific fields

When `status: reconciled`, add:

```yaml
why_tests_missed: <typically "stages tested in isolation; no test crossed multiple stages with realistic data">
reconciliation_note_path: register/notes/<entry-id>.md
stages_changed: [<which stage indices changed>]
```

When `status: divergent`, add:

```yaml
divergent_stages: [<indices>]
escalation: architect | user
```

## When to create one

The Architect creates boundary entries during plan-phase forward-mode whenever:

- A task's brief or design-doc describes a "pipeline", "flow", or "stages".
- A code-survey finds a function whose body is a chain of `let*` / `pcase` / sequential calls with different error-handling per step.
- An interface-document drift scan finds prose describing a multi-step process that has no structured representation.

## When to mark `load_bearing: true`

Set `load_bearing: true` when:

- The boundary has a `short_circuit_policy: deliberate` stage.
- The boundary's input or output is consumed by multiple modules.
- The boundary spans a security-relevant decision (validation, authorization, scope-resolution).

Load-bearing boundaries get on-touch Architect review during execute and **end-of-cycle deep audit** during integrate.
