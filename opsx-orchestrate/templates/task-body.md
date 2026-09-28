# Task body template

A task body is the unit-of-work document the implementor reads. It carries: what to build, which seam rows and scenarios it implements, which pending tests it promotes, what counts as a discovery, and the provenance that makes the meta-discovery loop possible.

Tasks live at `<change>/tasks/open/<task-name>.md` (active) or `<change>/tasks/closed/<task-name>.md` (after merge), or `.tasks/<task-name>.md` (externalised — out of scope for the active change).

## Frontmatter

```yaml
---
task_name: <task-slug>
description: <one-line>
change: <change-name>
status: ready | blocked | needs_review | done | failed | externalised
task_class: feature | test | doc | refactor | bug | contract | infrastructure
on_critical_path: true | false

cites_seams:                       # rows of design.md § Seams; also held in state
  - seam/violation-info
  - seam/canonical-constructor
cites_scenarios:                   # rows of design.md § Scenario coverage; task file only
  - scope § A violation names its resource

relations:
  blocked_by:
    - <task-name>: { status: done }
    - <task-name>: { status: done }

# Provenance — REQUIRED on follow-up tasks (i.e. when this task was
# discovered, not user-authored). For top-level tasks, leave unset.
discovered_from: <source task name | architect-finding-id | reviewer-finding-id>
discovered_by: implementor | reviewer | architect | pm | user
discovered_class: shape-fragmentation | vocabulary-mismatch | responsibility-leakage
                  | dead-branch | interface-drift | mutation | invariant-gap
                  | spec-signal | deviation | scope-question | duplication

# Filled by execute phase
merge_commit: <SHA>
findings_path: <path to reviewer findings file>

# Audit
created_at: <iso-ts>
started_at: <iso-ts>
completed_at: <iso-ts>
reviewed_at: <iso-ts>
---
```

## Body sections

### Summary
One paragraph. What this task accomplishes.

### Cited seams
For each id in `cites_seams`, the row from `design.md` quoted verbatim and a short paragraph on what the implementor should pressure-test about it. Seam rows are *reference material to pressure-test, not authority to defer to*: "This row was written before the code existed; if implementation reveals it's wrong, push back in `## Discoveries`."

### Implementation steps
Numbered list. Concrete. Names files, functions, signatures.

### Verification
The exact command(s) to run, and the list of pending tests this task promotes (from the coverage rows in `cites_scenarios`): the implementor removes their pending marker in the implementation commit and reports the last 10 lines of output.

### Out-of-scope
What's deliberately not in this task. Reduces the "while I was there I also fixed Y" surface.

### Observations
Empty in `tasks/open/`. The implementor appends here during execution. Per the brainstorm § implementor: out-of-scope findings during implementation belong in `## Observations`, not in new tasks. The reviewer reads observations alongside the merge.

What belongs in `## Observations`:
- Departures from the task body's prescribed approach (with evidence)
- Latent issues noticed in adjacent code while implementing
- Tests that pass but are weakly asserted; mocks that diverge from production
- Spec/design contradictions or ambiguities the implementation forced you to resolve
- Push-backs against seam rows the brief cited

What does NOT belong in `## Observations` and DOES merit a new task:
- A user-visible bug or correctness regression that genuinely can't wait for review

### Discoveries
The structured form of significant findings. Implementor fills this when discoveries warrant integrate-phase attention. Each entry:

```markdown
- discovery_id: disc-<task-name>-<seq>
  class: <one of the discovered_class values>
  description: <one paragraph>
  affected_seam: <seam-id, if any>
  observed:
    happened: true | false
    evidence: <where it happened | reasoning only: <the reasoning>>
  recommendation: <one sentence — "amend seam/X to name Y as the constructor" or "split seam/Z into two">
```

Discoveries are read by the orchestrator in integrate phase alongside the conformance check. They are **not passed to the reviewer** (author-blind constraint).

## Implementor brief assembly

The orchestrator assembles the implementor's prompt by concatenating:

1. The task body (this file).
2. The cited seam rows and the coverage rows the task promotes, verbatim from `design.md`.
3. The `design.md` decisions those rows link to.
4. The project standards (from overlay's `roles/implementor.md` if present) and the project priors.

The implementor sees all of this **before starting work**. The brief framing is fixed and lives in `roles/implementor.md`: seam rows are reference material to pressure-test, not authority to defer to.

## Externalisation

When a finding turns out to be cross-cutting (not in-scope for the active change), it gets externalised: the task moves to `.tasks/<task-name>.md` instead of `<change>/tasks/open/`. The frontmatter stays the same; only `status: externalised` and the file path change. PM digest tracks externalisation pressure.

The full rule for in-change vs `.tasks/` lives in `externalisation.md`.
