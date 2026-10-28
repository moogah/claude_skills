# Task body template

A task body is the unit-of-work document the implementor reads. It carries: what to build, which seam rows and scenarios it implements, which pending tests it promotes, what counts as a discovery, and the provenance that makes the meta-discovery loop possible.

Tasks live at the home, `.orchestrator/changes/<change>/tasks/open/<task-name>.md` (active) or `…/tasks/closed/<task-name>.md` (after merge), or `.tasks/<task-name>.md` (externalised — out of scope for the active change).

## Frontmatter

```yaml
---
task_name: <task-slug>
description: <one-line>
change: <change-name>
status: ready | in_progress | needs_review | done | failed | blocked | externalised   # the only copy of the task's status (state.md § Task status)
task_class: feature | test | doc | refactor | bug | contract | infrastructure
on_critical_path: true | false

cites_seams:                       # rows of design.org § Seams
  - seam/violation-info
  - seam/canonical-constructor
cites_scenarios:                   # '<capability> § <title>' in docs/specs/; task file only
  - scope § A violation names its resource

relations:
  blocked_by:                      # spawned once each named task has merged (flows/execute.md § 1), not after its review; `state.py check start` refuses before
    - <task-name>: { status: merged }
    - <task-name>: { status: merged }

# Task override (flows/plan.md § 5) — only when the plan moves this task off the role
# defaults (Implementor opus/medium, Reviewer opus/high; SKILL.md § Agent settings).
# Omit the block otherwise. Written by plan before batch_composed; nothing changes it after.
agent:
  implementor: sonnet, medium      # <model>, <effort>; model opus | sonnet | fable, never haiku
  # reviewer: fable, high          # optional; raise only, never below the role default
  reason: <one line the task body bears out, e.g. "one config value, steps spelled out">

# Provenance — REQUIRED on follow-up tasks (i.e. when this task was
# discovered, not user-authored; `state.py check new` refuses one without
# discovered_from and discovered_by). For top-level tasks, leave unset.
discovered_from: <source task name | architect-finding-id | reviewer-finding-id>
discovered_by: implementor | reviewer | architect | pm | user
discovered_class: shape-fragmentation | vocabulary-mismatch | responsibility-leakage
                  | dead-branch | interface-drift | mutation | invariant-gap
                  | spec-signal | deviation | scope-question | duplication
---
```

Nothing else goes in the frontmatter; `state.py conventions` reports any other key. In particular the task's merge commit is its `Merge task <name>` commit on the change branch, its review is `cycles/<cycle-id>/reviews/<name>.md`, and its times are those commits' times: each fact has one source, and a copy here would be a second one (`docs/specs/state-layer.org` § The record).

## Body sections

### Summary
One paragraph. What this task accomplishes.

### Cited seams
For each id in `cites_seams`, the row from `design.org` quoted verbatim and a short paragraph on what the implementor should pressure-test about it. Seam rows are *reference material to pressure-test, not authority to defer to*: "This row was written before the code existed; if implementation reveals it's wrong, push back in `## Discoveries`."

### Implementation steps
Numbered list. Concrete. Names files, functions, signatures.

### Verification
The exact command(s) to run, and the list of pending tests this task promotes (the tests `coverage.py --scenario` shows `pending` for its `cites_scenarios`): the implementor removes their pending marker in the implementation commit and reports the last 10 lines of output.

### Out-of-scope
What's deliberately not in this task. Reduces the "while I was there I also fixed Y" surface.

### Observations
Empty in `tasks/open/`. The implementor appends here during execution. Per the brainstorm § implementor: out-of-scope findings during implementation belong in `## Observations`, not in new tasks. The orchestrator scans them after the merge (`flows/execute.md` § 5); the Reviewer never sees them.

What belongs in `## Observations`:
- Departures from the task body's prescribed approach (with evidence)
- Tests that pass but are weakly asserted; mocks that diverge from production
- Spec/design contradictions or ambiguities the implementation forced you to resolve
- Push-backs against seam rows the brief cited
- Speculative needs, one line each labelled `thought:`

What does NOT belong in `## Observations`: a defect in code or text that exists outside the task. It is a `## Discoveries` entry with its kind (`issue` or `question`), listed in the report's `candidate_tasks`; the orchestrator routes it (`roles/implementor.md` § What belongs in `## Observations`).

### Discoveries
The structured form of significant findings. Implementor fills this when discoveries warrant integrate-phase attention. Each entry:

```markdown
- discovery_id: disc-<task-name>-<seq>
  class: <one of the discovered_class values>
  description: <one paragraph>
  affected_seam: <seam-id, if any>
  kind: issue | question                  # templates/architect-finding.md § Kind
  severity: high | medium | low
  evidence: <the line, run or output | for a question: the reasoning and the reproduction to try>
  recommendation: <one sentence — "amend seam/X to name Y as the constructor" or "split seam/Z into two">
```

Discoveries are read by the orchestrator in integrate phase alongside the conformance check. They are **not passed to the reviewer** (author-blind constraint).

## Implementor brief assembly

The orchestrator assembles the implementor's prompt by concatenating:

1. The task body (this file).
2. The cited seam rows, verbatim from `design.org`, and the cited scenarios from `docs/specs/` with their `coverage.py` rows.
3. The `design.org` decisions those rows link to.
4. The project standards (from overlay's `roles/implementor.md` if present) and the project priors.

The implementor sees all of this **before starting work**. The brief framing is fixed and lives in `roles/implementor.md`: seam rows are reference material to pressure-test, not authority to defer to.

## Externalisation

When a finding turns out to be cross-cutting (not in-scope for the active change), it gets externalised: the task moves to `.tasks/<task-name>.md` instead of `<change>/tasks/open/`. The frontmatter stays the same; only `status: externalised` and the file path change.

The full rule for in-change vs `.tasks/` lives in `externalisation.md`.
