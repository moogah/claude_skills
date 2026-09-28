# Architect finding template

Architect findings are **structured**, **cite specific lines**, and carry a **severity that decides routing**. They go in `<repo>/.orchestrator/cycles/<cycle-id>/findings/<finding-id>.md`. The state file's `architect_findings` array carries the index fields in JSON form.

## Form

```yaml
---
finding_id: arch-<cycle-id>-<seq>
trigger: premise-check | conformance
severity: blocking | advisory | informational
class: shape-fragmentation | vocabulary-mismatch | responsibility-leakage
       | dead-branch | interface-drift | mutation | invariant-gap | duplication
title: <one-line, ≤80 chars>
discovered_from: <task-name | batch-<cycle-id>>
discovered_by: architect
discovered_at: <iso-ts>
seam: <seam/<name> this finding is about, if any>
observed:
  happened: true | false
  evidence: <a line, a run, an output, a shipped sentence | reasoning only: <the reasoning>>
---

## Locations

- file: scope-validation.el:504
  context: |
    constructs :error :tool :resource :command :message
- file: scope-shell-tools.el:181
  context: |
    constructs :tool :resource :reason :validation-type
- file: scope-expansion.el:504
  context: |
    consumes both, branches on presence

## Why tests missed it

<One sentence. The why-tests-missed line is the highest-leverage field
in this finding — it feeds the meta-discovery loop and the curation
cycle's index. Don't skip it. Typical patterns:

- "Each call site has its own test that pins its own shape; no test crosses the boundary."
- "Stages tested in isolation; no test crossed multiple stages with realistic data."
- "Per-call-site tests pinned their own subset; no test covered the full vocabulary."
- "Invariant stated in design.md; the coverage row was `none` and no test asserted it.">

## Recommended resolution

<One paragraph. Specific. Names files and functions. Avoids "consider"
and "perhaps" — the Architect's job is to recommend, not to deliberate.
When the code is right and a seam row is stale, say which row you
amended and how. Example:

"Extract canonical violation-info constructor as
build-violation-info in scope-validation.el; have callers in
scope-shell-tools.el:181 and scope-expansion.el:504 go through it;
delete the ad-hoc constructions. Amended seam/violation-info's owning
symbols to name build-violation-info as the one constructor.">
```

## Observed

`observed` says whether the problem has happened. `happened` is `true` when the finding can point at it: a failing run, a wrong output, a user report, a false statement in a shipped document (the ask's definition, `templates/ask.md` § Record), or a defect present in merged code that the finding cites by line: a function nothing calls, two bodies that are the same today. It is `false` when the harm is what would happen under a future change (a third consumer, a later sweep, a future maintainer); then `evidence` starts with `reasoning only:` and gives the reasoning. Required on `blocking` and `advisory` findings; `state.py record add findings` refuses them without it. `informational` may leave it null. What happens to a reasoning-only finding is decided once, in `flows/integrate.md` § 7: it is `noted`, not scheduled.

## Severity routing

| Severity | Effect |
|---|---|
| `blocking` | Holds the integrate gate (`blocking_findings_resolved`) until resolved: inline-fixed, follow-up task created, or the merge reverted. At plan, one whose `blocks` names a batch task is settled before `batch_composed`. |
| `advisory` | Becomes a follow-up task in the externalisation channel (in-change or `.tasks/`) when `observed.happened` is true or a prior asks for that work. Blocks nothing. |
| `informational` | Lands in the PM digest's "trends to watch" section. Blocks nothing. |

Default: `blocking` when merged code contradicts a seam row it cites or a promoted test fails; `advisory` for everything else that clears the bar. The overlay's `architect.severity-overrides` can raise or lower a class. A finding can carry `severity_override_reason` if the Architect chose a non-default severity (e.g. promoting a duplication finding to blocking because it's the third instance of the same duplication class).

## Routing

- **`interface-drift`, code right and row stale** → the Architect amends the seam row and names it here; the orchestrator records a `doc-correction` ask (`templates/ask.md`) with `status: applied`, `applied_via: design.md <seam-id>`, listed under "Applied without asking". Not Implementor work.
- **`interface-drift`, design in question** → a `decision` ask written by the orchestrator from this finding; the `recommended_resolution` must say what the seam or scenario is for, `observed` whether the drift is in merged code or reasoning only, and the finding what it blocks. The finding is not the ask.
- **`blocking`** with any other class → routes to a follow-up task in the active batch; the integrate gate doesn't close until resolved.
- **`advisory`** → follow-up task with `discovered_class` set; orchestrator decides this-batch / next-batch / `.tasks/`.
- **Any severity with `observed.happened: false`**, unless a project prior (`overlay.md` § Priors) or the user has asked for that kind of work or the finding states a measured cost → `resolution: noted`; no task, no `.tasks/` item, no `decision` ask; a stale sentence it exposed may still be a `doc-correction` (`flows/integrate.md` § 7). A `blocking` finding that rests on reasoning only is a contradiction; the Architect re-grades it `advisory`.
- **`informational`** → no task; appears in PM digest's "trends to watch" section. PM tracks recurrence; if the same informational class fires three cycles running, PM proposes promoting to advisory.

## Resolution states

The state-file `architect_findings[].resolution` field tracks how each finding was handled:

- `pending` — not yet acted on (blocks integrate gate if severity is `blocking`)
- `inline-fixed` — orchestrator applied an inline fix
- `design-amended` — the code was right and the Architect amended the seam row (and the coverage rows or Decision that named the same thing); the orchestrator's `doc-correction` ask carries the path
- `followup-task-<task-name>` — became a task; field carries the task name
- `reverted` — the implicated merge was reverted
- `accepted-with-note` — user explicitly chose to accept the divergence; carries the ask id whose `decision_readback` is the rationale
- `noted` — reasoning-only finding (`observed.happened: false`) kept in its file and not scheduled; carries no task name. Restored to the routing above if a real cycle later shows the problem happening.
