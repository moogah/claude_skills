# Architect finding template

Architect findings are **structured**, **cite specific lines**, and carry a **severity that decides routing**. They go in `<repo>/.orchestrator/cycles/<cycle-id>/findings/<finding-id>.md`. The state file's `architect_findings` array carries the same fields in JSON form.

## Form

```yaml
---
finding_id: arch-<cycle-id>-<seq>
trigger: on-touch | end-of-cycle | between-cycle
severity: blocking | advisory | informational
class: shape-fragmentation | vocabulary-mismatch | responsibility-leakage
       | dead-branch | interface-drift | mutation | invariant-gap | duplication
title: <one-line, ≤80 chars>
discovered_from: <task-name | batch-id | "whole-repo">
discovered_by: architect
discovered_at: <iso-ts>
register_entry: <entry-id this finding maps to, if any>
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
- "Invariant stated in design doc; no test asserted it directly.">

## Recommended resolution

<One paragraph. Specific. Names files and functions. Avoids "consider"
and "perhaps" — the Architect's job is to recommend, not to deliberate.
Example:

"Extract canonical violation-info constructor as
build-violation-info in scope-validation.el; have callers in
scope-shell-tools.el:181 and scope-expansion.el:504 go through it;
delete the ad-hoc constructions. The shape entry
register/shape/violation-info should move from speculated to
reconciled; the reconciliation note's entry diff captures the three
divergent forms.">
```

## Observed

`observed` says whether the problem has happened. `happened` is `true` when the finding can point at it: a failing run, a wrong output, a user report, a false statement in a shipped document (the ask's definition, `templates/ask.md` § Record), or a defect present in merged code that the finding cites by line: a function nothing calls, two bodies that are the same today. It is `false` when the harm is what would happen under a future change (a third consumer, a later sweep, a future maintainer); then `evidence` starts with `reasoning only:` and gives the reasoning. Required on `blocking` and `advisory` findings; `state.py record add findings` refuses them without it. `informational` may leave it null. What happens to a reasoning-only finding is decided once, in `flows/integrate.md` § 7: it is `noted`, not scheduled.

## Severity routing

| Severity | Effect |
|---|---|
| `blocking` | Pauses merge of the implicated task(s). Must be resolved (inline-fixed, follow-up task created, or merge reverted) before integrate phase can close. |
| `advisory` | Becomes a follow-up task in the externalisation channel (in-change or `.tasks/`). Doesn't block merge. |
| `informational` | Lands in the PM digest's "trends to watch" section. Doesn't block anything. |

Severity is set per-class with an overlay-configurable default. Core defaults:

| Class | Default severity |
|---|---|
| `shape-fragmentation` | blocking |
| `vocabulary-mismatch` | blocking |
| `responsibility-leakage` | advisory |
| `dead-branch` | advisory |
| `interface-drift` | blocking (if against load-bearing entry) / advisory (otherwise) |
| `mutation` | advisory |
| `invariant-gap` | advisory |
| `duplication` | advisory |

The overlay's `architect.severity-overrides` field can override per-class. A finding can also carry `severity_override_reason` if the Architect chose a non-default severity (e.g. promoting a duplication finding to blocking because it's the third instance of the same duplication class).

## Routing

- **`blocking`** with `interface-drift` against an out-of-date design doc → an ask record per `templates/ask.md`, triaged: `doc-correction` when the code is right and the document is stale (applied and listed), `decision` when the design is in question (asked). Not implementor work. (The reviewer's "spec is wrong" direction; the highest-value findings.) The finding's `recommended_resolution` must say what the document or rule is for; `observed` says whether the drift is in a shipped document or reasoning only.
- **`blocking`** with any other class → routes to a follow-up task in the active batch; merge of the implicated task pauses; integrate gate doesn't close until resolved.
- **`advisory`** → follow-up task with `discovered_class` set; orchestrator decides this-batch / next-batch / `.tasks/`.
- **Any severity with `observed.happened: false`**, unless a project prior (`overlay.md` § Priors) or the user has asked for that kind of work or the finding states a measured cost → `resolution: noted`; no task, no `.tasks/` item, no `decision` ask; a stale sentence it exposed may still be a `doc-correction` (`flows/integrate.md` § 7). A `blocking` finding that rests on reasoning only is a contradiction; the Architect re-grades it `advisory`.
- **`informational`** → no task; appears in PM digest's "trends to watch" section. PM tracks recurrence; if the same informational class fires three cycles running, PM proposes promoting to advisory.

## Resolution states

The state-file `architect_findings[].resolution` field tracks how each finding was handled:

- `pending` — not yet acted on (blocks integrate gate if severity is `blocking`)
- `inline-fixed` — orchestrator applied an inline fix
- `followup-task-<task-name>` — became a task; field carries the task name
- `reverted` — the implicated merge was reverted
- `accepted-with-note` — user explicitly chose to accept the divergence; carries the ask id whose `decision_readback` is the rationale
- `noted` — reasoning-only finding (`observed.happened: false`) kept in its file and not scheduled; carries no task name. Restored to the routing above if a real cycle later shows the problem happening.
