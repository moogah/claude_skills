# Reconciliation note template

A **reconciliation note** records the lifecycle event when a `speculated` register entry moves to `confirmed`, `divergent`, or `reconciled`. The note is the audit trail that turns one cycle's discoveries into the next cycle's better speculation priors.

Notes live at `<repo>/.orchestrator/cycles/<cycle-id>/reconciliations/<entry-id>.md`. The state file's `register_touched[].reconciliation_note_path` points to the note. Notes are also long-tail: they survive in the cycle archive and are read by the curation cycle (v2) to distill cross-cycle patterns.

## Form

```markdown
---
entry_id: register/shape/violation-info
tier: shape
cycle_id: cycle-<ts>
status_from: speculated
status_to: reconciled
load_bearing: true
discovered_from:
  - task-extract-canonical-violation-info-constructor
discovered_by:
  - implementor
  - architect    # for the on-touch finding that produced the task
recorded_at: <iso-ts>
---

## What changed

<One paragraph. Concrete. Names the prior assumption and what now
holds.

Example:
"The register entry stated violation-info had three optional keys
(:reason, :message, :error) and three required (:tool, :resource,
:command). Implementation collapsed the three error fields into a
single required :reason, dropping :message and :error in producers
and adding a single fallback in consumers. Required keys now: :tool,
:resource, :command, :reason.">

## Why tests missed it

<One sentence. The single highest-leverage line in the note. Feeds
the meta-discovery loop and the curation cycle's index.

Example:
"Each call site had its own test pinning its own subset of the three
error keys; no test crossed the boundary or asserted the union, so
divergent shapes all passed.">

## Prior shape (verbatim)

```yaml
<the entry's prior fields, exactly as they were before the
reconciliation, so future readers can diff and so the lineage is
traceable.>
```

## New shape (verbatim)

```yaml
<the entry's new fields. If status_to is `confirmed`, this section
is identical to what was speculated and can be elided with a
"unchanged from speculation" note. If `reconciled`, this is the
patched form. If `divergent`, this section captures the actual
implementation shape (which the entry has not yet been updated to
match).>
```

## Meta-discovery hooks

<Optional. Include only when the reconciliation pattern repeats
something seen in prior cycles. The PM digest reads these to update
speculation priors.

Example:
"This is the third reconciliation in 4 cycles where a 'three error
fields' speculation collapsed to one. The forward-mode prior
'separate error codes for separate failure modes' is over-fitting at
this boundary; future shape entries at scope/* should default to a
single :reason field unless evidence demands more.">
```

## When `status_to: divergent`

Divergent reconciliations are merge-blockers. The note must additionally include:

```markdown
## Divergence escalation

- routes_to: architect | user
- blocks_merge_of: [<task names whose merges depend on this contract>]
- proposed_resolution: <update entry | update code | accept divergence with policy note>
- decision_pending_on: <"user decision on error-shape question" | "architect re-audit" | "follow-up task T-XXX">
```

A `divergent` entry without a `divergence escalation` section is malformed; the integrate exit gate refuses to close.

## When `status_to: confirmed`

Confirmed reconciliations are the cheapest case — the speculation matched. The note can be terse:

```markdown
## What changed

Speculation matched implementation. No edit to the entry.

## Why tests missed it

N/A — speculation held.
```

The integrate gate accepts confirmed notes without a `prior shape` / `new shape` section, since they're identical.

## Notes vs entries

The reconciliation note is the **event log**. The register entry is the **current state**. Updating an entry without writing a note is a state-file bug; writing a note without updating the entry (when the `status_to` requires it) is also a state-file bug. The integrate gate checks both.

## Why this is its own artifact

The brainstorm doc names this explicitly: *the system that distinguishes a system that gets smarter from one that keeps rediscovering the same thing*. The reconciliation note is the structural fix for "learns and forgets" — once a discovery is captured here, the curation cycle can index it, future plan phases can cite it, and the meta-discovery loop has something concrete to cluster on.
