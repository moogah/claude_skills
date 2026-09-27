# Reconciliation note template

A **reconciliation note** records the lifecycle event when a `speculated` register entry moves to `confirmed`, `divergent`, or `reconciled`. The note is the audit trail that turns one cycle's discoveries into the next cycle's better speculation priors.

Notes live at `<repo>/.orchestrator/cycles/<cycle-id>/reconciliations/<tier>-<name>.md` (the entry id without `register/`, slashes as dashes; `register/shape/violation-info` → `shape-violation-info.md`). The state file's `register_touched[].reconciliation_note_path` points to the note. Notes are also long-tail: they survive in the cycle archive and are read by the curation cycle (v2) to distill cross-cycle patterns.

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
prior_note_path: <path of the note this one supersedes, or null>
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

## Entry diff

```diff
<unified diff of the entry's YAML block before and after — `diff -u`
on the two versions, or `git diff <before-commit>..<after-commit> --
<register file>` restricted to the entry when the register is
committed. Never two copies of the entry. `confirmed` omits this
section; `divergent` replaces it with `## Observed shape`.>
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

Divergent reconciliations are merge-blockers. The entry has not been changed, so in place of `## Entry diff` the note carries the actual implementation shape, and must additionally include the escalation:

```markdown
## Observed shape

<a fenced yaml block: the implementation's actual shape, no prose>

## Divergence escalation

- routes_to: architect | user
- blocks_merge_of: [<task names whose next-cycle work depends on this contract>]
- proposed_resolution: <update entry | update code | accept divergence with policy note>
- decision_pending_on: <"architect re-audit" | "follow-up task T-XXX" | ask-<cycle-id>-<seq>>
```

When `routes_to: user`, the question, options and recommendation live in the ask record (`templates/ask.md`), not here; `decision_pending_on` carries the ask id and `blocks_merge_of` becomes the ask's `blocks`. A `divergent` entry without a `divergence escalation` section is malformed; the integrate exit gate refuses to close.

## When `status_to: confirmed`

Confirmed reconciliations are the cheapest case — the speculation matched. The note can be terse:

```markdown
## What changed

Speculation matched implementation. No edit to the entry beyond `status: confirmed` and `status_changed_at`.

## Why tests missed it

N/A — speculation held.
```

The integrate gate accepts confirmed notes without an `entry diff` section, since there is no diff.

## Notes vs entries

The register entry is the **current state** and carries no history — no `status_note`, amendment or cycle-suffixed fields, dated paragraphs or `prior_*` snapshots; its `reconciliation_note_path` points at the latest note and each note's `prior_note_path` links the chain. The note is the **event log** and carries an entry diff, never two copies of the entry. Updating an entry without writing a note is a state-file bug; writing a note without updating the entry (when the `status_to` requires it) is also a state-file bug. The integrate gate checks both.

## Why this is its own artifact

The brainstorm doc names this explicitly: *the system that distinguishes a system that gets smarter from one that keeps rediscovering the same thing*. The reconciliation note is the structural fix for "learns and forgets" — once a discovery is captured here, the curation cycle can index it, future plan phases can cite it, and the meta-discovery loop has something concrete to cluster on.
