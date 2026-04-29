# Task update stanza template

When integrate's open-task refinement step (`flows/integrate.md` § 7) determines an open task body should absorb cycle learning but the existing prose remains valid, a structured stanza is appended. Tasks may accumulate stanzas across cycles, newest-last.

When the existing prose is *demonstrably false or dead*, integrate edits in place instead — see "Edit-in-place rule" at the bottom of this file.

## Where the stanza goes

Append at the bottom of the task body, after `## Out-of-scope` and before `## Observations` / `## Discoveries` (those are execute-phase artifacts and integrate must not write to them). If the task body has no `## Out-of-scope` section, append after the last existing section that is not `## Observations` / `## Discoveries`.

## Stanza form

```markdown
## Cycle <N> updates (cycle-<ts>)

### Cited register entries
- `register/<tier>/<entry-id>`: <prior status> → <new status>. <One-line of what changed>. See `<reconciliation-note-path>`.

### User-resolved decisions
- `<ask-id>`: <one-line summary of the question and the user's choice>. **Implication for this task**: <one sentence>.

### Meta-discoveries
- `<kind>` / `<label>`: <implication-for-next-plan, reframed concretely for this task — one sentence>.

### Already-shipped inline fixes
- `<finding-id>`: <one-line of what was fixed and where>. **Implication for this task**: <usually "step X is now no-op", "verify the fix held", or "this task no longer needs subtask Y">.

### Obsolescence flag
> Cycle <N> claims this task may now be wholly obsolete because <one-sentence reason>. **User disposition required**: close, refine, or rescope. Do not start work until dispositioned.
```

Each H3 subsection is optional; omit empty subsections. The "Obsolescence flag" subsection appears only when integrate's refinement set `obsolescence_flagged: true` on the task's `task_refinements` entry.

## Multiple stanzas across cycles

A task that survives several cycles without being executed may accumulate stanzas. They stack newest-last. Older stanzas are not edited; future readers see the cycle-by-cycle drift.

If a later cycle determines that a prior stanza's claim is itself now stale (e.g. cycle N flagged obsolescence, cycle N+2 reversed it), append a new stanza with a top-level `> Cycle <N+2>: supersedes Cycle <N>'s obsolescence flag — <one-sentence reason>.` quote line. Don't edit the older stanza.

## Edit-in-place rule (the alternative path)

When prose is **demonstrably false or dead**, edit in place instead of appending. Triggers:

- Prose names a register-entry shape, field, or vocabulary member that was reconciled away this cycle (not present in the new shape).
- Prose prescribes a code change (numbered step, file edit, function add/remove) that an inline fix or merged in-cycle task already shipped.
- Prose cites a `file:fn` that was deleted or renamed by an inline fix this cycle.
- A verification command references an artifact that no longer exists.

Replace the false text with the corrected statement. At the top of the section that was edited, leave a single-line breadcrumb:

```markdown
> Cycle <N>: obviated/corrected by inline fix; see <reconciliation-note-path-or-finding-id>.
```

Don't leave dead prose; do leave an audit trail. If a numbered step is *fully* obviated (the work shipped elsewhere with no remaining residue), delete the step rather than striking it through, and reflect the deletion in the breadcrumb: `> Cycle <N>: original step <K> ('<short summary>') shipped via <ref>; removed.`.

## Why both modes exist

Append-only is safer (preserves user-tuned prose, easy to audit) but accumulates. Edit-in-place is cleaner but risks lossy edits. The split — false/dead → in-place; everything else → append — keeps the audit trail visible while preventing the open task list from drifting into self-contradiction.

## What this template does NOT do

- It does not replace the cited register entries' own reconciliation notes (`templates/reconciliation-note.md`) — those are the authoritative event log per entry.
- It does not capture findings the implementor discovered mid-task — those go in `## Observations` / `## Discoveries` per `templates/task-body.md`.
- It does not record cycle-N learnings into a closed task. Tasks in `<change>/tasks/closed/` are frozen at merge time and integrate does not touch them.
