# Externalisation: in-change vs `.tasks/`

Every task that gets created — by Implementor, Reviewer, Architect, PM, or the orchestrator itself — has to land somewhere. The choice between **in-change** and **externalised** governs whether the task is part of the active change or part of the cross-cutting backlog.

The rule, lifted from both existing skills and promoted to core:

| Place | Path | Use when |
|---|---|---|
| **In-change** | `<change>/tasks/open/<task-name>.md` | The task contributes to the active change's stated outcome (per `proposal.md`). Discovery happened *in* this change and resolution belongs *with* this change. |
| **Externalised** | `<repo>/.tasks/<task-name>.md` | The task is genuinely cross-cutting: it surfaced during this change but the work it requires is outside the active scope. Resolving it here would require reopening the proposal. |

When in doubt, externalise. In-change task bloat is what produces "the change never finishes"; externalised task accumulation is what the PM's externalisation-pressure signal exists to manage.

## Heuristic: would in-change resolution change the proposal?

If filing the task in-change would force the user to update `proposal.md` to acknowledge a new sub-outcome, externalise it. The proposal is the speculation about *outcome*; tasks that don't fit the speculation aren't in-change tasks.

If filing the task in-change would only require updating `design.md` (implementation strategy), it's borderline — usually still in-change because design is allowed to evolve mid-change.

If the task is a follow-up to an in-change task that just merged, with the same scope and the same modules, it's in-change. Cascading follow-ups within scope are normal.

## Heuristic: who discovered it, and where

| Discoverer | Default location |
|---|---|
| Implementor (deviation in active task) | In-change (often as `## Observations`, not a new task at all) |
| Implementor (latent bug surfaced by regression chasing) | In-change if the bug is in modules this change touches; externalised otherwise |
| Reviewer (sub-par-code finding) | In-change |
| Reviewer (design-drift finding) | In-change |
| Reviewer (spec-signal finding) | Routes to user; if user accepts, in-change for revision tasks; if rejects, no task |
| Architect (shape-fragmentation, vocabulary-mismatch on touched code) | In-change |
| Architect (responsibility-leakage on adjacent code) | Externalised — this is the *consolidation round* the Architect exists to make unnecessary; if the adjacent code is in scope, in-change |
| Architect (interface-drift against design doc) | Routes to user as integrate ask |
| PM (cascade cluster) | Spawns Architect audit; resulting tasks follow Architect rules |
| Orchestrator (manual conflict-resolution) | In-change |
| User (ad-hoc) | User chooses |

## Externalised task structure

Externalised tasks at `<repo>/.tasks/<task-name>.md` use the standard task-body template (`templates/task-body.md`) with `status: externalised`. They MUST carry full provenance fields (`discovered_from`, `discovered_by`, `discovered_class`) — without them the PM's externalisation-pressure signal can't cluster.

Externalised tasks **may** still cite register entries; this is what enables PM's cluster-coherence check ("is this cluster ready to be promoted into a change?").

## Externalisation pressure (PM's job)

PM monitors `.tasks/` growth:
- **Monotonic growth over ≥3 cycles** → externalisation-pressure signal in digest.
- **A coherent cluster** (≥3 tasks with overlapping modules and discovered_class) → ask in digest: "promote cluster X into the active change?"

The user dispositions: promote, leave externalised, or open a new change for the cluster.

PM is the **only role** that can promote externalised tasks back into the active change. Other roles can only externalise; promotion is queue-rebalancing, which is PM's domain.

## Does the orchestrator pull from `.tasks/`?

**V1: no.** The active change has its own task queue; `.tasks/` is strictly the backlog for cross-cutting work that surfaced from past changes.

This is the brainstorm's open Q5 — worth revisiting once a project routinely has dry spells in the active change while `.tasks/` accumulates pressure. Until then, `.tasks/` is pull-only-by-promotion (PM-driven), not by orchestrator-when-idle.

## Deep archive — where retired tasks live (decided for v1, used by v2)

`<repo>/.tasks/archive/` is the deep archive. When the deferred curation cycle (v2) compresses or supersedes externalised tasks, the originals move to `<repo>/.tasks/archive/<task-name>.md` and the active externalised entry retains a back-reference.

V1 doesn't write to `.tasks/archive/` — there's no curation cycle yet. The path is reserved so v2 doesn't re-decide it.

## Tasks that do NOT get filed (Observations, not tasks)

The Implementor's `## Observations` section is **the alternative to filing a task**. Out-of-scope findings that:

- Are small enough to read alongside the merged task,
- Don't constitute a user-visible bug or correctness regression,
- Are evidence the reviewer benefits from seeing in context,

→ go in `## Observations` on the merged task body. They are *not* externalised and they are *not* in-change tasks. They live with the task that surfaced them.

The reviewer reads them alongside the diff. The orchestrator scans them in execute step 5 (`flows/execute.md`) and may promote individual observations to follow-up tasks if they cross the bar.

This is the brainstorm's "where do worker observations belong" answer: in the merged task body's `## Observations`, not as new tasks. New tasks fragment context — reserve them for the rare bug-or-regression case.

## Provenance is mandatory on externalised tasks

`discovered_from`, `discovered_by`, `discovered_class` are **mandatory** on externalised tasks. Without them:

- PM's cascade detection can't see the originating cluster.
- PM's externalisation-pressure signal can't cluster by class.
- The curation cycle (v2) can't index by source.
- The meta-discovery loop has nothing structural to learn from.

The orchestrator refuses to externalise a task whose provenance fields are unset.
