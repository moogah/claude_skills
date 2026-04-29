# Curation cycle (v2 — DEFERRED)

This file is a **placeholder**. The curation cycle is specified in the brainstorm doc (`~/org/roam/20260426113643-task_orchestrator_skills.org` § Curation cycle) but is **deferred to v2**. V1 ships without it.

## Why deferred

The curation cycle is the slower-tempo loop that maintains the read-model projection of the corpus (register, indices, distilled meta-discoveries) so the prose corpus can grow without polluting plan-phase context.

It only fires once corpus pollution is real. Specifying it against a hypothetical corpus produced none of the discrimination the design needs:

- What's a "cluster" depends on what classes of finding actually recur in the project.
- What's a useful "compression" depends on which prose readers re-walk.
- What's a useful "index key" depends on what queries plan actually runs.

V1 ships, projects accumulate corpora through some cycles, and **then** v2 specifies curation against measured pollution patterns.

## V1 placeholder behaviour

Where v1 needs a curation hook, it does the cheapest thing:

- **Distill**: not done. Discoveries surfaced by integrate's reconciliation lifecycle remain in their reconciliation notes; the register is not edited beyond what reconciliation produces.
- **Cluster**: PM does the simplest cluster detection (cascade signal: `discovered_from` clustering). No cross-cycle clustering.
- **Compress**: not done. Cycle archives remain verbatim.
- **Index**: not done. The register itself is the only index v1 has.
- **Prune**: not done. Default to keep.

The cost: plan phase reads the full register. As the register grows, this cost grows. V2 curation distills the register hot path; v1 lives with the linear growth.

## Triggers (specified now to avoid v2 painting itself into a corner)

The triggers v2 will use are spec'd in v1 so the state file has the right fields:

- **Corpus-size delta** over N cycles exceeds threshold (more prose added than the read model is absorbing).
- **Closed-but-unindexed task count** crosses threshold.
- **Externalisation pressure** signal from PM crosses threshold (`.tasks/` growth without absorption).
- **Time-based** fallback for long-running projects (weekly, monthly — overlay-configurable).
- **Manual**: `/curate` for an explicit pass.

The state file's `cycle_log.counts` already includes everything these triggers need: closed-task count, externalisation count, time-since-last-curation. V2 reads them; v1 writes them.

## Deep archive location (decided in v1 to avoid v2 rework)

`<repo>/.tasks/archive/` is the deep archive. Decided now because:

- Closest to where externalisation already lives.
- Survives `git worktree`.
- One home per project.
- Doesn't pollute the cycle archive at `.orchestrator/cycles/<cycle-id>/`.

Closed-task bodies that get compressed (by v2) keep a back-reference to `<repo>/.tasks/archive/<task-name>.md`.

## V2 phase shape (single-pass first)

When v2 ships, the curation cycle is a **single distillation pass** running all five operations together. Not mirrored as plan/execute/integrate. The brainstorm noted that the mirrored shape may be needed if workload outgrows single-pass; promote then.

## Safeguards (load-bearing in v2)

- **Default to keep, never delete.** Compression and archival reversible; deletion isn't.
- **Originals preserved verbatim in deep archive.** Compression produces a hot-path summary; the source survives.
- **Reviewable proposals, not autonomous edits.** Same contract as Architect: Curator (or Architect+PM in v2) proposes; user dispositions.
- **Externalised tasks get find-ability treatment, not removal.**
- **Auditable index.** The index is itself a queryable, diffable, version-controlled artifact.

These are spec'd here so v2 implementation has them at hand.

## Boundary with regular cycle (v2)

- **Scope**: regular cycle's integrate operates on this cycle's outputs. Curation operates on the **historical corpus** (closed tasks, accumulated review notes, aged externalised entries, cumulative PM-digest stream).
- **Tempo**: regular cycle runs once per batch. Curation runs threshold-triggered, possibly weeks apart.

The two cycles communicate through shared artifacts (the register, the index, the externalised backlog), never by synchronous handoff. **Curation never blocks a regular cycle from proceeding; a regular cycle never blocks curation from running.**

## When to revisit v2 spec

Revisit when:
1. The register at any project exceeds 50 entries AND has not been pruned.
2. The cycle archive at any project exceeds 20 cycles.
3. PM's cascade detection consistently fires on the same cluster across ≥5 cycles (suggests durable speculation prior is needed; that's a curation operation).
4. The user asks for it.

Until then, this stub stands.
