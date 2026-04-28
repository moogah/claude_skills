---
name: tasks-orchestrator
description: "Orchestrate batched, agent-driven implementation across a project — planning, executing, and integrating cycles of tasks with role-separated agents (Implementor, Reviewer, Architect, PM). Use when working with a repo that has `.claude/orchestrator/config.yaml` or when the user invokes `/tasks-orchestrator`, `/opsx-tasks generate`, `/architect-audit`, `/pm-digest`, `/curate`. Handles: (1) running plan / execute / integrate phases of a change, (2) spawning role agents in worktrees, (3) author-blind review enforcement, (4) maintaining the interfaces register and reconciling speculated entries against implementation discoveries, (5) producing PM digests with cascade detection, (6) integrate→plan handshake artifacts that close the cycle loop. Skip for ad-hoc one-off edits that don't span a batch — those don't need the orchestrator."
---

# tasks-orchestrator

Central skill that runs the **plan / execute / integrate** cycle for batched, agent-driven implementation. Lives globally; reads a per-project overlay at `<repo>/.claude/orchestrator/` for project specifics.

## Resolution: where am I, what do I read?

1. **Walk up from `$cwd`** looking for `.claude/orchestrator/config.yaml`. First hit wins; that directory is `$REPO_ROOT`.
2. **Parse `config.yaml`** — required fields validated; missing-required = hard error. Optional fields fall through to core defaults.
3. **Append role overlays** from `<repo>/.claude/orchestrator/roles/*.md` (if present) to the corresponding core role briefs at agent-spawn time.
4. **No overlay found** → warn the user explicitly; fall back to sensible defaults (see `overlay.md`).

Full overlay contract: **[overlay.md](overlay.md)**.

## What phase am I in?

Read `<repo>/.orchestrator/state.json`. Its `phase` field is one of `plan` | `execute` | `integrate`. Each phase has its own flow doc and exit gate. **A later phase refuses to start if the prior phase's `phase_gates.<prior>.passed` is `false`.**

| Phase | Flow doc | Defining ops |
|---|---|---|
| Plan | [flows/plan.md](flows/plan.md) | Consume prior integrate's handshake → Architect forward-mode → batch composition → task generation → implementor briefs |
| Execute | [flows/execute.md](flows/execute.md) | Per-task: worktree + Implementor + on-touch Architect + author-blind Reviewer; sequential merge with regression check |
| Integrate | [flows/integrate.md](flows/integrate.md) | Register reconciliation + end-of-cycle Architect audit + PM digest + meta-discovery + goal-drift check + handshake artifact |

State-file shape and exit gates: **[state.md](state.md)**.

If the work is trivially small (one-line edit, single-file doc fix, config tweak), use the inline path: **[flows/inline.md](flows/inline.md)**. Bailout to standard cycle if it grows.

The curation cycle (slower-tempo corpus grooming) is **deferred to v2**: see **[flows/curation.md](flows/curation.md)** for the placeholder and reasons.

## Roles

The orchestrator deploys four roles. Read the relevant role file before spawning:

- **[roles/implementor.md](roles/implementor.md)** — does the task at expert level in a worktree; produces diff + structured `## Observations` and `## Discoveries`; reports to orchestrator only (never to reviewer).
- **[roles/reviewer.md](roles/reviewer.md)** — author-blind review of one merged diff; rigorous-not-contrarian. **The reviewer-spawn helper enforces author-blindness at the substrate level** — see `flows/execute.md` § 7.
- **[roles/architect.md](roles/architect.md)** — watches structural drift across the batch via 8 signal classes; runs at three triggers (on-touch, end-of-cycle, between-cycle); maintains the interfaces register.
- **[roles/project-manager.md](roles/project-manager.md)** — hybrid deterministic+thin-agent form; counts come from state file, never from LLM; cascade detection can spawn Architect audits.

**Not every step needs an agent.** Triage inline-vs-worktree before spawning.

## Change artifact set

- **Interfaces register** at `<project-root>/interfaces.{org,md}` — lifelong, growing, four tiers: shape / vocabulary / boundary / invariant. Per-entry `status: speculated | confirmed | divergent | reconciled` and `load_bearing` flag. Replaces `architecture.md`. Templates: [templates/register-entry-shape.md](templates/register-entry-shape.md), [-vocabulary.md](templates/register-entry-vocabulary.md), [-boundary.md](templates/register-entry-boundary.md), [-invariant.md](templates/register-entry-invariant.md).
- **`proposal.md`** carries a goal-status header mirroring the register lifecycle: [templates/proposal-status-header.md](templates/proposal-status-header.md).
- **`design.md`** keeps its current scope: implementation strategy, technical decisions, alternatives considered.
- **Provenance fields on every follow-up task and reconciliation**: `discovered_from`, `discovered_by`, `discovered_class`, `reconciled_into`. Enforced by [externalisation.md](externalisation.md) and the integrate-phase exit gate.

## Templates

Output forms — read at the point each is produced:

- [templates/architect-finding.md](templates/architect-finding.md)
- [templates/pm-digest.md](templates/pm-digest.md)
- [templates/reconciliation-note.md](templates/reconciliation-note.md)
- [templates/task-body.md](templates/task-body.md)

## Externalisation

In-change vs `.tasks/` rule: **[externalisation.md](externalisation.md)**. Rule of thumb: in-change tasks contribute to the proposal's stated outcome; cross-cutting findings go to `.tasks/` with full provenance. PM is the only role that can promote externalised tasks back into the active change.

## Loop closure: integrate → plan handshake

The keystone transition is **integrate → plan**, not execute → review. Each plan reads `<repo>/.orchestrator/handshake-<prior-cycle-id>.json` as a hard input contract. Required fields: `register_diff`, `pm_digest_path`, `meta_discoveries`, `user_resolved_goal_drift`. Empty list is allowed; missing field is not. Plan refuses to start if the file is missing or any field is unset.

This is the structural fix for "learns and forgets" — without it, the orchestrator becomes a queue runner.

## When NOT to use this skill

- Single ad-hoc edits with no batch context — just edit; don't bring in worktrees and agents.
- Projects without `.claude/orchestrator/config.yaml` AND with no Makefile / no test runner — the orchestrator can't validate anything; defer until the project is onboarded.
- Across-multiple-changes work — v1 is single-change at a time.

## Mode detection at entry

When invoked without an explicit phase:

1. Read `state.json` — if it exists and `phase_gates.<current_phase>.passed` is false, **resume that phase**.
2. If `state.json` is absent or `phase_gates.integrate.passed` is true, **start a new plan phase**.
3. If `state.json`'s `phase_gates.integrate.passed` is false but `phase_gates.execute.passed` is true, **resume integrate**.
4. If `state.json` exists but its `schema_version` is not recognised, **refuse to run** and direct user to recover or abandon.

Never silently start a new cycle while a prior cycle is open.

## Critical requirements

- **Counts come from the state file, never from the LLM.** The PM digest's deterministic pass is the source of truth for every number; the agent pass only frames prose.
- **Author-blind review is enforced at the harness level**, not by discipline. The reviewer-spawn helper has no path to the Implementor's report, observations, discoveries, identity, or scratch files.
- **Phase exit gates are mandatory.** A later phase refuses to start if a prior gate hasn't passed.
- **Provenance fields are mandatory on follow-up tasks and reconciliations.** The orchestrator refuses to externalise without them.
- **The integrate→plan handshake artifact is the loop-closure contract.** Without all four required fields, plan refuses to start.
