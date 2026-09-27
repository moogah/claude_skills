---
name: opsx-orchestrate
description: "Orchestrate batched, agent-driven implementation across a project — planning, executing, and integrating cycles of tasks with role-separated agents (Implementor, Reviewer, Architect, PM). Use when working with a repo that has `.claude/orchestrator/config.yaml` or when the user invokes `/opsx-orchestrate`, `/opsx-tasks generate`, `/architect-audit`, `/pm-digest`, `/curate`. Handles: (1) running plan / execute / integrate phases of a change, (2) spawning role agents in worktrees, (3) author-blind review enforcement, (4) maintaining the interfaces register and reconciling speculated entries against implementation discoveries, (5) producing PM digests with cascade detection, (6) integrate→plan handshake artifacts that close the cycle loop. Skip for ad-hoc one-off edits that don't span a batch — those don't need the orchestrator."
---

# opsx-orchestrate

Central skill that runs the **plan / execute / integrate** cycle for batched, agent-driven implementation. Lives globally; reads a per-project overlay at `<repo>/.claude/orchestrator/` for project specifics.

## Resolution: where am I, what do I read?

1. **Walk up from `$cwd`** looking for `.claude/orchestrator/config.yaml`. First hit wins; that directory is `$REPO_ROOT`.
2. **Parse `config.yaml`** — required fields validated; missing-required = hard error. Optional fields fall through to core defaults.
3. **Append role overlays** from `<repo>/.claude/orchestrator/roles/*.md` (if present) to the corresponding core role briefs at agent-spawn time.
4. **No overlay found** → warn the user explicitly; fall back to sensible defaults (see `overlay.md`).

Full overlay contract: **[overlay.md](overlay.md)**.

## What phase am I in?

Run `state.py status` (the state tool: `python3 ~/.claude/skills/opsx-orchestrate/scripts/state.py`; the flow docs write it `state.py`). It prints the phase (`plan` | `execute` | `integrate`), each gate's failing checks, the task table, open asks and what to resume. Each phase has its own flow doc and exit gate. **A later phase refuses to start if the prior phase's gate has not passed** — `state.py phase set` enforces it.

| Phase | Flow doc | Defining ops |
|---|---|---|
| Plan | [flows/plan.md](flows/plan.md) | Consume prior integrate's handshake → Architect forward-mode → batch composition → task generation → implementor briefs |
| Execute | [flows/execute.md](flows/execute.md) | Per-task: worktree + Implementor + on-touch Architect + author-blind Reviewer; sequential merge with regression check |
| Integrate | [flows/integrate.md](flows/integrate.md) | Register reconciliation + end-of-cycle Architect audit + PM digest + meta-discovery + goal-drift check + handshake artifact |

State-file shape, the state tool's verbs and the exit gates: **[state.md](state.md)**.

If the work is trivially small (one-line edit, single-file doc fix, config tweak), use the inline path: **[flows/inline.md](flows/inline.md)**. Bailout to standard cycle if it grows.

The curation cycle (slower-tempo corpus grooming) is **deferred to v2**: see **[flows/curation.md](flows/curation.md)** for the placeholder and reasons.

## Roles

The orchestrator deploys four roles. Read the relevant role file before spawning:

- **[roles/implementor.md](roles/implementor.md)** — does the task at expert level in a worktree; produces diff + structured `## Observations` and `## Discoveries`; reports to orchestrator only (never to reviewer).
- **[roles/reviewer.md](roles/reviewer.md)** — author-blind review of one merged diff; rigorous-not-contrarian. **The reviewer-spawn helper enforces author-blindness at the substrate level** — see `flows/execute.md` § 7.
- **[roles/architect.md](roles/architect.md)** — watches structural drift across the batch via 8 signal classes; runs at three triggers (on-touch, end-of-cycle, between-cycle); maintains the interfaces register.
- **[roles/project-manager.md](roles/project-manager.md)** — hybrid deterministic+thin-agent form; the deterministic pass is `state.py counts`, never the LLM; cascade detection can spawn Architect audits.

**Not every step needs an agent.** Triage inline-vs-worktree before spawning.

## Change artifact set

- **Interfaces register** at `<project-root>/interfaces.{org,md}` — lifelong, growing, four tiers: shape / vocabulary / boundary / invariant. Per-entry `status: speculated | confirmed | divergent | reconciled` and `load_bearing` flag. Replaces `architecture.md`. Templates: [templates/register-entry-shape.md](templates/register-entry-shape.md), [-vocabulary.md](templates/register-entry-vocabulary.md), [-boundary.md](templates/register-entry-boundary.md), [-invariant.md](templates/register-entry-invariant.md).
- **Scaffolding** at `<change>/scaffolding/` — Architect-generated executable contracts (failing tests, mapping function shells, vocabulary `pcase` scaffolds) tied to speculative register entries. Per-tier defaults (invariant / vocabulary / boundary on; shape opt-in); per-project overlay can disable. Reconciled at integrate by treating diffs as evidence. Full contract: **[scaffolding.md](scaffolding.md)**.
- **`proposal.md`** carries a goal-status header mirroring the register lifecycle: [templates/proposal-status-header.md](templates/proposal-status-header.md).
- **`design.md`** keeps its current scope: implementation strategy, technical decisions, alternatives considered.
- **Provenance fields on every follow-up task and reconciliation**: `discovered_from`, `discovered_by`, `discovered_class`, `reconciled_into`. Enforced by [externalisation.md](externalisation.md) and the integrate-phase exit gate.

## Templates

Output forms — read at the point each is produced:

- [templates/architect-finding.md](templates/architect-finding.md)
- [templates/ask.md](templates/ask.md) — every question to the user
- [templates/pm-digest.md](templates/pm-digest.md)
- [templates/reconciliation-note.md](templates/reconciliation-note.md)
- [templates/task-body.md](templates/task-body.md)

## Externalisation

In-change vs `.tasks/` rule: **[externalisation.md](externalisation.md)**. Rule of thumb: in-change tasks contribute to the proposal's stated outcome; cross-cutting findings go to `.tasks/` with full provenance. PM is the only role that can promote externalised tasks back into the active change.

## Loop closure: integrate → plan handshake

The keystone transition is **integrate → plan**, not execute → review. Each plan reads `<repo>/.orchestrator/handshake-<prior-cycle-id>.json` as a hard input contract. Required fields: `register_diff`, `pm_digest_path`, `meta_discoveries`, `user_resolved_goal_drift`, `asks_for_user_open`, `asks_for_user_resolved` and `task_refinements`. Empty list is allowed; missing field is not. `state.py init` refuses to start a cycle if the file is missing or any field is unset; `state.py handshake` writes it with the mechanical fields filled from state.

This is the structural fix for "learns and forgets" — without it, the orchestrator becomes a queue runner.

## When NOT to use this skill

- Single ad-hoc edits with no batch context — just edit; don't bring in worktrees and agents.
- Projects without `.claude/orchestrator/config.yaml` AND with no Makefile / no test runner — the orchestrator can't validate anything; defer until the project is onboarded.
- Across-multiple-changes work — v1 is single-change at a time.

## Mode detection at entry

When invoked without an explicit phase:

1. Run `state.py status`. Its `resume` line says which of these applies: resume the current phase; advance (`phase set`) because the prior gate passed; `close` because integrate passed; or `init` a new cycle because the last one is closed.
2. No state file (`not_initialized`): **start a new plan phase** with `state.py init` (it needs the prior handshake, or `--first-cycle`).
3. A legacy or invalid file: `status` says so; `state.md` § Recovery gives the steps. Do not write it by hand.

Never silently start a new cycle while a prior cycle is open; `init` refuses, and `close --abandon --why` is the explicit exit.

## Critical requirements

- **`state.json` is written only through `scripts/state.py`.** A heredoc, `jq`, `sed` or Write against it is a process bug. The tool owns the schema (unknown keys and long text are refused), the transitions, the timestamps, the gate evaluation and the handshake; narrative goes to `state.py note` or to the file that exists for it.
- **Counts come from the state file, never from the LLM.** `state.py counts` is the PM digest's deterministic pass and the source of truth for every number; the agent pass only frames prose.
- **Author-blind review is enforced at the harness level**, not by discipline. The reviewer-spawn helper has no path to the Implementor's report, observations, discoveries, identity, or scratch files.
- **Phase exit gates are mandatory.** A later phase refuses to start if a prior gate hasn't passed.
- **Provenance fields are mandatory on follow-up tasks and reconciliations.** The orchestrator refuses to externalise without them.
- **The integrate→plan handshake artifact is the loop-closure contract.** Without every required field (see `state.md`), plan refuses to start.
- **Every question to the user is an ask record** per `templates/ask.md`, triaged by kind. Only `decision` kinds reach the user as questions, in the template's rendering, and decisions are read back before they are applied. An `AskUserQuestion` raised mid-execute is reserved for asks that block a merge.
