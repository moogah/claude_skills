---
name: orchestrate
description: "Orchestrate batched, agent-driven implementation across a project — planning, executing, and integrating cycles of tasks with role-separated agents (Implementor, Reviewer, Architect, PM). Use when working in a directory, a repo or a workspace above one, that has `.claude/orchestrator/config.yaml` or when the user invokes `/orchestrate`, `/pm-digest`, `/curate`. Handles: (1) the design round that turns spec scenarios into seams and pending tests, (2) running plan / execute / integrate phases of a change, (3) spawning role agents in worktrees, (4) author-blind review enforcement, (5) checking merged code against the change's design and amending it, (6) producing PM digests with cascade detection, (7) integrate→plan handshake artifacts that close the cycle loop. Skip for ad-hoc one-off edits that don't span a batch — those don't need the orchestrator."
---

# orchestrate

Central skill that runs the **plan / execute / integrate** cycle for batched, agent-driven implementation. Lives globally; reads a per-project overlay at `<home>/.claude/orchestrator/` for project specifics.

## Resolution: where am I, what do I read?

1. **Walk up from `$cwd`** looking for `.claude/orchestrator/config.yaml`. First hit wins; that directory is the **home** (`$ORCH_HOME`). It holds the process, `.orchestrator/` (one directory per change under `.orchestrator/changes/<change>/`: `change.json`, state, cycles, briefs, reviews, the inbox, the task files) and `.tasks/`, and its git repo tracks them. The home is usually a **workspace**, a directory holding worktrees of one or more repositories; a repository's root can be the home too (`overlay.md` § Repositories).
2. **Parse `config.yaml`** — required fields validated; missing-required = hard error. Optional fields fall through to core defaults.
3. **Read the repositories** from `config.yaml` (`repos:`, or the one-repository shorthands `code-root:` and the bare repository root; `overlay.md` § Repositories). A **change belongs to one repository** and is worked in its own worktree of it, on the branch named after the change, its **change integration branch**, which its tasks merge into. That worktree is the change's **code root** (`$CODE_ROOT`): where task worktrees branch from, code is merged and tested, and code commits are made. It also holds the **record**: `docs/specs/`, one org file per capability (`templates/spec.md`), and `docs/changes/<change>/` with the change's `intent.org` and `design.org`. `state.py change open <name>` creates the worktree (or adopts one) and the change's directory; several changes may be open at once, each with its own state. When a hand-off produced several changes of one repository, they share a **workspace integration branch** (`research` § Hand-off): cut from the repository's main, carrying the glossary and the spec edits; each change is opened from it (`--from`, recorded in `change.json`), takes it in before its design round and before its whole-change review, and merges into it when done (`flows/design.md` § When it runs, `flows/integrate.md` § 9); its merge to main is the user's call. The orchestrator's cwd is the home, and every git or test command that belongs in the code root says so (`git -C "$CODE_ROOT"`, or a `cd` in a subshell). `eval "$(state.py where --sh)"` exports `ORCH_HOME`, `ORCH_WORKTREES`, `ORCH_CHANGE`, `ORCH_CHANGE_DIR`, `ORCH_REPO` and `CODE_ROOT` for the resolved change; the flow docs write `<change-dir>` for `$ORCH_CHANGE_DIR`, the change's directory `<home>/.orchestrator/changes/<change>/`. Task worktrees are placed from the home by the overlay's `worktrees.pattern`, never under a code root (`state.py worktree <branch>` prints the path). A home or repository whose record is still under `openspec/` is converted first with the `openspec-convert` skill; `where` warns.
4. **Append role overlays** from `<home>/.claude/orchestrator/roles/*.md` (if present) to the corresponding core role briefs at agent-spawn time, followed by `priors.md` (the user's standing rules, `overlay.md` § Priors) when it exists, and by the path of the product's glossary, `$CODE_ROOT/docs/glossary.org` (`templates/glossary.md`), and of any subject note it links, when it exists: the reference vocabulary for anything the agent writes that the user will read. Some agents start without the project's instructions, so the brief names it.
5. **No overlay found** → warn the user explicitly; fall back to sensible defaults (see `overlay.md`), or onboard the home with `state.py home init` (`overlay.md` § Bootstrap). **An overlay that drifted** from the skill's schema (`where` warns: a removed key, a retired concept in a role overlay, a `../` path to the home) is relayed to the user once and corrected by hand (`overlay.md` § Drift).

Full overlay contract: **[overlay.md](overlay.md)**.

## Before the first cycle: the design round

A change enters the orchestrator with its intent and its spec edits, committed in the code root: `docs/changes/<change>/intent.org` (`templates/intent.md`) and the requirements and scenarios it adds, changes or removes, edited in place in `docs/specs/` (`templates/spec.md`). The user may write both; otherwise an **opening** phase agent writes them from the user's words, `.tasks/` items or a split (`flows/open.md`). The Architect's **design round** (`flows/design.md`) then writes `docs/changes/<change>/design.org` (`templates/design.md`), Context, Decisions and the Seams table, and the pending tests the scenarios imply. It runs once per change, before `state.py init --first-cycle`, and needs no state file. The orchestrator does not write a design before the round, itself or through another agent: a change it opens or splits gets an intent and spec edits, and the round writes the design (`flows/design.md` § When it runs). Plan drafts tasks from the seams and the scenarios still pending; integrate checks merged code against them.

## Two contexts: the dispatcher and the phase agent

The session that invokes this skill is the **dispatcher**. It reads this file and the output of `state.py status` (the state tool: `python3 ~/.claude/skills/orchestrate/scripts/state.py`; the flow docs write it `state.py`), and nothing else: no flow doc, no role file, no artifact, no code; the one exception is an ask file it must put to the user, read once when `status` lists it (id, question, path), never the inbox in one `cat` (the record: five files at once, +14k context). Every phase runs in a **phase agent**, spawned with the `Agent` tool (`subagent_type: general-purpose`) from a fresh context, which reads its flow doc, spawns the role agents the flow names, runs the gate, makes the close commit and hands back one line. The dispatcher's context holds hand-back lines, the user's prompts and the blocking calls, so it stays near its floor for the whole change.

The record shows why: 82% of the orchestrator's context was accumulated history (235k tokens per call against a 45k floor); one session ran five cycles and reached 529k with four compactions; the main thread cost twice what all Implementors cost. A phase agent oriented in 64k in the dry run of 2026-10-13.

In the flow, role and template docs, **"the orchestrator" is the phase agent running that flow**; where the dispatcher is meant, it is named.

### The dispatcher's loop

1. `state.py status` (orientation in about 2 KB: phase, gates, tasks, the inbox, `resume`). `state.py where` prints the home, its repositories and its open changes with their phase, and works with no state file. With more than one change open, every `state.py` call names its change: `state.py --change <change> <verb>`, or `export ORCH_CHANGE=<change>` once; the flow docs write `state.py <verb>`. A call that cannot tell which change is meant refuses (`no_change`) and lists them.
2. Handle the inbox first if `status` says an answer is waiting: that is a sync point (`templates/ask.md` § Sync points), done by the phase agent the next step spawns, not by the dispatcher.
3. Spawn the phase agent `resume` calls for: opening a change (the user asks for one, takes up `.tasks/` items, answers a split, or answers `rethink` on the MVP decision; the brief carries the source, `flows/open.md` § The brief; its agent runs `state.py change open`, which makes the worktree), the design round (no state file yet, `design.org` has no Seams section), `plan` (including `init`), `execute`, `integrate` (including `close` and its commit), or the inline path. The brief is fixed and short:

   > You are the phase agent for `<phase>` of change `<change>` in cycle `<cycle-id>` (or: for the design round of `<change>`). Home: `<path>`. `export ORCH_CHANGE=<change>`, then run `state.py status`, then read and follow `~/.claude/skills/orchestrate/flows/<phase>.md` to its exit gate and close commit. Read a role file at the moment you spawn that role and a template at the moment you produce its artifact; do not read `SKILL.md`. When nothing can run until a worker reports, end your turn with no hand-back; you are woken when the worker reports. You hand back once: at the phase's end, or on a stop. You cannot ask the user: an ask that would take the blocking call is a file, and you hand back `stopped` naming it after finishing everything that does not wait on it. Hand back one line (§ Hand-back lines), never a report. What the user reads uses the terms of `<code-root>/docs/glossary.org` (when it exists).

4. On the hand-back: `state.py status` again. Gate passed → spawn the next phase agent. `stopped` on an ask → the inline rule (`templates/ask.md`): when the user is present, the dispatcher makes the blocking call for that one ask (one question per call), writes the answer with `state.py record set asks <id> decision=<label>` (or `status=answered` with the free text, for readback by the phase agent), and resumes the same phase agent by message so its context, worktrees and running workers are kept; when the user is absent, the dispatcher stops with the one-line pointer. `failed` → the line says what; the dispatcher reports it and stops. Design-round questions are ask files like any other, with `blocks: plan` (`flows/design.md` § 5): on `stopped, asks <ids> block plan` the dispatcher asks every one in one sitting, one per call, straight from `status`, and resumes the agent when all are answered; when the user is absent it stops with the pointer and the change waits. A round that hands back `stopped, <n> open questions` before any cycle exists has no state to file them in: spawn plan, whose agent opens the cycle and writes the files.
5. Relay to the user, after each hand-back: the line itself, then the open decisions. While the user is present they are put one per call, never as a list in chat: asks whose `:BLOCKS:` names work waiting now first, then the rest in `status` order, each call carrying the ask's recommendation and a `Not now` option that leaves the ask open (`templates/ask.md` § The inline rule). When the user is absent, they are relayed from `status` in one line each (id, question, path), and an ask that blocks nothing never stops the session. `process` asks are never put. The user answers in the call, the file or chat, and the dispatcher writes a chat answer with `state.py record set asks <id> decision=<label>` and a readback's yes with `readback=confirmed`, then commits the file. The record: four non-blocking asks once took the call after integrate, the dispatcher read the state tool's source to learn how a readback is confirmed, and the answers were left uncommitted; then, over the 56 cycles on `cf2577d`, the users asked again and again to go through the asks one at a time, the rule that only relayed them was broken in at least 116 calls in all four homes, and two dispatchers wrote the preference into memory.

A phase agent with workers still running does not hand back until they finish: a turn it ends to wait is not a hand-back, and a hand-back made while a worker runs reaches the dispatcher as if the phase had stopped. A hand-back that names a running worker or a wait (`__WAIT__`, "waiting on") is resumed by message. The record: in one home a design round, two executes and an integrate handed back while their workers ran, in 5 sessions over 4 changes. An ask raised mid-execute reaches the dispatcher when the running Implementors end, minutes later, with the merge chain already stopped as `flows/execute.md` § 9 says. Resume by message when the agent stopped on an ask; spawn fresh when it finished its phase.

### Hand-back lines

Every agent the orchestrator spawns hands back **one line of at most 200 characters** and writes everything else to the file the flow names for it. A path in the line is relative to the home (`.orchestrator/changes/<change>/cycles/<id>/premise-check.md`), and a line whose file the brief named carries none (the Implementor's and the Reviewer's); the record: 13 of 14 role lines broke the limit with an absolute path of 149-170 characters, and after the relative rule 509 of 1,105 lines on `cf2577d` still did, the Implementor's and the Reviewer's with the absolute path their brief handed them. The line states the outcome and points at the file; the reader opens the file only when the next step needs it. The record: hand-backs and notifications carried 55% of the main thread's tokens.

| Agent | Line | Detail lives in |
|---|---|---|
| Phase agent | `<phase> <cycle-id>: gate passed` · `<phase> <cycle-id>: stopped, ask <id> blocks <task\|merge\|phase>; <path>` (several: `asks <ids> block …`) · `<phase> <cycle-id>: failed, <why>; <path>` · `plan: not started, change <stopped\|rethink>; <intent path>` (an MVP answer applied, `flows/plan.md` § 1) | `state.json`, the cycle's artifacts, the ask file, `intent.org` |
| Opening agent | `open <change>: <c> capabilities, <s> scenarios, <q> open questions; <intent path>` · `open: failed, <why>` | `intent.org`, the spec edits, the `Open: <change>` commit |
| Implementor | `done <task> <sha>` with `deviated` / `candidates=<n>` appended when true · `blocked <task>: <one line>` · `failed <task>: <one line>` | `cycles/<id>/reports/<task>.md`, written by the Implementor; the task file's `## Observations` and `## Discoveries` |
| Reviewer | `reviewed <task> issues=<n> questions=<n> spec-signal=<n> thoughts=<n>` | `cycles/<id>/reviews/<task>.md`, the path the brief named |
| Architect | `design-round <change>: <n> seams, <k> pending tests, <u> uncited scenarios, <q> open questions` · `premise-check <cycle-id>: <n> holds, <m> wrong, <f> findings; <path>` · `conformance <cycle-id>: <f> findings, <a> rows amended; <path>` | `design.org`, `premise-check.md`, `findings/`, `design-diff.json` |

A worker runs its test command in the foreground and reports once, after its commit; a report sent while a run continues in the background wakes the orchestrator twice (8 such wake-ups in one session of the record).

### Phases

Each phase has its own flow doc and exit gate. **A later phase refuses to start if the prior phase's gate has not passed** — `state.py phase set` enforces it.

| Phase | Flow doc | Defining ops |
|---|---|---|
| Opening | [flows/open.md](flows/open.md) | Once per change the orchestrator opens: `intent.org` and the spec edits from the user's words, `.tasks/` items or a split, committed in the code root; no code read, no question asked (they join the round's sitting) |
| Design round | [flows/design.md](flows/design.md) | Once per change, before the first cycle: Architect writes `design.org` and the pending tests until `coverage.py --since <base>` shows no uncited scenario; open questions become ask files blocking plan, asked by the dispatcher |
| Plan | [flows/plan.md](flows/plan.md) | Consume prior integrate's handshake → draft candidate tasks from `design.org` and the pending scenarios → Architect premise check against HEAD → batch composition → implementor briefs |
| Execute | [flows/execute.md](flows/execute.md) | Per-task: worktree + Implementor + author-blind Reviewer (with probe licence); each task merged as it lands (serial chain) with regression check |
| Integrate | [flows/integrate.md](flows/integrate.md) | Design conformance (Architect) + PM digest + meta-discovery + goal-drift check + open-task refinement + handshake artifact + `close`; the whole-change review at each integrate that merged code; when no task on the main path remains, the MVP decision, or done |

State-file shape, the state tool's verbs and the exit gates: **[state.md](state.md)**.

If the work is trivially small (one-line edit, single-file doc fix, config tweak), use the inline path: **[flows/inline.md](flows/inline.md)**, run by a phase agent like any phase. Bailout to standard cycle if it grows.

The curation cycle (slower-tempo corpus grooming) is **deferred to v2**: see **[flows/curation.md](flows/curation.md)** for the placeholder and reasons.

## Roles

The orchestrator deploys four roles. The phase agent reads the relevant role file before spawning; every role hands back one line (§ Hand-back lines):

- **[roles/implementor.md](roles/implementor.md)** — does the task at expert level in a worktree; produces diff + structured `## Observations` and `## Discoveries`; writes its report to `cycles/<id>/reports/<task>.md` for the orchestrator only (never the reviewer).
- **[roles/reviewer.md](roles/reviewer.md)** — author-blind review of one merged diff against the cited seam rows; rigorous-not-contrarian; may probe the merge candidate read-only. **The reviewer-spawn helper enforces author-blindness at the substrate level** — see `flows/execute.md` § 7.
- **[roles/architect.md](roles/architect.md)** — designs the seams that satisfy the spec and writes the pending tests (design round); checks the batch's premises at plan; checks merged code against the design at integrate and amends it. Three runs, no execute-time run.
- **[roles/project-manager.md](roles/project-manager.md)** — hybrid deterministic+thin-agent form; the deterministic pass is `state.py counts`, never the LLM; cascade detection can spawn a scoped conformance run.

**Not every step needs an agent.** Triage inline-vs-worktree before spawning.

## Change artifact set

The record is in the code root, under `docs/`; the process is in the home, under `.orchestrator/`.

- **`docs/specs/`** is the one copy of the requirements and scenarios, an org file per capability, edited in place by each change on the branch its tasks merge into; `git diff <base> -- docs/specs/` is the change's spec record ([templates/spec.md](templates/spec.md)).
- **`design.org`** (`docs/changes/<change>/`) is the Architect's home: Context, Decisions and **Seams** (one row per contract between two parts of the code: id `seam/<name>`, statement, owning symbols, scenarios, tests). Rows hold current state only; the file's `git log` is the history. Template: [templates/design.md](templates/design.md).
- **Scenario coverage is computed, never written.** The tests cite `<capability> § <title>`, a scenario no test is worth writing for carries `:TEST: none: <reason>` in the spec, and `scripts/coverage.py` prints one row per scenario with its status (`templates/spec.md` § Coverage).
- **Pending tests** in the project's test tree, written by the design round for behaviour HEAD does not yet satisfy, marked with the overlay's `test.pending-style` (a style the runner reports as pending, never failing). Promotion is removing the marker, done by the Implementor in the commit that makes the test pass.
- **`intent.org`** (`docs/changes/<change>/`) carries what the diff cannot: why, goals, non-goals, capabilities, removals and their reasons, the new terms, a link to the research that preceded it, the change's `#+BASE:` and its `#+STATUS:` line ([templates/intent.md](templates/intent.md)). A done change stays where it is, its status line saying so.
- **`docs/glossary.org`** is the product's agreed terms, edited in place, the vocabulary of everything the user reads ([templates/glossary.md](templates/glossary.md)). A change's research, when it had any, is `docs/changes/<change>/research/` (the `research` skill).
- **The change's directory** is process: `<home>/.orchestrator/changes/<change>/` (`<change-dir>` in the flows, `$ORCH_CHANGE_DIR` in their shell) holds `change.json` (the repository, branch, worktree and base `state.py change open` recorded), `state.json`, `cycles/`, `inbox/`, the handshakes and the test outputs. `state.py where` lists every change's directory with its phase.
- **Task files** are process: `<change-dir>/tasks/open/` and `…/closed/` ([templates/task-body.md](templates/task-body.md)).
- **The hardening list** is process: `<home>/.orchestrator/changes/<change>/hardening.org`, the change's reasoned defects off the main path, shown to the user with the MVP ([templates/hardening.md](templates/hardening.md)).
- **Per-cycle Architect artifacts**: `cycles/<id>/premise-check.md` (plan § 3) and `cycles/<id>/design-diff.json` (integrate § 1).
- **Ask files**: one org file per question, `<change-dir>/inbox/<ask-id>.org` while open; `cycles/<id>/asks/<ask-id>.org` once applied, declined or superseded ([templates/ask.md](templates/ask.md)).
- **Provenance fields on every follow-up task**: `discovered_from`, `discovered_by`, `discovered_class`. Enforced by [externalisation.md](externalisation.md).

## Templates

Output forms — read at the point each is produced:

- [templates/intent.md](templates/intent.md) — `intent.org` and its status line
- [templates/spec.md](templates/spec.md) — a spec file, its scenarios' citations, `coverage.py`
- [templates/glossary.md](templates/glossary.md) — `docs/glossary.org`, the product's agreed terms
- [templates/design.md](templates/design.md) — `design.org` and its Seams table
- [templates/architect-finding.md](templates/architect-finding.md) — a finding's kind, the YAGNI check, routing by the main path
- [templates/hardening.md](templates/hardening.md) — the hardening list, the MVP decision, the closing summary
- [templates/ask.md](templates/ask.md) — every question to the user, as an org file in `<change-dir>/inbox/`
- [templates/pm-digest.md](templates/pm-digest.md)
- [templates/task-body.md](templates/task-body.md)

## Externalisation

In-change vs `.tasks/` rule: **[externalisation.md](externalisation.md)**. Rule of thumb: in-change tasks contribute to the intent's stated outcome; cross-cutting findings go to `.tasks/` with full provenance. Promotion back into the active change is the user's decision, on a `decision` ask the PM proposes (`externalisation.md` § Externalisation pressure).

## Loop closure: integrate → plan handshake

The keystone transition is **integrate → plan**, not execute → review. Each plan reads `<change-dir>/handshake-<prior-cycle-id>.json` as a hard input contract. Required fields: `design_diff`, `pm_digest_path`, `meta_discoveries`, `user_resolved_goal_drift`, `asks_for_user_open`, `asks_for_user_resolved` and `task_refinements`. Empty list is allowed; missing field is not. `state.py init` refuses to start a cycle if the file is missing or any field is unset; `state.py handshake` writes it with the mechanical fields filled from state.

This is the structural fix for "learns and forgets" — without it, the orchestrator becomes a queue runner.

## When NOT to use this skill

- Single ad-hoc edits with no batch context — just edit; don't bring in worktrees and agents.
- Projects with no test runner at all — the orchestrator can't validate anything. A project with one and no `.claude/orchestrator/config.yaml` in the repo or a workspace above it is onboarded first: `state.py home init` with the user (`overlay.md` § Bootstrap), never a copy of another home's overlay.

## Mode detection at entry

When invoked without an explicit phase, the dispatcher runs its loop (§ Two contexts):

1. Run `state.py status`. Its `resume` line says which of these applies: resume the current phase; advance (`phase set`) because the prior gate passed; `close` because integrate passed; or `init` a new cycle because the last one is closed. The dispatcher spawns the phase agent that line calls for; the phase agent runs the verb.
2. No state file (`not_initialized`): a change `state.py where` does not list wants `state.py change open` first (`flows/open.md` § 2; a change the user opened by hand, with its intent on its own branch, is adopted there with `--worktree`); a change with no `intent.org` wants opening (`flows/open.md`); a `design.org` without a Seams section (or none at all) wants the design round first; otherwise **start a new plan phase** (its agent runs `state.py init`, which needs the prior handshake, or `--first-cycle`).
3. A legacy or invalid file: `status` says so; `state.md` § Recovery gives the steps. Do not write it by hand.

Never silently start a new cycle while a prior cycle is open; `init` refuses, and `close --abandon --why` is the explicit exit.

## Sync points

Where the orchestrator reads the inbox and what it does with an answered file there are listed once, in `templates/ask.md` § Sync points, which the phase agents read. The dispatcher only sees from `status` that an answer waits, and spawns or resumes the phase agent that handles it.

## Critical requirements

- **The dispatcher's context holds pointers only.** It reads `SKILL.md` and `state.py status`; every phase runs in a phase agent from a fresh context (§ Two contexts). A dispatcher that reads a flow doc, a role file, an artifact or code is a process bug.
- **Every spawned agent hands back one line; the detail is a file** (§ Hand-back lines). A report, a findings list or a narrative in a hand-back is a process bug; the orchestrator opens the file when the next step needs it.
- **`state.json` is written only through `scripts/state.py`.** A heredoc, `jq`, `sed` or Write against it is a process bug. The tool owns the schema (unknown keys and long text are refused), the transitions, the timestamps, the gate evaluation and the handshake; narrative goes to `state.py note` or to the file that exists for it.
- **Counts come from the state file, never from the LLM.** `state.py counts` is the PM digest's deterministic pass and the source of truth for every number; the agent pass only frames prose.
- **Author-blind review is enforced at the harness level**, not by discipline. The reviewer-spawn helper has no path to the Implementor's report, observations, discoveries, identity, or scratch files.
- **The design is the contract, and holds only contracts.** Tasks cite seam rows, briefs and reviews quote them, and integrate's conformance check amends them when the code has rightly outgrown them. Nothing else catalogues interfaces. A row or Decision that names a guard, error string, encoding, helper or test is wrong by construction and is cut, not amended (`templates/design.md`). A stale row or sentence beside right code is a **record correction**: fixed, committed, one journal line; never an ask.
- **Phase exit gates are mandatory.** A later phase refuses to start if a prior gate hasn't passed.
- **Every code change takes one of three paths**: a batch task (Implementor + author-blind Reviewer), the inline path (`flows/inline.md`, within its triage), or an inline fix for a review finding (`flows/execute.md` § 8). The orchestrator never edits the code directly, not to apply an ask's answer, not to add scope to a running task; the one exception is a text correction, prose only (`templates/architect-finding.md` § Routing). The record under `docs/` is not code: the design round, an applied answer, a record correction and integrate write it in the code root, the change's worktree; an Implementor never does. **A change is reported done only after the whole-change review** (`flows/integrate.md` § 9) has run and its blocking findings are resolved.
- **Provenance fields are mandatory on follow-up tasks.** The orchestrator refuses to externalise without them.
- **The integrate→plan handshake artifact is the loop-closure contract.** Without every required field (see `state.md`), plan refuses to start.
- **Scope takes the YAGNI check; defects are reported and routed, never dropped.** A feature, option, generalisation or question to the user earns work only when an intent goal needs it or on a demonstrated reason (it happened, a prior, the user asked, a measured cost); otherwise it is not built and not asked about, and an agent that thinks it matters writes one `thought` line. Every defect an agent finds in code or text that exists is reported with a kind (`issue`, `question`, `thought`) and severity, whatever its likelihood; the orchestrator alone routes it: on the change's main path (the uses the intent's goals name) it is reproduced and fixed in the change, an untrue sentence in a shipped document is fixed in the change, anything else goes on the change's hardening list. The one statement is `templates/architect-finding.md`. The user's own words (a prior, an ask decision, an intent sentence) outrank a `design.org` sentence.
- **A change produces a working MVP first, then stops for the user.** When the main path works end to end (no task on it open, whatever else the change still holds; `flows/plan.md` § 6), the change asks one `decision` (harden / rethink / stop) carrying the hardening list and every spec correction made as a record correction, and nothing more runs until the user answers (`templates/hardening.md`). Hardening is worked only after the user chooses it.
- **The glossary is the vocabulary of everything the user reads**: asks, hand-back lines, relays and chat. A domain term it does not have is glossed where it is used, or added to `$CODE_ROOT/docs/glossary.org` first (`templates/glossary.md`). When the user asks what a term means, the explanation is written into the glossary in the same turn and committed in the code root; an explanation given only in chat is lost by the next phase. No check runs against the glossary until a misuse is observed.
- **Every question to the user is an org file in `<change-dir>/inbox/`** per `templates/ask.md`, written the moment it is raised, in any phase, and triaged by kind. Only `decision` kinds reach the user as questions; the user answers in the file; an answer outside the offered options is read back before it is applied. When a question blocks the session is stated once, in `templates/ask.md` § The inline rule; the call is the dispatcher's, since a phase agent has no user (§ Two contexts). Nothing else blocks a session on a question.
- **Process artifacts are tracked in git, in the home repo.** `.orchestrator/` is never gitignored there (`state.py init` refuses when the home ignores it and when the code root contains a `.orchestrator/` of its own; it writes `.orchestrator/.gitignore` for the test outputs). When the orchestrator commits is listed once, in `templates/ask.md` § Commit rule. Process commits are made in the home; record (`docs/`) and code commits in the code root. The home's `.gitignore` ignores the worktrees folder and any repository checkout nested inside the home (`where` warns when it does not), so no code checkout appears as an embedded repository in the home's history.
- **When the home and the code root differ, "in one commit" means one commit per repository.** Every rule that says so (an applied ask: its file moves in the home, the edit it applies lands in the code root) is then one commit in each repository with the same subject, the code-root commit's message naming the ask id or the change. The record and the code share the code root, so the design round's `design.org` with its pending tests, and a record correction with its edit, are one commit there.
