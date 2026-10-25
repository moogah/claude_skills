# State file shape

The orchestrator's state file (`<home>/.orchestrator/changes/<change>/state.json`, one per change, so several changes run cycles side by side) is the source of truth for everything the PM digest counts and everything the cycle's exit gates check. Counts never come from the LLM — every number in a PM digest is derived from this file by the state tool.

The file survives the lifetime of one cycle (plan → execute → integrate); `close` archives it to `.orchestrator/changes/<change>/cycles/<cycle-id>/state.json` and the next `init` starts a fresh one carrying the counts history.

## Writing state

**State is written only through the state tool**, `scripts/state.py` in this skill's directory (`python3 ~/.claude/skills/orchestrate/scripts/state.py <verb> …`, written `state.py <verb>` in the flow docs). A heredoc, `jq`, `sed` or Write call against `state.json` is a process bug: the tool owns the schema, the transitions, the timestamps and the derived numbers, and it refuses what does not fit. Subagents that touch state use the same verbs. The tool finds the home by walking up from its cwd for `.claude/orchestrator/config.yaml`, then for `.orchestrator/` holding a change; `--home <path>` (alias `--repo`) names it outright. It resolves the change from `--change <name>`, then `$ORCH_CHANGE`, then the open change whose worktree holds the cwd, then the only open change; otherwise it refuses `no_change` and lists them. It reads the repositories and the worktree placement from the home's `config.yaml` (`overlay.md` § Repositories) and the change's repository, branch and worktree from `<home>/.orchestrator/changes/<change>/change.json`. An `@file` argument resolves against the cwd first, then the home.

Run `state.py status` before anything else in a session, in the dispatcher and in every phase agent (`SKILL.md` § Two contexts); it is the orientation (phase, gates with their failing checks, the task table, open asks and which of them have an answer waiting in the inbox, pending blocking findings, the last journal lines) in about 2 KB, and it says what to resume.

| Verb | Does |
|---|---|
| `status` | Orientation, as above, plus `paths: {home, code_root}`. Read-only. Its `resume` hint names the inbox answers waiting, if any. |
| `inbox` | Read-only. Every `.orchestrator/changes/<change>/inbox/*.org` joined with the index: the question, the answer and the option it names (`exact` / `fuzzy` / `none`), other ask ids the answer mentions, a label written under another heading, whether git tracks the file, and every index/file mismatch. Works on a closed cycle. The sync points run it (`templates/ask.md` § Sync points). |
| `validate` | Schema check, plus the inbox's file problems. On a file of an older schema, prints what `migrate` would do. |
| `migrate` | Converts a file of an older schema (1.0 to 1.4) to the current one; `repo_root` becomes `home`, and an old `repo_root` that differs from the home is parked under `parked.top_level` and reported as `rehomed_from`; other non-schema content (the old `register_touched` list among it) is parked in `cycles/<id>/legacy-state-extras.json`, a backup is kept. Writes `.orchestrator/.gitignore` when absent (the same content `init` writes). Every ask record becomes a file (`ask_files` in the output); a legacy "resolved" ask is applied history under its cycle's `asks/`. |
| `home init [--repo <name>=<path> …] \| [--code-root <dir>] [--project <slug>] [--language <l>] [--test-command <cmd>] [--parser <p>] [--worktrees-parent <dir>] [--worktrees-pattern <p>]` | Writes a new home's `.claude/orchestrator/config.yaml` from the skill's schema (`overlay.md` § Bootstrap), with `overlay-version`, and an empty `roles/`; the home is `--home` or the cwd. Refuses `overlay_exists`, `repo_missing`, `repo_not_git`, `home_not_git` (a repository-root home must be a git work tree), `bad_repo_arg`. Reports `add_to_home_gitignore`. Needs no state. |
| `where [--sh]` | Prints the home, its shape and repositories, the worktree placement, every open change (`{name, repo, branch, worktree, base, state, cycle, phase, closed}`), and for the resolved change its `code_root`, `change_dir` and `state`; plus `problems`, `warnings` (the home's `.gitignore`, an `openspec/` record, the legacy layout, and overlay drift: `overlay.md` § Drift) and `legacy_layout`. Works with no state file and no change. `--sh` prints `export ORCH_HOME=… ORCH_WORKTREES=…` and, with a change, `ORCH_CHANGE=… ORCH_CHANGE_DIR=… ORCH_REPO=… CODE_ROOT=…` for the flows' bash blocks (`eval "$(state.py where --sh)"`). |
| `change open <name> [--repo <name>] [--from <ref>] [--worktree <path>]` | Opens a change: creates its worktree of the repository at the overlay's `worktrees` path for the branch `<name>` (an existing branch is checked out; otherwise one is made from `--from`, default the checkout's HEAD), or adopts the checkout `--worktree` names (or one already at that path on that branch), and writes `<home>/.orchestrator/changes/<name>/change.json` with the repository, branch, worktree, base commit and `from`. Refuses `change_exists`, `which_repo` (several repositories and no `--repo`), `unknown_repo`, `worktree_occupied`, `git_failed`, `misplaced_artifacts`, `two_homes`. Needs no state. |
| `worktree <branch> [--sh]` | The path a worktree of the change's repository takes for `<branch>` (`worktrees.parent` + `worktrees.pattern`); `--sh` prints `WORKTREE_PATH=…`. The flows use it for task worktrees. |
| `init [--change <name>] [--test-command <cmd>] [--prior-handshake <path> \| --first-cycle] [--cycle <id>]` | Starts a cycle of the resolved change in `plan` (`--change` must match it; `--test-command` defaults to the repository's `test.command`). Refuses while that change's cycle is open; `no_home` when no `config.yaml` is found walking up and no `--home` is given; `no_change` / `change_not_open` (open it first); `orchestrator_ignored` when the home repo's `.gitignore` ignores `.orchestrator/` (the code repo may keep ignoring it as a guard); `repo_missing` / `repo_not_git` (a `repos:` path; `code_root_missing` / `code_root_not_git` for the `code-root:` shorthand); `misplaced_artifacts` (`<code-root>/.orchestrator/` exists; the message names the home: move it there with a plain `mv` and run `init` again); `two_homes` (`<code-root>/.claude/orchestrator/config.yaml` exists); `bad_code_root` (a list); `bad_worktree_pattern`. An `openspec/specs/` left in the home (a record not yet moved to `docs/` in the code root) is a warning in the result, not a refusal. `current_branch` comes from `git -C <code-root> rev-parse --abbrev-ref HEAD`. Validates the prior handshake's required fields (plan's refusal). Carries `history` from the archived cycles and every file in the inbox into the new index (`inbox_carried`). Writes `.orchestrator/.gitignore` (test outputs, the lock, migration backups) when absent. |
| `set field=value …` | The four settable scalars: `baseline_snapshot`, `baseline_status`, `current_branch`, `test_command`. |
| `task add <name> --file <path> --class <class> [--cites seam/a,seam/b] [--blocked-by t1] [--critical] [field=value …]` | Adds a task with spec fields only; stamps `created_at` on the entry and, when the task file exists and carries the key, in its frontmatter (the record: two task files dated by hand 30-35 minutes after their commit). |
| `task set <name> [<status>] [field=value …]` | The transition. Validates the status enum and the transition rules; sets `started_at` / `completed_at` / `reviewed_at`; requires `merge_commit=` for `needs_review`; counts a rejection on `needs_review → in_progress`. `--force --why "<reason>"` allows a non-standard move and records a `deviation` journal entry. |
| `task remove <name> --why "<reason>"` | Only `ready` / `failed` / `externalised` tasks. |
| `record add findings <json \| @file.json>` | Appends a finding. The id (`arch-<cycle>-<n>`) is assigned when omitted; `path` defaults to `cycles/<id>/findings/<finding-id>.md`. |
| `record add asks <json \| @file.json \| @draft.org>` | Writes the ask file (`templates/ask.md`) and appends its index entry. Takes the full field set (`question`, `about`, `observed`, `options`, …); the prose lands in the file, not in state. An org draft's own prose is kept. A top-level `ref` is read as `raised_by.ref` and a string `raised_by` as the role, as the org form reads `:REF:` and `:RAISED_BY:`. The id is assigned when omitted, skipping any file already on disk; a `decision` without `about`, `observed` and two to four options is refused; `"revised_from": "<old-id>"` marks the old ask `superseded` and moves its file. A terminal status at add (`confirmation`, or the retired `doc-correction`, recorded `applied`) writes straight into `cycles/<id>/asks/`. |
| `record add agents <json \| @file.json>` | Records one spawned agent's setting (§ Agent entry): `agent_id` (the id the `Agent` call returned), `role`, `task` when it has one, `model`, `effort`, and for a task override `source: task-override` with its `reason`. The tool sets `cycle` and `recorded_at`. Refuses `duplicate_id` (a resumed agent keeps its first entry) and `invalid_record` (an unknown role, model or effort; `haiku`; a task override with no task; an empty reason). With no open cycle (no state file, or the cycle closed) the entry is written to `agents-pending.json` beside the state, keeping the closed cycle's id as its `cycle`, and the next `init` takes it in (`agents_carried`). |
| `record set findings <id> field=value …` | Updates fields on a finding: its resolution. |
| `record set asks <id> field=value …` | Edits the file and the index together: re-reads the file (the file wins; a deleted drawer is restored from the index), applies the fields (`status`, `blocks`, `kind`, `about`, `observed`, `options`, `recommendation`, `default_if_unanswered`, `decision`, `decision_readback`, `applied_via`, `revised_from`), rewrites the index entry from the file. `readback=confirmed` writes the user's yes under `* Readback` (refuses when there is no readback), so a yes given in chat is recorded by a verb, not by hand; `status=answered\|applied\|declined\|deferred` refuses when `* Answer` is empty and no `decision=` is given; `applied` needs `applied_via`; crossing into or out of `applied` / `declined` / `superseded` moves the file (`git mv` when tracked, else rename; `moved_via` reported). `id` and `path` are not settable. |
| `note <kind> "<text>" [--ref <id>] [--path <file>] [--by <role>]` | One journal line (≤ 280 chars). Kinds: `discovery`, `decision`, `inline-fix`, `deviation`, `push-back`, `note`. Longer text goes in a file and `--path` points at it. |
| `phase set <plan\|execute\|integrate>` | Advances one phase; refuses unless the prior gate passed. |
| `gate check <phase>` | Evaluates every check that is a function of state or file existence and stores the results; lists the checks it cannot compute. |
| `gate set <phase> <check>=true …` | Asserts a non-computable check (`batch_composed`, `open_tasks_refined_against_handshake`). Computed checks cannot be asserted. |
| `gate pass <phase>` | Re-evaluates, then sets `passed` and `passed_at` if every check is true; otherwise names the failing checks. |
| `counts [--write]` | The PM deterministic pass: counts, ratios, by-status, history, critical-path readout, class table, follow-ups by source, fired signals, and `agents` (§ Agent entry: spend by role from the agents' transcripts, and the quality signals beside it). `--write` saves `cycles/<id>/pm-signals.json`. |
| `context <agent-id>` | A running agent's context: the tokens of the last call in its transcript (`$CLAUDE_CONFIG_DIR` or `~/.claude`, `projects/*/*/subagents/agent-<id>.jsonl`; a unique prefix of the id will do), against the overlay's `thresholds.phase-agent-context` (default 400000), with `over`. The dispatcher's reading (`SKILL.md` § The dispatcher's loop). Refuses `no_transcript`, `ambiguous_agent`. Needs no state. |
| `handshake [--meta-discoveries <json\|@file>] [--task-refinements <json\|@file>] [--design-diff <json\|@file>]` | Assembles `handshake-<cycle-id>.json` from state (asks, goal-drift decisions, digest path), the design diff (read from `cycles/<id>/design-diff.json` unless given), the candidate tasks (read from `cycles/<id>/candidate-tasks.json`; `bad_candidate_tasks` when one does not fit or a finding's `candidate-task-<name>` has none) and the two judgment fields, and validates it. |
| `close [--abandon --why "<reason>"]` | Archives state and handshake to `cycles/<id>/` and marks the cycle closed. Requires the integrate gate unless abandoning. Reports `inbox_left` (open, answered, deferred); those files stay in the inbox for the next `init`. |

Every verb prints JSON. A refusal is `{"ok": false, "error": "<slug>", "message": …}` on stderr, exit 1. The rules the tool enforces:

- **Closed schema.** Unknown top-level keys, unknown task fields and unknown record fields are refused. Narrative goes through `note` or into the file that exists for it (findings, reviews, `design.org`, the ask files, the digest); state holds the path.
- **Pointers, enums, timestamps.** Every string field in state is capped at 280 characters. An ask's title (`question`) and `decision_readback` are capped at 600; its prose (`about`, options, the answer) lives in the file and has no cap. The one-paragraph fields of a finding live in its file.
- **Counts are derived**, never stored. `cycle_log.counts` and the ratios do not exist in the file; `counts` computes them from task statuses and timestamps.
- **Timestamps come from the tool.**
- **Transitions are checked** (see Status semantics). A deviation is possible and is recorded, not hidden.

## Top-level shape

```json
{
  "schema_version": "1.5",
  "session_id": "orch-<unix-ts>",
  "cycle_id": "cycle-<unix-ts>",
  "phase": "plan | execute | integrate",
  "home": "<absolute path>",
  "code_root": "<absolute path>",
  "change_name": "<change-name>",
  "baseline_snapshot": ".orchestrator/changes/<change>/baseline-<cycle-id>.txt",
  "baseline_status": 0,
  "current_branch": "main",
  "test_command": "<resolved from overlay or default>",
  "history_window": 5,
  "prior_handshake": ".orchestrator/changes/<change>/handshake-<prior-cycle-id>.json",
  "closed_at": null,
  "tasks": [ /* see Task entry */ ],
  "architect_findings": [ /* see Finding entry */ ],
  "asks_for_user": [ /* see Ask entry */ ],
  "agents": [ /* see Agent entry */ ],
  "journal": [ /* see Journal entry */ ],
  "cycle_log": { /* see Cycle log */ },
  "phase_gates": { /* see Phase gates */ }
}
```

`schema_version` is a hard field — the tool refuses to write a file whose version it does not own, and `status` says so. `home` is the directory holding `.claude/orchestrator/config.yaml`, where `.orchestrator/` and `.tasks/` live. `code_root` is the change's worktree (`change.json`), the git work tree where code is merged and committed; it is the home only when the home is the repository and the change was adopted on its checkout. `prior_handshake` is null only on a first cycle (`init --first-cycle`).

## Task entry

```json
{
  "task_name": "setup-module",
  "task_file": ".orchestrator/changes/<change>/tasks/open/setup-module.md",
  "task_class": "feature | test | doc | refactor | bug | contract | infrastructure",
  "on_critical_path": true,
  "worktree_path": "<home>/<worktrees.parent>/<pattern for task-setup-module-<ts>>",
  "branch_name": "task-setup-module-<ts>",
  "agent_task_id": "<Agent id or null>",
  "status": "ready | setup_complete | in_progress | completed | needs_review | reviewed | done | failed | blocked | externalised",
  "merge_commit": null,
  "after_snapshot": null,
  "regression_detected": false,
  "worktree_removed": false,
  "review_mode": "inline | delegated | null",
  "findings_path": null,
  "findings_count": null,
  "followups_created": [],
  "dependents_repointed": [],
  "implementor_report_path": null,
  "discovered_from": null,
  "discovered_by": null,
  "discovered_class": null,
  "cites_seams": [],
  "blocked_by": [],
  "blocker_note": null,
  "rejections": 0,
  "started_at": null,
  "completed_at": null,
  "reviewed_at": null
}
```

### Field rules

- **`status`** moves forward along `ready → setup_complete → in_progress → completed → needs_review → reviewed → done` (steps may be skipped), sideways into `failed` / `blocked` / `externalised` from any non-`done` state, and back only by `failed → ready`, `blocked → ready`, `needs_review → in_progress` and `reviewed → in_progress` (re-implementation after rejection; `rejections` is incremented). Anything else needs `--force --why` and is journaled as a deviation. Every transition is persisted by the verb that makes it.
- **`task_class`** comes from the project overlay's `taxonomy` field. PM uses it for class-distribution and cohort-velocity queries.
- **`on_critical_path`** says the task is on the change's main path, so the MVP decision waits for it; set when the task is added, by its source (`flows/plan.md` § 6; `task add --critical`, or `task set <name> on_critical_path=true`). Defaults to `false`. Overlay's `critical-path` field can override.
- **`merge_commit`** is mandatory before `status` can advance to `needs_review`; the tool refuses otherwise. Reviewer worktrees diff against this.
- **`implementor_report_path`** stores the implementor's deviations + discoveries report. **This file is read by the orchestrator only — never passed to the reviewer.** See `flows/execute.md` for the author-blind constraint.
- **`discovered_*`** fields are mandatory on follow-up tasks. An unset `discovered_*` on a task whose source is another task is a state-file bug; the orchestrator refuses to start the next phase.
- **`cites_seams`** is the list of seam ids (`seam/<name>`, rows of the change's `design.org`) the implementor brief cites. The plan gate requires one on every non-externalised task; integrate's conformance check enumerates the cited rows; the task file carries the same list plus `cites_scenarios`, which state does not hold.
- **`blocked_by`** is a list of `task_name`s. `blocker_note` is free-text for non-task blockers (external dependencies); when the blocker is a user decision, the value is the ask id (`ask-<cycle-id>-<seq>`, see Ask entry); when it is a finding, the finding id. Both feed the PM digest's blocked-path-aging signal, which re-surfaces an ask id rather than raising a new ask. A merge conflict is `blocked` with `blocker_note: merge-conflict`, not a status of its own.
- **`after_snapshot`** is the path of the post-merge test output for this task (`.orchestrator/changes/<change>/after-<task>-<ts>.txt`); `regression_detected` points at it as evidence.
- **`agent_task_id`** is omitted when the orchestrator implements inline (no agent); `setup_complete` is an optional intermediate the flows do not require, and going `ready → in_progress` directly is the normal move.
- **`rejections`** is maintained by the tool.

### Status semantics

| Status | Meaning |
|---|---|
| `ready` | In the plan-phase batch; not yet started |
| `setup_complete` | Worktree created; agent spawned but not yet running |
| `in_progress` | Implementor agent running (also: re-implementing after a rejected review or an inline-fix scope) |
| `completed` | Implementor finished; commit landed; tests passed; merged to the change integration branch; **not yet reviewed** |
| `needs_review` | `merge_commit` recorded; awaiting reviewer |
| `reviewed` | Reviewer finished; findings recorded; awaiting orchestrator's inline-fix-or-followup decision (inline fixes happen in this state) |
| `done` | Reviewed and accepted; any inline fixes applied; ready to unblock dependents |
| `failed` | Implementation or test failed; worktree retained for debugging |
| `blocked` | Blocked by another task, an ask, a blocking finding, a merge conflict or an external dependency; `blocker_note` says which |
| `externalised` | Out-of-scope; moved to `.tasks/` backlog. Carries `discovered_*` provenance |

The `completed → needs_review` and `needs_review → reviewed → done` separation enforces author-blind review: the reviewer agent never sees a task whose status is `completed`, only `needs_review`, and the implementor's report is never available to the reviewer.

## Architect finding entry

The state record is the index line; the finding file (`templates/architect-finding.md`) at `path` carries the locations in full, the recommended resolution and the reasoning.

```json
{
  "finding_id": "arch-<cycle-id>-<seq>",
  "trigger": "premise-check | conformance",
  "kind": "issue | question",
  "severity": "high | medium | low",
  "class": "shape-fragmentation | vocabulary-mismatch | responsibility-leakage | dead-branch | interface-drift | mutation | invariant-gap | duplication",
  "title": "<one-line>",
  "path": ".orchestrator/changes/<change>/cycles/<cycle-id>/findings/<finding-id>.md",
  "locations": [{ "file": "<path>", "line": 504 }],
  "why_tests_missed": "<one sentence>",
  "discovered_from": "<task or batch>",
  "seam": "<seam/<name> or null>",
  "evidence": "<the line, the run, the output, the shipped sentence | for a question: the reasoning and the reproduction to try>",
  "decoration": "blocking | hardening | null",
  "resolution": "pending | inline-fixed | design-amended | candidate-task-<name> | followup-task-<task-name> | reverted | accepted-with-note | hardening-list",
  "blocking_merge_until_resolved": true
}
```

`kind`, `severity` and `evidence` are the Architect's (`templates/architect-finding.md` § Kind); `record add findings` refuses a finding without a `kind` of `issue` or `question` and a non-empty `evidence` (a `thought` is a journal line, not a finding). `decoration` is the orchestrator's routing (§ Routing of that file), `null` until it routes; `decoration: blocking` with `resolution: pending` blocks the integrate exit gate. The `resolution` values above are the one list; `hardening-list` is a finding on the change's hardening list (`templates/hardening.md`). `counts` reports the findings by kind and how many are on the hardening list. Files from schema 1.4 and earlier carry `severity` (blocking / advisory / informational) and `observed`; `migrate` converts them (§ Recovery).

## Ask entry

Every question to the user, whichever role raised it, is one org file (`templates/ask.md` is the format's normative text). State holds only the index entry; the file is the record, and when the two disagree the file wins (`record set` rewrites the entry from it; `validate` and `inbox` report the mismatch).

```json
{
  "id": "ask-<cycle-id>-<seq>",
  "kind": "decision | doc-correction | process | environment | confirmation",
  "status": "open | answered | applied | deferred | declined | superseded",
  "blocks": [],
  "path": ".orchestrator/changes/<change>/inbox/<id>.org | .orchestrator/cycles/<cycle-from-id>/asks/<id>.org"
}
```

`path` is a function of `id` and `status`: the inbox while `open`, `answered` or `deferred`; the asks directory of the cycle the id names once `applied`, `declined` or `superseded`. The question, the options, the answer and the readback are read from the file whenever a verb needs them (`inbox`, `handshake`, the gates). Only `decision` kinds are presented to the user (`flows/integrate.md` § 3a, `flows/execute.md` § 9). A task blocked on an ask carries the ask id in `blocker_note`. The goal-drift ask is recognised by its four options (`revise` / `split` / `abandon` / `continue`) in the file; the plan gate's `user_signed_off_goal_drift` passes only when its answer names one of them exactly, and the handshake's `user_resolved_goal_drift` carries that label.

## Agent entry

One per spawned agent: the agent setting it was spawned at and why (`SKILL.md` § Agent settings). The spawner writes it with `record add agents` once the `Agent` call has returned the id; the dispatcher writes a phase agent's at its first hand-back.

```json
{
  "agent_id": "<the id the Agent call returned>",
  "role": "phase-open | phase-design | phase-plan | phase-execute | phase-integrate | phase-inline | architect-design | architect-premise | architect-conformance | implementor | implementor-rework | reviewer-task | reviewer-whole-change | pm",
  "task": "<task name, or null>",
  "cycle": "<the cycle the agent worked in, or null before the first cycle>",
  "model": "opus | sonnet | fable",
  "effort": "low | medium | high | xhigh | max",
  "source": "role-default | task-override",
  "reason": "role default | <the task file's one-line reason>",
  "recorded_at": "<iso-ts>"
}
```

`haiku` is refused: it runs read-only helpers only, and those (`web-fetch`) are not recorded. A file of schema 1.5 written before this list existed gets an empty one on read, with no migration. An agent counts in the cycle it worked in, whichever file holds its entry: an integrate phase agent recorded after `close` sits in `agents-pending.json`, then in the next cycle's list, with the closed cycle's id. `counts` reads the archives, the state and the pending file, so after a change's last `close`, whose integrate agent no `init` ever takes in, a `counts` run still reports it under that cycle.

`counts` joins each entry to the agent's transcript (`$CLAUDE_CONFIG_DIR` or `~/.claude`, `projects/*/*/subagents/agent-<id>.jsonl`, as `context` finds it) and reports under `agents`:

- `by_role`: per role, the agents and how many had a transcript (`counted`), the settings they were spawned at, the task overrides, requests, tokens (input, cache read, cache write, output), `cost_weight` (input + 0.1 × cache read + 1.25 × cache write + 5 × output: E3's proxy, blind to the model, so it compares with E3's numbers), `priced_weight` (the same scaled by the model's input price against Opus's), `usd` at list price, the role's share of the cost weight, the cost weight per agent, and the median wall-clock minutes. Each API request counts once (by `requestId`).
- `by_agent`: the same per agent, with the setting it ran at per request (`ran`, from `message.model` and `effort`). A request logged on several lines counts each usage field at its largest; a `<synthetic>` line (an API error, zero usage) is not a request.
- `missing_transcripts` (the agent ids with no transcript found) and `setting_mismatches` (an agent that ran at another setting than its entry: an environment variable outranks the call).
- `quality`: Reviewer rejections (`rejections` on the tasks), rework tasks (a task sent back to an Implementor, or with an `implementor-rework` agent), and defects found after done: a conformance finding of kind `issue`, a task whose `discovered_from` begins `whole-change` (`flows/integrate.md` § 9), or a follow-up of a task already done that an Implementor did not raise and that is not in that task's `followups_created`. All three again by the agent setting of each task's Implementor (`by_implementor_setting`; `unrecorded` when none was recorded, `whole change` for a defect no one task owns).
- `history`: per cycle of the history window, and `before-first-cycle`, the cost weight, priced weight and dollars by role, and the three quality counts.

## Journal entry

The one place for short narrative that has no other home: orchestrator discoveries, design decisions, inline fixes, deviations from the flow, implementor push-backs. One line each; the long form is a file.

```json
{ "at": "<iso-ts>", "kind": "discovery | decision | inline-fix | deviation | push-back | note",
  "by": "orchestrator | implementor | reviewer | architect | pm | null",
  "ref": "<task, finding, ask or seam id, or null>", "text": "<≤ 280 chars>", "path": "<file or null>" }
```

The handshake carries the journal; plan reads it for discoveries and decisions. Nothing in state is the paragraph form of a discovery; that is the finding file, the review, a seam row or the digest.

## Cycle log

```json
{
  "started_at": "<iso-ts>",
  "phase_started_at": { "plan": "<iso>", "execute": "<iso>", "integrate": null },
  "history": [
    { "cycle_id": "cycle-<earlier-ts>",
      "counts": { "created": 5, "carried": 2, "started": 7, "completed": 4, "reviewed": 3, "rejected": 0, "inline_fixes": 1, "externalised": 2, "blocked": 0, "failed": 0, "done": 3 } }
  ]
}
```

`history` carries the previous `history_window` cycles' counts (default 5), filled by `init` from the archived states. The current cycle's counts and the four ratios (`drainage` = completed/created, `review_balance` = reviewed/completed, `rejection_rate` = rejected/completed, `externalisation_pressure` = externalised/created) are computed by `counts`; they are not stored. `created` is the tasks first drafted in the cycle; a task an earlier cycle of the change held (by name, from the archived states) is `carried`, so a queue that shrinks never reads as one that grows. `rejected` counts send-backs only; `inline_fixes` counts the cycle's `inline-fix` journal lines, the review findings the orchestrator fixed without one. Beside them, `findings.hardening_list` is the entries of the change's `hardening.org`, whoever routed them, and `critical_path.on_path_total` includes the on-path task files in `tasks/open/` that no batch of the cycle holds (`on_path_unbatched`). The record: in three homes the PM digest corrected or disowned these four numbers, at least 8 cycles.

## Phase gates

Each gate is a structured record of whether the phase's exit conditions are met. `gate pass <phase>` is the only thing that sets `passed: true`; `phase set` refuses to advance otherwise. There is no prose in a gate: a reason for a hand-asserted check is a `note`.

```json
{
  "plan": {
    "passed": false,
    "checks": {
      "prior_integrate_consumed": false,
      "batch_composed": false,
      "briefs_cite_seams": false,
      "user_signed_off_goal_drift": false
    }
  },
  "execute": {
    "passed": false,
    "checks": {
      "all_tasks_executed_or_stopped": false,
      "all_reviews_completed": false,
      "no_orphan_in_progress": false
    }
  },
  "integrate": {
    "passed": false,
    "checks": {
      "blocking_findings_resolved": false,
      "pm_digest_produced": false,
      "user_asks_routed": false,
      "open_tasks_refined_against_handshake": false,
      "handshake_artifact_written": false
    }
  }
}
```

Computed by `gate check` from state and the files on disk: all of execute's checks; plan's `prior_integrate_consumed`, `briefs_cite_seams` (every non-`externalised` task has a non-empty `cites_seams`), `user_signed_off_goal_drift`; integrate's `blocking_findings_resolved`, `pm_digest_produced` (a non-empty `## Signals` section), `user_asks_routed` (every open ask in the handshake; no index/file mismatch or missing file; no open ask with an answer the orchestrator has not acted on, where a written `* Readback` counts as acted on; every non-terminal file in the inbox and every terminal one under `cycles/<id>/asks/`), `handshake_artifact_written` (file present with every required field). Asserted by the orchestrator with `gate set`: `batch_composed`, `open_tasks_refined_against_handshake`. The flow docs say what each check means.

## Integrate→plan handshake artifact

`handshake` writes `<home>/.orchestrator/changes/<change>/handshake-<cycle-id>.json` when integrate closes. The next plan phase reads this file as a hard input contract; `init` refuses to start a cycle whose prior handshake lacks any required field (a prior handshake written before schema 1.2, which carries `register_diff` instead of `design_diff`, is accepted once).

```json
{
  "cycle_id": "cycle-<ts>",
  "produced_at": "<iso-ts>",
  "design_diff": [
    { "seam": "seam/violation-info", "change": "amended", "ref": "arch-cycle-<ts>-3" }
  ],
  "pm_digest_path": ".orchestrator/changes/<change>/cycles/<cycle-id>/pm-digest.md",
  "meta_discoveries": [
    { "kind": "vocabulary-cluster", "scope": "scope/bash-parser-boundary", "evidence": ["task-x", "task-y", "task-z"], "implication_for_next_plan": "<one sentence>" }
  ],
  "user_resolved_goal_drift": [
    { "ask": "ask-<cycle-id>-<seq>", "decision": "revise | split | abandon | continue", "rationale": "<the readback>" }
  ],
  "asks_for_user_open": [ /* index entries with status open; plan reads the file at each path */ ],
  "asks_for_user_resolved": [ /* index entries with status answered, applied or deferred, plus decision, decision_readback and applied_via read from the file */ ],
  "task_refinements": [
    {
      "task": ".orchestrator/changes/<change>/tasks/open/<name>.md",
      "impact": [
        { "channel": "design-diff", "ref": "seam/violation-info" },
        { "channel": "meta-discovery", "ref": "vocabulary-cluster/permissive-default-vs-closed-vocabulary" },
        { "channel": "user-resolved-ask", "ref": "ask-arch-cycle-<id>-2" },
        { "channel": "inline-fix", "ref": "arch-cycle-<id>-9" }
      ],
      "obsolescence_flagged": false
    }
  ],
  "candidate_tasks": [
    {
      "name": "got-home-carries-line", "source": "arch-cycle-<id>-3", "discovered_by": "architect",
      "discovered_class": "invariant-gap", "critical": true,
      "quote": "<the finding, discovery or decision, verbatim>",
      "evidence": ".orchestrator/changes/<change>/cycles/<cycle-id>/findings/<file>", "placement": "in-change"
    }
  ],
  "journal": [ /* the cycle's journal, copied */ ]
}
```

`pm_digest_path`, `user_resolved_goal_drift` and the two `asks_for_user_*` lists are assembled from state; `design_diff` is read from `cycles/<cycle-id>/design-diff.json` (integrate § 1) or passed as `--design-diff`; `candidate_tasks` is read from `cycles/<cycle-id>/candidate-tasks.json`, which execute § 5, § 8 and integrate § 7 append to (absent means `[]`; `name`, `source`, `discovered_by`, `critical` and `quote` required, `discovered_class`, `evidence` and `placement` optional, nothing else); `meta_discoveries` and `task_refinements` are the orchestrator's judgment, passed as JSON (usually `@file`). **The eight fields are required** (`design_diff`, `pm_digest_path`, `meta_discoveries`, `user_resolved_goal_drift`, `asks_for_user_open`, `asks_for_user_resolved`, `task_refinements`, `candidate_tasks`; a prior handshake written before `candidate_tasks` existed is read with it empty); the tool defines the list once. An empty list is allowed; a missing field is not. Plan reads `design_diff` to know which seam rows moved; `meta_discoveries` to weigh when drafting the batch; `user_resolved_goal_drift` to know whether the intent was revised; `task_refinements` for what each open task must absorb and which were flagged as candidate-obsolete for user disposition; `candidate_tasks` for the tasks it writes; `journal` for the discoveries and decisions the cycle recorded.

## Recovery

`status` on a missing, legacy or invalid state file says which, and the orchestrator does not invent state:

1. Older schema (`1.0` to `1.4`): `validate` shows the migration plan; `migrate` converts it, renames `repo_root` to `home` (an old value that differs from the home is parked under `parked.top_level` and reported as `rehomed_from`) and adds `code_root`, writes `.orchestrator/.gitignore` when absent, renames `cites_register_entries` to `cites_seams`, maps old finding triggers to the current two (the original kept as `_legacy_trigger` in the extras), turns a finding's old `observed` and `severity` into `kind` (`happened: false` is a `question`, anything else an `issue`), `evidence` and, for a `blocking` one, `decoration: blocking` (the old values kept in the extras; such a finding's routing is redone at its next integrate), writes every ask record as a file (open ones into the inbox, resolved ones under their cycle's `asks/`; a file already there is left alone, so the verb can be rerun), parks every other non-schema key and field, `register_touched` included, in `cycles/<id>/legacy-state-extras.json` (nothing is deleted) and keeps a `state.json.pre-migrate-<ts>` backup. A cycle migrated while at integrate re-runs `handshake` before `gate pass integrate`, since the handshake it wrote earlier carries the old field.
2. Invalid current-schema file (someone wrote it by hand): `validate` lists the offending keys; fix them by hand, then continue through the tool.
3. Missing file mid-cycle: restore `cycles/<cycle-id>/state.json` if an archive exists for the cycle; otherwise ask the user whether to recover or to abandon the cycle. This is an `environment`-kind ask (`templates/ask.md`): the blocking call, at once, since there is no state file to index an ask file.
4. An ask file and its index entry disagree (`validate` and `inbox` say how): a hand-edited `:STATUS:` is reconciled by `record set asks <id> status=<that value>`; a deleted drawer is restored by any `record set`; a file missing from its `path` is restored from git; a file in the inbox with no index entry is carried in by the next `init` or indexed now with `record add asks @<path>`.
5. Never silently start a new cycle — `init` refuses while a cycle is open; `close --abandon --why` is the explicit way out.
6. The one-change layout (a `state.json` at `.orchestrator/` itself, with `cycles/`, `inbox/` and the handshakes beside it, from before `changes/<change>/`): `where` and `status` say `legacy_layout`; `migrate` moves it all under `.orchestrator/changes/<change-name>/`, rewrites the paths in the state file and the handshakes, writes `change.json` adopting the old code root on its branch, then runs the schema migration if one is due. The task files were already under `changes/<change>/tasks/` and do not move.

The home is a workspace holding worktrees of one or more repositories, or a repository's root (`overlay.md` § Repositories). A home with no git behaves as before: nothing is tracked and `init` does not refuse.

## Cycle archive

`close` archives the state file and the handshake; every other file was written there during the cycle, by the step named beside it. This is the one listing; the flows point here.

```
.orchestrator/changes/<change>/cycles/<cycle-id>/
  state.json              # close: frozen snapshot, closed_at set
  handshake.json          # close: the artifact above
  briefs/<task-name>.md   # execute § 1: what the Implementor saw
  premise-check.md        # plan § 3
  design-diff.json        # integrate § 1
  pm-signals.json         # counts --write
  pm-digest.md            # integrate § 3
  meta-discoveries.json   # integrate § 4, input to the handshake
  task-refinements.json   # integrate § 7: each open task's impact set, input to the handshake
  candidate-tasks.json    # execute § 5, § 8 and integrate § 7: tasks the next plan writes, input to the handshake
  asks/<ask-id>.org       # applied, declined and superseded asks (templates/ask.md); moved here by record set
  findings/<finding-id>.md
  reviews/<task-name>.md  # and reviews/whole-change.md (integrate § 9)
  reports/<task-name>.md  # the Implementor's structured report, orchestrator-only
  legacy-state-extras.json  # only when migrate ran in this cycle
```

At the change's level, `.orchestrator/changes/<change>/`: `change.json` (the repository, branch, worktree, base and `from` recorded by `change open`); `state.json`; `inbox/<ask-id>.org`, every open, answered and deferred ask of the change across cycles (`init` indexes what it finds there); `handshake-<cycle-id>.json`; `agents-pending.json`, agent entries recorded while no cycle was open, until the next `init` takes them in (§ Agent entry); `tasks/open/` and `tasks/closed/`; `hardening.org`; the test outputs `baseline-<cycle-id>.txt` and `after-<task>-<ts>.txt`, named from state (`baseline_snapshot`, `tasks[].after_snapshot`). At the `.orchestrator/` level: `changes/`, and `.gitignore`, written by `init`, which keeps the test outputs, `*.tmp`, the locks and the migration backups out of git at any depth; everything else under `.orchestrator/` is tracked by the home repo and committed there at the points `templates/ask.md` § Commit rule lists.

This directory is the long-tail audit trail. The curation cycle (v2, deferred) reads it.
