# State file shape

The orchestrator's state file (`<repo-root>/.orchestrator/state.json`) is the source of truth for everything the PM digest counts and everything the cycle's exit gates check. Counts never come from the LLM — every number in a PM digest is derived from this file by the state tool.

The file survives the lifetime of one cycle (plan → execute → integrate); `close` archives it to `.orchestrator/cycles/<cycle-id>/state.json` and the next `init` starts a fresh one carrying the counts history.

## Writing state

**State is written only through the state tool**, `scripts/state.py` in this skill's directory (`python3 ~/.claude/skills/opsx-orchestrate/scripts/state.py <verb> …`, written `state.py <verb>` in the flow docs). A heredoc, `jq`, `sed` or Write call against `state.json` is a process bug: the tool owns the schema, the transitions, the timestamps and the derived numbers, and it refuses what does not fit. Subagents that touch state use the same verbs.

Run `state.py status` before anything else in a session; it is the orientation (phase, gates with their failing checks, the task table, open asks, pending blocking findings, the last journal lines) in about 2 KB, and it says what to resume.

| Verb | Does |
|---|---|
| `status` | Orientation, as above. Read-only. |
| `validate` | Schema check. On a legacy (1.0) file, prints what `migrate` would do. |
| `migrate` | Converts a schema-1.0 file; non-schema content is parked in `cycles/<id>/legacy-state-extras.json`, a backup is kept. |
| `init --change <name> --test-command <cmd> [--prior-handshake <path> \| --first-cycle] [--cycle <id>]` | Starts a cycle in `plan`. Refuses while a cycle is open. Validates the prior handshake's required fields (plan's refusal). Carries `history` from the archived cycles. |
| `set field=value …` | The four settable scalars: `baseline_snapshot`, `baseline_status`, `current_branch`, `test_command`. |
| `task add <name> --file <path> --class <class> [--cites a,b] [--blocked-by t1] [--critical] [field=value …]` | Adds a task with spec fields only. |
| `task set <name> [<status>] [field=value …]` | The transition. Validates the status enum and the transition rules; sets `started_at` / `completed_at` / `reviewed_at`; requires `merge_commit=` for `needs_review`; counts a rejection on `needs_review → in_progress`. `--force --why "<reason>"` allows a non-standard move and records a `deviation` journal entry. |
| `task remove <name> --why "<reason>"` | Only `ready` / `failed` / `externalised` tasks. |
| `record add findings\|asks\|register-touched <json \| @file>` | Appends a record. Ids are assigned (`arch-<cycle>-<n>`, `ask-<cycle>-<n>`) when omitted; a finding's `path` defaults to `cycles/<id>/findings/<finding-id>.md`. |
| `record set <list> <id> field=value …` | Updates fields on a record: ask decisions, finding resolutions, register dispositions. |
| `note <kind> "<text>" [--ref <id>] [--path <file>] [--by <role>]` | One journal line (≤ 280 chars). Kinds: `discovery`, `decision`, `inline-fix`, `deviation`, `push-back`, `note`. Longer text goes in a file and `--path` points at it. |
| `phase set <plan\|execute\|integrate>` | Advances one phase; refuses unless the prior gate passed. |
| `gate check <phase>` | Evaluates every check that is a function of state or file existence and stores the results; lists the checks it cannot compute. |
| `gate set <phase> <check>=true …` | Asserts a non-computable check (`batch_composed`, `scaffolding_generated_for_tiered_entries`, `open_tasks_refined_against_handshake`). Computed checks cannot be asserted. |
| `gate pass <phase>` | Re-evaluates, then sets `passed` and `passed_at` if every check is true; otherwise names the failing checks. |
| `counts [--write]` | The PM deterministic pass: counts, ratios, by-status, history, critical-path readout, class table, follow-ups by source, fired signals. `--write` saves `cycles/<id>/pm-signals.json`. |
| `handshake [--meta-discoveries <json\|@file>] [--task-refinements <json\|@file>]` | Assembles `handshake-<cycle-id>.json` from state (register diff, asks, goal-drift decisions, digest path) plus the two judgment fields, and validates it. |
| `close [--abandon --why "<reason>"]` | Archives state and handshake to `cycles/<id>/` and marks the cycle closed. Requires the integrate gate unless abandoning. |

Every verb prints JSON. A refusal is `{"ok": false, "error": "<slug>", "message": …}` on stderr, exit 1. The rules the tool enforces:

- **Closed schema.** Unknown top-level keys, unknown task fields and unknown record fields are refused. Narrative goes through `note` or into the file that exists for it (findings, reviews, reconciliation notes, `asks.md`, the digest); state holds the path.
- **Pointers, enums, timestamps.** Every string field is capped at 280 characters (`question` and `decision_readback`: 600). The one-paragraph fields of a finding live in its file.
- **Counts are derived**, never stored. `cycle_log.counts` and the ratios do not exist in the file; `counts` computes them from task statuses and timestamps.
- **Timestamps come from the tool.**
- **Transitions are checked** (see Status semantics). A deviation is possible and is recorded, not hidden.

## Top-level shape

```json
{
  "schema_version": "1.1",
  "session_id": "orch-<unix-ts>",
  "cycle_id": "cycle-<unix-ts>",
  "phase": "plan | execute | integrate",
  "repo_root": "<absolute path>",
  "change_name": "<change-name>",
  "baseline_snapshot": ".orchestrator/baseline-<cycle-id>.txt",
  "baseline_status": 0,
  "current_branch": "main",
  "test_command": "<resolved from overlay or default>",
  "history_window": 5,
  "prior_handshake": ".orchestrator/handshake-<prior-cycle-id>.json",
  "closed_at": null,
  "tasks": [ /* see Task entry */ ],
  "register_touched": [ /* see Register-touched entry */ ],
  "architect_findings": [ /* see Finding entry */ ],
  "asks_for_user": [ /* see Ask entry */ ],
  "journal": [ /* see Journal entry */ ],
  "cycle_log": { /* see Cycle log */ },
  "phase_gates": { /* see Phase gates */ }
}
```

`schema_version` is a hard field — the tool refuses to write a file whose version it does not own, and `status` says so. `prior_handshake` is null only on a first cycle (`init --first-cycle`).

## Task entry

```json
{
  "task_name": "setup-module",
  "task_file": "openspec/changes/<change>/tasks/open/setup-module.md",
  "task_class": "feature | test | doc | refactor | bug | contract | infrastructure",
  "on_critical_path": true,
  "worktree_path": "<repo>/.worktrees/task-setup-module-<ts>",
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
  "reconciled_into": null,
  "cites_register_entries": [],
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
- **`on_critical_path`** is set during plan phase from a per-batch LLM read of `proposal.md` (`task add --critical`, or `task set <name> on_critical_path=true`). Defaults to `false`. Overlay's `critical-path` field can override.
- **`merge_commit`** is mandatory before `status` can advance to `needs_review`; the tool refuses otherwise. Reviewer worktrees diff against this.
- **`implementor_report_path`** stores the implementor's deviations + discoveries report. **This file is read by the orchestrator only — never passed to the reviewer.** See `flows/execute.md` for the author-blind constraint.
- **`discovered_*`** fields are mandatory on follow-up tasks. An unset `discovered_*` on a task whose source is another task is a state-file bug; the orchestrator refuses to start the next phase.
- **`reconciled_into`** points to the register entry ID that absorbed this discovery. An unset `reconciled_into` on a `done` task whose `discovered_class` requires register integration is the integrate-phase exit-gate failure.
- **`cites_register_entries`** is the list of register-entry IDs the implementor brief cited. Used by the on-touch Architect trigger and by integrate's reconciliation gate to enumerate touched entries.
- **`blocked_by`** is a list of `task_name`s. `blocker_note` is free-text for non-task blockers (external dependencies); when the blocker is a user decision, the value is the ask id (`ask-<cycle-id>-<seq>`, see Ask entry); when it is a finding, the finding id. Both feed the PM digest's blocked-path-aging signal, which re-surfaces an ask id rather than raising a new ask. A merge conflict is `blocked` with `blocker_note: merge-conflict`, not a status of its own.
- **`after_snapshot`** is the path of the post-merge test output for this task (`.orchestrator/after-<task>-<ts>.txt`); `regression_detected` points at it as evidence.
- **`agent_task_id`** is omitted when the orchestrator implements inline (no agent); `setup_complete` is an optional intermediate the flows do not require, and going `ready → in_progress` directly is the normal move.
- **`rejections`** is maintained by the tool.

### Status semantics

| Status | Meaning |
|---|---|
| `ready` | In the plan-phase batch; not yet started |
| `setup_complete` | Worktree created; agent spawned but not yet running |
| `in_progress` | Implementor agent running (also: re-implementing after a rejected review or an inline-fix scope) |
| `completed` | Implementor finished; commit landed; tests passed; merged to integration branch; **not yet reviewed** |
| `needs_review` | `merge_commit` recorded; awaiting reviewer |
| `reviewed` | Reviewer finished; findings recorded; awaiting orchestrator's inline-fix-or-followup decision (inline fixes happen in this state) |
| `done` | Reviewed and accepted; any inline fixes applied; ready to unblock dependents |
| `failed` | Implementation or test failed; worktree retained for debugging |
| `blocked` | Blocked by another task, an ask, a blocking finding, a merge conflict or an external dependency; `blocker_note` says which |
| `externalised` | Out-of-scope; moved to `.tasks/` backlog. Carries `discovered_*` provenance |

The `completed → needs_review` and `needs_review → reviewed → done` separation enforces author-blind review: the reviewer agent never sees a task whose status is `completed`, only `needs_review`, and the implementor's report is never available to the reviewer.

## Register-touched entry

Every register entry the cycle's tasks cited or modified. `record add register-touched` at plan (or when first cited); `record set register-touched <entry-id> …` at integrate.

```json
{
  "entry_id": "register/shape/violation-info",
  "entry_tier": "shape | vocabulary | boundary | invariant",
  "load_bearing": true,
  "status_at_plan": "speculated",
  "status_at_integrate": "confirmed | divergent | reconciled | unchanged",
  "cited_by_tasks": ["setup-module", "wire-validator"],
  "modified_by_tasks": ["wire-validator"],
  "reconciliation_note_path": null,
  "why_tests_missed": null,
  "scaffolding_path": "openspec/changes/<change>/scaffolding/shapes/violation-info.test.el",
  "scaffolding_diff_status": "untouched | modified | rejected",
  "scaffolding_status_at_integrate": "untouched | modified | rejected | promoted | archived"
}
```

Integrate's reconciliation exit gate enumerates this list. Every entry whose `status_at_integrate` is null (or `unchanged` when the entry was actually modified) blocks the cycle from closing.

The three `scaffolding_*` fields are null on tiers that opted out of scaffolding for this project (`scaffolding.tiers` in the overlay). Otherwise:

- `scaffolding_path` is set during plan-phase forward-mode when the Architect generates the file.
- `scaffolding_diff_status` is observed during execute by inspecting the diff against the merge-base for the scaffolding subtree.
- `scaffolding_status_at_integrate` is set during integrate's reconciliation step. `untouched` / `modified` / `rejected` are the diff-status mirrors; `promoted` (file migrated to a permanent location) and `archived` (enforcement landed elsewhere) are integrate-only dispositions. See `scaffolding.md` for the reconciliation-by-diff table.

## Architect finding entry

The state record is the index line; the finding file (`templates/architect-finding.md`) at `path` carries the locations in full, the recommended resolution and the reasoning.

```json
{
  "finding_id": "arch-<cycle-id>-<seq>",
  "trigger": "on-touch | end-of-cycle | between-cycle",
  "severity": "blocking | advisory | informational | spec-signal",
  "class": "shape-fragmentation | vocabulary-mismatch | responsibility-leakage | dead-branch | interface-drift | mutation | invariant-gap | duplication",
  "title": "<one-line>",
  "path": ".orchestrator/cycles/<cycle-id>/findings/<finding-id>.md",
  "locations": [{ "file": "<path>", "line": 504 }],
  "why_tests_missed": "<one sentence>",
  "discovered_from": "<task or batch>",
  "observed": { "happened": true, "evidence": "<a line, a run, an output, a shipped sentence | reasoning only: <the reasoning>>" },
  "resolution": "pending | inline-fixed | followup-task-<task-name> | reverted | accepted-with-note | noted",
  "blocking_merge_until_resolved": true
}
```

`severity: blocking` with `resolution: pending` blocks the integrate exit gate. `record add findings` refuses a `blocking` or `advisory` finding without `observed` (`templates/architect-finding.md` § Observed); records written before the field existed read as `null`. `resolution: noted` is a reasoning-only finding kept in its file and not scheduled (`flows/integrate.md` § 7); `counts` reports how many.

## Ask entry

Every question to the user, whichever role raised it. The schema lives in one place, `templates/ask.md`; this array holds the records for the current cycle, and the handshake copies them out unchanged.

```json
{
  "id": "ask-<cycle-id>-<seq>",
  "kind": "decision | doc-correction | process | environment | confirmation",
  "raised_by": { "role": "...", "ref": "..." },
  "question": "...", "about": "...", "observed": { "happened": true, "evidence": "..." },
  "options": [ { "label": "...", "consequence": "..." } ],
  "recommendation": { "option": "...", "why": "..." },
  "default_if_unanswered": "...", "blocks": [],
  "status": "open | answered | applied | deferred | declined | superseded",
  "decision": null, "decision_readback": null, "applied_via": null, "revised_from": null
}
```

Only `decision` kinds are presented to the user (`flows/integrate.md` § 3a, `flows/execute.md` § 9). A task blocked on an ask carries the ask id in `blocker_note`. The goal-drift ask is recognised by its four options (`revise` / `split` / `abandon` / `continue`); the tool derives the plan gate's `user_signed_off_goal_drift` and the handshake's `user_resolved_goal_drift` from it.

## Journal entry

The one place for short narrative that has no other home: orchestrator discoveries, design decisions, inline fixes, deviations from the flow, implementor push-backs. One line each; the long form is a file.

```json
{ "at": "<iso-ts>", "kind": "discovery | decision | inline-fix | deviation | push-back | note",
  "by": "orchestrator | implementor | reviewer | architect | pm | null",
  "ref": "<task, finding, ask or entry id, or null>", "text": "<≤ 280 chars>", "path": "<file or null>" }
```

The handshake carries the journal; plan reads it for discoveries and decisions. Nothing in state is the paragraph form of a discovery; that is the finding file, the review, the reconciliation note or the digest.

## Cycle log

```json
{
  "started_at": "<iso-ts>",
  "phase_started_at": { "plan": "<iso>", "execute": "<iso>", "integrate": null },
  "history": [
    { "cycle_id": "cycle-<earlier-ts>",
      "counts": { "created": 7, "started": 7, "completed": 4, "reviewed": 3, "rejected": 0, "externalised": 2, "blocked": 0, "failed": 0, "done": 3 } }
  ]
}
```

`history` carries the previous `history_window` cycles' counts (default 5), filled by `init` from the archived states. The current cycle's counts and the four ratios (`drainage` = completed/created, `review_balance` = reviewed/completed, `rejection_rate` = rejected/completed, `externalisation_pressure` = externalised/created) are computed by `counts`; they are not stored.

## Phase gates

Each gate is a structured record of whether the phase's exit conditions are met. `gate pass <phase>` is the only thing that sets `passed: true`; `phase set` refuses to advance otherwise. There is no prose in a gate: a reason for a hand-asserted check is a `note`.

```json
{
  "plan": {
    "passed": false,
    "checks": {
      "prior_integrate_consumed": false,
      "batch_composed": false,
      "briefs_cite_register": false,
      "scaffolding_generated_for_tiered_entries": false,
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
      "all_touched_entries_dispositioned": false,
      "all_scaffolding_dispositioned": false,
      "blocking_findings_resolved": false,
      "pm_digest_produced": false,
      "user_asks_routed": false,
      "open_tasks_refined_against_handshake": false,
      "handshake_artifact_written": false
    }
  }
}
```

Computed by `gate check` from state and the files on disk: all of execute's checks; plan's `prior_integrate_consumed`, `briefs_cite_register`, `user_signed_off_goal_drift`; integrate's `all_touched_entries_dispositioned`, `all_scaffolding_dispositioned`, `blocking_findings_resolved`, `pm_digest_produced` (a non-empty `## Signals` section), `user_asks_routed` (open asks in the handshake; decision asks in `asks.md`), `handshake_artifact_written` (file present with every required field). Asserted by the orchestrator with `gate set`: `batch_composed`, `scaffolding_generated_for_tiered_entries`, `open_tasks_refined_against_handshake`. The flow docs say what each check means.

## Integrate→plan handshake artifact

`handshake` writes `<repo>/.orchestrator/handshake-<cycle-id>.json` when integrate closes. The next plan phase reads this file as a hard input contract; `init` refuses to start a cycle whose prior handshake lacks any required field.

```json
{
  "cycle_id": "cycle-<ts>",
  "produced_at": "<iso-ts>",
  "register_diff": [
    { "entry_id": "register/shape/violation-info", "from": "speculated", "to": "reconciled", "note_path": "..." }
  ],
  "pm_digest_path": ".orchestrator/cycles/<cycle-id>/pm-digest.md",
  "meta_discoveries": [
    { "kind": "vocabulary-cluster", "scope": "scope/bash-parser-boundary", "evidence": ["task-x", "task-y", "task-z"], "implication_for_next_plan": "<one sentence>" }
  ],
  "user_resolved_goal_drift": [
    { "ask": "ask-<cycle-id>-<seq>", "decision": "revise | split | abandon | continue", "rationale": "<the readback>" }
  ],
  "asks_for_user_open": [ /* Ask entries with status open, copied unchanged */ ],
  "asks_for_user_resolved": [ /* Ask entries with status answered, applied or deferred */ ],
  "task_refinements": [
    {
      "task": "openspec/changes/<change>/tasks/open/<name>.md",
      "modes": ["created"] | ["in-place"] | ["append"] | ["in-place", "append"] | [],
      "applied_learnings": [
        { "channel": "register-diff", "ref": "register/shape/violation-info", "from": "speculated", "to": "reconciled" },
        { "channel": "meta-discovery", "ref": "vocabulary-cluster/permissive-default-vs-closed-vocabulary" },
        { "channel": "user-resolved-ask", "ref": "ask-arch-cycle-<id>-2" },
        { "channel": "inline-fix", "ref": "arch-cycle-<id>-9" },
        { "channel": "finding", "ref": "arch-cycle-<id>-10" },
        { "channel": "open-ask", "ref": "ask-arch-cycle-<id>-10A" },
        { "channel": "deferred-ask", "ref": "ask-arch-cycle-<id>-2" }
      ],
      "obsolescence_flagged": false
    }
  ],
  "journal": [ /* the cycle's journal, copied */ ]
}
```

`register_diff`, `pm_digest_path`, `user_resolved_goal_drift` and the two `asks_for_user_*` lists are assembled from state; `meta_discoveries` and `task_refinements` are the orchestrator's judgment, passed as JSON (usually `@file`). **The seven fields are required** (`register_diff`, `pm_digest_path`, `meta_discoveries`, `user_resolved_goal_drift`, `asks_for_user_open`, `asks_for_user_resolved`, `task_refinements`); the tool defines the list once. An empty list is allowed; a missing field is not. Plan reads `register_diff` to know what's now `confirmed` / `divergent` / `reconciled`; `meta_discoveries` to update speculation priors; `user_resolved_goal_drift` to know whether the proposal was revised; `task_refinements` to know which open tasks already absorbed cycle learning (so plan doesn't re-touch them) and which were flagged as candidate-obsolete for user disposition; `journal` for the discoveries and decisions the cycle recorded.

## Recovery

`status` on a missing, legacy or invalid state file says which, and the orchestrator does not invent state:

1. Legacy schema (`1.0`): `validate` shows the migration plan; `migrate` converts it, parks every non-schema key and field in `cycles/<id>/legacy-state-extras.json` (nothing is deleted) and keeps a `state.json.pre-migrate-<ts>` backup.
2. Invalid current-schema file (someone wrote it by hand): `validate` lists the offending keys; fix them by hand, then continue through the tool.
3. Missing file mid-cycle: restore `cycles/<cycle-id>/state.json` if an archive exists for the cycle; otherwise ask the user whether to recover or to abandon the cycle. This is an `environment`-kind ask (`templates/ask.md`): asked at once, in block form, since there is no state file to record it in.
4. Never silently start a new cycle — `init` refuses while a cycle is open; `close --abandon --why` is the explicit way out.

## Cycle archive

`close` writes:

```
.orchestrator/cycles/<cycle-id>/
  state.json              # frozen snapshot, closed_at set
  handshake.json          # the artifact above
  pm-signals.json         # counts --write
  pm-digest.md            # the cycle's digest
  asks.md                 # the asks as presented, with the decisions (templates/ask.md)
  findings/<finding-id>.md
  reconciliations/<tier>-<name>.md   # the entry id without "register/", slashes as dashes
  reviews/<task-name>.md
  reports/<task-name>.md             # the Implementor's structured report, orchestrator-only
  legacy-state-extras.json           # only when migrate ran in this cycle
```

`baseline-<cycle-id>.txt` and `after-<task>-<ts>.txt` stay at `.orchestrator/` and are named from state (`baseline_snapshot`, `tasks[].after_snapshot`).

```
```

This directory is the long-tail audit trail. The curation cycle (v2, deferred) reads it.
