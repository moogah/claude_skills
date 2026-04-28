# State file shape

The orchestrator's state file (`.orchestrator/state.json`) is the source of truth for everything the PM digest counts and everything the cycle's exit gates check. Counts never come from the LLM — every number in a PM digest must trace to a field in this file.

The file lives at `<repo-root>/.orchestrator/state.json`. It survives the lifetime of one cycle (plan → execute → integrate); a new cycle replaces it but the prior cycle's state is archived to `.orchestrator/cycles/<cycle-id>/state.json`.

## Top-level shape

```json
{
  "schema_version": "1.0",
  "session_id": "orch-<unix-ts>",
  "cycle_id": "cycle-<unix-ts>",
  "phase": "plan | execute | integrate",
  "repo_root": "<absolute path>",
  "change_name": "<change-name>",
  "baseline_snapshot": ".orchestrator/baseline-<ts>.txt",
  "baseline_status": 0,
  "current_branch": "main",
  "test_command": "<resolved from overlay or default>",
  "history_window": 5,
  "tasks": [ /* see Task entry */ ],
  "register_touched": [ /* see Register-touched entry */ ],
  "architect_findings": [ /* see Finding entry */ ],
  "cycle_log": { /* see Cycle log */ },
  "phase_gates": { /* see Phase gates */ }
}
```

`schema_version` is a hard field — flows refuse to run against an unrecognised version. Bump on any breaking shape change.

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
  "started_at": null,
  "completed_at": null,
  "reviewed_at": null
}
```

### Field rules

- **`status`** transitions are unidirectional except for `failed → ready` (re-queue) and `needs_review → in_progress` (re-implementation after rejection). Every transition must be persisted.
- **`task_class`** comes from the project overlay's `taxonomy` field. PM uses it for class-distribution and cohort-velocity queries.
- **`on_critical_path`** is set during plan phase from a per-batch LLM read of `proposal.md`. Defaults to `false`. Overlay's `critical-path` field can override.
- **`merge_commit`** is mandatory before `status` can advance to `needs_review`. Reviewer worktrees diff against this.
- **`implementor_report_path`** stores the implementor's deviations + discoveries report. **This file is read by the orchestrator only — never passed to the reviewer.** See `flows/execute.md` for the author-blind constraint.
- **`discovered_*`** fields are mandatory on follow-up tasks. An unset `discovered_*` on a task whose source is another task is a state-file bug; the orchestrator refuses to start the next phase.
- **`reconciled_into`** points to the register entry ID that absorbed this discovery. An unset `reconciled_into` on a `done` task whose `discovered_class` requires register integration is the integrate-phase exit-gate failure.
- **`cites_register_entries`** is the list of register-entry IDs the implementor brief cited. Used by the on-touch Architect trigger and by integrate's reconciliation gate to enumerate touched entries.
- **`blocked_by`** is a list of `task_name`s. `blocker_note` is free-text for non-task blockers (user decisions, external dependencies). Both feed the PM digest's blocked-path-aging signal.

### Status semantics

| Status | Meaning |
|---|---|
| `ready` | In the plan-phase batch; not yet started |
| `setup_complete` | Worktree created; agent spawned but not yet running |
| `in_progress` | Implementor agent running |
| `completed` | Implementor finished; commit landed; tests passed; merged to integration branch; **not yet reviewed** |
| `needs_review` | `merge_commit` recorded; awaiting reviewer |
| `reviewed` | Reviewer finished; findings recorded; awaiting orchestrator's inline-fix-or-followup decision |
| `done` | Reviewed and accepted; any inline fixes applied; ready to unblock dependents |
| `failed` | Implementation or test failed; worktree retained for debugging |
| `blocked` | Blocked by another task or external dependency |
| `externalised` | Out-of-scope; moved to `.tasks/` backlog. Carries `discovered_*` provenance |

The `completed → needs_review` and `needs_review → reviewed → done` separation enforces author-blind review: the reviewer agent never sees a task whose status is `completed`, only `needs_review`, and the implementor's report is never available to the reviewer.

## Register-touched entry

Every register entry the cycle's tasks cited or modified.

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
  "why_tests_missed": null
}
```

Integrate's reconciliation exit gate enumerates this list. Every entry whose `status_at_integrate` is null (or `unchanged` when the entry was actually modified) blocks the cycle from closing.

## Architect finding entry

```json
{
  "finding_id": "arch-<cycle-id>-<seq>",
  "trigger": "on-touch | end-of-cycle | between-cycle",
  "severity": "blocking | advisory | informational",
  "class": "shape-fragmentation | vocabulary-mismatch | responsibility-leakage | dead-branch | interface-drift | mutation | invariant-gap | duplication",
  "title": "<one-line>",
  "locations": [{ "file": "<path>", "line": 504 }],
  "why_tests_missed": "<one sentence>",
  "recommended_resolution": "<one paragraph>",
  "discovered_from": "<task or batch>",
  "resolution": "pending | inline-fixed | followup-task-<task-name> | reverted | accepted-with-note",
  "blocking_merge_until_resolved": true
}
```

`severity: blocking` with `resolution: pending` blocks the integrate exit gate. See `templates/architect-finding.md` for the prose form.

## Cycle log

Updated continuously through the cycle; PM digest reads from it.

```json
{
  "started_at": "<iso-ts>",
  "phase_started_at": { "plan": "<iso>", "execute": "<iso>", "integrate": null },
  "counts": {
    "created": 11,
    "started": 9,
    "completed": 5,
    "reviewed": 4,
    "rejected": 1,
    "externalised": 5,
    "blocked": 1
  },
  "history": [
    {
      "cycle_id": "cycle-<earlier-ts>",
      "counts": { "created": 7, "completed": 4, "reviewed": 3, "rejected": 0, "externalised": 2, "blocked": 0 }
    }
  ],
  "ratios_this_cycle": {
    "drainage": 0.45,
    "review_balance": 0.80,
    "rejection_rate": 0.25,
    "externalisation_pressure": 0.45
  }
}
```

`history` carries the previous `history_window` cycles' counts (default 5). PM digest queries operate on `counts` + `history` only — never on prose. The orchestrator writes ratios so the PM agent prose pass can render them without recomputing.

## Phase gates

Each gate is a structured record of whether the phase's exit conditions are met. Setting a gate's `passed: true` is the only way the next phase can start.

```json
{
  "plan": {
    "passed": false,
    "checks": {
      "batch_composed": false,
      "briefs_cite_register": false,
      "user_signed_off_goal_drift": false,
      "prior_integrate_consumed": false
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
      "blocking_findings_resolved": false,
      "pm_digest_produced": false,
      "user_asks_routed": false,
      "handshake_artifact_written": false
    }
  }
}
```

The gate fields aren't decorative. Plan phase **refuses to run** if `phase_gates.integrate.passed` from the prior cycle is false — that's how the integrate→plan handshake is enforced. Execute refuses to run if `phase_gates.plan.passed` is false. Integrate refuses to close if any of its checks fail.

## Integrate→plan handshake artifact

When integrate closes successfully, it writes `<repo>/.orchestrator/handshake-<cycle-id>.json`. The next plan phase reads this file as a hard input contract; if any field is missing, plan refuses to start.

```json
{
  "cycle_id": "cycle-<ts>",
  "produced_at": "<iso-ts>",
  "register_diff": [
    { "entry_id": "register/shape/violation-info", "from": "speculated", "to": "reconciled", "note_path": "..." }
  ],
  "pm_digest_path": ".orchestrator/cycles/<cycle-id>/pm-digest.md",
  "meta_discoveries": [
    { "kind": "vocabulary-cluster", "scope": "scope/bash-parser-boundary", "evidence": ["task-x", "task-y", "task-z"] }
  ],
  "user_resolved_goal_drift": [
    { "decision": "revise | split | abandon | continue", "rationale": "<short>" }
  ],
  "asks_for_user_open": [],
  "asks_for_user_resolved": []
}
```

Plan reads `register_diff` to know what's now `confirmed` / `divergent` / `reconciled`; reads `meta_discoveries` to update speculation priors; reads `user_resolved_goal_drift` to know whether the proposal was revised. **All four fields are required.** An empty list is allowed; a missing field is not.

## Recovery

If the state file is missing or schema-mismatched at the start of a phase, the orchestrator does not invent state:

1. Look for the most recent `.orchestrator/state-<ts>.json` archive matching the current `cycle_id`. If found and schema matches, restore.
2. Otherwise, ask the user whether to recover from the last known-good archive or to abandon the cycle.
3. Never silently start a new cycle — the integrate→plan handshake depends on every cycle being explicitly closed or explicitly abandoned.

## Cycle archive

When a cycle closes:

```
.orchestrator/cycles/<cycle-id>/
  state.json              # frozen snapshot
  pm-digest.md            # the cycle's digest
  handshake.json          # the artifact above
  findings/<finding-id>.md
  reconciliations/<entry-id>.md
  baseline-<ts>.txt
  after-<task>-<ts>.txt
```

This directory is the long-tail audit trail. The curation cycle (v2, deferred) reads it.
