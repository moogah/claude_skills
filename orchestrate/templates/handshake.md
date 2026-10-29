# Handshake template

The **handshake** is the file integrate writes for the next plan, `<change-dir>/handshake-<cycle-id>.json`: what the plan acts on, gathered in one place so an autonomous plan agent can keep the change moving without reading the cycle's files itself (`flows/integrate.md` § 8, `flows/plan.md` § 1). It carries the plan's context and nothing else: **no counts, ratios or signals**. The record: in 11 of 12 cycle pairs the plan read the handshake, and no count, ratio, throughput row or computed signal of the PM digest left a trace in any plan; what changed plans was the meta-discoveries, the carried asks, the journal and the task text rewritten at integrate (research `state-tool`, E5).

Integrate writes it; the state layer only checks it. `state.py gate integrate` fails `handshake_written` while a field is missing or a candidate or design-diff element does not fit, and `state.py cycle start` refuses to start the next cycle on a prior handshake missing a field. An empty list is allowed; a missing field is not.

## Form

```json
{
  "cycle_id": "cycle-<ts>",
  "design_diff": [
    { "seam": "seam/violation-info", "change": "amended", "ref": "arch-cycle-<ts>-3" }
  ],
  "meta_discoveries": [
    { "kind": "vocabulary-cluster", "scope": "scope/bash-parser-boundary", "evidence": ["task-x", "task-y", "task-z"],
      "implication_for_next_plan": "<one sentence>" }
  ],
  "asks_for_user_open": [
    { "id": "ask-<cycle-id>-<seq>", "kind": "decision", "blocks": ["<task>"], "path": "<change-dir>/inbox/<id>.org" }
  ],
  "asks_for_user_resolved": [
    { "id": "ask-<cycle-id>-<seq>", "kind": "decision", "status": "applied | answered | deferred",
      "path": "<change-dir>/cycles/<cycle-id>/asks/<id>.org",
      "decision": "<the * Answer>", "decision_readback": "<the * Readback, or null>", "applied_via": "<ref, or null>" }
  ],
  "user_resolved_goal_drift": [
    { "ask": "ask-<cycle-id>-<seq>", "decision": "revise | split | abandon | continue", "rationale": "<the readback>" }
  ],
  "goal_drift_recommendation": { "recommendation": "none | revise | split | abandon | continue", "reason": "<one line>" },
  "task_refinements": [
    { "task": "<change-dir>/tasks/open/<name>.md",
      "impact": [ { "channel": "design-diff | meta-discovery | user-resolved-ask | inline-fix", "ref": "<id>" } ],
      "obsolescence_flagged": false }
  ],
  "candidate_tasks": [
    { "name": "got-home-carries-line", "source": "arch-cycle-<id>-3", "discovered_by": "architect",
      "discovered_class": "invariant-gap", "critical": true, "quote": "<the finding, discovery or decision, verbatim>",
      "evidence": "<change-dir>/cycles/<cycle-id>/findings/<file>", "placement": "in-change" }
  ],
  "journal": [ /* the cycle's `note` entries from the event log, as `state.py log` prints them */ ]
}
```

## Where each field comes from

| Field | Source | Plan reads it for |
|---|---|---|
| `design_diff` | `cycles/<cycle-id>/design-diff.json` (integrate § 1); `[]` when absent | which seam rows moved |
| `meta_discoveries` | `cycles/<cycle-id>/meta-discoveries.json` (integrate § 4), the PM's | what to weigh when drafting the batch |
| `asks_for_user_open` | `state.py asks --cycle <cycle-id>`, its `open` | the questions still waiting, and what they block |
| `asks_for_user_resolved` | `state.py asks --cycle <cycle-id>`, its `resolved`: the files answered, applied, deferred, declined or superseded since the cycle started, and every `answered` or `deferred` one still in the inbox (it waits on someone, so it is carried until applied), with their `* Answer`, `* Readback` and `:APPLIED_VIA:` | what the user decided |
| `user_resolved_goal_drift` | the goal-drift ask files (a `decision` whose options are revise / split / abandon / continue) answered with one of those labels exactly | whether the intent was revised |
| `goal_drift_recommendation` | the PM's reading of the cycle against the intent's Goals (`roles/project-manager.md`); `none` when nothing drifted | whether a goal-drift ask is warranted, and the user's context for it |
| `task_refinements` | `cycles/<cycle-id>/task-refinements.json` (integrate § 7) | what each open task must absorb; which are candidate-obsolete |
| `candidate_tasks` | `cycles/<cycle-id>/candidate-tasks.json` (execute § 5, § 8 and integrate § 7); `[]` when absent | the tasks it writes |
| `journal` | `state.py log --cycle <cycle-id> --kind note` | the discoveries and decisions the cycle recorded |

A **candidate** has `name`, `source`, `discovered_by`, `critical` and `quote`, and may have `discovered_class`, `evidence` and `placement` (`in-change` or `.tasks`, default `in-change`), nothing else; names are unique. `critical` is the route: blocking on the main path. A finding routed `candidate-task-<name>` (`state.py route`) must have a candidate of that name, or `handshake_written` fails. Plan writes each candidate as a task file whose `discovered_*` fields are the candidate's, and `state.py check new <name>` refuses one that does not name its source.

A **design-diff** element is `{seam, change, ref}`, `change` one of `added`, `amended`, `retired`, `ref` the finding or ask id that moved it.

## A handshake written before this template

A prior handshake written by the old state tool carries `pm_digest_path` and no `goal_drift_recommendation` (the recommendation was in the digest). `state.py cycle start` accepts it as the prior with that one field absent, and reads it with `candidate_tasks` empty when that is absent too, so a change live on the old tool starts its next cycle on this one with nothing converted. Plan reads the digest's goal-drift line for that one cycle.
