# Integrate phase

The backward channel firing, plus the loop-closing operation. Integrate consumes the cycle's discoveries and produces the inputs the next plan requires.

A cycle that produces follow-up tasks but leaves speculated entries un-dispositioned has discovered without integrating — the failure the integrate phase exists to prevent. Integrate's exit gate enforces this structurally; it does not close until **every touched register entry has a disposition**.

## Operations

### 1. Register reconciliation (the load-bearing operation)

Enumerate every speculated entry the cycle touched (from the state file's `register_touched` array). For each:

- **`speculated → confirmed`** if the implementation matched. Write a terse reconciliation note (per `templates/reconciliation-note.md`) recording "speculation matched".
- **`speculated → divergent`** if the implementation pushed back AND the divergence is unresolved. Write a divergent reconciliation note with `routes_to: architect | user` and `proposed_resolution`. **Divergent entries block merge** of any task that cited them, until resolved.
- **`speculated → reconciled`** if the entry was updated to match the discovery. Write a reconciliation note with `prior_shape`, `new_shape`, and the **mandatory `why_tests_missed` line**.

The state file's `register_touched[i].status_at_integrate` flips to one of `confirmed` / `divergent` / `reconciled` for every touched entry. An entry whose `status_at_integrate` remains null is a state-file bug; the integrate gate refuses to close.

This is the gate that distinguishes a system that gets smarter from one that keeps rediscovering the same thing.

#### Scaffolding diffs as evidence

For touched entries with a `scaffolding_path` (per `scaffolding.md`), the diff against the merge-base of the scaffolded file is **evidence** for the status transition. The classification is mechanical, not narrative:

| Scaffolding diff during execute | Register entry transitions to | `scaffolding_status_at_integrate` | Reconciliation note carries |
|---|---|---|---|
| Untouched + scaffold green at end of cycle | `confirmed` | `untouched` (transient) → set to `promoted` or `archived` below | "Scaffold passed unchanged. Speculation matched." |
| Modified by Implementor + reviewer accepted | `reconciled` | `modified` | `prior_form` (pre-diff), `new_form` (post-diff), `why_tests_missed` — the diff is the substrate |
| Modified by Implementor + reviewer rejected | `divergent` | `rejected` | `divergence_evidence` cites the rejected modification; `escalation: architect \| user` |
| Strict-skip still skipping | (gate failure) | (un-dispositioned) | Integrate refuses to close until disposition is set |

Then for every confirmed/reconciled scaffold, set the **final disposition**:

- **`promoted`** — the scaffold migrates to its permanent home (`test/`, the target module). Integrate generates a follow-up task carrying `discovered_class: scaffolding-promotion` to perform the migration. The task is added to the next cycle's batch unless promoted in-cycle.
- **`archived`** — enforcement landed via a different mechanism (e.g. a runtime check at file:fn). The reconciliation note records *where* enforcement actually lives.
- **`rejected`** (already set above for divergent) — speculation was wrong; scaffold is deleted; the divergent entry's resolution path determines next steps.

`scaffolding_status_at_integrate` must be one of `promoted | archived | rejected` for every scaffolded file. Transient `untouched` / `modified` is not a final disposition; the gate enumerates these and refuses to close.

The mandatory `why_tests_missed` line on `reconciled` entries gets concrete substrate — the diff itself — rather than the Architect's narrative reconstruction. Reconciliation moves from judgment to mechanical classification, with the Architect's prose layer reduced to "what pattern does this diff exemplify, for the meta-discoveries field?"

### 2. Architect end-of-cycle audit

Full signal-class run across all the cycle's diffs + the register. (Not a separate trigger; one of integrate's defining operations. See `roles/architect.md` for the eight signal classes.)

Output: zero or more findings written to `<repo>/.orchestrator/cycles/<cycle-id>/findings/`. Each finding has a severity that decides its **routing target** (the actual task-file write, when one is needed, happens in step 7's curation sweep so creation and refinement are unified):

- **`blocking`** with `interface-drift` against an out-of-date design doc → routes to **the user** as an integrate-phase ask.
- **`blocking`** with any other class → routed for follow-up task creation in step 7; merge of the implicated task pauses; integrate gate doesn't close until the finding's `resolution` is no longer `pending`.
- **`advisory`** → routed for follow-up task creation in step 7 with provenance; orchestrator decides this-batch / next-batch / `.tasks/`.
- **`informational`** → no task; PM digest's "trends to watch" section.

### 3. PM digest

Two passes, per `roles/project-manager.md`:

1. **Deterministic pass** — produces `<repo>/.orchestrator/cycles/<cycle-id>/pm-signals.json` with all counts, ratios, fired-signals list, candidate-asks list.
2. **Agent pass** — turns that into the digest prose at `<repo>/.orchestrator/cycles/<cycle-id>/pm-digest.md` (per `templates/pm-digest.md`).

If the agent pass fails, the deterministic output remains and is recoverable.

### 4. Meta-discovery surfacing

The PM's "trends to watch" + the Architect's pattern-class clusters get distilled into updated speculation priors for the next plan.

Examples:
- "We keep finding vocabulary mismatches at the bash-parser/scope boundary." → Future forward speculation in that area should probe vocabulary first.
- "Cascade follow-ups cluster around responsibility-leakage in scope-expansion.el." → That module's stated responsibility is drifting; weight its module-purpose audit higher next cycle.

Meta-discoveries land in the integrate→plan handshake artifact's `meta_discoveries` field, structured as:

```json
{
  "kind": "vocabulary-cluster | shape-fragmentation-cluster | invariant-gap-class | other",
  "scope": "<module / boundary / change name>",
  "evidence": ["<task-id>", "<task-id>", ...],
  "implication_for_next_plan": "<one sentence>"
}
```

Per-cycle meta-discoveries surface in this digest and act on the next plan. Recurring meta-discoveries across many cycles distill, via the deferred curation cycle (v2), into durable speculation priors.

### 5. Goal-drift check

Does the gap between "tasks complete" and "proposal.md outcome reachable" suggest the proposal itself is drifting? The PM's goal-drift query fires when:

- Critical-path completion ratio is stagnant or declining for K cycles.
- Non-critical-path completions continue at normal pace.
- Drainage stays positive (so it's not throughput inversion).

If the query fires:
- The PM digest carries a goal-drift recommendation: revise / split / abandon / continue.
- The proposal status header (per `templates/proposal-status-header.md`) flips to `divergent`.
- An ask routes to the user with the three structured options.

The user's decision lands in the handshake artifact's `user_resolved_goal_drift` field. Plan refuses to start the next cycle until the user has dispositioned (or explicitly chosen `continue`).

### 6. Externalisation review

`.tasks/` pressure check — has the cross-cutting backlog grown to the point where a cluster should be promoted into the active change?

The PM checks: of the externalised tasks, is there a cluster whose `discovered_class` distribution and modules-touched are coherent enough to constitute a sub-change? If yes:

- Surface as an ask: "promote cluster `vocabulary-mapping` (5 tasks, 3 cycles old) into the active change?"
- The user dispositions: promote, leave externalised, or open a new change.

### 7. Open-task refinement

The cycle's task list is both an input (what plan produced) and an output (what the next cycle inherits). Without this step, plan-time prose ages: it cites registers in their pre-cycle shape, ignores meta-discoveries that change implementor defaults, and prescribes work that inline fixes already shipped. The next implementor reads stale instructions, the brief overlay only patches what's cited, and the cycle has discovered without integrating *into the work that remains*.

This is the **single curation point** for the open task list. Both shapes of curation happen here:

1. **Refining existing open tasks** — absorbing register-diff, meta-discoveries, user-resolved asks, and inline fixes into the bodies of tasks that remain in `<change>/tasks/open/`.
2. **Creating new tasks** from this cycle's findings, asks, and discoveries — the conversion of step 2's audit findings, step 4's meta-discoveries, and step 5/6's user routing into actual files in `<change>/tasks/open/` (or `.tasks/` per `externalisation.md`).

Steps 2–6 *identify* what needs to land; step 7 *lands it*. Earlier steps may name file paths in their findings (`producing follow-up task X` etc.) but the file write is here, so a single sweep over the open task list keeps creation and refinement coherent.

Refinement runs **after** externalisation review (so externalised tasks have already left `<change>/tasks/open/`) and **before** the handshake (so the handshake can record what was created and refined).

#### Create new tasks from cycle outputs

Walk the cycle's outputs for new-task triggers:

- **Architect findings** (from step 2). Per the routing in step 2: `blocking` with `interface-drift` → user ask (no task); `blocking` other class → in-batch follow-up task with `discovered_from: <finding-id>`, `discovered_by: architect`, `discovered_class: <finding.class>`; `advisory` → follow-up task, orchestrator decides this-batch vs next-batch vs `.tasks/`; `informational` → no task.
- **User-asked questions** (`asks_for_user_open`, from steps 2 & 5). Each open ask gets a disposition task in `<change>/tasks/open/` with `status: blocked`, `relations.blocked-by: <task-this-blocks>`, `discovered_from: <finding-id>`, body templated against the ask's options. The task closes when the user resolves the ask in a future cycle's handshake.
- **User-resolved asks with deferred implementation** (`asks_for_user_resolved[i]` where `applied_via` indicates deferral, e.g. `deferred-to-cycle-N`). If the deferral target is *not* an existing open task, create one carrying the user's decision in its body and `discovered_from: <ask-id>`.
- **Meta-discoveries with concrete forward-looking work** (`meta_discoveries[i].implication_for_next_plan` names a specific task or rewire). If the implication is concrete enough to be its own task and is not absorbed by an existing open task's refinement, create the task with `discovered_from: meta-discovery/<label>`, `discovered_class: <meta.kind>`.

For each created task, apply the externalisation rule (`externalisation.md`): in-change if it contributes to the active proposal's outcome; `.tasks/` if cross-cutting. Externalised tasks carry the same provenance fields plus `status: externalised`.

Each created task is also added to the handshake's `task_refinements` array (with `modes: ["created"]`) so the create/refine accounting is unified.

#### Compute each open task's impact set

For each task in `<change>/tasks/open/<task-name>.md`, intersect against the cycle's outputs:

- **(a) Register-diff hits.** `task.cites_register_entries ∩ register_diff[].entry_id`. Each hit names a cited entry whose `status` flipped this cycle (`speculated → confirmed | divergent | reconciled`).
- **(b) Meta-discovery hits.** Any `meta_discoveries[i]` whose `scope` matches one of the task's cited entries, OR whose `evidence` array names this task or any task that cited the same register entries.
- **(c) User-resolved-ask hits.** Any `asks_for_user_resolved[i]` whose `register_changes` modify a cited entry, OR whose `code_changes` touch a file in the task's "Files to modify" list, OR whose `applied_via` names this task as the deferral target.
- **(d) Inline-fix hits.** Any `audit_inline_fixed_findings[i]` whose locations overlap the task's "Files to modify" or implicate code paths the task prescribes.

A task with an empty impact set across all four channels is left untouched.

#### Refine: edit-in-place vs append

Two refinement modes, chosen mechanically per impact:

**Edit in place** when the task's existing prose is **demonstrably false or dead** in light of the impact:

- Prose names a register-entry shape, field, or vocabulary member that was reconciled away this cycle (not present in the new shape).
- Prose prescribes a code change (numbered step, file edit, function add/remove) that an inline fix or a merged in-cycle task already shipped.
- Prose cites a code path (`file:fn`) that was deleted or renamed by an inline fix this cycle.
- A verification command references an artifact that no longer exists.

The edit replaces the false text with the corrected statement and leaves a one-line provenance breadcrumb at the top of the edited section: `> Cycle <N>: obviated/corrected by inline fix; see <reconciliation-note-path-or-finding-id>.`. Don't leave dead prose; do leave an audit trail.

**Append a `## Cycle <N> updates (cycle-<ts>)` stanza** otherwise:

- A cited register entry's status flipped but the task's prose still applies (the entry's contract is now firmer or has minor additions; the work remains).
- A meta-discovery is relevant to how this task should approach its work (e.g., a clustering pattern that changes the implementor's default).
- A user-resolved ask has implications for this task's verification or implementation choices without invalidating existing prose.
- A related cycle artifact (inline fix, merged task) provides context the implementor should know about going in.
- The task may now be **wholly obsolete** — flag for user disposition; do not auto-close. Append a stanza that names the obsolescence claim and invites the user to close the task.

Stanza form: see `templates/task-update-stanza.md`. Tasks may accumulate stanzas across cycles, newest-last.

Both modes preserve the task's frontmatter and any existing `## Observations` / `## Discoveries` (those are execute-phase artifacts and must not be touched).

#### Record the refinement in the handshake

For every refined task, append to the handshake's `task_refinements` array:

```json
{
  "task": "openspec/changes/<change>/tasks/open/<name>.md",
  "modes": ["in-place"] | ["append"] | ["in-place", "append"],
  "applied_learnings": [
    { "channel": "register-diff", "ref": "register/<tier>/<id>", "from": "speculated", "to": "reconciled" },
    { "channel": "meta-discovery", "ref": "<kind>/<label>" },
    { "channel": "user-resolved-ask", "ref": "<ask-id>" },
    { "channel": "inline-fix", "ref": "<finding-id>" }
  ],
  "obsolescence_flagged": false
}
```

`obsolescence_flagged: true` surfaces in the next plan as a candidate-close. Plan does not auto-close; the user (or PM ask) dispositions.

A task that was touched but not modified — i.e. impact set was non-empty but inspection determined no actual prose change is warranted — still gets a refinement entry with `modes: []` and the channel(s) considered. This makes "we looked, we decided nothing was stale" auditable rather than indistinguishable from "we never looked."

### 8. Write the integrate→plan handshake artifact

The cycle's loop-closing artifact. Path: `<repo>/.orchestrator/handshake-<cycle-id>.json`.

```json
{
  "cycle_id": "<this-cycle>",
  "produced_at": "<iso-ts>",
  "register_diff": [
    { "entry_id": "...", "from": "speculated", "to": "reconciled", "note_path": "..." }
  ],
  "pm_digest_path": ".orchestrator/cycles/<cycle-id>/pm-digest.md",
  "meta_discoveries": [...],
  "user_resolved_goal_drift": [...],
  "asks_for_user_open": [],
  "asks_for_user_resolved": [],
  "task_refinements": [
    {
      "task": "openspec/changes/<change>/tasks/open/<name>.md",
      "modes": ["created"] | ["in-place"] | ["append"] | ["in-place", "append"] | [],
      "applied_learnings": [
        { "channel": "register-diff | meta-discovery | user-resolved-ask | inline-fix | finding | open-ask | deferred-ask",
          "ref": "<id>",
          "from": "<optional, for register-diff>",
          "to": "<optional, for register-diff>" }
      ],
      "obsolescence_flagged": false
    }
  ]
}
```

**All five required fields are mandatory** (`register_diff`, `meta_discoveries`, `user_resolved_goal_drift`, `asks_for_user_open` / `asks_for_user_resolved` as a pair, and `task_refinements`). An empty list is allowed; a missing field is not. The next plan's first operation is to read this file; if any field is missing, plan refuses to start.

This is the structural fix for the brainstorm's "learns and forgets" failure mode. Without the handshake, plan degrades into "pull from the top of the backlog" and the orchestrator becomes a queue runner.

## Inputs (from execute)

- All Implementor reports (held by orchestrator since execute, not seen by reviewers).
- All Reviewer findings.
- All Architect on-touch findings from execute.
- The set of merged diffs.
- The set of touched register entries (`state.json` `register_touched`).
- `phase_gates.execute.passed: true` (mandatory).

## Exit gate

| Check | Condition |
|---|---|
| `all_touched_entries_dispositioned` | Every `register_touched[i].status_at_integrate` is set (not null) |
| `all_scaffolding_dispositioned` | Every `register_touched[i]` with a non-null `scaffolding_path` has `scaffolding_status_at_integrate` set to one of `promoted` / `archived` / `rejected` (transient `untouched` / `modified` is not a final disposition). No-op when `scaffolding.enabled: false` |
| `blocking_findings_resolved` | Every `architect_findings[i]` with `severity: blocking` has `resolution != pending` |
| `pm_digest_produced` | `pm-digest.md` exists with non-empty `signals` and `asks` sections |
| `user_asks_routed` | Every entry in PM digest's `Asks for the user` section has a corresponding entry in `handshake.asks_for_user_open` (so plan picks them up next cycle) |
| `open_tasks_refined_against_handshake` | Every still-open task in `<change>/tasks/open/` has been considered by step 7. A task either has an entry in `handshake.task_refinements` (with `modes` populated, possibly empty) or has been excluded explicitly because it had no impact-set hits. New tasks created in step 7 are present on disk and have a `task_refinements` entry with `modes: ["created"]`. Findings flagged in step 2 for follow-up task creation each have a corresponding created task. |
| `handshake_artifact_written` | `handshake-<cycle-id>.json` exists with all five required fields populated |

When all seven pass, `phase_gates.integrate.passed` flips to `true`. The next plan refuses to start otherwise — that's the loop-closure contract.

## Cycle archive

When integrate closes successfully, the orchestrator archives the cycle:

```
.orchestrator/cycles/<cycle-id>/
  state.json              # frozen snapshot of the cycle's state file
  pm-digest.md
  handshake.json
  findings/<finding-id>.md
  reconciliations/<entry-id>.md
  reviews/<task-name>.md
  baseline-<ts>.txt
  after-<task>-<ts>.txt
```

The active state file at `<repo>/.orchestrator/state.json` is reset for the next cycle (with carried-over `history` window).

This archive is what the deferred curation cycle (v2) reads. It is also the audit trail the user can walk through to understand any past decision.

## Loop closure: integrate → plan

The keystone transition is integrate → plan, **not** execute → review. Each plan is *required* to consume the prior integrate's outputs. Without that as a hard input contract, plan degrades into queue-running and the orchestrator becomes a glorified task runner.

This is the loop that gives the system *compounding leverage*: each cycle's discoveries make the next cycle's speculations better. It is the operationalisation, at cycle altitude, of the two-way information flow.

## What integrate does **not** do

- Integrate does not modify code. (Reconciliation notes update register entries, which are artifacts; inline fixes from blocking findings happen in the active batch's worktrees, which is the orchestrator's responsibility but happens in execute-mode mechanics — `git merge --abort` then re-spawn — even if triggered from integrate's findings.)
- Integrate does not run new test suites — the cycle's tests already ran in execute.
- Integrate does not re-implement tasks — re-implementation is execute's job; integrate only routes findings.
