# Integrate phase

The backward channel firing, plus the loop-closing operation. Integrate consumes the cycle's discoveries and produces the inputs the next plan requires.

A cycle that leaves the design saying something the merged code no longer does has discovered without integrating — the failure the integrate phase exists to prevent. Integrate's first operation checks the code against the seams the batch cited and amends the rows the code has rightly outgrown.

## Operations

### 1. Design conformance (the load-bearing operation)

The Architect runs against the seams this cycle's tasks cited (`roles/architect.md` § Conformance): for each cited row in `design.md` § Seams, do the owning symbols exist and remain the single producer or mapping; do the coverage row's tests exist, carry no pending marker, and pass. Plus a dead-branch scan across every diff merged this cycle: is any replaced implementation still on a live path; is any touched function now unreachable. The check reads each merged diff in full (its divergence from its merge base), the modules it touched and their immediate neighbours, and the project priors.

Output: zero or more findings written to `<repo>/.orchestrator/cycles/<cycle-id>/findings/`, each indexed with `state.py record add findings '{...}'` (`trigger: conformance`; the id comes back); a check with zero findings is one `state.py note note "conformance: zero findings" --by architect`. Each finding has a severity that decides its **routing target** (the actual task-file write, when one is needed, happens in step 7's curation sweep so creation and refinement are unified); the routing lands as `state.py record set findings <id> resolution=<inline-fixed|followup-task-<name>|reverted|accepted-with-note|noted>`:

- **Code right, row stale** (`interface-drift` where the merged code is what the change wanted): the Architect amends the seam row in place, and with it the coverage rows and the Decision that named the same thing, and sets `resolution: design-amended`; the orchestrator records one `doc-correction` ask (`templates/ask.md`) with `status: applied`, `applied_via: design.md <seam-id>`, listed in § 3a under "Applied without asking". Not Implementor work. `design-diff.json` lists the seam row only.
- **Design in question** (the code and the row disagree and the row may be right): a `decision` ask, written by the orchestrator from the finding (which says what the seam is for, whether the drift is in merged code or reasoning only, and what it blocks), presented in § 3a.
- **`blocking`** with any other class → routed for follow-up task creation in step 7; the integrate gate doesn't close until the finding's `resolution` is no longer `pending`. (`blocking` at integrate holds the gate; there is no merge left to pause.)
- **`advisory`** → routed for follow-up task creation in step 7 with provenance; orchestrator decides this-batch / next-batch / `.tasks/`.
- **Any severity with `observed.happened: false`**, unless a project prior (`overlay.md` § Priors) or the user has asked for that kind of work or the finding states a measured cost → `state.py record set findings <id> resolution=noted`: no task, no `.tasks/` item, no `decision` ask. A stale sentence the finding exposed is still a `doc-correction`, applied and listed, so the next plan does not re-derive the work from it. A `blocking` finding that rests on reasoning only is a contradiction; the Architect re-grades it `advisory`.
- **`informational`** → no task; PM digest's "trends to watch" section.

Then the step writes `<repo>/.orchestrator/cycles/<cycle-id>/design-diff.json`: a JSON array, one element per seam row this cycle added, amended or retired, `{"seam": "seam/<name>", "change": "added | amended | retired", "ref": "<finding-id or ask-id>"}`; an empty array when nothing changed. Step 8's handshake carries it as `design_diff`; step 7 uses it to find the open tasks whose cited rows moved. The amended `design.md` is committed with the cycle's other integrate artifacts.

The record shows why this is scoped to cited seams and the dead-branch scan rather than a full structural audit: across two projects the audit's structural classes fired rarely, the findings that paid were dead code the user's priors licensed deleting, and the catalogue the audit maintained was written more than it was read. Restore the wider audit on a structural class that fires in a real cycle and this check did not catch.

### 2. (Folded into step 1)

The end-of-cycle audit is the conformance check above. Nothing runs here.

### 3. PM digest

Two passes, per `roles/project-manager.md`:

1. **Deterministic pass** — `state.py counts --write` produces `<repo>/.orchestrator/cycles/<cycle-id>/pm-signals.json`: counts, ratios, history, by-status, critical-path readout, class table, follow-ups by source, open asks, blocked tasks, the findings count with how many are reasoning-only and `noted`, and the fired signals it can compute. Blocked tasks whose `blocker_note` is an ask id re-surface that id rather than raising a new ask; a blocked task with another kind of blocker is a candidate ask the orchestrator writes per `templates/ask.md`.
2. **Agent pass** — turns that into the digest prose at `<repo>/.orchestrator/cycles/<cycle-id>/pm-digest.md` (per `templates/pm-digest.md`).

If the agent pass fails, the deterministic output remains and is recoverable.

### 3a. Ask triage, presentation and readback

The one place in the cycle where questions reach the user. Per `templates/ask.md`:

1. Collect every ask with `status: open` (`state.py status` lists their ids; the records are in `asks_for_user`), including those recorded during execute (`flows/execute.md` § 9) and plan.
2. Triage by kind. `doc-correction` and `confirmation` are applied and listed; `process` is counted; `environment` should already have been asked. Only `decision` is presented. A finding that a line in `priors.md` already answers gets no ask record: it is `noted` with the prior cited in `evidence` (`overlay.md` § Priors). If an ask record already exists (raised mid-execute, or carried in the handshake), it is `declined` with the prior as `decision_readback` and listed under "Applied without asking".
3. Fill any field a stub or finding lacks (`about`, `observed`, `options`) with `state.py record set asks <id> …` when that is a matter of reading code or docs; send a finding back to its role when it would mean redoing the analysis (`templates/ask.md` § What a finding must carry). A finding that exposes both a stale sentence and a product question yields two records: a `doc-correction` and a `decision`.
4. Write `<repo>/.orchestrator/cycles/<cycle-id>/asks.md` in the template's rendering: orientation, applied-without-asking, triage table, settle-now blocks, can-wait lines.
5. Send the chat message: orientation, settle-now blocks, can-wait lines, the path. Asks first; the cycle's results after or in a separate message.
6. On the user's answer, send the readback (each decision as a consequence) and wait for a yes. Then `state.py record set asks <id> decision=<label> decision_readback="<the readback>" status=answered`, append to the Decisions section, and apply. `record set asks <id> status=applied applied_via=<ref>` only once it is applied. A reply that states a general rule rather than a choice is also appended, in the user's words, to the overlay's `priors.md` (`overlay.md` § Priors), with a `confirmation` record listed under "Applied without asking".
7. Asks the user leaves unanswered keep `status: open`, their default applies at the boundary it names, and they are carried in the handshake.

A cycle with no `decision`-kind asks writes an `asks.md` whose triage table is empty and says so in one line; that is a valid outcome, not a gate failure.

### 4. Meta-discovery surfacing

The PM's "trends to watch" + the Architect's pattern-class clusters get distilled into what the next plan should weigh when drafting its batch.

Examples:
- "We keep finding vocabulary mismatches at the bash-parser/scope boundary." → The next design round or plan should look for a missing seam there before drafting tasks.
- "Cascade follow-ups cluster around responsibility-leakage in scope-expansion.el." → That module's stated responsibility is drifting; the next conformance check weighs its module-purpose question higher.

Meta-discoveries land in the integrate→plan handshake artifact's `meta_discoveries` field, structured as:

```json
{
  "kind": "vocabulary-cluster | shape-fragmentation-cluster | invariant-gap-class | other",
  "scope": "<module / boundary / change name>",
  "evidence": ["<task-id>", "<task-id>", ...],
  "implication_for_next_plan": "<one sentence>"
}
```

Write them as a JSON array to `<repo>/.orchestrator/cycles/<cycle-id>/meta-discoveries.json`; step 8 passes it to the handshake. Per-cycle meta-discoveries surface in this digest and act on the next plan. Recurring meta-discoveries across many cycles distill, via the deferred curation cycle (v2), into durable design priors.

### 5. Goal-drift check

Does the gap between "tasks complete" and "proposal.md outcome reachable" suggest the proposal itself is drifting? The PM's goal-drift query fires when:

- Critical-path completion ratio is stagnant or declining for K cycles.
- Non-critical-path completions continue at normal pace.
- Drainage stays positive (so it's not throughput inversion).

If the query fires:
- The PM digest carries a goal-drift recommendation: revise / split / abandon / continue.
- The proposal status header (per `templates/proposal-status-header.md`) flips to `divergent`.
- An ask record of kind `decision` is raised (`templates/ask.md`) with the four fixed options revise / split / abandon / continue, and presented in § 3a. The tool recognises the goal-drift ask by exactly those four option labels.

The user's decision lands in the ask record; `state.py handshake` copies it into `user_resolved_goal_drift` as `{ask, decision, rationale}`. Plan refuses to start the next cycle until the user has dispositioned (or explicitly chosen `continue`).

### 6. Externalisation review

`.tasks/` pressure check — has the cross-cutting backlog grown to the point where a cluster should be promoted into the active change?

The PM checks: of the externalised tasks, is there a cluster whose `discovered_class` distribution and modules-touched are coherent enough to constitute a sub-change? If yes:

- Raise an ask record (kind `decision`; default if unanswered: leave externalised): "promote cluster `vocabulary-mapping` (5 tasks, 3 cycles old) into the active change?"
- The user dispositions: promote, leave externalised, or open a new change.

### 7. Open-task refinement

The cycle's task list is both an input (what plan produced) and an output (what the next cycle inherits). Without this step, plan-time prose ages: it cites seam rows in their pre-cycle shape, ignores meta-discoveries that change implementor defaults, and prescribes work that inline fixes already shipped. The next implementor reads stale instructions, the brief overlay only patches what's cited, and the cycle has discovered without integrating *into the work that remains*.

This is the **single curation point** for the open task list. Both shapes of curation happen here:

1. **Refining existing open tasks** — absorbing the design diff, meta-discoveries, user-resolved asks, and inline fixes into the bodies of tasks that remain in `<change>/tasks/open/`.
2. **Creating new tasks** from this cycle's findings, asks, and discoveries — the conversion of step 1's conformance findings (and plan's premise-check findings), step 4's meta-discoveries, and step 5/6's user routing into actual files in `<change>/tasks/open/` (or `.tasks/` per `externalisation.md`).

Steps 1–6 *identify* what needs to land; step 7 *lands it*. Earlier steps may name file paths in their findings (`producing follow-up task X` etc.) but the file write is here, so a single sweep over the open task list keeps creation and refinement coherent.

Refinement runs **after** externalisation review (so externalised tasks have already left `<change>/tasks/open/`) and **before** the handshake (so the handshake can record what was created and refined).

#### Create new tasks from cycle outputs

Walk the cycle's outputs for new-task triggers:

- **Architect findings** (from step 1, and from plan's premise check). A task is created only on a demonstrated reason: `observed.happened` is true, or a project prior (`overlay.md` § Priors) or the user asked for that kind of work, or the finding states a measured cost. "The user asked" means the user's words: a prior, an ask decision, or a proposal sentence the user wrote; a `design.md` sentence is an agent's plan and a prior outranks it, so a design sentence a prior contradicts is a `doc-correction`. Otherwise `record set findings <id> resolution=noted`: no task, no `.tasks/` item, no `decision` ask (a `doc-correction` may still be applied); the digest lists the noted count in one line. Per the routing in step 1: `interface-drift` → a `doc-correction` (applied) or a `decision` ask (no task); `blocking` other class → in-batch follow-up task with `discovered_from: <finding-id>`, `discovered_by: architect`, `discovered_class: <finding.class>`; `advisory` → follow-up task, orchestrator decides this-batch vs next-batch vs `.tasks/`; `informational` → no task. The same test applies to Reviewer findings and Implementor `## Discoveries` that reach this step: each carries `observed`. Implementor `## Observations` were triaged in execute § 5 and are not re-read here; the conformance check finds what an observation pointed at on its own.
- **Open asks** (`asks_for_user` with `status: open`, from steps 1, 5, 6, plan and execute) create no task. Each task named in an ask's `blocks` carries `status: blocked` and `blocker_note: <ask-id>` (`templates/ask.md` § Blocking without a second artifact); the note is cleared when the ask is applied.
- **User-resolved asks with deferred implementation** (`asks_for_user_resolved[i]` where `applied_via` indicates deferral, e.g. `deferred-to-cycle-N`). If the deferral target is *not* an existing open task, create one carrying the user's decision in its body and `discovered_from: <ask-id>`.
- **Meta-discoveries with concrete forward-looking work** (`meta_discoveries[i].implication_for_next_plan` names a specific task or rewire). If the implication is concrete enough to be its own task and is not absorbed by an existing open task's refinement, create the task with `discovered_from: meta-discovery/<label>`, `discovered_class: <meta.kind>`.

For each created task, apply the externalisation rule (`externalisation.md`): in-change if it contributes to the active proposal's outcome; `.tasks/` if cross-cutting. Each is registered with `state.py task add <name> --file <path> --class <class> discovered_from=<id> discovered_by=<role> discovered_class=<class>`; an externalised one gets `task set <name> externalised` straight after.

Each created task is also added to the `task_refinements` list (with `modes: ["created"]`) so the create/refine accounting is unified. When step 7 has created tasks, re-run `state.py counts --write` so `pm-signals.json` and the digest's throughput table include them; the digest's numbers must match the state file at close.

#### Compute each open task's impact set

For each task in `<change>/tasks/open/<task-name>.md`, intersect against the cycle's outputs:

- **(a) Design-diff hits.** `task.cites_seams ∩ design_diff[].seam`. Each hit names a cited seam row that was added, amended or retired this cycle.
- **(b) Meta-discovery hits.** Any `meta_discoveries[i]` whose `scope` matches one of the task's cited seams, OR whose `evidence` array names this task or any task that cited the same seams.
- **(c) User-resolved-ask hits.** Any `asks_for_user_resolved[i]` whose `applied_via` names this task, or names a task or inline fix whose files overlap this task's "Files to modify" list or whose seams this task cites.
- **(d) Inline-fix hits.** Any finding with `resolution: inline-fixed` (or `inline-fix` journal entry) whose locations overlap the task's "Files to modify" or implicate code paths the task prescribes.

A task with an empty impact set across all four channels is left untouched.

#### Refine: edit-in-place vs append

Two refinement modes, chosen mechanically per impact:

**Edit in place** when the task's existing prose is **demonstrably false or dead** in light of the impact:

- Prose names a symbol, field or value that a seam row amended this cycle no longer carries.
- Prose prescribes a code change (numbered step, file edit, function add/remove) that an inline fix or a merged in-cycle task already shipped.
- Prose cites a code path (`file:fn`) that was deleted or renamed by an inline fix this cycle.
- A verification command references an artifact that no longer exists.

The edit replaces the false text with the corrected statement and leaves a one-line provenance breadcrumb at the top of the edited section: `> Cycle <N>: obviated/corrected by inline fix; see <finding-id or design commit>.`. Don't leave dead prose; do leave an audit trail.

**Append a `## Cycle <N> updates (cycle-<ts>)` stanza** otherwise:

- A cited seam row was amended but the task's prose still applies (the statement is now firmer or has minor additions; the work remains).
- A meta-discovery is relevant to how this task should approach its work (e.g., a clustering pattern that changes the implementor's default).
- A user-resolved ask has implications for this task's verification or implementation choices without invalidating existing prose.
- A related cycle artifact (inline fix, merged task) provides context the implementor should know about going in.
- The task may now be **wholly obsolete** — flag for user disposition; do not auto-close. Raise an ask record of kind `confirmation` (`default_if_unanswered: leave open`, `blocks: [<task>]`) and append a stanza that names the claim and the ask id.

Stanza form: see `templates/task-update-stanza.md`. Tasks may accumulate stanzas across cycles, newest-last.

Both modes preserve the task's frontmatter and any existing `## Observations` / `## Discoveries` (those are execute-phase artifacts and must not be touched).

#### Record the refinement for the handshake

For every refined task, append to `<repo>/.orchestrator/cycles/<cycle-id>/task-refinements.json` (a JSON array; step 8 passes it to the handshake):

```json
{
  "task": "openspec/changes/<change>/tasks/open/<name>.md",
  "modes": ["in-place"] | ["append"] | ["in-place", "append"],
  "applied_learnings": [
    { "channel": "design-diff", "ref": "seam/<name>" },
    { "channel": "meta-discovery", "ref": "<kind>/<label>" },
    { "channel": "user-resolved-ask", "ref": "<ask-id>" },
    { "channel": "inline-fix", "ref": "<finding-id>" }
  ],
  "obsolescence_flagged": false
}
```

`obsolescence_flagged: true` surfaces in the next plan as a candidate-close. Plan does not auto-close; the `confirmation` ask carries the disposition.

A task that was touched but not modified — i.e. impact set was non-empty but inspection determined no actual prose change is warranted — still gets a refinement entry with `modes: []` and the channel(s) considered. This makes "we looked, we decided nothing was stale" auditable rather than indistinguishable from "we never looked."

### 8. Write the integrate→plan handshake artifact

The cycle's loop-closing artifact. Path: `<repo>/.orchestrator/handshake-<cycle-id>.json`. Written by

```bash
state.py handshake --meta-discoveries @.orchestrator/cycles/<cycle-id>/meta-discoveries.json \
                   --task-refinements @.orchestrator/cycles/<cycle-id>/task-refinements.json
```

which reads `design_diff` from `cycles/<cycle-id>/design-diff.json` (step 1; `--design-diff @path` overrides, and an absent file means `[]`), fills `pm_digest_path`, `user_resolved_goal_drift`, the two ask lists and the journal from state, and validates the result:

```json
{
  "cycle_id": "<this-cycle>",
  "produced_at": "<iso-ts>",
  "design_diff": [
    { "seam": "seam/<name>", "change": "added | amended | retired", "ref": "<finding-id or ask-id>" }
  ],
  "pm_digest_path": ".orchestrator/cycles/<cycle-id>/pm-digest.md",
  "meta_discoveries": [...],
  "user_resolved_goal_drift": [...],
  "asks_for_user_open": [ /* records per templates/ask.md with status open, copied unchanged */ ],
  "asks_for_user_resolved": [ /* the same records with decision, decision_readback, applied_via set */ ],
  "task_refinements": [
    {
      "task": "openspec/changes/<change>/tasks/open/<name>.md",
      "modes": ["created"] | ["in-place"] | ["append"] | ["in-place", "append"] | [],
      "applied_learnings": [
        { "channel": "design-diff | meta-discovery | user-resolved-ask | inline-fix | finding | open-ask | deferred-ask",
          "ref": "<id>" }
      ],
      "obsolescence_flagged": false
    }
  ]
}
```

**All seven required fields are mandatory** (`design_diff`, `pm_digest_path`, `meta_discoveries`, `user_resolved_goal_drift`, `asks_for_user_open`, `asks_for_user_resolved`, `task_refinements`; the tool defines the list once). An empty list is allowed; a missing field is not. The next plan's first operation is to read this file; `state.py init` refuses to start if any field is missing.

This is the structural fix for the brainstorm's "learns and forgets" failure mode. Without the handshake, plan degrades into "pull from the top of the backlog" and the orchestrator becomes a queue runner.

## Inputs (from execute)

- All Implementor reports (held by orchestrator since execute, not seen by reviewers).
- All Reviewer findings.
- Plan's premise-check findings, if any.
- The set of merged diffs.
- The seam rows and coverage rows this cycle's tasks cite (`cites_seams` in state; the rows in `design.md`).
- `phase_gates.execute.passed: true` (mandatory; `state.py phase set integrate` enforces it).

## Exit gate

| Check | Condition |
|---|---|
| `blocking_findings_resolved` | Every `architect_findings[i]` with `severity: blocking` has `resolution != pending` |
| `pm_digest_produced` | `pm-digest.md` exists with a non-empty `signals` section (the asks table may be empty) |
| `user_asks_routed` | Every `asks_for_user[]` record with `status: open` appears in `handshake.asks_for_user_open`, and every one of kind `decision` appears in `cycles/<cycle-id>/asks.md` (so plan picks them up next cycle) |
| `open_tasks_refined_against_handshake` | Every still-open task in `<change>/tasks/open/` has been considered by step 7. A task either has an entry in `handshake.task_refinements` (with `modes` populated, possibly empty) or has been excluded explicitly because it had no impact-set hits. New tasks created in step 7 are present on disk and have a `task_refinements` entry with `modes: ["created"]`. Findings flagged in step 1 for follow-up task creation each have a corresponding created task; `noted` findings have none. **Asserted**: `state.py gate set integrate open_tasks_refined_against_handshake=true` |
| `handshake_artifact_written` | `handshake-<cycle-id>.json` exists with all seven required fields populated |

Four of the five are computed by `state.py gate check integrate`; the fifth is asserted. `state.py gate pass integrate` sets the gate when all pass; then `state.py close`. The next plan refuses to start otherwise — that's the loop-closure contract.

## Cycle archive

`state.py close` archives the cycle (state and handshake; the other files were written there during the cycle):

```
.orchestrator/cycles/<cycle-id>/
  state.json              # frozen snapshot of the cycle's state file
  pm-signals.json         # counts --write
  pm-digest.md
  asks.md                 # the cycle's asks as presented, with the decisions
  handshake.json
  premise-check.md        # plan § 3
  design-diff.json        # step 1 input to the handshake
  meta-discoveries.json   # step 4 input to the handshake
  task-refinements.json   # step 7 input to the handshake
  findings/<finding-id>.md
  reviews/<task-name>.md
  reports/<task-name>.md  # Implementor reports, orchestrator-only
```

The active state file stays in place, marked `closed_at`; the next cycle's `state.py init` replaces it and carries the counts `history` from the archives.

This archive is what the deferred curation cycle (v2) reads. It is also the audit trail the user can walk through to understand any past decision.

## Loop closure: integrate → plan

The keystone transition is integrate → plan, **not** execute → review. Each plan is *required* to consume the prior integrate's outputs. Without that as a hard input contract, plan degrades into queue-running and the orchestrator becomes a glorified task runner.

This is the loop that gives the system *compounding leverage*: each cycle's discoveries make the next cycle's batch better. It is the operationalisation, at cycle altitude, of the two-way information flow.

## What integrate does **not** do

- Integrate does not modify code. (Conformance amends `design.md`, which is an artifact; inline fixes from blocking findings happen in the active batch's worktrees, which is the orchestrator's responsibility but happens in execute-mode mechanics — `git merge --abort` then re-spawn — even if triggered from integrate's findings.)
- Integrate does not run new test suites — the cycle's tests already ran in execute; the conformance check may re-run them read-only to confirm a promoted test passes.
- Integrate does not re-implement tasks — re-implementation is execute's job; integrate only routes findings.
