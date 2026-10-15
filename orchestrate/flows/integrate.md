# Integrate phase

The backward channel firing, plus the loop-closing operation. Integrate consumes the cycle's discoveries and produces the inputs the next plan requires.

A cycle that leaves the design saying something the merged code no longer does has discovered without integrating — the failure the integrate phase exists to prevent. Integrate's first operation checks the code against the seams the batch cited and amends the rows the code has rightly outgrown.

## Operations

### 1. Design conformance (the load-bearing operation)

The Architect runs against the seams this cycle's tasks cited (`roles/architect.md` § Conformance): for each cited row in `design.org` § Seams, do the owning symbols exist and remain the single producer or mapping; do the scenarios the cycle's tasks cite (`cites_scenarios`) pass: `coverage.py --scenario … --output <the cycle's last after-file>` shows each `pass`, not `pending`, `failed`, `not run` or `uncited`. Plus a dead-branch scan across every diff merged this cycle: is any replaced implementation still on a live path; is any touched function now unreachable. The check reads each merged diff in full (its divergence from its merge base), the modules it touched and their immediate neighbours, and the project priors.

Output: zero or more findings written to `<change-dir>/cycles/<cycle-id>/findings/`, each indexed with `state.py record add findings '{...}'` (`trigger: conformance`; the id comes back), and a `thought:` journal line per speculative need; a check with zero findings is one `state.py note note "conformance: zero findings" --by architect`. The Architect hands back one line with the counts (`SKILL.md` § Hand-back lines); the orchestrator reads the findings files, not the hand-back, and routes each by `templates/architect-finding.md` § Routing, reading `intent.org`'s Goals for the main path (the task-file write, when one is needed, happens in step 7's curation sweep so creation and refinement are unified). The routing lands as `state.py record set findings <id> decoration=<blocking|hardening> resolution=<value>`, one of the values `state.md` § Architect finding entry lists:

- **Which side is right is decided by a test, not by the code.** An edit that makes the design say what the code does is a record correction only when a test pins the code's behaviour on the point the edit changes; the finding's resolution names that test. An edit that removes or weakens something the design promised and the code does not do ("each call has its own timeout" where no call has one) is never a record correction, whatever the spec scenarios require: the code not keeping a promise is behaviour present in merged code, so it is a finding against the code (`roles/architect.md` § What the Architect reports). It is a `decision` ask only when the user's own words (a spec scenario, the intent, a prior) conflict with the promise; a product question the Architect sees is not enough, and the finding's recommended resolution carries it. With no pinning test, a disagreement is **Design in question** below. The record: one replay arm's conformance cut "with its own timeout" from a Decision because the spec did not require it; in another arm of the same cycle the Reviewer filed the same untimed call as blocking (a stop on a hung target never returns), and the rework bounded it. Earlier, conformance amended the design twice to match untested code, once each way (D14). A re-run of that arm's conformance under this rule turned the cut into a finding against the code; with "a `decision` ask when the promise is in doubt" as the escape, it also sent a defect it recommended fixing (a state call that fails on raw bytes) to the user as a question, hence the user's-words test.
- **Code right, row stale** (`interface-drift` where the merged code is what the change wanted and a test pins it): a record correction (`templates/design.md` § Seams). The Architect amends the seam row in place, and with it the Decision and the spec scenario steps that named the same thing, keeping each at the contract: when the drift is a guard, error string, encoding or helper the Implementor added, the row is cut back, never extended with it, and a Decision never gains one. It sets `resolution: design-amended`; the orchestrator writes one journal line, `state.py note note "record-correction: <what>" --ref <seam-id>`, and commits the amended text with a subject naming the row. Every commit that corrects record text says `record correction` in its subject, and one that corrects a scenario in `docs/specs/` names the pinning test in its body (`Pinned by: <test>`, one line per scenario), so the MVP decision and the closing summary can list each spec correction with its diff and test (`templates/hardening.md`). No ask file, no question: the record shows 47 of 47 such files were questions nobody answered, and 39 of them would not have existed under this rule. The correction reaches whatever the row cites: a scenario in `docs/specs/` that states the stale thing is corrected in the same commit, in place. A record correction does not rename a scenario, since tests cite its title; a title that must change is a task. Not Implementor work. `design-diff.json` lists the seam row only, with the finding id as `ref` (one per row; the first finding when two amended the same row).
- **Design in question** (the code and the row disagree and the row may be right): a `decision` ask file, written to the inbox by the orchestrator from the finding (which says what the seam is for, its kind and evidence, and what it blocks), listed in § 3a.
- **`blocking`** (any other finding that breaks the main path, or an untrue sentence that is not a record correction) → routed for an in-change follow-up task in step 7, whose first step is the reproduction; the integrate gate doesn't close until the finding's `resolution` is no longer `pending`. (`blocking` at integrate holds the gate; there is no merge left to pause.)
- **`hardening`** (any other defect) → an entry on the change's hardening list (`templates/hardening.md`), `resolution=hardening-list`. A stale sentence the finding also exposed is still corrected now, as above.

Then the step writes `<change-dir>/cycles/<cycle-id>/design-diff.json`: a JSON array, one element per seam row this cycle added, amended or retired, `{"seam": "seam/<name>", "change": "added | amended | retired", "ref": "<finding-id or ask-id>"}`; an empty array when nothing changed. Step 8's handshake carries it as `design_diff`; step 7 uses it to find the open tasks whose cited rows moved. The amended `design.org` and spec text are committed in the code root (`Integrate <cycle-id>: record corrections`) before the home's close commit.

The record shows why this is scoped to cited seams and the dead-branch scan rather than a full structural audit: across two projects the audit's structural classes fired rarely, the findings that paid were dead code the user's priors licensed deleting, and the catalogue the audit maintained was written more than it was read. Restore the wider audit on a structural class that fires in a real cycle and this check did not catch.

### 2. (Folded into step 1)

The end-of-cycle audit is the conformance check above. Nothing runs here.

### 3. PM digest

Two passes, per `roles/project-manager.md`:

1. **Deterministic pass** — `state.py counts --write` produces `<change-dir>/cycles/<cycle-id>/pm-signals.json`: counts, ratios, history, by-status, critical-path readout, class table, follow-ups by source, open asks, blocked tasks, the findings by kind and how many are on the hardening list, and the fired signals it can compute. Blocked tasks whose `blocker_note` is an ask id re-surface that id rather than raising a new ask; a blocked task with another kind of blocker is a candidate ask the orchestrator writes per `templates/ask.md`.
2. **Agent pass** — turns that into the digest prose at `<change-dir>/cycles/<cycle-id>/pm-digest.md` (per `templates/pm-digest.md`).

When § 9's whole-change review runs this cycle, the agent pass waits for its hand-back, so the digest counts its findings and corrections (§ 9).

If the agent pass fails, the deterministic output remains and is recoverable.

### 3a. Inbox and readback

The integrate sync point (`templates/ask.md` § Sync points). Per `templates/ask.md`:

1. Run `state.py inbox`. Answered files are handled before anything else: a matched option label is applied (`state.py record set asks <id> status=answered`, apply, then `record set asks <id> status=applied applied_via=<ref>`; the file moves to `cycles/<cycle-id>/asks/` and is committed in the home with the change that applies it; when the change is code in a separate code root, the code commit there carries the same subject); free text gets a `* Readback` paragraph in the file (`record set asks <id> decision_readback="<the readback>"`) and stays `open` until the user confirms it there or in chat. A reply that states a general rule rather than a choice is also appended, in the user's words, to the overlay's `priors.md` (`overlay.md` § Priors), with a `confirmation` file written applied.
2. Collect every open file (`inbox` lists them with their paths), including those written during execute (`flows/execute.md` § 9) and plan. Triage by kind. `confirmation` files were written applied into `cycles/<cycle-id>/asks/` when raised; record corrections are journal lines (`record-correction:`), counted by the digest, never files; `process` files sit in the inbox and are counted, never presented; `environment` should already have been asked. Only `decision` is presented. A finding that a line in `priors.md` or a sentence the user wrote in `intent.org` already answers gets no ask file: it is `accepted-with-note` with the prior or sentence cited, or routed as the prior directs (`overlay.md` § Priors; the user's words outrank a `design.org` sentence, so the contradicted design sentence is a record correction). If an ask file already exists (raised mid-execute, or carried in the handshake), it is `declined` with the prior as `decision_readback`.
3. Fill any field a PM candidate question or a finding lacks (`about`, `observed`, `options`) with `state.py record set asks <id> …` when that is a matter of reading code or docs; send a finding back to its role when it would mean redoing the analysis (`templates/ask.md` § What a finding must carry). A finding that exposes both a stale sentence and a product question yields a record correction (journaled) and one `decision` file.
4. The message to the user is the dispatcher's (`SKILL.md` § Two contexts, step 5): after the phase agent's hand-back it prints, from `state.py status`, one line per open decision (the id, the question, its path `inbox/<id>.org`), blocking ones first, and the count of files applied without asking and of process notes. Asks first; the cycle's results after or in a separate message. Nothing blocks on an unanswered decision except under the inline rule (`templates/ask.md` § The inline rule), through the dispatcher after a `stopped` hand-back.
5. Asks the user leaves unanswered keep `status: open`, their default applies at the boundary the file names, and they are carried in the handshake.

A cycle with no `decision`-kind asks has nothing to present and says so in one line; that is a valid outcome, not a gate failure.

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

Write them as a JSON array to `<change-dir>/cycles/<cycle-id>/meta-discoveries.json`; step 8 passes it to the handshake. Per-cycle meta-discoveries surface in this digest and act on the next plan. Recurring meta-discoveries across many cycles distill, via the deferred curation cycle (v2), into durable design priors.

### 5. Goal-drift check

Does the gap between "tasks complete" and "the outcome `intent.org` states is reachable" suggest the intent itself is drifting? The PM's goal-drift query fires when:

- Critical-path completion ratio is stagnant or declining for K cycles.
- Non-critical-path completions continue at normal pace.
- Drainage stays positive (so it's not throughput inversion).

If the query fires:
- The PM digest carries a goal-drift recommendation: revise / split / abandon / continue.
- The intent's status line becomes `divergent <date> <ask-id>`, with the signal and its evidence under `* Status` (`templates/intent.md`), committed in the code root.
- An ask file of kind `decision` is written to the inbox (`templates/ask.md`) with the four fixed options revise / split / abandon / continue, and listed in § 3a. The tool recognises the goal-drift ask by exactly those four option labels in the file.

The user's decision lands in the file's `* Answer`; `state.py handshake` reads it into `user_resolved_goal_drift` as `{ask, decision, rationale}`, with `decision` the exact-matched label. Plan refuses to start the next cycle until the user has dispositioned (or explicitly chosen `continue`).

### 6. Externalisation review

`.tasks/` pressure check — has the cross-cutting backlog grown to the point where a cluster should be promoted into the active change?

The PM checks: of the externalised tasks, is there a cluster whose `discovered_class` distribution and modules-touched are coherent enough to constitute a sub-change? If yes:

- Write an ask file (kind `decision`; default if unanswered: leave externalised): "promote cluster `vocabulary-mapping` (5 tasks, 3 cycles old) into the active change?"
- The user dispositions: promote, leave externalised, or open a new change.

### 7. Open-task refinement

The cycle's task list is both an input (what plan produced) and an output (what the next cycle inherits). Without this step, plan-time prose ages: it cites seam rows in their pre-cycle shape, ignores meta-discoveries that change implementor defaults, and prescribes work that inline fixes already shipped. The next implementor reads stale instructions, the brief overlay only patches what's cited, and the cycle has discovered without integrating *into the work that remains*.

This is the **single refinement point** for the open task list, and integrate's one point for writing task files (every point that writes one is listed in `externalisation.md` § Who writes a task file). Both shapes of curation happen here:

1. **Refining existing open tasks** — absorbing the design diff, meta-discoveries, user-resolved asks, and inline fixes into the bodies of tasks that remain in `<change>/tasks/open/`.
2. **Creating new tasks** from this cycle's findings, asks, and discoveries — the conversion of step 1's conformance findings (and plan's premise-check findings), step 4's meta-discoveries, and step 5/6's user routing into actual files in `<change>/tasks/open/` (or `.tasks/` per `externalisation.md`).

Steps 1–6 *identify* what needs to land; step 7 *lands it*. Earlier steps may name file paths in their findings (`producing follow-up task X` etc.) but the file write is here, so a single sweep over the open task list keeps creation and refinement coherent.

Refinement runs **after** externalisation review (so externalised tasks have already left `<change>/tasks/open/`) and **before** the handshake (so the handshake can record what was created and refined).

#### Create new tasks from cycle outputs

Walk the cycle's outputs for new-task triggers:

- **Architect findings** (from step 1, and from plan's premise check), by their routing in step 1: `interface-drift` → a record correction (journaled) or a `decision` ask (no task); `blocking` → an in-change follow-up task with `discovered_from: <finding-id>`, `discovered_by: architect`, `discovered_class: <finding.class>`, `resolution=followup-task-<name>`; `hardening` → no task now: the hardening-list entry is the record until the MVP decision. A `design.org` sentence is an agent's plan, not the user asking, so a design sentence a prior contradicts is a record correction. The same routing applies to Reviewer findings and Implementor `## Discoveries` that reach this step. Implementor `## Observations` were triaged in execute § 5 and are not re-read here.
- **Scope** proposed by a finding, a thought or a meta-discovery (an option, a generalisation, a refactor for a future caller) becomes a task only under the YAGNI check (`templates/architect-finding.md` § The YAGNI check); otherwise it stays the line it is.
- **Open asks** (the inbox files with `status: open`, indexed in `asks_for_user`, from steps 1, 5, 6, plan and execute) create no task. Each task named in an ask's `blocks` carries `status: blocked` and `blocker_note: <ask-id>` (`templates/ask.md` § Blocking without a second artifact); the note is cleared when the ask is applied.
- **User-resolved asks with deferred implementation** (`asks_for_user_resolved[i]` where `applied_via` indicates deferral, e.g. `deferred-to-cycle-N`). If the deferral target is *not* an existing open task, create one carrying the user's decision in its body and `discovered_from: <ask-id>`.
- **Meta-discoveries with concrete forward-looking work** (`meta_discoveries[i].implication_for_next_plan` names a specific task or rewire). If the implication is concrete enough to be its own task and is not absorbed by an existing open task's refinement, create the task with `discovered_from: meta-discovery/<label>`, `discovered_class: <meta.kind>`.

For each created task, apply the externalisation rule (`externalisation.md`): in-change if it contributes to the active intent's outcome; `.tasks/` if cross-cutting. Each is registered with `state.py task add <name> --file <path> --class <class> discovered_from=<id> discovered_by=<role> discovered_class=<class>`; an externalised one gets `task set <name> externalised` straight after.

Each created task is also added to the `task_refinements` list (with `modes: ["created"]`) so the create/refine accounting is unified. When step 7 has created tasks, re-run `state.py counts --write` so `pm-signals.json` and the digest's throughput table include them; the digest's numbers must match the state file at close.

#### Compute each open task's impact set

For each task in `<change>/tasks/open/<task-name>.md`, intersect against the cycle's outputs:

- **(a) Design-diff hits.** `task.cites_seams ∩ design_diff[].seam`. Each hit names a cited seam row that was added, amended or retired this cycle.
- **(b) Meta-discovery hits.** Any `meta_discoveries[i]` whose `scope` matches one of the task's cited seams, OR whose `evidence` array names this task or any task that cited the same seams.
- **(c) User-resolved-ask hits.** Any `asks_for_user_resolved[i]` whose `applied_via` names this task, or names a task or inline fix whose files overlap the files this task's Implementation steps name, or whose seams this task cites.
- **(d) Inline-fix hits.** Any finding with `resolution: inline-fixed` (or `inline-fix` journal entry) whose locations overlap the files the task's Implementation steps name, or implicate code paths the task prescribes.

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
- The task may now be **wholly obsolete** — flag for user disposition; do not auto-close. Write a `decision` ask file (options: close, refine, rescope, do nothing; `blocks: [<task>]`; `default_if_unanswered: leave open`) in the inbox and append a stanza that names the claim and the ask id. If the task is in this cycle's state, set it `blocked` with `blocker_note: <ask-id>`; an open task that was never batched has no state entry, and the next plan's § 2 leaves it out of the batch while the ask is open.

Stanza form: see `templates/task-update-stanza.md`. Tasks may accumulate stanzas across cycles, newest-last.

Both modes preserve the task's frontmatter and any existing `## Observations` / `## Discoveries` (those are execute-phase artifacts and must not be touched).

#### Record the refinement for the handshake

For every refined task, append to `<change-dir>/cycles/<cycle-id>/task-refinements.json` (a JSON array; step 8 passes it to the handshake):

```json
{
  "task": ".orchestrator/changes/<change>/tasks/open/<name>.md",
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

`obsolescence_flagged: true` surfaces in the next plan as a candidate-close. Plan does not auto-close; a `decision` ask file (options: close, refine, rescope, do nothing; `:BLOCKS:` the task) carries the disposition.

A task that was touched but not modified — i.e. impact set was non-empty but inspection determined no actual prose change is warranted — still gets a refinement entry with `modes: []` and the channel(s) considered. This makes "we looked, we decided nothing was stale" auditable rather than indistinguishable from "we never looked."

### 8. Write the integrate→plan handshake artifact

The cycle's loop-closing artifact. Path: `<change-dir>/handshake-<cycle-id>.json`. Written by

```bash
state.py handshake --meta-discoveries @$ORCH_CHANGE_DIR/cycles/<cycle-id>/meta-discoveries.json \
                   --task-refinements @$ORCH_CHANGE_DIR/cycles/<cycle-id>/task-refinements.json
```

which reads `design_diff` from `cycles/<cycle-id>/design-diff.json` (step 1; `--design-diff @path` overrides, and an absent file means `[]`; an `@path` resolves against the cwd first, then the home), fills `pm_digest_path`, `user_resolved_goal_drift`, the two ask lists and the journal from state, and validates the result. Its shape, with an example, is in `state.md` § Integrate→plan handshake artifact.

**All seven required fields are mandatory** (`design_diff`, `pm_digest_path`, `meta_discoveries`, `user_resolved_goal_drift`, `asks_for_user_open`, `asks_for_user_resolved`, `task_refinements`; the tool defines the list once). An empty list is allowed; a missing field is not. The next plan's first operation is to read this file; `state.py init` refuses to start if any field is missing.

This is the structural fix for the brainstorm's "learns and forgets" failure mode. Without the handshake, plan degrades into "pull from the top of the backlog" and the orchestrator becomes a queue runner.

### 9. Whole-change review, at each integrate whose cycle merged code

Per-task reviews see one diff and its cited rows. At every integrate whose cycle merged code (a task merge, an inline fix, an inline-path commit), the orchestrator spawns one more Reviewer over the whole change, whether or not an open task remains; a cycle that merged only record text skips it. It runs after § 1 and before § 3's agent pass and § 7, so the digest counts its findings and § 7 creates the tasks its `blocking` findings need. Whether a task remains decides only the MVP decision below, from § 1's routing, the open and blocked tasks, and the candidate tasks execute left for § 7. The record: a cycle that merged seven tasks deferred this review because one doc task remained; `/code-review` then found two behaviour changes of one Decision (reload no longer reaching a package's files, one failing file stopping its package) that no per-task review had in scope, both inside the "interactions between tasks" this review looks for (package-boundaries `cycle-1792083023`). The record: run alongside the PM pass, the review's one finding and the spec correction it led to were missing from the digest (bookmarks `cycle-1791996711`).

- **Take in the workspace integration branch** first, when the change has one (`change.json`'s `from`) and it has moved: `git -C "$CODE_ROOT" merge --no-edit <that branch>`, the test command once after it (a regression here is one of this change's merges against another's, an `environment` ask with both branches named), committed `Take in <branch>: <change>`. The review then sees the change as it will merge.
- **Diff**: `git -C "$CODE_ROOT" diff <base>..HEAD`, where `<base>` is `intent.org`'s `#+BASE:`. This covers every path a change took: its spec edits and design, task merges, inline-path commits, inline fixes, ask answers, record corrections.
- **Input** (author-blind as in execute § 7, with the whole design in place of the cited rows): the diff, `intent.org`, every Seams row of `design.org`, `coverage.py --since <base> --output <the cycle's last after-file>`, the overlay's `roles/reviewer.md` and `priors.md`. No report, no Observations, no review files.
- **Looks for** what no per-task review could: interactions between tasks (a setting, a wrapper and a doc that disagree), a scenario whose test passes while the surface the user would use does not, a change that bypassed review, a contract weakened in one task and relied on in another.
- **Output**: `cycles/<cycle-id>/reviews/whole-change.md` in the Reviewer's format, one hand-back line. Its findings are routed as in execute § 8: `blocking` ones become in-change tasks for one more cycle (§ 7, `discovered_by: reviewer`), and the change goes no further while one is open; `hardening` ones join the hardening list; a `spec-signal` is an ask.
- **The MVP decision**: when the review leaves no blocking finding, no open task remains, and the intent's status line has not yet reached `mvp`, the main path works end to end and the change stops for the user (`templates/hardening.md` § The MVP decision): the orchestrator writes the status line `mvp <date> <HEAD> <ask-id>` and the one `decision` ask (harden / rethink / stop, with the hardening list and the record corrections to the specs in it, `:BLOCKS: plan`), commits both, finishes the phase (gate, `close`, the close commit) and hands back `integrate <cycle-id>: stopped, ask <id> blocks plan; <path>`. Resumed with the answer, it applies it as that template says; with no answer, nothing more runs on the change (`flows/plan.md` § 1).
- **Done**: after the user chose `stop`, or after `harden` once this review again leaves no blocking finding and no open task remains, the orchestrator writes `#+STATUS: done <date> <HEAD>` in `intent.org` with the closing summary in its `* Status` (`templates/hardening.md` § The closing summary) and commits it in the code root with a subject naming the change (`templates/intent.md`). The change's documents stay where they are. A change with a workspace integration branch then merges into it: in that branch's worktree (`state.py worktree <branch>` gives its path; `git -C "$CODE_ROOT" worktree add <path> <branch>` when it does not exist), `git merge --no-ff <change> -m "Merge <change>"`; the next change of the hand-off takes it in at its design round. The branch's merge to main is the user's, recorded in the research brief that made it, never the orchestrator's.

The record: after the orchestrator reported two changes done, the user's smoke check, `/code-review` and UAT found nine defects and more; the cross-cutting ones (settings × wrapper × `SKILL.md`) had never been in any one task's diff.

## Inputs (from execute)

- All Implementor reports (held by orchestrator since execute, not seen by reviewers).
- All Reviewer findings.
- Plan's premise-check findings, if any.
- The set of merged diffs.
- The seam rows and scenarios this cycle's tasks cite (`cites_seams` in state, `cites_scenarios` in the task files; the rows in `design.org`, the scenarios in `docs/specs/`).
- `phase_gates.execute.passed: true` (mandatory; `state.py phase set integrate` enforces it).

## Exit gate

| Check | Condition |
|---|---|
| `blocking_findings_resolved` | Every `architect_findings[i]` with `decoration: blocking` has `resolution != pending` (severity is the reporter's high / medium / low; the decoration is the routing, `templates/architect-finding.md`) |
| `pm_digest_produced` | `pm-digest.md` exists with a non-empty `signals` section (the asks table may be empty) |
| `user_asks_routed` | Every open ask is in `handshake.asks_for_user_open` (so plan picks them up next cycle); no index/file mismatch and no missing file; no `open` ask has an answer the orchestrator has not acted on (an exact label is applied; anything else has its `* Readback` written, and may stay open awaiting the user's yes); every non-terminal file is in `inbox/` and every terminal one under `cycles/<cycle-id>/asks/`. Git tracking is reported per file by `state.py inbox`, not gated |
| `open_tasks_refined_against_handshake` | Every still-open task in `<change>/tasks/open/` has been considered by step 7. A task either has an entry in `handshake.task_refinements` (with `modes` populated, possibly empty) or has been excluded explicitly because it had no impact-set hits. New tasks created in step 7 are present on disk and have a `task_refinements` entry with `modes: ["created"]`. Findings routed `blocking` in step 1 each have a corresponding created task; `hardening-list` findings have none until the user chooses to harden. **Asserted**: `state.py gate set integrate open_tasks_refined_against_handshake=true` |
| `handshake_artifact_written` | `handshake-<cycle-id>.json` exists with all seven required fields populated |

Four of the five are computed by `state.py gate check integrate`; the fifth is asserted. `state.py gate pass integrate` sets the gate when all pass; then `state.py close`. The next plan refuses to start otherwise — that's the loop-closure contract.

## Cycle archive

`state.py close` archives the cycle's state and handshake beside the files the cycle wrote there; the listing is `state.md` § Cycle archive.

The active state file stays in place, marked `closed_at`; the next cycle's `state.py init` replaces it and carries the counts `history` from the archives.

This archive is what the deferred curation cycle (v2) reads. It is also the audit trail the user can walk through to understand any past decision.

## Commit

After `close`, in the home: `git -C "$ORCH_HOME" add .orchestrator && git -C "$ORCH_HOME" commit -m "Orchestrator: <cycle-id> integrate closed"` (add `.tasks` too when integrate touched it). Ask files were committed when created and when applied (`templates/ask.md` § Commit rule); this commit carries the rest: the archived state and handshake, the digest and signals, the findings, `design-diff.json`, the refined and created task files. The record integrate changed (`design.org`, spec text, the intent's status line) was committed in the code root (§ 1, § 5, § 9). `close` reports `inbox_left`; open and deferred files stay in the inbox for the next cycle's `init`.

## Loop closure: integrate → plan

The keystone transition is integrate → plan, **not** execute → review. Each plan is *required* to consume the prior integrate's outputs. Without that as a hard input contract, plan degrades into queue-running and the orchestrator becomes a glorified task runner.

This is the loop that gives the system *compounding leverage*: each cycle's discoveries make the next cycle's batch better. It is the operationalisation, at cycle altitude, of the two-way information flow.

## What integrate does **not** do

- Integrate does not modify code. (Conformance amends `design.org` and spec text, which are record, not code. Inline fixes are made in execute, in the `reviewed` state, `flows/execute.md` § 8.)
- Integrate does not run new test suites — the cycle's tests already ran in execute; the conformance check may re-run them read-only to confirm a promoted test passes.
- Integrate does not re-implement tasks — re-implementation is execute's job; integrate only routes findings.
