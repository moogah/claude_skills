# Integrate phase

The backward channel firing, plus the loop-closing operation. Integrate consumes the cycle's discoveries and produces the inputs the next plan requires.

A cycle that leaves the design saying something the merged code no longer does has discovered without integrating — the failure the integrate phase exists to prevent. Integrate's first operation checks the code against the seams the batch cited and amends the rows the code has rightly outgrown.

## Operations

### 1. Design conformance (the load-bearing operation)

The Architect, spawned at its role default (`model: opus`, `effort: high`) with the description `architect-conformance: <cycle-id>`, runs against the seams this cycle's tasks cited (`roles/architect.md` § Conformance): for each cited row in `design.org` § Seams, do the owning symbols exist and remain the single producer or mapping; do the scenarios the cycle's tasks cite (`cites_scenarios`) pass: `coverage.py --scenario … --output <the cycle's last after-file>` shows each `pass`, not `pending`, `failed`, `not run` or `uncited`. Plus a dead-branch scan across every diff merged this cycle: is any replaced implementation still on a live path; is any touched function now unreachable. The check reads each merged diff in full (its divergence from its merge base), the modules it touched and their immediate neighbours, and the project priors.

Output: zero or more finding files written to `<change-dir>/cycles/<cycle-id>/findings/<finding-id>.md` by the Architect (`templates/architect-finding.md`, `trigger: conformance`, ids `arch-<cycle-id>-<seq>`), and a `thought:` journal line per speculative need; a check with zero findings is one `state.py note note "conformance: zero findings" --by architect` (a journal line is at most 280 characters, or the tool refuses `too_long`; a longer text goes in a file that `--path` names). The Architect hands back one line with the counts (`SKILL.md` § Hand-back lines); the orchestrator reads the findings files, not the hand-back, with the check's `thought:` journal lines (rerouted as execute § 8 says), and routes each by `templates/architect-finding.md` § Routing, reading `intent.org`'s Goals for the main path (the task-file write, when one is needed, happens in step 7's curation sweep so creation and refinement are unified). The routing lands in the event log as `state.py route <id> --decoration <blocking|hardening> --resolution <value>`, one of the values `templates/architect-finding.md` § Resolution states lists:

- **Which side is right is decided by a test, not by the code.** An edit that makes the design say what the code does is a record correction only when a test pins the code's behaviour on the point the edit changes; the finding's resolution names that test. An edit that removes or weakens something the design promised and the code does not do ("each call has its own timeout" where no call has one) is never a record correction, whatever the spec scenarios require: the code not keeping a promise is behaviour present in merged code, so it is a finding against the code (`roles/architect.md` § What the Architect reports). It is a `decision` ask only when the user's own words (a spec scenario, the intent, a prior) conflict with the promise; a product question the Architect sees is not enough, and the finding's recommended resolution carries it. With no pinning test, a disagreement is **Design in question** below. The record: one replay arm's conformance cut "with its own timeout" from a Decision because the spec did not require it; in another arm of the same cycle the Reviewer filed the same untimed call as blocking (a stop on a hung target never returns), and the rework bounded it. Earlier, conformance amended the design twice to match untested code, once each way (D14). A re-run of that arm's conformance under this rule turned the cut into a finding against the code; with "a `decision` ask when the promise is in doubt" as the escape, it also sent a defect it recommended fixing (a state call that fails on raw bytes) to the user as a question, hence the user's-words test.
- **Code right, row stale** (`interface-drift` where the merged code is what the change wanted and a test pins it): a record correction (`templates/design.md` § Seams). The Architect amends the seam row in place, and with it the Decision and the spec scenario steps that named the same thing, keeping each at the contract: when the drift is a guard, error string, encoding or helper the Implementor added, the row is cut back, never extended with it, and a Decision never gains one. It sets `resolution: design-amended`; the orchestrator writes one journal line, `state.py note note "record-correction: <what>" --ref <seam-id>`, and commits the amended text with a subject naming the row. Every commit that corrects record text says `record correction` in its subject, and one that corrects a scenario in `docs/specs/` names the pinning test in its body (`Pinned by: <test>`, one line per scenario), so the MVP decision and the closing summary can list each spec correction with its diff and test (`templates/hardening.md`). No ask file, no question: the record shows 47 of 47 such files were questions nobody answered, and 39 of them would not have existed under this rule. The correction reaches whatever the row cites: a scenario in `docs/specs/` that states the stale thing is corrected in the same commit, in place. A record correction does not rename a scenario, since tests cite its title; a title that must change is a task. Not Implementor work. `design-diff.json` lists the seam row only, with the finding id as `ref` (one per row; the first finding when two amended the same row).
- **Design in question** (the code and the row disagree and the row may be right): a `decision` ask file, written to the inbox by the orchestrator from the finding (which says what the seam is for, its kind and evidence, and what it blocks), listed in § 3a.
- **`blocking`** (any other finding that breaks the main path, or an untrue sentence that is not a record correction) → a candidate task in step 7, which the next plan writes with the reproduction as its first step; the integrate gate doesn't pass until the finding's `resolution` is no longer `pending` (`candidate-task-<name>` once step 7 routes it). (`blocking` at integrate holds the gate; there is no merge left to pause.)
- **`hardening`** (any other defect) → an entry on the change's hardening list (`templates/hardening.md`), `resolution=hardening-list`. A stale sentence the finding also exposed is still corrected now, as above.

Then the step writes `<change-dir>/cycles/<cycle-id>/design-diff.json`: a JSON array, one element per seam row this cycle added, amended or retired, `{"seam": "seam/<name>", "change": "added | amended | retired", "ref": "<finding-id or ask-id>"}`; an empty array when nothing changed. Step 8's handshake carries it as `design_diff`; step 7 uses it to find the open tasks whose cited rows moved. The amended `design.org` and spec text are committed in the code root (`Integrate <cycle-id>: record corrections`) before the home's close commit.

The record shows why this is scoped to cited seams and the dead-branch scan rather than a full structural audit: across two projects the audit's structural classes fired rarely, the findings that paid were dead code the user's priors licensed deleting, and the catalogue the audit maintained was written more than it was read. Restore the wider audit on a structural class that fires in a real cycle and this check did not catch.

### 2. (Folded into step 1)

The end-of-cycle audit is the conformance check above. Nothing runs here.

### 3. PM pass

The PM, spawned at its role default (`model: sonnet`, `effort: medium`) with the description `pm: <cycle-id>`, reads the cycle against the intent's Goals (`roles/project-manager.md`) and writes two things the handshake carries: the meta-discoveries (§ 4, `cycles/<cycle-id>/meta-discoveries.json`) and the goal-drift recommendation (§ 5). It also proposes candidate questions in its hand-back, from which the orchestrator writes the ask files (`templates/ask.md`); a task blocked on an ask that is already open re-surfaces that id (`state.py log --kind blocker-note`) rather than raising a new ask. It computes no counts; spend and quality by role are `report.py`'s, on demand (`report.md`).

When § 9's whole-change review runs this cycle, the PM waits for its hand-back, so its findings and corrections are in what the PM reads (§ 9).

### 3a. Inbox and readback

The integrate sync point (`templates/ask.md` § Sync points). Per `templates/ask.md`:

1. Run `asks.py inbox`. Answered files are handled before anything else: a matched option label is applied (`asks.py set <id> status=answered`, apply, then `asks.py set <id> status=applied applied_via=<ref>`; the file moves to `cycles/<cycle-id>/asks/` and is committed in the home with the change that applies it; when the change is code in a separate code root, the code commit there carries the same subject); free text gets a `* Readback` paragraph in the file (`asks.py set <id> decision_readback="<the readback>"`) and stays `open` until the user confirms it there or in chat. A reply that states a general rule rather than a choice is also appended, in the user's words, to the overlay's `priors.md` (`overlay.md` § Priors), with a `confirmation` file written applied.
2. Collect every open file (`asks.py inbox` lists them with their paths), including those written during execute (`flows/execute.md` § 9) and plan. Triage by kind. `confirmation` files were written applied into `cycles/<cycle-id>/asks/` when raised; record corrections are journal lines (`record-correction:`), never files; `process` files sit in the inbox, never presented; `environment` should already have been asked. Only `decision` is presented. A finding that a line in `priors.md` or a sentence the user wrote in `intent.org` already answers gets no ask file: it is `accepted-with-note` with the prior or sentence cited, or routed as the prior directs (`overlay.md` § Priors; the user's words outrank a `design.org` sentence, so the contradicted design sentence is a record correction). If an ask file already exists (raised mid-execute, or carried in the handshake), it is `declined` with the prior as `decision_readback`.
3. Fill any field a PM candidate question or a finding lacks (`about`, `observed`, `options`) with `asks.py set <id> …` when that is a matter of reading code or docs; send a finding back to its role when it would mean redoing the analysis (`templates/ask.md` § What a finding must carry). A finding that exposes both a stale sentence and a product question yields a record correction (journaled) and one `decision` file.
4. The message to the user is the dispatcher's (`SKILL.md` § Two contexts, step 5): after the phase agent's hand-back it prints, from `asks.py inbox`, one line per open decision (the id, the question, its path `inbox/<id>.org`), blocking ones first, and the count of files applied without asking and of process notes. Asks first; the cycle's results after or in a separate message. Nothing blocks on an unanswered decision except under the inline rule (`templates/ask.md` § The inline rule), through the dispatcher after a `stopped` hand-back.
5. Asks the user leaves unanswered keep `status: open`, their default applies at the boundary the file names, and they are carried in the handshake.

A cycle with no `decision`-kind asks has nothing to present and says so in one line; that is a valid outcome, not a gate failure.

### 4. Meta-discovery surfacing

The PM distils the cycle's patterns (the Architect's finding classes, the Reviewer's recurring findings, the follow-ups' provenance, the journal's discoveries) into what the next plan should weigh when drafting its batch.

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

The PM writes them as a JSON array to `<change-dir>/cycles/<cycle-id>/meta-discoveries.json`; step 8 copies it into the handshake. Per-cycle meta-discoveries act on the next plan. Recurring meta-discoveries across many cycles distill, via the deferred curation cycle (v2), into durable design priors.

### 5. Goal-drift check

Does the gap between "tasks complete" and "the outcome `intent.org` states is reachable" suggest the intent itself is drifting? The PM reads the intent's Goals against what the cycles did: the main-path tasks (`on_critical_path`) still open and those merged, the work that merged off the main path, the follow-ups and the findings. It writes a goal-drift recommendation, `none`, `revise`, `split`, `abandon` or `continue`, with a one-line reason; the handshake carries it as `goal_drift_recommendation` (`templates/handshake.md`). A recommendation is only ever a recommendation.

When it is anything but `none` and the orchestrator agrees the drift is real:
- The intent's status line becomes `divergent <date> <ask-id>`, with the signal and its evidence under `* Status` (`templates/intent.md`), committed in the code root.
- An ask file of kind `decision` is written to the inbox (`templates/ask.md`) with the four fixed options revise / split / abandon / continue, and listed in § 3a. The tool recognises the goal-drift ask by exactly those four option labels in the file.

The user's decision lands in the file's `* Answer`; the handshake carries it in `user_resolved_goal_drift` as `{ask, decision, rationale}`, with `decision` the exact-matched label. The next plan's gate (`goal_drift_signed_off`) does not pass until the user has dispositioned (or explicitly chosen `continue`).

### 6. Externalisation review

`.tasks/` pressure check — has the cross-cutting backlog grown to the point where a cluster should be promoted into the active change?

The PM reads: of the externalised tasks, is there a cluster whose `discovered_class` distribution and modules-touched are coherent enough to constitute a sub-change? If yes:

- Write an ask file (kind `decision`; default if unanswered: leave externalised): "promote cluster `vocabulary-mapping` (5 tasks, 3 cycles old) into the active change?"
- The user dispositions: promote, leave externalised, or open a new change.

### 7. Route the cycle's outputs to the next plan

The cycle's task list is both an input (what plan produced) and an output (what the next cycle inherits). Without this step, plan-time prose ages: it cites seam rows in their pre-cycle shape, ignores meta-discoveries that change implementor defaults, and prescribes work that inline fixes already shipped. The next implementor reads stale instructions, the brief overlay only patches what's cited, and the cycle has discovered without integrating *into the work that remains*.

**Integrate writes no task text.** It collects what the cycle found and asked, routes each item, and records where it goes; the next plan writes every task body, new or refined (`flows/plan.md` § 2), and the Architect checks what plan wrote against HEAD (§ 3). The phase agent runs at a routing setting (`SKILL.md` § Agent settings); task text is where most wrong premises in the record began, and it is written at plan's. The record: the first integrate on the routing setting wrote seven critical-path task bodies, one of them on a premise false at HEAD (an advice it said sat behind a load-time guard was installed unconditionally), and said in its hand-back that the next premise check should verify them (bookmarks `cycle-1792862019`).

Two outputs, both read by the handshake (§ 8):

1. **Candidate tasks**, `<change-dir>/cycles/<cycle-id>/candidate-tasks.json`, for the work that needs a task the cycle has not got. Execute § 5 and § 8 may already have added entries; this step adds the rest.
2. **Each open task's impact set**, `<change-dir>/cycles/<cycle-id>/task-refinements.json`, naming what in this cycle touches a task that remains in `<change>/tasks/open/`.

Steps 1–6 *identify* what needs to land; step 7 records where it lands, in one sweep over the cycle's outputs and the open task list. It runs **after** externalisation review (so externalised tasks have already left `<change>/tasks/open/`) and **before** the handshake.

#### Route new task candidates

Walk the cycle's outputs for new-task triggers:

- **Architect findings** (from step 1, and from plan's premise check), by their routing in step 1: `interface-drift` → a record correction (journaled) or a `decision` ask (no task); `blocking` → a candidate, `critical: true`, `discovered_by: architect`, `discovered_class: <finding.class>`, and `state.py route <id> --resolution candidate-task-<name>`; `hardening` → no task now: the hardening-list entry is the record until the MVP decision. A `design.org` sentence is an agent's plan, not the user asking, so a design sentence a prior contradicts is a record correction. The same routing applies to Reviewer findings (the whole-change review's included, § 9) and Implementor `## Discoveries` that reach this step. Implementor `## Observations` were triaged in execute § 5 and are not re-read here.
- **Scope** proposed by a finding, a thought or a meta-discovery (an option, a generalisation, a refactor for a future caller) becomes a candidate only under the YAGNI check (`templates/architect-finding.md` § The YAGNI check); otherwise it stays the line it is.
- **Open asks** (the inbox files with `status: open`, from steps 1, 5, 6, plan and execute) create no candidate. Each task named in an ask's `blocks` carries `status: blocked` and a `state.py blocker <task> <ask-id>` note (`templates/ask.md` § Blocking without a second artifact); the task returns to `ready` when the ask is applied.
- **User-resolved asks with deferred implementation** (the files `state.py asks --all` lists answered or applied this cycle, whose `applied_via` indicates deferral, e.g. `deferred-to-cycle-N`). The work the decision asks for is routed by the main-path question first (`templates/architect-finding.md` § Routing), or by the option the user chose when it said "before the MVP" or "after the MVP" (`templates/ask.md` § The file, `* Options`): on the main path, and the deferral target is *not* an existing open task, a candidate with `source: <ask-id>`, `discovered_by: user`, `critical: true`, and the user's decision (the matched label and the readback) as its `quote`; when the target is an open task, it is that task's impact (below); off the main path, the decision becomes a `decision (hardening)` entry on the hardening list (`templates/hardening.md`) and no task, until the user chooses to harden. The record: in one change 13 of the 14 tasks after the design's were the user's answers, each placed in the change and holding the MVP decision back; the user, of one: "shouldn't have been considered critical path".
- **Meta-discoveries with concrete forward-looking work** (`meta_discoveries[i].implication_for_next_plan` names a specific task or rewire). If the implication is concrete enough to be its own task and is not an existing open task's impact, a candidate with `source: meta-discovery/<label>`, `discovered_by: orchestrator`, `discovered_class: <meta.kind>`, `critical: false` (`flows/plan.md` § 6).

A candidate is a JSON object (`templates/handshake.md`; `state.py gate integrate` fails `handshake_written` on one that does not fit):

```json
{ "name": "<proposed-task-name>", "source": "<finding-id | ask-id | review file#F<n> | <task>/discovery-<n> | meta-discovery/<label>>",
  "discovered_by": "architect | reviewer | implementor | orchestrator | user", "discovered_class": "<class>",
  "critical": true, "quote": "<the finding, discovery or decision, verbatim>",
  "evidence": "<path to the file that holds the rest>", "placement": "in-change | .tasks" }
```

`quote` is the source's own words, not a paraphrase and not steps: what it says the code does, the decision the user made. `placement` is the externalisation rule (`externalisation.md`): in-change if it contributes to the active intent's outcome; `.tasks` if cross-cutting. A finding routed to a candidate has `resolution=candidate-task-<name>`; `handshake_written` fails when that name has no entry. No task file is written here; plan writes each one (`state.py check new`), and its `discovered_*` fields are the candidate's.

#### Compute each open task's impact set

For each task in `<change>/tasks/open/<task-name>.md`, intersect against the cycle's outputs:

- **(a) Design-diff hits.** `task.cites_seams ∩ design_diff[].seam`. Each hit names a cited seam row that was added, amended or retired this cycle.
- **(b) Meta-discovery hits.** Any `meta_discoveries[i]` whose `scope` matches one of the task's cited seams, OR whose `evidence` array names this task or any task that cited the same seams.
- **(c) User-resolved-ask hits.** Any ask answered or applied this cycle whose `applied_via` names this task, or names a task or inline fix whose files overlap the files this task's Implementation steps name, or whose seams this task cites.
- **(d) Inline-fix hits.** Any finding routed `inline-fixed` (`state.py log --kind finding-routed`), or `inline-fix` journal line, whose locations overlap the files the task's Implementation steps name, or implicate code paths the task prescribes.
- **(e) Premise-wrong hits.** A `premise-wrong: <task>` journal line from execute (`flows/execute.md` § 9): the claim, `channel: premise-wrong`, `ref` the report path. The task stays `blocked` until plan rewrites it.

A task with an empty impact set across all five channels gets an entry with an empty `impact`, so "we looked, nothing touches it" is auditable rather than indistinguishable from "we never looked".

The task may now be **wholly obsolete**: flag it for user disposition; do not auto-close. Write a `decision` ask file (options: close, refine, rescope, do nothing; `blocks: [<task>]`; `default_if_unanswered: leave open`) in the inbox and set `obsolescence_flagged: true`. Set its file's `status: blocked` and record `state.py blocker <task> <ask-id>`; the next plan's § 2 leaves it out of the batch while the ask is open.

#### Record the impact sets for the handshake

For every open task, one element of `<change-dir>/cycles/<cycle-id>/task-refinements.json` (a JSON array; step 8 passes it to the handshake):

```json
{
  "task": ".orchestrator/changes/<change>/tasks/open/<name>.md",
  "impact": [
    { "channel": "design-diff", "ref": "seam/<name>" },
    { "channel": "meta-discovery", "ref": "<kind>/<label>" },
    { "channel": "user-resolved-ask", "ref": "<ask-id>" },
    { "channel": "inline-fix", "ref": "<finding-id>" },
    { "channel": "premise-wrong", "ref": "<report path>" }
  ],
  "obsolescence_flagged": false
}
```

The task file is not touched here. Plan § 2 applies each impact to the body, in place or as a stanza (`templates/task-update-stanza.md`), and a flagged task gets its obsolescence stanza there.

### 8. Write the integrate→plan handshake artifact

The cycle's loop-closing artifact, `<change-dir>/handshake-<cycle-id>.json`, written by the orchestrator in the form `templates/handshake.md` gives, each field from its source there:

- `design_diff` from `cycles/<cycle-id>/design-diff.json` (step 1), `meta_discoveries` from `meta-discoveries.json` (step 4), `task_refinements` from `task-refinements.json` and `candidate_tasks` from `candidate-tasks.json` (step 7, execute § 5 and § 8), each copied as it is; an absent file is `[]`;
- `asks_for_user_open` and `asks_for_user_resolved` from `state.py asks --all` (the open files; those answered, applied or deferred this cycle);
- `user_resolved_goal_drift` from the goal-drift ask files answered with one of the four labels (step 5), and `goal_drift_recommendation` from the PM (step 5);
- `journal` from `state.py log --cycle <cycle-id> --kind note`.

**Every field is required.** An empty list is allowed; a missing field is not. It carries no counts, ratios or signals. `state.py gate integrate` checks it (`handshake_written`: every field, every candidate and design-diff element in shape, a candidate for every finding routed `candidate-task-<name>`); the next plan's first operation is to read it, and `state.py cycle start` refuses a prior handshake missing a field.

This is the structural fix for the brainstorm's "learns and forgets" failure mode. Without the handshake, plan degrades into "pull from the top of the backlog" and the orchestrator becomes a queue runner.

### 9. Whole-change review, at each integrate whose cycle merged code

Per-task reviews see one diff and its cited rows. At every integrate whose cycle merged code (a task merge, an inline fix, an inline-path commit), the orchestrator spawns one more Reviewer over the whole change, at its role default (`model: opus`, `effort: xhigh`) with the description `reviewer-whole-change: <cycle-id>`, whether or not an open task remains; a cycle that merged only record text skips it. It runs after § 1 and before § 3's PM pass and § 7, so the PM reads its findings and § 7 routes its `blocking` findings to candidates. Whether a task on the main path remains (`on_critical_path`, `flows/plan.md` § 6) decides only the MVP decision below, from § 1's routing, the open and blocked tasks, and the candidates in `candidate-tasks.json` with `critical: true`, which count as open main-path tasks. The record: a cycle that merged seven tasks deferred this review because one doc task remained; `/code-review` then found two behaviour changes of one Decision (reload no longer reaching a package's files, one failing file stopping its package) that no per-task review had in scope, both inside the "interactions between tasks" this review looks for (package-boundaries `cycle-1792083023`). The record: run alongside the PM pass, the review's one finding and the spec correction it led to were missing from what the PM wrote (bookmarks `cycle-1791996711`).

- **Take in the workspace integration branch** first, when the change has one (`change.json`'s `from`) and it has moved: `git -C "$CODE_ROOT" merge --no-edit <that branch>`, the test command once after it (a regression here is one of this change's merges against another's, an `environment` ask with both branches named), committed `Take in <branch>: <change>`. The review then sees the change as it will merge.
- **Diff**: `git -C "$CODE_ROOT" diff <base>..HEAD`, where `<base>` is `intent.org`'s `#+BASE:`. This covers every path a change took: its spec edits and design, task merges, inline-path commits, inline fixes, ask answers, record corrections.
- **Input** (author-blind as in execute § 7, with the whole design in place of the cited rows): the diff, `intent.org`, every Seams row of `design.org`, `coverage.py --intent docs/changes/<change>/intent.org --output <the cycle's last after-file>`, the overlay's `roles/reviewer.md` and `priors.md`, and its scratch directory, `review-whole-change/` under the session's scratch directory, with a pointer to `roles/implementor.md` § A machine shared with other agents. No report, no Observations, no review files.
- **Looks for** what no per-task review could: interactions between tasks (a setting, a wrapper and a doc that disagree), a scenario whose test passes while the surface the user would use does not, a change that bypassed review, a contract weakened in one task and relied on in another.
- **Output**: `cycles/<cycle-id>/reviews/whole-change.md` in the Reviewer's format, one hand-back line. Its findings, and its Thoughts, are routed as in execute § 8: an untrue sentence in prose is corrected there and then as a text correction (`templates/architect-finding.md` § Routing) and holds nothing; other `blocking` ones become candidates on the main path for one more cycle (§ 7, `discovered_by: reviewer`, `source: whole-change-<cycle-id>`, which plan writes as `discovered_from` and `report.py` reads as a defect found after done, `critical: true`), and the change goes no further while one is open; `hardening` ones join the hardening list; a `spec-signal` is an ask.
- **The MVP decision**: when the review leaves no blocking finding, no open task or `critical` candidate on the main path remains, and the intent's status line has not yet reached `mvp`, the main path works end to end and the change stops for the user (`templates/hardening.md` § The MVP decision). Open tasks off the main path do not hold it back: they are listed in the ask and wait for the answer. The record: in every home the change ran one to five cycles after its last design task merged before the MVP decision, while the user pulled it forward by hand ("when this cycle is done we are stopping, reguardless of new tasks"): the orchestrator writes the status line `mvp <date> <HEAD> <ask-id>` and the one `decision` ask (harden / rethink / stop, with the hardening list and the record corrections to the specs in it, `:BLOCKS: plan`), commits both, finishes the phase (`state.py gate integrate`, the close commit) and hands back `integrate <cycle-id>: stopped, ask <id> blocks plan; <path>`. Resumed with the answer, it applies it as that template says; with no answer, nothing more runs on the change (`flows/plan.md` § 1).
- **Done**: after the user chose `stop`, or after `harden` once this review again leaves no blocking finding and no open task or candidate remains other than those the answer left, the orchestrator writes `#+STATUS: done <date> <HEAD>` in `intent.org` with the closing summary in its `* Status` (`templates/hardening.md` § The closing summary) and commits it in the code root with a subject naming the change (`templates/intent.md`). The change's documents stay where they are. A change with a workspace integration branch then merges into it: in that branch's worktree (`home.py worktree <branch>` gives its path; `git -C "$CODE_ROOT" worktree add <path> <branch>` when it does not exist), `git merge --no-ff <change> -m "Merge <change>"`; the next change of the hand-off takes it in at its design round. The branch's merge to main is the user's, recorded in the research brief that made it, never the orchestrator's.

The record: after the orchestrator reported two changes done, the user's smoke check, `/code-review` and UAT found nine defects and more; the cross-cutting ones (settings × wrapper × `SKILL.md`) had never been in any one task's diff.

## Inputs (from execute)

- All Implementor reports (held by orchestrator since execute, not seen by reviewers).
- All Reviewer findings.
- Plan's premise-check findings, if any.
- The set of merged diffs.
- The seam rows and scenarios this cycle's tasks cite (`cites_seams` and `cites_scenarios` in the task files; the rows in `design.org`, the scenarios in `docs/specs/`).
- Execute closed: the home commit `Orchestrator: <cycle-id> execute closed`, made once `state.py gate execute` passed; `state.py status` reads the phase as integrate from it.

## Exit gate

| Check | Kind | Condition |
|---|---|---|
| `blocking_findings_resolved` | computed | No finding routed `blocking` (`state.py route`) has resolution `pending` (severity is the reporter's high / medium / low; the decoration is the routing, `templates/architect-finding.md`) |
| `handshake_written` | computed | `handshake-<cycle-id>.json` has every required field, its candidates and design-diff elements fit `templates/handshake.md`, and every finding routed `candidate-task-<name>` has that candidate |
| `asks_routed` | computed | Every open ask in the inbox is in the handshake's `asks_for_user_open` (so plan picks it up next cycle), and no `open` ask has an answer the orchestrator has not acted on (an exact label is applied; anything else has its `* Readback` written, and may stay open awaiting the user's yes). A file in the wrong directory for its status, and git tracking, are reported by `asks.py inbox`, not gated |
| `open_tasks_refined_against_handshake` | asserted | Every still-open task in `<change>/tasks/open/` has been considered by step 7: it has an entry in the handshake's `task_refinements`, with an empty `impact` when nothing touched it. Every `blocking` finding and every main-path answered ask that needs new work has an entry in `candidate_tasks`; `hardening-list` findings have none until the user chooses to harden. No task file was written or edited by this phase, other than a `status: blocked`. `state.py assert open_tasks_refined_against_handshake --by phase-integrate` |

`state.py gate integrate` evaluates the computed checks afresh and shows the asserted one with who asserted it and when; it exits 0 only when all four hold. The next plan does not start otherwise — that's the loop-closure contract. A cycle whose integrate cannot close ends with `state.py cycle abandon --why "<reason>"`.

## Commit

Once the gate passes, in the home: `git -C "$ORCH_HOME" add .orchestrator && git -C "$ORCH_HOME" commit -m "Orchestrator: <cycle-id> integrate closed"` (add `.tasks` too when integrate touched it). That commit closes the cycle; nothing is archived and nothing moves, since every file of the cycle was written where it stays (`state.md` § The change's directory). Ask files were committed when created and when applied (`templates/ask.md` § Commit rule); this commit carries the rest: the handshake, the event log, the findings, `design-diff.json`, `meta-discoveries.json`, `candidate-tasks.json` and `task-refinements.json`. The record integrate changed (`design.org`, spec text, the intent's status line) was committed in the code root (§ 1, § 5, § 9). Open and deferred asks stay in the inbox for the next cycle.

The cycle's directory is what the deferred curation cycle (v2) reads. It is also the audit trail the user can walk through to understand any past decision.

## Loop closure: integrate → plan

The keystone transition is integrate → plan, **not** execute → review. Each plan is *required* to consume the prior integrate's outputs. Without that as a hard input contract, plan degrades into queue-running and the orchestrator becomes a glorified task runner.

This is the loop that gives the system *compounding leverage*: each cycle's discoveries make the next cycle's batch better. It is the operationalisation, at cycle altitude, of the two-way information flow.

## What integrate does **not** do

- Integrate does not modify code. (Conformance amends `design.org` and spec text, which are record, not code. Inline fixes are made in execute, in the `reviewed` state, `flows/execute.md` § 8. The one edit integrate makes in a code file is a text correction at § 9, prose only, `templates/architect-finding.md` § Routing.)
- Integrate does not run new test suites — the cycle's tests already ran in execute; the conformance check may re-run them read-only to confirm a promoted test passes.
- Integrate does not re-implement tasks — re-implementation is execute's job; integrate only routes findings.
- Integrate does not write or edit task text. A new task is a candidate and an open task's update is its impact set (§ 7); the next plan writes both (`flows/plan.md` § 2).
