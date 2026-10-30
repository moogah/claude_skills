# The state layer

The state layer answers two questions about a change and refuses what would make the answers false (`docs/specs/state-layer.org`, the normative text; this file is how to use it):

1. **Where does this change stand, and what comes next?** At every stage: open, design round, MVP decision, plan, execute, integrate, done. Any fresh session, the dispatcher included, gets the same answer from `state.py status`.
2. **Is this really done?** A task, gate or phase passes only on computed checks. An asserted check is recorded as asserted, never as computed.

**Files and git are the record.** There is no state file. Each fact has one source and no second copy: a task's status is its task file's `status:`, its merge is its `Merge task <name>` commit, a phase's close is a home commit, an ask is its file. The state layer reads them all, and writes one file: the change's **event log**, `<change-dir>/events.jsonl`, for the few facts files and git do not hold. The record: the facts a stored `state.json` held twice drifted from the files by hand (rejections, a start time, inline fixes), and the one state file kept by an LLM before the tool marked gates passed over 13 tasks with no merge commit (research `state-tool`, E3, P1).

The tool is `scripts/state.py` in this skill's directory (`python3 ~/.claude/skills/orchestrate/scripts/state.py <verb> …`, written `state.py <verb>` in the flow docs). It finds the home by walking up from its cwd for `.claude/orchestrator/config.yaml` (`--home <path>` names it), and the change from `--change <name>`, then `$ORCH_CHANGE`, then the open change whose worktree holds the cwd, then the only open change; otherwise it refuses `no_change` and lists them. Every verb prints JSON. A refusal is `{"ok": false, "error": "<slug>", "message": …}` on stderr, exit 1; a gate that does not pass prints its report on stdout and exits 1.

Run `state.py status` before anything else in a session, in the dispatcher and in every phase agent (`SKILL.md` § Two contexts).

## What is not here

Three tools sit beside the state layer and are not part of it:

| Tool | Does | Doc |
|---|---|---|
| `scripts/home.py` | `where [--sh]` (the home, its repositories, its open changes; `eval "$(home.py where --sh)"` exports `ORCH_HOME`, `ORCH_WORKTREES`, `ORCH_CHANGE`, `ORCH_CHANGE_DIR`, `ORCH_REPO`, `CODE_ROOT`), `init` (a new home's `config.yaml`), `open <name>` (a change's worktree, branch and `change.json`), `worktree <branch> [--sh]` (where a task worktree goes) | `overlay.md` § Bootstrap, § Repositories |
| `scripts/asks.py` | `add`, `set <id> field=value …`, `inbox`: writes and moves the ask files | `templates/ask.md` |
| `scripts/report.py` | `spend` (spend and quality by role, on demand), `context <agent-id>` (a running agent's context) | `report.md` |

Git and worktree operations (branching, merging, removing a worktree) are the agents', in the flows' bash blocks. So are the files agents write: task files, briefs, reviews, findings, `design.org`, the handshake.

## Verbs

| Verb | Does |
|---|---|
| `status` | Read-only. The change's `stage` and `next` step (§ Stages), the open `cycle` and `phase` with that phase's `gate` report, the `intent`'s approval and status line, the `inbox` (`{open, answered}`), the `asks` still waiting (open or answered, with `:BLOCKS:`), those that block work first, and `convention_breaks` when there are any. One call; needs no open cycle. |
| `gate [<phase>]` | Read-only. The open cycle's gate for its phase (or the named one), every computed check evaluated afresh, every asserted check with who asserted it and when (§ Gates). Exit 1 when it does not pass. |
| `conventions` | Read-only. Every break of the record's forms (§ Conventions), with where it is and the form expected. |
| `check done <task>` | Refuses (`not_done`) unless the task's `Merge task <task>` commit is on the change branch, a review file names that commit, and the tests pass at it: `after-<task>-<sha12>.status` exists and holds 0, or, against a red baseline (`baseline-<cycle-id>.status` non-zero), `after-<task>-<sha12>.compare` holds 0 (`flows/execute.md` § 4). It reads the files and runs nothing. Prints the merge commit. Run before writing `status: done`. |
| `check start <task>` | Refuses (`blocked`) while a task in its `blocked_by` has no merge commit on the change branch. Run before spawning its Implementor. |
| `check new <task>` | Refuses (`bad_task`) a follow-up (a task a handshake's `candidate_tasks` names, or one carrying any `discovered_*` field) without `discovered_from` and `discovered_by`, and a status outside the set. Run when plan writes a task file. |
| `cycle start [--cycle <id>]` | Starts a cycle in plan: appends `cycle-started` to the log. Refuses `cycle_open` while one is open; `not_ready` before the design round's Seams exist (or at done, or while the MVP decision waits); `mvp_decision_due` when the main path has ended; `bad_prior_handshake` naming each missing field of the newest `handshake-<cycle-id>.json`; `no_prior_handshake` when a cycle of the change closed and no handshake is there. The id defaults to `cycle-<epoch>`. |
| `cycle abandon --why "<reason>"` | Ends the open cycle without closing it (`cycle-abandoned`). The only way out of a cycle whose integrate cannot close. |
| `assert <check> --by <role>` | Records an asserted check of the open phase (`asserted`). Refuses `computed_check` for a check a script decides, `wrong_phase` for another phase's. |
| `route <finding-id> [--decoration blocking\|hardening] [--resolution <r>]` | Records a finding's routing and how it ended (`finding-routed`); the latest value of each field counts. Resolutions: `templates/architect-finding.md` § Resolution states. |
| `blocker <task> "<note>"` | A blocked task's note (`blocker-note`): the ask id, the finding id, `merge-conflict`, or an external dependency. Read only while the task's file says `blocked`. |
| `note <kind> "<text>" [--ref <id>] [--by <role>] [--path <file>]` | One journal line, at most 280 characters (`note`). Kinds: `discovery`, `decision`, `inline-fix`, `deviation`, `push-back`, `note`. The long form goes in a file and `--path` points at it. |
| `fix-link <commit> --task <t> --review <n>` | Links an inline-fix commit to the review finding it answers, when its subject does not say so (`inline-fix-link`). |
| `correct <seq> --why "<reason>"` | A correction naming an entry of the log (`correction`); every reader leaves the corrected entry out. |
| `log [--cycle <id>] [--kind <kind>]` | Read-only. The log with corrections applied. |
| `asks [--all \| --cycle <id>]` | Read-only. The inbox's ask files (with `--all`, every cycle's `asks/` too) with the fields the handshake's ask lists carry (`templates/handshake.md`). With `--cycle`, `open` (every open file) and `resolved` (every other ask file the home's git touched since that cycle's `cycle-started` or that is changed and not yet committed, and every `answered` or `deferred` file still in the inbox, carried until it is applied): the handshake's two lists. |

## Stages

`status` reads the stage from the record, in this order:

| Stage | Read from | Next step |
|---|---|---|
| `not open` | no `change.json` | open the change (`flows/open.md`) |
| `open, no intent` | no `docs/changes/<change>/intent.org` in the code root | write the intent (`flows/open.md`) |
| `done` | `intent.org` `#+STATUS: done …` | nothing |
| `MVP decision` | `#+STATUS: mvp <date> <commit> <ask-id>`, that ask unanswered | wait for the user's answer to `<ask-id>` |
| `intent, no design` | `design.org` has no Seams row | the design round (`flows/design.md`) |
| `cycle open` | the log's last `cycle-started` has no `integrate closed` commit and no `cycle-abandoned`; the phase is the first of plan, execute, integrate with no `Orchestrator: <cycle-id> <phase> closed` commit in the home | resume that phase; or, when its gate passes, close it and go on |
| `main path ended` | no open task (`status` other than `done`, `externalised`) has `on_critical_path: true`, at least one has it, and no `mvp` or `done` status | integrate's MVP decision (`flows/integrate.md` § 9), not another plan |
| `designed, no cycle open` | none of the above | start a plan: `state.py cycle start` (`flows/plan.md`) |

**Closing a phase** is a commit in the home, made by the phase agent once `state.py gate <phase>` passes: `git -C "$ORCH_HOME" add .orchestrator && git -C "$ORCH_HOME" commit -m "Orchestrator: <cycle-id> <phase> closed"`. The commit carries the phase's files and the event log. A cycle is closed by its `integrate closed` commit with `handshake-<cycle-id>.json` written. Nothing else records a phase: a subject in another form is not read as a close (§ Conventions).

## Task status

A task's status is its task file's `status:` (`templates/task-body.md`), and nowhere else. The orchestrator writes it; the check verbs say when it may.

| Status | Meaning | Written when |
|---|---|---|
| `ready` | Drafted, not started | plan writes the file (`check new`) |
| `in_progress` | An Implementor is working on it (also: re-implementing after a rejected review) | execute spawns it (`check start` first) |
| `needs_review` | Merged to the change branch as `Merge task <name>`; awaiting its Reviewer | execute merges it |
| `done` | Reviewed and accepted, any inline fixes applied | `check done` passes |
| `failed` | Implementation or tests failed; the worktree is kept for debugging | |
| `blocked` | Waiting on another task, an ask, a blocking finding, a merge conflict or an external dependency; `state.py blocker` says which | |
| `externalised` | Out of scope; moved to `.tasks/` (`externalisation.md`) | |

`done`, `failed`, `blocked` and `externalised` are **stopped**: execute's gate passes when every task in the batch is stopped. A review that sends a task back is a `<task>-rework` brief and review file; there is no rejection counter. The task's times are its commits' times.

## The event log

`<change-dir>/events.jsonl`: one JSON object per line, `{"seq", "at", "cycle", "kind", …the fields its kind names}`. Only the verbs above write it. An entry is never edited or removed; a mistake is answered by `state.py correct <seq>`, and every reader applies the correction. It is tracked by the home repo and committed at each phase close.

| Kind | Fields | Written by |
|---|---|---|
| `cycle-started` | `prior_handshake` (or absent on a first cycle) | `cycle start` |
| `cycle-abandoned` | `why` | `cycle abandon` |
| `asserted` | `phase`, `check`, `by` | `assert` |
| `finding-routed` | `finding`, `decoration`, `resolution` | `route` |
| `blocker-note` | `task`, `note` | `blocker` |
| `note` | `note_kind`, `text`, `ref`, `by`, `path` | `note` |
| `inline-fix-link` | `commit`, `task`, `review` | `fix-link` |
| `correction` | `corrects`, `why` | `correct` |

A cycle's start is the one fact the log holds before anything of the cycle is on disk: `status` names the open cycle from it at once.

## Gates

Each phase's gate is a list of checks. `state.py gate` evaluates every computed check afresh each time it reports, so a gate that passed and whose facts later changed (a merge commit removed, a finding re-routed) reads as failing. An asserted check passes once it is asserted in that cycle and phase, and is shown with `kind: asserted`, `by` and `at`, never as computed. The flow docs say what each check means for the work.

| Phase | Check | Kind | Passes when |
|---|---|---|---|
| plan | `prior_handshake_complete` | computed | the prior handshake `cycle start` read has every required field (`templates/handshake.md`); a first cycle has none |
| plan | `batch_composed` | asserted | `state.py assert batch_composed --by <role>` |
| plan | `batch_cites_seams` | computed | the batch (the task briefs under `cycles/<cycle-id>/briefs/`, less review, rework, premise-check and conformance briefs) is not empty, every brief has a task file, and every task in it cites a seam |
| plan | `follow_ups_name_their_source` | computed | every follow-up in the batch has `discovered_from` and `discovered_by` |
| plan | `goal_drift_signed_off` | computed | no goal-drift ask in the inbox (a `decision` whose options are revise / split / abandon / continue) is unanswered or answered with anything but one of those labels |
| plan | `mvp_decision_not_due` | computed | the main path has not ended (§ Stages) |
| execute | `batch_stopped` | computed | the batch is not empty and every task in it is stopped, with a status in the set |
| execute | `done_tasks_merged_and_reviewed` | computed | every `done` task in the batch passes `check done`: merged, reviewed at its merge, tests passing at its merge |
| execute | `blocked_by_order` | computed | every merged task in the batch merged after each task in its `blocked_by` |
| integrate | `blocking_findings_resolved` | computed | no finding routed `blocking` has resolution `pending` |
| integrate | `handshake_written` | computed | `handshake-<cycle-id>.json` has every required field, its candidates and design-diff elements fit, and every finding routed `candidate-task-<name>` has that candidate |
| integrate | `asks_routed` | computed | every open ask in the inbox is in the handshake's `asks_for_user_open`, and no open ask has an answer without a readback |
| integrate | `open_tasks_refined_against_handshake` | asserted | `state.py assert open_tasks_refined_against_handshake --by <role>` |

There is no PM digest and no gate on one. The **author-blind review** (the Reviewer never sees who wrote the code, nor the Implementor's report) is prose in `flows/execute.md`, followed by the phase agent; no code checks it, and these docs do not call it enforced.

## Conventions

The record is readable only while agents keep these forms. `state.py conventions` reports each break with where it is and the form expected; a break in a form a computed check reads fails that check rather than being guessed around (a task merged as `Merge branch 'task-x-1793…'` has no merge commit as far as `check done` is concerned, and the check names the commit).

- **Merge commits on the change branch**: `Merge task <name>`, and `Merge task <name> (rework)` for a rework; a `: <one line>` may follow.
- **Phase-close commits in the home**: `Orchestrator: <cycle-id> <phase> closed`, exactly.
- **Inline-fix commits**: `Inline fix (review <task> #<n>)`; a `: <one line>` may follow. When a commit cannot carry it, `state.py fix-link`.
- **Test runs**: `<change-dir>/after-<task>-<sha12>.status` and, against a red baseline, `.compare`, named for the merge commit, beside the output `.txt`; `baseline-<cycle-id>.status` beside `baseline-<cycle-id>.txt`. The exit codes are tracked and are the record; the outputs are not.
- **Review and brief files**: `cycles/<cycle-id>/reviews/<task>.md`, `<task>-rework.md`, `whole-change.md`; `cycles/<cycle-id>/briefs/<task>.md`, `<task>-rework.md`, `review-<task>.md`, `premise-check.md`, `conformance.md`. A review file's frontmatter names the merge commit it reviewed: `merge_commit: <sha>`.
- **Task-file frontmatter**: the keys `templates/task-body.md` names, `status` from § Task status, `task_name` the file's name. A key holding a fact whose source is elsewhere (`merge_commit`, the times) is a break that names that source; so is `decoration` or `resolution` in a finding file.

## Recovery

- **A missing or unreadable event log mid-cycle**: restore it from git (it is committed at each phase close); what was appended since the last close is redone (`cycle start` again if the log lost it, the assertions, the routings). An unrestorable log is an `environment` ask (`templates/ask.md`).
- **A convention break**: fix the file, or, for a commit, record the link the commit should have carried (`fix-link`) or merge again in the form; history is not rewritten on a shared branch without the user.
- **A wrong entry in the log**: `state.py correct <seq> --why`, then record the right fact.
- **A cycle that cannot close**: `state.py cycle abandon --why`; the next `cycle start` reads the newest handshake there is, and a change none of whose cycles closed starts as on a first cycle.
- **A change live on the old state tool** (a `state.json` in its directory): it starts its next cycle on this layer with nothing converted, once its open cycle has closed on the old tool. Its `state.json` is left as it is and read by nothing; `state.py conventions` lists the fields its task and finding files still carry, to remove by hand; the old handshake is read as the prior (`templates/handshake.md` § A handshake written before this template).

## The change's directory

`<home>/.orchestrator/changes/<change>/`, tracked by the home repo (only `.orchestrator/.gitignore`'s test outputs, `*.txt`, and `*.tmp` stay out; `home.py open` writes it):

```
change.json               # home.py open: repository, branch, worktree, base, from
events.jsonl              # the event log
handshake-<cycle-id>.json # integrate § 8: the next plan's input (templates/handshake.md)
hardening.org             # templates/hardening.md
inbox/<ask-id>.org        # open, answered and deferred asks (templates/ask.md)
tasks/open/<name>.md      # task files (templates/task-body.md); tasks/closed/ after merge
baseline-<cycle-id>.status # execute § 1: the baseline's exit code (its output, .txt, untracked)
after-<task>-<sha12>.status # execute § 4, with .compare: the record that the tests pass at the merge (output .txt untracked)
cycles/<cycle-id>/
  briefs/<task>.md        # plan § 5: the batch, and what the Implementor sees; review-<task>.md, <task>-rework.md
  premise-check.md        # plan § 3
  reviews/<task>.md       # and <task>-rework.md, whole-change.md (integrate § 9)
  reports/<task>.md       # the Implementor's structured report, orchestrator-only
  findings/<finding-id>.md
  asks/<ask-id>.org       # applied, declined and superseded asks
  design-diff.json        # integrate § 1
  meta-discoveries.json   # integrate § 4
  task-refinements.json   # integrate § 7
  candidate-tasks.json    # execute § 5, § 8 and integrate § 7
```

Every file is written by the step named beside it; nothing is archived at close, since nothing moves. The record of the change itself (`intent.org`, `design.org`, `docs/specs/`) is in the code root, under `docs/`.
