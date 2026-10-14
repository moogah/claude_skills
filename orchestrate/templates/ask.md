# Ask template

An **ask** is a question the orchestrator needs the user to answer. It is one org file, `<home>/.orchestrator/inbox/<ask-id>.org`, written the moment the ask is raised, in any phase. The file is the record: the user answers in it, git carries its history, and the orchestrator reads it at its sync points. `state.json` keeps only an index entry `{id, kind, status, blocks, path}` (`state.md`) so `status`, `counts` and the gates can read the inbox without parsing it; when the two disagree the file wins and `record set` rewrites the index from it.

Any role can raise one: a Reviewer `spec-signal`, an Architect `interface-drift` finding against a stale design doc, an Implementor push-back that only the user can settle, a PM blocked-path or goal-drift signal, a plan-time escalation. None of them writes the ask. **The orchestrator writes the ask** from the finding, because the PM does not read code and the finding roles do not know the reader. The finding must give the orchestrator what the file needs (see "What a finding must carry").

The reader is whoever the overlay's `asks.reader` names (`overlay.md`). Default: a technical product manager who has not watched the development and has a shallow view of the internals.

The record shows why the unit is a file and not a chat turn: every dropped or re-asked question was free text inside a status message, every tool-form question was answered but one such call sat open 950 minutes overnight on a question that blocked nothing, and in every cycle of one project the orchestrator invented files of its own to hold open threads across sessions.

## The file

```org
:PROPERTIES:
:ID: ask-cycle-1790026644-9
:KIND: decision
:STATUS: open
:RAISED_BY: architect
:REF: arch-cycle-1790026644-1
:HAPPENED: yes
:END:
#+TITLE: Should a video you fetch by hand with --download-video be added to the archive?

* About

--download-video fetches one YouTube video by URL; it is not tied to any Reddit post. The archive is the set of videos the scraper has indexed from posts; each has a record, and a video YouTube has removed gets a 'dead' mark so scrape runs stop retrying it.

* What happened

The seam row in design.md, the spec and the docs all say a hand fetch clears the dead mark. The code does not: it calls the downloader and never touches the index. Verified against the code at the cycle-3 conformance check.

* Options

- Fix the docs and the code :: Every hand fetch is indexed into the archive and clears the dead mark. Videos you fetch for reasons unrelated to the archive are indexed too.
- Do nothing to the code :: --download-video stays a plain fetch: the file lands on disk and the archive does not know about it. The docs are corrected to say so.

* Recommendation

- Do nothing to the code :: What has been observed is a false sentence in the docs; nothing has been observed to need hand fetches in the archive.

* If unanswered

Docs corrected at this integrate; code untouched.

* Answer

# An option label from above, or your own words; everything below this line is read as your answer.
```

**Drawer.** A file-level `:PROPERTIES:` drawer in the org-roam shape; `:ID:` makes every ask an `[[id:…]]` link target. The drawer is the tool's: `state.py record add asks` writes it and `record set asks` updates it; a hand-edited `:STATUS:` is reconciled by the next `record set`. Keys, written only when set except the first three:

| Key | Rule |
|---|---|
| `:ID:` | `ask-<cycle-id>-<seq>`, the file's basename. The cycle is in the id; there is no `CYCLE` key. |
| `:KIND:` | One of five; decides routing (table below). |
| `:STATUS:` | `open` → `answered` → `applied`; or `deferred`, `declined`, `superseded`. Decides the path (below). |
| `:RAISED_BY:` | `architect | reviewer | implementor | pm | orchestrator`. |
| `:REF:` | The artifact that raised it: a finding id, a discovery id, a review file path, the premise check, a signal name. Tasks and notes point back with `discovered_from: <id>` or `ask: <id>`; the ask does not list them. |
| `:HAPPENED:` | `yes` only if the problem has happened, as `templates/architect-finding.md` § Observed defines it (a defect present in merged code, cited by line, included). An agent's reading of what *could* go wrong is `no`. With the first sentence of `* What happened` this is the finding's `observed`. |
| `:BLOCKS:` | Task names, `merge:<task>`, `plan` (a design-round question: plan drafts nothing until it is answered, `flows/design.md` § 5), `next-plan`, `done` (the change is not reported done while it is open). Absent means nothing waits on this. Decides the inline rule below and the routing in `flows/execute.md` § 9. |
| `:APPLIED_VIA:` | Set with `status=applied`: the edit, task or commit that applied the decision. |
| `:REVISED_FROM:` | On a reversal, the id of the ask whose decision this one replaces; the old file becomes `superseded`. |

No timestamps: git has them.

**Path.** A pure function of id and status. `inbox/<id>.org` while `open`, `answered` or `deferred`; `cycles/<cycle-from-id>/asks/<id>.org` once `applied`, `declined` or `superseded`. `record set` moves the file when the status crosses that line (`git mv` when tracked); `validate` reports a file in the wrong directory for its status, an index entry with no file, and an inbox file with no index entry.

**Title.** `#+TITLE:` is the question: one plain sentence a reader who has not seen the code can answer, ending in `?`, one line, 600 characters at most. Not a title.

**Prose.** One paragraph per line, so a field change is a one-line diff; the parser joins what the user re-wraps. Sections are matched by their level-1 heading, case-insensitively, in any order. `Observed` is accepted for `What happened`, `Default` for `If unanswered`, `Decision` for `Answer`. Unknown sections and `** ` subheadings pass through untouched. `# ` comment lines are stripped on read, which is how the Answer hint disappears.

**Who writes which section.**

| Section | Writer | Rule |
|---|---|---|
| `* About` | orchestrator | What the thing is *for*, in product terms, written from the code and docs, not copied from the finding. Any term in the title, `* About` or `* Options` that is not in the product's `docs/glossary.org` (`templates/glossary.md`) is glossed here; more than three glosses means the ask is not plain yet. A term being in the change's `intent.org` does not exempt it: the terms that confused the user on the record were all in intents they had approved. Process vocabulary (seam, speculated, reconciled, disposition, the `arch-` / `disc-` / `eoc-` id families) never appears in these sections; a `seam/<name>` id appears only in `:REF:` and `:APPLIED_VIA:`. |
| `* What happened` | orchestrator | What happened, or what the reasoning is, and what the user would notice. One paragraph. Its first sentence is the evidence `:HAPPENED:` rests on. |
| `* Options` | orchestrator | Two to four `- label :: consequence` items; a consequence says what the user would notice, not what the code would do. The do-nothing option is always present and always last. |
| `* Recommendation` | orchestrator | One item: the label and why, and the why says whether it rests on something observed. A recommendation that does not meet the demonstrated-reason rule (`templates/architect-finding.md` § Demonstrated reason) is the do-nothing option. |
| `* If unanswered` | orchestrator | Every ask has one, so a cycle can close with open asks: what happens and at which boundary (this integrate, next plan, the change's done). A default never creates a task or backlog item; if it would need work, the default is do nothing. |
| `* Answer` | user | An option label or free text. |
| `* Readback` | orchestrator | Exists only when needed (free text or a rule): the answer restated as a consequence, 600 characters at most. |
| `* Discussion` | anyone | Created when first needed; one `** <date> <who>` subheading per turn; preserved byte for byte by the tool. The orchestrator appends the turns of a discussion held in chat. |

`record add asks` takes the JSON fields (`question`, `about`, `observed`, `options`, `recommendation`, `default_if_unanswered`, `raised_by`, `blocks`, …) or `@draft.org`, a file in this shape whose prose it keeps; `record set asks <id> field=value …` edits one field in place (`about=…`, `status=…`, `applied_via=…`, `decision_readback=…`).

## Kinds and routing

| Kind | What it is | What happens |
|---|---|---|
| `decision` | Only the user can choose, and the choice changes the product | A file in the inbox. The only kind that becomes a question. |
| `doc-correction` | Retired 2026-10-13. The code is right and a document is stale | Not an ask. A **record correction**: the orchestrator fixes the text, keeps it at the contract (`templates/design.md` § Seams), commits it with a subject naming the row or sentence and writes one journal line `state.py note note "record-correction: <what>" --ref <seam-id>`; the digest counts the lines. The record: 47 of 47 such files were titled as questions nobody answered, and 39 would not have existed once rows hold contracts only. Files of this kind already on disk keep it; the tool still reads them. |
| `confirmation` | A default already applied; a PM action the PM may take alone (a conformance run, a re-rank); a prior recorded from the user's words (`overlay.md` § Priors) | The same: written applied into `cycles/<cycle-id>/asks/`, with `* About` and `* What happened` and no options; its `* Answer` says "Applied without asking: <what>. Object if that is not what you meant." Never a question. |
| `environment` | A licence, a wedged container, a permission, a missing or unreadable `state.json` or handshake | The blocking call, at once, alone. Also a file when the user is absent, so the next session finds it. The only kind that may bypass `state.json`. |
| `process` | About the orchestration itself: the design round, the state file, the skill text | A file in the inbox with no options; never presented. The digest counts them. The user copies the file into the process-improvement workspace when reviewing the process. |

Triage happens when the orchestrator writes the file, not when the digest is rendered. A finding routed "to the user" by a role brief is a candidate; the kind decides whether it is asked. One finding can yield a record correction and a file: the stale sentence is fixed and journaled now, and the product question it exposed is a separate `decision`. The record shows this pairing is the common case.

## Answer protocol

The user writes under `* Answer` either an option's label or free text. The orchestrator writes a chat answer ("ask-3: narrow the clause") into the file the same way; chat is the fallback, the file is the channel.

- `state.py inbox` (and `status`, in short) reports every open file whose `* Answer` is non-empty, with the option label it names and how surely: `exact` (the label, matched case-insensitively, whitespace folded, trailing punctuation ignored), `fuzzy` (one label is close), or `none` (free text). It also lists other ask ids the answer mentions, and an option label written under some other heading as an `answer_candidate`.
- An `exact` match is applied at the next sync point with no readback: `record set asks <id> status=answered`, apply it, then `record set asks <id> status=applied applied_via=<ref>`. The file moves to `cycles/<cycle-id>/asks/`.
- Free text, a `fuzzy` match, a candidate under another heading, or any reply that states a rule, gets a `* Readback` paragraph restating it as a consequence (`record set asks <id> decision_readback="…"`), and `:STATUS:` becomes `answered` only when the user writes "yes" (or a correction) under `* Readback` or says so in chat. The file stays `open` meanwhile and does not hold the integrate gate; `status` counts it under `awaiting_confirmation` and reports `readback_confirmed` once the yes is there. A reply that states a general rule rather than a choice is also appended, in the user's words, to the overlay's `priors.md` (`overlay.md` § Priors) at readback time, with a `confirmation` file written applied; a correction at readback edits the prior line in place.
- Applying a decision means doing what the chosen option's consequence names, and nothing more. When that is a code change, it takes one of the three paths every code change takes (`SKILL.md` § Critical requirements): a batch task, the inline path, or an inline fix for a review finding; the orchestrator never edits the code directly to apply an answer. The record: an answer applied straight to `settings.json` with no Implementor and no Reviewer undid a rule the user had just asked for, found a day later by `/code-review`. A seam row the decision moves is amended at integrate § 1 with the ask id as the `ref` in `design-diff.json`; an open task the decision contradicts is handled at integrate § 7. A requirement or scenario in `docs/specs/` that the decision settles or contradicts is edited in place in the code-root commit that applies it: the spec may have been written before the question was answered, and nothing later rewrites it. The record: a design-round answer became Decision D4 while the delta kept both options and the main spec still said a rejected file is treated as absent; only the whole-change review caught it (bookmarks `cycle-1791996711`).
- A reversal is a new file with `:REVISED_FROM: <old-id>` (`"revised_from": "<old-id>"` in the JSON given to `record add asks`); the old file becomes `superseded` and moves in the same command.
- `deferred` and `declined` carry the user's words in `* Answer`.

## Sync points

The one list of where the orchestrator reads the inbox; the flows and `SKILL.md` point here. `state.py status` at session start and at a phase agent's start; each phase boundary (`phase set`, `gate pass`, `init`); every `ScheduleWakeup` firing; the user's next prompt. `status` prints `inbox: N open, M answered (ids)` and its `resume` hint says when answers are waiting; `state.py inbox` prints the full scan. An answered file is handled before any other work at that point, by the phase agent (the dispatcher only sees that an answer waits): readback if free text; apply; `record set … status=applied applied_via=…`; the move; the commit.

## The inline rule

For every phase; this is the one statement of when a question blocks the session, and the flows point here. The blocking call (`AskUserQuestion`) is for an `environment` ask, and for an ask that blocks work waiting now while the user is present: its `:BLOCKS:` names a task or a merge of the current phase (in execute, a merge in this batch or a task not yet spawned; in plan, a task of this batch; on the inline path, its own commit), or `plan` (the design round's open questions). An ask whose `:BLOCKS:` is `next-plan`, `done` or absent never takes the call. Present means the user acted within the last 15 minutes of the session (by the timestamps of their messages; a session with no message from the user at all counts as absent). Otherwise, in every phase including integrate, the ask is the file: each task it blocks is set `blocked` with `blocker_note: <ask-id>`, the orchestrator finishes everything that does not wait on it, passes the phase gate if it can (`blocked` counts as stopped) and moves on, and stops with a one-line pointer to the file only when the gate cannot pass. It never waits for the answer, with one exception: the design round's open questions are asked in full before plan, and the change waits for them when the user is absent (`flows/design.md` § 5; they have no `If unanswered` default, and the record shows the ones built on provisionally were the ones reversed). One question per call, always. The call is the dispatcher's (`SKILL.md` § Two contexts): a phase agent has no user, so it writes the file, finishes what does not wait, hands back `stopped` naming the ask, and is resumed once the answer is in the file. The record shows the 950-minute call was made at integrate, two hours after the user's last action, about a sentence in a spec that nothing waited on.

## Commit rule

`.orchestrator/` is tracked by the home repo. `state.py init` writes `.orchestrator/.gitignore` (the test outputs `baseline-*.txt` and `after-*.txt`, `*.tmp`, the lock file, the migration backups) when absent and refuses to start when the home repo ignores the directory (an `environment` ask: delete the line). The orchestrator commits, in the home:

- an ask file when it is created (`git add .orchestrator/inbox/<id>.org`; the subject names the id, so `git log --grep=ask-` finds the trail);
- a readback when it is written, together with the user's answer it restates (one commit, the subject naming the id), and with it any `priors.md` line and `confirmation` file the answer produced;
- an applied ask alongside the change that applies it (the moved file and the edit or merge in one commit, as the `--download-video` reversal was; one commit per repository with the same subject when the change is in a separate code root; a spec or `design.org` edit is in the code root, since the record lives under `docs/` there);
- everything else, `state.json` included, at phase close (`Orchestrator: <cycle-id> <phase> closed`). Between those commits the index at HEAD may lag the files; the files are the record.

A new ask is then a new file in `git status`; an answer is a diff of one section; a reversal is a second commit on the file. The record shows the project that gitignored `.orchestrator/` lost its handshakes to removed worktrees three times.

## Chat

The chat still carries, from the dispatcher: a new inbox file (one line: the id, the question, the path, read from `state.py status` after a hand-back); a readback when one is needed; an `environment` call; a stop with the one-line pointer. Asks come first; phase results follow, or go in a separate message. Narration is unchanged.

## What a finding must carry

A role that routes a finding to the user (`roles/reviewer.md` `route-to-user`, `roles/architect.md` interface-drift against a stale doc, `roles/implementor.md` stop-and-ask) writes, in the finding, the three things the orchestrator cannot infer from the diff:

- what the feature, flag or rule is for (a sentence; the orchestrator rewrites it for the reader),
- its `observed` block (every finding now carries one; `templates/architect-finding.md` § Observed),
- what it blocks: a task, a merge, the next plan, the change's done, or nothing.

A finding routed to the user without these is not asked as it stands. When the gap is a matter of reading (the flag's help text, the doc the finding cites, whether the ledger has entries), the orchestrator fills it; when it is a matter of redoing the analysis, the finding goes back to the role. A PM candidate question in the digest is the same: the orchestrator writes its file, since the PM does not read code.

## Blocking without a second artifact

An ask that blocks tasks does not get a disposition task. The orchestrator sets `status: blocked` and `blocker_note: <ask-id>` on each task named in `:BLOCKS:` (the field `state.md` already defines for user decisions). The PM's stale and blocked-path queries read `blocker_note` and re-surface the existing ask id; they do not raise a new ask for a decision that is already open. When the ask is applied, the note is cleared and the task returns to `ready`.

The alternative, a disposition task whose body is one paragraph and a pointer to `inbox/<id>.org`, was considered and left out because it writes the same text a second time; restore it if a real cycle shows the task tree needs the placeholder.

## Worked examples

Three asks from the record, each shown as it was asked and as this template writes it.

### 1. A decision that was reversed within eleven minutes

As asked (Project A, cycle 3, 2026-09-23 17:47, in the blocking call):

> Conformance finding (blocking, verified): `--download-video` calls the downloader directly and never records the fetch, so fetching a `dead` video by hand does NOT release its ledger line (and never indexes the file). The design/spec/docs text will be corrected either way. Do you also want the code fixed?
> Options: Text fix + .tasks (Recommended) / Text fix + in-change task / Text fix only.

The user chose the in-change code fix, a critical-path task was created, and eleven minutes later the user reversed it: the flag is for one-off downloads that are not part of the archive. The question never said what the flag was for.

As this template writes it: the file under "The file" above, at `.orchestrator/inbox/ask-cycle-1790026644-9.org`, committed when written. With `* About` in front of them, the user would have recognised the one-off use at once; the reversal turned on exactly that sentence. The answer `do nothing to the code` matches the label and is applied at the next sync point with no readback; the file moves to `cycles/cycle-1790026644/asks/` in the commit that corrects the docs.

### 2. A doc correction that was asked seven times (historical: since 2026-10-13 this is a record correction, a journal line and a commit, not a file)

As asked (Project B, cycle 1 plan, seven asks of this shape, each with the recommendation "apply"):

> ask-cycle-1789334430-1 — design.md D4 numbers the removed scaffold stages "4–6"; the live pipeline (`config/workspaces/scaffold.org:157-224`) has five stages and the removed ones are 3–5. Doc fix; the seam row `seam/workspace-scaffold-pipeline` in the same design.md already records the correct numbering.

As this template writes it, at `cycles/cycle-1789334430/asks/ask-cycle-1789334430-1.org`, never in the inbox, committed with the edit:

```org
:PROPERTIES:
:ID: ask-cycle-1789334430-1
:KIND: doc-correction
:STATUS: applied
:RAISED_BY: architect
:REF: cycles/cycle-1789334430/premise-check.md
:HAPPENED: yes
:APPLIED_VIA: design.md D4, edited at plan
:END:
#+TITLE: Should design.md D4 number the removed scaffold stages 3–5, as the pipeline does?

* About

D4 is the design decision that removed three stages from the workspace scaffold pipeline. The pipeline has five stages; the decision text numbers the removed ones 4–6.

* What happened

The live pipeline (scaffold.org:157-224) has five stages and the removed ones are 3–5. The seam row for the pipeline in the same design.md already records the correct numbering.

* Answer

Applied without asking: stage numbers in design.md D4 corrected from 4–6 to 3–5 to match the code (scaffold.org:157-224). Object if the document was right.
```

This is the user's standing rule of 2026-09-26, which until now lived in one task file's `## Observations`.

### 3. A question about something that has not happened

As asked (Project B, cycle 2, design question OD-3, 2026-09-24):

> Should stamps carry a format version? If what relpath means ever changes, unversioned stamps could not be told apart; cheap to fix now and impossible once real stamps exist.

The user answered: "Let's punt on this decision, I want to use this a while before we decide on polish like this."

As this template writes it, at `.orchestrator/inbox/ask-cycle-1789935943-3.org`, shown after that answer and the orchestrator's readback:

```org
:PROPERTIES:
:ID: ask-cycle-1789935943-3
:KIND: decision
:STATUS: open
:RAISED_BY: architect
:REF: cycles/cycle-1789935943/premise-check.md
:HAPPENED: no
:END:
#+TITLE: Should each bookmark stamp carry a format version number from the start?

* About

A stamp is the small set of properties written into a bookmark so it can be found again after its repository moves: which repository, and the file's path inside it. Every bookmark the feature touches gets one.

* What happened

Nothing yet; the reasoning is that if the meaning of the path property ever changed, old stamps could not be told from new ones. No stamp has been written and no change of meaning is planned.

* Options

- Add a version property now :: Every stamp carries one more property that nothing reads today.
- Do nothing :: Stamps have two properties. If the meaning ever changes, a one-off migration rewrites them; old stamps are recognisable by the missing version.

* Recommendation

- Do nothing :: Nothing has happened that a version would have helped with, and the migration cost, if it ever comes, is bounded.

* If unanswered

No version property. Revisit only if the stamp's meaning changes.

* Answer

Let's punt on this decision, I want to use this a while before we decide on polish like this.

* Readback

No version property; the question is reopened only if the stamp's meaning changes. Say yes here or in chat.
```

The answer is free text that states a rule, so it gets the readback and the line goes to `priors.md`; on yes the status becomes `answered`, then `applied` with `:APPLIED_VIA: design.org § Decisions`. Under the plain-language rule it needed one gloss (stamp), given in `* About`.

## What this template does not do

- It does not replace the finding templates. A finding is the evidence; the ask is the question. The finding keeps its severity, class and `discovered_from`; the ask carries none of them.
- It does not put the file in the digest. The digest carries the open decisions with their paths and the counts (`templates/pm-digest.md`).
- It does not render a per-cycle page. The index is `state.py status`; the applied-without-asking list is the set of files in `cycles/<cycle-id>/asks/`; the decisions are the files' `* Answer` and `* Readback` sections. Restore a page if a live cycle shows the reader wants one.
- It does not create disposition tasks (see "Blocking without a second artifact").
- It does not count ask quality. Answers outside the offered options, reversals and clarification requests are visible in the files; counting them as a PM signal is deferred until a cycle shows the need.

## Measurement

Two tools in the process-improvement workspace. `findings/tools/chat-channels.py` reads the session transcripts: expect no blocking call open longer than 15 minutes unless its kind is `environment`, and one question per call. `findings/tools/ask-inventory.py` keys on the files: every ask with a `path`, and in git every ask file added in a commit naming it and every applied ask with two or more commits on its file. The 2026-09-26 baseline is 62 asks, 14 with options, 26 with a recommendation, 39 with no source.
