# Ask template

An **ask** is a question the orchestrator needs the user to answer. It is the only artifact whose reader is the user, so it has the strictest form in the skill: one persisted record, one rendering, and a plain-language rule.

Any role can raise one: a Reviewer `spec-signal`, an Architect `interface-drift` finding against a stale design doc, an Implementor push-back that only the user can settle, a PM blocked-path or goal-drift signal, a plan-time escalation. None of them writes the ask. **The orchestrator writes the ask** from the finding, because the PM does not read code and the finding roles do not know the reader. The finding must give the orchestrator what the record needs (see "What a finding must carry").

The reader is whoever the overlay's `asks.reader` names (`overlay.md`). Default: a technical product manager who has not watched the development and has a shallow view of the internals.

## Record

Lives in `state.json` `asks_for_user[]` (see `state.md`) and is copied unchanged into the handshake's `asks_for_user_open` / `asks_for_user_resolved`. This is the only schema; `state.md` and the flows point here.

```json
{
  "id": "ask-<cycle-id>-<seq>",
  "kind": "decision | doc-correction | process | environment | confirmation",
  "raised_by": { "role": "architect | reviewer | implementor | pm | orchestrator",
                 "ref": "<finding-id | discovery-id | reconciliation-note path | signal name>" },
  "question": "<one plain sentence, ending in a question mark>",
  "about": "<one or two sentences: what the feature, flag, file or rule is FOR, in product terms>",
  "observed": { "happened": true, "evidence": "<where and when> | reasoning only: <the reasoning>" },
  "options": [
    { "label": "<short>", "consequence": "<what the user would notice if this is chosen>" },
    { "label": "Do nothing", "consequence": "<what stays as it is>" }
  ],
  "recommendation": { "option": "<label>", "why": "<one sentence; cites observed>" },
  "default_if_unanswered": "<what happens, and at which boundary: this integrate | next plan | archive>",
  "blocks": [],
  "status": "open | answered | applied | deferred | declined | superseded",
  "decision": null,
  "decision_readback": null,
  "applied_via": null,
  "revised_from": null
}
```

| Field | Rule |
|---|---|
| `id` | `ask-<cycle-id>-<seq>`. The only key for the id (not `ask_id`). |
| `kind` | One of five; decides routing (table below). |
| `raised_by.ref` | The artifact that raised it. Tasks and notes point back with `discovered_from: <id>` or `ask: <id>`; the ask does not list them. |
| `question` | One sentence a reader who has not seen the code can answer. Ends in `?`. Not a title. |
| `about` | What the thing is *for*. Written by the orchestrator from the code and docs, not copied from the finding. |
| `observed.happened` | `true` only if the problem has occurred: a failing run, a wrong output, a user report, a false statement in a shipped document. An agent's reading of what *could* go wrong is `false`, with the reasoning in `evidence`. |
| `options` | Two to four. Each `consequence` says what the user would notice, not what the code would do. The do-nothing option is always present and always last. |
| `recommendation.why` | Must say whether it rests on something observed. A recommendation that rests on reasoning only defaults to the do-nothing option unless the user has asked for that work. |
| `default_if_unanswered` | Every ask has one, so a cycle can close with open asks. A default never creates a task or backlog item; if the default would need work, the default is do nothing. |
| `blocks` | Task names, `merge:<task>`, `next-plan`, `archive`. Empty means nothing waits on this. Decides "settle now" versus "can wait" and the mid-execute routing in `flows/execute.md`. |
| `status` | `open` → `answered` (user confirmed the readback) → `applied` (`applied_via` set). `deferred` and `declined` carry the user's words in `decision`. `superseded` points at the newer ask in `revised_from`'s counterpart. |
| `decision_readback` | The decision restated as a consequence, in the words the user said yes to. Copied verbatim into task-update stanzas. |
| `revised_from` | On a reversal, the id of the ask whose decision this one replaces. The old record becomes `superseded`. |

**Plain-language rule.** Any term in `question`, `about` or `options` that does not appear in the change's `proposal.md` is explained in `about`, which the rendering always places directly under the question; the question itself may use the term unglossed. If that takes more than three terms, the ask is not plain yet: rewrite it. Process vocabulary (register, tier, speculated, reconciled, scaffold, on-touch, load-bearing, disposition, and the `arch-` / `disc-` / `eoc-` id families) never appears in these three fields.

## Kinds and routing

| Kind | What it is | What happens |
|---|---|---|
| `decision` | Only the user can choose, and the choice changes the product | Presented in `asks.md` and in chat. The only kind that becomes a question. |
| `doc-correction` | The code is right and a document is stale | Applied inline, listed under "Applied without asking" with the path. The user can object. Never a question. |
| `confirmation` | A default already applied; an obsolescence flag on a task; a PM action the PM may take alone (an Architect audit, a re-rank); a prior recorded from the user's words (`overlay.md` § Priors) | Same list. Never a question. |
| `environment` | A licence, a wedged container, a permission, a missing or unreadable `state.json` or handshake | Asked at once, alone, in block form. The only kind that may bypass `state.json`. |
| `process` | About the orchestration itself: the register lifecycle, the state file, whether to track `.orchestrator/` | Recorded with `status: open`; not presented. The digest says "N process notes in `<path>`". The user pulls them into the process-improvement workspace when reviewing the process. |

Triage happens when the orchestrator writes the record, not when the digest is rendered. A finding routed "to the user" by a role brief is a candidate; the kind decides whether it is asked. One finding can yield two records, each with one kind: the stale sentence is a `doc-correction` applied now, and the product question it exposed is a separate `decision`. The record shows this pairing is the common case.

## Rendering: `.orchestrator/cycles/<cycle-id>/asks.md`

Written once from the records at integrate's presentation step (`flows/integrate.md` § 3a) and appended to at readback. Plan-time and execute-time asks that were presented earlier are appended when presented, so this file is the cycle's complete ask trail.

```markdown
# Asks — change: <change-name> — cycle <n>

## What this change is about

<Two or three short paragraphs for the reader named in the overlay: what the change
does in product terms, what shipped this cycle, and one paragraph defining any term the
asks below cannot avoid. From the two rewrites the record shows worked: lead with the
product, not the phase.>

## Applied without asking

- <path>: <one line, what was corrected and why the code is taken as right>. Object if the document was right.
- <task-name>: <default applied>.

## Triage

| # | Question | Blocks | Recommendation |
|---|---|---|---|
| 1 | <question> | <task or boundary> | <option label> |
| 2 | <question> | — | <option label> |

Blocking asks first. Process notes: N (in `state.json` `asks_for_user`, kind `process`).

## Settle now

### 1. <question>

<`about`.>

<What happened, or what the reasoning is, and what you would notice. One paragraph.>

- **<label>** — <consequence>.
- **<label>** — <consequence>.
- **Do nothing** — <consequence>.

Recommendation: **<label>**. <why>. This has happened: <evidence> | This has not happened; the reasoning is <evidence>.
If unanswered: <default>.

## Can wait

- **2.** <question> Default: <default>. Ask for the full version if you want it.

## Decisions

<Appended at readback. One line per ask: "<id>: <decision_readback>. Confirmed <date>. Applied via <applied_via>.">
```

A "settle now" block is one whose `blocks` is non-empty, or whose default would be wrong to apply silently. Everything else is a "can wait" line; the user can ask for any of them in full.

## The chat message

The orchestrator's message to the user is the "What this change is about" section, the "Settle now" blocks in full, the "Can wait" lines, and the path to `asks.md`. **Asks come first**; phase results follow, or go in a separate message. The record shows the user answered when the text was in front of them and skipped a third of the questions put through the tool form alone, so the text is pasted, not linked.

A merge-blocking ask raised mid-execute (`flows/execute.md`) is one block in this shape, sent alone, and appended to `asks.md`.

## Readback

When the user answers, the orchestrator sends one message restating every decision as a consequence:

> 1. Hand-fetched videos will not be added to the archive; the docs will say so.
> 2. …
>
> Say yes, or correct any line.

On yes, each record's `decision_readback` is set to that line, `status` becomes `answered`, and the Decisions section of `asks.md` gains the line. Only then is the decision applied and `status` set to `applied` with `applied_via`. A correction at readback edits the record in place; a later reversal creates a new record with `revised_from` and marks the old one `superseded`. A reply that states a general rule rather than a choice is also appended, in the user's words, to the overlay's `priors.md` (`overlay.md` § Priors), with a `confirmation` record listed under "Applied without asking".

## What a finding must carry

A role that routes a finding to the user (`roles/reviewer.md` `route-to-user`, `roles/architect.md` interface-drift against a stale doc, `roles/implementor.md` stop-and-ask) writes, in the finding, the three things the orchestrator cannot infer from the diff:

- what the feature, flag or rule is for (a sentence; the orchestrator rewrites it for the reader),
- its `observed` block (every finding now carries one; `templates/architect-finding.md` § Observed),
- what it blocks: a task, a merge, the next plan, the archive, or nothing.

A finding routed to the user without these is not asked as it stands. When the gap is a matter of reading (the flag's help text, the doc the finding cites, whether the ledger has entries), the orchestrator fills it; when it is a matter of redoing the analysis, the finding goes back to the role. PM stubs are always completed by the orchestrator, since the deterministic pass has no LLM.

## Blocking without a second artifact

An ask that blocks tasks does not get a disposition task. The orchestrator sets `status: blocked` and `blocker_note: <ask-id>` on each task named in `blocks` (the field `state.md` already defines for user decisions). The PM's stale and blocked-path queries read `blocker_note` and re-surface the existing ask id; they do not raise a new ask for a decision that is already open. When the ask is applied, the note is cleared and the task returns to `ready`.

The alternative, a disposition task whose body is one paragraph and a pointer to `asks.md#ask-<id>`, was considered and left out because it writes the same text a second time; restore it if a real cycle shows the task tree needs the placeholder.

## Worked examples

Three asks from the record, each shown as it was asked and as this template would ask it.

### 1. A decision that was reversed within eleven minutes

As asked (Project A, cycle 3, 2026-09-23 17:47, `AskUserQuestion`):

> Audit finding (blocking, verified): `--download-video` calls the downloader directly and never registers, so fetching a `dead` video by hand does NOT release its ledger line (and never indexes the file). The register/spec/docs text will be corrected either way. Do you also want the code fixed?
> Options: Text fix + .tasks (Recommended) / Text fix + in-change task / Text fix only.

The user chose the in-change code fix, a critical-path task was created, and eleven minutes later the user reversed it: the flag is for one-off downloads that are not part of the archive. The question never said what the flag was for.

As this template asks it:

```json
{
  "id": "ask-cycle-1790026644-9",
  "kind": "decision",
  "raised_by": { "role": "architect", "ref": "arch-cycle-1790026644-1" },
  "question": "Should a video you fetch by hand with --download-video be added to the archive?",
  "about": "--download-video fetches one YouTube video by URL; it is not tied to any Reddit post. The archive is the set of videos the scraper has indexed from posts; each has a record, and a video YouTube has removed gets a 'dead' mark so scrape runs stop retrying it.",
  "observed": { "happened": true, "evidence": "The register, the spec and the docs all say a hand fetch clears the dead mark. The code does not: it calls the downloader and never touches the index. Verified against the code at the cycle-3 audit." },
  "options": [
    { "label": "Fix the docs and the code", "consequence": "Every hand fetch is indexed into the archive and clears the dead mark. Videos you fetch for reasons unrelated to the archive are indexed too." },
    { "label": "Do nothing to the code", "consequence": "--download-video stays a plain fetch: the file lands on disk and the archive does not know about it. The docs are corrected to say so." }
  ],
  "recommendation": { "option": "Do nothing to the code", "why": "What has been observed is a false sentence in the docs; nothing has been observed to need hand fetches in the archive." },
  "default_if_unanswered": "Docs corrected at this integrate; code untouched.",
  "blocks": [],
  "status": "open"
}
```

Rendered, it is a "can wait" line with the do-nothing default. With `about` in front of them, the user would have recognised the one-off use at once; the reversal turned on exactly that sentence.

### 2. A doc correction that was asked seven times

As asked (Project B, cycle 1 plan, seven asks of this shape, each with the recommendation "apply"):

> ask-cycle-1789334430-1 — design.md D4 numbers the removed scaffold stages "4–6"; the live pipeline (`config/workspaces/scaffold.org:157-224`) has five stages and the removed ones are 3–5. Doc fix; register entry `workspace-scaffold-pipeline` already records the correct numbering.

As this template handles it: `kind: doc-correction`, `observed.happened: true` (the code is the evidence), `status: applied`, `applied_via: design.md D4, edited at plan`. It is never a question. In `asks.md` it is one line:

> - `design.md` D4: stage numbers corrected from 4–6 to 3–5 to match the code (`scaffold.org:157-224`). Object if the document was right.

This is the user's standing rule of 2026-09-26, which until now lived in one task file's `## Observations`.

### 3. A question about something that has not happened

As asked (Project B, cycle 2, design question OD-3, 2026-09-24):

> Should stamps carry a format version? If what relpath means ever changes, unversioned stamps could not be told apart; cheap to fix now and impossible once real stamps exist.

The user answered: "Let's punt on this decision, I want to use this a while before we decide on polish like this."

As this template asks it:

```json
{
  "id": "ask-cycle-1789935943-3",
  "kind": "decision",
  "raised_by": { "role": "architect", "ref": "forward-mode cycle-1789935943" },
  "question": "Should each bookmark stamp carry a format version number from the start?",
  "about": "A stamp is the small set of properties written into a bookmark so it can be found again after its repository moves: which repository, and the file's path inside it. Every bookmark the feature touches gets one.",
  "observed": { "happened": false, "evidence": "reasoning only: if the meaning of the path property ever changed, old stamps could not be told from new ones. No stamp has been written yet and no change of meaning is planned." },
  "options": [
    { "label": "Add a version property now", "consequence": "Every stamp carries one more property that nothing reads today." },
    { "label": "Do nothing", "consequence": "Stamps have two properties. If the meaning ever changes, a one-off migration rewrites them; old stamps are recognisable by the missing version." }
  ],
  "recommendation": { "option": "Do nothing", "why": "Nothing has happened that a version would have helped with, and the migration cost, if it ever comes, is bounded." },
  "default_if_unanswered": "No version property. Revisit only if the stamp's meaning changes.",
  "blocks": [],
  "status": "open"
}
```

Rendered, it is a "can wait" line whose default is the answer the user gave. Under the plain-language rule it needed one gloss (stamp), given in `about`.

## What this template does not do

- It does not replace the finding templates. A finding is the evidence; the ask is the question. The finding keeps its severity, locations and `why_tests_missed`; the ask carries none of them.
- It does not put the record in the digest. The digest carries the triage table and the path to `asks.md` (`templates/pm-digest.md`).
- It does not create disposition tasks (see "Blocking without a second artifact").
- It does not count ask quality. Answers outside the offered options, reversals and clarification requests are visible in the records; counting them as a PM signal is deferred until a cycle shows the need.

## Measurement

`findings/tools/ask-inventory.py` in the process-improvement workspace reads every handshake's ask records and keys on `question`, `options`, `recommendation` and `raised_by`. It is the before/after measure for this template: the 2026-09-26 baseline is 62 asks, 14 with options, 26 with a recommendation, 39 with no source.
