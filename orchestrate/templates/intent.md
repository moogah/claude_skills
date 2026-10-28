# Intent: what a change is for

A change's intent is `docs/changes/<change>/intent.org` in the code root. It holds what the diff cannot carry: why the change exists, what it is for and not for, which capabilities it touches, and why it removes what it removes. The requirement and scenario edits are not here; they are the change's edits to `docs/specs/` (`templates/spec.md`), and `git diff <base>` is their record. When the change is done the file stays where it is, its status line saying so.

The intent is written with the change's spec edits, before the design round: by the user, or by the orchestrator when it opens or splits a change (`flows/open.md`). It is committed in the code root, the change's worktree, on the change integration branch, the one its tasks merge into.

## Form

```org
#+TITLE: <change>
#+STATUS: speculated
#+BASE: <the commit the change starts from, on its repository>
#+APPROVED: <date> <by the user, in the hand-off review or the design round's sitting>

* Why
<What prompted the change, in the user's terms.>

* Research
[[file:research/][research/]]: <what it holds: assumptions with their origin, failure modes, evidence>

* Goals
- <An outcome the change commits to.>

* Non-goals
- <What a reader might expect and the change does not do.>

* Capabilities
- workspaces: <what the change does to it>
- portable-bookmarks: <…>

* Out of scope
- <Work noticed and deliberately left, with where it went (a .tasks/ item, a later change).>

* Removals
- workspaces § <requirement or scenario title>: <why it goes; what a user who relied on it does instead>

* New terms
- *<term>*: <what it is, one or two sentences>. Example: <the user's own use>. Avoid: <rejected words>. <(agreed <date>)>

* Open Questions
** <The question in plain terms>
- <Option>: <the consequence the user would notice>
- <Option>: <…>
Recommendation: <option>, because <…>. The spec is written to it. Affects: workspaces § <title>.
```

- **`#+BASE:`** is written once, with the change's first record commit: the `base` of `change.json`, which `home.py open` records as the merge-base of the branch with its `--from` ref when there is one, else HEAD; a branch adopted with its hand-off commit already on it has the fork point as its base, not that commit, and `home.py open` refuses when the intent's line disagrees (the record: two bases, and `coverage.py --since` seeing 3 scenarios by one and 34 by the other). `coverage.py --intent docs/changes/<change>/intent.org` scopes coverage to the change's scenarios (those the diff from it touches, and those Capabilities names); the whole-change review diffs from it (`flows/integrate.md` § 9).
- **`#+APPROVED:`** is the user's approval of the Goals and Non-goals as written, dated. An intent the user wrote carries it from the start. One an agent wrote (a research hand-off, `flows/open.md`) gets it when the user approves it: in the research skill's hand-off review, or, absent, as the first entry of the design round's sitting (`flows/design.md` § 5). The round does not run plan on an unapproved intent. The record: five intents written by hand-off agents carried nineteen open questions and nine coined terms, and nothing had asked the user whether the goals were theirs.
- **Capabilities** names each spec file the change edits by its capability id (`templates/spec.md`).
- **Capabilities** may also name a scenario the spec already states and the code does not meet (`workspaces § <title>: stated, not met`); the round takes it as one of the change's (`flows/open.md` § 3). A change opened from a workspace integration branch names **every** scenario the hand-off's spec edits added or changed for it this way, one line each, since those edits lie before its base and the diff from it shows none of them (`~/.claude/skills/research/SKILL.md` § Hand-off); a parenthesis may follow (`stated, not met (changed)`). The lines are written from the spec text, not retyped: `coverage.py --intent` lists any that match no scenario, and a title renamed later is corrected here in the same commit, like a test's citation.
- **`* Research`** links the research that preceded the change: `docs/changes/<change>/research/`, or a home's research brief by path (`~/.claude/skills/research/SKILL.md` § Hand-off). The design round reads its assumptions and failure modes. Left out when no research preceded the change.
- **`* New terms`** lists each domain term the intent or the spec edits use that `docs/glossary.org` does not have (`templates/glossary.md`): the term, what it is, an example from the user's own use, and words to avoid. Agents coined almost every term that later confused the user, in documents the user had approved, and explained them in chat during execution; this list is where they are agreed instead. A term agreed during research carries `(agreed <date>)`; the rest are agreed in the design round's sitting (`flows/design.md` § 5). An agreed term is written into `docs/glossary.org` in the same commit; the list stays as the change's record of the terms it introduced.
- **`* Open Questions`**, after New terms, holds what the source left to the user and the spec text depends on; each entry is deleted when the round's sitting answers it (`flows/open.md` § 4, `flows/design.md` § 5).
- **Removals** lists each requirement or scenario the change deletes from `docs/specs/`, with its reason: the diff shows that a scenario went, never why. Empty sections are left out.
- **Goals** and **Non-goals** are the stated outcome: plan's `on_critical_path` and integrate's goal-drift check measure against them. The uses the Goals name, worked end to end, are the change's **main path**: what the MVP must do (`templates/hardening.md`), which tasks hold the MVP decision back (`flows/plan.md` § 6), and what decides whether a defect or a user's decision is worked before the MVP decision or goes on the hardening list (`templates/architect-finding.md` § Routing). A goal that cares about an error or an input says so, since only what a goal names is on the main path.

## The status line

The intent is the change's **outermost speculation**. Like the seam rows and Decisions in `design.org` it is written before implementation and checked against what implementation reveals; unlike them it carries a status, because its lifecycle is a user decision rather than a conformance check. Treating the stated outcome as immutable once written is what produces "ship the wrong thing on time".

| `#+STATUS:` | Meaning |
|---|---|
| `speculated` | Fresh; the forward chain has not yet stabilised against it |
| `divergent <date> <ask-id>` | A goal-drift signal is open; the ask carries revise / split / abandon / continue |
| `reconciled <date> <choice>` | The user answered the goal-drift ask with `<choice>` |
| `mvp <date> <commit> <ask-id>` | The main path works end to end and the MVP decision (harden / rethink / stop) is with the user; `<commit>` is the code root's HEAD then |
| `done <date> <commit>` | The user chose to stop at the MVP, or the hardening they chose is finished; the whole-change review found nothing blocking and no open task remains other than those the MVP answer left; `<commit>` is the code root's HEAD then |

What a status needs beyond its line goes in a `* Status` section at the end of the file, newest first: for `divergent`, the signal (`pm-goal-drift`, `reviewer-spec-signal`, `architect-conformance` or `user`) and one paragraph of evidence; for `reconciled`, the prior outcome statement verbatim and one paragraph on what changed and why; for `done`, the closing summary (`templates/hardening.md` § The closing summary). Older text is in `git log -- docs/changes/<change>/intent.org`.

## Lifecycle

- **`→ divergent`**: the user takes the goal-drift recommendation the handshake carries (`flows/integrate.md` § 5); integrate writes the line and the `* Status` entry and raises the `decision` ask with the four options.
- **`divergent → reconciled`**: the user's answer in the ask file's `* Answer`; the handshake's `user_resolved_goal_drift` carries `{ask, decision, rationale}`. On `revise` or `split` the Goals are rewritten and the line returns to `speculated`, the prior outcome kept in `* Status`.
- **`→ mvp`**: integrate § 9, the first time the whole-change review leaves no blocking finding and no open task on the main path remains. The orchestrator writes the line and raises the MVP decision.
- **`mvp →`**: on the user's answer: `harden` keeps the line until the hardening is finished; `rethink` makes it `divergent <date> <ask-id>` (signal `user`) and the change is revised (`flows/open.md`); `stop` makes it `done`.
- **`→ done`**: integrate § 9, after `stop`, or after `harden` when the whole-change review again leaves no blocking finding and no open task remains other than those the answer left. The orchestrator writes the line with the closing summary and commits it in the code root with a subject naming the change.

Integrate writes the line in every case but one: an answer to the MVP decision applied at the next plan (`flows/plan.md` § 1) is written there. Execute never writes it. A file with no `#+STATUS:` line gets a warning at plan and nothing more.

## Cost asymmetry

Amending a seam row costs minutes, revising a Decision hours, revising the intent days. The system absorbs a discovery at the lowest level that can hold it, but must not hide the expensive option when it is warranted: the status line is what makes goal drift visible, where otherwise goal-invalidating signals are absorbed as row amendments until the gap is too big to ignore.
