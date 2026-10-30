# Opening a change

The step that turns what the user asked for into a change the design round can take: `docs/changes/<change>/intent.org` (`templates/intent.md`) and the change's edits to `docs/specs/` (`templates/spec.md`), committed in the code root. Like the round, it is not a cycle and needs no state file; it runs in a phase agent the dispatcher spawns (`SKILL.md` § Two contexts) at `opus`, `medium` (`SKILL.md` § Agent settings: the intent and the spec edits are what every later role cites), which writes the record, commits it and hands back one line. The design round follows (`flows/design.md`).

## When it runs

- The user asks in the session for a feature, a fix or a change of behaviour. The source is their words.
- The user takes up `.tasks/` items, by naming them or by answering "open a new change" on a cluster ask (`externalisation.md` § Externalisation pressure). The source is the items.
- The user answers `split` on a goal-drift ask (`templates/intent.md` § Lifecycle). The source is the old intent and the answer; the new intent's Why names the change it came from.
- The user answers `rethink` on the MVP decision (`templates/hardening.md`) or `revise` on a goal-drift ask. The source is the intent and the answer, and the change is revised in place: the Goals, Non-goals and spec edits are rewritten from the answer, `#+BASE:` is kept, the prior Goals are quoted under `* Status` and the status line returns to `speculated`. The commit is `Revise: <change>`; the design round then re-runs for the seams the new goals move.

Not when the user wrote the intent and spec edits themselves: the round reads them as they are. Not for work a task in an open change covers.

## The brief

The dispatcher's brief is the fixed one (`SKILL.md` § The dispatcher's loop) with the source in place of the cycle:

> You are the phase agent for opening a change. Home: `<path>`. Source: `<the user's words, verbatim>` | `<.tasks/ paths>` | `<split ask id and its change>`. Run `state.py where`, then read and follow `<skill-dir>/flows/open.md` to its commit. …

The dispatcher copies the user's words; it reads nothing to write the brief.

## Operations

### 1. Read

The source; the `* Purpose` of every file under `docs/specs/`, to find the capabilities the source touches, and those files in full; `docs/glossary.org` if it exists; `priors.md`; the two templates. **Not the code.** The round is the design side's one read of HEAD (`flows/design.md` § When it runs); a claim the source makes about the code ("the layout switch autosaves without the predicate check") goes into the intent's Why as the source states it, and the round checks it.

### 2. Name, repository and base

- `<change>`: the user's name for it, or a kebab-case phrase naming the outcome (`workspace-state-writes-honour-guards`). `docs/changes/<change>/` must not exist in the repository, except when revising a change in place (§ When it runs), which keeps its name, worktree and base.
- The repository: the home's only one, or the one the source names (`--repo`); a change belongs to one repository (`overlay.md` § Repositories). A source that needs two repositories is two changes, the second opened after the first with its `* Why` naming it.
- `state.py change open <change> [--repo <name>] [--from <ref>]` makes the change's worktree on the branch `<change>`, its change integration branch, from the repository checkout's HEAD or the ref given (the hand-off's workspace integration branch when the research skill made one; `research` § Hand-off), and writes `change.json`. From here `eval "$(state.py where --sh)"` gives `$CODE_ROOT`, that worktree; the intent and the spec edits are written and committed there. A change the user opened by hand (its intent already on its own branch) is adopted with `--worktree <path>` and the rest of this flow is skipped, as § When it runs says.
- `#+BASE:`: the `base` in `change.json` (`git -C "$CODE_ROOT" rev-parse HEAD` at open), written before the commit of § 5.

### 3. Spec edits

In place, in the form `templates/spec.md` gives:

- A behaviour the change adds or alters is a requirement (`The <subject> SHALL …`) with scenarios that state what a user, an operator or a caller observes: the input and the output, the file left behind, the message shown. A scenario names no symbol, guard or encoding; those are the round's and the code's.
- A behaviour the source reports broken, where the spec already states it rightly, gets no spec edit: the intent's Capabilities line names the scenario (`workspaces § <title>: stated, not met`), and the round takes it as one of the change's.
- A new capability is a new file. A removal deletes the requirement or scenario and gets a line in the intent's Removals.
- Existing titles are kept: they are citation keys. A rename is the round's, in the commit that updates the citations.

### 4. Intent and open questions

The intent per `templates/intent.md`, `#+STATUS: speculated`: Why in the user's terms (quoting them when the source is their words), Goals, Non-goals, Capabilities, Removals, New terms.

Use the glossary's terms and the user's own words. Every domain term the intent or the spec edits use that the glossary does not have goes in `* New terms`, to be agreed in the round's sitting; a plain phrase is better than a new term where one will do.

A question the source leaves to the user and the spec text depends on (which of two behaviours is intended, whether something is in scope, a default the product should have) goes in the intent's `* Open Questions`; a question about a case the source never raised does not (`templates/architect-finding.md` § The YAGNI check). Each entry has the question in plain terms, two to four options with the consequence the user would notice, the recommendation and why, and the requirements and scenarios it affects. The spec is written in the form the recommendation implies, and the entry says so. Nothing about how to build it; that is the round's.

These are asked with the round's own questions, in its one sitting before plan (`flows/design.md` § 5), not here: one sitting per change. The record: the round's one question was asked in a single call and answered in twelve minutes, and the spec text written before that answer stayed wrong until the whole-change review caught it (bookmarks `cycle-1791996711`). The answer protocol now edits the spec in place (`templates/ask.md` § Answer protocol).

### 5. Commit

First `coverage.py --root "$CODE_ROOT" --intent docs/changes/<change>/intent.org`: it must parse the edited specs, list the change's scenarios and no name that matches none (`uncited` is expected; the round cites them). Then, in the code root, `intent.org` and the spec files, staged by name, in one commit `Open: <change>`, with the coverage summary line in its message. The home has nothing to commit; a `.tasks/` item taken up is named by path in the intent's Why and its file is left as it is.

## Hand-back

`open <change>: <c> capabilities, <s> scenarios, <q> open questions; docs/changes/<change>/intent.org` · `open: failed, <why>`

The dispatcher relays the line, so the user has the intent's path, and spawns the design round for `<change>`.

## What opening does not do

- It does not read the code or write a design. The round does both, once.
- It does not ask the user anything. Its questions wait for the round's sitting.
- It does not write tasks. Plan § 2 drafts them from the design.
