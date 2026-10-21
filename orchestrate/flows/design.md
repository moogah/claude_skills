# Design round

The step between a change's spec edits and its first cycle. The Architect reads the scenarios the change touched and the code as it stands, writes the seams that will satisfy them into `docs/changes/<change>/design.org`, and writes the pending tests those scenarios and seams imply. Plan (`flows/plan.md`) drafts tasks from this document and the pending scenarios; integrate (`flows/integrate.md` § 1) checks merged code against it.

The round is not a cycle. It runs before `state.py init --first-cycle` and needs no state file. Its outputs are files in the code root: `design.org`, the pending tests, and a `:TEST: none:` property on each scenario no test is worth writing for. Like a phase, it runs in a phase agent the dispatcher spawns (`SKILL.md` § Two contexts); that agent spawns the Architect, checks and commits its output, and hands back one line.

## When it runs

- Once per change, after its `intent.org` and spec edits are committed in the code root (`templates/intent.md`, `templates/spec.md`), by the user or by opening (`flows/open.md`). The round writes `design.org` whole (Context, Decisions, Seams), or completes one the user wrote. The round is the one read of HEAD on the design side. The orchestrator does not write or verify a design before it, itself or through another agent: when it opens or splits a change it writes the intent and the spec edits, and the round writes Context, Decisions and the rest with the seams. A design written outside the skill (the user's own session) is read against HEAD as it stands, however recent; nothing tells whether its author checked. The record: an agent wrote a split-out change's design and read the same fifteen code files the round re-read a minute later; in another change the orchestrator drafted seven Decisions in four minutes with no code commit after them, and the round still rewrote four, one of them around a symbol that never existed.
- When the change has a workspace integration branch (`change.json`'s `from`; `research` § Hand-off) that has moved since `#+BASE:` (another change of the hand-off merged into it), the round takes it in first: `git -C "$CODE_ROOT" merge --no-edit <that branch>`; a conflict in `docs/specs/` or `docs/glossary.org` is reconciled by hand as a record correction (the later change keeps its own edits and takes the earlier change's), committed `Take in <branch>: <change>`, and `#+BASE:` is left as it was (the whole-change review's diff still covers the change's own work). A conflict in code before any task ran is an `environment` ask.
- Again only when a `decision` the user made, or a spec change, alters a seam. The re-run is scoped to that seam: amend its row, its scenarios' `:TEST:` properties and its pending tests; leave the rest. Inside an open cycle a re-run's asks and findings go through state as usual (`record add asks`, `record add findings`); the round itself still writes only `design.org`, those properties and tests.

A change with no scenario that needs a seam (a doc change, a config tweak) skips the round; say so in one line at the top of `design.org`'s Seams section.

## Operations

### 1. Spawn the Architect

The orchestrator assembles the brief (`roles/architect.md` § Design round): `intent.org` and its `#+BASE:`, the change's spec edits (`git -C "$CODE_ROOT" diff <base> -- docs/specs/`) and the spec files they touch, the output of `coverage.py --since <base>` (the change's scenarios and the tests already citing them) and of `coverage.py --scenario` for each scenario the intent's Capabilities names as stated but not met (`flows/open.md` § 3; these are the change's too), the intent's `* Open Questions` if any, the existing `design.org` if any, the overlay's `roles/architect.md`, `priors.md`, the test command and the test tree's layout, and `test.pending-style` from the overlay. The Architect works in the code root, the change's worktree, not a task worktree: it writes `design.org`, the pending tests and the `:TEST:` properties of § 3, and nothing else.

### 2. Seams

For each scenario, the Architect names the seam that satisfies it: a boundary the code must honour, stated in one sentence, with its owning symbols and the test that pins it. One row per seam in `design.org` § Seams (`templates/design.md`). Rules:

- A seam is a contract between two parts of the code, not a feature. "One writer for the ledger file; the heal pass is its only reader" is a seam. "Dead videos are skipped" is a scenario.
- Symbols, never line numbers. A row that cannot name a symbol at HEAD names the symbol the design introduces. Owning symbols are the boundary's entry points, not every helper.
- A row states the contract, never the implementation: no guard, error string, encoding, helper name, test name or transient marker (`templates/design.md` § Seams). The same binds the Decisions the round writes. A Decision that replaces a mechanism names what the old one provided and whether the new one keeps it, each kept or dropped property as a row or as a Removals sentence in the intent; the record: reload and per-file error isolation left with `jf/load-module` unstated, and no Reviewer had them in scope.
- A row holds current state only. No status, history, importance flags, dated notes or messages to future roles. The file's `git log` is the history.
- Existing design decisions that name a seam ("D1: two advice seams") link to the row by id.
- Seams, options and generality take the YAGNI check (`templates/architect-finding.md` § The YAGNI check): each serves a scenario the change's goals need, or has a demonstrated reason. A second implementation, a configuration point or an extension hook for a case nobody raised is not designed; the Architect names it in one `thought:` line in its report, which the phase agent copies into the round's commit message.
- The design checklist (`roles/architect.md` § Design checklist) is applied to the finished table: one producer per shape, one mapping per vocabulary, one canonical translation per boundary, every invariant with a test or a stated reason for none.

### 3. How each scenario is tested

For each scenario the change touched (`coverage.py --since <base>`), the Architect decides how it is tested:

- **`acceptance`** when a test can drive the scenario through a surface the user or an operator would (a CLI entry point on a temp directory, a compose service, `emacs --batch` against the tangled file, a real `git`) and assert on what that surface shows or leaves behind (its output, its exit code, the file it wrote). These need no seam to write and survive design changes. The overlay's `roles/architect.md` says what surfaces the project has.
- **`contract`** when the scenario has no such surface and a seam's test stands in for it.
- When a plain test already pins the scenario at HEAD, cite it; its kind is whatever it drives. Do not add a second test to change the label.
- **`none`** when neither is worth writing: no driveable surface and no seam whose test would pin the behaviour, or a cost stated (a fixture that takes longer than the feature). The scenario gets `:TEST: none: <reason>` in the spec (`templates/spec.md`), naming the seam that stands in when one exists. Behaviour-driven testing is not always an option; the property says where it is not, once, so no later role re-derives it.

The record of each choice is the test's own citation, or that property; there is no table to keep in step.

### 4. Pending tests

For every `acceptance` and `contract` row whose behaviour HEAD does not yet satisfy, the Architect writes the test now, in the test tree:

- In the file that already tests the owning module (for an acceptance test, the module the surface lives in); a new file only when none exists. An existing test that already pins a scenario may gain the scenario citation in its docstring or header; nothing else in it changes.
- Citing its scenario in the form `templates/spec.md` § A scenario's test gives (for Buttercup, `(Scenario: <capability> § <title>)` at the end of the `it` string, so the run output carries it); a contract test also cites `seam/<name>`.
- Marked pending with the overlay's `test.pending-style` (`xfail-strict` for pytest, `xit` for Buttercup, or the project's value). The style must be one the runner reports as pending, never as failing: a red baseline would disable the post-merge regression check for every cycle of the change.
- **Only for behaviour that does not hold at HEAD.** A scenario HEAD already satisfies gets a plain test that passes today; a strict pending marker on it would fail on day one.
- The test drives real code; it does not assert module ownership or the absence of a symbol.

`coverage.py` then lists the scenario as `pending`. Promotion is removing the marker, which the Implementor does in the commit that makes the test pass (`roles/implementor.md`); a task's Verification section lists the pending tests it promotes.

### 5. Open questions

A question only the user can settle (which of two seams; whether a scenario is in scope; a default the product should have) is a `decision`, and **every one the round raises is asked before plan starts, in one sitting**. A question earns its place by the YAGNI check: an intent goal or a scenario the change touches needs its answer. A question about a case nobody raised is not asked; it is a `thought:` line like any other speculative need. None waits, none is deferred to a later cycle, and nothing is drafted on a provisional answer. The Architect writes each into `design.org` § Open Questions (created when the file has none): the question in plain terms, two to four options with the consequence the user would notice, its recommendation and why, and the seam rows and pending tests that depend on it. It drafts those rows and tests in the form the recommendation implies and says so in the entry; it writes **no Decision** for the question. Each entry reaches the user as a `decision` ask file, like every question (`SKILL.md` § Critical requirements), with `blocks: [plan]` and `ref: design.org § Open Questions`. The file needs a cycle to index it: inside an open cycle (a scoped re-run) the round's phase agent writes the files; before the first cycle it hands back `design-round <change>: stopped, <n> open questions; design.org § Open Questions`, the dispatcher spawns plan, and the plan agent opens the cycle and writes them (`flows/plan.md` § 2). Either way the agent hands back `stopped` naming the asks, and the dispatcher asks each, one question per call, while the user is present. The resumed agent writes each answer into § Decisions and into the spec it settles, in place (`templates/ask.md` § Answer protocol), applies the ask (`applied_via=design.org D<n>`), and re-runs the round scoped to any seam whose answer differs from the recommendation (§ When it runs). When the user is absent, the change waits: the files stay in the inbox, the user answers there or in the next session's calls, and plan drafts nothing. A design-round question has no `If unanswered` default.

The intent's `* Open Questions` (written when the change was opened, `flows/open.md` § 4) join the same sitting: the Architect copies each into § Open Questions as written, marked `(intent)`, with the seam rows and pending tests its recommendation implies, and they are asked and answered like the round's own. An intent question's answer is written into the spec and the intent it settles, its entry is deleted from the intent, and it gets a Decision only when it moves a seam.

An intent with no `#+APPROVED:` line (`templates/intent.md`) joins the sitting as the first entry of all, `(intent) Goals`: the Goals and Non-goals as written, the options "approve as written" or a free-text answer that changes them; on approval the line is written with the date, on a change the sections are rewritten from the answer and the line written then. Plan does not run on an unapproved intent. Then the intent's `* New terms` not marked `(agreed <date>)` join the sitting as one more entry, `(intent) New terms`, asked next: each term with what it is and its example, the options "agree all as written" or a free-text answer that renames or rejects some. On the answer each agreed term is written into `docs/glossary.org` (`templates/glossary.md`; created with the first term) and marked agreed in the intent; a renamed term is renamed in the intent and the spec edits; a rejected one is replaced by the plain phrase the user gives. One commit in the code root, with the ask's application.

The record shows why. In one change the four questions asked at the round held; in the next the Architect wrote provisional decisions for five, two cycles built on them, the user answered sixteen hours later, two of the five were reversed, and a cycle of its own was spent on the rework (`ws-agent-tools-ask-answers`). On the eight decisions of those changes the user chose the recommended option every time, yet the old lean default (recommend doing nothing unless a problem had been observed) would have chosen the opposite on exactly the two later reversed; the system does not decide these.

### 6. Commit

The Architect reports the files it wrote. The orchestrator reads `design.org` against the checklist and runs `coverage.py --since <base>` (no `uncited` scenario among the change's; `pending` is expected), runs `build.pre-commit` when the overlay defines it and the test command once, both in the code root, to confirm the baseline is green with the pending tests reported as pending (or, where the project's baseline is red before the change, that the overlay defines `test.compare` and the failing set is unchanged; `flows/execute.md` § 4), stages the reported files and commits `Design round: <change>` (or `Design round (<seam-id>): <change>` for a scoped re-run) in the code root: `design.org`, the spec properties and the tests in one commit. The Architect does not commit.

## Inputs

- `intent.org`, the change's spec edits since its `#+BASE:`, `coverage.py --since <base>`, `design.org` when one exists, HEAD, the test tree.
- The overlay: `test.command`, `test.pending-style`, `build.pre-commit`, `roles/architect.md`, `priors.md`.

## Exit

No gate; the round is complete when `design.org` has a Seams table, `coverage.py --since <base>` shows no `uncited` scenario among the change's (every one is cited by a test that exists, pending or not, or carries `:TEST: none:`), every § Open Questions entry has its Decision (or, for an `(intent)` entry, its answer in the spec and the intent, which then has no `* Open Questions`), and the baseline run is green. The phase agent records the `coverage.py` summary line in its commit message. The phase agent hands back `design-round <change>: gate passed` only then; `state.py init --first-cycle` follows. With entries still open it hands back `stopped` (§ 5), and plan opens the cycle to ask them.

## What the round does not do

- It does not write tasks. Plan § 2 drafts them from the seams and the pending scenarios.
- It does not implement anything. A pending test's body drives real code; the code that makes it pass is the Implementor's.
- It does not maintain a project-wide catalogue. One `design.org` per change; a seam two changes share is the restore condition for a project-level index, not a reason to build one now.
