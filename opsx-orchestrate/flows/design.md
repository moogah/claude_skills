# Design round

The step between specs and the first cycle. The Architect reads the spec scenarios and the code as it stands, writes the seams that will satisfy them into `design.md`, and writes the pending tests those scenarios and seams imply. Plan (`flows/plan.md`) drafts tasks from this document; integrate (`flows/integrate.md` § 1) checks merged code against it.

The round is not a cycle. It runs before `state.py init --first-cycle` and needs no state file. Its outputs are files in the home (`design.md`) and the code root (the pending tests). Like a phase, it runs in a phase agent the dispatcher spawns (`SKILL.md` § Two contexts); that agent spawns the Architect, checks and commits its output, and hands back one line.

## When it runs

- Once per change, after `specs/` exists (OpenSpec's `proposal → specs → design → tasks`; the round appends to the `design.md` that chain produces, or writes the OpenSpec sections first when the change has none).
- Again only when a `decision` the user made, or a spec change, alters a seam. The re-run is scoped to that seam: amend its row, its coverage rows and its pending tests; leave the rest. Inside an open cycle a re-run's asks and findings go through state as usual (`record add asks`, `record add findings`); the round itself still writes only `design.md` and tests.

A change with no scenario that needs a seam (a doc change, a config tweak) skips the round; say so in one line at the top of `design.md`'s Seams section.

## Operations

### 1. Spawn the Architect

The orchestrator assembles the brief (`roles/architect.md` § Design round): `proposal.md`, every `specs/*/spec.md`, the existing `design.md`, the overlay's `roles/architect.md`, `priors.md`, the test command and the test tree's layout, and `test.pending-style` from the overlay. The Architect works in the main checkout, not a worktree: it edits `design.md` at the home and writes the pending tests in the code root's main checkout, and writes nothing else.

### 2. Seams

For each scenario, the Architect names the seam that satisfies it: a boundary the code must honour, stated in one sentence, with its owning symbols and the test that pins it. One row per seam in `design.md` § Seams (`templates/design.md`). Rules:

- A seam is a contract between two parts of the code, not a feature. "One writer for the ledger file; the heal pass is its only reader" is a seam. "Dead videos are skipped" is a scenario.
- Symbols, never line numbers. A row that cannot name a symbol at HEAD names the symbol the design introduces. Owning symbols are the boundary's entry points, not every helper.
- A row states the contract, never the implementation: no guard, error string, encoding, helper name, test name or transient marker (`templates/design.md` § Seams). The same binds the Decisions the round writes.
- A row holds current state only. No status, history, importance flags, dated notes or messages to future roles. `git log -- design.md` is the history.
- Existing design decisions that name a seam ("D1: two advice seams") link to the row by id.
- The design checklist (`roles/architect.md` § Design checklist) is applied to the finished table: one producer per shape, one mapping per vocabulary, one canonical translation per boundary, every invariant with a coverage row.

### 3. Scenario coverage

One row per spec scenario in `design.md` § Scenario coverage, with `kind`:

- **`acceptance`** when a test can drive the scenario through a surface the user or an operator would (a CLI entry point on a temp directory, a compose service, `emacs --batch` against the tangled file, a real `git`) and assert on what that surface shows or leaves behind (its output, its exit code, the file it wrote). These need no seam to write and survive design changes. The overlay's `roles/architect.md` says what surfaces the project has.
- **`contract`** when the scenario has no such surface and a seam's test stands in for it.
- When a plain test already pins the scenario at HEAD, cite it; its kind is whatever it drives. Do not add a second test to change the label.
- **`none`** when neither is worth writing: no driveable surface and no seam whose test would pin the behaviour, or a cost the row states (a fixture that takes longer than the feature). A `none` row always carries the reason and, when one exists, the seam that stands in. Behaviour-driven testing is not always an option; the table says where it is not, once, so no later role re-derives it.

### 4. Pending tests

For every `acceptance` and `contract` row whose behaviour HEAD does not yet satisfy, the Architect writes the test now, in the test tree:

- In the file that already tests the owning module (for an acceptance test, the module the surface lives in); a new file only when none exists. An existing test that already pins a scenario may gain the scenario citation in its docstring or header; nothing else in it changes.
- Named by scenario in the project's convention: the docstring or header cites `Scenario: <capability> § <title>`; a contract test also cites `seam/<name>`.
- Marked pending with the overlay's `test.pending-style` (`xfail-strict` for pytest, `xit` for Buttercup, or the project's value). The style must be one the runner reports as pending, never as failing: a red baseline would disable the post-merge regression check for every cycle of the change.
- **Only for behaviour that does not hold at HEAD.** A scenario HEAD already satisfies gets a plain test that passes today; a strict pending marker on it would fail on day one.
- The test drives real code; it does not assert module ownership or the absence of a symbol.

The coverage row's `test` column names the test. Promotion is removing the marker, which the Implementor does in the commit that makes the test pass (`roles/implementor.md`); a task's Verification section lists the pending tests it promotes.

### 5. Open questions

A question only the user can settle (which of two seams; whether a scenario is in scope; a default the product should have) is a `decision`, and **every one the round raises is asked before plan starts, in one sitting**. None waits, none is deferred to a later cycle, and nothing is drafted on a provisional answer. The Architect writes each into `design.md` § Open Questions (created when the file has none): the question in plain terms, two to four options with the consequence the user would notice, its recommendation and why, and the seam rows and pending tests that depend on it. It drafts those rows and tests in the form the recommendation implies and says so in the entry; it writes **no Decision** for the question. Outside a cycle there is no state file to index an ask file, so the dispatcher carries the questions (`SKILL.md` § Two contexts): the phase agent hands back `stopped: <n> open questions; design.md § Open Questions`; the dispatcher asks each, one question per call, while the user is present; the phase agent is resumed with the answers and writes each into § Decisions, re-running the round scoped to any seam whose answer differs from the recommendation (§ When it runs). When the user is absent, the change waits: the questions stay in the file, the user answers there or in the next session's calls, and plan does not start (`flows/plan.md` § 2). A design-round question has no `If unanswered` default.

The record shows why. In one change the four questions asked at the round held; in the next the Architect wrote provisional decisions for five, two cycles built on them, the user answered sixteen hours later, two of the five were reversed, and a cycle of its own was spent on the rework (`ws-agent-tools-ask-answers`). On the eight decisions of those changes the user chose the recommended option every time, yet the lean default rule (`templates/ask.md` § Recommendation) would have chosen the opposite on exactly the two later reversed; the system does not decide these.

### 6. Commit

The Architect reports the files it wrote. The orchestrator reads `design.md`'s two sections against the checklist, runs `build.pre-commit` when the overlay defines it and the test command once, both in the code root, to confirm the baseline is green with the pending tests reported as pending, stages the reported files and commits `Design round: <change>` (or `Design round (<seam-id>): <change>` for a scoped re-run): in the code root for the tests and in the home for `design.md`, one commit when they are the same repo. The Architect does not commit.

## Inputs

- `proposal.md`, `specs/`, `design.md` (OpenSpec's sections), HEAD, the test tree.
- The overlay: `test.command`, `test.pending-style`, `build.pre-commit`, `roles/architect.md`, `priors.md`.

## Exit

No gate; the round is complete when `design.md` has a Seams table and a coverage row for every scenario in `specs/`, every `acceptance` and `contract` row names a test that exists, every § Open Questions entry has its Decision, and the baseline run is green. The phase agent hands back `design-round <change>: gate passed` only then; `state.py init --first-cycle` follows.

## What the round does not do

- It does not write tasks. Plan § 2 drafts them from the seams and coverage rows.
- It does not implement anything. A pending test's body drives real code; the code that makes it pass is the Implementor's.
- It does not maintain a project-wide catalogue. One `design.md` per change; a seam two changes share is the restore condition for a project-level index, not a reason to build one now.
