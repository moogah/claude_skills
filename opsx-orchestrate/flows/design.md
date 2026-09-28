# Design round

The step between specs and the first cycle. The Architect reads the spec scenarios and the code as it stands, writes the seams that will satisfy them into `design.md`, and writes the pending tests those scenarios and seams imply. Plan (`flows/plan.md`) drafts tasks from this document; integrate (`flows/integrate.md` § 1) checks merged code against it.

The round is not a cycle. It runs before `state.py init --first-cycle` and needs no state file. Its outputs are files in the repository.

## When it runs

- Once per change, after `specs/` exists (OpenSpec's `proposal → specs → design → tasks`; the round appends to the `design.md` that chain produces, or writes the OpenSpec sections first when the change has none).
- Again only when a `decision` the user made, or a spec change, alters a seam. The re-run is scoped to that seam: amend its row, its coverage rows and its pending tests; leave the rest. Inside an open cycle a re-run's asks and findings go through state as usual (`record add asks`, `record add findings`); the round itself still writes only `design.md` and tests.

A change with no scenario that needs a seam (a doc change, a config tweak) skips the round; say so in one line at the top of `design.md`'s Seams section.

## Operations

### 1. Spawn the Architect

The orchestrator assembles the brief (`roles/architect.md` § Design round): `proposal.md`, every `specs/*/spec.md`, the existing `design.md`, the overlay's `roles/architect.md`, `priors.md`, the test command and the test tree's layout, and `test.pending-style` from the overlay. The Architect works in the main checkout, not a worktree, and writes only `design.md` and test files.

### 2. Seams

For each scenario, the Architect names the seam that satisfies it: a boundary the code must honour, stated in one sentence, with its owning symbols and the test that pins it. One row per seam in `design.md` § Seams (`templates/design.md`). Rules:

- A seam is a contract between two parts of the code, not a feature. "One writer for the ledger file; the heal pass is its only reader" is a seam. "Dead videos are skipped" is a scenario.
- Symbols, never line numbers. A row that cannot name a symbol at HEAD names the symbol the design introduces.
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

A question only the user can settle (which of two seams; whether a scenario is in scope) is a `decision`. It is asked now when a seam row or a pending test depends on the answer; otherwise it can wait. Outside a cycle there is no state file to hold an ask record, so one asked now is put like an `environment` ask (`templates/ask.md`): the Settle-now block, at once; the question and the answer are then written into `design.md` § Open Questions (created when the file has none) and § Decisions. No record, no finding. One that can wait is written into § Open Questions and raised as an ask record at plan § 3.

### 6. Commit

The Architect reports the files it wrote. The orchestrator reads `design.md`'s two sections against the checklist, runs `build.pre-commit` when the overlay defines it, runs the test command once to confirm the baseline is green with the pending tests reported as pending, stages the reported files and commits: `Design round: <change>` (or `Design round (<seam-id>): <change>` for a scoped re-run). The Architect does not commit.

## Inputs

- `proposal.md`, `specs/`, `design.md` (OpenSpec's sections), HEAD, the test tree.
- The overlay: `test.command`, `test.pending-style`, `build.pre-commit`, `roles/architect.md`, `priors.md`.

## Exit

No gate; the round is complete when `design.md` has a Seams table and a coverage row for every scenario in `specs/`, every `acceptance` and `contract` row names a test that exists, and the baseline run is green. `state.py init --first-cycle` follows.

## What the round does not do

- It does not write tasks. Plan § 2 drafts them from the seams and coverage rows.
- It does not implement anything. A pending test's body drives real code; the code that makes it pass is the Implementor's.
- It does not maintain a project-wide catalogue. One `design.md` per change; a seam two changes share is the restore condition for a project-level index, not a reason to build one now.
