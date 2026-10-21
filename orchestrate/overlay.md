# Overlay convention

The core skill is project-agnostic. Each project carries its own overlay at `<home>/.claude/orchestrator/`, where the home is a workspace directory holding worktrees of the project's repositories, or a repository's own root. The overlay names the repositories and supplies each one's test command and build hooks, the worktree placement, role-prompt extensions, threshold tuning, and task taxonomy.

## Layout

```
<home>/.claude/orchestrator/
  config.yaml              # required if the directory exists
  roles/
    implementor.md         # OPTIONAL — appended to core implementor brief
    reviewer.md            # OPTIONAL — language idioms, test idioms
    architect.md           # OPTIONAL — acceptance surfaces, drift hot spots
    project-manager.md     # OPTIONAL — project-specific PM conventions
  priors.md                # OPTIONAL — the user's standing rules, one line each; appended to every role brief
  hooks/
    test-command.sh        # OPTIONAL — wraps the test runner
    pre-commit.sh          # OPTIONAL — tangle, codegen, format
    worktree-init.sh       # OPTIONAL — submodules, runtime setup
```

The directory is **flat under `.claude/`** — not nested under `.claude/skills/` — to signal that this is config the central skill consumes, not a self-contained skill. It is created by `state.py home init` (§ Bootstrap) and checked by `state.py where` (§ Drift).

## config.yaml schema

```yaml
# Required: the schema version this file was written against (§ Drift) and the project identity
overlay-version: 1
project: <slug>
language: elisp | go | typescript | python | rust | other

# The repositories (§ Repositories). A workspace home names each one; a change belongs to one of them
# and is worked in its own worktree of it. `path` is a checkout of the repository (any work tree of it),
# relative to the home or absolute; the per-repository build/test/worktree blocks override the top-level ones.
repos:
  <name>:
    path: <dir>
    language: elisp | go | typescript | python | rust | other   # OPTIONAL; default: the top-level language
    build: { … }        # OPTIONAL, same keys as below
    test: { … }         # OPTIONAL, same keys as below
    worktree: { … }     # OPTIONAL, same keys as below
# One-repository shorthands, instead of repos:
#   code-root: <dir>   # the repository checkout, relative to the home (a workspace holding one repository)
#   neither            # the home is the repository (a repository root as its own home)

# Required: build & test (top level: shared by every repository; per repository under repos.<name>)
build:
  pre-commit: ./bin/tangle-org.sh   # OPTIONAL
test:
  command: ./bin/run-tests.sh       # REQUIRED
  parser: emacs-results-txt | exit-code | junit-xml | tap   # default: exit-code
  compare: python3 ~/.claude/skills/orchestrate/scripts/compare.py --parser <parser>   # OPTIONAL; required when the baseline is red: `<cmd> <baseline-file> <after-file>` exits non-zero and prints the test ids that fail after but not before (flows/execute.md § 4)
  pending-style: xfail-strict | xit | <project value>   # how the design round marks a pending test; must be a style the runner reports as pending, never as failing (a red baseline disables regression detection). Default: xfail-strict for pytest, xit for buttercup

# Where worktrees go (§ Worktrees): under the home, never under a code root. `pattern` is the path under
# `parent` for a branch; it must contain <branch>, and <repo> when the home has several repositories.
worktrees:
  parent: .worktrees             # default: .worktrees, relative to the home
  pattern: <repo>/<branch>       # default: <repo>/<branch> for a workspace, <branch> for a repository root

# Optional: where the process and the record live (defaults shown); <change> is a placeholder
artifacts:
  tasks-root: .orchestrator/changes/<change>/tasks   # in-change tasks; relative to the home
  externalised-root: .tasks                          # cross-cutting backlog; relative to the home
  specs-root: docs/specs                             # the record, relative to the code root
  intent-pattern: docs/changes/<change>/intent.org   # relative to the code root
  design-pattern: docs/changes/<change>/design.org   # relative to the code root

# Required: worktree handling (per repository; top level is shared)
worktree:
  init: ./bin/init-worktree-runtime.sh   # OPTIONAL — runs after `git worktree add`, inside the new worktree
  needs-submodules: true                  # default: false

# OPTIONAL: task taxonomy — what classes are valid for this project
taxonomy:
  - feature
  - test
  - doc
  - refactor
  - bug
  - contract
  - infrastructure
  - tangle           # project-specific: literate-org tangling
  - load-test        # project-specific

# OPTIONAL: PM threshold tuning
thresholds:
  history-window: 5                        # K
  drainage-trigger-ratio: 1.0
  drainage-trigger-consecutive-cycles: 3
  cascade-trigger-followup-count: 3
  stale-task-cycles: 3
  review-starvation-ratio: 1.5
  phase-agent-context: 400000             # the dispatcher's: a phase agent past it hands over (SKILL.md)
  externalisation-pressure-growth: monotonic-3-cycles

# OPTIONAL: critical-path declaration
critical-path:
  inference: llm-read-proposal       # default; one read per plan phase
  override-tasks: []                   # explicit list, if known up front
  override-labels: []                  # task_class values that are always critical-path

# OPTIONAL: Architect extensions
architect:
  drift-hotspots:
    - <module name>: <one-line — what to watch>

# OPTIONAL: Reviewer language-specific extensions
reviewer:
  checks:
    - cl-lib-vs-seq
    - lexical-binding-headers
    - tangle-source-vs-tangled-output

# OPTIONAL: who reads the questions the orchestrator puts to the user (templates/ask.md)
asks:
  reader: "technical product manager who has not watched development and has a shallow view of the internals"   # default

# OPTIONAL: the user's standing rules (see § Priors); default: this path when the file exists
priors: priors.md
```

## Repositories

Three shapes of home, one model: a home has one or more **repositories**, and a **change belongs to exactly one** of them, worked in its own worktree of that repository on a branch named after the change (its change integration branch). That worktree is the change's **code root**. A feature that spans two repositories is two changes, sequenced by the hand-off that produced them.

| Shape | `config.yaml` | The repositories | Typical |
|---|---|---|---|
| **Workspace** (the usual case) | `repos:` | Each named entry's `path`, a checkout of that repository anywhere (`~/emacs`, or a worktree under the home); several repositories, and several worktrees of one, live side by side under the home | A workspace directory holding worktrees of a frontend monorepo and a backend service |
| **Workspace, one repository** | `code-root: <dir>` | That checkout, named after its directory | A workspace holding one repository's checkout |
| **Repository root** | neither | The home itself, named after its directory | A repository whose root carries the overlay, no workspace above it |

The same flows run in all three. `state.py where` prints the shape, the repositories and every open change (its repository, branch, worktree and phase); `state.py change open <name> [--repo <name>] [--from <ref>]` creates the change's worktree from a ref of the repository (default: the checkout's HEAD; the hand-off's workspace integration branch when there is one) and writes `<home>/.orchestrator/changes/<name>/change.json`; `--worktree <path>` adopts a checkout the user made by hand. A change's state, cycles, inbox, handshakes and test outputs live in that directory, so changes run cycles independently; the task files were always there.

## Worktrees

Worktrees are placed **from the home**, by `worktrees.parent` (relative to the home) and `worktrees.pattern` (the path under it for a branch, with `<repo>` and `<branch>` placeholders): a change's worktree is the pattern applied to its branch; a task's worktree (`flows/execute.md` § 1) is the pattern applied to the task branch, so it lands beside the change's, never inside it. The pattern must contain `<branch>`, and `<repo>` when the home has several repositories, since two of them may both have a `main`. The workspace's own tooling owns the convention; the overlay states it and the skill follows it (`state.py worktree <branch>` prints the path). The home's `.gitignore` ignores the parent, and any repository checkout nested in the home (`where` warns).

## Resolution rule

At skill invocation:

1. The orchestrator resolves the overlay by **walking up from `$cwd`** looking for `.claude/orchestrator/config.yaml`. The first hit wins; record its directory as the home (`$ORCH_HOME`). It holds the process, `.orchestrator/` and `.tasks/`; the record is under `docs/` in each change's code root.
2. If found, parse `config.yaml` (§ Validation says what is checked, and what is not).
3. Optional fields fall through to core defaults.
4. Role overlays are found by role name, `roles/<role>.md` (§ Layout; no key names them), and are read and **appended** to the corresponding core role briefs at agent-spawn time, followed by `priors.md` when it exists.
5. The change is resolved (`--change`, `$ORCH_CHANGE`, the worktree holding `$cwd`, or the only open one); its `change.json` names the repository and the worktree, which is the code root (`$CODE_ROOT`).
6. Hook scripts referenced by `build.pre-commit`, `test.command`, `worktree.init` are resolved relative to the code root and run there, or in a task worktree (§ Hook contract), with `ORCH_HOME`, `ORCH_CHANGE`, `ORCH_CHANGE_DIR`, `ORCH_REPO`, `ORCH_WORKTREES` and `CODE_ROOT` in their environment: an overlay never spells a relative path from a code root to the home (`../`), since the code root's depth under the home is the workspace's convention, not the skill's.

If no overlay is found, the orchestrator warns explicitly and falls back to:

| Default |
|---|
| repositories: the home itself |
| `test.command`: `make test` if Makefile present, else error |
| `test.parser`: `exit-code` |
| `test.compare`: none (the exit-code rule; an `environment` ask at the first red baseline) |
| `worktrees.parent`: `.worktrees` (relative to the home) |
| `worktrees.pattern`: `<repo>/<branch>` for a workspace, `<branch>` for a repository root |
| `worktree.needs-submodules`: false |
| `artifacts.tasks-root`: `.orchestrator/changes/<change>/tasks` (home) |
| `artifacts.externalised-root`: `.tasks` (home) |
| `artifacts.specs-root`: `docs/specs` (code root) |
| `artifacts.intent-pattern`: `docs/changes/<change>/intent.org` (code root) |
| `artifacts.design-pattern`: `docs/changes/<change>/design.org` (code root) |
| `test.pending-style`: `xfail-strict` (pytest) or `xit` (buttercup), else error when the design round needs it |
| `taxonomy`: `[feature, test, doc, refactor, bug, contract, infrastructure]` |
| `thresholds`: see `templates/pm-digest.md` defaults |
| `asks.reader`: "technical product manager who has not watched development and has a shallow view of the internals" |
| `priors`: none appended |

The fallback behaviour exists so the orchestrator can run on a project that hasn't been onboarded yet, but the warning is loud — running without an overlay is not the steady state.

## Why walk up rather than require the home

- Works from sub-directories without needing to `cd` to the home.
- Works from inside the code root: the walk-up passes through it to the home.
- Matches how editors and other tooling resolve project-local config (`.editorconfig`, `package.json`, `pyproject.toml`).

## Role overlay append semantics

Role overlay files are **appended**, not substituted. The core role brief is the spine; the overlay extends it. Example for elisp:

Core `roles/reviewer.md` contains the rigorous-not-contrarian / thoughtful-maintainer mindset.

Project overlay `<home>/.claude/orchestrator/roles/reviewer.md` adds:

```markdown
## Project-specific reviewer checks (elisp)

- Verify `lexical-binding: t` header on every new `.el` file.
- Flag `cl-lib` usage where `seq` would suffice (modern elisp idiom).
- For literate-org modules: verify the `.org` source was edited and tangled, not the `.el` directly.
- Flag `nconc` / `assq-delete-all` / shared-mutable-state patterns at module boundaries.
```

The orchestrator concatenates: core brief + `\n\n## Project-specific extensions\n\n` + overlay contents + `\n\n## Project priors\n\n` + `priors.md` contents (when present).

## Priors

`priors.md` holds the user's standing rules for the project, one line each, in the user's own words with the date and where they said it (a session id, or the ask id when the words came from an ask file):

```markdown
- 2026-09-20 session 618fe4f9: "It feels like overengineering to put in place checks in our code which only exist to make sure that we've done our job properly"
- 2026-09-20 session 89ac5a4d: "In general I'll favor getting this code cleaned up and removing leftovers from prior implementations"
- 2026-09-24 session c0e9e2a6: "I don't want to keep increasing scope"
- 2026-09-26 session c0e9e2a6: "Let's punt on this decision, I want to use this a while before we decide on polish like this"
```

**Write rule.** When a decision or a free-text reply states a general rule rather than a one-off choice ("in general", "I don't want to keep", "overengineering", "use it a while before"), the orchestrator appends the line and writes a `confirmation`-kind ask file citing it, applied, into `cycles/<cycle-id>/asks/` (`templates/ask.md`). The user can object. The line is the quote, not a paraphrase.

**Read rule.** The file is appended to every role brief after the role overlay, and the orchestrator reads it at plan § 1 and at integrate § 3a and § 7. A role may treat a prior as a user request: "removing leftovers" makes dead-code cleanup scope that passes the YAGNI check (`templates/architect-finding.md`) without an ask. A prior is never re-asked. A finding or a question that a prior already answers is handled as the prior says (a defect the user has accepted is `accepted-with-note` with the prior cited); no ask file is written for it, and one that already exists is `declined` with the prior as its readback. A prior is the user's own words and outranks a `design.org` sentence; a design sentence a prior contradicts is a record correction (`templates/design.md` § Seams).

The file is markdown, not a `config.yaml` list, because the spawn contract already concatenates markdown and a quoted sentence is safer there than in YAML.

## Hook contract

Hooks are bash scripts run by the orchestrator at fixed lifecycle points:

| Hook | When | Cwd | Failure semantics |
|---|---|---|---|
| `worktree.init` | After `git worktree add`, before agent spawn | inside the new worktree (beside the change's, under `worktrees.parent`) | Failure aborts task setup; status → failed |
| `build.pre-commit` | Before commit in a worktree (after agent finishes) | inside the worktree | Failure aborts commit; status → failed; worktree retained |
| `test.command` | After every merge to the change integration branch, and as the final implementor verification step | inside the code root or worktree (depending on phase) | Non-zero status indicates regression; baseline-vs-after comparison runs |
| `test.compare` | At a red baseline, after each merge (`flows/execute.md` § 4) | inside the code root | Non-zero: a test fails after that did not before |

Hooks must be idempotent. The orchestrator may re-run them on retry. Every hook sees `ORCH_HOME`, `ORCH_CHANGE`, `ORCH_CHANGE_DIR`, `ORCH_REPO`, `ORCH_WORKTREES` and `CODE_ROOT` (what `state.py where --sh` exports), so a script that needs the home writes `$ORCH_HOME/…`.

## Bootstrap

A home is made by `state.py home init`, never by copying another home's overlay: a copy carries the other project's knowledge and whatever the skill has since retired (the record: one overlay copied on 2026-10-15 brought an interfaces register, a `scaffolding.md` and a `roles-overlay` key the skill no longer had). The verb writes `config.yaml` from this schema, with `overlay-version`, the required keys filled and the optional blocks as comments, and an empty `roles/`; it refuses when a config exists.

```bash
state.py --home <dir> home init --repo <name>=<path> [--repo …] [--test-command <cmd>] [--parser <p>] [--worktrees-pattern <p>]   # a workspace
state.py --home <dir> home init --code-root <path> …      # a workspace holding one repository
state.py --home <dir> home init …                        # a repository root as its own home
```

The dispatcher runs it with the user present, in one sitting, one question per call (`templates/ask.md` § The inline rule applies to the session, not to a file: there is no change yet): the repositories and their checkouts; the test command and its parser; the worktree placement when the workspace has a convention (§ Worktrees); the pre-commit and worktree-init hooks; the pending-test style. Then the optional blocks the user wants, from what they know of the project: taxonomy, drift hot spots, reviewer checks, the asks' reader, priors. The result's `add_to_home_gitignore` lists what the home's `.gitignore` must ignore. Role overlays are written by hand, against the current `roles/*.md`, with only what this project adds. The home is done when `state.py where` reports no problems and no warnings; the first change follows with `state.py change open`.

## Drift

The overlay is the project's; the schema is the skill's, and the skill moves. `config.yaml` carries `overlay-version: 1`, and `state.py where` warns when the version is missing or differs, when a key is not in this schema or names a retired concept (`scaffolding`, `artifacts.register`, `architect.interfaces-document`, `architect.severity-overrides`, `roles-overlay`, `worktree.parent`, each with what replaced it), when a hook or `test.compare` climbs to the home with `../` (hooks see `$ORCH_HOME`), and when a role overlay or `priors.md` names a retired concept (the interfaces register, scaffolding, load-bearing entries). A warning is a review item for the user, not a refusal: the home keeps running, and the overlay is corrected by hand. The version is bumped when a key changes meaning, not when one is added.

## Validation

`state.py where` is the one check of the overlay: its warnings (§ Drift) are relayed to the user once per session and stop nothing. No tool checks that a hook is executable, that `taxonomy` matches the tasks in flight or that `thresholds` are positive; a hook that cannot run fails where it runs, with its own error. The orchestrator does not invent missing config.
