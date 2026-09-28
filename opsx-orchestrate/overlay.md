# Overlay convention

The core skill is project-agnostic. Each project repo carries its own overlay at `<repo>/.claude/orchestrator/`. The overlay supplies the project's test command, build hooks, role-prompt extensions, threshold tuning, and task taxonomy.

## Layout

```
<repo>/.claude/orchestrator/
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

The directory is **flat under `.claude/`** — not nested under `.claude/skills/` — to signal that this is config the central skill consumes, not a self-contained skill.

## config.yaml schema

```yaml
# Required: project identity
project: <slug>
language: elisp | go | typescript | python | rust | other

# Required: build & test
build:
  pre-commit: ./bin/tangle-org.sh   # OPTIONAL
test:
  command: ./bin/run-tests.sh       # REQUIRED
  parser: emacs-results-txt | exit-code | junit-xml | tap   # default: exit-code
  pending-style: xfail-strict | xit | <project value>   # how the design round marks a pending test; must be a style the runner reports as pending, never as failing (a red baseline disables regression detection). Default: xfail-strict for pytest, xit for buttercup

# Required: artifact roots — where the orchestrator finds tasks
artifacts:
  tasks-root: openspec/changes/<change>/tasks   # in-change tasks; <change> is a placeholder
  externalised-root: .tasks                      # cross-cutting backlog
  proposal-pattern: openspec/changes/<change>/proposal.md
  design-pattern: openspec/changes/<change>/design.md

# Required: worktree handling
worktree:
  init: ./bin/init-worktree-runtime.sh   # OPTIONAL — runs after `git worktree add`
  needs-submodules: true                  # default: false
  parent: .worktrees                       # default: .worktrees

# OPTIONAL: role-prompt extensions
roles-overlay:
  implementor: roles/implementor.md
  reviewer: roles/reviewer.md
  architect: roles/architect.md
  project-manager: roles/project-manager.md

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
  externalisation-pressure-growth: monotonic-3-cycles

# OPTIONAL: critical-path declaration
critical-path:
  inference: llm-read-proposal       # default; one read per plan phase
  override-tasks: []                   # explicit list, if known up front
  override-labels: []                  # task_class values that are always critical-path

# OPTIONAL: Architect severity overrides
architect:
  severity-overrides:
    duplication: blocking         # promoted from advisory in this project
    mutation: informational       # demoted from advisory; this project tolerates it
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

## Resolution rule

At skill invocation:

1. The orchestrator resolves the overlay by **walking up from `$cwd`** looking for `.claude/orchestrator/config.yaml`. The first hit wins; record its directory as `$REPO_ROOT`.
2. If found, parse `config.yaml`. Required fields are validated; missing-required is a hard error (orchestrator refuses to start).
3. Optional fields fall through to core defaults.
4. Markdown role overlays referenced by `roles-overlay.*` are read and **appended** to the corresponding core role briefs at agent-spawn time, followed by `priors.md` when it exists.
5. Hook scripts referenced by `build.pre-commit`, `test.command`, `worktree.init` are resolved relative to `$REPO_ROOT`.

If no overlay is found, the orchestrator warns explicitly and falls back to:

| Default |
|---|
| `test.command`: `make test` if Makefile present, else error |
| `test.parser`: `exit-code` |
| `worktree.parent`: `.worktrees` |
| `worktree.needs-submodules`: false |
| `artifacts.tasks-root`: `tasks` (flat) |
| `artifacts.externalised-root`: `.tasks` |
| `artifacts.design-pattern`: `openspec/changes/<change>/design.md` |
| `test.pending-style`: `xfail-strict` (pytest) or `xit` (buttercup), else error when the design round needs it |
| `taxonomy`: `[feature, test, doc, refactor, bug, contract, infrastructure]` |
| `thresholds`: see `templates/pm-digest.md` defaults |
| `asks.reader`: "technical product manager who has not watched development and has a shallow view of the internals" |
| `priors`: none appended |

The fallback behaviour exists so the orchestrator can run on a project that hasn't been onboarded yet, but the warning is loud — running without an overlay is not the steady state.

## Why walk up rather than require `$REPO_ROOT`

- Works from sub-directories without needing to `cd` to the repo root.
- Doesn't require `git` to be present (some projects use `jj`, `hg`, or no VCS).
- Matches how editors and other tooling resolve project-local config (`.editorconfig`, `package.json`, `pyproject.toml`).

## Role overlay append semantics

Role overlay files are **appended**, not substituted. The core role brief is the spine; the overlay extends it. Example for elisp:

Core `roles/reviewer.md` contains the rigorous-not-contrarian / thoughtful-maintainer mindset.

Project overlay `<repo>/.claude/orchestrator/roles/reviewer.md` adds:

```markdown
## Project-specific reviewer checks (elisp)

- Verify `lexical-binding: t` header on every new `.el` file.
- Flag `cl-lib` usage where `seq` would suffice (modern elisp idiom).
- For literate-org modules: verify the `.org` source was edited and tangled, not the `.el` directly.
- Flag `nconc` / `assq-delete-all` / shared-mutable-state patterns at module boundaries.
```

The orchestrator concatenates: core brief + `\n\n## Project-specific extensions\n\n` + overlay contents + `\n\n## Project priors\n\n` + `priors.md` contents (when present).

## Priors

`priors.md` holds the user's standing rules for the project, one line each, in the user's own words with the date and where they said it:

```markdown
- 2026-09-20 session 618fe4f9: "It feels like overengineering to put in place checks in our code which only exist to make sure that we've done our job properly"
- 2026-09-20 session 89ac5a4d: "In general I'll favor getting this code cleaned up and removing leftovers from prior implementations"
- 2026-09-24 session c0e9e2a6: "I don't want to keep increasing scope"
- 2026-09-26 session c0e9e2a6: "Let's punt on this decision, I want to use this a while before we decide on polish like this"
```

**Write rule.** When a decision or a free-text reply states a general rule rather than a one-off choice ("in general", "I don't want to keep", "overengineering", "use it a while before"), the orchestrator appends the line and records a `confirmation`-kind ask citing it, listed under "Applied without asking" in `asks.md` (`templates/ask.md`). The user can object. The line is the quote, not a paraphrase.

**Read rule.** The file is appended to every role brief after the role overlay, and the orchestrator reads it at plan § 1 and at integrate § 3a and § 7. A role may treat a prior as a user request: "removing leftovers" makes a dead-code finding actionable without an ask. A prior is never re-asked. A finding that a prior already answers is `noted` with the prior cited in `evidence`; no ask record is created for it, and one that already exists is `declined` with the prior as its readback. A prior is the user's own words and outranks a `design.md` sentence; a design sentence a prior contradicts is a `doc-correction`.

The file is markdown, not a `config.yaml` list, because the spawn contract already concatenates markdown and a quoted sentence is safer there than in YAML.

## Hook contract

Hooks are bash scripts run by the orchestrator at fixed lifecycle points:

| Hook | When | Cwd | Failure semantics |
|---|---|---|---|
| `worktree.init` | After `git worktree add`, before agent spawn | inside the new worktree | Failure aborts task setup; status → failed |
| `build.pre-commit` | Before commit in a worktree (after agent finishes) | inside the worktree | Failure aborts commit; status → failed; worktree retained |
| `test.command` | After every merge to integration branch, and as the final implementor verification step | inside repo root or worktree (depending on phase) | Non-zero status indicates regression; baseline-vs-after comparison runs |

Hooks must be idempotent. The orchestrator may re-run them on retry.

## Validation

The orchestrator validates `config.yaml` at start of every phase:

- All required fields present.
- Hook scripts exist and are executable.
- `taxonomy` values match what's in flight on existing tasks.
- `thresholds.*` numerical fields are positive numbers.

Validation failure is a hard stop. The orchestrator does not invent missing config.
