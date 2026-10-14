---
name: openspec-convert
description: "Convert a repository or orchestrator home from the OpenSpec layout (openspec/specs/, openspec/changes/ with delta specs, proposal.md and design.md) to the orchestrate skill's living org record (docs/specs/<capability>.org, docs/changes/<change>/{intent,design}.org). Use when a project still has an openspec/ directory, when `state.py where` warns that openspec/specs/ is still in the home, or when the user asks to migrate off OpenSpec. One-time migration; the orchestrate skill does not read the old layout."
---

# openspec-convert

Moves a project's record from OpenSpec's markdown layout to the living org record the `orchestrate` skill reads, once. Afterwards there is one copy of the specs, edited in place by each change; no delta, sync or archive step; the git diff is the change record. The target forms are the orchestrate skill's templates: `~/.claude/skills/orchestrate/templates/spec.md`, `intent.md` and `design.md`. Read them before converting.

The OpenSpec CLI and the `/opsx-*` skills are not needed and are not used.

## The two layouts

| | Home is the code repository | Home above the code repository |
|---|---|---|
| Example | `~/src/reddit_scraper` | `~/emacs-workspaces/emacs-mcp` (code root `emacs/`) |
| `openspec/` lives in | the repository | the home |
| `docs/` goes to | the repository | the code root (`code-root` in `.claude/orchestrator/config.yaml`) |
| Commits | one | one in the code root (the record), one in the home (removals, overlay, tasks) |

`state.py where` (`~/.claude/skills/orchestrate/scripts/state.py`) prints the home and the code root. Below, `$HOME_DIR` is the directory holding `openspec/`, `$CODE_ROOT` where `docs/` goes; they are the same directory in the first layout.

## The converter

`python3 ~/.claude/skills/openspec-convert/scripts/spec_convert.py <verb> …`, stdlib only; every verb prints a report and exits 1 on a refusal.

| Verb | Does |
|---|---|
| `tree SPECS_DIR OUT_DIR` | `openspec/specs/` → `docs/specs/`. A capability directory with one file becomes `<cap>.org`; with several, `<cap>/<file>.org`. Titles byte for byte. Rare constructs it keeps as written are listed in the report. |
| `change CHANGE_DIR OUT_DIR` | `proposal.md` → `intent.org`, `design.md` → `design.org`. |
| `apply CHANGE_DIR SPECS_ORG [--base BASE_ORG]` | Writes a change's delta specs into the org specs in place: ADDED appended, MODIFIED replaced. With `--base` (the org form of the specs the delta was written against), a MODIFIED block another change has also changed is a three-way merge, conflicts marked. REMOVED and RENAMED are refused: apply them by hand. |
| `count PATH…` | Requirements and scenarios per file, `.md` or `.org` (a directory is walked), and a total line. The before-and-after check. |
| `file SRC OUT [--cap ID] [--kind spec\|change]` | One file: a stray spec outside a capability directory, or one change document. |

`~/.claude/skills/orchestrate/scripts/coverage.py adopt-rows DESIGN_ORG --write` moves a converted design's legacy `* Scenario coverage` table into `:TEST:` properties on the scenarios (the `none` rows, and rows naming a test that carries no citation). After that, delete the table: coverage is computed.

## Procedure

Show the user each commit before it lands; push nothing.

### 0. Before

- No tracked changes in `$HOME_DIR` or `$CODE_ROOT`. Untracked files that are not yours stay untracked: stage the conversion by name, never `git add docs/`.
- No orchestrator cycle open. `state.py status` shows a closed cycle as `resume` calling for `init`; a state file it reports as `legacy` is closed when its cycle is archived under `.orchestrator/cycles/<id>/` and the last commit says integrate closed. Do not `migrate` a legacy state for the conversion; the next `init` replaces it.
- Note the branch each repository is on. The record commit lands on the current branch; if that is not the one the user wants it on, ask before step 5.
- List the active changes: `ls openspec/changes | grep -v '^archive$'`. Each is converted in place (step 2). It is not archived under OpenSpec first.
- `spec_convert.py count openspec/specs` and keep its total line.

### 1. Specs

- `spec_convert.py tree openspec/specs "$CODE_ROOT/docs/specs"`.
- Read the report. Settle with the user any capability-level oddity it names, such as a stray `specs/spec.md` at the top level whose scenario titles overlap another capability's. Settle it by its citations: the capability the tests already cite is the one to keep.
- `spec_convert.py count "$CODE_ROOT/docs/specs"`: the total must match step 0's. `tree` also prints `titles identical: True`; that line is the check that no citation key changed.
- Leave the files as converted. An OpenSpec preamble stays an unheaded paragraph before `* Requirements` and no `* Purpose` is written; a title with markdown backticks keeps them, since titles are citation keys.

### 2. Active changes

For each active change `<c>`:

- `spec_convert.py apply openspec/changes/<c> "$CODE_ROOT/docs/specs"`. When two active changes MODIFY the same requirement, apply the second with `--base`, the org conversion of the main specs before either change, and show the user the merged block.
- `spec_convert.py change openspec/changes/<c> "$CODE_ROOT/docs/changes/<c>"`. Then, in `intent.org`:
  - add `#+BASE:` (the code-root commit the change started from: its first commit, or the user's word);
  - add `#+STATUS:` (`speculated`, or what the old proposal status header said);
  - move the proposal's Why and goals into the template's sections.
- A `design.org` with a `* Scenario coverage` table: `coverage.py adopt-rows <design.org> --write`, then delete the table.
- Task files: `openspec/changes/<c>/tasks/{open,closed}/` → `$HOME_DIR/.orchestrator/changes/<c>/tasks/{open,closed}/`. Update `task_file` paths in the open cycle's state only through `state.py`; no cycle should be open (step 0).

### 3. What else `openspec/` holds

- `project.md`: compare it with the project's `CLAUDE.md`. Move what only it says into `CLAUDE.md` or `docs/`; it is not kept.
- `research/` and similar notes: move to `$CODE_ROOT/docs/research/` and fix the links that point at the old path (`git grep 'openspec/research'`).
- `config.yaml` (the OpenSpec CLI's) and `openspec/changes/archive/`: deleted. Git history keeps the archived changes; they are not converted.
- Then `git rm -r openspec/`. Ignored files (`__pycache__/`, editor backups) survive it: check that what is left on disk is only ignored files, then `rm -rf openspec`. A leftover `openspec/specs/` keeps `state.py where` warning.

### 4. Live references

`git grep -n -i 'openspec\|opsx'` in each repository, and `grep -rn -i 'openspec\|opsx' ~/.claude/projects/<slug>/memory/` for each directory the project is worked from (`<slug>` is the path with `/` as `-`). Fix what tells a running process or a future session where things are or what to run; leave history as it is.

- **Fix:**
  - the overlay `.claude/orchestrator/config.yaml`: drop the `artifacts` keys that name `openspec/` paths (the defaults are the new layout), any `forward-mode` keys naming `/opsx-*` skills, and any section `~/.claude/skills/orchestrate/overlay.md` does not define that resolved under `openspec/changes/` (`scaffolding:`); fix a header comment naming `opsx-orchestrate`;
  - the role overlays `roles/*.md` (`openspec/specs` → `docs/specs`);
  - `CLAUDE.md` and the memory files that say where the record is or which skills to run. Memory is outside the repository: edit it, it is not part of a commit;
  - project tools and hooks that build `openspec/changes/` paths or validate task files there;
  - permissions in `.claude/settings*.json` for the `openspec` CLI;
  - open `.tasks/` items (any status that means work remains: `ready`, `externalised`, `blocked`) that cite spec paths.
- **Leave:**
  - `.orchestrator/cycles/` (the cycle archives), handshakes, and a closed cycle's `state.json` (the next `init` replaces it);
  - `.tasks/` items whose work is finished (`done`, `resolved`, `promoted`, `superseded`, `declined`) and every `source:` provenance line;
  - the retired register (`interfaces.org` / `interfaces.md`) and provenance comments in code and tests (`promoted-from: openspec/changes/…`);
  - data files that merely contain the word.

### 5. Check and commit

- `python3 ~/.claude/skills/orchestrate/scripts/coverage.py --root "$CODE_ROOT"` runs. The citations that resolve to no scenario are listed. Each is either a capability that existed only in archived changes (fine), or a title the conversion changed (a bug: fix it before committing). `coverage.py` reads Emacs Lisp tests only; for another language every scenario shows `uncited` and this check sees nothing, so rely on `tree`'s `titles identical: True`.
- `state.py where` gives no `openspec/specs/` warning.
- The grep of step 4 finds only history.
- Commit the record in the code root (`Record: the specs move into docs/specs/ as org`, with the step 1 totals in the message). Commit the removals, the overlay, the task moves and the reference fixes in the home when it is a separate repository (`Record moved to docs/ in the code root`).

## What conversion does not do

- Convert archived changes, cycle archives or closed tasks.
- Backfill scenario citations in tests. `coverage.py` lists the uncited scenarios; a change cites a scenario when it touches it.
- Rename capabilities or titles. Titles are citation keys and stay byte for byte.
