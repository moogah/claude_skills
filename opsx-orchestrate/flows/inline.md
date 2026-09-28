# Inline path

For trivial tasks where agent + worktree overhead dominates the work. The entire cycle collapses into the orchestrator's own context.

## When to take the inline path

The triage criteria (lifted from older emacs skill §4 and promoted to core):

| Signal | Take inline path |
|---|---|
| One-line edit | Yes |
| Single-file doc fix | Yes |
| Single call-site rename | Yes |
| Regenerating one artifact (e.g. tangling one literate file) | Yes |
| Editing one config value | Yes |
| Fixing a typo in a comment or docstring | Yes |
| Anything touching ≥2 files | **No** — go to standard cycle |
| Anything touching a seam's owning symbols (a `## Seams` row in the change's `design.md`) | **No** — a seam edit deserves the full Reviewer path and integrate's conformance check |
| Anything that requires running tests with state setup | **No** — needs a worktree |
| Anything where the verification command's output isn't trivially predictable | **No** |

When in doubt, take the standard path. The cost of running a small task as a worktree is small; the cost of taking an inline path on something that turns out to be larger is the bailout (see below).

## Collapsed cycle

In the inline path, plan / execute / integrate collapse into the orchestrator's own context:

- **Plan** = "is this trivial enough?" (the triage above).
- **Execute** = the inline edit + verification.
- **Integrate** = "does the trivial edit change a seam row in `design.md`?"

The integrate question is **not skipped** — that's what catches the case where a "trivial" edit surfaces a contract issue. Most inline tasks integrate to "no; close." But the question still gets asked.

## Inline operations

1. **Make the edit** in the orchestrator's own checkout (no worktree).
2. **Run the verification command** (overlay's `test.command`, or a narrower command if obviously sufficient).
3. **Check the seam rows**: does this edit change a seam row in the change's `design.md` (its statement or owning symbols)? If yes, amend the row in place and record a `doc-correction` ask (`templates/ask.md`) with `status: applied`, listed under "Applied without asking" at the next integrate. A design in question is a `decision` ask, presented at the next integrate unless it blocks this inline commit.
4. **Update the task file**'s `status` to `done` directly (skipping `needs_review` / `reviewed`). Inline path skips formal review because the orchestrator IS the reviewer. If a cycle is open, the task is in state too: `state.py task add <name> …` if it was not, then `state.py task set <name> done`; between cycles the task file is the record.
5. **Commit** with the standard task-name commit message.

## Bailout rule

**If an inline turns out larger than triaged: stop, revert, reschedule as a worktree task.**

Specifically: if any of these happen during the inline edit, bail out:

- The edit touches a second file unexpectedly.
- The verification command fails in a way that isn't trivially fixable.
- An adjacent issue surfaces that warrants its own fix.
- The change requires re-reading more than a paragraph of context.
- The change touches a seam's owning symbols.

Bail-out procedure:
1. `git checkout .` to revert the working tree (no commit was made yet — inline path commits last).
2. Mark the task `status: ready` (it remains in the open queue).
3. Add a note in the task body: "inline path bailed out: <reason>; rescheduled as worktree."
4. The next standard cycle picks it up.

If a commit was already made and bail-out is needed, revert via `git revert <commit>` rather than `git reset` — preserves the audit trail.

## Why preserve the integrate question

The brainstorm names this explicitly. Even trivial edits can surface contract issues; pretending they can't is what produces silent contract drift. The integrate question is cheap to ask:

- Did this edit touch a seam's owning symbols? → check `cites_seams` of the implicit "task" against the `## Seams` rows in `design.md`.
- Did the verification command's output reveal anything new? → grep the after-file.
- Was there an adjacent observation worth recording? → write it down in the task body's `## Observations`.

Most answers are "no; close." That's fine. The discipline is in asking, not in finding.

## What inline path does NOT skip

- The integrate question (above).
- Provenance fields if the inline task is a follow-up: `discovered_from`, `discovered_by`, `discovered_class`.
- Recording the task in state when a cycle is open (so `state.py counts` sees it; counts are derived, nothing is bumped by hand).

## What inline path DOES skip

- Worktree creation.
- Implementor agent spawn.
- Reviewer agent spawn (orchestrator is the reviewer).
- The `needs_review` / `reviewed` intermediate states (status goes `ready → done` directly).
- Author-blind enforcement (irrelevant — single actor).
- The full integrate phase (replaced by the integrate question).

## When NOT to use inline even if technically eligible

If the change is part of a larger batch where parallel implementation is the point of using the orchestrator at all, take the standard path even for individually trivial tasks. The point of the standard path is throughput; collapsing every trivial task into inline serializes the batch.

The orchestrator's batch-composer (in `flows/plan.md`) decides this. Inline path is for **out-of-cycle** trivial fixes the user asks for between cycles, not for tasks already in a batch.
