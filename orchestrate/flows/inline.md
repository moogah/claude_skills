# Inline path

You are the phase agent for the inline path: one small edit the user asked for, made in your own context.

## Collapsed cycle

The cycle's three phases run here as three steps of yours:

- Plan → the triage (§ When to take the inline path).
- Execute → the edit, its verification and your review of the diff (§ Inline operations, steps 1–2).
- Integrate → the integrate question (§ Inline operations, step 3).

Run all three. Ask the integrate question even when the edit looks trivial: it is what catches a "trivial" edit that surfaces a contract issue, and most answers are "no; close".

You are the reviewer. Make the edit in the code root, the change's worktree. Create no worktree. Spawn no Implementor and no Reviewer.

## When NOT to use inline even if technically eligible

The inline path takes only a request outside a cycle's batch: between cycles, or while a cycle is open.

- The request is for a task already in a cycle's batch, however small → do not make it; that cycle's execute does. Write no task file and no note. Hand back `inline <task>: bailed out, <why>; <task file>`, naming the batch task's file.

## When to take the inline path

Triage the request before any edit. The triage keeps the work to one file and off every seam, because your review runs at `sonnet`, `medium`. A change that needs a stronger reviewer than that → **No**.

| Signal | Take inline path |
|---|---|
| One-line edit | Yes |
| Single-file doc fix | Yes |
| Single call-site rename | Yes |
| Regenerating one artifact (e.g. tangling one literate file) | Yes |
| Editing one config value | Yes |
| Fixing a typo in a comment or docstring | Yes |
| Anything touching ≥2 files | **No** |
| Anything touching a seam's owning symbols (a `## Seams` row in the change's `design.org`) | **No**: a seam edit needs the full Reviewer path and integrate's conformance check |
| Anything that requires running tests with state setup | **No**: it needs a worktree |
| Anything where the verification command's output isn't trivially predictable | **No** |

- In doubt → **No**.
- **No** → write the task file (§ Inline operations, step 0), then bail out with nothing to revert (§ Bailout rule), the triage's reason as the note.
- **No**, and the request also puts the change's design or a spec in question → bail out all the same and file no ask. Name the question in the note, so the next plan's Architect sees it with the whole design.
- **Yes** → § Inline operations.

## Inline operations

"Bail out" in these steps means: follow § Bailout rule.

**0. Write the task file** in the change's `tasks/open/` at the home, from the request your brief carries. Read `templates/task-body.md` § Frontmatter and § Body sections for its fields. Give it `status: ready`.

- The task came from a discovery → fill its provenance fields `discovered_from`, `discovered_by`, `discovered_class`, and run `state.py check new <name>`.

**1. Make the edit** in the code root.

- A cycle is open and an after-test is running in the code root → wait for it to finish before you edit.
- The edit touches a second file → bail out.
  - A text correction in another file (step 3) is not part of the edit and does not count.
- The edit touches a seam's owning symbols → bail out.
- The edit needs more than a paragraph of context re-read → bail out.

**2. Verify and review.**

1. Run the verification command in the code root: the overlay's `test.command`, or a narrower command if obviously sufficient.
2. Keep its output as `<change-dir>/after-<task>-<ts>.txt`.
3. The command fails in a way that isn't trivially fixable → bail out.
4. Read the diff, `git -C "$CODE_ROOT" diff`, against the request. The tests and `git status` do not show what the edit says; the diff does.

**3. Ask the integrate question.** Check each of these:

- **Seam rows.** Check the task's `cites_seams` against the `## Seams` rows in the change's `design.org`. The edit changes a row's statement or owning symbols → bail out and leave the row as it is (a record correction is integrate's).
- **The after-file.** Grep it for anything new the verification output revealed.
- **What the edit exposed** in the code or text beside it. Route each finding (`templates/architect-finding.md` § Routing):
  - A defect the change's design already plans to fix (a Decision, a seam row, a pending test) → not exposed. Do not route it and do not bail out; name it as planned, and where, in step 4's line.
  - An untrue sentence in the file being edited → correct it in the same edit, prose only.
  - An untrue sentence in another file → correct it, prose only, in a commit of its own with the subject `Text correction: <what>`.
  - A defect off the change's main path → one entry on the change's hardening list, `:REF:` the task. Read `templates/hardening.md` § The hardening list for the entry's form.
  - A defect on the change's main path → bail out. Name the defect and its evidence in the bailout note; the next plan rewrites the task with the defect in view.
  - A speculative need → one `thought:` line in the task body's `## Observations`.
  - Write no new task file and no follow-up candidate for any of these.
- **The design.** The edit puts the change's design in question → file a `decision` ask in the change's inbox (read `templates/ask.md` § The file for its fields) that blocks your commit. Leave the edit uncommitted. Hand back `inline <task>: stopped, ask <id> blocks commit; <ask file>`. Put no question to the user: the dispatcher asks (`templates/ask.md` § The inline rule).

**4. Update the task file** at the home.

1. Add one line saying what was checked for what the edit exposed and what was found, "nothing" included.
2. Set `status: done` directly, with no `needs_review` or `reviewed` between.

The task file is the task's record, whether or not a cycle is open; while one is open, its `status: done` is what shows the task done in that cycle, outside its batch. Do not run `state.py check done` for it: an inline task has no `Merge task` commit and no review file.

**5. Commit** with the subject `Inline task <task>: <description>`, the message naming the verification command.

- Commit the code in the code root, and the task file and any ask file in the home.
- The home is the code root (one working tree) → one commit.
- Otherwise → one commit in each.

Hand back:

```
inline <task>: done <sha>; <task file>
```

## Bailout rule

**If an inline turns out larger than triaged: stop, revert, and put the task back for a later cycle's batch.**

1. Revert the code root:
   - No commit made yet (the inline path commits last) → `git -C "$CODE_ROOT" checkout .`
   - A commit already made → `git revert <commit>` in the code root, not `git reset`, so the history keeps it.
   - The triage refused the request → nothing to revert.
2. Leave the task in `tasks/open/` with `status: ready`.
3. Add a note to the task body: `inline path bailed out: <reason>; rescheduled as worktree.` The reason states what was found and where its evidence is: a record for the next plan, not steps.
4. Hand back:

```
inline <task>: bailed out, <why>; <task file>
```
