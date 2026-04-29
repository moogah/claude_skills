---
name: opsx-apply
description: Implement tasks from an OpenSpec change. Use when the user wants to start implementing, continue implementation, or work through task files generated from an OpenSpec design.
---

Implement tasks from an OpenSpec change by processing files in `tasks/open/`.

**Input**: Optionally specify a change name (e.g., `/opsx-apply add-auth`). If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **Select the change**

   If a name is provided, use it. Otherwise:
   - Infer from conversation context if the user mentioned a change
   - Auto-select if only one active change exists
   - If ambiguous, run `openspec list --json` to get available changes and use
     the **AskUserQuestion tool** to let the user select

   Always announce: "Using change: <name>" and how to override
   (e.g., `/opsx-apply <other>`).

2. **Scan task files**

   Read frontmatter and body for every file in:

   - `openspec/changes/<name>/tasks/open/*.md`
   - `openspec/changes/<name>/tasks/closed/*.md`

   For each open task, resolve status:
   - If frontmatter `status: blocked` and every `blocked-by:<dep>` relation
     names a task now present in `tasks/closed/`, treat the task as `ready`
     (and rewrite its frontmatter to match)
   - Otherwise keep the recorded status

   **Handle states:**
   - No task files at all → suggest running `/opsx-tasks generate` to create
     tasks from design
   - All tasks in `tasks/closed/` → congratulate, suggest archive
   - Otherwise proceed to implementation

3. **Read shared context once**

   Read the change's design, architecture, specs, and proposal into context
   up front (so you can cross-check if a task's embedded context feels thin):

   ```bash
   openspec instructions apply --change "<name>" --json
   ```

   Use the `contextFiles` list it returns.

4. **Show current progress**

   ```
   ## Implementing: <change-name>

   **Progress:** N closed / M total

   **Ready:**
   - setup-module — Create module structure
   - write-spec-file — Draft spec.md

   **Blocked:**
   - implement-core — blocked by setup-module

   Starting with: setup-module
   ```

5. **Implement work (loop until done or blocked)**

   For each ready task (file-order / `ls` order within `tasks/open/` is fine
   unless a different order is obvious from dependencies):

   - Read the full task file. Its body is self-contained — rely on it for
     implementation steps, rationale, and verification.
   - Make the code changes required.
   - Run the verification commands listed in the task (the project's test
     command for the affected area — see architecture.md). Fix failures
     before closing.
   - **Discovery workflow**: If you uncover unexpected work:
     - Create a new task with `relations: [discovered-from:<current>]` via the
       `opsx-tasks create` subcommand (or by writing the file directly)
     - Types of discoveries: bugs, questions, related work, blockers,
       missing tests
     - Do NOT expand the current task's scope
     - Do NOT update design.md mid-task (user reviews discoveries later)
   - Close the task: move the file from `tasks/open/<name>.md` to
     `tasks/closed/<name>.md` and set frontmatter `status: done`. If any other
     open tasks had `blocked-by:<this>`, re-evaluate their status.
   - Continue to the next ready task.

   **Pause if:**
   - Task body is unclear → ask for clarification
   - Implementation reveals a design issue → suggest updating artifacts
   - Error or blocker encountered → report and wait for guidance
   - User interrupts

6. **On completion or pause, show status**

   - Tasks completed this session
   - Overall progress: "N/M tasks closed"
   - If all done: suggest `/opsx-archive`
   - If paused: explain why and wait for guidance

**Output During Implementation**

```
## Implementing: <change-name>

Working on setup-module (1/5): Create module structure
[...implementation happening...]
✓ Closed: setup-module

Working on implement-core (2/5): Implement core logic
[...]
✓ Closed: implement-core
```

**Output On Completion**

```
## Implementation Complete

**Change:** <change-name>
**Progress:** 5/5 tasks closed ✓

### Completed This Session
- ✓ setup-module: Create module structure
- ✓ implement-core: Implement core logic
...

All tasks closed. You can archive this change with `/opsx-archive`.
```

**Output On Pause**

```
## Implementation Paused

**Change:** <change-name>
**Progress:** 4/7 tasks complete

### Issue Encountered
<description of the issue>

**Options:**
1. <option 1>
2. <option 2>
3. Other approach

What would you like to do?
```

**Guardrails**
- Keep going through tasks until done or blocked
- Always read the shared context files once (step 3) so you can sanity-check
  task bodies against the source artifacts
- Task bodies are the source of truth for implementation — don't expand scope
- If a task is ambiguous, pause and ask before implementing
- Keep code changes minimal and scoped to each task
- Move the file to `tasks/closed/` and re-check dependents immediately after
  completing
- Pause on errors, blockers, or unclear requirements — don't guess
- Don't skip verification — the task's Verification section lists the
  commands that define "done"

**Fluid Workflow Integration**

This skill supports the "actions on a change" model:
- **Can be invoked anytime**: Before all artifacts are done (if tasks exist),
  after partial implementation, interleaved with other actions
- **Allows artifact updates**: If implementation reveals design issues,
  suggest updating artifacts — not phase-locked, work fluidly
