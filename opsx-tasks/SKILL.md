---
name: opsx-tasks
description: Manage task files for an OpenSpec change. Use when listing, creating, updating, or viewing tasks — or when generating the initial set of tasks from design artifacts.
---

Manage task files for an OpenSpec change.

**Input**: A subcommand and optionally a change name or task name:
- `list [change]` — show task status overview
- `show <task-name> [change]` — display a single task's full content
- `create [change]` — create a new ad-hoc task
- `update <task-name> [change]` — update a task's status or content
- `generate [change]` — generate the initial task set from design artifacts

If no subcommand is given, default to `list`.

## Subcommands

### list

Show task status for a change.

1. Select the change (infer from context, auto-select if only one, or prompt)
2. Scan `tasks/open/*.md` and `tasks/closed/*.md`, read frontmatter
3. Resolve dependencies: if a `blocked-by` task is in `tasks/closed/`, update
   status of the dependent from `blocked` to `ready`
4. Display:

```
## Tasks: <change-name>

### Open (X)
| Name            | Description              | Status  | Blocked By      |
|-----------------|--------------------------|---------|-----------------|
| setup-module    | Create module structure  | ready   | —               |
| implement-core  | Implement core logic     | blocked | setup-module    |

### Closed (Y)
| Name          | Description             |
|---------------|-------------------------|
| write-spec    | Draft behavioural spec  |

Total: Z tasks (X open, Y closed)
```

### show

Display a task's full content.

1. Select the change
2. Find the task file in `tasks/open/` or `tasks/closed/`
3. Display the full file content (frontmatter + body)

### create

Create a new ad-hoc task.

1. Select the change
2. Ask for: task name (kebab-case), description, and whether it's blocked by
   anything
3. If the user describes what the task should do, generate the full body
   following the task content guidelines below
4. Write to `tasks/open/<name>.md`
5. If this task has `enables:` relations, check whether the enabled tasks need
   updating

### update

Update a task's status or content.

1. Select the change
2. Find the task file
3. Apply the requested change:
   - **Status change**: Update frontmatter `status` field
   - **Move to closed**: Move file from `tasks/open/` to `tasks/closed/`, set
     `status: done`
   - **Reopen**: Move file from `tasks/closed/` to `tasks/open/`, set
     `status: ready`
   - **Add relation**: Append to `relations` list
   - **Edit content**: Update the markdown body
4. If completing a task, check whether it unblocks other tasks and update their
   status

### generate

Generate the initial set of task files from design artifacts. This is the
primary task creation workflow, used when the `tasks` artifact is next in the
schema sequence.

**Steps:**

1. Select the change
2. Read all context: design.md, architecture.md, specs, proposal
3. Parse design.md structure (## and ### headings), extract sections,
   decisions, patterns
4. For each logical chunk, generate a self-contained task file
5. **Preview before creating**:
   ```
   ## Proposed Tasks for <change-name>

   1. <name> — <description> [ready]
   2. <name> — <description> [blocked-by: 1]
   3. <name> — <description> [ready]
   ...

   Total: N tasks | Ready: X | Blocked: Y
   ```
   Get user approval. Offer to adjust granularity or grouping.
6. Create task files in `tasks/open/<name>.md`
7. Ensure `tasks/closed/` directory also exists (create with `.gitkeep` if it
   would otherwise be empty)

## Task File Format

Each task is a markdown file at `tasks/open/<name>.md` or
`tasks/closed/<name>.md`:

```markdown
---
name: <short-kebab-id>
description: <one-line human-readable summary>
change: <change-name>
status: ready
relations:
  - blocked-by:<other-task-name>
  - discovered-from:<other-task-name>
---

<rich markdown body>
```

### Frontmatter fields

- **name**: Short machine-readable identifier, kebab-case
  (e.g., `wire-scope-validator`)
- **description**: One-line human-readable summary of the task
- **change**: The openspec change name this task belongs to
- **status**: One of `blocked`, `ready`, `needs-review`, `done`
- **relations**: List of labeled references to other tasks:
  - `blocked-by:<name>` — cannot start until the named task is done
  - `discovered-from:<name>` — spawned during work on another task
  - `enables:<name>` — completing this unblocks the named task

### Markdown body

The body MUST be self-contained. An agent should be able to implement the task
by reading only this file — no need to reference design.md or specs during
work.

Body structure:

```markdown
## Files to modify
- path/to/file.ext (new|modify)

## Implementation steps
1. [Specific actionable step with details]
2. [Another step — include code patterns if relevant]
3. [Be concrete — mention functions, variables, patterns]

## Design rationale
[WHY from design.md — extract and embed]
[The architectural reasoning behind this implementation]
[Include relevant context about technology choices, patterns, tradeoffs]

## Design pattern
[Pattern to follow, with examples]
[Reference similar code: "See path/to/example for the pattern"]

## Verification
- Run tests: the project's test command for the affected area (see architecture.md)
- [Specific scenarios / tests that should pass]
- [Acceptance criteria — what "done" means]

## Context
design.md § [section] '[section title]'
architecture.md § [section] (if relevant)
```

## Task Ordering

When generating tasks, sequence them so prerequisites come first. Typical
patterns:

- Infrastructure/setup (new modules, shared helpers) before features that
  depend on them
- Test scaffolding before implementation when doing TDD
- Validation of existing behaviour (characterization tests) before refactors
- Integration tasks last

The final task should verify the overall change (full test run, spec scenarios
pass).

## Granularity

- Target: 1-4 hours per task (one focused session)
- If a section feels larger: split into multiple tasks
- If a section feels smaller: combine with related sections
- Infrastructure/setup before features, shared utilities before dependents

## Context Extraction

- Don't reference design.md — EXTRACT and EMBED relevant context into the task
- Include WHY (rationale) not just WHAT (steps)
- Include patterns/examples mentioned in design
- Include integration points from design/architecture
- Extract acceptance criteria from specs

## Task Lifecycle

- New tasks are created in `tasks/open/<name>.md` with status `ready` or
  `blocked`
- When a task is completed, move it to `tasks/closed/<name>.md` and set status
  to `done`
- If work on a task reveals new work, create a new task with
  `discovered-from:<original>`
- If a task needs review, set status to `needs-review`
- To reopen a closed task, move it back to `tasks/open/` and set status to
  `ready`

## Guardrails

- **Self-contained**: Extract and embed design context, don't just reference
- **Actionable**: Each task must have concrete implementation steps
- **Scoped**: Target 1-4 hours per task (one session)
- **Test-aware**: Reference the test command from architecture.md so the
  implementer knows how to verify
- **Verified**: Each task must have clear acceptance criteria
- **Sequenced**: Use blocked-by relations where order matters
- **User approval**: Always preview and get confirmation before generating
  tasks
- **Context extraction**: Include WHY (rationale) not just WHAT (steps)
