---
name: opsx-continue
description: Continue working on a change - create the next artifact (Experimental). Use when the user wants to progress their change, create the next artifact, or continue their workflow.
---

Continue working on a change by creating the next artifact.

**Input**: Optionally specify a change name after `/opsx-continue` (e.g., `/opsx-continue add-auth`). If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **If no change name provided, prompt for selection**

   Run `openspec list --json` to get available changes sorted by most recently modified. Then use the **AskUserQuestion tool** to let the user select which change to work on.

   Present the top 3-4 most recently modified changes as options, showing:
   - Change name
   - Schema (from `schema` field if present, otherwise "spec-driven")
   - Status (e.g., "0/5 tasks", "complete", "no tasks")
   - How recently it was modified (from `lastModified` field)

   Mark the most recently modified change as "(Recommended)" since it's likely what the user wants to continue.

   **IMPORTANT**: Do NOT guess or auto-select a change. Always let the user choose.

2. **Check current status**
   ```bash
   openspec status --change "<name>" --json
   ```
   Parse the JSON to understand current state. The response includes:
   - `schemaName`: The workflow schema being used (e.g., "spec-driven")
   - `artifacts`: Array of artifacts with their status ("done", "ready", "blocked")
   - `isComplete`: Boolean indicating if all artifacts are complete

3. **Act based on status**:

   ---

   **If all artifacts are complete (`isComplete: true`)**:
   - Congratulate the user
   - Show final status including the schema used
   - Suggest: "All artifacts created! Next: run `/opsx-apply` to start implementing tasks, or `/opsx-tasks list` to review them."
   - STOP

   ---

   **If artifacts are ready to create** (status shows artifacts with `status: "ready"`):
   - Pick the FIRST artifact with `status: "ready"` from the status output
   - Get its instructions:
     ```bash
     openspec instructions <artifact-id> --change "<name>" --json
     ```
   - Parse the JSON. The key fields are:
     - `context`: Project background (constraints for you - do NOT include in output)
     - `rules`: Artifact-specific rules (constraints for you - do NOT include in output)
     - `template`: The structure to use for your output file
     - `instruction`: Schema-specific guidance
     - `outputPath`: Where to write the artifact
     - `dependencies`: Completed artifacts to read for context
   - **Create the artifact file**:
     - Read any completed dependency files for context
     - Use `template` as the structure - fill in its sections
     - Apply `context` and `rules` as constraints when writing - but do NOT copy them into the file
     - Write to the output path specified in instructions
   - Show what was created and what's now unlocked
   - STOP after creating ONE artifact

   ---

   **If no artifacts are ready (all blocked)**:
   - This shouldn't happen with a valid schema
   - Show status and suggest checking for issues

4. **After creating an artifact, show progress**
   ```bash
   openspec status --change "<name>"
   ```

**Output**

After each invocation, show:
- Which artifact was created
- Schema workflow being used
- Current progress (N/M complete)
- What artifacts are now unlocked
- Prompt: "Run `/opsx-continue` to create the next artifact"

**Artifact Creation Guidelines**

The artifact types and their purpose depend on the schema. Use the `instruction` field from the instructions output to understand what to create.

**spec-driven-tasks schema** (proposal → specs → architecture → design → tasks) - **RECOMMENDED**:
- **proposal.md**: Ask user about the change if not clear. Fill in Why, What Changes, Capabilities, Impact.
  - The Capabilities section is critical - each capability listed will need a spec file.
- **specs/<capability>/spec.md**: Create one spec per capability listed in the proposal's Capabilities section (use the capability name, not the change name).
  - Include scenarios with clear success criteria.
  - Use `### Requirement:` and `#### Scenario:` headers for proper parsing.
- **architecture.md**: Document components, interfaces, boundaries, and testability.
  - **IMPORTANT**: Engage user in dialog about testing approach:
    - Test framework (ecosystem-appropriate: Jest/Vitest, pytest, Go test, etc.)
    - Test file location (`test/`, `__tests__/`, or co-located)
    - Naming conventions (files and test functions)
    - How to run tests (the project's test command and common subsets)
    - Mock/stub patterns
    - Test helpers location
    - Scenario mapping strategy
  - Use **AskUserQuestion** to gather testing preferences
  - Document all decisions in architecture.md Testing Approach section
- **design.md**: Document technical decisions and implementation approach.
  - Reference architecture for structure and testing approach
  - Explain how implementation will map to components and interfaces
  - Include patterns, integration approach, and any design trade-offs
- **tasks/**: Generate self-contained task files in `tasks/open/*.md`.
  - Use the `/opsx-tasks generate` workflow (preview + user approval, then
    file creation)
  - Each task has YAML frontmatter (name, description, change, status,
    relations) and a self-contained body (files to modify, implementation
    steps, design rationale, verification, context)

**After tasks are generated**, suggest `/opsx-apply` to start implementing, or `/opsx-tasks list` to review them first.

**spec-driven-tdd schema** (proposal → specs → architecture → tests → design):
- Similar to spec-driven-tasks but includes a **tests/** artifact between architecture and design
- **tests/**: Create actual test files (not documentation).
  - Location determined by architecture.md
  - Create test files that map scenarios from specs
  - Use naming conventions from architecture.md
  - Tests should be runnable but may fail (TDD red phase)
  - Use the framework's skip/pending/xfail mechanism for tests that require future implementation
  - Map tests to scenarios via function names and comments
  - Example comment: `"Scenario: specs/<capability>/spec.md § 'Requirement name'"`
- Design references tests and explains how implementation will make them pass

**spec-driven schema** (proposal → specs → design) - legacy:
- Old schema without architecture artifact
- Skip directly from specs to design (loses testing approach documentation)
- Not recommended for new changes

**IMPORTANT**: Implementation work is tracked through task files in
`tasks/open/` and `tasks/closed/`. Use `/opsx-tasks` to manage them and
`/opsx-apply` to implement.

For other schemas, follow the `instruction` field from the CLI output.

**Guardrails**
- Create ONE artifact per invocation
- Always read dependency artifacts before creating a new one
- Never skip artifacts or create out of order
- If context is unclear, ask the user before creating
- Verify the artifact file exists after writing before marking progress
- Use the schema's artifact sequence, don't assume specific artifact names
- **IMPORTANT**: `context` and `rules` are constraints for YOU, not content for the file
  - Do NOT copy `<context>`, `<rules>`, `<project_context>` blocks into the artifact
  - These guide what you write, but should never appear in the output
