---
name: opsx-ff
description: Create a change and generate all artifacts needed for implementation in one go. Use when the user wants to quickly move from idea to implementation-ready without stepping through each artifact individually.
---

Fast-forward through artifact creation - generate everything needed to start implementation.

**Input**: The argument after `/opsx-ff` is the change name (kebab-case), OR a description of what the user wants to build.

**Steps**

1. **If no input provided, ask what they want to build**

   Use the **AskUserQuestion tool** (open-ended, no preset options) to ask:
   > "What change do you want to work on? Describe what you want to build or fix."

   From their description, derive a kebab-case name (e.g., "add user authentication" → `add-user-auth`).

   **IMPORTANT**: Do NOT proceed without understanding what the user wants to build.

2. **Create the change directory**
   ```bash
   openspec new change "<name>" --schema spec-driven-tasks
   ```
   This creates a scaffolded change at `openspec/changes/<name>/` using the spec-driven-tasks schema.

3. **Get the artifact build order**
   ```bash
   openspec status --change "<name>" --json
   ```
   Parse the JSON to get:
   - `artifacts`: list of all artifacts with their status and dependencies

4. **Create artifacts in sequence until design is complete**

   Use the **TaskCreate tool** to track progress through the artifacts.

   Loop through artifacts in dependency order (artifacts with no pending dependencies first):

   a. **For each artifact that is `ready` (dependencies satisfied)**:
      - Get instructions:
        ```bash
        openspec instructions <artifact-id> --change "<name>" --json
        ```
      - The instructions JSON includes:
        - `context`: Project background (constraints for you - do NOT include in output)
        - `rules`: Artifact-specific rules (constraints for you - do NOT include in output)
        - `template`: The structure to use for your output file
        - `instruction`: Schema-specific guidance for this artifact type
        - `outputPath`: Where to write the artifact
        - `dependencies`: Completed artifacts to read for context
      - Read any completed dependency files for context
      - **Special handling for artifact types:**
        - **architecture**: Engage user in dialog about testing approach using **AskUserQuestion**:
          - Test framework and version
          - Test file location
          - Naming conventions
          - How to run tests
          - Mock/stub patterns
          - Test helpers approach
          - Document all decisions in architecture.md
        - **tests**: Create actual test files (not documentation):
          - Use test location/framework from architecture.md
          - Create test files mapping spec scenarios to test cases
          - Use naming from architecture.md conventions
          - Include test helpers if specified
          - Tests should be runnable but may fail (TDD red phase)
          - Use skip/expected-fail for tests requiring future work
        - **Other artifacts**: Create using `template` as structure
      - Apply `context` and `rules` as constraints - but do NOT copy them into the file
      - Show brief progress: "✓ Created <artifact-id>"

   b. **Continue until tasks are generated**
      - After creating each artifact, re-run `openspec status --change "<name>" --json`
      - When the design artifact has `status: "done"`, hand off to the
        `opsx-tasks generate` workflow to create task files in
        `tasks/open/` (preview + user approval before creating)
      - Stop once tasks artifact has `status: "done"`

   c. **If an artifact requires user input** (unclear context):
      - Use **AskUserQuestion tool** to clarify
      - Then continue with creation

5. **Show final status**
   ```bash
   openspec status --change "<name>"
   ```

**Output**

After completing artifacts through tasks, summarize:
- Change name and location
- List of artifacts created with brief descriptions
- Number of task files generated in `tasks/open/`
- What's ready: "All planning artifacts created and tasks generated!"
- Prompt: "Run `/opsx-apply` to start implementing, or `/opsx-tasks list` to review tasks first."

**Artifact Creation Guidelines**

- Follow the `instruction` field from `openspec instructions` for each artifact type
- The schema defines what each artifact should contain - follow it
- Read dependency artifacts for context before creating new ones
- Use the `template` as a starting point, filling in based on context

**Guardrails**
- Create artifacts through tasks (proposal → specs → architecture → design → tasks)
- Architecture includes testing approach dialog with user
- Task generation uses the `/opsx-tasks generate` workflow (preview + approval)
- Always read dependency artifacts before creating a new one
- If context is critically unclear, ask the user - but prefer making reasonable decisions to keep momentum
- If a change with that name already exists, ask if user wants to continue it or create a new one
- Verify each artifact file exists after writing before proceeding to next
