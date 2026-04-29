---
name: opsx-verify
description: Verify implementation matches change artifacts before archiving. Use when the user wants to validate that implementation is complete, correct, and coherent before archiving a change.
---

Verify that an implementation matches the change artifacts (specs, tasks, design).

**Input**: Optionally specify a change name after `/opsx-verify` (e.g., `/opsx-verify add-auth`). If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **If no change name provided, prompt for selection**

   Run `openspec list --json` to get available changes. Use the **AskUserQuestion tool** to let the user select.

   Show changes that have task files under `tasks/open/` or `tasks/closed/`.
   Include the schema used for each change if available.
   Mark changes with files in `tasks/open/` as "(In Progress)".

   **IMPORTANT**: Do NOT guess or auto-select a change. Always let the user choose.

2. **Check status to understand the schema**
   ```bash
   openspec status --change "<name>" --json
   ```
   Parse the JSON to understand:
   - `schemaName`: The workflow being used (e.g., "spec-driven")
   - Which artifacts exist for this change

3. **Scan task files**

   Read all task files in the change:

   - `openspec/changes/<name>/tasks/open/*.md`
   - `openspec/changes/<name>/tasks/closed/*.md`

   Parse frontmatter (name, description, status, relations) and body for each.

4. **Load artifacts**

   ```bash
   openspec instructions apply --change "<name>" --json
   ```

   This returns the change directory and context files. Read all available artifacts from `contextFiles`.

5. **Initialize verification report structure**

   Create a report structure with three dimensions:
   - **Completeness**: Track tasks and spec coverage
   - **Correctness**: Track requirement implementation and scenario coverage
   - **Coherence**: Track design adherence and pattern consistency

   Each dimension can have CRITICAL, WARNING, or SUGGESTION issues.

6. **Verify Completeness**

   **Task Completion:**
   - From the scan in step 3, count open vs closed task files
   - If open tasks exist:
     - Add CRITICAL issue for each open task
     - Include task name and description
     - Recommendation: "Complete task <name>: <description>" or
       "Move to tasks/closed/ if already implemented"

   **Spec Coverage**:
   - If delta specs exist in `openspec/changes/<name>/specs/`:
     - Extract all requirements (marked with "### Requirement:")
     - For each requirement:
       - Search codebase for keywords related to the requirement
       - Assess if implementation likely exists
     - If requirements appear unimplemented:
       - Add CRITICAL issue: "Requirement not found: <requirement name>"
       - Recommendation: "Implement requirement X: <description>"

6. **Verify Correctness**

   **Requirement Implementation Mapping**:
   - For each requirement from delta specs:
     - Search codebase for implementation evidence
     - If found, note file paths and line ranges
     - Assess if implementation matches requirement intent
     - If divergence detected:
       - Add WARNING: "Implementation may diverge from spec: <details>"
       - Recommendation: "Review <file>:<lines> against requirement X"

   **Test Coverage** (if architecture.md and tests exist):
   - Read architecture.md to determine test location and framework
   - Read test files from the specified location
   - For each scenario in delta specs (marked with "#### Scenario:"):
     - Check if test case exists covering the scenario
     - Look for test function names or comments referencing scenario
     - If test exists:
       - Run tests using command from architecture.md (if possible)
       - Check test status (passing/failing/skipped)
       - If test failing after implementation:
         - Add CRITICAL: "Test failing: <test-name> for scenario <scenario>"
         - Recommendation: "Fix implementation to make test pass"
       - If test skipped:
         - Add WARNING: "Test skipped: <test-name> for scenario <scenario>"
         - Recommendation: "Implement test or remove skip if complete"
     - If no test exists for scenario:
       - Add WARNING: "No test for scenario: <scenario name>"
       - Recommendation: "Add test case for scenario: <description>"

   **Test Execution** (if tests exist):
   - Attempt to run tests using command from architecture.md
   - Report pass/fail counts
   - If any tests fail:
     - Add CRITICAL for each failing test
     - Include failure message if available
   - If tests cannot be run (environment issue):
     - Add SUGGESTION: "Verify tests pass in proper environment"

7. **Verify Coherence**

   **Design Adherence**:
   - If design.md exists in contextFiles:
     - Extract key decisions (look for sections like "Decision:", "Approach:", "Architecture:")
     - Verify implementation follows those decisions
     - If contradiction detected:
       - Add WARNING: "Design decision not followed: <decision>"
       - Recommendation: "Update implementation or revise design.md to match reality"
   - If no design.md: Skip design adherence check, note "No design.md to verify against"

   **Code Pattern Consistency**:
   - Review new code for consistency with project patterns
   - Check file naming, directory structure, coding style
   - If significant deviations found:
     - Add SUGGESTION: "Code pattern deviation: <details>"
     - Recommendation: "Consider following project pattern: <example>"

9. **Generate Verification Report**

   **Summary Scorecard:**

   ```
   ## Verification Report: <change-name>

   ### Summary
   | Dimension    | Status                    |
   |--------------|---------------------------|
   | Completeness | X/Y tasks closed, N reqs  |
   | Correctness  | M/N reqs covered          |
   | Coherence    | Followed/Issues           |
   ```

   **Issues by Priority**:

   1. **CRITICAL** (Must fix before archive):
      - Open tasks in `tasks/open/`
      - Missing requirements
      - Each with specific, actionable recommendation

   2. **WARNING** (Should fix):
      - Spec/design divergences
      - Missing scenario coverage
      - Each with specific recommendation

   3. **SUGGESTION** (Nice to fix):
      - Pattern inconsistencies
      - Minor improvements
      - Each with specific recommendation

   **Final Assessment**:
   - If CRITICAL issues: "X critical issue(s) found. Fix before archiving."
   - If only warnings: "No critical issues. Y warning(s) to consider. Ready for archive (with noted improvements)."
   - If all clear: "All checks passed. Ready for archive."

**Verification Heuristics**

- **Completeness**: Focus on objective checklist items (checkboxes, requirements list)
- **Correctness**: Use keyword search, file path analysis, reasonable inference - don't require perfect certainty
- **Coherence**: Look for glaring inconsistencies, don't nitpick style
- **False Positives**: When uncertain, prefer SUGGESTION over WARNING, WARNING over CRITICAL
- **Actionability**: Every issue must have a specific recommendation with file/line references where applicable

**Graceful Degradation**

- If only tasks exist (no specs/design): verify task completion only, skip other checks
- If tasks + specs exist: verify completeness and correctness, skip design
- If full artifacts: verify all three dimensions
- Always note which checks were skipped and why

**Output Format**

Use clear markdown with:
- Table for summary scorecard
- Grouped lists for issues (CRITICAL/WARNING/SUGGESTION)
- Code references in format: `file.ts:123`
- Specific, actionable recommendations
- No vague suggestions like "consider reviewing"
