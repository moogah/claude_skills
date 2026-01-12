---
name: github-actions-troubleshooting
description: Troubleshoot and fix failing GitHub Actions pipeline steps. Use when user reports a failing pipeline with error output.
---

# GitHub Actions Troubleshooting

## Overview

When a GitHub Actions pipeline fails, systematically identify the problem, locate the failing step, and fix it. This skill helps interpret error output and determine the right fix.

## When to Use This Skill

- User reports a failing GitHub Actions workflow
- User provides error output from CI
- User mentions a specific failing step (e.g., "Stack Unit Tests failed")
- Need to fix pipeline configuration issues

## Troubleshooting Workflow

### Step 1: Parse the Error Output

Look for key information in the error:
- **Which workflow file**: Often shown in logs or URL
- **Step name**: e.g., "Stack Unit Tests", "Run unit tests"
- **Line number**: e.g., "line 232 of deploy-stacks-main.yml"
- **Error message**: The actual failure (test failure, command not found, etc.)

**Example error:**
```
Run npm run test -- --coverage

> oncall-data-sync-stack@1.0.0 test
> jest --coverage

No tests found, exiting with code 1
```

**Parse this as:**
- Command: `npm run test -- --coverage`
- Problem: No tests found, jest exiting with code 1
- Likely fix needed: Add `--passWithNoTests` flag

### Step 2: Locate the Failing Step

Read the workflow file at the specified line:

```bash
# If user mentions line 232 of deploy-stacks-main.yml
```

Look for:
- The `name:` of the step
- The `run:` command being executed
- Any `working-directory:` specified
- Conditional `if:` statements

**Example:**
```yaml
- name: Stack Unit Tests
  working-directory: ./stacks/${{ matrix.stack_env.stack }}
  run: npm run test -- --coverage
```

### Step 3: Identify the Root Cause

Common failure patterns:

**Tests not found:**
- Symptom: "No tests found, exiting with code 1"
- Cause: Test files deleted or no tests exist
- Fix: Add `--passWithNoTests` flag to jest command

**Module not found:**
- Symptom: "Cannot find module './some-file'"
- Cause: File deleted but still imported
- Fix: Remove import or restore file

**Command not found:**
- Symptom: "command not found: some-command"
- Cause: Missing dependency or wrong command name
- Fix: Install dependency or correct command

**Wrong working directory:**
- Symptom: "package.json not found" or similar
- Cause: Command run in wrong directory
- Fix: Update `working-directory` in workflow

### Step 4: Determine Where to Fix

**Fix in workflow file** when:
- Issue is with how the step runs (flags, directory, conditions)
- Need to add/remove pipeline-specific configuration
- Example: Adding `--passWithNoTests` to workflow run command

**Fix in package.json** when:
- Issue affects both local and CI execution
- Test script needs permanent modification
- Want consistent behavior everywhere
- Example: Changing test script to include `--passWithNoTests` by default

**Fix in source code** when:
- Actual code/test failure
- Import errors, type errors, etc.
- Test assertions failing

### Step 5: Apply the Fix

Update the appropriate file(s) and verify:

**Workflow file fix:**
```yaml
# Before
run: npm run test -- --coverage

# After
run: npm run test -- --coverage --passWithNoTests
```

**Package.json fix:**
```json
{
  "scripts": {
    "test": "jest --passWithNoTests"
  }
}
```

## Common Issues and Fixes

### Empty Test Suite
**Error:** "No tests found, exiting with code 1"
**Fix:** Add `--passWithNoTests` to jest command (workflow or package.json)

### Multiple Workflow Files
**Error:** Unclear which workflow is failing
**Fix:** Check error output for workflow name, or look at GitHub Actions UI for which workflow ran

### Matrix Strategy Failures
**Error:** Only one matrix item failing
**Fix:** cd to that specific directory and run the command locally to debug

### Cached Dependencies
**Error:** "Module not found" but package.json has it
**Fix:** May be cache issue - workflow often has cache step, check cache key

## Example Troubleshooting Session

**User reports:**
> "Stack Unit Tests failing with 'No tests found' error"

**Actions:**
1. Ask for error output or workflow file name
2. User provides: "Line 232 of deploy-stacks-main.yml"
3. Read that file at line 232:
   ```yaml
   - name: Stack Unit Tests
     run: npm run test -- --coverage
   ```
4. Identify issue: jest with no tests exits with code 1
5. Determine fix location: Both package.json (permanent) and workflow (explicit)
6. Apply fix:
   - Update package.json: `"test": "jest --passWithNoTests"`
   - Update workflow: `run: npm run test -- --coverage --passWithNoTests`
7. Verify fix will work

## Best Practices

- **Read the actual workflow file** - Don't guess, verify what's running
- **Check working directory** - Commands run in specific contexts
- **Consider both locations** - Sometimes fix in package.json is cleaner
- **Test locally first** - Run the same command locally to verify fix
- **Update related workflows** - If multiple workflows run same command
