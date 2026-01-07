---
name: software-testing
description: "Software testing guidance with decision tree. Use when: (1) Writing or improving tests, (2) User asks about testing approaches, (3) Need guidance on test scope or assertions, (4) Working with specific test frameworks. Routes to specialized testing strategies via reference files."
---

# Software Testing

## Testing Decision Tree

```
Need to write tests?
    ↓
    Legacy/undocumented code needing refactoring?
    ├─ YES → Use characterization-testing skill
    └─ NO  → Continue
              ↓
              What type of testing guidance needed?
              │
              ├─ Unit testing principles?
              │  └─→ See references/unit-testing.md
              │
              ├─ Using Jest?
              │  └─→ See references/jest-testing.md
              │
              ├─ Using Vitest?
              │  └─→ See references/vitest-testing.md
              │
              ├─ Testing React components?
              │  └─→ See references/react-testing.md
              │
              └─ Testing Node.js code?
                 └─→ See references/node-testing.md
```

## Core Testing Principles

Apply these principles across all testing approaches:

### Keep Test Scope Small

Run only the tests you need. Prefer narrow scope over full test suites.

**Good:**
```bash
npm test src/auth/login.test.js
npm test -- --testNamePattern="login validation"
```

**Avoid:**
```bash
npm test  # Running entire suite unnecessarily
```

### Use Sub Agents for Test Execution

When running tests that produce verbose output, use a sub agent to preserve context in the main conversation. The sub agent runs the tests and summarizes only the important information.

**When to use sub agents:**
- Running test suites with many tests
- Tests that produce verbose output or stack traces
- When context preservation is important

**How to use sub agents:**
```
Use Task tool with subagent_type="general-purpose"
Ask the sub agent to:
1. Determine the correct test command for the project
2. Run the tests for the specified file(s)
3. Report the EXACT commands executed
4. Summarize results as:
   - How many test suites and tests ran
   - How many passed
   - What tests failed (with error messages and stack traces)
```

**Example:**
```
Task(
  description: "Run and summarize component tests",
  subagent_type: "general-purpose",
  prompt: "Run tests for src/components/Button.test.js.

           First, determine the correct test command (check if this is an Nx monorepo,
           package.json scripts, etc.).

           Then run the tests and provide:
           1. The EXACT command(s) you executed
           2. How many test suites and tests ran
           3. How many passed
           4. What tests failed (with error messages and stack traces)

           Including the exact commands is important for debugging."
)
```

**Benefits:**
- Preserves main conversation context
- Filters verbose test output to essential information
- Maintains focus on what matters (failures, error messages)
- Provides exact commands for debugging and reproducibility

### Use Specific Assertion Values

Write assertions with actual expected values, not generic placeholders.

**Good:**
```javascript
expect(user.name).toBe('Alice')
expect(getUserById).toHaveBeenCalledWith('user-123')
```

**Avoid:**
```javascript
expect(users.length).toBe(2)
expect(getUserById).toHaveBeenCalledWith(expect.any(String))
```

### Check Repo-Specific Skills First

Before running tests:
1. Check what Project skills are available for testing guidance
3. Check test configuration files
4. Ask user if uncertain

## Reference Guide

- **references/unit-testing.md** - Unit testing principles (SRP, isolation, mocking, what to test)
- **references/jest-testing.md** - Jest commands, matchers, and patterns
- **references/vitest-testing.md** - Vitest commands and differences from Jest
- **references/react-testing.md** - React Testing Library query priorities and patterns
- **references/node-testing.md** - Node.js-specific testing (async, streams, file system)
