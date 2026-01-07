---
name: software-testing
description: Software testing guidance with decision tree. Use when: (1) Writing or improving tests, (2) User asks about testing approaches, (3) Need guidance on test scope or assertions, (4) Working with specific test frameworks. Routes to specialized testing strategies via reference files.
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
