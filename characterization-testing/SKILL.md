---
name: characterization-testing
description: Guide for characterization testing - capturing current system behavior to enable safe refactoring. Use when working with legacy code, undocumented systems, or code that needs modification but lacks tests. Helps create a safety net before making changes by testing what the code actually does (not what it should do). Also known as Golden Master testing or Approval testing.
---

# Characterization Testing

## What is Characterization Testing

Characterization testing captures the current behavior of a system, documenting what the code actually does rather than what it should do. Unlike traditional testing that validates correctness against specifications, characterization tests create a snapshot of existing behavior to detect unintended changes during refactoring or modification.

This approach is particularly valuable for legacy code that lacks documentation or tests. The tests act as a safety net, allowing confident modification of code whose behavior may not be fully understood.

## When to Use Characterization Testing

Apply characterization testing when:

### Primary Use Cases
- **Legacy code refactoring**: Code that works but needs structural improvement
- **Undocumented systems**: Behavior is unclear or poorly documented
- **Pre-modification safety net**: Need to change code that lacks test coverage
- **Behavior documentation**: Capturing what a system does for future reference
- **Regression prevention**: Ensuring changes don't alter existing behavior

### Decision Criteria
Use characterization testing when you need to answer: "What does this code do right now?" rather than "Does this code meet the requirements?"

Do NOT use characterization testing for:
- New feature development (use TDD instead)
- Code with known bugs you want to fix immediately (fix first, then test)
- Well-tested code (add to existing tests instead)

## Core Workflow

Follow this workflow when applying characterization testing:

### 1. Identify Target Code
- Determine which code sections need characterization
- Focus on code you'll modify or need to understand
- Start with smaller, more manageable sections

### 2. Understand Current Behavior
- Run the code with various inputs
- Observe outputs, side effects, and state changes
- Note any surprising or unexpected behavior
- Document what the code actually does (even if incorrect)

### 3. Create Characterization Tests
- Capture observed behavior in tests
- Test what the code does, not what it should do
- Include edge cases and boundary conditions
- Document any known issues or quirks in test comments

### 4. Verify Test Coverage
- Ensure tests fail when code changes
- Run tests to establish baseline (all should pass)
- Verify tests detect intentional modifications
- Adjust tests if they miss important behaviors

### 5. Refactor with Confidence
- Make intended code changes
- Run characterization tests after each change
- Investigate any test failures (intended vs unintended)
- Keep tests updated as you understand behavior better

### 6. Evolve Tests
- Convert characterization tests to proper unit tests over time
- Fix captured bugs and update tests accordingly
- Add specification-based tests for new understanding
- Remove characterization tests when proper tests exist

## Key Principles

### Test What It Does, Not What It Should Do
Capture actual behavior, even if incorrect. The goal is to detect changes, not validate correctness. Document known issues in comments.

### Start Small, Expand Gradually
Begin with a small section of code. Characterize it completely before moving to the next section. This builds confidence and understanding incrementally.

### Minimal Refactoring to Enable Testing
Make only the smallest changes necessary to get code into a test harness. Break dependencies carefully and minimally. See techniques.md for dependency-breaking approaches.

### Accept Imperfect Tests Initially
Early characterization tests may be incomplete or awkward. They improve as understanding grows. Perfect is the enemy of good enough.

### Tests Are Temporary Scaffolding
Characterization tests are transitional. As you understand the code better, replace them with proper specification-based tests.

## Techniques

For detailed techniques on breaking dependencies, snapshot testing, and other advanced approaches, see:
- **[references/techniques.md](references/techniques.md)** - Breaking dependencies, snapshot testing, approval testing methods, minimal safe refactoring, and scratch refactoring

## Common Patterns

For guidance on specific scenarios and how to handle them, see:
- **[references/patterns.md](references/patterns.md)** - Legacy code refactoring, documenting undocumented behavior, regression prevention, and preparatory refactoring

## Workflow Template

For a step-by-step checklist to guide a characterization testing session, see:
- **[assets/workflow-template.md](assets/workflow-template.md)** - Structured checklist with questions and success criteria

## Progressive Approach

Characterization testing is iterative:

1. **Initial Pass**: Capture obvious behavior with simple tests
2. **Deeper Understanding**: Add tests for edge cases and complex interactions
3. **Refinement**: Improve test quality as understanding grows
4. **Transition**: Replace with specification-based tests when appropriate

Start with rough characterization and refine over time. Don't aim for perfection in the first iteration.

## Integration with Refactoring

Characterization testing enables the legacy code refactoring cycle:

1. **Characterize**: Create tests for current behavior
2. **Refactor**: Improve code structure safely
3. **Enhance**: Add new features with proper tests
4. **Iterate**: Repeat for other code sections

This cycle gradually transforms legacy code into well-tested, maintainable code while minimizing risk.

## Success Indicators

You're successfully using characterization testing when:

- Tests capture enough behavior to detect unintended changes
- You can refactor with confidence that tests will catch regressions
- Understanding of code behavior improves through test creation
- Tests serve as documentation of actual system behavior
- Gradual transition to specification-based tests occurs naturally

## Common Pitfalls

Avoid these mistakes:

### Testing Too Much at Once
Start small. Characterizing an entire system at once is overwhelming and error-prone.

### Trying to Fix Bugs Immediately
Resist the urge to fix discovered bugs while characterizing. Document them and address them later with proper tests in place.

### Over-Engineering Tests
Keep characterization tests simple. They're temporary scaffolding, not production test suites.

### Skipping Verification
Always verify tests actually detect changes by intentionally breaking code and ensuring tests fail.

### Treating as Permanent Tests
Plan for evolution. Characterization tests should transition to specification-based tests as understanding grows.
