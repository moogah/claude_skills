---
name: characterization-testing
description: Guide for characterization testing - capturing current system behavior to enable safe refactoring. Use when working with legacy code, undocumented systems, or code that needs modification but lacks tests. Helps create a safety net before making changes by testing what the code actually does (not what it should do).
---

# Characterization Testing

## What is Characterization Testing

Characterization testing captures the current behavior of a system, documenting what the code actually does rather than what it should do. Unlike traditional testing that validates correctness against specifications, characterization tests create a snapshot of existing behavior to detect unintended changes during refactoring.

This approach is valuable for legacy code that lacks documentation or tests. The tests act as a safety net, allowing confident modification of code whose behavior may not be fully understood.

## When to Use

Apply characterization testing when:

- **Legacy code refactoring**: Code works but needs structural improvement
- **Undocumented systems**: Behavior is unclear or poorly documented
- **Pre-modification safety net**: Need to change code that lacks test coverage
- **Regression prevention**: Ensuring changes don't alter existing behavior

Use when you need to answer "What does this code do right now?" rather than "Does this code meet requirements?"

## Core Workflow

### 1. Identify Target Code
Determine which code sections need characterization. Focus on code you'll modify. Start with smaller, manageable sections.

### 2. Understand Current Behavior
Run the code with various inputs. Observe outputs, side effects, and state changes. Document what the code actually does (even if incorrect).

### 3. Create Characterization Tests
Capture observed behavior in tests. Test what the code does, not what it should do. Include edge cases and boundary conditions.

### 4. Verify Test Coverage
Ensure tests fail when code changes. Run tests to establish baseline. Intentionally break something to verify tests catch it.

### 5. Refactor with Confidence
Make intended code changes. Run tests after each change. Investigate any failures to determine if they're intended or bugs.

### 6. Evolve Tests
Convert characterization tests to proper unit tests over time. Fix captured bugs and update tests accordingly. Remove characterization tests when proper tests exist.

## Key Principles

**Test What It Does, Not What It Should Do**: Capture actual behavior, even if incorrect. The goal is detecting changes, not validating correctness.

**Start Small, Expand Gradually**: Begin with a small section. Characterize it completely before moving to the next. This builds confidence incrementally.

**Tests Are Temporary Scaffolding**: Characterization tests are transitional. As you understand the code better, replace them with proper specification-based tests.

## Example

You need to refactor a complex pricing calculation method:

1. Run the method with various inputs (standard product, discounted product, bulk quantity)
2. Write tests asserting these specific input/output pairs
3. Add tests for edge cases (null, zero, negative values)
4. Verify tests pass with current code
5. Intentionally break the calculation to verify tests fail
6. Fix the break, verify tests pass again
7. Now refactor the method (extract methods, improve names, simplify logic)
8. Run tests continuously - any failure requires investigation
9. Once satisfied with structure, improve tests with better assertions
10. Gradually replace with specification-based tests as understanding grows

## Common Pitfalls

**Testing too much at once**: Start small. Characterizing entire systems is overwhelming.

**Trying to fix bugs immediately**: Document bugs but fix them later once tests are in place.

**Treating as permanent tests**: Plan for evolution. These tests should transition to proper tests as understanding grows.
