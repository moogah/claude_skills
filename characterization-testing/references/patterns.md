# Characterization Testing Patterns

This document describes common scenarios where characterization testing applies and patterns for handling each situation.

## Table of Contents

1. Legacy Code Refactoring
2. Documenting Undocumented Behavior
3. Regression Prevention
4. Preparatory Refactoring Before Feature Addition
5. Understanding Inherited Code
6. Safe Bug Fixing
7. Incremental Modernization

## Legacy Code Refactoring

**Context**: Code works but structure is poor. Needs improvement without behavioral changes.

### Pattern

1. **Select Target**: Choose a specific area to refactor (method, class, module)
2. **Characterize Current Behavior**: Write tests capturing what it does now
3. **Verify Tests Catch Changes**: Intentionally break something, ensure tests fail
4. **Refactor Structure**: Improve design while keeping tests green
5. **Evolve Tests**: Replace characterization tests with proper unit tests
6. **Iterate**: Move to next area

### Key Considerations

- Start with code you need to modify, not the whole system
- Characterize at appropriate level (don't test too high or too low)
- Accept that tests will be imperfect initially
- Refactor incrementally, running tests after each change

### Example Workflow

You need to refactor a complex calculation method:

1. Run method with various inputs, capture outputs
2. Write tests asserting these specific input/output pairs
3. Add tests for edge cases (null, zero, negative, boundary values)
4. Verify tests pass with current code
5. Refactor method (extract methods, improve names, simplify logic)
6. Run tests continuously, investigate any failures
7. Once satisfied with structure, review tests for improvements
8. Replace "magic number" assertions with meaningful assertions

### Common Challenges

**Challenge**: Code has external dependencies (database, filesystem, network)
**Solution**: Break dependencies minimally, use test doubles for characterization

**Challenge**: Tests require too much setup
**Solution**: Characterize at a higher level initially, then work downward

**Challenge**: Behavior seems inconsistent or random
**Solution**: Investigate what state or inputs affect behavior, control them in tests

## Documenting Undocumented Behavior

**Context**: Code works but nobody knows exactly what it does. Need to document actual behavior.

### Pattern

1. **Treat Tests as Documentation**: Write tests that clearly show behavior
2. **Use Descriptive Test Names**: Test names describe scenarios and expected behavior
3. **Cover Representative Cases**: Don't test everything, focus on important scenarios
4. **Document Surprises**: Use comments for unexpected or counterintuitive behavior
5. **Organize by Scenario**: Group related tests by use case or feature

### Key Considerations

- Tests serve dual purpose: safety net and documentation
- Favor readability over DRY (some duplication acceptable for clarity)
- Use clear, business-meaningful test names
- Add comments explaining "why" not just "what"

### Example Workflow

You inherited a pricing calculation function:

1. Test with standard product (document happy path)
2. Test with discounted product (document discount behavior)
3. Test with bulk quantities (document volume pricing)
4. Test with invalid inputs (document error handling)
5. Test edge cases discovered through code reading (zero price, maximum quantity)
6. Document any surprising behavior in comments ("Note: function rounds down, not to nearest")

### Documentation Through Tests

Tests become living documentation:
- More trustworthy than comments (tests prove behavior)
- Automatically updated (tests break when behavior changes)
- Executable examples for future developers
- Specification of what code actually does

### Common Challenges

**Challenge**: Too many scenarios to test exhaustively
**Solution**: Focus on important scenarios and representative examples

**Challenge**: Hard to name tests descriptively
**Solution**: Use sentence-like names describing scenario and outcome

**Challenge**: Tests don't capture context or reasoning
**Solution**: Add comments explaining business rules or historical context

## Regression Prevention

**Context**: Code works in production. Any changes risk breaking working functionality.

### Pattern

1. **Identify Critical Paths**: What functionality must not break?
2. **Characterize Critical Behavior**: Test these paths thoroughly
3. **Establish Baseline**: All tests pass with current code
4. **Make Changes**: Modify code as needed
5. **Verify No Regressions**: Tests should remain green (unless change is intentional)
6. **Investigate Failures**: Determine if failure is expected or a regression

### Key Considerations

- Focus on user-visible behavior and critical business logic
- Test at appropriate level (often integration/system level)
- Maintain test suite as changes occur
- Balance coverage with maintenance burden

### Example Workflow

You need to add a feature to a working checkout process:

1. Write tests for current checkout flow (normal purchase, multiple items, discount codes)
2. Test edge cases (empty cart, out of stock, payment failures)
3. Run tests to establish baseline
4. Add new feature (keeping tests green)
5. Add new tests for new feature
6. Run full suite to ensure no regressions
7. Deploy with confidence

### Regression Testing Strategy

**Prioritize**: Test most important and most fragile code first
**Automate**: Regression tests should run automatically
**Maintain**: Update tests when behavior intentionally changes
**Expand**: Add tests when bugs are found

### Common Challenges

**Challenge**: Too many scenarios to protect against regressions
**Solution**: Risk-based testing - focus on high-impact areas

**Challenge**: Tests become maintenance burden as code evolves
**Solution**: Delete characterization tests once proper tests exist

**Challenge**: Hard to distinguish intentional changes from regressions
**Solution**: Clear test names and documentation of expected behavior

## Preparatory Refactoring Before Feature Addition

**Context**: Need to add a feature to existing code. Current structure makes feature addition difficult.

### Pattern

1. **Characterize Relevant Code**: Test code that will be modified
2. **Refactor for Feature**: Restructure to make feature addition straightforward
3. **Verify Behavior Unchanged**: Tests should remain green
4. **Add Feature**: Implement new functionality (now easier)
5. **Add Feature Tests**: Write proper tests for new behavior
6. **Clean Up**: Remove characterization tests if no longer needed

### Key Considerations

- Characterize only code you'll modify (not entire system)
- Refactor just enough to enable feature (don't over-engineer)
- Keep characterization and feature work separate
- Two-phase approach: refactor with green tests, then add feature

### Example Workflow

You need to add tax calculation to a pricing function that's currently hard to extend:

1. Characterize existing pricing function (test current behavior)
2. Refactor to extract pricing steps into separate, composable methods
3. Run characterization tests (should still pass)
4. Add tax calculation as a new step
5. Write proper tests for tax calculation
6. Optionally refactor characterization tests into proper unit tests
7. Delete characterization tests if they're now redundant

### Preparatory Refactoring Principles

**Make the change easy, then make the easy change**: First refactor for clarity, then add feature

**Keep tests green**: Characterization tests enable confident refactoring

**Refactor, then add**: Don't mix refactoring with feature work

**Test new code properly**: Characterization is for old code, use TDD for new features

### Common Challenges

**Challenge**: Unclear how to refactor for the feature
**Solution**: Use scratch refactoring to explore, then apply learnings

**Challenge**: Refactoring breaks too many things
**Solution**: Smaller refactoring steps, or characterize at higher level

**Challenge**: Feature addition still requires touching untested code
**Solution**: Characterize that code too, iterate until feature area is safe

## Understanding Inherited Code

**Context**: New to a codebase. Need to understand what code does before modifying it.

### Pattern

1. **Read Code**: Get general understanding
2. **Form Hypotheses**: Guess what code does
3. **Test Hypotheses**: Write characterization tests to verify guesses
4. **Discover Surprises**: Note where behavior differs from expectations
5. **Document Learning**: Tests capture your understanding
6. **Build Confidence**: Tests enable future changes

### Key Considerations

- Tests serve as learning tool and documentation
- Start with high-level behavior, drill down as needed
- Don't assume code is correct (test what it does, not what it should do)
- Accept that understanding builds gradually

### Example Workflow

You inherited a data transformation function:

1. Read the code, form hypothesis about transformation
2. Write test with sample input, guess output
3. Run test, see actual output
4. Adjust understanding and test
5. Test additional cases to refine understanding
6. Document surprising behaviors in comments
7. Now you understand enough to modify safely

### Learning Through Characterization

Tests help learning by:
- Making implicit behavior explicit
- Revealing edge cases and special handling
- Showing how different components interact
- Creating experiments to test understanding

### Common Challenges

**Challenge**: Code is too complex to understand through testing alone
**Solution**: Combine with scratch refactoring, debugger exploration, and asking experts

**Challenge**: Tests reveal the code is buggy
**Solution**: Document bugs in test comments, fix later once safety net is established

**Challenge**: Behavior varies based on hidden state
**Solution**: Investigate what state matters, control it in tests

## Safe Bug Fixing

**Context**: Bug found in production code. Need to fix without introducing new bugs.

### Pattern

1. **Characterize Working Behavior**: Test scenarios that currently work correctly
2. **Write Failing Test for Bug**: Create test that demonstrates the bug
3. **Fix Bug**: Modify code to make bug test pass
4. **Verify No Regressions**: Characterization tests should still pass
5. **Keep Bug Test**: It documents the bug and prevents recurrence
6. **Clean Up**: Refactor if needed, with full test suite protecting

### Key Considerations

- Characterize before fixing (establish what should keep working)
- Bug test should fail before fix, pass after fix
- Test both the bug scenario and nearby working scenarios
- Keep bug test permanently as regression test

### Example Workflow

Bug report: function returns wrong result for negative inputs

1. Write tests for positive inputs (characterize working cases)
2. Write test for negative input showing bug (test fails as expected)
3. Fix the bug
4. Run all tests (bug test passes, characterization tests still pass)
5. Consider adding more negative input cases
6. Refactor if needed (tests protect)

### Safe Bug Fixing Principles

**Characterize first**: Know what works before changing anything

**Reproduce bug in test**: If you can't write a failing test, you don't understand the bug

**Fix minimally**: Smallest change to fix bug

**Verify no new bugs**: Characterization tests catch new problems

### Common Challenges

**Challenge**: Can't reproduce bug in test
**Solution**: Bug may depend on state, timing, or environment - investigate further

**Challenge**: Fix breaks other functionality
**Solution**: Good thing you had characterization tests! Understand why and fix properly

**Challenge**: Bug is deeply embedded in tangled code
**Solution**: Characterize at higher level, make minimal fix, plan refactoring later

## Incremental Modernization

**Context**: Old codebase needs modernization (new language features, libraries, patterns). Can't rewrite everything at once.

### Pattern

1. **Select Modernization Target**: Choose one area to modernize
2. **Characterize Current Behavior**: Ensure you understand what it does
3. **Modernize Implementation**: Apply new patterns/features
4. **Verify Behavior Unchanged**: Tests should remain green
5. **Improve Tests**: Update to use modern testing practices
6. **Iterate**: Move to next area

### Key Considerations

- Incremental approach (modernize piece by piece)
- Behavior preservation (tests ensure equivalence)
- Mix old and new code temporarily (that's okay)
- Gradually improve testing along with code

### Example Workflow

Modernizing legacy callback-based code to use async/await:

1. Characterize current behavior with callback tests
2. Refactor one function to async/await
3. Update calling code to await instead of using callbacks
4. Run tests (should still pass)
5. Improve tests to use async testing patterns
6. Move to next function

### Incremental Modernization Strategy

**Plan the path**: Identify order for modernization (dependencies matter)

**Small steps**: Modernize small pieces, not entire subsystems

**Mixed state is okay**: Old and new patterns can coexist temporarily

**Test continuously**: Each step should maintain green tests

**Document progress**: Track what's modernized and what remains

### Common Challenges

**Challenge**: Modernization requires large-scale changes
**Solution**: Find smaller modernization steps, or use adapter pattern temporarily

**Challenge**: New and old approaches conflict
**Solution**: Isolate modernized code, use adapters to bridge differences

**Challenge**: Tests themselves are legacy and hard to maintain
**Solution**: Modernize tests alongside code, or rewrite tests using modern practices

## Pattern Selection Guide

Choose patterns based on your goal:

**Goal: Improve code structure** → Legacy Code Refactoring

**Goal: Understand code** → Understanding Inherited Code or Documenting Undocumented Behavior

**Goal: Protect against breaking changes** → Regression Prevention

**Goal: Add feature to difficult code** → Preparatory Refactoring Before Feature Addition

**Goal: Fix bug safely** → Safe Bug Fixing

**Goal: Modernize old code** → Incremental Modernization

Patterns often combine. For example:
- Understanding code + Refactoring
- Regression prevention + Bug fixing
- Preparatory refactoring + Feature addition

Use characterization testing as the foundation, combine patterns as needed.
