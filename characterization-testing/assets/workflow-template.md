# Characterization Testing Session Checklist

Use this template to guide a characterization testing session. Check off items as you complete them.

## Session Setup

### Define Scope
- [ ] Identify specific code section to characterize
- [ ] Determine why characterization is needed (refactoring, bug fix, feature addition, understanding)
- [ ] Set boundaries (what's in scope, what's out of scope)
- [ ] Estimate effort (small: one method, medium: one class, large: one module)

### Gather Context
- [ ] Read the code to understand structure
- [ ] Review any existing documentation
- [ ] Check for existing tests (might provide insights)
- [ ] Identify dependencies (external services, database, filesystem, etc.)
- [ ] Note any obvious bugs (document, don't fix yet)

### Setup Test Environment
- [ ] Ensure code can be run in test harness
- [ ] Identify test framework to use
- [ ] Create test file/class for characterization tests
- [ ] Set up any necessary test fixtures or data

## Phase 1: Understand Current Behavior

### Exploration
- [ ] Run the code with typical inputs and observe outputs
- [ ] Try edge cases (null, empty, zero, negative, maximum values)
- [ ] Note any side effects (file creation, database changes, API calls)
- [ ] Identify state that affects behavior
- [ ] Test error conditions and invalid inputs
- [ ] Document surprising or unexpected behaviors

### Key Questions
- What does this code do with normal inputs?
- What does it do with edge cases?
- What side effects does it produce?
- What state does it depend on?
- What can go wrong?
- Are there any hidden behaviors?

## Phase 2: Create Characterization Tests

### Write Tests
- [ ] Test typical/common usage scenarios
- [ ] Test edge cases and boundary values
- [ ] Test error handling and invalid inputs
- [ ] Verify side effects (files created, state changed, etc.)
- [ ] Capture outputs (return values, exceptions, state changes)
- [ ] Use snapshot/approval testing if output is complex
- [ ] Add descriptive test names (describe scenario and behavior)
- [ ] Document known issues or bugs in test comments

### Test Quality Checks
- [ ] Each test has clear, descriptive name
- [ ] Tests are independent (can run in any order)
- [ ] Tests are deterministic (same result each run)
- [ ] Known issues are documented in comments
- [ ] Tests capture behavior, not implementation details

### Key Questions
- Do tests cover the important behaviors?
- Are test names clear and descriptive?
- Do tests accurately capture what code does now?
- Are surprises documented?
- Can tests detect changes?

## Phase 3: Verify Test Coverage

### Validation
- [ ] Run all tests (should all pass with current code)
- [ ] Intentionally break the code to verify tests catch changes
- [ ] Fix the intentional break, verify tests pass again
- [ ] Check test output is clear and actionable
- [ ] Review test coverage (not 100%, but sufficient for needs)
- [ ] Ensure tests run quickly enough for frequent use

### Coverage Assessment
- [ ] Critical behaviors are tested
- [ ] Common use cases are covered
- [ ] Important edge cases are tested
- [ ] Error conditions are verified
- [ ] Key side effects are checked

### Key Questions
- Do tests actually fail when code changes?
- Is coverage sufficient for planned work?
- Are tests fast enough to run frequently?
- Do test failures provide clear information?

## Phase 4: Refactor or Modify (Optional)

If proceeding with refactoring or modification:

### Before Refactoring
- [ ] All characterization tests passing
- [ ] Committed current state (version control)
- [ ] Clear goal for refactoring/modification
- [ ] Plan for smallest safe changes

### During Refactoring
- [ ] Make one small change at a time
- [ ] Run tests after each change
- [ ] Investigate any test failures immediately
- [ ] Keep tests green (unless change is intentional)
- [ ] Document any intentional behavior changes

### After Refactoring
- [ ] All tests passing (or updated intentionally)
- [ ] Code is in better state than before
- [ ] Behavior preservation verified
- [ ] Consider improving/replacing characterization tests

## Phase 5: Test Evolution

### Improve Tests
- [ ] Replace vague assertions with specific ones
- [ ] Convert snapshot tests to explicit assertions where appropriate
- [ ] Add specification-based tests for newly understood behavior
- [ ] Remove redundant characterization tests
- [ ] Improve test organization and naming
- [ ] Add proper documentation to important tests

### Decide Test Fate
For each characterization test, decide:
- [ ] **Keep as-is**: Still valuable for regression prevention
- [ ] **Improve**: Convert to proper unit test with better assertions
- [ ] **Replace**: Write specification-based test, delete characterization test
- [ ] **Delete**: No longer needed (behavior covered by other tests)

## Session Reflection

### Success Criteria
- [ ] Characterization tests capture important behaviors
- [ ] Tests enable confident modification of code
- [ ] Understanding of code has improved
- [ ] Tests serve as documentation
- [ ] Test suite is maintainable
- [ ] Any refactoring completed successfully

### Learning Capture
Document insights:
- What did you learn about the code?
- What surprised you?
- What patterns did you discover?
- What would you do differently next time?
- What technical debt did you identify?

### Next Steps
- [ ] Identify next area to characterize (if any)
- [ ] Plan for technical debt addressing
- [ ] Schedule test evolution (convert characterization to proper tests)
- [ ] Share learnings with team
- [ ] Update documentation based on discoveries

## Common Issues and Solutions

### Issue: Tests are too brittle
**Solution**: Test behaviors, not implementation details. Use more abstract assertions.

### Issue: Tests require too much setup
**Solution**: Consider testing at a higher level, or break dependencies to simplify.

### Issue: Can't figure out what code does
**Solution**: Use scratch refactoring to explore, or debug with various inputs.

### Issue: Behavior seems random or inconsistent
**Solution**: Identify hidden state or inputs affecting behavior. Control them in tests.

### Issue: Too many test cases needed
**Solution**: Use equivalence classes and representative examples. Don't test exhaustively.

### Issue: Tests take too long to run
**Solution**: Mock expensive operations, test at smaller units, or parallelize tests.

### Issue: Breaking dependencies is risky
**Solution**: Make smallest changes possible. Use extract-and-override or parameter injection.

## Tips for Success

- **Start small**: Characterize manageable chunks, not entire systems
- **Accept imperfection**: Tests will improve over time
- **Document surprises**: Unusual behavior should be noted
- **Run tests frequently**: Immediate feedback on changes
- **Iterate**: Characterization is an iterative process
- **Don't fix bugs yet**: Document them, fix after tests are in place
- **Keep tests simple**: They're temporary scaffolding, not production tests
- **Verify tests catch changes**: Always test your tests
- **Time-box exploration**: Don't get stuck understanding everything
- **Focus on needs**: Characterize what you need for current work

## Session Complete

Review this checklist and mark the session complete when:
- Tests capture sufficient behavior for intended work
- Tests are verified to catch changes
- Understanding has improved enough to proceed confidently
- Next steps are clear

Remember: Characterization testing is iterative. You can always return to add more tests as needs evolve.
