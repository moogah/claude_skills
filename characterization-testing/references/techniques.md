# Characterization Testing Techniques

This document provides detailed techniques for applying characterization testing effectively.

## Table of Contents

1. Breaking Dependencies
2. Snapshot Testing
3. Approval Testing
4. Minimal Safe Refactoring
5. Scratch Refactoring
6. Combinatorial Testing
7. State Verification

## Breaking Dependencies

Dependencies make code difficult to test in isolation. Breaking them is often necessary to get legacy code into a test harness.

### Two Reasons to Break Dependencies

**Sensing**: Inspect behavior you can't otherwise observe (internal calculations, side effects, state changes)

**Separation**: Run code in a test harness outside of its production environment

### Principles for Breaking Dependencies

- Make the smallest change possible to enable testing
- Preserve existing behavior exactly
- Accept initial ugliness - tests enable future cleanup
- Document why dependencies were broken in specific ways

### Common Dependency-Breaking Techniques

#### Extract Interface/Protocol
When you need to replace a concrete dependency with a test double, define an interface that both production and test implementations satisfy.

#### Parameter Injection
Pass dependencies as parameters rather than creating them internally. This allows test code to provide test doubles.

#### Extract and Override
Move problematic code to a separate method that can be overridden in a test subclass. Use sparingly as it creates testing-specific inheritance.

#### Expose Internal State
Temporarily expose private state to enable verification. Mark as testing-only and plan to remove once proper tests exist.

#### Extract Method
Pull out a section of code into its own method to make it testable independently. Often the first step before other techniques.

### Dependency-Breaking Strategy

1. Identify the minimal dependency preventing testing
2. Choose the least invasive technique that enables testing
3. Apply the technique with surgical precision
4. Verify behavior is unchanged
5. Write characterization test
6. Iterate on other dependencies as needed

## Snapshot Testing

Snapshot testing captures complex output and compares it against future runs. Particularly useful when output is large, structured, or difficult to specify.

### When to Use Snapshot Testing

- Complex data structures or objects
- Large text output (HTML, JSON, XML)
- Generated files or documents
- UI rendering output
- Database state after operations

### Snapshot Testing Workflow

1. **Capture Initial Snapshot**: Run code and save output as baseline
2. **Review Snapshot**: Verify captured behavior is actually current behavior
3. **Run Tests**: Compare future outputs against baseline
4. **Investigate Differences**: Determine if changes are intended or bugs
5. **Update Snapshots**: Accept intended changes, fix bugs for unintended ones

### Snapshot Testing Principles

- Snapshots should be reviewable and human-readable when possible
- Store snapshots in version control
- Make snapshots as focused as possible (test smaller units)
- Document what each snapshot represents
- Plan to replace snapshots with specific assertions as understanding grows

### Handling Snapshot Changes

When snapshot tests fail:
- Review the diff carefully
- Determine if change was intentional
- If intentional: update snapshot
- If unintentional: fix the bug, keep snapshot
- If unsure: investigate before updating

### Snapshot Testing Limitations

- Can be brittle (sensitive to irrelevant changes)
- May hide important behavioral changes in noise
- Harder to understand than explicit assertions
- Can become maintenance burden if overused

Use for initial characterization, then refine into specific assertions.

## Approval Testing

Approval testing is a variant of snapshot testing where output is explicitly approved by a human reviewer.

### Approval Testing Workflow

1. **Generate Output**: Run code and capture output
2. **Review Output**: Human examines output in detail
3. **Approve**: If correct, approve output as baseline
4. **Compare**: Future runs compare against approved baseline
5. **Re-Review on Changes**: Any difference requires human review

### When Approval Testing Shines

- Complex algorithms with hard-to-specify output
- Visual output (images, rendered documents, UI)
- Large data transformations
- Generated code or text
- Systems with many edge cases

### Approval vs Snapshot Testing

- **Approval**: Requires explicit human review before becoming baseline
- **Snapshot**: Automatic capture without mandatory review

Use approval testing when human judgment is needed to verify correctness. Use snapshot testing when you just need to detect changes.

### Making Approval Testing Effective

- Keep approved outputs reviewable (format nicely, not binary when possible)
- Organize approvals by feature or scenario
- Document what makes output "correct" for each approval
- Establish process for when to re-approve changed output
- Consider pair review for complex approvals

## Minimal Safe Refactoring

Certain refactorings are mechanical and safe enough to apply without tests. Use these to create seams for testing.

### Safe Refactoring Techniques

These preserve behavior by definition:

- **Extract Method**: Pull code into a new method (no logic change)
- **Rename**: Change names without changing behavior
- **Extract Variable**: Name an expression without changing evaluation
- **Introduce Parameter**: Pass something instead of creating internally
- **Extract Constant**: Name a magic value

### Principles of Minimal Safe Refactoring

- Make only one change at a time
- Verify code still compiles/runs after each change
- Don't fix bugs or improve logic yet
- Stop as soon as you can get a test in place
- Accept temporary ugliness (tests enable future cleanup)

### Strategy

1. Identify the smallest refactoring that enables testing
2. Apply it mechanically (no logic changes)
3. Verify behavior unchanged (manual testing if necessary)
4. Write characterization test
5. Now you can refactor more confidently

### When NOT to Use Minimal Safe Refactoring

If the code is so tangled that even extract method is risky, consider other options:
- Characterize at a higher level first (integration tests)
- Use scratch refactoring to understand before committing
- Consider rewriting small sections with tests first

## Scratch Refactoring

Scratch refactoring is exploratory refactoring with the explicit goal of understanding, not keeping the changes.

### The Scratch Refactoring Process

1. **Branch or Copy**: Work in throwaway space
2. **Refactor Aggressively**: Rename, extract, reorganize without fear
3. **Take Notes**: Document insights about structure and behavior
4. **Revert Everything**: Delete all changes
5. **Apply Learnings**: Use insights to create targeted tests and minimal refactorings

### Goals of Scratch Refactoring

- Understand structure and dependencies
- Identify natural seams for testing
- Learn what the code actually does
- Discover hidden assumptions
- Build mental model before committing to changes

### Scratch Refactoring Principles

- Timeboxed: Set a limit (30-60 minutes)
- No pressure for perfection: Just exploration
- Must revert: This is not the final refactoring
- Take notes: Capture insights for later
- Focus on understanding, not production quality

### What to Look For

During scratch refactoring, note:
- Natural boundaries for testing
- Hidden dependencies that need breaking
- Surprising behavior or edge cases
- Opportunities for simplification
- Core logic vs boilerplate

### After Scratch Refactoring

With fresh understanding:
1. Write characterization tests at identified boundaries
2. Apply minimal safe refactorings to enable testing
3. Break dependencies discovered as problematic
4. Now confidently refactor with tests in place

## Combinatorial Testing

Legacy code often has complex conditional logic. Combinatorial testing identifies important combinations of inputs and states to characterize.

### Identifying Test Combinations

Look for:
- Multiple input parameters
- Conditional branches (if/else, switch)
- State that affects behavior
- Different execution paths

### Combinatorial Strategies

**All Pairs**: Test all pairs of parameter values (not all combinations)
- Catches most interaction bugs
- More practical than full combinatorial

**Boundary Values**: Focus on edge cases
- Minimum/maximum values
- Empty/null/zero cases
- Just above/below boundaries

**Equivalence Classes**: Group similar inputs
- Test one representative from each class
- Reduces redundant tests

### Practical Approach

1. List all parameters and possible values
2. Identify most important combinations (risk-based)
3. Start with boundary values and error cases
4. Add common valid cases
5. Add pairs of parameters that likely interact
6. Stop when diminishing returns (not full coverage)

### Combinatorial Testing for Characterization

Goal is not exhaustive testing, but:
- Capture enough behavior to detect changes
- Understand major execution paths
- Document important edge cases
- Create safety net for refactoring

Add more combinations as needed during refactoring.

## State Verification

Code with mutable state requires verifying state changes, not just return values.

### State Verification Techniques

**Direct Inspection**: Check state directly if accessible
- Read properties/fields after operation
- Query object state through public interface

**Indirect Verification**: Observe state through behavior
- Call methods that depend on state
- Check side effects (files, database, external systems)

**State Snapshots**: Capture entire state before and after
- Serialize object state
- Compare snapshots to detect changes

### State Verification Challenges

- State may not be directly observable (private fields)
- State may be distributed across multiple objects
- Timing issues with asynchronous state changes
- External state (filesystem, database) harder to verify

### Making State Observable

To verify hidden state:
- Extract and expose getters (temporarily if needed)
- Use reflection/introspection (language-dependent)
- Test at higher level where state becomes visible
- Break dependencies to observe state indirectly

### State Verification Strategy

1. Identify what state changes in the code
2. Determine how to observe those changes
3. Capture initial state (baseline)
4. Execute operation
5. Verify state changes match expected behavior
6. Document any surprising state interactions

## Technique Selection Guide

Choose techniques based on context:

**High dependency coupling**: Start with scratch refactoring, then minimal safe refactoring, then break dependencies

**Complex output**: Use snapshot or approval testing

**Complex logic with many paths**: Use combinatorial testing

**Stateful systems**: Focus on state verification techniques

**Well-structured but untested**: Snapshot testing may suffice

**Tangled mess**: Scratch refactoring first to understand, then characterize at higher level

Mix and match techniques as needed. Start with easiest technique that provides value.
