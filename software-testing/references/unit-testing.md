# Unit Testing

## Core Principles

### Single Responsibility Principle

Each test should verify one behavior. If a test fails, it should be immediately clear what broke.

**Good:**
```javascript
test('validates email format', () => {
  expect(isValidEmail('user@example.com')).toBe(true)
})

test('rejects email without @', () => {
  expect(isValidEmail('userexample.com')).toBe(false)
})
```

**Avoid:**
```javascript
test('email validation works', () => {
  expect(isValidEmail('user@example.com')).toBe(true)
  expect(isValidEmail('userexample.com')).toBe(false)
  expect(isValidEmail('')).toBe(false)
  // Too many behaviors in one test
})
```

### Test Behavior, Not Implementation

Focus on what the code does (outputs, side effects), not how it does it.

**Good:** Testing that a function returns the correct result
**Avoid:** Testing that a function calls specific internal methods

### Arrange-Act-Assert (AAA)

Structure tests in three clear phases:
1. **Arrange**: Set up test data and conditions
2. **Act**: Execute the code under test
3. **Assert**: Verify the expected outcome

## What to Test

- **Public interfaces**: Functions, methods, components that other code depends on
- **Business logic**: Calculations, validations, transformations
- **Edge cases**: Boundary conditions, empty inputs, null/undefined
- **Error handling**: Expected failures and error messages

## What NOT to Test

- **Implementation details**: Private methods, internal state
- **Framework internals**: React's rendering, library behavior
- **Third-party libraries**: Assume they work (test your usage of them)
- **Trivial code**: Simple getters, constants, pass-through functions

## Isolation and Mocking

### When to Mock

- External dependencies (APIs, databases, file system)
- Non-deterministic behavior (dates, random numbers)
- Slow operations (network calls, large computations)
- Dependencies that make testing difficult

### When NOT to Mock

- Simple, pure functions
- Code you own that's easy to test directly
- When mocking makes tests more complex than testing the real thing

**Prefer real implementations when practical.** Over-mocking can lead to tests that pass but don't reflect actual behavior.
