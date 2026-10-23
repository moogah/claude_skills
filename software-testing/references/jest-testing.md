# Jest Testing

## Running Tests

```bash
# Run specific test file
jest path/to/test.js

# Run tests matching pattern
jest --testNamePattern="login"

# Watch mode
jest --watch

# Coverage
jest --coverage

# Update snapshots
jest -u
```

## Jest-Specific Patterns

### Snapshot Testing

Use sparingly. Good for component output, bad for complex objects that change frequently.

```javascript
expect(component).toMatchSnapshot()
```

### Mock Functions

```javascript
const mockFn = jest.fn()
mockFn.mockReturnValue(42)
mockFn.mockResolvedValue({ data: 'value' })

expect(mockFn).toHaveBeenCalledWith('expected-arg')
expect(mockFn).toHaveBeenCalledTimes(1)
```

### Timers

```javascript
jest.useFakeTimers()
jest.advanceTimersByTime(1000)
jest.runAllTimers()
```

## Common Jest Matchers

```javascript
expect(value).toBe(42)                    // Strict equality
expect(value).toEqual({ a: 1 })           // Deep equality
expect(value).toBeNull()
expect(value).toBeDefined()
expect(array).toContain('item')
expect(string).toMatch(/pattern/)
expect(fn).toThrow('error message')
```
