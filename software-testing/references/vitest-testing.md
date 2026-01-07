# Vitest Testing

## Running Tests

```bash
# Run all tests (watch mode by default)
vitest

# Run tests once
vitest run

# Run specific test file
vitest run path/to/test.js

# Coverage
vitest --coverage

# UI mode
vitest --ui
```

## Key Differences from Jest

### Watch Mode Default

Vitest runs in watch mode by default (unlike Jest). Use `vitest run` for CI.

### Import from 'vitest'

```javascript
import { describe, test, expect, vi } from 'vitest'

// 'vi' is Vitest's equivalent to Jest's 'jest' global
const mockFn = vi.fn()
vi.useFakeTimers()
```

### Native ESM Support

Vitest has better native ES modules support. No need for transform configuration for modern JavaScript.

### Faster Execution

Vitest uses Vite's transformation pipeline and runs tests in parallel by default, making it significantly faster than Jest for most projects.

## Vitest-Specific Features

### Browser Mode

```javascript
import { page } from '@vitest/browser/context'

test('interactive test', async () => {
  await page.click('#button')
})
```

### Concurrent Tests

```javascript
test.concurrent('runs in parallel', async () => {
  // Test runs concurrently with other concurrent tests
})
```
