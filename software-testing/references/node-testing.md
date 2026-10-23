# Node.js Testing

## Async Testing Patterns

### Async/Await (Preferred)

```javascript
test('fetches data', async () => {
  const result = await fetchData()
  expect(result).toEqual({ data: 'value' })
})
```

### Promises

```javascript
test('fetches data', () => {
  return fetchData().then(result => {
    expect(result).toEqual({ data: 'value' })
  })
})
```

## Testing File System Operations

### Option 1: Use Real Temp Files

```javascript
import { mkdtemp, rm } from 'fs/promises'
import { tmpdir } from 'os'
import { join } from 'path'

let tempDir

beforeEach(async () => {
  tempDir = await mkdtemp(join(tmpdir(), 'test-'))
})

afterEach(async () => {
  await rm(tempDir, { recursive: true })
})
```

### Option 2: Mock fs Module

```javascript
// Jest
jest.mock('fs/promises')

// Vitest
vi.mock('fs/promises', () => ({
  readFile: vi.fn(),
  writeFile: vi.fn()
}))
```

## Testing Streams

```javascript
import { Readable } from 'stream'

test('processes stream', async () => {
  const stream = Readable.from(['chunk1', 'chunk2'])
  const result = await processStream(stream)
  expect(result).toBe('chunk1chunk2')
})
```

## Testing Environment Variables

```javascript
const originalEnv = process.env.NODE_ENV

beforeEach(() => {
  process.env.NODE_ENV = 'test'
})

afterEach(() => {
  process.env.NODE_ENV = originalEnv
})
```

## Testing HTTP Servers

### Option 1: Supertest (Integration)

```javascript
import request from 'supertest'
import { app } from './app.js'

test('GET /users returns users', async () => {
  const response = await request(app).get('/users')
  expect(response.status).toBe(200)
  expect(response.body).toEqual([{ id: 1, name: 'Alice' }])
})
```

### Option 2: Mock HTTP Layer (Unit)

Mock fetch or http modules rather than spinning up real servers.
