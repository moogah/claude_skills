# React Testing

## Query Priority

Use queries in this priority order (most to least preferred):

1. **getByRole** - Accessible to assistive tech, preferred
2. **getByLabelText** - Forms and inputs
3. **getByPlaceholderText** - If no label
4. **getByText** - Non-interactive content
5. **getByTestId** - Last resort only

**Good:**
```javascript
const button = screen.getByRole('button', { name: /submit/i })
const input = screen.getByLabelText('Email')
```

**Avoid:**
```javascript
const button = screen.getByTestId('submit-button')  // Last resort
const input = container.querySelector('#email')     // Never
```

## Query Variants

### Sync Queries

- **getBy\***: Throws if not found (use for elements that should exist)
- **queryBy\***: Returns null if not found (use to assert non-existence)
- **findBy\***: Returns promise (use for async elements)

### Multiple Elements

- **getAllBy\***, **queryAllBy\***, **findAllBy\*** - Return arrays

## Async Testing

```javascript
// Wait for element to appear
const element = await screen.findByText('Loaded')

// Wait for element to disappear
await waitForElementToBeRemoved(() => screen.queryByText('Loading'))

// Wait for arbitrary condition
await waitFor(() => {
  expect(screen.getByRole('alert')).toHaveTextContent('Success')
})
```

## User Interactions

Use `@testing-library/user-event` over `fireEvent`:

```javascript
import userEvent from '@testing-library/user-event'

const user = userEvent.setup()
await user.click(button)
await user.type(input, 'text to type')
await user.selectOptions(select, ['option1'])
```

## Testing Principles

- **Test behavior users see**, not implementation details
- **Don't test state directly**, test what renders
- **Avoid testing props**, test rendered output
- **Query by accessible attributes** (role, label), not test IDs when possible
