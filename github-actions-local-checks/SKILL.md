---
name: github-actions-local-checks
description: Proactively run GitHub Actions pipeline checks locally before pushing code. Use when making code changes to catch test, lint, and type-check failures early.
---

# GitHub Actions Local Checks

## Overview

When working in a repository with GitHub Actions, run relevant pipeline checks locally before pushing. This catches failures early and is much faster than waiting for CI to run and report errors.

## When to Use This Skill

- Before committing and pushing code changes
- After making significant changes to ensure tests still pass
- When adding new features or fixing bugs
- Before creating a pull request

## Identifying Steps to Run Locally

Look at workflow files in `.github/workflows/*.yml` to identify runnable steps:

### Run Locally (Fast Feedback)
- **Tests**: `npm test`, `npm run test:unit`, `jest --coverage`
- **Linting**: `npm run lint`, `eslint .`
- **Type Checking**: `npm run type-check`, `tsc --noEmit`
- **Prettier/Formatting**: `npx prettier --check .`
- **Build validation**: `npm run build` (if fast)

### Skip Locally (CI-Only)
- **Deployments**: `cdk deploy`, deployment scripts
- **Docker builds/pushes**: Slow and require credentials
- **Integration tests**: If they require external services
- **Codecov uploads**: CI handles this

## Workflow Pattern

1. **Identify the relevant workflow file**
   - For PRs: Usually `.github/workflows/run-tests.yml` or similar
   - For deployments: Usually `.github/workflows/deploy-*.yml`

2. **Find test/lint steps in the workflow**
   - Look for `run:` commands under `steps:`
   - Note the `working-directory:` if specified
   - Identify which commands to run

3. **Run commands locally**
   ```bash
   # Example: Run tests for a specific stack
   cd stacks/oncall-data-sync-stack
   npm run test -- --coverage

   # Example: Run linting
   npm run lint

   # Example: Type checking
   npm run type-check
   ```

4. **Fix any failures before pushing**

## Common Patterns

### Matrix strategies
If workflow uses matrix (e.g., testing multiple stacks), run checks for each affected area:
```yaml
matrix:
  stack: [stack-a, stack-b, stack-c]
```
→ Run tests in each stack directory you modified

### Working directories
Match the workflow's `working-directory`:
```yaml
- name: Run tests
  working-directory: ./stacks/my-stack
  run: npm test
```
→ Run `cd stacks/my-stack && npm test` locally

### Conditional steps
If workflow has `if:` conditions, check if they apply to your changes:
```yaml
- name: Integration tests
  if: github.event_name == 'push'
  run: npm run test:integration
```
→ Skip if you're just testing locally, but run if condition matches

## Example

**Scenario:** Modified code in `stacks/oncall-data-sync-stack/`

**Workflow file shows:**
```yaml
- name: Run unit tests
  working-directory: ./stacks/oncall-data-sync-stack
  run: npm run test -- --coverage
```

**Run locally:**
```bash
cd stacks/oncall-data-sync-stack
npm run test -- --coverage
```

**Result:** Catches test failures before pushing, saves time waiting for CI.

## Benefits

- **Faster feedback loop**: Seconds vs minutes waiting for CI
- **Cheaper**: No wasted CI compute on failures
- **Better workflow**: Fix issues immediately while context is fresh
- **Fewer failed builds**: Keep CI green
