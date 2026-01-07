# Concrete Examples

This document provides 20 concrete scenarios demonstrating when to capture learnings and when to filter them out. Each example includes context, analysis, and the appropriate action.

## Examples: SHOULD CAPTURE

### Example 1: Repeated Testing Command Correction

**Context:**
User asks Claude to run tests before deploying 3 times. Each time Claude runs `npm test`, user corrects: "No, use `npm run test:integration -- --coverage`"

**Analysis:**
- Repetition: 3 times ✅
- Non-obvious: Specific command with flags ✅
- Future applicability: Will apply to all test runs ✅
- Complexity: Meaningful (specific flags matter) ✅

**Decision:** CREATE new local skill: `repo-testing-workflow`

**Skill content:**
```markdown
---
name: repo-testing-workflow
description: Testing commands for this repository. Use when running tests or preparing for deployment.
---

# Repository Testing Workflow

## Test Commands

**Integration tests with coverage:**
```bash
npm run test:integration -- --coverage
```

Always run this before deployment.
```

---

### Example 2: Prettier Formatting Preferences

**Context:**
User asks Claude to format code 3 different times across 2 sessions. Each time specifies: "Use Prettier with --single-quote and --trailing-comma all and --print-width 100"

**Analysis:**
- Repetition: 3 times across sessions ✅
- Non-obvious: Specific user preference ✅
- Future applicability: Will apply to all formatting ✅
- Generally applicable: Not repo-specific ✅

**Decision:** CREATE new user-level skill: `code-formatting-preferences`

**Skill content:**
```markdown
---
name: code-formatting-preferences
description: User's preferred code formatting settings. Use when formatting code or setting up formatting tools.
---

# Code Formatting Preferences

## Prettier Configuration

When using Prettier, apply these settings:
- `--single-quote`: Use single quotes
- `--trailing-comma all`: Trailing commas everywhere
- `--print-width 100`: 100 character line width
```

---

### Example 3: Database Connection Struggle Resolved

**Context:**
Claude struggles to help with database query. User provides: "Connection string is in config/database.yml, use the 'analytics' database. Schema is in docs/schema.sql. Always use read-only connection for queries."

**Analysis:**
- Explicit statement of important pattern ✅
- Non-obvious: Repo-specific DB setup ✅
- Future applicability: All future DB queries ✅
- References specific files: config/database.yml ✅

**Decision:** CREATE new local skill: `project-database-access`

**Skill content:**
```markdown
---
name: project-database-access
description: Database access patterns for this project. Use when querying the database or helping with data analysis.
---

# Project Database Access

## Connection Setup

- **Config location:** `config/database.yml`
- **Database name:** `analytics`
- **Connection type:** Read-only for queries
- **Schema documentation:** `docs/schema.sql`

## Guidelines

Always use read-only connection when running queries to avoid accidental modifications.
```

---

### Example 4: API Error Handling Pattern

**Context:**
User corrects Claude's error handling 2 times: "In this repo, we always wrap API errors in our custom ApiError class from utils/errors.ts and include the request ID. Never use generic Error."

**Analysis:**
- Explicit correction: 2 times ✅
- Non-obvious: Repo-specific convention ✅
- Future applicability: All API error handling ✅
- References specific file: utils/errors.ts ✅

**Decision:** CREATE new local skill: `repo-error-handling`

**Skill content:**
```markdown
---
name: repo-error-handling
description: Error handling conventions for this repository. Use when writing error handling code or debugging API issues.
---

# Repository Error Handling

## API Errors

Always use the custom `ApiError` class from `utils/errors.ts`:

```typescript
import { ApiError } from './utils/errors'

throw new ApiError('Error message', { requestId })
```

Never use generic `Error` class for API errors.
```

---

### Example 5: User Requests PDF Watermark Feature

**Context:**
User uses pdf skill successfully, then asks: "Can you add watermark to PDFs? This seems useful to add to the pdf skill." Claude implements watermark functionality successfully.

**Analysis:**
- Explicit request to update skill ✅
- Related to existing skill domain ✅
- Extends without making unfocused ✅
- Natural fit with other PDF operations ✅

**Decision:** UPDATE existing user-level skill: `pdf`

**Action:** Read pdf/SKILL.md, add new section:
```markdown
## Add Watermark

Add text or image watermark to PDF pages...
[implementation details]
```

---

### Example 6: CloudWatch Logging Convention

**Context:**
During debugging, user mentions: "We always log to CloudWatch in this repo, not stdout. Use the logger from utils/logger.ts with the service name."

**Analysis:**
- Explicit statement ✅
- Non-obvious: Repo-specific pattern ✅
- Future applicability: All logging code ✅
- References specific file: utils/logger.ts ✅

**Decision:** CREATE new local skill: `repo-logging-practices`

**Action:** After debugging completes, suggest at natural stopping point

---

### Example 7: Git Commit Message Style

**Context:**
User asks Claude to create commits 4 times. Each time, user modifies commit message to match pattern: `[TYPE] Brief description` where TYPE is FEAT/FIX/DOCS/REFACTOR. User says: "I always use this format"

**Analysis:**
- Repetition: 4 times + explicit statement ✅
- Non-obvious: User preference ✅
- Future applicability: All commits ✅
- Generally applicable: Across all projects ✅

**Decision:** CREATE new user-level skill: `git-commit-style`

---

### Example 8: React Component File Structure

**Context:**
User corrects Claude 3 times: "In this repo, React components go in components/[feature]/ComponentName/index.tsx with styles in styles.module.css in same directory"

**Analysis:**
- Repetition: 3 times ✅
- Non-obvious: Repo-specific structure ✅
- Future applicability: All new components ✅
- Team convention ✅

**Decision:** CREATE new local skill: `repo-component-structure`

---

### Example 9: Environment Variable Pattern

**Context:**
User corrects Claude 2 times: "All environment variables must be prefixed with MYAPP_ and validated in config/env.ts with zod schemas. Never use process.env directly."

**Analysis:**
- Repetition: 2 times with emphasis ✅
- Non-obvious: Repo-specific pattern ✅
- Future applicability: All env var usage ✅
- References specific file: config/env.ts ✅

**Decision:** CREATE new local skill: `repo-environment-config`

---

### Example 10: Deployment Approval Workflow

**Context:**
User walks Claude through deployment 2 times: "Deploy to staging, send Slack message to #deployments channel with staging URL, wait for QA approval, then deploy to production"

**Analysis:**
- Repetition: 2 times (multi-step process) ✅
- Non-obvious: Company workflow ✅
- Future applicability: All deployments ✅
- Significant complexity ✅

**Decision:** CREATE new local skill: `deployment-approval-process`

---

## Examples: SHOULD FILTER OUT

### Example 11: Single Typo Fix

**Context:**
User asks: "Fix the typo in line 42 of README.md - change 'teh' to 'the'"

**Analysis:**
- Repetition: Single instance ❌
- Complexity: Trivial ❌
- Claude already knows: How to fix typos ❌

**Decision:** FILTER OUT - Too trivial, one-time task

---

### Example 12: Standard Git Command

**Context:**
User asks: "Show me what files have changed" and Claude runs `git status`. User says "Perfect, thanks"

**Analysis:**
- Repetition: Single instance ❌
- Claude already knows: Standard git commands ❌
- Non-obvious: No, standard operation ❌

**Decision:** FILTER OUT - Claude already knows git commands

---

### Example 13: One-Time Workaround

**Context:**
User says: "The API is down right now, so let's hardcode this response temporarily for testing"

**Analysis:**
- Future applicability: No, temporary workaround ❌
- Explicit statement: Yes, but marked as temporary ⚠️
- Repetition: Single instance ❌

**Decision:** FILTER OUT - Temporary solution, won't apply to future

---

### Example 14: Information in Code Comments

**Context:**
User asks: "What's the database connection string?" Claude finds it in code comments with clear documentation.

**Analysis:**
- Non-obvious: No, well-documented in code ❌
- Future applicability: Already discoverable ❌
- Claude struggled: No, found it easily ❌

**Decision:** FILTER OUT - Information already easily accessible

---

### Example 15: User Exploring Options

**Context:**
Session where user tries 3 different approaches to styling:
- First: "Try CSS modules"
- Then: "Actually, let's use styled-components"
- Finally: "Hmm, maybe Tailwind instead"

**Analysis:**
- Repetition: Multiple attempts but no consensus ❌
- Established pattern: No, still exploring ❌
- User settled: No, still experimenting ❌

**Decision:** FILTER OUT - User is exploring, not establishing pattern. Wait for user to settle on preferred approach.

---

### Example 16: Framework Default Behavior

**Context:**
User asks: "Make sure React re-renders when state changes." Claude explains this is automatic React behavior.

**Analysis:**
- Non-obvious: No, standard framework behavior ❌
- Claude already knows: Yes ❌
- Meaningful information: No ❌

**Decision:** FILTER OUT - Standard framework knowledge

---

### Example 17: Task-Specific Data

**Context:**
User provides: "For this calculation, use discount rate of 15% and tax rate of 8.5%"

**Analysis:**
- Future applicability: Specific to this calculation ❌
- Repetition: Single instance ❌
- General pattern: No, specific values ❌

**Decision:** FILTER OUT - Task-specific data, not a reusable pattern

---

### Example 18: Inconsistent Preferences

**Context:**
- Task A: User says "Use double quotes for strings"
- Task B: User says "Single quotes are fine"
- Task C: User uses both interchangeably

**Analysis:**
- Consistency: No clear pattern ❌
- Established preference: No ❌
- Repetition: Yes, but conflicting ⚠️

**Decision:** FILTER OUT - Inconsistent information. If this continues, ask user to clarify their preference before capturing.

---

### Example 19: Standard Npm Command

**Context:**
User asks: "Install the dependencies" and Claude runs `npm install`

**Analysis:**
- Claude already knows: Yes ❌
- Non-obvious: No ❌
- Repetition: Even if repeated, standard command ❌

**Decision:** FILTER OUT - Standard operation Claude already handles correctly

---

### Example 20: Already Documented Pattern

**Context:**
User says: "Make sure to follow the coding style in CONTRIBUTING.md"

**Analysis:**
- Already documented: Yes, in CONTRIBUTING.md ❌
- Claude can read: Yes, documentation is accessible ❌
- Adds value: No, just points to existing docs ❌

**Decision:** FILTER OUT - Information already well-documented and accessible. Claude can read CONTRIBUTING.md when needed.

---

## Summary Patterns

### Strong Signals for Capturing:
1. **User explicitly says** "always do X" or "in this repo, we..."
2. **Corrections repeated 2-3+ times** with same guidance
3. **User provides non-obvious context** that resolves struggle
4. **Multi-step workflows** user walks through multiple times
5. **Repo-specific patterns** not obvious from code alone

### Strong Signals for Filtering Out:
1. **Single instance** without explicit "always" statement
2. **Standard operations** Claude already knows (git, npm, etc.)
3. **Temporary workarounds** user marks as non-permanent
4. **User still exploring** multiple options without settling
5. **Well-documented** in accessible code comments or docs
6. **Task-specific data** rather than reusable patterns
7. **Trivial operations** (typo fixes, simple edits)
8. **Inconsistent patterns** without clear user preference

### When in Doubt:
- **Wait for confirmation**: Watch for pattern to repeat or user to explicitly state importance
- **Ask the user**: "This seems like a pattern worth capturing. Should I create a skill for this?"
- **Err on the side of caution**: Better to miss a learning than create unnecessary skills
