# Concrete Examples

This document provides essential examples demonstrating when to capture learnings and when to filter them out. Focus on the nuanced decisions rather than obvious cases.

## Examples: SHOULD CAPTURE

### Example 1: Repeated Testing Command Correction

**Context:**
User asks Claude to run tests before deploying 3 times. Each time Claude runs `npm test`, user corrects: "No, use `npm run test:integration -- --coverage`"

**Analysis:**
- Repetition: 3 times ✅
- Non-obvious: Specific command with flags ✅
- Future applicability: Will apply to all test runs ✅

**Decision:** CREATE new local skill: `repo-testing-workflow`

**Why this example matters:** Canonical case of repo-specific pattern with repetition

---

### Example 2: User Preferences Across Sessions

**Context:**
User asks Claude to format code 3 different times across 2 sessions. Each time specifies: "Use Prettier with --single-quote and --trailing-comma all and --print-width 100"

**Analysis:**
- Repetition: 3 times across sessions ✅
- Non-obvious: Specific user preference ✅
- Future applicability: Will apply to all formatting ✅
- Generally applicable: Not repo-specific ✅

**Decision:** CREATE new user-level skill: `code-formatting-preferences`

**Why this example matters:** Shows user-level vs local distinction - this applies across all projects

---

### Example 3: Struggle Resolved by Key Insight

**Context:**
Claude struggles to help with database query. User provides: "Connection string is in config/database.yml, use the 'analytics' database. Schema is in docs/schema.sql. Always use read-only connection for queries."

**Analysis:**
- Struggle → resolution pattern ✅
- Non-obvious: Repo-specific DB setup ✅
- Future applicability: All future DB queries ✅

**Decision:** CREATE new local skill: `project-database-access`

**Why this example matters:** Shows the "struggle then insight" detection pattern - when user's context unlocks the solution, that's a strong signal to capture

---

### Example 4: Update Existing vs Create New

**Context:**
User uses pdf skill successfully, then asks: "Can you add watermark to PDFs? This seems useful to add to the pdf skill." Claude implements watermark functionality successfully.

**Analysis:**
- Explicit request to update skill ✅
- Related to existing skill domain ✅
- Extends without making unfocused ✅

**Decision:** UPDATE existing user-level skill: `pdf`

**Why this example matters:** CRITICAL - demonstrates when to extend existing skill rather than create new one. Watermarks are just another PDF operation.

---

### Example 5: Multi-Step Workflow Complexity

**Context:**
User walks Claude through deployment 2 times: "Deploy to staging, send Slack message to #deployments channel with staging URL, wait for QA approval, then deploy to production"

**Analysis:**
- Repetition: 2 times (but multi-step process) ✅
- Non-obvious: Company workflow ✅
- Significant complexity: Multi-step with human approval ✅

**Decision:** CREATE new local skill: `deployment-approval-process`

**Why this example matters:** Shows that complexity/significance can lower the repetition threshold - a 4-step workflow seen twice is worth capturing

---

## Examples: SHOULD FILTER OUT

### Example 6: Single Trivial Operation

**Context:**
User asks: "Fix the typo in line 42 of README.md - change 'teh' to 'the'"

**Analysis:**
- Repetition: Single instance ❌
- Complexity: Trivial ❌
- Claude already knows: How to fix typos ❌

**Decision:** FILTER OUT - Too trivial, one-time task

**Why this example matters:** Obvious case - single trivial operations don't warrant skills

---

### Example 7: User Still Exploring

**Context:**
Session where user tries 3 different approaches to styling:
- First: "Try CSS modules"
- Then: "Actually, let's use styled-components"
- Finally: "Hmm, maybe Tailwind instead"

**Analysis:**
- Repetition: Multiple attempts but no consensus ❌
- Established pattern: No, still exploring ❌
- User settled: No, still experimenting ❌

**Decision:** FILTER OUT - Wait for user to settle on preferred approach

**Why this example matters:** CRITICAL - shows that repetition alone isn't enough. User must establish a preference, not just explore options. Watch for phrases like "hmm", "maybe", "let's try" vs "always use", "we use".

---

### Example 8: Inconsistent Guidance

**Context:**
- Task A: User says "Use double quotes for strings"
- Task B: User says "Single quotes are fine"
- Task C: User uses both interchangeably

**Analysis:**
- Consistency: No clear pattern ❌
- Established preference: No ❌
- Repetition: Yes, but conflicting ⚠️

**Decision:** FILTER OUT - Ask user to clarify their preference before capturing

**Why this example matters:** CRITICAL - shows when to ask clarifying questions rather than capture. If you notice inconsistency, ask: "I've noticed different approaches for X - which should I use?"

---

### Example 9: Temporary Workaround

**Context:**
User says: "The API is down right now, so let's hardcode this response temporarily for testing"

**Analysis:**
- Future applicability: No, temporary workaround ❌
- Explicit statement: Yes, but marked as temporary ⚠️

**Decision:** FILTER OUT - Temporary solution, won't apply to future

**Why this example matters:** Shows importance of listening for "temporary", "for now", "until" - these signal non-permanent patterns

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
