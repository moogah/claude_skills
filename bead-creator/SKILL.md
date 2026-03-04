---
name: bead-creator
description: "Create self-contained Beads issues for work tracking. Use when creating new beads for bugs, tasks, questions, or follow-up work - whether from specifications, discovered during implementation, or standalone requests. Handles proper formatting, labeling, dependencies, and ensures beads are actionable with embedded context."
---

# Bead Creator

Create self-contained, actionable Beads issues with proper context and structure.

## Core Principle

**Beads must be self-contained.** Each bead description must contain all context needed for implementation without referencing external documents.

## Creating Simple Beads

For straightforward tasks, use the basic pattern:

```bash
bd create "Add TypeScript strict mode to stack config" \
  -t task -p 2 -l typescript,infrastructure

bd update beads-xxx --description "Enable TypeScript strict mode for oncall-background-processes-stack.

Files to modify:
- stacks/oncall-background-processes-stack/tsconfig.json

Changes:
1. Add 'strict: true' to compilerOptions
2. Add 'strictNullChecks: true' (redundant but explicit)
3. Ensure 'noImplicitAny: true' is present

Design rationale: Strict mode catches type errors early and improves code quality. This stack uses CDK infrastructure code where type safety is critical for preventing runtime deployment errors.

Verification:
- Run bash scripts/type-check.sh
- Confirm no new type errors introduced"
```

## Creating Follow-Up Beads During Implementation

When discovering work during implementation, externalize it immediately:

### Bug Discovered

```bash
bd create "Fix null check in assignment-day-utils.js" \
  -t bug -p 1 -l bugfix,follow-up \
  --deps "discovered-from:beads-abc" \
  --description "While implementing dual-write, discovered missing null check in assignment-day-utils.js line 47. The getCompanyId() function doesn't handle undefined assignmentDay input, causing crashes in edge cases.

Files to modify:
- utils/assignment-day-utils.js

Fix:
Add null check before accessing assignmentDay.companyId

Context: Discovered during beads-abc implementation when testing edge cases"
```

### Unclear Requirement

```bash
bd create "Clarify error handling strategy for Apollo failures" \
  -t task -p 2 -l question,follow-up \
  --deps "discovered-from:beads-abc" \
  --description "During implementation, uncertain about: should Apollo failures in dual-write mode fail the entire operation or just log warnings? Need product/architecture decision.

Context: Working on ShiftUpdate dual-write implementation. AppSync failures currently fail the operation, but unclear if Apollo should behave the same way."
```

### Related Work Discovered

```bash
bd create "Apply dual-write pattern to 3 other Lambdas" \
  -t task -p 2 -l migration,follow-up \
  --deps "discovered-from:beads-abc" \
  --description "Same dual-write pattern needed for:
- NotifyOnShiftCancel (similar event trigger)
- CalculateCoveredShift (similar data flow)
- CreateUnassigned (similar mutation pattern)

Use ShiftUpdateIntegration as reference implementation.

Implementation: Follow the established pattern from beads-abc"
```

## Bead Description Template

Every bead description should follow this structure:

```
[One-line summary of what this accomplishes]

Files to modify:
- path/to/file1.ext
- path/to/file2.ext

Implementation steps:
1. Specific actionable step with details
2. Another step, include code patterns if relevant
3. Be concrete - mention functions, variables, patterns

Design rationale: [WHY - the reasoning behind this]
[Architectural context explaining this implementation]
[Technology choices, patterns, tradeoffs]

Design pattern: [Pattern to follow if applicable]
[Reference similar code if relevant]

Verification:
- How to test this works
- Acceptance criteria
- What "done" looks like
```

## Label Conventions

Use consistent labeling for easy querying:

```bash
# Project/feature labels
-l migration,lambda,apollo

# Type labels
-l bugfix,follow-up,question,blocker

# Querying by label
bd list --label migration
bd ready --label apollo
```

## Dependency Patterns

Add dependencies when tasks must be sequenced:

```bash
# Task Y depends on X completing first
bd dep add beads-yyy beads-xxx

# Track discovery without blocking
--deps "discovered-from:beads-xxx"

# Multiple dependencies
--deps "discovered-from:beads-abc,blocks:beads-xyz"
```

Common dependency patterns:
- Infrastructure before application code
- Data migrations before feature rollout
- Shared utilities before dependent features

## Best Practices

**Self-contained context:**
- Extract and embed all necessary information in the description
- Don't reference external documents - copy the relevant context
- Include WHY (rationale) not just WHAT (steps)

**Actionable steps:**
- Specific file paths
- Concrete code patterns
- Clear verification criteria

**Appropriate granularity:**
- Target 1-4 hours per bead (one focused session)
- Split larger work into multiple beads
- Combine tiny tasks together

**Proper classification:**
- Type: task (feature work), bug (fixes), question (needs clarification)
- Priority: 1 (urgent), 2 (normal), 3 (low)
- Labels: project names, feature areas, task types

**Track relationships:**
- Use `discovered-from` for follow-up work
- Use `blocks` for hard dependencies
- Use consistent labels for related work
