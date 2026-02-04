---
name: openspec-to-beads
description: Convert OpenSpec artifacts (proposal.md, design.md, tasks.md, specs/) into self-contained Beads issues for implementation tracking. Use when you have completed OpenSpec planning artifacts and need to create trackable implementation tasks, or when converting any specification or design document into actionable Beads issues.
---

# OpenSpec to Beads Conversion

Convert OpenSpec design artifacts into self-contained Beads issues for implementation.

## Critical Principle

**Beads must be self-contained.** Extract and embed relevant context from OpenSpec rather than referencing it. Agents need complete context within the bead description.

## What to Include in Each Bead

Every bead description should contain:

1. **Implementation steps** - Specific files to modify, code patterns to follow
2. **Design context** - Architectural rationale, technology choices
3. **Acceptance criteria** - How to verify completion
4. **Related context** - Background needed to understand the task

## Simple Task Example

```bash
bd create "Add TypeScript strict mode to stack config" \
  -t task -p 2 -l typescript,oncall-background-processes

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
- Confirm no new type errors introduced
- Existing type errors in this stack are acceptable (document count if any)"
```

## Complex Task Example

```bash
bd create "Implement dual-write for ShiftUpdate Lambda" \
  -t task -p 1 -l migration,lambda,apollo

bd update beads-xxx --description "Add Apollo dual-write support to ShiftUpdateIntegration Lambda.

Files to modify:
- dynamodb-event-functions/ShiftUpdateIntegration/index.js
- dynamodb-event-functions/ShiftUpdateIntegration/shift-update-integration-stack.ts

Implementation steps:
1. Import layer modules in index.js:
   const { executeAssignmentDayOperation } = require('/opt/nodejs/assignment-day-migration-manager');
   const { updateAssignmentDayInApollo } = require('/opt/nodejs/apollo-assignment-day-mutations');

2. Wrap existing updateAssignmentDay() with dual-write pattern:
   - appSyncOperation: existing GraphQL call
   - apolloOperation: call updateAssignmentDayInApollo() with companyId
   - Pass to executeAssignmentDayOperation()

3. Update stack file bundling config:
   - Add layer modules to externalModules array
   - Ensure amplifyLayer is attached

Design pattern: Use established dual-write migration pattern (see NotifyAdminsOnAccept for reference). The migration manager handles feature flag logic for routing to AppSync vs Apollo based on APOLLO_OPERATIONS env var.

Environment behavior:
- NONE: AppSync only
- DUALWRITE: Both APIs, prefer AppSync result (except company 8)
- APPLOI_QA: Company 8 → Apollo, others → AppSync
- ON: All companies → Apollo

Verification:
- Confirm Lambda builds without errors
- Check CloudWatch logs for dual-write execution
- Run bash scripts/type-check.sh"
```

## Label Conventions

Use the OpenSpec change name as a label:

```bash
# If OpenSpec change is "refresh-dev-cycle-docs"
bd create "Update README with new workflow" \
  -t task -p 2 -l refresh-dev-cycle-docs,docs

# Makes querying easy
bd list --label refresh-dev-cycle-docs
```

## Dependency Handling

Add dependencies when tasks must be sequenced:

```bash
# Task Y depends on X completing first
bd dep add beads-yyy beads-xxx
```

Common dependency patterns:
- Infrastructure before application code
- Data migrations before feature rollout
- Shared utilities before dependent features
