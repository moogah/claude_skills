---
name: bead-implementation
description: Execute on a Beads issue with proper discovery workflow and mandatory session close protocol. Use when starting work on a bead, during implementation when discovering new work that should be externalized, or when completing a session to ensure all work is properly committed and pushed.
---

# Bead Implementation Workflow

Execute on beads with narrow focus, proper discovery, and mandatory session close.

## Core Principle

**One bead per session.** Complete only ONE bead per session, then commit and push immediately.

## Finding and Claiming Work

```bash
# Find available work
bd ready                              # All ready work
bd ready --label migration            # Ready work with specific label

# Review issue details
bd show beads-xxx

# Claim work
bd update beads-xxx --status=in_progress
```

## During Implementation: Create Follow-Up Beads

Stay narrowly focused. Externalize any unexpected work by creating new beads.

### Bug Discovered

```bash
bd create "Fix null check in assignment-day-utils.js" \
  -t bug -p 1 -l bugfix,follow-up \
  --deps "discovered-from:beads-abc" \
  --description "While implementing dual-write, discovered missing null check in assignment-day-utils.js line 47. The getCompanyId() function doesn't handle undefined assignmentDay input, causing crashes in edge cases..."
```

### Unclear Requirement

```bash
bd create "Clarify error handling strategy for Apollo failures" \
  -t task -p 2 -l question,follow-up \
  --deps "discovered-from:beads-abc" \
  --description "During implementation, uncertain about: should Apollo failures in dual-write mode fail the entire operation or just log warnings? Need product/architecture decision..."
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

Use ShiftUpdateIntegration as reference implementation..."
```

### Multiple Dependencies

```bash
bd create "Refactor shared mutation logic" \
  -t task -p 2 -l refactor,follow-up \
  --deps "discovered-from:beads-abc,blocks:beads-xyz" \
  --description "Extract common dual-write pattern into shared utility. Blocks implementation of other Lambda migrations until complete..."
```

### Key Practices

- Use `--deps "discovered-from:beads-xxx"` to link follow-up beads to their origin
- Multiple dependencies: `--deps "discovered-from:beads-abc,blocks:beads-xyz"`
- Add labels: `follow-up`, `question`, `blocker`, `bugfix`
- `discovered-from` is non-blocking - tracks relationships without affecting execution

## Completing Work

```bash
# Mark work complete with review label
bd label add beads-xxx needs-review
bd close beads-xxx
```

## Finding Review Work

```bash
# Query beads needing review
bd list --label needs-review --status=closed

# After review, remove the label
bd label remove beads-xxx needs-review
```

## Session Close Protocol (Mandatory)

**CRITICAL**: Work is NOT complete until `git push` succeeds.

### Seven Steps

**1. File Issues for Remaining Work**
- Document follow-up tasks as new beads
- Close completed issues: `bd close <id1> <id2> ...`
- Update in-progress issues with current status

**2. Run Quality Gates** (if code was modified)
```bash
bash scripts/type-check.sh    # TypeScript validation
npm test                       # Run tests (if applicable)
```

**3. Sync Beads**
```bash
bd sync --from-main           # Pull beads updates from main
```

**4. Commit Changes**
```bash
git status                    # Check what changed
git add <files>               # Stage code changes
git commit -m "..."           # Commit with descriptive message
```

**5. Push to Remote** (CRITICAL - never skip)
```bash
git pull                      # Get latest changes
git push                      # Push to remote
git status                    # MUST show "up to date with origin"
```

**6. Verify Completion**
- Confirm all changes are committed and pushed
- Check `git status` shows clean working tree
- Verify push succeeded without errors

**7. Hand Off Context**
- Provide summary of what was completed
- Note any issues filed for next session
- Highlight any blockers or dependencies

### Critical Rules

- **Never stop before pushing** - Leaves work stranded locally
- **Never defer pushing** - Execute immediately
- **Retry if push fails** until successful
- Push completion is non-negotiable
