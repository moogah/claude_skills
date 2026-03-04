---
name: bead-implementation
description: "Execute on a Beads issue with proper discovery workflow and mandatory session close protocol. Use when starting work on a bead, during implementation when discovering new work that should be externalized, or when completing a session to ensure all work is properly committed and pushed."
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

**Use the bead-creator skill** for guidance on creating follow-up beads for:
- Bugs discovered during implementation
- Unclear requirements needing clarification
- Related work identified
- Dependencies and blocking relationships

Key practices:
- Use `--deps "discovered-from:beads-xxx"` to link follow-up beads to their origin
- Add appropriate labels: `follow-up`, `question`, `blocker`, `bugfix`
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
