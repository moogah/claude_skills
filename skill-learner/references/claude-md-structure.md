# CLAUDE.md Structure Examples

Minimal examples showing structure. Claude can infer content from patterns.

## User-Level (~/.claude/CLAUDE.md)

```markdown
# Session Initialization
At the start of each session, immediately invoke the `skill-learner` skill using the Skill tool.

---

# Code Style
- Prefer single quotes in JavaScript
- Use 2-space indentation

# Workflow Practices
- Run tests before committing
- TDD: Write tests before implementation

# Tool Preferences
- Prettier: `--single-quote --print-width 100`
- npm: Always use --save-dev for dev dependencies
```

**Key pattern:** Organized sections with bullet points. Content grows naturally through skill-learner.

---

## Project-Level (./CLAUDE.md)

```markdown
# Project: Example App

## Overview
- Stack: React + TypeScript frontend, Node.js backend
- Architecture: Nx monorepo (apps in apps/, libs in libs/)

## Development Commands
- Build: `npm run build`
- Test: `npm run test`
- Deploy staging: `npm run deploy:staging`

## Important Locations
- Config: `config/database.yml`
- Schema: `docs/schema.sql`
- API docs: `docs/api/`

## Architecture Patterns
- JWT tokens in httpOnly cookies
- Use Prisma ORM for database (never raw SQL)
- Events via AWS EventBridge
```

**Key pattern:** Project context, commands, file locations, team conventions.

---

## Path-Specific Rules (.claude/rules/api.md)

```markdown
---
paths: "src/api/**/*.ts"
---

# API Guidelines

- Validate input with Zod schemas
- Include OpenAPI documentation comments
- Use standard error format from libs/shared/errors
- All endpoints require authentication middleware
```

**Key pattern:** YAML frontmatter with `paths:` glob, then conventions for those files.

---

## Path-Specific Rules (.claude/rules/frontend/react.md)

```markdown
---
paths:
  - "apps/web/src/**/*.tsx"
  - "apps/admin/src/**/*.tsx"
---

# React Component Guidelines

- Functional components with hooks (no class components)
- Props: Define TypeScript interface, use destructuring
- State: React hooks for local, Zustand for global
- Data fetching: Use React Query
- Styling: Tailwind CSS classes
```

**Key pattern:** Multiple path patterns, component-specific conventions.

---

## Content Guidelines

**DO:**
- Use bullet points for scannable content
- Reference file paths with backticks
- Be specific ("Use 2-space indentation" not "Format properly")
- Keep sections focused

**DON'T:**
- Write long paragraphs (prefer bullets)
- Duplicate information across files
- Exceed 400 lines per file (split into .claude/rules/)
- Include temporary or one-off instructions

---

## File Organization

### When to split into .claude/rules/

When project CLAUDE.md exceeds ~300 lines or has path-specific content:

```
.claude/
├── CLAUDE.md           # Main: overview, commands, general conventions
└── rules/
    ├── api.md          # Backend API conventions
    ├── frontend/
    │   ├── react.md    # React component patterns
    │   └── styles.md   # Styling guidelines
    └── testing.md      # Test conventions
```

Each rules file uses `paths:` frontmatter to apply conditionally.

---

## Growth Through Iteration

**v1:** Start minimal (like examples above)
**v2:** Add related learnings to same section
**v3:** Split into multiple sections as content grows
**v4:** Extract path-specific rules to .claude/rules/ when needed

Don't anticipate everything upfront - let memory grow naturally through skill-learner.
