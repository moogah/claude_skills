# Decision Framework

Detailed decision trees for the skill-learner workflow. See examples.md for concrete scenarios.

## 1. Significance Assessment

Determine if a learning is worth capturing:

```
Start: Potential learning detected
    ↓
Question 1: Is it repeated 2-3+ times OR explicitly stated by user?
    ├─ No → FILTER OUT (insufficient signal strength)
    └─ Yes → Continue to Question 2
         ↓
Question 2: Is it non-obvious information Claude wouldn't know by default?
    ├─ No → FILTER OUT (Claude already knows this)
    └─ Yes → Continue to Question 3
         ↓
Question 3: Will it apply to future similar tasks?
    ├─ No → FILTER OUT (too specific to current context)
    └─ Yes → Continue to Question 4
         ↓
Question 4: Does it involve meaningful complexity or decision-making?
    ├─ No → FILTER OUT (too trivial)
    └─ Yes → CAPTURE THIS LEARNING
         ↓
    Proceed to Triage (Section 2)
```

## 2. Triage: Skill vs CLAUDE.md

Determine whether the learning should be captured as a skill or in CLAUDE.md files:

```
Start: Significant learning confirmed
    ↓
Question: What TYPE of knowledge is this?
    ├─ Procedural/Workflow → Is it complex? (multi-step, decision trees, tool integration)
    │   ├─ Yes (3+ steps, conditional logic, tools) → CAPTURE AS SKILL
    │   └─ No (1-2 simple steps) → Continue to CLAUDE.md triage
    │
    ├─ Conventions/Preferences → CLAUDE.md triage
    ├─ Contextual Knowledge → CLAUDE.md triage
    └─ Reference Data → CLAUDE.md triage
         ↓
    Proceed to CLAUDE.md Level Decision (Section 3)
```

### Capture as SKILL when:

- Complex workflow with 3+ distinct steps or decision points
- Requires tool integration with bundled scripts, references, or assets
- Multi-step procedure with conditional logic or branching
- Contains reusable code that would be written repeatedly (scripts/)
- Needs resource bundles (templates, assets, reference docs)
- Represents standalone capability in distinct domain

### Capture in CLAUDE.md when:

- Simple convention (1-2 step preference or formatting rule)
- Code style preference (indentation, quotes, naming)
- Command shortcut (single command with specific flags)
- Context about codebase (architecture notes, file locations)
- Team pattern (review process, commit message style)
- Path-specific rule (convention that applies to specific directories)
- Always-on knowledge that should be in every conversation

### Quick Test:

**If uncertain:** If it takes more than 3 steps to execute OR requires code/scripts → Skill. Otherwise → CLAUDE.md.

## 3. CLAUDE.md Level Decision

Once determined to capture in CLAUDE.md, choose the appropriate hierarchical level:

```
Start: Decided to capture in CLAUDE.md
    ↓
Question 1: Is it path-specific (applies only to certain files/directories)?
    ├─ Yes → .claude/rules/*.md with paths frontmatter
    └─ No → Continue to Question 2
         ↓
Question 2: Is it specific to this codebase/repo/team?
    ├─ Yes → Project-level: ./CLAUDE.md or ./.claude/CLAUDE.md
    └─ No → Continue to Question 3
         ↓
Question 3: Does it apply to all your projects?
    ├─ Yes → User-level: ~/.claude/CLAUDE.md
    └─ No/Uncertain → Project-level (default when uncertain)
```

### User-Level (~/.claude/CLAUDE.md)

**When to use:**
- Personal preferences across ALL projects
- Language/framework conventions you always follow
- Tool usage patterns you prefer universally
- General engineering practices you apply everywhere

**Examples:**
- "I prefer single quotes in JavaScript/TypeScript"
- "Always run tests before committing"
- "Use descriptive variable names, avoid abbreviations"
- "Format code with Prettier: --single-quote --print-width 100"

### Project-Level (./CLAUDE.md or ./.claude/CLAUDE.md)

**When to use:**
- Team-shared conventions for THIS project
- Architecture patterns specific to this codebase
- Project-specific file locations or structure
- Team workflows (not path-specific)
- Build/test/deploy commands for this repo

**Examples:**
- "This is an Nx monorepo with apps in apps/ and libs in libs/"
- "Database schema documented in docs/schema.sql"
- "Deploy to staging first, get QA approval before production"
- "Connection strings in config/database.yml, use 'analytics' database"

### Path-Specific Rules (./.claude/rules/*.md)

**When to use:**
- Conventions that only apply to specific directories or file patterns
- Different rules for different parts of the codebase
- Language-specific guidelines (TypeScript vs Python files)
- Component-specific patterns (API vs frontend)

**Examples:**
- API endpoints: "All files in src/api/ must include input validation"
- React components: "Components in src/components/ must use functional components"
- Test files: "Tests in tests/ must follow Arrange-Act-Assert pattern"
- Config files: "YAML files in config/ must be validated against schema"

**Frontmatter format:**
```markdown
---
paths: "src/api/**/*.ts"
---

# API Development Rules

- All endpoints must include input validation
- Use standard error response format
```

### Project-Local (./CLAUDE.local.md)

**When to use:**
- Personal preferences ONLY for this project
- Your local development setup
- Sandbox URLs, test data specific to your workflow
- Not ready for team (experimental patterns)

**Examples:**
- "Use my local dev database at localhost:5432"
- "My staging URL is https://jefffarr-staging.example.com"
- "Test with my test account: jeff.test@example.com"

## 4. Action Determination (For Skills)

### 4A. Update Existing vs Create New

```
Start: Significant learning confirmed
    ↓
Question 1: Does an existing skill cover this domain?
    ├─ No → CREATE NEW SKILL (go to Section 2B for location)
    └─ Yes → Continue to Question 2
         ↓
Question 2: Does this learning directly extend the existing skill's purpose?
    ├─ No → CREATE NEW SKILL (distinct domain)
    └─ Yes → Continue to Question 3
         ↓
Question 3: Would adding this make the existing skill unfocused or too broad?
    ├─ Yes → CREATE NEW SKILL (avoid bloat)
    └─ No → UPDATE EXISTING SKILL
```

### 4B. Local vs User-Level

```
Start: Decided to create new skill
    ↓
Question 1: Does it reference specific file paths, modules, or components in this repo?
    ├─ Yes → LOCAL SKILL (.claude/skills in repo)
    └─ No → Continue to Question 2
         ↓
Question 2: Is it tied to team conventions that wouldn't apply elsewhere?
    ├─ Yes → LOCAL SKILL
    └─ No → Continue to Question 3
         ↓
Question 3: Does it contain repo-specific schemas, APIs, or data structures?
    ├─ Yes → LOCAL SKILL
    └─ No → Continue to Question 4
         ↓
Question 4: Is it generally applicable across projects?
    ├─ Yes → USER-LEVEL SKILL (~/.claude/skills)
    └─ No/Uncertain → USER-LEVEL SKILL (default when uncertain)
```

## 5. Timing Decisions

### 5A. When to Suggest

| Situation | User State | Learning Urgency | Decision |
|-----------|------------|------------------|----------|
| User mid-explanation | Focused | Low | DEFER to end-of-task |
| User mid-explanation | Focused | High (conflicts with skill) | INTERRUPT with apology |
| Task has errors/blockers | Problem-solving | Any | DEFER to after resolution |
| Task just completed | Available | Any | SUGGEST now (preferred) |
| User says "thanks" | Wrapping up | Any | SUGGEST now |
| User in flow (3+ rapid tasks) | Focused | Low | DEFER to natural pause |
| User in flow (3+ rapid tasks) | Focused | High (contradicts skill) | INTERRUPT briefly |
| User explicitly asks about skill | Available | Any | SUGGEST immediately |

### 5B. Timing Flowchart

```
Start: Ready to get user approval
    ↓
Question 1: Does this learning contradict an existing skill (creating active harm)?
    ├─ Yes → INTERRUPT NOW (brief, apologetic)
    └─ No → Continue to Question 2
         ↓
Question 2: Is user mid-explanation, mid-question, or debugging an error?
    ├─ Yes → DEFER (wait for natural stopping point)
    └─ No → Continue to Question 3
         ↓
Question 3: Has user just completed a task or said "thanks"?
    ├─ Yes → SUGGEST NOW (optimal timing)
    └─ No → Continue to Question 4
         ↓
Question 4: Is user in rapid flow state (multiple quick tasks)?
    ├─ Yes → DEFER (wait for pause or slow-down)
    └─ No → SUGGEST NOW
```

## 6. Edge Case: Conflicting Information

If user corrections conflict across different times:

```
I've noticed different approaches for [X]:
- In task A, you preferred [approach 1]
- In task B, you preferred [approach 2]

Which approach should I capture in the skill, or does it depend on [some context]?
```

Only capture once consistency is established.

## 7. Quick Reference

**Capture when:**
- 2-3+ repetitions OR explicit user statement
- Non-obvious information
- Future applicability
- Meaningful complexity

**Triage: Skill vs CLAUDE.md:**
- Complex workflow (3+ steps, tools, conditional logic) → Skill
- Simple convention/preference/context → CLAUDE.md
- When uncertain: If needs code/scripts → Skill, otherwise → CLAUDE.md

**CLAUDE.md Level:**
- Path-specific → .claude/rules/*.md (with frontmatter)
- Project-specific → ./CLAUDE.md or ./.claude/CLAUDE.md
- User-wide → ~/.claude/CLAUDE.md
- When uncertain → Project-level

**For Skills:**

**Update existing skill when:**
- Learning fits existing skill's domain
- Extends without making unfocused

**Create new skill when:**
- Distinct domain/capability
- Would make existing skill too broad

**Local skill when:**
- Repo-specific files/paths/schemas/APIs
- Team conventions for this project

**User-level skill when:**
- Generally applicable across projects
- When uncertain (default)

**Timing:**
- Interrupt: Only for conflicts with existing skills/patterns
- Suggest: After task completion (preferred)
- Defer: During mid-task, debugging, or flow state
