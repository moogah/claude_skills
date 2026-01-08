---
name: skill-learner
description: "Helps Claude iteratively capture learnings and patterns from sessions into skills. Use this skill when: (1) User explicitly corrects Claude's approach or provides feedback, (2) Claude notices repeated patterns across multiple tasks in the session, (3) User mentions repo-specific conventions or preferences, (4) Claude struggles with similar tasks multiple times, (5) User requests skill updates or creation, (6) Session reveals gaps in existing skill coverage. This skill operates proactively but asks permission before interrupting current work."
---

# Skill Learner

## Overview

This skill enables Claude to recognize learnings during sessions and incorporate them into the skills system through updates or new skill creation. It operates proactively but respects the user's workflow, asking permission before making changes.

## Core Principles

- **Capture only non-obvious, repeatable patterns** - Skip trivial one-off tasks
- **Extend existing skills when related; create new ones for distinct domains** - Avoid skill proliferation
- **Always review with user before implementing** - Never assume, always ask
- **Minimize interruption to current work** - Prefer end-of-task suggestions
- **Always be minimal and concise** - Incorporate only direct observations, only solve the immediate problem, grow through iteration
- **Consider appropriate hierarchy** - Local skills for repo-specific, user-level for broadly applicable

## Workflow Decision Tree

The skill-learning process follows six sequential steps:

1. **Detect** - Recognize learnings worth capturing
2. **Assess** - Evaluate significance and filter out noise
3. **Triage** - Decide if learning belongs in skill or CLAUDE.md file
4. **Determine** - Decide on action details (update/create, level/location)
5. **Approve** - Get user permission at appropriate time
6. **Implement** - Execute the change using appropriate tools

## Step 1: Detecting Learnings Worth Capturing

Watch for these signals during sessions:

### Explicit Corrections
User directly corrects Claude's approach or provides specific guidance:
- "Actually, do it this way instead..."
- "In this repo, we always..."
- "No, use X not Y because..."
- Corrects terminology or technical approach

### Repeated Patterns
Similar tasks or requests appear multiple times:
- Claude solves same type of problem 3+ times in session
- User makes same request with consistent requirements
- User repeatedly provides same context or explanation

### Repo-Specific Conventions
User mentions patterns specific to the codebase:
- Specific testing patterns: "Always run X before Y"
- Code style preferences beyond linters
- Deployment or build procedures
- Team-specific workflows or architectures
- Database connection patterns or schemas

### Performance Struggles Leading to Insights
Claude struggles then succeeds after receiving key information:
- Multiple approaches attempted before success
- User provides crucial missing context that unlocks solution
- Error patterns that required specific fixes
- Discovery of non-obvious dependencies or requirements

### Coverage Gaps
Tasks that don't trigger any existing skill:
- User requests help with tasks Claude handles only generically
- User mentions tools/frameworks that could benefit from specialized guidance
- Workflow sequences that would be valuable to formalize

## Step 2: Assessing Significance

Use decision-framework.md for detailed criteria. Key filters:

### Significance Threshold
- **Capture** if: Appears 2-3+ times OR user explicitly states importance
- **Filter out** if: One-off task, trivial variation, already obvious to Claude

### Non-Obvious Check
- **Capture** if: Contains information Claude wouldn't know by default
- **Filter out** if: Claude already knows this from training

### Future Applicability
- **Capture** if: Will apply to future similar tasks
- **Filter out** if: Highly specific to current unique context

### Examples of What to Filter Out
- Single typo fixes
- Standard operations Claude already handles well
- User preferences that are inconsistent or one-time
- Information that's easily discoverable from code/docs

See references/decision-framework.md for flowcharts and references/examples.md for concrete scenarios.

## Step 3: Triaging to Skill vs CLAUDE.md

Once significance is confirmed, determine whether to capture as a skill or in CLAUDE.md.

**Quick rule:** Complex workflow (3+ steps, tools, conditional logic) → Skill. Simple preference/convention/context → CLAUDE.md.

**CLAUDE.md levels:**
- Path-specific → .claude/rules/*.md (with `paths:` frontmatter)
- Project-specific → ./CLAUDE.md or ./.claude/CLAUDE.md
- User-wide → ~/.claude/CLAUDE.md

See references/decision-framework.md for detailed decision trees and references/claude-md-structure.md for minimal examples.

## Step 4: Determining Action Type

Make decisions about how to capture the learning. The specific decisions depend on whether you're creating a skill or writing to CLAUDE.md.

### For Skills

Three key decisions for skill-based learnings:

#### Decision 1: Update Existing vs Create New

**Update existing skill when:**
- Learning directly relates to an existing skill's domain
- Fills a gap or corrects information in existing skill
- Extends capabilities of existing workflow
- Examples: Adding PDF watermark to pdf skill, adding testing pattern to existing testing skill

**Create new skill when:**
- Learning represents a distinct domain or capability
- No existing skill covers this area
- Would make existing skill unfocused or too broad
- Examples: First-time repo deployment process, new database-specific patterns

**Principle:** Prefer updates to avoid skill proliferation, but don't force unrelated content together.

#### Decision 2: Local vs User-Level

**Local (.claude/skills in repo) when:**
- Specific to this codebase's architecture
- References specific file paths, modules, or components in this repo
- Tied to team conventions that wouldn't apply elsewhere
- Contains repo-specific schemas, APIs, or data structures
- Examples: "repo-deployment", "project-database-queries"

**User-level (~/.claude/skills) when:**
- Generally applicable engineering best practices
- Language/framework patterns (React, Python, testing strategies)
- Tool usage patterns (git workflows, docker commands)
- Cross-project workflow improvements
- Examples: "code-formatting-preferences", "testing-best-practices"

**When uncertain:** Start user-level. It's easier to migrate to local later if needed.

#### Decision 3: Timing and Interruption Strategy

See decision-framework.md for the timing decision matrix. Quick reference:

- **Interrupt mid-task:** ONLY when learning conflicts with existing skill (active harm)
- **Defer to end-of-task:** Preferred for most learnings (after task complete, natural stopping point)
- **Defer to end-of-session:** When user is in urgent/flow state but learning is valuable

### For CLAUDE.md Files

Three key decisions for CLAUDE.md-based learnings:

#### Decision 1: File Organization

**For path-specific rules:**
- Create new file in .claude/rules/ with descriptive name (e.g., `api-guidelines.md`)
- Add YAML frontmatter with `paths` field using glob patterns
- Organize by domain: `frontend/`, `backend/`, etc. if many rules exist
- Example filename: `.claude/rules/api-guidelines.md` or `.claude/rules/frontend/react-components.md`

**For project-level:**
- Add to existing ./CLAUDE.md (or ./.claude/CLAUDE.md) or create if doesn't exist
- Choose appropriate section (create if needed)
- Keep organized with clear headings

**For user-level:**
- Add to existing ~/.claude/CLAUDE.md or create if doesn't exist
- Organize into logical sections (Code Style, Workflow, Tools, etc.)
- Keep focused on personal preferences

**For project-local:**
- Add to ./CLAUDE.local.md (personal, not committed)
- Use for temporary or experimental patterns

#### Decision 2: Section Placement

- Place learning under most relevant existing section
- Create new section if no good fit (use ## for top-level, ### for subsections)
- Keep sections focused (one topic per section)
- Group related items together for easy scanning

**Common section names:**
- Code Style (formatting, naming, language-specific)
- Workflow Practices (testing, git, review process)
- Tool Preferences (CLI flags, build commands)
- Project Overview (architecture, tech stack)
- Development Commands (build, test, deploy)
- Important Locations (config files, schemas, docs)

#### Decision 3: Content Format

- Use bullet points for scannable content
- Include concrete examples when helpful
- Reference file paths with backticks
- Keep additions concise (2-5 bullets typically)
- Be specific over vague ("Use 2-space indentation" not "Format properly")

See references/claude-md-structure.md for detailed examples of well-organized CLAUDE.md files.

## Step 5: Getting User Approval

Always ask before implementing changes. Use appropriate timing and clear communication.

### Approval Request Format for Skills

```
I noticed [specific learning]. This seems worth capturing because [reason].

I suggest [updating skill-name / creating skill-name] to include:
- [Key point 1]
- [Key point 2]

Would you like me to:
1. Do this now
2. Do this after we finish [current task]
3. Skip it
```

### Approval Request Format for CLAUDE.md

```
I noticed [specific learning]. This seems like a good [convention/pattern] to capture.

I suggest adding to [file-path]:

Section: [section-name]
- [Key point 1]
- [Key point 2]

Would you like me to:
1. Do this now
2. Do this after we finish [current task]
3. Skip it
```

**For path-specific rules, include the glob pattern:**

```
I noticed [specific learning] that applies to files in [directory].

I suggest creating .claude/rules/[name].md:

---
paths: [glob-pattern]
---

# [Section Title]

- [Key point 1]
- [Key point 2]

Would you like me to:
1. Do this now
2. Do this after [current task]
3. Skip it
```

### Timing Strategies

**During Task (Interrupt Mode) - Use Sparingly**
- **ONLY when:** User's feedback directly contradicts existing skill, creating active harm
- **Example:** "The skill tells me to use approach X, but you just said never do X"
- **Ask:** "I notice this conflicts with [skill-name]. Should I update that skill now or after this task?"

**After Task Completion (Deferred Mode) - PREFERRED**
- **When:** Learning is valuable but not urgent
- **At natural stopping points:** Task complete, user says "thanks", user pauses
- **Ask:** "I noticed [pattern] during this session. Would you like me to [action]?"

**After Session (Observation Mode)**
- **When:** Pattern emerges across multiple tasks in session
- **After demonstrating:** Pattern occurs 2-3 times
- **Say:** "I've noticed we're doing [X] repeatedly. This might be worth capturing in a skill. Should I create one?"

**Never Interrupt When:**
- User is mid-explanation or asking questions
- Task has encountered errors or blockers
- User seems focused or in flow state
- Learning is trivial or highly specific to current unique task

## Step 6: Implementing the Change

Once approved, execute the change promptly and efficiently using the appropriate tools.

### For Skills

#### For New Skills

For creating new skills, invoke the /skill-creator skill and refer to the instructions there.

**Validate:**
```bash
~/.claude/skills/skill-creator/scripts/quick_validate.py <path-to-skill>
```

#### For Updating Existing Skills

1. **Read the current skill:**
   - Use Read tool to examine current SKILL.md
   - Identify appropriate section to extend
   - Match existing style and structure

2. **Make minimal additions:**
   - Add new information to relevant section
   - Include concrete example if helpful
   - Keep additions concise and focused
   - Don't restructure unless necessary

3. **Validate:**
   - Run quick_validate.py to ensure still valid

### For CLAUDE.md Files

#### Determine File Path

1. **For path-specific rules:**
   - Create in `.claude/rules/` with descriptive name
   - Use Write tool if file doesn't exist
   - Use Edit tool if file exists

2. **For project-level:**
   - File path: `./CLAUDE.md` or `./.claude/CLAUDE.md`
   - Use Write tool if file doesn't exist
   - Use Edit tool if file exists

3. **For user-level:**
   - File path: `~/.claude/CLAUDE.md`
   - Use Write tool if file doesn't exist
   - Use Edit tool if file exists

4. **For project-local:**
   - File path: `./CLAUDE.local.md`
   - Use Write tool if file doesn't exist
   - Use Edit tool if file exists

#### For New Rules Files

When creating new files in `.claude/rules/`:

```markdown
---
paths: "pattern/here/**/*.ext"
---

# Section Title

- Learning point 1
- Learning point 2
```

**YAML frontmatter notes:**
- Always quote glob patterns
- Use standard glob syntax (**, *, {}, etc.)
- Multiple patterns: `paths: "src/**/*.ts, tests/**/*.ts"`
- Validate YAML syntax after writing

#### For Existing Files

1. **Read the current file:**
   - Use Read tool to examine current content
   - Identify appropriate section or create new one
   - Match existing style and structure

2. **Add content maintaining consistency:**
   - Add to existing section if appropriate
   - Create new section with ## or ### if needed
   - Use bullet points for scannable content
   - Include concrete examples when helpful
   - Keep additions concise (2-5 bullets typically)

3. **Use Edit tool with targeted changes:**
   - Match indentation and formatting
   - Preserve existing section structure
   - Add content in logical location

#### Validate

After writing to CLAUDE.md or rules files:

1. **Verify file was written successfully** (re-read to confirm)
2. **For rules files with frontmatter:**
   - Parse YAML to ensure valid syntax
   - Validate paths field contains valid glob patterns
   - Check that markdown content follows frontmatter
3. **Check file size** (warn if >400 lines, suggest splitting)
4. **Confirm content placement** (content added to appropriate section)

### Growth Through Iteration

New skills should start minimal and grow based on real usage:
- v1: Core pattern + one example
- v2: Add variations discovered in actual use
- v3: Add error handling for issues encountered
- v4: Add references/ directory if content grows large

Don't try to anticipate everything upfront - let skills evolve naturally.

## References

- **decision-framework.md** - Detailed flowcharts and decision trees for assessing significance, choosing update vs create, local vs user-level, and timing strategies
- **examples.md** - 15-20 concrete scenarios with "Should Capture" / "Should Not Capture" judgments showing the framework in action
