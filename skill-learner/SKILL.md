---
name: skill-learner
description: Helps Claude iteratively capture learnings and patterns from sessions into skills. Use this skill when: (1) User explicitly corrects Claude's approach or provides feedback, (2) Claude notices repeated patterns across multiple tasks in the session, (3) User mentions repo-specific conventions or preferences, (4) Claude struggles with similar tasks multiple times, (5) User requests skill updates or creation, (6) Session reveals gaps in existing skill coverage. This skill operates proactively but asks permission before interrupting current work.
---

# Skill Learner

## Overview

This skill enables Claude to recognize learnings during sessions and incorporate them into the skills system through updates or new skill creation. It operates proactively but respects the user's workflow, asking permission before making changes.

## Core Principles

- **Capture only non-obvious, repeatable patterns** - Skip trivial one-off tasks
- **Extend existing skills when related; create new ones for distinct domains** - Avoid skill proliferation
- **Always review with user before implementing** - Never assume, always ask
- **Minimize interruption to current work** - Prefer end-of-task suggestions
- **Start new skills minimal and concise** - Incorporate only direct observations, grow through iteration
- **Consider appropriate hierarchy** - Local skills for repo-specific, user-level for broadly applicable

## Workflow Decision Tree

The skill-learning process follows five sequential steps:

1. **Detect** - Recognize learnings worth capturing
2. **Assess** - Evaluate significance and filter out noise
3. **Determine** - Decide on action type (update/create, local/user-level)
4. **Approve** - Get user permission at appropriate time
5. **Implement** - Execute the change using skill-creator tools

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

## Step 3: Determining Action Type

Make three key decisions about how to capture the learning:

### Decision 1: Update Existing vs Create New

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

### Decision 2: Local vs User-Level

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

### Decision 3: Timing and Interruption Strategy

See decision-framework.md for the timing decision matrix. Quick reference:

- **Interrupt mid-task:** ONLY when learning conflicts with existing skill (active harm)
- **Defer to end-of-task:** Preferred for most learnings (after task complete, natural stopping point)
- **Defer to end-of-session:** When user is in urgent/flow state but learning is valuable

## Step 4: Getting User Approval

Always ask before implementing skill changes. Use appropriate timing and clear communication.

### Approval Request Format

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

## Step 5: Implementing the Change

Once approved, execute the skill change promptly and efficiently.

### For New Skills

1. **Initialize structure:**
   ```bash
   ~/.claude/skills/skill-creator/scripts/init_skill.py <skill-name> --path <~/.claude/skills OR .claude/skills>
   ```

2. **Write minimal SKILL.md:**
   - Complete frontmatter with clear description (include "Use when..." triggers)
   - Brief overview (1-2 sentences of what and why)
   - Core workflow or pattern (the actual learning captured)
   - One concrete example showing usage
   - **Avoid:** Extensive background, multiple variations, comprehensive edge cases

3. **Example minimal skill structure:**
   ```markdown
   ---
   name: repo-deployment
   description: Deployment workflow for repository-name. Use when deploying changes to staging or production environments.
   ---

   # Deployment Workflow

   ## Overview
   Standard deployment process for this repository.

   ## Workflow
   1. Run tests: `npm test`
   2. Build: `npm run build`
   3. Deploy to staging: `./deploy.sh staging`
   4. Verify on staging URL
   5. Deploy to production: `./deploy.sh production`

   ## Example
   User: "Deploy my changes to production"
   → Follow steps 1-5, confirming at step 4 before step 5
   ```

4. **Clean up template files:**
   - Delete unused scripts/example.py
   - Delete unused references/api_reference.md
   - Delete assets/ directory if not needed

5. **Validate and package:**
   ```bash
   ~/.claude/skills/skill-creator/scripts/quick_validate.py <path-to-skill>
   ~/.claude/skills/skill-creator/scripts/package_skill.py <path-to-skill>
   ```

### For Updating Existing Skills

1. **Read the current skill:**
   - Use Read tool to examine current SKILL.md
   - Identify appropriate section to extend
   - Match existing style and structure

2. **Make minimal additions:**
   - Add new information to relevant section
   - Include concrete example if helpful
   - Keep additions concise and focused
   - Don't restructure unless necessary

3. **Validate and re-package:**
   - Run quick_validate.py to ensure still valid
   - Run package_skill.py to update distribution

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
