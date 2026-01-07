# Decision Framework

This document provides detailed decision trees and flowcharts for the skill-learner workflow. Use this as a reference when assessing learnings and determining appropriate actions.

## 1. Significance Assessment Flowchart

Use this flowchart to determine if a learning is worth capturing:

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
    Proceed to Action Determination (Section 2)
```

### Significance Examples

**Should Capture:**
- User corrects approach 3 times with same correction
- User mentions non-standard repo convention
- Claude struggles, user provides key insight that resolves it
- Pattern emerges across multiple distinct tasks

**Should Filter Out:**
- Single typo correction
- Standard git commands Claude already knows
- User's temporary workaround for one-time issue
- Information easily found in existing code comments

## 2. Action Determination Framework

Once significance is confirmed, determine the appropriate action:

### 2A. Update Existing vs Create New Decision Tree

```
Start: Significant learning confirmed
    ↓
Question 1: Does an existing skill cover this domain?
    ├─ No → CREATE NEW SKILL (go to Section 2C for location)
    └─ Yes → Continue to Question 2
         ↓
Question 2: Does this learning directly extend the existing skill's purpose?
    ├─ No → CREATE NEW SKILL (distinct domain)
    └─ Yes → Continue to Question 3
         ↓
Question 3: Would adding this make the existing skill unfocused or too broad?
    ├─ Yes → CREATE NEW SKILL (avoid bloat)
    └─ No → UPDATE EXISTING SKILL
         ↓
    Proceed to implementation (Step 5 in SKILL.md)
```

### 2B. Examples: Update vs Create

**Update Existing Skill:**
- Learning: User wants to add watermarks to PDFs
- Existing: pdf skill with merge, split, extract operations
- Decision: UPDATE - watermarks are another PDF operation that fits naturally

- Learning: User specifies Prettier with specific flags
- Existing: code-formatting-preferences skill with ESLint configs
- Decision: UPDATE - Prettier fits with other formatting tools

**Create New Skill:**
- Learning: User shows deployment workflow for this specific repo
- Existing: General git-workflow skill exists
- Decision: CREATE - deployment is distinct and repo-specific

- Learning: User explains database connection and query patterns for project DB
- Existing: General sql-best-practices skill exists
- Decision: CREATE - specific connection patterns warrant their own skill

### 2C. Local vs User-Level Decision Tree

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

### 2D. Examples: Local vs User-Level

**Local Skills (.claude/skills):**
- "repo-deployment" - References ./deploy.sh, specific staging URLs
- "project-database" - Contains this project's DB schema and connection strings
- "team-testing-workflow" - Uses team-specific test harness in this repo
- "api-endpoints" - Documents this application's specific API routes

**User-Level Skills (~/.claude/skills):**
- "prettier-preferences" - General code formatting preferences across all projects
- "react-testing-patterns" - Best practices for React testing (any React project)
- "git-commit-conventions" - User's preferred commit message style
- "python-linting-setup" - How user likes to configure Python linters

**Edge Cases:**
- Team convention that applies to multiple repos → USER-LEVEL (still broadly applicable)
- Framework-specific pattern for framework only used in this repo → USER-LEVEL (framework knowledge is transferable)

## 3. Timing Decision Matrix

Determine when to suggest the skill update/creation to the user.

### 3A. Timing Decision Table

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

### 3B. Timing Decision Flowchart

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

### 3C. Interruption Script Examples

**High-Priority Interrupt (Conflict with Existing Skill):**
```
⚠️ Quick note: I just suggested [action] based on the [skill-name] skill, but you
mentioned we should actually [different approach]. This contradicts the existing skill.

Should I update [skill-name] now to reflect this, or wait until after we finish
[current task]?
```

**Preferred Timing (End of Task):**
```
I noticed [specific pattern/correction] during this task. This seems worth capturing
because [reason].

Would you like me to [update skill-name / create new skill for X] to include:
- [Key point 1]
- [Key point 2]

Options:
1. Do this now
2. Do this after we finish [next task if applicable]
3. Skip it
```

**Deferred Suggestion (After Multiple Tasks):**
```
Before we move on - I've noticed [pattern] across the last few tasks we worked on.
This could be valuable to capture in a skill.

Should I create a [skill-name] skill to remember this for future sessions?
```

## 4. Complete Decision Process Example

Let's walk through a complete example using all decision trees:

### Scenario
User asks Claude to deploy code 3 times during session. Each time:
1. First attempt: Claude suggests `git push`
2. User corrects: "No, run `npm run test:integration` first"
3. Claude does that
4. User then says: "Now run `./deploy.sh staging`, verify the staging URL works, then run `./deploy.sh production`"

### Decision Process

**Step 1: Detect**
- Signal type: Repeated pattern (3 times) + Explicit corrections
- Detection: ✅ Clear learning opportunity

**Step 2: Assess Significance**
- Q1: Repeated 3 times AND explicitly stated? → YES
- Q2: Non-obvious (repo-specific workflow)? → YES
- Q3: Will apply to future deployments? → YES
- Q4: Meaningful complexity (multi-step)? → YES
- **Result: CAPTURE THIS LEARNING** ✅

**Step 3A: Update vs Create**
- Q1: Existing skill covers deployment? → NO (assuming no existing skill)
- **Result: CREATE NEW SKILL** ✅

**Step 3B: Local vs User-Level**
- Q1: References specific files (./deploy.sh)? → YES
- **Result: LOCAL SKILL** ✅
- **Location: `.claude/skills/repo-deployment`**

**Step 3C: Timing**
- Q1: Contradicts existing skill? → NO (no existing deployment skill)
- Q2: User mid-task? → NO (just finished 3rd deployment)
- Q3: User just completed task? → YES
- **Result: SUGGEST NOW** ✅

**Step 4: Approval Request**
```
I've noticed we've followed the same deployment workflow 3 times now:
1. Run integration tests
2. Deploy to staging with ./deploy.sh
3. Verify staging
4. Deploy to production

This seems like a standard process for this repo. Would you like me to create a
"repo-deployment" skill to remember this workflow for future deployments?

Options:
1. Yes, create it now
2. Yes, but let's refine the steps first
3. No, skip it
```

**Step 5: Implementation** (if user approves)
- Initialize: `init_skill.py repo-deployment --path .claude/skills`
- Write minimal SKILL.md with the 4-step workflow
- Clean up template files
- Validate and package

## 5. Edge Cases and Special Situations

### When User Provides Conflicting Information

If user corrections conflict across different times:
1. Note the inconsistency
2. Ask user to clarify which approach is preferred
3. Only capture once consistency is established

Example:
```
I've noticed different approaches for [X]:
- In task A, you preferred [approach 1]
- In task B, you preferred [approach 2]

Which approach should I capture in the skill, or does it depend on [some context]?
```

### When Learning is Partial

If Claude suspects there's a pattern but hasn't seen it enough times:
1. Continue working normally
2. Make mental note to watch for pattern
3. Only suggest after pattern is confirmed (2-3+ instances)

Don't prematurely suggest based on single instance.

### When User is Learning/Exploring

If user is experimenting or trying different approaches (not settling on one):
1. Recognize this as exploration, not established pattern
2. Don't suggest capturing until user settles on preferred approach
3. Watch for user saying "this is the right way" or consistently using one approach

### When Multiple Learnings Emerge Simultaneously

If multiple distinct learnings emerge in same session:
1. Prioritize based on significance and user corrections
2. Suggest one at a time (don't overwhelm)
3. Use end-of-session to suggest remaining learnings if appropriate

Example:
```
Before we wrap up, I noticed a couple of patterns today:
1. [Pattern A] - would fit in a [skill-name] skill
2. [Pattern B] - would be a new [skill-name] skill

Would you like me to capture either or both of these?
```

## 6. Summary Quick Reference

**Capture when:**
- 2-3+ repetitions OR explicit user statement
- Non-obvious information
- Future applicability
- Meaningful complexity

**Update existing skill when:**
- Learning fits existing skill's domain
- Extends without making unfocused
- Clear relationship to existing content

**Create new skill when:**
- Distinct domain/capability
- No existing skill covers it
- Would make existing skill too broad

**Local skill when:**
- Repo-specific files/paths
- Team conventions for this project
- Project-specific schemas/APIs

**User-level skill when:**
- Generally applicable best practices
- Tool/framework patterns across projects
- When uncertain (easier to migrate later)

**Timing:**
- Interrupt: Only for conflicts with existing skills
- Suggest: After task completion (preferred)
- Defer: During mid-task, debugging, or flow state
