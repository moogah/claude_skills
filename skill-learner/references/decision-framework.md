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
    Proceed to Action Determination (Section 2)
```

## 2. Action Determination

### 2A. Update Existing vs Create New

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

### 2B. Local vs User-Level

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

## 3. Timing Decisions

### 3A. When to Suggest

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

### 3B. Timing Flowchart

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

## 4. Edge Case: Conflicting Information

If user corrections conflict across different times:

```
I've noticed different approaches for [X]:
- In task A, you preferred [approach 1]
- In task B, you preferred [approach 2]

Which approach should I capture in the skill, or does it depend on [some context]?
```

Only capture once consistency is established.

## 5. Quick Reference

**Capture when:**
- 2-3+ repetitions OR explicit user statement
- Non-obvious information
- Future applicability
- Meaningful complexity

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
- Interrupt: Only for conflicts with existing skills
- Suggest: After task completion (preferred)
- Defer: During mid-task, debugging, or flow state
