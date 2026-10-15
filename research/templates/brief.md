# The brief: the converging object

`<effort>/brief.org` is the one file the research converges in. It is rewritten each round, never appended to as a log; `git log -p -- brief.org` is the record of how the goals moved. The user reads it to see where the effort stands; the hand-off is written from it.

## Form

```org
#+TITLE: Research: <effort, in plain words>
#+OPTIONS: ^:{} -:nil
#+STATUS: open

<One paragraph: what this effort is for, when it started, where its evidence, threads and probes are.>

* Why

<The user's words, verbatim and dated. Add later quotes that changed the direction.>

* Goals (speculated)

- *R1. <Outcome, in the user's terms.>* <What makes it observable.>

* Non-goals

- <What a reader might expect and this does not do.>

* Decided

|       Date | Decision |
|------------+----------|
| <date>     | <One sentence; what it replaces, if anything.> |

* Terms

<Only while there is no code root; otherwise the terms go in docs/glossary.org.>

- *<term>*: <what it is, one or two sentences>. Example: <the user's use>. Avoid: <rejected words>.

* Assumptions

- <Assumption>. Origin: <where it came from: the user, a shelved design, prior art, a guess>.

* Tensions with the record

- <Where this conflicts with an existing goal, decision or document, and how it is resolved.>

* Open Questions

** <The question in plain terms>
<Critical or not, and why. Options with their consequence, the recommendation.>

* What the evidence shows so far

Descriptive; each line cites its evidence file.

- *<Finding in one phrase.>* <One or two sentences> (E1).

* Threads

| Thread | Question | File | Layout | Status |
|--------+----------+------+--------+--------|

* Probes

| Probe | Question | File | Result |
|-------+----------+------+--------|

* Evidence

| Id | Question | File | Status |
|----+----------+------+--------|
```

## Rules

- **Goals** are headed `(speculated)` until the user confirms them; the heading drops the word when they do.
- **Non-goals** are only the user's; a proposed one is marked "proposed, not yet confirmed".
- **Decided** is the user's decisions only, one row each, dated. A reversal is a new row naming what it replaces; the old row stays.
- **Open Questions** say whether each is critical (its answer could change a goal, a non-goal or a spec scenario). A closed question keeps one line saying where it was closed.
- Empty sections are left out.
- **`#+STATUS:`** is `open`, `handing-off <date>` (the hand-off is being written and reviewed; `SKILL.md` § Closing), `done <date>` (with a paragraph under the title naming the hand-off: each change, its branch, its base, its place in the sequence, the workspace integration branch when there is one and what was decided about main, and the date of the hand-off review) or `no-go <date>` (with the reason).
