# The refinement entry

One entry in the skills workspace's `refinements.org`, appended at the end of `* Refinements`, for every process row applied (or handed to a fresh session) from a run there. It keeps the form every entry since 2026-09-27 has: what it acts on, what changed, what was left out and why, what was verified, and the after-measure.

## Form

```org
** <date> <What changed, in plain words>

Acts on [[file:proposals/<date>-<slug>.org][the proposals of <date>]] § P<n> (and § P<m>): <the observation, in a sentence>. Skills =<sha>= on =spec-orchestration-refinement=, not installed.

- *Changed* =<skill>/<file>= (§ <section>): <what it now says or does>.
- *New* =<skill>/<file>=: <what it is>.

Left out, with the reason: <what a reader might expect and was not done; each with why: the YAGNI check, the user's choice, the limit, a later step>.

Verification: <what was checked before the commit: a grep, the tests, a fixture run, =rule-homes.py=>.

Measure: <the after-measure line, `templates/after-measure.md`>.
```

When the limit applies (more than three skill files), the entry is written the same way with `Not yet applied: handed to a fresh session` in place of the skills commit and of Verification; that session fills them in and commits.

Later runs append `Observed (<date>, <proposals file>): held | failed | not yet measurable: <value>` to the Measure paragraph. Nothing else in an entry is rewritten; a later change is a later entry.
