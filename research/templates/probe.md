# A probe: one time-boxed experiment

`<effort>/probes/<slug>.org`. A probe settles one question by trying something, faster than argument would. It is throwaway: what it builds is evidence, not product.

## Form

```org
#+TITLE: Probe: <the question>
#+OPTIONS: ^:{} -:nil
#+STATUS: planned

* Question
<One question with a yes or no answer.>

* Expected result
<What you expect, and what would count as yes and as no. Written before running.>

* Time box
<Minutes or tool calls. One attempt.>

* Setup
<Where it runs (scratch directory, worktree, isolated Emacs target) and why that is isolated from
the user's real data.>

* Run
<What was done, the commands or calls, and their output, trimmed.>

* Result
<yes | no | inconclusive>, <date>. <One paragraph: what was observed against the expectation.>
```

## Rules

- **Question, expected result and time box are written before the run.** A probe without an expectation cannot fail, so it cannot fail fast.
- **One attempt.** `#+STATUS:` becomes `yes`, `no` or `inconclusive`.
- **Inconclusive is not retried.** It becomes a thread or an evidence question of its own; the probe file links to it.
- **Isolation.** Nothing touches the user's live data: an isolated Emacs target with scratch data, a scratch directory, a worktree. Clean up afterwards and say so in `* Run`.
- Add a row to the brief's `* Probes` table when the probe is planned, and record its result there and, when it bears on a goal, in `* What the evidence shows so far`.
