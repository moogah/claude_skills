# A thread: one line of investigation

`<effort>/threads/<slug>/thread.org`, worked in its own Claude session that the user starts. The thread follows the skill's loop inside its bounds and returns to the brief through `* Hand-back`.

## Form

```org
#+TITLE: Thread: <the line of investigation>
#+OPTIONS: ^:{} -:nil
#+STATUS: open

A line of investigation of the research effort [[file:../../brief.org][<effort>]], opened <date>. It runs in its
own Claude session; its findings come back to the brief through § Hand-back.

* How to start the session

Start Claude Code in the workspace root, =<workspace root>= (so the session loads the workspace's
memory), and say:

#+begin_example
Work the research thread at <path from the workspace root>/thread.org.
#+end_example

* Question

<One question.>

* Why

<The user's words that opened it; the evidence that makes it worth a thread.>

* Start from

- <The brief's goals and open question it serves.>
- <Evidence files, each with the one line that matters here.>

* Sub-questions

1. <…>

* Bounds

- <What is out: e.g. no code, no skill text; proposals for those go in § Hand-back.>
- One question at a time to the user, under the brief's depth rule.
- Research agents, if any, write to [[file:evidence/][evidence/]] in this directory.
- Probes are time-boxed and state their question and expected result first.

* Findings

<Observed facts gathered in the thread, marked [O] or [I].>

** Decisions in this thread

|       Date | Decision |
|------------+----------|

* Hand-back

What returns to the brief: the decisions (with dates), proposed goal edits, and open items marked
critical or not. The thread is done when the user calls it and no critical item is open.
```

## Closing a thread

When the user calls it done: set `#+STATUS: done`; in the brief, apply the goal edits, add Decided rows that summarise the thread's decisions and link to it, close or annotate the open question it served, list its open items (critical or not), and mark the thread `done <date>` in the `* Threads` table. Commit both files together.
