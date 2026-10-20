# The proposals file

One org file per run: `<home>/.orchestrator/proposals/<date>-<slug>.org` in a home, `proposals/<date>-<slug>.org` in the skills workspace. It is the run's deliverable and the record of the user's decisions; the next run in the same place reads it for open measures (`:MEASURED:`), for rows already declined, and, in the skills workspace, for intake rows already taken. Keep it short: its length is reading handed to the user, which the run counts under `* Cost`.

## Form

```org
#+TITLE: Proposals: <place, in plain words>, <date>
#+OPTIONS: ^:{} -:nil
#+DATE: <date>
#+PLACE: <home path> | skills workspace
#+READ: <what the run read: changes and cycle ids, or since <date or commit>; the last cycle id read>
#+SKILLS: <the installed skills commit the cycles ran on>
#+STATUS: proposed | answered <date> | applied <date>

* Measures

Open after-measures in this place, run before anything below was proposed.

| Measure | Belongs to | Reads | Result | Value |
|---------+------------+-------+--------+-------|
| <the count or check, with its expected value> | <proposals file § P<n>, or the refinement entry> | <file, script or output> | held / failed / substituted held / substituted failed / not yet measurable | <the number or line read; for not yet measurable, why> |

* Read

<Few cycles: one line per source read. Many cycles: one line per map or finding written, with its path.>

* Proposals

** P1. <The change, in one line>
:PROPERTIES:
:CLASS:          refinement | new step | new role
:DIMENSION:      quality | speed | cost (machine) | cost (user)
:SCOPE:          cycle | change | repository | workspace | process
:LANDS_IN:       <file and section, or the next step for a new step or role>
:FOR:            skills
:RECOMMENDATION: apply | defer | decline
:MEASURE:        <count or check, expected value> :: <file, script or output it reads>
:DECISION:
:APPLIED_VIA:
:MEASURED:       open
:END:

- Observation :: <What happened, cited: =path:line=, =session@line=, a commit.> Noise rule: <recurs (n cycles, n homes) | mechanism (what breaks what) | past the floor (the number against the floor)>.
- Change :: <What would change, at the place named, in a sentence or two.>
- Scope :: <Where the cause lives, and why not a narrower scope.>
- Recommendation :: <Why apply, defer or decline; for a defer, what would make it ready.>
- Cost :: <New step or role, and any row that moves cost: each currency, per cycle, with what it saves.>
- Left out :: <What a reader might expect this row to do and it does not, with the reason.>

* Seen once

- <Observation, cited.> <What would make it a cause: a second cycle, a mechanism.>

* Held over

- <Candidate proposal beyond this run's five, with its observation cited.>

* Cost

- Machine :: <tokens: input, cache write, cache read, output>; <wall clock>; <agents spawned>.
- The user's :: <prompts typed>; <questions asked>; <decisions requested: rows put to the user>; <reading handed over: this file's lines and words, and any other file the user was asked to read>.

* Answer

<The user's words, dated, verbatim.>
```

## Rules for the fields

- `:FOR: skills` only on a process row of a run in a home; omit it otherwise. Its decision is `send` (the skills workspace reads it in place) or `decline`.
- `:MEASURE:` on repository, workspace and process rows, and on a process row sent from a home (the skills run may sharpen it). Absent on cycle and change rows. A row that needs one and has none is recommended `defer`.
- `:DECISION:` is the user's: `apply`, `defer`, `decline`, or `send`. `:APPLIED_VIA:` is filled when applied: the commit (`<repo> <sha>`) or `path § section`.
- `:MEASURED:` starts `open`. Later runs write `held <date>`, `failed <date>`, `substituted held <date>` or `not yet measurable <date>: <why>`, each followed by the value read (`templates/after-measure.md` § Results). Rows with no measure carry no `:MEASURED:`.
- A row proposed again after an earlier decline cites the earlier file and row in its Observation and says what is new.
- Property values are one line each. The prose under the drawer is the description list above; every item fits in a few lines.

## Example row

Illustrative; the numbers are not from the record.

```org
** P2. Implementor briefs stop restating the overlay's test command
:PROPERTIES:
:CLASS:          refinement
:DIMENSION:      cost (machine)
:SCOPE:          workspace
:LANDS_IN:       .claude/orchestrator/roles/implementor.md
:RECOMMENDATION: apply
:MEASURE:        no brief in the next three cycles contains the test command :: grep -c over cycles/*/briefs/*.md
:DECISION:
:APPLIED_VIA:
:MEASURED:       open
:END:

- Observation :: 14 of 14 briefs in cycles 1792372508-1792419670 repeat the command already in =config.yaml= =test.command= (=cycles/*/briefs/*.md=, grep). Noise rule: recurs (5 cycles).
- Change :: Drop the "Run the tests with" paragraph from the role overlay; the core brief already names the overlay's command.
- Scope :: The paragraph is this home's overlay text; the core brief is right.
- Recommendation :: Apply: one paragraph, no behaviour changes.
- Left out :: Trimming the core brief; nothing there was observed to repeat.
```
