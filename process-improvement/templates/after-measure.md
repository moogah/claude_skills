# The after-measure line

Every proposal that outlives one change (repository, workspace, process) names its after-measure before it is applied: what to count or check, the value that means it worked, and the file or output it reads. The next run in that place runs it first (`SKILL.md` § 1). Cycle and change rows name none; the change's MVP decision judges them.

## Form

One line, in the proposal's `:MEASURE:` property and, for a process row, in the refinement entry's `Measure:` line and in `roadmap.org` § Unmeasured after-measures:

```
<what is counted or checked> <expected value> :: <file, script or output it reads> [:: <where: which home, which cycles>]
```

Examples from the record:

- `bypass writes to state.json: 0 :: findings/tools/state-writes.py bypass`
- `Implementor-end to Reviewer-spawn latency within 0-3 min :: findings/tools/agent-timeline.py`
- `every buttercup failure in an after-file is named :: compare.py over <change-dir>/after-*.txt :: package-boundaries cycle 2`
- `rule-homes failures: 0 :: findings/tools/rule-homes.py skills/orchestrate`
- `no ask with no :BLOCKS: takes AskUserQuestion :: the dispatcher's transcript, AskUserQuestion calls against ask ids`

## What makes one good

- **It names the value.** "0 failures", "under 100k", "within 0-3 min"; not "fewer" or "better". Without a value, held and failed cannot be told apart.
- **It reads a file already in git where it can.** On the record the cheapest measures counted ask files, journal lines and review files; the dearest needed a replay or a live run. A measure that needs a run says so and is `not yet measurable` until one happens.
- **It keys on the artifact, and says which.** Three of fifteen measures run on 2026-10-12 broke because the artifact they keyed on changed shape. Name the artifact so the next run can tell when it has.
- **It uses a script that exists.** A new script is new instrumentation, itself a proposal with a demonstrated reason (`SKILL.md` § What this skill does not do). A grep or `wc` over named files needs no script.
- **It can fail.** A measure that the change makes true by construction (the file exists because the change wrote it) shows nothing; measure the behaviour the change was for.

## Results

Written by the run that measures, in the proposals file's `* Measures` table and on the source:

```
held <date>: <value read> (<file or output>)
failed <date>: <value read> (<file or output>)
substituted held <date>: <value read> (<the artifact read instead>, because <the original changed shape>)
not yet measurable <date>: <why: no cycle since; the artifact not produced; a run needed>
```

In the skills workspace the result is also an `Observed (<date>, <proposals file>)` line appended to the refinement entry's measure paragraph; a held measure leaves `roadmap.org` § Unmeasured, a failed one stays there, marked failed, until the proposal that answers it is applied or declined.
