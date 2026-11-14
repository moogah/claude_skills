---
name: process-improvement
description: "Identify and propose improvements to the research and orchestrate skills and their roles, along quality, speed and cost, at the scope each is true for (a cycle, a change, a repository, a workspace, the whole process); run the open after-measures first; write a proposals file of a few rows the user decides on; apply the accepted rows where their scope lives, never installing or pushing. Use when the user invokes `/process-improvement`, asks to review or improve the process, to read the cycles a home has run, to run the after-measures, or to triage process notes; in an orchestrator home (`.claude/orchestrator/config.yaml`) or in the skills workspace (`home.org` with its homes table and `refinements.org`). Skip during a running cycle (the PM digest carries a cycle's signals), for a defect in the product's code (the change's record takes it), and for designing a new role or step (a research effort or a refinement session does that)."
---

# process-improvement

Formalises the loop the skills workspace ran by hand from 2026-09-26: the skills run in the homes, the record is read and described, a judgement names what to change, the change is made where it belongs, and the next run checks whether it worked. A run ends in a **proposals file** the user decides on; nothing is applied before the user answers.

The user's attention is one of the two costs this skill weighs. A run hands the user a few rows and one question, not a report.

## Terms

The terms agreed in the research (`research/process-improvement-skill/brief.org` § Terms in the skills workspace):

- **Proposal**: one row of the run's output: an improvement to a skill or a role, resting on a cited observation, with its dimension, its scope, the place it would land and a recommendation. Not a *finding* (a finding judges what happened; a proposal says what to change) and not an *ask* (the orchestrator's question to the user during a change).
- **Scope**: the smallest unit an improvement is true for: a cycle, a change, a repository, a workspace, or the whole process.
- **Dimension**: which of quality, speed or cost an improvement moves.
- **Currency**: which of the two costs a cost row moves: **machine** (tokens, context, agents' wall clock) or **the user's** (asks raised, decisions requested, reading handed to the user). A saving in one bought with the other is a cost increase.
- **After-measure**: what a proposal names before it is applied: the count or check, and the file or output it reads, that shows on the next run whether the change worked (`templates/after-measure.md`).

## Where a run happens

One skill, run on demand in either place. Resolve the place first:

| Place | Found by | Reads | Writes the proposals file at |
|---|---|---|---|
| A home | walking up from the cwd to `.claude/orchestrator/config.yaml` (the `orchestrate` skill's resolution) | that home's record: `.orchestrator/changes/*/` (cycles, briefs, reviews, digests, `meta-discoveries.json`, journals, hardening lists, ask files of every kind, `process` included), `.tasks/`, the overlay (`config.yaml`, `roles/*.md`, `priors.md`), its research efforts, and its session logs under `~/.claude/projects/` | `<home>/.orchestrator/proposals/<date>-<slug>.org` |
| The skills workspace | a `home.org` whose `* The homes that use them` table lists the homes, beside `refinements.org` and `roadmap.org` | the homes' proposals files in place (§ Intake), the maps, `intake/`, the copied logs, the findings, `refinements.org`, `roadmap.org`, the skill text on its branch | `proposals/<date>-<slug>.org` |

Neither found: ask the user once which place is meant. A home not listed in the skills workspace's `home.org` is not read from there.

`<slug>` names what the run read (`cycles-since-cf2577d`, `package-boundaries`). One proposals file per run, committed in that place's repository.

## The run

### 1. Measures first

Before reading anything new, run every open after-measure in this place and report each as **held**, **failed** or **not yet measurable**. This step changes no text; it is the one thing the skill does before the user answers. When a measure keys on cycles not yet read, the agents that read them (§ 2) collect its count, and the result is judged before § 3; the record: every measure of the first run keyed on the 56 unread cycles.

- The open measures, in a home: every row of an earlier proposals file there with `:MEASURE:` set and `:MEASURED:` `open` or `not yet measurable`. In the skills workspace: every bullet of `roadmap.org` § Unmeasured after-measures, and the `:MEASURE:` of every applied row of an earlier proposals file there not yet in that list.
- Each result names the file, script or output it read and the value it found (`templates/after-measure.md` § Results). "Not yet measurable" says why: no cycle since the change, the artifact it keys on not produced yet, a run needed.
- **Check the measure before trusting it.** A measure that reads zero items where the record has some is broken, not held. When the artifact it keys on changed shape, read the nearest equivalent and report the result as `substituted`, naming both. The record: three of fifteen measures run on 2026-10-12 could not see the artifact they were written for (asks had become org files; a counter blind to journal lines; a two-commit rule the new flow never produced).
- Record the results: in the proposals file's `* Measures` table; in a home also on each source row's `:MEASURED:` line; in the skills workspace also as an `Observed (<date>, <proposals file>)` line in the refinement entry the measure belongs to, and a held measure leaves `roadmap.org` § Unmeasured (forward-only). A failed measure is an observation for § 3 of this run.
- Tell the user the tally and each failure in a few lines before going further.

### 2. Read the record and describe it

Describe before judging, as separate steps; the description holds no verdict.

- **What to read.** In a home: the cycles since the last proposals file (its `#+READ:` line), or every cycle on a first run. In the skills workspace: the intake (§ Intake), and the homes' record and logs for the cycles the run is about.
- **Few cycles** (one change, a handful of cycles): read the files directly; the description is the proposals file's `* Read` section, one line per source.
- **Many cycles** (several homes or changes): the description is a map. Refresh the log copy first where the place keeps one (in the skills workspace, `maps/opsx-orchestrate/08-session-logs.org` § 1). Spawn one agent per home or change, in the background, each writing one objective map file (in the skills workspace, `maps/opsx-orchestrate/<NN>-<slug>.org` under its `CONVENTIONS.org`; in a home, beside the proposals file in `.orchestrator/proposals/<date>-<slug>/`) and handing back one line. Tell each: org-mode, `#+OPTIONS: ^:{} -:nil`, every bullet `[O]` observed or `[I]` inferred, locations as `path:line` or `session@line`, numbers from scripts over the sources, no severity or recommendation. A judging document over the maps (in the skills workspace, a dated file in `findings/`) is allowed; it is working material, and the proposals file is the deliverable.
- Read the earlier proposals files in this place, every refinement entry's "Left out" (skills workspace), and `roadmap.org` § Deferred: a row declined before is not proposed again unless a new observation is cited, and the row then names the old decision.

### 3. Judge: observations to proposals

**The noise rule.** An observation becomes a proposal only when it is a cause, not variation. It counts when one of these holds:

1. **It recurs**: the same thing in two or more cycles, or in two homes.
2. **It has a mechanism one can point at**: a rule the text states and a run that broke it, a tool that printed a wrong verdict, two files that contradict each other, a defect in present text or code. One instance is enough; a reasoned defect is never held back for a count (the `orchestrate` skill's YAGNI rule: scope takes the YAGNI check, defects are reported and routed).
3. **A number moved past the noise floor.** The record's floor: the C4 control arm, the same text on the same inputs two days apart, differed by 55% in main-thread calls, by a third in hand-back size, by one against seven conformance findings and by three doc-correction files. A one-cycle difference smaller than that, with no mechanism, is noise.

Anything else goes under `* Seen once` in the proposals file, one line with its citation; the next run checks whether it recurred. Deming's warning stands behind this: changing a system in response to normal variation makes it worse.

**Scope** is where the cause lives, not where the symptom appeared. When the cause is in the skill text it is process scope, wherever it was seen. When the cause is in the skill text but a home can carry a workaround in its overlay now, that is two rows: the workspace row applied in the home, and the process row marked for the skills; the skills run cites the home row's measure as its local proof. The record: the shared git stash rotated four of five Implementors' work; the home put the rule in its briefs and `priors.md` the same day and wrote a process ask for the skill.

**Few rows.** Five proposals at most per run, the ones with the most evidence and the widest effect first. Further candidates go under `* Held over` with their observation, first in line next run; none is dropped.

**Each row** (`templates/proposals.md`) carries the observation cited to the record, the dimension (and for cost, its currency), the scope, where it lands, the recommendation (`apply`, `defer`, `decline`) with its reason, and for a repository, workspace or process row the after-measure (`templates/after-measure.md`). A row that outlives one change and whose measure cannot be stated is recommended `defer`, never `apply`. Cycle and change rows name no measure: the change's MVP decision judges them.

**A cost row says which currency it moves**, and a row that saves one currency by spending the other says so; the net is judged in the row's recommendation.

**A new step or role** is a proposal of class `new step` or `new role`, at a higher bar than a refinement:

- it cites the repetition or the failure it answers, and passes the noise rule by recurrence or mechanism, not by one number;
- it carries a cost line in both currencies (what the new step costs per cycle and in the user's attention, against what it saves);
- it names the next step: a research effort when it would change a skill's goals (its north star, a skill's stated purpose, a non-goal), a refinement session otherwise.

The skill designs none of them; the design is that next step's work. Applying such a row writes the next step's entry point (§ 5).

**Not the product.** The skill judges the process and the skills, never the product's code, specs or design. A product defect seen on the way goes to the change's record by the existing path: an `issue` entry on the open change's hardening list (`~/.claude/skills/orchestrate/templates/hardening.md`), or a `.tasks/` item of the home with its provenance when no change is open. It is not a proposal. A run in the skills workspace writes nothing into a home but the marks on process asks (§ Intake): it lists each product defect under `* For the homes` in its proposals file, one line with the home and the citation, and the user routes it, or the next run in that home does.

### 4. Present and decide

Write the proposals file, commit it, and give the user, in one message: the measures tally (failures named), then one line per proposal (number, the change, dimension, scope, recommendation), the file's path, and one question: take the recommended set, or decide row by row. Row by row is one question per row, in the order of the file. The user may also answer in the file under `* Answer`. Record the user's words, dated, under `* Answer` and each row's `:DECISION:`.

No row is applied before the user answers.

### 5. Apply the accepted rows where their scope lives

In the same session, after the answer. Each applied row gets `:APPLIED_VIA:` (the commit, or the path and section).

| Scope | Lands in | How |
|---|---|---|
| Cycle | the open cycle's briefs (`<change-dir>/cycles/<id>/briefs/`), an ask, or the change's hardening list | the edit, committed in the home; an ask is raised with `asks.py add`, never written by hand |
| Change | what the process wrote about the change: its hardening list, an open question in its `intent.org`, its base or branch in `change.json` (through `state.py`), a dangling reference in its record | committed in the repository the file lives in (the home, or the code root for `docs/`) |
| Repository | the repository's entry under `repos:` in the home's `config.yaml`; the code root's `CLAUDE.md` or `docs/` | committed there |
| Workspace | the home's overlay: `.claude/orchestrator/config.yaml`, `roles/<role>.md`, `priors.md` (one line in the user's words, `orchestrate` `overlay.md` § Priors) | committed in the home |
| Process, run in a home | not applied in the home: the row keeps `:FOR: skills`, its decision is `send`, and the skills workspace reads it in place | nothing to commit beyond the proposals file |
| Process, run in the skills workspace | the skill text in the skills worktree on its branch (`spec-orchestration-refinement`, the `skills/` worktree), with an entry in `refinements.org` (`templates/refinement-entry.md`) naming the after-measure, and the measure added to `roadmap.org` § Unmeasured after-measures | committed on the branch; the entry and roadmap committed in the workspace |

- **The limit.** A process row whose edit touches more than three files of the skills (a script and its test count as two) gets its refinement entry written, marked `Not yet applied`, and the edit is handed to a fresh session: tell the user to start Claude Code in the skills workspace with "Apply the refinement `<entry title>` from refinements.org".
- **A new step or role**, accepted: in the skills workspace, a `roadmap.org` § Next item naming the research effort or the refinement session to run, with the row's observation and cost line; in a home, the row is process scope and goes to the skills as above.
- **Deferred rows**: in the skills workspace also a bullet under `roadmap.org` § Deferred until a demonstrated reason, with the reason. **Declined rows** stay only in the proposals file, with the user's words; the next run reads them (§ 2).
- **Never installed or pushed.** The skill does not fast-forward `~/.claude/skills`, push, or tag; those are the user's acts. Say in the closing message that the branch is ahead of the installed text when it is.

### 6. Close: the run's own cost

Count the run's cost in both currencies with `scripts/cost.py` (below), write it under `* Cost` in the proposals file, set `#+STATUS:`, commit, and end with one line to the user: what was applied, where, and the cost.

## Intake: process rows from the homes

The home's proposals file is the channel. A run in a home marks its process rows `:FOR: skills`. A run in the skills workspace reads every listed home's `.orchestrator/proposals/*.org` in place and takes each row with `:FOR: skills` and `:DECISION: send` as an observation of its own, cited as `<home>/.orchestrator/proposals/<file>.org § P<n>`. Nothing is copied into the skills workspace, and nothing is written into the home from there but the marks below: a row is taken once a proposals file in the skills workspace cites it.

The homes' `:KIND: process` ask files reach the skills the same way: a run in the home reads them and turns them into rows. Where a home has process asks and no proposals file yet, the skills run may read the ask files directly as working material, citing them in place. A skills run that read them that way marks each one when the user agrees, the one write into a home: through that home's `asks.py set` (the status; `applied_via` for an applied one; the marking in `* Answer` only when the user left it empty), one commit per home. The record: the PMs flagged an open process ask as stale every cycle for a change's whole life.

## Counting the two currencies

| Currency | Counted from | How |
|---|---|---|
| Machine | session transcripts under `~/.claude/projects/<project>/` (a parent `<session>.jsonl`, its `<session>/subagents/*.jsonl`) | `scripts/cost.py`: tokens per assistant message once (a message spans several records), split into input, cache-write, cache-read and output; wall clock from first to last record; agents spawned |
| The user's | the same transcripts and the home's files | typed prompts and question-tool calls (`cost.py`); ask files of kind `decision` raised (`inbox/` and `cycles/*/asks/`); reading handed over, as the lines and words of the files the user is asked to read (`cost.py --reading <file>`) |

```
python3 ~/.claude/skills/process-improvement/scripts/cost.py [--project <dir>] [--session <id-prefix>] [--since <ISO time>] [--reading <file> ...]
```

With no `--project` it uses the project directory of the cwd; with no `--session`, the most recently written transcript there (the running session). `--since` counts only records from that time, so a run that started part-way through a session counts itself. It prints org lines for `* Cost`. For a cycle's or a change's cost, point it at the dispatcher's session and the window the cycle ran in.

## What this skill does not do

- It is not a role in every cycle. It runs only when invoked; what a cycle learns goes to the next plan in its handshake.
- It does not review the product. Product defects go to the change's record (§ 3).
- It does not install, push or tag.
- It applies nothing before the user answers, at any scope; running the open measures is the one exception and changes nothing.
- It does not design a new role or step; it proposes one and names the next step.
- It is not a telemetry system. It measures with the record and the scripts that exist; new instrumentation is itself a proposal with a demonstrated reason. The record: the workspace's measurement scripts grew to 82, 21 of 40 cited once.

## Borrowed

| From | What | Source |
|---|---|---|
| Agile retrospectives (Derby & Larsen; the Scrum Guide) | One or two actions per round; review the last round's actions before new ones (here: measures first) | scrumguides.org/scrum-guide.html; scrum.org/resources/blog/ditch-unfinished-action-items (read 2026-10-20) |
| PDSA and Deming's System of Profound Knowledge | A change is a prediction checked afterwards; act on special causes, not common variation (the noise rule) | en.wikipedia.org/wiki/PDCA; en.wikipedia.org/wiki/W._Edwards_Deming (read 2026-10-20) |
| Six Sigma DMAIC | Check the measurement before the change is measured with it | en.wikipedia.org/wiki/DMAIC (read 2026-10-20) |
| Google SRE postmortems | A measurable end state per action; "a postmortem without subsequent action is indistinguishable from no postmortem"; the loop's own toil is surveyed (the run reports its cost) | sre.google/workbook/postmortem-culture/; sre.google/sre-book/postmortem-culture/ (read 2026-10-20) |
| Etsy debriefing guide | Describe before judging; the "dashboard trap" (new instrumentation as a superficial fix) | extfiles.etsy.com/DebriefingFacilitationGuide.pdf (2016; read 2026-10-20) |
| Toyota Improvement Kata | The expected result is written before the experiment (the after-measure before apply) | Rother, IK Reference Guide, public.websites.umich.edu/~jmondisa/TK/KATA_Files/IK_Reference_Guide.pdf (read 2026-10-20) |
| CMMI CAR and OPM | Two-tier scope: a project's improvement goes to the organisation once shown effective locally (a home's row as local proof for the skills) | wibas.com/cmmi/carsp-23-record-causal-analysis-data; wibas.com/cmmi/organizational-performance-management-opm-cmmi-svc (read 2026-10-20) |
| Anthropic skill-creator | Observed repetition as the trigger for new structure; the user's feedback as the primary signal | raw.githubusercontent.com/anthropics/skills/main/skills/skill-creator/SKILL.md (read 2026-10-20) |
| Anthropic, "Demystifying evals for AI agents" | Fix the grader before blaming the agent; read transcripts, not scores alone | anthropic.com/engineering/demystifying-evals-for-ai-agents (read 2026-10-20) |
| AgentField, "agents that ship themselves" | The improver proposes and a person promotes: "It still does not get to promote itself" (never install) | agentfield.ai/blog/agents-that-ship-themselves (read 2026-10-20) |

The research that produced this skill, with every source cited and the workspace's own loop measured: `~/emacs-workspaces/claude skills/research/process-improvement-skill/` (brief; evidence E1 prior art, E2 the loop as practised).
