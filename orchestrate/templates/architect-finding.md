# Finding template

A **finding** is a defect, or a suspected defect, that an agent reports about code or text that exists. This file is the one statement of how a finding is labelled (§ Kind), when a piece of scope earns work (§ The YAGNI check) and how a finding is routed (§ Routing); the roles and the flows point here. Architect findings use the form below; the Reviewer's findings (`roles/reviewer.md` § Output contract) and the Implementor's discoveries (`roles/implementor.md` § Discoveries format) carry the same labels in their own files.

Reporting and routing are separate steps, done by different agents. The agent that finds a defect reports it, whatever it thinks of the defect's size, likelihood or place; the orchestrator's routing alone decides whether it is fixed now, goes on the hardening list, or goes to the user. The reason: Anthropic's prompting guides for current Claude models say that a model given a bar for what to report investigates just as thoroughly, finds the bug, and then leaves out what it judges to be below the bar. This skill's record shows the same: four of the nine defects `/code-review` found after a change was reported done had been written as journal lines below a bar, while every reasoned defect written as a finding was acted on.

## Form

Architect findings go in `<change-dir>/cycles/<cycle-id>/findings/<finding-id>.md`; the state file's `architect_findings` array carries the index fields in JSON form (`state.md`).

```yaml
---
finding_id: arch-<cycle-id>-<seq>
trigger: premise-check | conformance
kind: issue | question                      # § Kind
severity: high | medium | low               # § Kind
class: shape-fragmentation | vocabulary-mismatch | responsibility-leakage
       | dead-branch | interface-drift | mutation | invariant-gap | duplication
title: <one-line, ≤80 chars>
discovered_from: <task-name | batch-<cycle-id>>
discovered_by: architect
discovered_at: <iso-ts>
seam: <seam/<name> this finding is about, if any>
evidence: <the line, the run, the output or the shipped sentence; for a question, the reasoning and the reproduction to try>
decoration: blocking | hardening            # written by the orchestrator's routing (§ Routing), not by the Architect
---

## Locations

- file: scope-validation.el:504
  context: |
    constructs :error :tool :resource :command :message
- file: scope-shell-tools.el:181
  context: |
    constructs :tool :resource :reason :validation-type
- file: scope-expansion.el:504
  context: |
    consumes both, branches on presence

## Why tests missed it

<One sentence. The why-tests-missed line is the highest-leverage field
in this finding — it feeds the meta-discovery loop and the curation
cycle's index. Don't skip it. Typical patterns:

- "Each call site has its own test that pins its own shape; no test crosses the boundary."
- "Stages tested in isolation; no test crossed multiple stages with realistic data."
- "Per-call-site tests pinned their own subset; no test covered the full vocabulary."
- "Invariant stated in design.org; the scenario carried `:TEST: none:` and no test asserted it.">

## Recommended resolution

<One paragraph. Specific. Names files and functions. Avoids "consider"
and "perhaps" — the Architect's job is to recommend, not to deliberate.
When the code is right and a seam row is stale, say which row you
amended and how. Example:

"Extract canonical violation-info constructor as
build-violation-info in scope-validation.el; have callers in
scope-shell-tools.el:181 and scope-expansion.el:504 go through it;
delete the ad-hoc constructions. Amended seam/violation-info's owning
symbols to name build-violation-info as the one constructor.">
```

## Kind

Every finding carries one kind label, borrowed from Conventional Comments. The kind is the reporter's confidence; `severity` is the reporter's estimate of harm.

- **`issue`**: a defect you can point at in code or text that exists now. A failing run; a wrong output; an untrue sentence in a shipped document (a README, a usage page, a help text, a spec, `design.org`, a comment, an agent's instructions); behaviour the code has today that is wrong on some input it accepts, however rare the input: a check that accepts a remote path, a reply that garbles at one buffer size, a predicate that is true in any Emacs whose server has the default name; an old implementation still running beside its replacement. `evidence` is the line, the run, the output or the sentence.
- **`question`**: you suspect such a defect but are unsure it exists, for example because you could not run the case. `evidence` is the reasoning and the reproduction that would settle it: the input, the command, the probe. Trying that reproduction is the next step (§ Routing).
- **`thought`**: a speculative need, a requirement that would arise only under a future change: a third consumer, a later maintainer, another platform, a preset or caller nobody has written yet. Cleanup that changes no behaviour today is a thought too: a function nothing calls, two bodies that agree today, a redundant guard, a missing test for code that is right. One line. It is never a task by itself (§ The YAGNI check), and it is not a finding file: the Reviewer writes it under `## Thoughts` at the end of its review, the Architect as `state.py note note "thought: <one line>" --by architect`, the Implementor in `## Observations`.

The line between `question` and `thought`: does the harm need something to change first? A wrong result on an input the code accepts today is an `issue` or a `question`, however unlikely that input. A wrong result only once someone adds a caller, a platform or a preset is a `thought`.

`severity` is how bad the defect is when it triggers: `high` (wrong or lost data, a security or isolation breach, a failure on a use the change exists for), `medium` (a wrong result or a misleading message that has a workaround), `low` (a minor wrong output, an untrue comment). It orders the hardening list; it does not route.

Style and preference (naming taste, formatting, an equivalent structure you would have written differently) are not findings of any kind.

`kind` replaced `observed.happened` on 2026-10-14. That flag's `reasoning only:` label named the method every finding uses, so it lumped a reasoned defect (wrong today, not yet seen failing) together with a speculative need. An `issue` is what `happened: true` meant; a `question` or a `thought` was `happened: false`.

## The YAGNI check

The one statement of when a piece of **scope** earns work: a feature, an option, a generalisation, a task, a `.tasks/` item, or a question to the user. The roles and the flows name it as the YAGNI check. It asks: is this needed for what the user asked for now? A piece of scope earns work when

- a goal in the change's `intent.org` needs it, or
- there is a demonstrated reason: it happened (a failing run, a user report), a project prior asks for that kind of work (`overlay.md` § Priors), the user asked for it in their own words (a prior, an ask answer, an intent sentence; a `design.org` sentence is an agent's plan, not the user asking), or a measured cost.

Otherwise it is not built and not asked about. An agent that thinks it matters writes it as one `thought` line, and it stays that line. Cleanup is scope: a project prior that asks for it ("removing leftovers") makes a dead-code or duplication thought into work without an ask.

The check applies to scope, not to defects. A defect in code or text that exists is reported and routed by § Routing whether or not anyone has seen it fail. Its fix stays inside the defect: the change that makes the wrong case right. An option, a generalisation or a new behaviour proposed along with the fix is scope and takes the check.

## Routing

The orchestrator routes each `issue` and `question`, at the step the flow names (`flows/execute.md` § 5 and § 8, `flows/integrate.md` § 1 and § 7, `flows/plan.md` § 3). The reporter does not route. Routing writes the **decoration**, in Conventional Comments' sense: `blocking` (fixed in the change, before the MVP decision) or `hardening` (on the change's hardening list).

First, the two routes that do not depend on the main path:

- **`interface-drift`, code right and row stale** → a record correction: the Architect amends the seam row at the contract level (cutting implementation detail, never adding it; `templates/design.md` § Seams) and names it here; the orchestrator journals `record-correction: <what>` and commits. Not Implementor work, not an ask (`flows/integrate.md` § 1 says when the code counts as right).
- **Design in question, or a Reviewer `spec-signal`** (the row or the spec may be what is wrong, and only the user's choice settles it) → a `decision` ask file written to the inbox by the orchestrator from the finding; the finding says what the seam or scenario is for, its kind and evidence, and what it blocks. The finding is not the ask. An `issue` among them carries a stopgap (`roles/reviewer.md` § Routing of a finding).

Every other `issue` and `question` is routed by one question:

> Does this defect break a use that the intent's goals name, worked end to end, on the inputs and errors those goals name?

That set of uses is the change's **main path**: the MVP is the main path working end to end (`templates/hardening.md`). An input is on it when a goal's words cover it: "gets each tool's result back as well-formed text" covers every result, including one with an unusual character. An input is off it when the defect needs a circumstance the goals do not mention: a size beyond the ordinary, another version or platform, a remote host, a second call while the first still runs, a failure of something the change depends on. A non-goal is always off it.

| Finding | Decoration | What happens |
|---|---|---|
| An `issue` on the main path | `blocking` | Reproduced, then fixed in the change: the fix starts with a failing test or probe that shows the defect, then makes it pass (`flows/execute.md` § 8 says how a probe on a real surface is pinned). An inline fix, a re-spawned Implementor, or an in-change task. |
| A `question` on the main path | `blocking` | The reproduction its evidence names is tried first, as the first step of the fix. Reproduced: it is an `issue` and is fixed as above. Not reproduced: it goes on the hardening list as a `question`, with what was tried. |
| An untrue sentence in a shipped document | `blocking` | Fixed in the change, never put on the hardening list. In `design.org` or `docs/specs/` it is a record correction when a test pins the behaviour the corrected sentence states (`flows/integrate.md` § 1); a sentence whose promise the code does not keep is a defect in the code, routed by this table, never a correction. Anywhere else (a README, a help text, a comment, agent instructions) it is an inline fix or a task. Every spec edit made as a record correction is shown to the user at the MVP decision and in the closing summary (`templates/hardening.md`). |
| Any other `issue` or `question`: an edge case, an error path, an unusual input, a robustness gap, a defect in code outside the change | `hardening` | One entry on the change's hardening list (`templates/hardening.md`), shown to the user with the MVP and worked only if they choose to harden. |

When the goals do not settle whether a defect is on the main path, it goes on the hardening list with the doubt stated in one clause; the user sees it with the MVP.

The reason for the line: an MVP has to work, so a defect on its main path cannot wait; every other defect waits until the user has seen the MVP, which may show that the wrong thing is being built and the hardening would be wasted.

Where the routing is recorded: an Architect finding with `state.py record set findings <id> decoration=<blocking|hardening> resolution=<…>`; a Reviewer finding or an Implementor discovery, which have no state entry, by the inline-fix journal line, the follow-up task's `discovered_from`, or the hardening-list entry's `:REF:`. A finding that reaches no fix, task, ask or hardening-list entry has been dropped, which is a process bug.

## Resolution states

The state-file `architect_findings[].resolution` field tracks how each finding was handled:

- `pending` — not yet routed (blocks the integrate gate when its decoration is `blocking`)
- `inline-fixed` — the orchestrator applied an inline fix (to code, or to a document other than `design.org`: a README, a usage page, a spec scenario the amended row cites)
- `design-amended` — the code was right and the Architect amended the seam row (and the Decision or scenario steps that named the same thing); the orchestrator's `record-correction:` journal line and the commit carry the path
- `followup-task-<task-name>` — became a task; the field carries the task name
- `reverted` — the implicated merge was reverted
- `accepted-with-note` — the user accepted the defect as it is; carries the ask id whose `decision_readback` is the rationale, or the `priors.md` line that already answered it
- `hardening-list` — on the change's hardening list, awaiting the MVP decision; it becomes `followup-task-<task-name>` when the user chooses to harden and stays `hardening-list` when they stop

`noted` was retired on 2026-10-14 with the demonstrated-reason rule it served; findings that carry it keep it.
