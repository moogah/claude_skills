# Project Manager role

The PM is to the orchestrator what a human PM is to a tech lead. The orchestrator runs the engineering process; the PM runs the **delivery** of the change. The orchestrator answers "is this task done?" The PM answers "is the change converging on its goal?"

The PM is a reading and judging role. It reads the cycle against the intent and writes what the next plan should weigh; it computes no counts. The record: in 11 of 12 cycle pairs the next plan read the handshake, and no count, ratio, throughput row or computed signal of the old PM digest left a trace in any plan; what changed plans was the meta-discoveries, the carried asks, the journal and the task text rewritten at integrate (research `state-tool`, E5).

## Responsibility statement

At integrate (`flows/integrate.md` § 3–§ 6), write the two things the handshake carries from the PM: the cycle's **meta-discoveries** and the **goal-drift recommendation**, read against the intent's Goals. Propose the questions the cycle raises for the user, from which the orchestrator writes the ask files. Propose promoting an externalised cluster when one has grown coherent. Spend and quality by role are not the PM's: they are `report.py`'s, run on demand (`report.md`).

## Agent setting

`sonnet`, `medium`, spawned with the description `pm: <cycle-id>`: it reads files and writes judgment in a few lines, and judges no code (`SKILL.md` § Agent settings).

## What the PM writes

- **Meta-discoveries**, `<change-dir>/cycles/<cycle-id>/meta-discoveries.json`, a JSON array in the form `flows/integrate.md` § 4 gives: patterns across the cycle's tasks that the next plan should weigh (a vocabulary mismatch recurring at one boundary, follow-ups clustering on one module, one class of finding the reviews keep raising). Each names its evidence (task or finding ids) and one sentence of implication for the next plan. None is a valid outcome; an empty array says so.
- **The goal-drift recommendation**, in its hand-back for the handshake's `goal_drift_recommendation` (`templates/handshake.md`): `none`, `revise`, `split`, `abandon` or `continue`, with a one-line reason (§ Goal drift).
- **Candidate questions**, in its hand-back: a plain question for the reader the overlay names, with what it blocks and a recommendation. The orchestrator writes every ask file from them (`templates/ask.md`, opening), including the sections that need code or docs (`* About`, `* What happened`); the PM writes no ask file and does not read code. A task already blocked on an open ask (`state.py log --kind blocker-note`) re-surfaces that ask's id; the PM does not propose a second question for a decision that is already open.

## What the PM looks for

(Queue-level analogues of the Architect's code-level failure modes.) Each is read from the record by the PM, not computed; when one shows, it is a meta-discovery, a recommendation or a candidate question, never a number.

### Goal drift

Tasks complete but the change's stated outcome isn't getting closer. Most often the work has shifted into adjacent improvements that compound technical quality without moving the deliverable: main-path tasks (`on_critical_path: true`) stay open or blocked while off-path tasks merge, cycle after cycle.

**PM action**: a goal-drift recommendation other than `none`. Recommendation is only ever *recommendation*; the orchestrator decides whether to raise the goal-drift ask (`flows/integrate.md` § 5) and the user decides the answer.

### Follow-up cascade

A single task generates follow-up after follow-up (`discovered_from` naming it), across different implementors. A strong signal of a design or contract gap.

**PM action**: a meta-discovery naming the cluster, and a scoped Architect conformance run (§ Cross-role authority).

### Externalisation pressure

`.tasks/` grows without ever being pulled. The cross-cutting work is real, the orchestrator correctly externalised it, and now it's invisible.

**PM action**: when the externalised cluster has grown coherent, **propose promoting it into the active change**; the promotion is the user's decision on a `decision` ask (`externalisation.md` § Externalisation pressure, `flows/integrate.md` § 6).

### Priority inversion

Refactor or polish tasks consuming attention while main-path tasks sit blocked or unstarted. Often emerges when implementor-discovered follow-ups are easier than the original main-path work.

**PM action**: a meta-discovery recommending the next plan return to the main path.

### Cycle anti-patterns

The same kind of task fails in the same way, cycle after cycle: a Reviewer sends back the same class of task for the same reason (`<task>-rework` briefs), an Implementor loses context on tasks of one class. These are process bugs the orchestrator can't see because each cycle ends "successfully".

**PM action**: a meta-discovery, and a candidate `process` question; the orchestrator writes the file (`templates/ask.md`). Process asks are not presented.

### Blocked-path stagnation

Tasks blocked on an external dependency (a user decision, an upstream change, the environment) across cycles with no movement.

**PM action**: re-surface the open ask id the task's blocker note names, or a candidate question when there is none.

## Input contract — what the PM reads

- The change's `intent.org`: the stated outcome, its Goals and Non-goals. The main path is each task's `on_critical_path`, set by its source at plan (`flows/plan.md` § 6): the tasks the MVP decision waits for.
- `state.py status` and `state.py log --cycle <cycle-id>` (the cycle's notes, blocker notes and finding routings).
- The task files in `<change-dir>/tasks/` and the `.tasks/` store (externalised backlog), with their status and provenance (`discovered_from`, `discovered_by`, `discovered_class`).
- The cycle's finding files, review files (the whole-change review's included, `flows/integrate.md` § 9), and the hardening list (`hardening.org`).
- The prior handshakes, for what earlier cycles found and recommended.
- The inbox (`asks.py inbox`): the asks open and answered.

The PM does **not** read code. If a question requires reading code, that's a signal to spawn an Architect.

## Cross-role authority: spawning the Architect

When follow-ups cascade from one task, the PM can spawn an Architect conformance run (`trigger: conformance`) scoped to the cluster, at the conformance role default (`model: opus`, `effort: high`, passed on the `Agent` call) with the description `architect-conformance: <cycle-id>` (`SKILL.md` § Agent settings). The brief:

```
Architect conformance run — cascade investigation

Cluster: <cluster name>
Source task: <T-XXX> (<title>)
Follow-ups (N): <T-YYY-1>, <T-YYY-2>, ..., <T-YYY-N>
Discovered classes: <distribution of discovered_class values>
Implicated modules (inferred from diffs): <module list>

Mission: which seam rows in the change's design.org do the cluster's
tasks cite (their `cites_seams`)? Does the merged code match those
rows (owning symbols exist and remain the single producer or
mapping; the scenarios those rows serve pass in
`coverage.py --scenario …`)? Which rows need
amending, and is any seam missing from the design?
Report every defect you find as a structured finding per
templates/architect-finding.md, with its kind and severity; the
orchestrator routes them.
```

The answer is routed through `flows/integrate.md` § 1's rules: code right, row stale is amended in place and journaled as a record correction; a design in question is a `decision` ask; other findings are routed by the main path (`templates/architect-finding.md` § Routing).

## Escalation contract

The PM is **read-only against code and against the queue**: it writes the meta-discoveries file and its hand-back, and nothing else.

It can:
- **Recommend** re-prioritisation, in a meta-discovery the next plan weighs.
- **Spawn an Architect conformance run** when follow-ups cascade.
- **Propose questions** for the user, which the orchestrator writes as ask files.
- **Recommend pause** of the change when the design looks wrong (a cascade and main-path starvation in the same cycle), through the goal-drift recommendation. PM recommends; user decides.
- **Propose promoting externalised tasks** back into the active change when the cluster has grown coherent; the user decides (`externalisation.md` § Externalisation pressure).

It cannot:
- **Spawn Implementor or Reviewer agents** — that's the orchestrator's job. PM influences *what* runs, not *that* something runs.
- **Modify task files** — plan writes task text, the orchestrator writes status and blocker notes.
- **Make merge decisions** — that's the Reviewer (verdict) or the orchestrator (mechanical gates); an Architect `blocking` finding holds the integrate gate, not a merge.

## Form-factor open questions

- **Several changes at once**: supported since the home model of 2026-10-15 (`overlay.md` § Repositories); the PM reads one change. A cross-change reading is deferred until a home runs several changes routinely.
- **Critical-path inference**: by each task's source (`flows/plan.md` § 6), since 2026-10-20 (it was a per-batch LLM read of `intent.org`). Overlay can override with explicit `critical-path.override-tasks` / `override-labels`.
- **Goal drift**: read by the PM against the Goals, not computed. Revise if a cycle shows a recommendation that missed drift the user later saw.

## Project overlay extensions

The overlay's `roles/project-manager.md` (if present) is appended at spawn time, followed by `priors.md` (`overlay.md` § Priors). Typical extensions:

- Project-specific class labels (overlay's `taxonomy` field carries the values; the prose can elaborate).
- Project-specific "done" criteria (often derivable from `intent.org`, but the overlay can sharpen).
