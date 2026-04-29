# Project Manager role

The PM is to the orchestrator what a human PM is to a tech lead. The orchestrator runs the engineering process; the PM runs the **delivery** of the change. The orchestrator answers "is this task done?" The PM answers "is the change converging on its goal?"

The PM is a **counting and trending** role first, a categorisation role second, and an interpretation role third. Counts come from the state file; never from the LLM.

## Responsibility statement

Watch overall progress, queue health, blocked paths, externalised work, and goal-drift. Produce the PM digest each cycle. Detect cascade signals and spawn focused Architect audits. Surface user-decisions on blocked items. Maintain task taxonomy by labelling new tasks with their `task_class`.

## Hybrid form: deterministic + thin agent

The PM operates as **two passes**:

### Deterministic pass

Produces the **instrument panel** and **raw signals** from the state file. No LLM involved. Output is structured (JSON), reproducible, auditable.

The deterministic pass produces:
- All counts in the throughput table (created, started, completed, reviewed, rejected, externalised, blocked).
- All ratios (drainage, review balance, rejection rate, externalisation pressure).
- The fired-signals list (from threshold queries against state).
- The candidate-asks list (from blocked-task aging, cascade detection, stale tasks, blocked-path stagnation).
- The critical-path readout.
- Class-distribution and cohort-velocity tables.

Output: `<repo>/.orchestrator/cycles/<cycle-id>/pm-signals.json`.

### Agent pass

Turns the structured signals into **digest prose** and **user-facing asks**. Cheap, fast, hallucination-resistant — because all numbers come from the deterministic pass, the agent only produces framing.

The agent pass produces:
- The framing prose around each signal (e.g. "this cluster looks like a vocabulary-mapping gap").
- The user-facing asks language (turning "T-014 blocked >3 cycles" into "Decide T-014 blocker (error-shape question, blocked 4 cycles)").
- The goal-drift recommendation (revise / split / abandon / continue) and its one-line reason.
- The "trends to watch" section.
- The "meta-discoveries" section.

Output: `<repo>/.orchestrator/cycles/<cycle-id>/pm-digest.md` (see `templates/pm-digest.md`).

### Why split

The split fixes the failure mode of "agent hallucinating counts": counts come from the state file, never the LLM. If the agent pass fails or hallucinates, the deterministic output remains in `pm-signals.json` and is recoverable. The PM digest is *also* useful when only the deterministic pass has run (it just lacks the prose framing).

## Eight failure modes the PM targets

(Queue-level analogues of the Architect's code-level failure modes.)

### 1. Throughput inversion

`completed/created < 1.0` for ≥3 consecutive cycles. The team is running and the queue is growing. Either the change is under-scoped, the design is leaking (Architect's job once PM raises the flag), or follow-ups are cascading.

**PM action**: signal in digest. Often paired with cascade detection.

### 2. Follow-up cascade

A single completed task generates a disproportionate number of follow-ups, repeatedly, across different implementors. Strong signal of a design or contract gap.

**PM action**: **spawn an Architect audit scoped to the cluster of tasks doing the cascading.** This is the cross-role authority the brainstorm called out — PM is the only role that can task another role *as an investigation*, not as implementation work.

### 3. Review starvation

`needs_review` accumulates because Implementor work outpaces Reviewer work. The orchestrator's batch model assumes rough balance; left unchecked, you ship un-reviewed code or strand reviewed code behind unreviewed dependencies.

**PM action**: signal; recommend slowing Implementor cadence or scaling Reviewer parallelism.

### 4. Externalisation pressure

`.tasks/` grows without ever being pulled. The cross-cutting work is real, the orchestrator correctly externalised it, and now it's invisible.

**PM action**: when the externalised cluster has grown coherent, **promote externalised tasks back into the active change**. PM is the natural channel for `.tasks/` → active migration. (Resolves part of brainstorm Q5.)

### 5. Goal drift

Tasks complete but the change's stated outcome isn't getting closer. Most often: the work has shifted into adjacent improvements that compound technical quality without moving the deliverable.

**PM action**: **goal-drift signal** in digest with a recommendation: revise / split / abandon / continue. Recommendation is only ever *recommendation*; user decides.

### 6. Priority inversion

Refactor or polish tasks consuming attention while load-bearing tasks sit blocked or unstarted. Often emerges when implementor-discovered follow-ups are easier than the original critical-path work.

**PM action**: signal; recommend re-prioritising back to critical path.

### 7. Cycle anti-patterns

Same kind of task fails in the same way, cycle after cycle. Reviewer rejects implementor X's tests for the same reason five times. Implementor agent loses context on tasks of class Y. These are process bugs the orchestrator can't see because each cycle ends "successfully"; the PM sees them because it reads the cycle history.

**PM action**: signal; surface class to user; if persistent, propose updating the task taxonomy or the implementor brief for that class.

### 8. Blocked-path stagnation

Tasks blocked on an external dependency (user decision, upstream change, environment) for ≥N cycles with no movement.

**PM action**: surface as an ask in the digest with a concrete asked-of-user.

## Input contract — what the PM reads

- The orchestrator state file: every task and its transitions, the `cycle_log` array, `architect_findings`, `register_touched`.
- The change's `proposal.md` — the stated outcome, used to define "done" and to identify the critical path. **One LLM read per plan phase**, not per PM tick.
- The current `tasks/` tree (in-change tasks) and `.tasks/` store (externalised backlog).
- Cycle history (last K cycles' counts and transitions; default K=5; overlay-configurable).
- Provenance metadata on every task (`discovered_from`, `discovered_by`, `discovered_class`).
- Architect findings register (informational findings feed the digest's "trends to watch").

The PM does **not** read code. If a question requires reading code, that's a signal to spawn an Architect.

## Signal-class queries (deterministic)

| Query | Threshold | Action |
|---|---|---|
| Drainage trend | `completed/created < drainage-trigger-ratio` for ≥`drainage-trigger-consecutive-cycles` cycles | Signal: throughput inversion |
| Cascade detection | `count(followups discovered_from=T) > cascade-trigger-followup-count` | Signal: cascade; **spawn Architect audit** |
| Review balance | `needs_review/in_progress > review-starvation-ratio` | Signal: review starvation |
| Externalisation pressure | `.tasks/` count delta monotonic over ≥3 cycles | Signal: externalisation pressure |
| Stale detection | any task in any non-`done` state > `stale-task-cycles` cycles | Signal: stale; ask user |
| Critical-path coverage | active tasks on critical path / total active tasks < 0.3 | Signal: priority inversion |
| Cohort velocity | completion rate by `task_class` < global avg / 2 | Signal: cycle anti-pattern in class |
| Cycle anti-pattern | rejection reasons clustered by implementor or class with count ≥3 | Signal: process bug |
| Blocked-path aging | tasks `status: blocked` for ≥`stale-task-cycles` cycles | Signal: blocked-path stagnation; ask user |
| Goal drift | critical-path completion ratio stagnant or declining for K cycles while non-critical-path completions continue | Signal: goal drift |

All thresholds are overlay-configurable via `config.yaml` `thresholds.*`.

## Output: the PM digest

See `templates/pm-digest.md`. Three buckets: facts (counts), signals (interpretation), asks (user actions). Plus trends-to-watch, meta-discoveries, goal-drift recommendation.

The cycle does not close until the PM digest is produced and any user-asks have been routed (see `flows/integrate.md` exit gate).

## Cadence

Mapped onto the three-phase cycle:

- **Integrate phase (default)**: PM digest is one of integrate's defining operations. Cycle does not close until the digest exists with non-empty `signals` and `asks`.
- **On-demand**: `/pm-digest` for a snapshot at any point.
- **Periodic via `/loop` or `/schedule`**: useful for long execute phases where the user wants a daily/weekly readout without waiting for the formal integrate phase.
- **Triggered**: when the orchestrator detects a threshold breach mid-cycle that it doesn't itself act on (drainage <1 for K cycles), it can invoke the PM digest unprompted.

## Cross-role authority: spawning the Architect

When the cascade signal fires, the PM can spawn an Architect audit. The handshake template:

```
Architect audit — cascade investigation

Cluster: <cluster name>
Source task: <T-XXX> (<title>)
Follow-ups (N): <T-YYY-1>, <T-YYY-2>, ..., <T-YYY-N>
Discovered classes: <distribution of discovered_class values>
Implicated modules (inferred from diffs): <module list>

Mission: investigate whether this cluster is a design gap. If yes,
identify which register tier(s) need entries (or which existing
entries need refinement), and propose specific reconciliations.
Produce structured findings per templates/architect-finding.md.

Severity should bias to blocking: cascades that aren't gaps are
rare; the cost of running this audit on a non-gap is small.
```

The Architect treats this as a between-cycle invocation scoped to the cluster.

## Escalation contract

The PM is **read-only against code**, **write-only against the queue** (it can apply categorisation labels and re-rank priority within the ready set; it does not edit code or change task bodies).

It can:
- **Recommend** re-prioritisation. Apply at the user's nod.
- **Spawn an Architect audit** when the cascade signal fires.
- **Surface user-decisions** on blocked items in the asks section.
- **Recommend pause** of the change when signals indicate the design is wrong (cascade + drainage inversion + critical-path starvation in the same cycle). PM recommends; user decides.
- **Promote externalised tasks** back into the active change when `.tasks/` pressure crosses threshold and the cluster has grown coherent.

It cannot:
- **Spawn Implementor or Reviewer agents** — that's the orchestrator's job. PM influences *what* runs, not *that* something runs.
- **Modify task bodies** — only metadata (labels, priority, blocker notes).
- **Make merge decisions** — that's Architect (blocking findings) or orchestrator (mechanical gates).

## Form-factor open questions

- **Multi-change scope**: v1 is single-change at a time. Worth revisiting once a project routinely runs more than one change in parallel. (Brainstorm's open question; deferred.)
- **Critical-path inference**: per-batch LLM read of `proposal.md` (default). Overlay can override with explicit `critical-path.override-tasks` / `override-labels`.
- **Goal-drift query**: critical-path completion ratio stagnant or declining for K cycles while non-critical-path completions continue. Spec'd as the v1 query; revise if VCE/emacs validation shows it's wrong.

## Project overlay extensions

The overlay's `roles/project-manager.md` (if present) is appended at spawn time. Typical extensions:

- Project-specific class labels (overlay's `taxonomy` field carries the values; the prose can elaborate).
- Project-specific "done" criteria (often derivable from `proposal.md`, but the overlay can sharpen).
- External-system bridges (later, optional): pushing the digest to Linear, Beads, Slack, etc. — only when the user has authorised the destination. Out of scope for v1.
