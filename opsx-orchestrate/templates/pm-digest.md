# PM digest template

The PM digest is **scannable**, **action-oriented**, and **ends with the asks triage table** (which may be empty). Three buckets: *facts* (counts), *signals* (interpretation), *asks* (the questions the cycle raised, per `templates/ask.md`).

The structure is fixed; the contents are derived. Every count traces to a state-file field; counts never come from the LLM. Only the prose framing the signals and asks is agent-generated.

## Output location

`<repo>/.orchestrator/cycles/<cycle-id>/pm-digest.md`

The state file's `phase_gates.integrate.checks.pm_digest_produced` flips to `true` when this file exists with a non-empty `signals` section. A cycle with nothing to ask closes cleanly.

## Form

```markdown
# PM digest — change: <change-name> — cycle <n>

**Produced**: <iso-ts>
**Phase**: integrate
**History window**: K=<value>

## Throughput (last K cycles)

```
                cycle-N-4  cycle-N-3  cycle-N-2  cycle-N-1  cycle-N
created            4          6          8          7         11
completed          4          5          4          4          5
reviewed           3          4          4          3          4
externalised       1          2          3          2          5
drainage         1.00       0.83       0.50       0.57       0.45  ⚠ trending down
```

## Critical path

- on-path active   : 2 / 9 active tasks  ⚠ priority inversion
- on-path blocked  : 1 (blocked-on: ask-cycle-N-3)
- on-path complete : 4 / 11 total on-path

## Signals

⚠ **THROUGHPUT**: drainage <1.0 for 4 cycles. Queue is growing.

⚠ **CASCADE**: task `T-042 (operation-section mapping)` has 6 follow-ups discovered across 3 implementors.
  → recommend Architect audit on the dispatch boundary.

⚠ **INVERSION**: 7 of 9 active tasks are off-critical-path refactors. Critical-path tasks T-019 and T-031 unstarted for 3 cycles.

✓ **REVIEW-BALANCE**: needs-review = 2, healthy.

◦ **EXTERNALISATION**: .tasks/ +5 this cycle (vocabulary-mapping cluster). Trend to watch.

## Asks for the user

| # | Question | Blocks | Recommendation |
|---|---|---|---|
| 1 | Should a video you fetch by hand be added to the archive? | — | Do nothing to the code |
| 2 | Keep working through the refactor batch, or return to the critical path first? | next-plan | Return to the critical path |

Full asks, options and defaults: `.orchestrator/cycles/<cycle-id>/asks.md`.
Applied without asking: 3 (listed there). Process notes: 1.

## Trends to watch

- Externalisation cluster around vocabulary-mapping suggests a future change may be needed.
- Implementor rejection rate on contract-test tasks: 3 of last 4.

## Meta-discoveries (this cycle)

- Vocabulary unknowns at the bash-parser/scope boundary are systematically under-speculated; future forward speculation in that area should probe vocabulary first.

## Goal-drift check

- Critical-path completion ratio: 4/11 = 0.36, stagnant for K=5 cycles while non-critical-path completions continue at ~5/cycle.
- **Recommendation**: <none | revise | split | abandon> — <one-line reason>
```

## Signal symbols

- ⚠ — actionable signal; warrants a disposition: an ask record, an Architect audit, a task, or a listed default
- ✓ — healthy on this dimension; included so the absence of a ✓ on a tracked dimension reads as missing data
- ◦ — informational; trend to watch but not yet actionable

PM agent prose is generated only for signal lines and the asks' question column. The throughput table, the critical-path readout and the asks table's other columns are rendered from state-file fields.

## Determinism boundary

The deterministic pass produces:

- All counts in the throughput table.
- The critical-path readout.
- The list of fired signals (from threshold queries against state).
- Candidate-ask stubs (from blocked-task aging and stale detection): id, kind, `raised_by: pm`, skeleton question, `blocks`. A subject that already carries an open ask id in `blocker_note` re-surfaces that id. Cascade detection produces an Architect audit, not an ask; the PM may spawn it alone.

The agent pass produces:

- The framing prose around each signal (e.g. "this cluster looks like a vocabulary-mapping gap").
- The asks' prose per `templates/ask.md` (a plain question and, when the orchestrator has supplied `about` and `observed`, the options and consequences). Fields that need code or docs are the orchestrator's, not the PM's.
- The goal-drift recommendation (revise / split / abandon / continue) and its one-line reason.

If the agent pass fails or hallucinates, the deterministic pass output remains in `.orchestrator/cycles/<cycle-id>/pm-signals.json` and is recoverable.

## Threshold defaults (overlay-configurable)

| Threshold | Default |
|---|---|
| `history_window` (K) | 5 |
| `drainage_trigger_ratio` | 1.0 |
| `drainage_trigger_consecutive_cycles` | 3 |
| `cascade_trigger_followup_count` | 3 |
| `stale_task_cycles` | 3 |
| `review_starvation_ratio` | (needs_review/in_progress) > 1.5 |
| `externalisation_pressure_growth` | monotonic over 3 cycles |

Per-project overlay overrides via `.claude/orchestrator/config.yaml` `thresholds.*`.
