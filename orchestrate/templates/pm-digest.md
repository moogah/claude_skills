# PM digest template

The PM digest is **scannable**, **action-oriented**, and **ends with the asks triage table** (which may be empty). Three buckets: *facts* (counts), *signals* (interpretation), *asks* (the questions the cycle raised, per `templates/ask.md`).

The structure is fixed; the contents are derived. Every count traces to a state-file field; counts never come from the LLM. Only the prose framing the signals and asks is agent-generated.

## Output location

`<change-dir>/cycles/<cycle-id>/pm-digest.md`

`state.py gate check integrate` computes `pm_digest_produced` from this file: it must exist with a non-empty `## Signals` section. A cycle with nothing to ask closes cleanly.

## Form

```markdown
# PM digest — change: <change-name> — <cycle-id>

**Produced**: <iso-ts>
**Phase**: integrate
**History window**: K=<value>

## Throughput (last K cycles)

```
                cycle-N-4  cycle-N-3  cycle-N-2  cycle-N-1  cycle-N
created            4          6          8          7         11
carried            0          1          2          4          3
completed          4          5          4          4          5
reviewed           3          4          4          3          4
rejected           0          1          0          0          1
inline fixes       1          0          2          3          2
externalised       1          2          3          2          5
drainage         1.00       0.83       0.50       0.57       0.45  ⚠ trending down
```

## Critical path

- on-path active   : 2 / 9 active tasks  ⚠ priority inversion
- on-path blocked  : 1 (blocked-on: ask-cycle-N-3)
- on-path complete : 4 / 11 total on-path

## Agents

Spend by role (this cycle's agents recorded when `counts` ran; this integrate's own phase agent and this PM, recorded later, appear under this cycle in the next digest's history)

```
role                   agents  setting(s)                 cost wt  share   per agent  priced wt  usd    E3 share  E3 per agent
phase-execute             1    sonnet/high                  1.42M  21.0%      1.42M      0.71M   3.10     17.9%       1.61M
implementor               5    opus/medium 4, sonnet/high 1 1.60M  23.7%      0.32M      1.44M   6.40     13.5%       0.35M
reviewer-task             5    opus/high                    1.50M  22.2%      0.30M      1.50M   6.20     12.6%       0.31M
...
not counted: 1 agent with no transcript (a1b2…); ran at another setting: none
```

Quality, by the Implementor's setting

```
setting          tasks  rejections  rework tasks  defects after done
opus/medium        4        1            1               0
sonnet/high        1        0            0               1   ⚠ t3-later, against t3
whole change       -        -            -               1
```

◦ **AGENT SETTINGS**: <one line: spend against the history and E3, read beside the quality signals; a setting the transcripts contradict>

## Signals

⚠ **THROUGHPUT**: drainage <1.0 for 4 cycles. Queue is growing.

⚠ **CASCADE**: task `T-042 (operation-section mapping)` has 6 follow-ups discovered across 3 implementors.
  → recommend a conformance run scoped to the cluster (the dispatch boundary's seam rows).

⚠ **INVERSION**: 7 of 9 active tasks are off-critical-path refactors. Critical-path tasks T-019 and T-031 unstarted for 3 cycles.

✓ **REVIEW-BALANCE**: needs-review = 2, healthy.

◦ **EXTERNALISATION**: .tasks/ +5 this cycle (vocabulary-mapping cluster). Trend to watch.

## Asks for the user

| # | Question | Blocks | Recommendation | File |
|---|---|---|---|---|
| 1 | Should a video you fetch by hand be added to the archive? | — | Do nothing to the code | `<change-dir>/inbox/ask-cycle-N-9.org` |
| 2 | Keep working through the refactor batch, or return to the critical path first? | next-plan | Return to the critical path | `<change-dir>/inbox/ask-cycle-N-3.org` |

Answer in the file's `* Answer` section (paths are relative to the home), or in chat.
Applied without asking: 3 (`cycles/<cycle-id>/asks/`). Process notes: 1 (inbox).
Findings: 3 issues, 1 question; on the hardening list: 2 (`.orchestrator/changes/<change>/hardening.org`).

## Trends to watch

- Externalisation cluster around vocabulary-mapping suggests a future change may be needed.
- Implementor rejection rate on contract-test tasks: 3 of last 4.

## Meta-discoveries (this cycle)

- The bash-parser/scope boundary has no seam row stating its vocabulary mapping; the next design round in that area should state the mapping before tasks are drafted.

## Goal-drift check

- Critical-path completion ratio: 4/11 = 0.36, stagnant for K=5 cycles while non-critical-path completions continue at ~5/cycle.
- **Recommendation**: <none | revise | split | abandon> — <one-line reason>
```

## Agents: reading spend against E3

The section shows whether the agent settings (`SKILL.md` § Agent settings) cut spend where the work is routine without costing quality (research `agent-model-effort`, R2, at `~/emacs-workspaces/claude-skills/research/agent-model-effort/brief.org`). The baseline is E3 (`evidence/e3-orchestrate-spend.org` there): the two sessions of 2026-10-22/23, 177 agents, every one inheriting the dispatcher's Opus 5.5 (some Fable 5.1) at `medium` or `high`. Its columns, in E3's cost weight (input + 0.1 × cache read + 1.25 × cache write + 5 × output, model-blind):

| role | E3 agents | E3 share | E3 per agent |
|---|---|---|---|
| phase-execute | 12 | 17.9% | 1.61M |
| implementor | 42 | 13.5% | 0.35M |
| reviewer-task | 44 | 12.6% | 0.31M |
| phase-plan | 14 | 9.2% | 0.71M |
| phase-integrate | 8 | 9.0% | 1.21M |
| architect-design | 6 | 7.8% | 1.40M |
| dispatcher (not an agent; not recorded) | 2 | 7.1% | 3.83M |
| reviewer-whole-change | 8 | 6.9% | 0.93M |
| architect-conformance | 8 | 4.7% | 0.63M |
| architect-premise | 7 | 4.5% | 0.70M |
| phase-design | 6 | 2.4% | 0.43M |
| pm | 8 | 1.9% | 0.25M |
| implementor-rework | 5 | 1.6% | 0.35M |
| phase-open | 5 | 0.7% | 0.15M |

- **Compare per agent, and in priced weight.** Shares move whenever one role's spend does, and the cost weight ignores the model, so a role moved from Opus to Sonnet shows its saving in `priced_weight` (the weight scaled by the model's price against Opus's), not in `cost_weight`. E3's runs were nearly all Opus, so its cost weight stands for its priced weight; its Fable requests make that an understatement of E3's spend. `usd` is list price; E3 has no dollar baseline.
- **Quality first.** Spend is read beside the quality table: a role or a setting whose spend fell while its rejections, rework tasks or defects found after done rose is the line to write. E3 records none of the three, so their baseline is the history window from the first cycle recorded.
- **What the PM writes**: one `◦ AGENT SETTINGS` line, or `⚠` when a quality count rose where spend fell, or when `setting_mismatches` lists an agent (an environment variable outranked the call, so the record does not say what ran). The PM proposes no change to a setting; a setting is changed in the skill or by a task override, by people reading this.

## Signal symbols

- ⚠ — actionable signal; warrants a disposition: an ask file, a conformance run, a task, or a listed default
- ✓ — healthy on this dimension; included so the absence of a ✓ on a tracked dimension reads as missing data
- ◦ — informational; trend to watch but not yet actionable

PM agent prose is generated only for signal lines and the asks' question column. The throughput table, the critical-path readout and the asks table's other columns are rendered from state-file fields.

## Determinism boundary

The deterministic pass is `state.py counts --write` (`roles/project-manager.md`). It produces:

- All counts in the throughput table, this cycle and the history window.
- The critical-path readout and the class table.
- The findings by kind and how many are on the hardening list (the one line under the asks table).
- The list of fired signals it can compute (throughput inversion, review starvation, priority inversion, cascade).
- The `## Agents` tables: spend by role from the recorded agents' transcripts, and the quality signals by the Implementor's setting (`state.md` § Agent entry), with the history window in `agents.history`.
- The open asks (with their paths) and the blocked tasks with their `blocker_note`, from which the agent pass proposes candidate questions; the orchestrator writes each ask file (`templates/ask.md`). A subject that already carries an open ask id in `blocker_note` re-surfaces that id. Cascade detection produces an Architect conformance run scoped to the cluster, not an ask; the PM may spawn it alone.

The agent pass produces:

- The framing prose around each signal (e.g. "this cluster looks like a vocabulary-mapping gap").
- The candidate questions (the asks table's question column), from which the orchestrator writes the ask files; the PM writes none (`templates/ask.md`, opening).
- The goal-drift recommendation (revise / split / abandon / continue) and its one-line reason.

If the agent pass fails or hallucinates, the deterministic pass output remains in `<change-dir>/cycles/<cycle-id>/pm-signals.json` and is recoverable.

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
| `phase_agent_context` | 400000 tokens; read by the dispatcher (`state.py context`), not the PM |

Per-project overlay overrides via `<home>/.claude/orchestrator/config.yaml` `thresholds.*`.
