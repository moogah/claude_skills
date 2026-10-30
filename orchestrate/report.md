# Spend and quality report

`scripts/report.py` is the on-demand report on what the agents cost and how well their work held: run by the user, or by `/process-improvement` when it reads the cycles a home has run. Nothing runs it each cycle, no gate needs it, and its numbers go in no handshake (`docs/specs/state-layer.org` § Integration Points). It writes nothing.

```
python3 <skill-dir>/scripts/report.py --change <change> spend [--cycle <cycle-id>]
python3 <skill-dir>/scripts/report.py context <agent-id>
```

## What it reads

- **The agents.** Every subagent transcript under the home's project directories (`$CLAUDE_CONFIG_DIR` or `~/.claude`, `projects/<the home's path, or a path under it>/*/subagents/agent-<id>.jsonl`). An agent counts for the change when the spawn description in its `agent-<id>.meta.json` reads `<role>: <task-name | cycle-id | change>` (`SKILL.md` § Agent settings), naming a task, a cycle, or the change itself for work outside any cycle (the design round). The role is that word; the setting is what the transcript ran at, request by request. A spawn whose description names no role counts in `unattributed_spawns`. A task whose file carries an `agent:` override (`templates/task-body.md`) and whose agent ran at another setting is listed in `override_mismatches` (an environment variable such as `CLAUDE_CODE_EFFORT_LEVEL` outranks the call).
- **The quality, from the record.** Reviewer rejections are the `<task>-rework` briefs; rework tasks are the tasks with one, or with an `implementor-rework` agent; defects found after done are a conformance finding of kind `issue`, a task whose `discovered_from` begins `whole-change`, and a follow-up of a merged task that neither its Implementor nor its Reviewer raised. All three again by the setting each task's Implementor ran at (`unrecorded` without one; `whole change` for a defect no one task owns).

`spend` prints `by_role` (agents, settings ran, task overrides, requests, tokens, `cost_weight`, `priced_weight`, `usd`, share, per agent, median minutes), `by_agent`, `quality`, and without `--cycle` the same per cycle in `by_cycle`. Each API request counts once (by `requestId`); a request logged on several lines counts each usage field at its largest; a `<synthetic>` line (an API error) is no request.

`context` is the dispatcher's reading of a running phase agent (`SKILL.md` § The dispatcher's loop): the tokens of the last call in its transcript against the overlay's `thresholds.phase-agent-context` (default 400000), with `over`.

## Reading spend against E3

The report shows whether the agent settings (`SKILL.md` § Agent settings) cut spend where the work is routine without costing quality (research `agent-model-effort`, R2, at `~/emacs-workspaces/claude-skills/research/agent-model-effort/brief.org`). The baseline is E3 (`evidence/e3-orchestrate-spend.org` there): the two sessions of 2026-10-22/23, 177 agents, every one inheriting the dispatcher's Opus 5.5 (some Fable 5.1) at `medium` or `high`. Its columns, in E3's cost weight (input + 0.1 × cache read + 1.25 × cache write + 5 × output, model-blind):

| role | E3 agents | E3 share | E3 per agent |
|---|---|---|---|
| phase-execute | 12 | 17.9% | 1.61M |
| implementor | 42 | 13.5% | 0.35M |
| reviewer-task | 44 | 12.6% | 0.31M |
| phase-plan | 14 | 9.2% | 0.71M |
| phase-integrate | 8 | 9.0% | 1.21M |
| architect-design | 6 | 7.8% | 1.40M |
| dispatcher (not an agent; not counted) | 2 | 7.1% | 3.83M |
| reviewer-whole-change | 8 | 6.9% | 0.93M |
| architect-conformance | 8 | 4.7% | 0.63M |
| architect-premise | 7 | 4.5% | 0.70M |
| phase-design | 6 | 2.4% | 0.43M |
| pm | 8 | 1.9% | 0.25M |
| implementor-rework | 5 | 1.6% | 0.35M |
| phase-open | 5 | 0.7% | 0.15M |

- **Compare per agent, and in priced weight.** Shares move whenever one role's spend does, and the cost weight ignores the model, so a role moved from Opus to Sonnet shows its saving in `priced_weight` (the weight scaled by the model's price against Opus's), not in `cost_weight`. E3's runs were nearly all Opus, so its cost weight stands for its priced weight; its Fable requests make that an understatement of E3's spend. `usd` is list price; E3 has no dollar baseline.
- **Quality first.** Spend is read beside the quality counts: a role or a setting whose spend fell while its rejections, rework tasks or defects found after done rose is the finding. E3 records none of the three, so their baseline is the earliest cycle the report reads.
- **Spawns before the description form** read as `unattributed_spawns`, not under a role; a cycle run before it is not comparable by role.
- The report proposes no change to a setting; a setting is changed in the skill or by a task override, by people reading it.
