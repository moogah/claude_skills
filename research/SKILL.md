---
name: research
description: "Run a research effort: turn an open-ended idea into agreed goals and non-goals with the user, one question at a time; spawn research agents on the knowledge domains and prior art; keep the evidence, the lines of investigation and the probes organised in org files; agree the domain vocabulary before anything is built; and hand off to the orchestrate skill's intent and specs, to a new project, or to a recorded no-go. Use when the user starts research (`/research`), asks to explore, research or pressure-test an idea before building it, opens a `research/` directory or a `docs/changes/<change>/research/` directory, or is about to build a new practice skill (each one starts as a research effort). Skip for a change whose goals the user has already stated in an intent; that goes to orchestrate's design round."
---

# research

The front end of research → plan → implement → verify. A research effort starts from the user's words, usually before they have a well-formed idea, and converges over many rounds into goals and non-goals that are observable, backed by evidence that they are achievable, with the known failure modes and a shared vocabulary. Plan, implement and verify belong to the `orchestrate` skill; research hands off to it.

The user's attention is the scarcest resource. Every rule below either spends it on a decision only the user can make, or saves it by keeping each context focused.

## Where an effort lives

Two scales; pick by what the user said, or ask once.

| Scale | Directory | When |
|---|---|---|
| Home | `<home>/research/<effort>/` | An idea not yet tied to one change; a new project; a bare workspace with no code yet. The usual case. |
| One change | `<code-root>/docs/changes/<change>/research/` | Research for a change that already has, or is about to have, an intent. Can later be referenced from or promoted to the home. |

`<home>` is the workspace directory the session runs in (for orchestrated projects, `state.py where` prints it). `<effort>` is a kebab-case name for the outcome.

```
<effort>/
  brief.org              the converging object: Why, goals, non-goals, Decided, open questions
  evidence/eN-<slug>.org one research agent's report each
  threads/<slug>/        one line of investigation each, worked in its own session
  probes/<slug>.org      one time-boxed experiment each
```

All files are org-mode. Start each with `#+TITLE:` and `#+OPTIONS: ^:{} -:nil`. Tell every spawned agent so; they default to markdown.

## Before the first question: read what is already known

1. The subject's org-roam index, if the effort's subject has one (`~/org/roam/<subject>/`): earlier prior-art surveys and the subject's terms. Do not survey again what is indexed there; cite it.
2. The product's glossary, if there is a code root: `<code-root>/docs/glossary.org`.
3. For a change-scale effort: the intent and the specs it names.

## The loop

Open the brief (`templates/brief.md`) with the user's words verbatim under `* Why` and commit it. Then repeat, in any order the conversation needs:

### Probe the user (goals and non-goals)

- **One question per message.** State the question in plain terms, give two to four options with the consequence the user would notice, and recommend one with the reason. Facts are the agent's job to find; ask the user only what only they can decide or know.
- **Depth rule.** Keep questioning a goal until (1) it is observable — the user would know it when they saw it working; (2) one more "why" adds nothing; and (3) the next answer would not change the brief. A "don't know" before then becomes an open question or a probe, not a stall.
- **Angles.** Work through these over the effort, not all at once: purpose (what the user does differently once it exists); who uses it; the observable outcome; what it must not become (non-goals); constraints; assumptions and where each came from; the domain's terms; what has been tried before.
- **Record each answer before asking the next.** A decision goes into `* Decided` with its date; a goal or non-goal is rewritten in place. Quote the user where their words carry the intent. The brief is rewritten, not appended to; `git log -p` on it is the record of how the goals moved. Commit at the end of each round.
- **When the user reverses a decision,** edit the goal, add a dated row saying what it replaces, and keep going. A goal that moves is the process working.

### Agree the vocabulary as it appears

Domain concepts first explained during execution are the failure this prevents. On the record, agents coined almost every term that later confused the user, in documents the user had approved.

- When you introduce a domain term, or the user uses one loosely, propose an entry: the term, one or two sentences on what it *is* (not what it does), an example from the user's own use, and the words to avoid. The user accepts, renames or rejects it.
- An explanation given in chat goes into the glossary in the same turn. Do not batch these.
- Accepted terms go straight into the product's `docs/glossary.org` (form: `~/.claude/skills/orchestrate/templates/glossary.md`). With no code root yet, they go in the brief's `* Terms` section and move to the glossary when one exists.
- A term moves up to the subject's org-roam note (`~/org/roam/<subject>/`) when a second project uses it: propose it, the user decides; the product glossary keeps a one-line link.

### Spawn research agents (evidence)

For knowledge domains, prior art (tools, processes, studies, skill collections), and the project's own history. Use `templates/evidence-agent.md` for the brief.

- Agents run in the background, in parallel when independent, each writing exactly one file `evidence/eN-<slug>.org` and replying with one line plus at most three sentences. Add a row to the brief's `* Evidence` table when spawning (`running`), and mark it `done` when it returns.
- Every bullet in an evidence file is marked `[O]` observed (read from a cited source, or run) or `[I]` inferred; the evaluative part is a separate, labelled section; unsettled questions go to `* Leads`, not chased.
- Scale the effort to the question: one agent for a fact, two to four for a comparison, more only for a broad survey.
- After each return, add what bears on the goals to the brief's `* What the evidence shows so far`, one descriptive line each, citing the file. Tell the user the consequential finding in a few sentences, then return to the open question.
- A prior-art survey for a subject is indexed in that subject's org-roam note (question, date, link) so the next effort reads it first.

### Probe (time-boxed experiments)

For a question a quick experiment can settle faster than argument. Use `templates/probe.md`.

- Write the question, the expected result and the time box **before** running it.
- One attempt. The result is `yes`, `no` or `inconclusive`. An inconclusive probe is not retried: it becomes a thread or an evidence question of its own.
- Probes are throwaway. Anything that writes to a real system runs in isolation (an isolated Emacs target with scratch data, a scratch directory, a worktree), never against the user's live data.

### Open a thread (a line of investigation)

When a sub-question needs its own sustained conversation with the user, or several lines run at once, give each its own thread so each context stays focused. Use `templates/thread.md`.

- A thread is a separate Claude session the user starts. Write `threads/<slug>/thread.org` with the question, why, the evidence to start from, sub-questions, bounds, and a `* Hand-back` section; add a row to the brief's `* Threads` table.
- Tell the user how to start it: Claude Code in the workspace root (so the session loads the workspace's memory), with "Work the research thread at `<path>/thread.org`."
- The thread session follows this skill's loop inside its bounds, records its decisions in its own file, and when the user calls it done writes them back to the brief (goal edits, Decided rows that link to the thread, open items marked critical or not) and commits.
- With Emacs available (below), offer a layout for the thread; add it only on the user's yes.

## When it is done

The user calls it, and **no critical item is open**. An item is critical if its answer could change a goal, a non-goal, or a spec scenario in the hand-off. Other open items travel with the hand-off, marked not critical.

Before the hand-off the brief must have:

- the goals and non-goals, each passing the depth rule;
- the new and changed terms, agreed with the user (for a change: as the intent's `* New terms`);
- the assumptions, each with its origin;
- the purpose facts only the user knows;
- every open question asked, none answered provisionally.

Set `#+STATUS: done <date>` with a paragraph naming the hand-off, and commit.

## Hand-off

| Outcome | What is written |
|---|---|
| One change | In the code root: `docs/changes/<change>/intent.org` per `~/.claude/skills/orchestrate/templates/intent.md`, with its `* New terms` and a `* Research` line linking `docs/changes/<change>/research/`; the spec edits per `templates/spec.md`; the glossary additions. The research directory keeps the change's assumptions with their origin and its failure modes; the design round reads them. Then orchestrate's design round takes over (it reads an intent the user's side wrote as it is; `flows/open.md` is not run). |
| Changes from a home effort | Zero or more changes opened as above, each intent's `* Why` linking back to the brief by path. |
| A new project | A workspace whose `research/` is the effort; its first changes follow later. |
| A new practice skill | The brief goes to the user and the agent writing the skill by hand. The skill names what it borrowed and from where (source and commit). |
| No-go | `#+STATUS: no-go <date>` and the reason in the brief. Nothing is queued for later. |

Purpose facts land in the `* Purpose` of the spec for the capability that owns the feature.

## Emacs, when available

If `~/emacs/bin/emacs-agent` exists, the user's Emacs is reachable:

- **Read freely** on the live session: `emacs-agent live call <tool> '<json>'` (`org_graph_query`, `read_roam_node`, `search_roam_nodes`, `query_roam_backlinks`, `org_graph_typed_edges`).
- **Write to the live session only with the user's yes**: `emacs-agent live call-write`. `workspace_add_layout` adds a layout for a thread without changing the screen: `{"directory": "<workspace root>", "layout_name": "line/<thread>", "source": "<effort>/<thread>", "description": "…", "split": "columns", "panes": [{"file": "<path>"}, {"split": "rows", "panes": [{"file": "…"}, {"file": "…"}]}]}`. Panes are files only (each must exist); the user opens a terminal for the Claude session in it (`C-c g`).
- **Typed edges** (`supports`, `contradicts`, `answers`, `derived-from`) are extracted only from notes under `~/org/roam/`, written as an `:EDGES:` drawer. Files in a workspace with an `:ID:` are found by title and tag only. Keep evidence as files in the effort; put only subject knowledge (terms, the survey index) in roam.
- **Write tests** run on an isolated target with a scratch data profile (`emacs-agent target start <name> --data scratch:<dir>`), confirmed by a read before the first write, and stopped afterwards. Never `live eval`.

## What research does not do

- It does not write a report with a roadmap. The brief converges; evidence files feed it.
- It does not design code. Seams, contracts and tasks are the design round's, after the hand-off.
- It does not answer the user's questions for them by a default rule; it asks, one at a time.

## Borrowed

| From | What | Source |
|---|---|---|
| spec-kit `/clarify` | One question at a time with a recommended answer; dated Q→A in the spec | github/spec-kit |
| Shape Up (shaping) | No-gos as a hand-off section; a no-go is routine and nothing is queued | basecamp.com/shapeup |
| XP spikes | Probes: one question, a time box, throwaway output | extremeprogramming.org (via secondary sources) |
| Anthropic multi-agent research system | Background research agents with a fixed brief; effort scaled to the question; artifacts passed by reference | anthropic.com/engineering/multi-agent-research-system |
| Co-STORM | Evidence tied to the question that found it; the user observes and occasionally steers | Stanford OVAL, Co-STORM |
| mattpocock/skills `domain-modeling` | Glossary entry form (what it is, words to avoid); write the term the moment it is settled; challenge loose terms | github.com/mattpocock/skills at `49dd158` |
| DDD ubiquitous language | Vocabulary scoped to a product or context, not one change | Evans, *Domain-Driven Design* |

The research that produced this skill, with every source cited: `~/emacs-workspaces/claude skills/research/research-phase/` (brief, evidence E1-E5, thread knowledge-base).
