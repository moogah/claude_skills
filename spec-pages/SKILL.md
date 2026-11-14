---
name: spec-pages
description: "Build a spec page: one self-contained HTML page per capability spec that shows what the system does, scenario by scenario, and its proof, every part of it computed from the spec, the tests that cite its scenarios and their recorded runs. Each scenario shows its When/Then, How it was checked (each test item's verdict per trial, each check with its kind, result and where it looks) and a walkthrough of the events its checks read. Use when the user invokes `/spec-pages`, asks for a page, a diagram or an explorable view of what a spec or capability does and how it is proven, or opens `docs/pages/`. Skip for a tour of the code (code-safari), a code review, a design, or a metrics dashboard: a spec page shows statements and their proof and writes no verdict of its own."
---

# spec-pages

The bottleneck in LLM-assisted work is keeping the user informed of what was built and how it works; "just read the docs" costs too much attention and loses detail. A spec page answers one question per scenario of a spec: *what does the system do here, and what shows it?* The reader starts from the spec's own statements, picks one, and sees how it was checked and what the system did, with details on demand. Every part of the page is computed, so a page read at a commit is true of that commit.

## Terms

Agreed with the user on 2026-11-13. Use them as written; a new term goes to the user before it goes on a page.

- **spec page**: one HTML page per capability spec, built from data computed from the spec and the tests that cite it. Avoid: report, dashboard.
- **How it was checked**: a scenario's proof section: each test item's verdict per trial, then each check under it. Avoid: evidence (that is what an eval agent reads), results.
- **check**: a scripted test of one fact. Its kind is *decide* (its failure fails the trial), *signal* (a word match, decides nothing) or *audited* (computed, then reviewed by the eval agent, decides nothing).
- **audit**: the eval agent's opinion of a check in one trial, agrees or suspect, with a reason. It never changes the check's result.
- **walkthrough**: a scenario's sequence of what the system did, drawn from recorded runs, in lanes: the events its checks read, the whole trial on demand. Avoid: trace (a run is not deterministic), diagram.
- **milestone**: one event in a walkthrough that a scenario or a check names: a merge, a fix commit, a test run, a journal entry, an ask, a spawn. With more than one trial, each row shows how many trials reached it.

## Where pages live

In the product's `docs/pages/`, beside the `docs/specs/` they document, committed with the code:

| File | Kind |
|---|---|
| `build.py` | written once per project, adapted from `references/example-execute/`; reads the sources, never writes them |
| `spec.template.html` | the page: layout, styles, script; no data |
| `<capability>.data.js` | generated: `window.SPEC_PAGE = {…}`, deterministic |
| `<capability>.html` | generated from the template, loading its data file |

When another session owns the specs or the tests, the pages directory is this skill's alone and the other session never edits it; the build only reads. Pages are not map material: a workspace's objective maps stay where they are.

## When pages are made

On demand: the user invokes `/spec-pages` or asks for a spec's page. Rebuilding after a spec change or a test run is the user's call. The data file records the commits it was read at (the specs' tree and the tests'), so what a page is true of can be told; the page does not show them (rule 10). No other skill calls this one.

## What a page shows

In this order, because the reader looks first for what the system does:

1. **Header.** The spec's title and a coverage line (scenarios; cited by a test; passing every trial; not cited yet). When results come from validation trials, one line says how many tests show one (rule 7).
2. **The list.** Every scenario, grouped under its requirement, each with a mark: passes, fails, mixed, or not cited. Filters: all, cited, uncited. The selected scenario is highlighted here, and its title and requirement are not repeated beside it.
3. **One scenario.** Its When and Then as two cards.
4. **How it was checked**, per tree (or build), a button per tree, a validation trial labelled on its button:
   - each test item that cites the scenario: its id, its question and its verdict per trial, with each trial's reason and what it quoted behind "why";
   - each check under it, one row each: what it computes, its kind in short words ("decides the trial", "signal · decides nothing", "audited · decides nothing", the full meaning on hover), its result per trial, a line when the eval agent's audit suspects it, and where it looks: the walkthrough rows it read, or "the end state, not one event", which opens to what the check found in each trial, since no event shows that proof;
   - when the tests have changed since the run: what the run's version cited and what the current one cites.
5. **The walkthrough.** The events this scenario's checks read, aligned across trials, in lanes (for the example: git, tests, the record, spawned agents at the boundary), each row keeping its number in the whole trial; "show the whole trial (n other events)" brings the rest back. When the checks read no single event, it says they read the end state. A row carries a badge for each check of this scenario that read it, with its result (✓ or ✕ for one trial, how many trials it passed for more), and with more than one trial, how many trials reached the row. Opened, a row shows each trial's call: how the build tied the event to it, the command with the tying text highlighted, and for a test run what it returned, its verdict line in red or green.

An uncited scenario says so, and shows the spec's `:TEST: none:` reason when it has one.

## Rules

1. **Computed only, first.** The first page of a project has no authored text. An authored part is added only after the user has read the computed page and asked for it, with checks designed for that part. The record: an authored "what the proof says" went stale without being flagged once its source was fixed (the outcome pages, `maps/outcomes/friction.org`, O-NS-12).
2. **Key on spec scenarios, and open on the list.** Not on test cases or test ids: the reader asks what the system does, and a test answers it.
3. **Every mark says what it means, on the page,** unless the user has read it and found it obvious. A colour, highlight or count with no stated meaning is a defect. The record: the first page highlighted the walkthrough rows a scenario's checks read, and the user could not tell what the highlight meant or what had confirmed the scenario; it was replaced by named check badges and the How it was checked section. A highlight marks only text the build matched on (the text that tied an event to its call); the caption saying so was dropped once the user found it obvious (2026-11-14).
4. **Show each layer of the proof apart.** A verdict and the checks under it can disagree, and the disagreement is the finding: in the example, every check of "A regression is fixed in the change like a blocking defect" passed in all three trials while the eval agent failed one, because the journal named the wrong goals. The item's verdict and its reasons, each check's result, and an audit that suspects a check are shown apart, each on its own line.
5. **Milestones come from the end state, linked to the calls that made them.** Not from the commands alone: one shell call holds several commands and reports one error, tools hide behind shell variables, and quoted text reads as a command (the effort's probe `milestones-from-transcripts`, no). Commits, record entries, asks and spawns come from the end state the test kept; each links to the call whose input carries its text, or for a commit whose subject was built from variables, the call that first printed its sha. Test runs come from the calls that ran the tests, as the test's own collector names them where it does, else the calls that returned a summary; kept output files are not a record of the runs (one trial overwrote its failing run; probe `milestones-from-end-state`). Order rows by the mean call number across trials, and say the order is approximate.
6. **Say what a link rests on.** Where the build ties a milestone to a check by matching the check's detail text, or any other heuristic, the page or its notes say so; a link that rests on another project's wording is reported, not hidden.
7. **Results from the latest counted run; failing that, the validation trial, labelled.** A test with no counted run on a tree shows the trial its validation names, and the page says it is one trial that does not count. The run, the tree's commit and the test's version are kept in the data file, not shown (rule 10); say when the tests have changed since the run.
8. **Deterministic data.** Sorted keys, paths masked (a trial's scratch directory becomes `<trial>/`), no timestamps of the build. Build twice and compare before showing the page. The data file's script tag carries a hash of its content (`?v=<hash>`), since a browser caches a `file://` script.
9. **Data apart from presentation, no dependencies.** The data file is loaded with `<script src>`, which works from `file://` where `fetch` does not. No external libraries. Colours are tokens on `:root` with a dark-mode override; the page works at phone width with no horizontal page scroll.
10. **Show the scenario's proof, not the plumbing.** What the reader does not use to answer *what does the system do here, and what shows it?* stays in the data file or behind a toggle, and nothing is said twice. The record (2026-11-14): the user found the first page about 30% too dense and removed the spec's status and Purpose, the commits read, each run's line (run, commit, test version, trial count), the legend of check kinds, the requirement repeated beside the scenario and its text under the cards, the caption on each verdict, and the check rows' expansion (what a check found and the audit's reason). What a check of the end state found came back the same day, behind "the end state": with no event in the walkthrough, it was the only proof such a check had on the page. The walkthrough showed every event of the trial, the same under each scenario its test cited, and 93% of its rows were read by no check of the scenario shown; it now shows only the events the scenario's checks read.

## Steps

1. **Read the sources.** The specs (`docs/specs/<capability>.org`: requirements, scenarios, `:TEST:`); what cites scenarios (an orchestrate home's `coverage.py`; an eval case's `expected.org` `:CITES:`); where results are kept (the evals' `history.jsonl`, each trial's details, kept evidence and transcript; a test runner's output); the project's glossary.
2. **Adapt the build** from `references/example-execute/build.py` (its `README.org` lists what is specific to that workspace), one source at a time: the spec parser first, then citations, then results, then milestones. Keep its shape: parse, link, write one data file, fill the template. A product repository whose tests are deterministic has one trial per run; its walkthrough comes from a traced test run, or is left out until one exists.
3. **Build and check.** Run the build twice and compare the data files. Open the page in a browser: a scenario with checks, one with none, one with a failure; light and dark; phone width; no console errors. A blank page is a script error: syntax-check the page's script (`node --check`) and reload past the cache.
4. **Show the user one scenario they know**, and ask what was unclear. Refine on their reading: what they could not tell is a rule 3 defect, fixed before anything is added.

## What spec-pages does not do

- It writes no verdict, summary or reading of its own; the page shows the spec's statements and the tests' results.
- It does not tour the code (code-safari) or review it.
- It never edits the specs, the tests or their results, and it does not install itself or any other skill.

## Borrowed

| From | What | Source |
|---|---|---|
| The reddit_scraper living-documentation demo | Scenarios as the filter for what is worth drawing; Given/When/Then as cards; lanes with the faked boundary drawn apart; plumbing hidden, details on demand; generated `*.data.js` apart from hand-written HTML; deterministic data | `moogah/reddit_scraper`, branch `claude/confident-fermi-fjw3y6` at `cf358f8`, `docs/living_documentation.md`, `docs/scenarios.html` |
| The outcome pages | What is computed and what is authored, kept apart; a page saying how it was checked | the skills workspace, `maps/outcomes/` (2026-11-12) |
| The research effort | The decisions, the terms and the two milestone probes | the skills workspace, `research/document-as-you-go/` (brief, `probes/`) |

The worked example, `references/example-execute/`, is the `execute` spec's page as built on 2026-11-13 (skills repo branch `skill-evals` at `b0d3358`, `orchestrate/docs/pages/`) and refined with the user on 2026-11-14 (`bde2c0e`, `b3012e5`).
