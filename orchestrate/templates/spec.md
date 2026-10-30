# Spec file: the living record of one capability

The specs are one copy, in org, in the code root: `docs/specs/`. A change edits them in place on the branch its tasks merge into; there is no delta, sync or archive step, and `git diff <base> -- docs/specs/` is the change's spec record (`templates/intent.md`).

## Where

- A capability with one file is `docs/specs/<capability>.org`; one with several is a directory, `docs/specs/<capability>/<file>.org`.
- A **capability id** is the path under `docs/specs/` without `.org`: `workspaces`, `bash-parser/security`. A citation that names a directory (`bash-parser`) resolves over its files.

## Form

```org
#+TITLE: workspaces

* Purpose
<What the capability is for, in a paragraph.>

* Requirements

** Requirement: Per-machine persistence and restoration
The package SHALL …

*** Scenario: An absent file starts fresh and saves normally
- *WHEN* no state file exists at startup
- *THEN* the registry starts empty
- *AND* the first save writes a file the next startup reads
  (a continuation line is indented two spaces)

*** Scenario: A tab in another frame counts as open
:PROPERTIES:
:TEST: none: batch Emacs cannot create a second frame; seam/on-screen's statement carries the rule
:END:
- *WHEN* …
```

- Requirement and scenario titles are **citation keys**: a test cites `<capability> § <scenario title>` byte for byte. Rename a title only in the commit that updates its citations; `coverage.py` lists every citation that resolves to no scenario.
- Steps are `- *WHEN*`, `- *THEN*`, `- *AND*` bullets. Inline code is `=x=`, or `~x~` when it holds `=`; code blocks are `#+begin_src <lang>` or `#+begin_example`. No markdown.
- Other top-level sections a capability needs (Key Concepts, Integration Points) sit beside `* Purpose` and `* Requirements`; only `** Requirement:` headings under `* Requirements` are requirements.
- A change removes a requirement or scenario by deleting it; its reason goes in the intent's `* Removals`.

## A scenario's test

A test carries its citation itself, in the form the runner prints when it can:

| where | form |
|---|---|
| a Buttercup `it` string (preferred: the run output shows its status) | `(it "… (Scenario: <capability> § <title>)" …)`; several: `(Scenario: a § X; b § Y)` |
| a comment inside the test | `;; Scenario: <capability> § <title>` (the title may finish on the next comment line) |
| an ERT docstring line | `Scenario: <capability> § '<title>'` |
| pytest, other runners | the test's docstring or header, `Scenario: <capability> § <title>` |

A contract test also cites `seam/<name>`. The `:TEST:` property holds what a test cannot: `none: <reason>` for a scenario no test is worth writing for (no driveable surface, or a stated cost; the seam that stands in when one exists), or `<file>::"<start of the it string>"` / `<file>::<ert test name>` for a test that carries no citation and is not this change's to edit (a legacy coverage row). `:TEST+:` adds another.

## Coverage

`python3 <skill-dir>/scripts/coverage.py --root "$CODE_ROOT"` prints one row per scenario: capability, title, status, citations. Status, worst first: `failed`, `not run`, `pending`, `cited` (no run output given), `pass`, `none: <reason>`, `uncited`. `--output <raw test-run output>` joins Buttercup lines on the `it` string, ERT lines on the test name and pytest `-v` lines on the node id, and refuses an output with no line per test (a compact report: give it the run's log or a verbose run); run from the home, it finds the code root through `state.py where`; `--intent docs/changes/<change>/intent.org` keeps **the change's scenarios**: those the diff from its `#+BASE:` touches and those its `* Capabilities` names as stated, not met (`templates/intent.md`), and lists any name that matches no scenario; `--since <commit>` keeps only the touched ones; `--capability <id>`, `--scenario '<capability> § <title>'` (a name matching no scenario is listed) and `--uncited` narrow further; `--json` for a tool. The design round's exit and integrate's conformance read it; nobody writes coverage by hand.

## Who edits `docs/specs/`

The change's author before the design round (the user, or the orchestrator opening or splitting a change, `flows/open.md`), the design round, an applied answer (`templates/ask.md` § Answer protocol) and a record correction. Never an Implementor: a scenario an implementation shows to be wrong is a push-back in `## Discoveries`.
