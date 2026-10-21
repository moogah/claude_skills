# Hardening list, MVP decision and closing summary

A change first produces a working **MVP** (minimum viable product): the smallest version of the change that works end to end for its main use, enough for the user to judge whether it is the right thing. Its **main path** is the uses the intent's goals name, worked end to end (`templates/architect-finding.md` § Routing). **Hardening** is the work after the MVP that makes it hold up: edge cases, error paths, unusual inputs, robustness. The order is the point: the MVP may show that the wrong thing is being built, and hardening done before then would be wasted.

## The hardening list

`<home>/.orchestrator/changes/<change>/hardening.org`, one per change: the reasoned defects and robustness work off the main path. The orchestrator creates it with its first entry and adds one entry each time it routes a finding `hardening`; no other agent writes it. It is committed in the home with the phase close.

```org
#+TITLE: Hardening list: <change>
#+OPTIONS: ^:{} -:nil

* issue (hardening): A recipe with a remote pane path makes the restore contact the remote host
:PROPERTIES:
:REF: cycles/cycle-1791689476/reviews/ws-recipe-layouts.md#2
:SEVERITY: medium
:FILES: workspaces.el:412
:END:
A layout whose pane names a /ssh: path passes the recipe check, and restoring it calls file-exists-p on that path, which opens a connection to the host and can hang the restore while it prompts. Local panes, the change's goal, are unaffected.

* question (hardening): Teardown may leave the per-target data directory behind when Emacs is killed
:PROPERTIES:
:REF: disc-ea-data-profile-1
:SEVERITY: low
:FILES: data-profile.el:88
:END:
Tried: started and stopped a target twice with the CLI; the directory was removed both times. Not tried: kill -9, which skips kill-emacs-hook.
```

- The heading is the finding's kind and decoration in the Conventional Comments form (`issue (hardening):`, `question (hardening):`) and one line in product terms. A defect in code outside the change says so in its body ("outside the change: …"), so the harden step can place its task (`externalisation.md`).
- `:REF:` is the finding id, the review file and finding number, or the discovery id; `:SEVERITY:` is the reporter's; `:FILES:` the locations.
- The body is one paragraph: what goes wrong, on which input, and what the user would notice. For a `question`, what was tried and what it showed.
- A user's decision whose work is off the main path (`flows/integrate.md` § 7) is an entry headed `decision (hardening):` and the decision in product terms, with `:REF: <ask-id>` and no `:SEVERITY:`; the body says what building it would change for the user. It is built like any entry when the user chooses to harden, without asking again.
- Entries are appended, newest last. The MVP decision lists them by severity, decisions last.

What it does not hold: an untrue sentence in a shipped document (fixed in the change), a speculative need or a cleanup that changes no behaviour (a `thought`, never on the list), style or polish (not a finding).

## The MVP decision

When integrate § 9 finds the main path working end to end (no open task on the main path remains, `on_critical_path` in `flows/plan.md` § 6; the whole-change review leaves no blocking finding) and the intent's status line has not yet reached `mvp`, the change stops for one decision. Open tasks off the main path do not hold it back; they wait for the answer. The orchestrator sets the intent's status line to `mvp <date> <HEAD> <ask-id>` (`templates/intent.md`) and writes one `decision` ask (`templates/ask.md`) with `:BLOCKS: plan`. Nothing more runs on the change until the user answers: plan drafts nothing while it is open (`flows/plan.md` § 1).

The file, in the plain language `templates/ask.md` requires:

- **Title**: "<The change, in the glossary's words> works end to end: harden it, rethink it, or stop here?"
- **`* About`**: what the change does now, one short paragraph per goal, and how to try it (the command, the key, the file to open).
- **`* What happened`**: the main-path tests that pass and the whole-change review's result.
- **`* Hardening list`**: one line per entry (its heading, highest severity first) and the path to `hardening.org`. When the list is empty, the section says so.
- **`* Open tasks off the main path`**: one line per open task of the change whose `on_critical_path` is false, in product terms, with where it came from (a design scenario no goal names, a meta-discovery, a scope task). When there are none, the section is left out.
- **`* Record corrections to the specs`**: every spec edit made as a record correction since `#+BASE:`, each with its commit, the scenario, the test that pins it and the diff in an `#+begin_example` block. They are shown here, not asked about; the user objects in `* Answer` if one is wrong. Listed by `git -C "$CODE_ROOT" log -i --grep='record correction' -p <base>..HEAD -- docs/specs/` (every record-correction commit says so in its subject and names the pinning test in its body, `flows/integrate.md` § 1). The text corrections made at integrate follow, each with its commit and diff (`git log -i --grep='text correction' -p <base>..HEAD`). When there are none, the section says so.
- **`* Options`**:
  - `harden` :: The <n> hardening items, and the open tasks off the main path, are worked; the change is done when they are and a last review passes.
  - `rethink` :: The MVP shows something other than what you want. Say what is wrong; the goals are rewritten from your words and the change runs again to a new MVP. The hardening list is kept for that MVP.
  - `stop` :: The change is done as it is. The hardening list stays as the record of the known defects left.
- **`* Recommendation`**: one option and why, from what the list holds and what the intent says the change is for.
- **`* If unanswered`**: "Nothing runs; the change waits at its MVP."

Applying the answer (`templates/ask.md` § Answer protocol; a free-text answer is read back first):

- **`harden`**: the next plan drafts a candidate task from each entry (`flows/plan.md` § 2) and the Architect checks them like any task not drafted from the design; each entry gets `:TASK: <task-name>` and its finding `resolution=followup-task-<task-name>`. The open tasks off the main path the answer covers join the next batch. When none remains open, integrate § 9 runs the whole-change review again and the change goes to `done`; there is no second MVP stop. An answer that hardens only some entries marks the others `:LEFT: <ask-id>`; an open task it leaves stays in `tasks/open/`, named in the closing summary as left.
The answer is applied by the resumed integrate agent, or, when the user answered after that agent finished, by the next plan agent before it opens a cycle (`flows/plan.md` § 1); either writes the intent's status line for it.

- **`rethink`**: the intent's status line becomes `divergent <date> <ask-id>`, signal `user`, the answer quoted under `* Status`; an opening agent revises the intent and the spec edits from the answer (`flows/open.md` § When it runs), the design round re-runs for the seams the new goals move, and the change runs to a new MVP decision, where the kept entries are shown again, re-routed against the new main path, and every open task's `on_critical_path` is set again (`flows/plan.md` § 6).
- **`stop`**: the change is done: the status line becomes `done <date> <HEAD>`, with the closing summary (below). Open tasks off the main path are left, and the summary names them.

## The closing summary

When the intent's status line becomes `done`, its `* Status` entry for `done` is the change's closing summary, committed in the code root with the status line:

- what the change does, one line per goal;
- the hardening list's outcome: the entries worked (with their task names) and the entries left (their headings), or "none";
- the open tasks left off the main path (their names and one line each), or "none";
- the record corrections to the specs and the text corrections, as in the MVP decision: commit, scenario or file, pinning test where there is one, diff.

The dispatcher's relay of the done hand-back names `intent.org`'s path, so the user reads the summary there.
