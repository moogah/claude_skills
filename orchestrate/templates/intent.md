# Intent: what a change is for

A change's intent is `docs/changes/<change>/intent.org` in the code root. It holds what the diff cannot carry: why the change exists, what it is for and not for, which capabilities it touches, and why it removes what it removes. The requirement and scenario edits are not here; they are the change's edits to `docs/specs/` (`templates/spec.md`), and `git diff <base>` is their record. When the change is done the file stays where it is, its status line saying so.

The intent is written with the change's spec edits, before the design round: by the user, or by the orchestrator when it opens or splits a change (`flows/open.md`). It is committed in the code root on the branch the change's tasks merge into.

## Form

```org
#+TITLE: <change>
#+STATUS: speculated
#+BASE: <the code-root commit the change starts from>

* Why
<What prompted the change, in the user's terms.>

* Goals
- <An outcome the change commits to.>

* Non-goals
- <What a reader might expect and the change does not do.>

* Capabilities
- workspaces: <what the change does to it>
- portable-bookmarks: <…>

* Out of scope
- <Work noticed and deliberately left, with where it went (a .tasks/ item, a later change).>

* Removals
- workspaces § <requirement or scenario title>: <why it goes; what a user who relied on it does instead>

* Open Questions
** <The question in plain terms>
- <Option>: <the consequence the user would notice>
- <Option>: <…>
Recommendation: <option>, because <…>. The spec is written to it. Affects: workspaces § <title>.
```

- **`#+BASE:`** is written once, with the change's first record commit: `git -C "$CODE_ROOT" rev-parse HEAD` before that commit. `coverage.py --since <base>` scopes coverage to the scenarios the change touched; the whole-change review diffs from it (`flows/integrate.md` § 9).
- **Capabilities** names each spec file the change edits by its capability id (`templates/spec.md`).
- **Capabilities** may also name a scenario the spec already states and the code does not meet (`workspaces § <title>: stated, not met`); the round takes it as one of the change's (`flows/open.md` § 3).
- **`* Open Questions`**, after Removals, holds what the source left to the user and the spec text depends on; each entry is deleted when the round's sitting answers it (`flows/open.md` § 4, `flows/design.md` § 5).
- **Removals** lists each requirement or scenario the change deletes from `docs/specs/`, with its reason: the diff shows that a scenario went, never why. Empty sections are left out.
- **Goals** and **Non-goals** are the stated outcome: plan's critical-path read and integrate's goal-drift check measure against them.

## The status line

The intent is the change's **outermost speculation**. Like the seam rows and Decisions in `design.org` it is written before implementation and checked against what implementation reveals; unlike them it carries a status, because its lifecycle is a user decision rather than a conformance check. Treating the stated outcome as immutable once written is what produces "ship the wrong thing on time".

| `#+STATUS:` | Meaning |
|---|---|
| `speculated` | Fresh; the forward chain has not yet stabilised against it |
| `confirmed <date>` | The forward chain has executed cleanly for ≥2 cycles with no goal-drift signal |
| `divergent <date> <ask-id>` | A goal-drift signal is open; the ask carries revise / split / abandon / continue |
| `reconciled <date> <choice>` | The user answered the goal-drift ask with `<choice>` |
| `done <date> <commit>` | The whole-change review found nothing blocking and no open task remains; `<commit>` is the code root's HEAD then |

What a status needs beyond its line goes in a `* Status` section at the end of the file, newest first: for `confirmed`, one paragraph on what stabilised it; for `divergent`, the signal (`pm-cascade`, `reviewer-spec-signal`, `architect-conformance` or `user`) and one paragraph of evidence; for `reconciled`, the prior outcome statement verbatim and one paragraph on what changed and why. Older text is in `git log -- docs/changes/<change>/intent.org`.

## Lifecycle

- **`speculated → confirmed`**: the PM digest sets it once the integrate→plan handshake has fired ≥2 cycles cleanly. No user action.
- **`→ divergent`**: the PM goal-drift signal fires (`flows/integrate.md` § 5); integrate writes the line and the `* Status` entry and raises the `decision` ask with the four options.
- **`divergent → reconciled`**: the user's answer in the ask file's `* Answer`; the handshake's `user_resolved_goal_drift` carries `{ask, decision, rationale}`. On `revise` or `split` the Goals are rewritten and the line returns to `speculated`, the prior outcome kept in `* Status`.
- **`→ done`**: integrate § 9, when the whole-change review leaves no blocking finding and no open task remains. The orchestrator writes the line and commits it in the code root with a subject naming the change.

Integrate writes the line in every case; execute and plan never do. A file with no `#+STATUS:` line gets a warning at plan and nothing more.

## Cost asymmetry

Amending a seam row costs minutes, revising a Decision hours, revising the intent days. The system absorbs a discovery at the lowest level that can hold it, but must not hide the expensive option when it is warranted: the status line is what makes goal drift visible, where otherwise goal-invalidating signals are absorbed as row amendments until the gap is too big to ignore.
