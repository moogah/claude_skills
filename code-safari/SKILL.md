---
name: code-safari
description: "Write a code safari: a broad, scoped tour of a body of code (a change, a repository or a subsystem) as one org file, the trunk, of about 6-12 stops, each saying what one part is, how it connects to the others and where to read further, optionally with an Emacs layout per stop that puts the code beside it. Agrees the scope with the user before writing; reopens a safari to extend it or answer from it, first naming the stops the code has moved under. Use when the user invokes `/code-safari`, asks for a code safari, a tour or the overall shape of some code (\"I no longer understand it\"), or opens a `safari/<subject>/trunk.org`. Skip for a deep dive into one part, a code review or a design: a safari describes what is there and judges nothing."
---

# code-safari

A code safari lets a reader who has not seen the code say what each part is and how the parts connect after one pass, without reading the code itself. It is broad, not deep: every stop ends in links to the exact places to read further, and the safari itself never explains code line by line. Its value is that it is concise and has a defined scope, agreed with the user before a word of it is written.

## Terms

- **code safari**: a short org document that tours a body of code in a fixed number of *stops*, each read beside the code it describes. Avoid: tour, walkthrough, guide (as names for the whole).
- **stop**: one section of a safari: what one part is, how it connects to the others, and where to go deeper; it names one layout. Example: "Stop 5: The fence". Avoid: step, chapter.
- **trunk**: the safari's main file, `trunk.org`, holding the scope, the summary, the map, the stops and the terms.
- **layout**: a saved Emacs window arrangement of a workspace; a safari's layouts are named `safari/NN-<slug>` and open the trunk at a stop on the left with code panes on the right.

## Where a safari lives

| Reader | Directory |
|---|---|
| The user (the default) | `<home>/safari/<subject>/trunk.org`, in the workspace the session runs in |
| Others too (the user says so in the scope round) | `<code-root>/docs/safari/<subject>/trunk.org`, committed with the code |

`<subject>` is the change's name, the repository's, or a kebab-case name for the subsystem. The trunk is the only file; layouts live in Emacs and are re-made from the trunk (§ Layouts).

## 1. Get a little familiar

Read only enough to draft an outline, before the first question:

- the project's `README`, `CLAUDE.md` and `docs/glossary.org`;
- for a change: its intent, design and the specs it names, `git log --oneline <base>..<branch>` and `git diff --stat <base>..<branch>`;
- the directory tree of the subject, and the headers or overviews of its largest files;
- the project's own record of known problems: a change's hardening list, `.tasks/`, the last run's failures. These are the only problems a safari reports (§ What a safari does not do).

No deep reading here. The scope round decides where the safari goes; reading first only makes the outline worth editing.

## 2. The scope round

Ask only what reading cannot tell you, one question per message, with two to four options and a recommendation:

1. **Purpose.** What the user wants to be able to do after the tour (review the change, extend a part, decide its next step, explain it to someone). This decides which parts get a stop and how far each zooms in.
2. **Readers.** Whether anyone else will read it. Yes puts it in the repository (§ Where a safari lives).
3. Only when reading left it open: **the subject** (a change against its base, a repository, or one subsystem).
4. Only when Emacs is reachable (§ Layouts): **layouts**, yes or no.

Then show the **draft outline**, in chat:

- the one-breath summary, three or four sentences;
- the numbered stops, one line each, about 6-12 of them (more only when the user adds them);
- *Left out*: what a reader might expect and the safari skips, each with a few words of why.

The user cuts, adds, merges or reorders stops. The agreed outline is the scope: it goes into the trunk's `* Scope`, dated, before any stop is written, and the stops follow it. A stop the user did not agree to is not written.

## 3. Write the trunk

From `templates/trunk.md`. One pass, then the anchor check, then show the user.

- **A stop** has four parts: *On screen* (what the layout's panes show, in words, so the stop stands alone without Emacs); *What it is*; *How it connects*, to other stops by number; *Go deeper*, the links. Each stop reads in a few minutes beside its code.
- **The map** is an ASCII picture whose boxes are the stops, with a sentence on how to read its arrows.
- **Anchors are text, never a line.** A link is `[[file:path::*Heading]]`, `[[file:path::/regexp/]]` or `[[file:path::exact text]]`, and never `::<number>`. Use `#+LINK:` abbreviations for the code root and the home. The record: the example's links and layouts all pointed at lines; written at 155 commits of its branch, they landed mid-text by 237, two days later.
- **The trunk records what it describes**: the commit (for a change: branch, base commit, head commit and the diffstat), in `* About this safari`.
- **Terms**: a domain word the reader needs goes in `* Words you'll meet`; use the project's glossary terms where they exist and link the glossary rather than redefining them.
- **The anchor check**, before the user sees it: every link's file exists and every search string is found in it at the recorded commit (`git show <commit>:<path> | grep -n -- '<string>'`). A string found more than once is made longer until it is found once.

Commit the trunk where it lives, unless the user's workspace leaves commits to them.

## 4. Layouts, when Emacs is there

The user's Emacs is reachable when `~/emacs/bin/emacs-agent` exists. Layouts are added only when the user said yes in the scope round, and the trunk stands alone either way.

- Each stop carries `:LAYOUT: safari/NN-<slug>` and `:PANES:`, the pane files with their text anchors (`path::anchor`, separated by ` | `). The trunk is therefore the layouts' source: there is no layout read tool, and no script to keep in step.
- `workspace_add_layout` panes take a file and a line only, so resolve each anchor to a line when the layout is added (`grep -n` in the working tree), never earlier:

  ```
  ~/emacs/bin/emacs-agent live call-write workspace_add_layout '{"directory": "<workspace root>", "layout_name": "safari/03-lean-start", "source": "safari/<subject>", "description": "Code safari, <subject>: <the stop in a line>", "split": "columns", "panes": [{"file": "<trunk path>", "line": <the stop heading>}, {"split": "rows", "panes": [{"file": "<code path>", "line": <resolved>}, {"file": "…", "line": <resolved>}]}]}'
  ```

  Two or three code panes on the right; paths relative to the workspace root.
- A layout is refreshed by `workspace_remove_layout` (`{"directory": …, "layout_name": …}`) and adding it again.
- Layouts added this way do not open on Emacs 30.1 until the `min-weight` bug is worked around (`~/emacs/.tasks/workspace-recipe-layout-emacs-30-1-min-weight.md`). When the user says one does not open, name that task.

## 5. Reopening a safari

A safari is a living document. When it is opened again, to answer a question from it or to extend it:

1. **The drift check, first.** Compare the recorded commit with the code: `git diff --stat <recorded>..HEAD -- <the files the stops link>`, and the anchor check (§ 3) at `HEAD`. Name the stops whose anchors are missing or whose code changed, in one message, and ask whether to refresh them. A refreshed stop is rewritten to the new code and its layout re-added. The recorded commit moves only when every stop is true at the new one.
2. **Inline or extend.** For a question, ask the user whether to answer it inline (in chat) or extend the safari, recommending one: inline when the answer is a detail of one part, extend when it is a part or a connection a reader of the scope would expect. The safari stays concise only if most answers stay in chat.
3. **An extension** goes into the stop it belongs to, or becomes a new stop the user agrees to, and adds a dated line to `* Scope` § Covered. Its anchors pass the check like the rest.

## What a safari does not do

- It is not a deep dive into any one part. *Go deeper* is where a deep dive starts.
- It is not a design, a review or a judgement of the code's quality. It describes what is there. It reports the problems the project's own record names (open tasks, hardening lists, known failures), citing them, and never ones the agent noticed itself.

## Borrowed

| From | What | Source |
|---|---|---|
| The fast-test-runner safari | The trunk's shape: one breath, theme table, ASCII map whose boxes are stops, four-part stops, words table; a layout per stop | `~/emacs-workspaces/emacs-testing-speedup/safari/fast-test-runner/` at `9d04394` |
| VS Code CodeTour | A tour pinned to a git `ref`; steps anchored by regex `pattern` rather than line; drift caught by a check (CodeTour Watch) | github.com/microsoft/codetour (v0.0.59) |
| Org link search options | `::*heading`, `::/regexp/` and text anchors that survive line shifts | Org manual, Search Options in File Links |
| DeepWiki | The commit a description was made from, shown with it ("Last indexed"); a hand-written page list with a purpose each (`.devin/wiki.json`), the nearest thing to `* Scope` | docs.devin.ai (DeepWiki) |
| arc42 § 5 | "Relevance over completeness": leave out the normal, simple and boring, and say so (*Left out*) | docs.arc42.org/section-5 |
| matklad, ARCHITECTURE.md | A codemap of coarse parts and how they relate, naming the things that rarely change | matklad.github.io/2021/02/06/ARCHITECTURE.md.html |
| C4 model | One zoom level per audience and story: the purpose question decides the zoom | c4model.com |
| Google docguide | Documentation "alive but frequently trimmed": the inline-or-extend question | google.github.io/styleguide/docguide/best_practices.html |

The research that produced this skill, with every source cited: `~/emacs-workspaces/claude-skills/research/code-safari/` (brief, evidence E1).
