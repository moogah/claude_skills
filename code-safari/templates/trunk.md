# The trunk

`safari/<subject>/trunk.org` is the whole safari. It is written once from the agreed outline, then kept true and extended, not appended to as a log; `git log -p` on it is its history. Every section below is required except the theme table and the layout lines, which a safari without a change or without Emacs leaves out.

## Form

```org
#+TITLE: Code Safari: <subject>
#+DATE: <date written>
#+STARTUP: showall
#+OPTIONS: ^:{} -:nil
#+LINK: code file:<absolute path of the code root>/%s
#+LINK: home file:<absolute path of the workspace home>/%s

A broad tour of <subject>: what each part is, and how the parts connect.  It
skips the details.  <With layouts:> Each stop names a layout in this
workspace, =safari/00-<slug>= through =safari/NN-<slug>=.  Switch to one and
this file opens on the left at that stop, with the code it describes on the
right.  The /Go deeper/ links jump to the exact place when you want more.

* About this safari

- Subject :: <a change | a repository | a subsystem>: <name, and where its code is: [[code:]]>
- Describes :: commit =<sha>= <for a change: of branch =<branch>=, cut from =<base branch>= at =<base sha>=; N commits, N files, +N / -N lines>
- For :: <what the user wants to be able to do after the tour, in their words>; read by <the user | the user and others>
- Written :: <date>

* Scope

Agreed <date>.

1. <Stop 1's title>: <its one line from the outline>
2. ...

Left out: <what a reader might expect and this skips, each with why>.

** Covered

- <date> :: Stops 1-N, as agreed.
- <date> :: <an extension: what was added, to which stop or as which new stop, and the question that asked for it>

* The <change | repository | subsystem> in one breath

<Three or four sentences: what it does, how, and the one number or fact that matters most.>

| Theme | Files | Lines | Stops |
|-------+-------+-------+-------|
| <for a change: a group of the diff, by directory or purpose> | N | +N / -N | 1, 4 |

* The map

How the pieces call each other.  Each box is a stop.

#+begin_example
<ASCII diagram; each box labelled [N] with its stop number>
#+end_example

<One or two sentences: how to read the arrows, and which stop the rest stands on.>

* Stop 1: <title>
:PROPERTIES:
:LAYOUT:   safari/01-<slug>
:PANES:    <path>::<anchor> | <path>::<anchor>
:END:

*On screen:* <what the layout's panes show, in words, so the stop reads without them>.

*What it is.*  <What this part is and what it promises, in a few short paragraphs or a list.
No line-by-line explanation.>

*How it connects.*  <To other stops, by number: what it takes from them and gives them.>

*Go deeper.*
- [[code:<path>::*<Heading>][<what is there>]], [[code:<path>::/<regexp>/][<what is there>]].
- <Design decisions, spec requirements, tests that pin this part; known problems from the
  project's own record, cited.>

* Words you'll meet

| Term | Means |
|------+-------|
| <term> | <what it is, in a line; or a link to the project's glossary entry> |
```

## Notes

- **Anchors.** Every link and every `:PANES:` entry anchors to a heading (`::*Heading`), a regexp (`::/regexp/`) or exact text (`::text`) that is found once in its file. Never `::<number>`.
- **`:PANES:`** lists the code panes only; the trunk pane is implied (the stop's own heading). Resolved to lines only when a layout is added (`SKILL.md` § Layouts).
- **Stop 0.** A stop numbered 0 for *why the code exists and where its record lives* (intent, design, research) suits a change; it counts toward the 6-12.
- **The last stop** for a change is often *where it stands and where it goes next*, read from the project's own record (handshake, hardening list, open decisions), cited.
- **Describes.** Rewritten only when every stop has been checked true at the new commit (`SKILL.md` § Reopening).
