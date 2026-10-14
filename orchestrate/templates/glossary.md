# Glossary: the product's agreed terms

A product's glossary is `docs/glossary.org` in the code root, beside `docs/specs/`. It is the reference vocabulary for anything written to the user (asks, hand-back lines, chat) and for the specs. Every role brief names it by path. It holds the product's domain terms only: not general programming concepts, not the orchestrator's process terms.

A term enters when the user accepts it: during research, or from a change intent's `* New terms` at intent approval. An explanation given in chat on request is written in at once, in the same turn. A term used by a second project moves up to the subject's org-roam note (`~/org/roam/<subject>/`); the agent proposes, the user decides, and this file keeps a one-line link.

## Form

```org
#+TITLE: Glossary
#+OPTIONS: ^:{} -:nil

* <term>
<What it is, in one or two sentences, plain; not what it does, not how it is built.>
- Example :: <the user's own use of it>
- Avoid :: <rejected synonyms, comma-separated>
- Specs :: <capability ids of the specs that use it>

* <term>
See [[id:<roam node id>][<term>]] (subject: <subject>).
```

Edited in place; `git log -- docs/glossary.org` is its history. No check runs against it until a misuse is observed.

Borrowed: the entry form (what it is, words to avoid) and "write it the moment it is settled" from mattpocock/skills `domain-modeling` at `49dd158`.
