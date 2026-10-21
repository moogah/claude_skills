# Design document

A change's design is `docs/changes/<change>/design.org` in the code root, beside its intent. The design round (`flows/design.md`) writes it: Context, Decisions, Seams and, while any are open, Open Questions. It is the Architect's home: plan drafts tasks from it, briefs quote its rows, the Reviewer reads them, and integrate's conformance check amends them. Which test covers which scenario is not written here; `coverage.py` computes it from the specs and the tests (`templates/spec.md` § Coverage).

## Form

```org
#+TITLE: <change>: design

* Context
<The code as it stands at the change's base, as far as the change needs it.>

* Decisions

** D1: One writer for the ledger file (seam/dead-video-ledger)
<What must hold and why, in a paragraph.>

* Seams

| id                     | statement | owning symbols | scenarios | tests |
|------------------------+-----------+----------------+-----------+-------|
| seam/dead-video-ledger | One writer (=DeadVideoLedger._rewrite=); the heal pass is the only reader; an unreadable file fails open for that pass | =reddit_scraper/core/dead_video_ledger.py=: =DeadVideoLedger.record=, =.remove=, =.lookup= | dead-video-index § A dead video on a later scrape pass; dead-video-index § An unreadable ledger file | =tests/test_dead_video_ledger.py= |
| seam/bookmark-advice   | Exactly two advice seams on =bookmark.el=: =:filter-return= on =bookmark-make-record= stamps the record; =:around= on =bookmark-handle-bookmark= resolves it; nothing else touches a record | =config/workspaces/portable-bookmarks.el=: =jf/portable-bookmarks--stamp=, =jf/portable-bookmarks--resolve= | portable-bookmarks § A bookmark made in a workspace carries its coordinates | =config/workspaces/test/portable-bookmarks-stamp-spec.el= |

* Open Questions

** Q1: <the question in plain terms>
<Options with the consequence the user would notice, the recommendation and why, the rows and pending tests that depend on it.>
```

Other sections a change needs (Risks / Trade-offs, Migration Plan) sit between Decisions and Seams. A cell holding `|` writes it `\vert{}`.

## Seams

One row per seam. A seam is a contract between two parts of the code: who produces a thing, who consumes it, what must hold at the boundary.

- **`id`** is `seam/<name>`, unique within the change. Tasks cite it in `cites_seams`; findings in `seam`; `design-diff.json` in `seam`.
- **`statement`** is one or two sentences of what must hold at the boundary: a contract. Not why (that is a Decision), not how the tests do it (that is the tests column), and not how the code does it: **no guard, error string, encoding, helper name, test name or transient marker** ("(new)", "pending until promoted"). Those live in the code and the tests, and each one a row names is a correction waiting to happen: in the record, 22 of 36 corrections were rows that did not list a guard, error or encoding an Implementor had added. A row or Decision that names one is wrong by construction and is **cut, not amended**.
- **`owning symbols`** are the entry points of the contract: the file and the symbols another part of the code calls or reads at this boundary (the producer, the consumer, the mapping), never every helper behind them and never line numbers. A symbol the design introduces is named before it exists, with no marker saying so.
- **`scenarios`** cites each spec scenario the seam serves as `<capability> § <scenario title verbatim>`, the capability id as `templates/spec.md` defines it, so the row can be grepped against the spec and fed to `coverage.py --scenario`.
- **`tests`** names the test files (or `file::test` where the runner supports it) that pin the seam.
- A row holds **current state only**: no status, no importance flag, no dated amendments, no "why tests missed", no message to a future role. `git log -- docs/changes/<change>/design.org` holds the history; a finding id or a `record-correction:` journal line in a commit message is the cross-reference.
- **A Decision that replaces a mechanism names what the old one provided and whether the new one keeps it**: each property kept or dropped (a reload path, per-file error isolation, where dependencies are installed, an order) as a seam row a Reviewer can cite, or a sentence in the intent's Removals when it is dropped on purpose. The record: D6 moved three packages from `jf/load-module` to `require`; reload and per-file error isolation went with it unstated, no row carried them, and `/code-review` found both after the cycle closed.
- A Decision that names a seam links to it by id ("D1 (seam/bookmark-advice)"). **The same rule binds Decisions**: a Decision records what must hold and why, never a guard, an error string, an encoding or a helper name; the conformance check does not write one in (the record shows D5's "how the line becomes point is an implementation choice" rewritten into a helper name and an encoding, and reversed twice).
- **Record correction.** When the code is right and a row, a Decision or a spec scenario is stale (a test pins what the code does; dropping a promise the code does not keep is a finding instead, `flows/integrate.md` § 1), the orchestrator fixes the text in `docs/` and commits it in the code root with a subject naming the row, and writes one journal line, `state.py note note "record-correction: <what>" --ref <seam-id>`. It is not an ask, carries no question and no file (`templates/ask.md` § Kinds); `design-diff.json` lists the row with that journal line's text as its `ref`. A stale row whose staleness is a missing guard, error or helper is corrected by cutting, not by adding the detail.

## What the design never carries

- Catalogue-style history: statuses, amendment logs, snapshots of prior text, cycle-suffixed keys.
- Implementation steps. Those are the task body's.
- Implementation detail: guards, error strings, encodings, helper names, test names, transient markers. Those are the code's and the tests'; a row that holds them is cut back to its contract.
- A scenario-to-test table. The tests carry their citations and `coverage.py` reads them; a scenario with no test worth writing says so in the spec (`:TEST: none: <reason>`).
- Framework, naming and mocking policy. Those are the overlay's role extensions.
