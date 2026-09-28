# Design document: the orchestrator's two sections

OpenSpec writes `design.md` with its own sections (Context, Goals / Non-Goals, Decisions, Risks / Trade-offs, Migration Plan, Open Questions). The design round (`flows/design.md`) appends two more. They are the Architect's home: plan drafts tasks from them, briefs quote their rows, the Reviewer reads them, and integrate's conformance check amends them.

## Seams

One row per seam. A seam is a contract between two parts of the code: who produces a thing, who consumes it, what must hold at the boundary.

```markdown
## Seams

| id | statement | owning symbols | scenarios | tests |
|---|---|---|---|---|
| seam/dead-video-ledger | One writer (`DeadVideoLedger._rewrite`); the heal pass is the only reader; an unreadable file fails open for that pass | `reddit_scraper/core/dead_video_ledger.py`: `DeadVideoLedger.record`, `.remove`, `.remove_blocked`, `.lookup` | dead-video-index § A dead video on a later scrape pass; dead-video-index § An unreadable ledger file | `tests/test_dead_video_ledger.py` |
| seam/bookmark-advice | Exactly two advice seams on `bookmark.el`: `:filter-return` on `bookmark-make-record` stamps the record; `:around` on `bookmark-handle-bookmark` resolves it; nothing else touches a record | `config/workspaces/portable-bookmarks.el`: `jf/portable-bookmarks--stamp`, `jf/portable-bookmarks--resolve` | bookmarks § A bookmark made in a workspace carries its coordinates; bookmarks § Jump restores point in a moved file | `config/workspaces/test/portable-bookmarks-stamp-spec.el`, `…-resolver-spec.el` |
```

Rules:

- **`id`** is `seam/<name>`, unique within the change. Tasks cite it in `cites_seams`; findings in `seam`; `design-diff.json` in `seam`.
- **`statement`** is one or two sentences of what must hold. Not why (that is a Decision), not how the tests do it (that is the tests column).
- **`owning symbols`** are file and symbol names, never line numbers. A symbol the design introduces is named before it exists.
- **`scenarios`** cites each spec scenario the seam serves as `<capability> § <scenario heading verbatim>`, where `<capability>` is the directory under `specs/`, so the row can be grepped against the spec.
- **`tests`** names the test files (or `file::test` where the runner supports it) that pin the seam.
- A row holds **current state only**: no status, no importance flag, no dated amendments, no "why tests missed", no message to a future role. `git log -- design.md` holds the history; a finding or ask id in a commit message is the cross-reference.
- A design Decision that names a seam links to it by id ("D1 (seam/bookmark-advice)").

## Scenario coverage

One row per `#### Scenario:` in the change's `specs/`.

```markdown
## Scenario coverage

| scenario | kind | test | note |
|---|---|---|---|
| dead-video-index § A dead video on a later scrape pass | acceptance | `tests/test_dead_video_ledger.py::test_dead_video_is_not_redownloaded_on_a_later_pass` | drives `WebArchiver.run('scrape_reddit')` on `tmp_path` with the downloader stubbed at `VideoDownloader` |
| dead-video-index § An unreadable ledger file | contract | `tests/test_dead_video_ledger.py::test_unreadable_ledger_fails_open` | seam/dead-video-ledger |
| bookmarks § Jump restores point in a moved file | none | — | no driveable surface for an interactive jump under `--batch`; seam/bookmark-advice's resolver spec stands in (contract) |
```

Rules:

- **`kind`** is `acceptance` (drives the scenario through a surface an operator would: a CLI on a temp directory, a compose service, `emacs --batch`, real `git`), `contract` (a seam's test stands in), or `none`.
- A **`none`** row always carries the reason (no driveable surface, or a stated cost) and the seam that stands in when one exists. This is where "behaviour-driven testing is not an option here" is decided, once.
- **`test`** names the test. A pending test (marked with the overlay's `test.pending-style`) exists only for behaviour HEAD does not yet satisfy; a scenario HEAD already satisfies has a plain, passing test.
- The table is complete when every scenario in `specs/` has a row. Plan § 2 drafts tasks from the rows whose tests are still pending; a task's Verification section lists the ones it promotes.

## What the sections never carry

- Catalogue-style history: statuses, amendment logs, snapshots of prior text, cycle-suffixed keys.
- Implementation steps. Those are the task body's.
- Framework, naming and mocking policy. Those are the overlay's role extensions; the coverage table is per scenario, not per framework.
