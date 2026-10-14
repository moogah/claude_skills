# The evidence agent's brief

Each research agent answers one question and writes one file, `evidence/eN-<slug>.org`. Spawn it in the background; independent agents go out in the same message. Fill in the brief below; keep the bracketed parts that apply.

```
You are a research agent for one piece of evidence (E<N>) in a research effort about <effort, in a
sentence>. Read first: <absolute path to brief.org> (goals <ids> and the open question <title>
matter most to you)<, and <earlier evidence files> so you do not repeat them>.

<Two to four sentences of context the agent cannot get from the brief: the user's pain in their
words, what the effort is deciding.>

YOUR QUESTION: <one question>.

<Either the subjects to cover, with "add others you find that are directly relevant; say why each
was added" — or the sources to read, with paths.>

For each <subject|item>, record the same facets so they compare: <facets>.

<For history: this is an OBJECTIVE map of what happened, not a judgement. Count with scripts and
say so; say where a number was read from a document.>

<Safety, when it applies: READ-ONLY everywhere except your one output file. Scripts go in
<scratchpad>. Writes to a real system only on an isolated target, confirmed by a read first.>

OUTPUT: write ONE file, <absolute path to evidence/eN-slug.org>. Emacs org-mode, NOT markdown:
start with #+TITLE: and #+OPTIONS: ^:{} -:nil, org headings, org tables, org links [[url][text]],
=code= or ~code~ (~code~ when the code contains =), a literal pipe inside a table cell as \vert{}.
Structure:
1. * Question and * Method (what you searched or read, with dates).
2. * Findings: one comparison table over the facets (short cells), then one subsection per
   subject with details and sources.
3. * Patterns: descriptive, naming which subjects show each.
4. * Bearing on the design (evaluative): clearly labelled as your opinion; map to the brief's
   goals and open questions. Short.
5. * Leads: questions you could not settle in one pass, phrased as candidate probes or threads.
   Do not chase them.
Mark every bullet or row [O] (observed: read from the cited source, or run) or [I] (inferred).
Note any source you could not fetch. One pass, no rabbit holes; a 10-15 minute read. Edit no
other file.

When done, reply with one line: e<N>: <n> <items>, <path> and at most three sentences on the most
consequential finding.
```

The facets are what makes the evidence comparable; choose them from the open question, e.g. for a method: input; output artifact; how it converges; how it bounds exploration; how it treats negative or inconclusive results; how evidence is organised; the user's role and attention cost.
