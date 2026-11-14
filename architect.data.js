window.SPEC_PAGE = {
 "capability": "architect",
 "cases": [
  {
   "capability": "architect",
   "id": "architect-conformance-clean",
   "items": [
    {
     "checks": [
      {
       "id": "AK-S3",
       "kind": "decide"
      },
      {
       "id": "AK-S4",
       "kind": "signal"
      },
      {
       "id": "AK-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A clean conformance says so in the journal",
      "integrate § Conformance that finds nothing says so in the journal",
      "record § A journal entry names who wrote it"
     ],
     "id": "AK-E1",
     "question": "Does a clean conformance say so in the journal, once?"
    },
    {
     "checks": [
      {
       "id": "AK-S1",
       "kind": "decide"
      },
      {
       "id": "AK-S2",
       "kind": "decide"
      },
      {
       "id": "AK-S12",
       "kind": "decide"
      },
      {
       "id": "AK-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Conformance lists the rows the cycle changed",
      "architect § Conformance hands back its counts",
      "architect § Conformance's hand-back names its design diff"
     ],
     "id": "AK-E2",
     "question": "Does conformance write its design diff, empty, and hand back a line naming it?"
    },
    {
     "checks": [
      {
       "id": "AK-S5",
       "kind": "decide"
      },
      {
       "id": "AK-S6",
       "kind": "decide"
      },
      {
       "id": "AK-S8",
       "kind": "decide"
      },
      {
       "id": "AK-S9",
       "kind": "decide"
      },
      {
       "id": "AK-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Conformance writes only its findings, its design diff and the text it corrects",
      "architect § No run commits, spawns an agent or writes a task",
      "architect § Probes leave their files in the scratch directory"
     ],
     "id": "AK-E3",
     "question": "Does conformance write nothing else, commit nothing, spawn no agent, write no task and file no ask?"
    },
    {
     "checks": [
      {
       "id": "AK-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § An Architect's finding carries its labels in its front matter",
      "reviewer § An Architect's finding says where, why the tests missed it and what to do",
      "architect § Style and preference are not reported",
      "architect § A speculative need or a cleanup is a thought line, not a finding"
     ],
     "id": "AK-E4",
     "question": "If the run writes a finding, is it a finding in the form, and is it a defect, not a thought or style?"
    },
    {
     "checks": [
      {
       "id": "AK-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A clean conformance says so in the journal",
      "architect § A cited seam the merged code no longer honours is a finding"
     ],
     "id": "AK-E5",
     "question": "Is the cycle's merged diff clean: no defect reported in it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "61359a8",
     "cites": {
      "AK-E1": [
       "architect § A clean conformance says so in the journal",
       "integrate § Conformance that finds nothing says so in the journal",
       "record § A journal entry names who wrote it"
      ],
      "AK-E2": [
       "architect § Conformance lists the rows the cycle changed",
       "architect § Conformance hands back its counts",
       "architect § Conformance's hand-back names its design diff"
      ],
      "AK-E3": [
       "architect § Conformance writes only its findings, its design diff and the text it corrects",
       "architect § No run commits, spawns an agent or writes a task",
       "architect § Probes leave their files in the scratch directory"
      ],
      "AK-E4": [
       "reviewer § An Architect's finding carries its labels in its front matter",
       "reviewer § An Architect's finding says where, why the tests missed it and what to do",
       "architect § Style and preference are not reported",
       "architect § A speculative need or a cleanup is a thought line, not a finding"
      ],
      "AK-E5": [
       "architect § A clean conformance says so in the journal",
       "architect § A cited seam the merged code no longer honours is a finding"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AK-E1": [
       "AK-S3",
       "AK-S4",
       "AK-S7"
      ],
      "AK-E2": [
       "AK-S1",
       "AK-S12",
       "AK-S13",
       "AK-S2"
      ],
      "AK-E3": [
       "AK-S10",
       "AK-S5",
       "AK-S6",
       "AK-S8",
       "AK-S9"
      ],
      "AK-E4": [
       "AK-S11"
      ],
      "AK-E5": [
       "AK-S14"
      ]
     },
     "items_then": [],
     "run": "20261114T123931",
     "trials": [
      {
       "calls": {
        "12": {
         "error": false,
         "text": "S=<trial>/scratch/architect-conformance; W=<trial>/home/.worktrees/tally/json-report; rm -rf $S/probe && mkdir -p $S/probe && git -C $W archive HEAD | tar -x -C $S/probe && cd $S/probe && sed -i '' 's/line.startswith((\"#\", \";\"))/line.startswith(\";\")/' tally/ledger.py && grep -n startswith tally/ledger.py && PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -t . 2>&1 | tail -3; printf '# note\\n2026-10-01,food,1.00\\n' > $S/hash.csv; PYTHONDONTWRITEBYTECODE=1 python3 -m tally.cli $S/hash.csv; echo \"exit=$?\"; cd $W && PYTHONDONTWRITEBYTECODE=1 python3 -m tally.cli $S/hash.csv; echo \" … [cut, 652 characters]",
         "tool": "Bash"
        },
        "8": {
         "error": false,
         "text": "S=<trial>/scratch/architect-conformance; mkdir -p $S; cd <trial>/home/.worktrees/tally/json-report && PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -t . -v > $S/suite.txt 2>&1; cat $S/suite.txt; cd <trial>/home && python3 ../skills/orchestrate/scripts/coverage.py --help 2>&1 | head -30",
         "tool": "Bash"
        }
       },
       "checks": {
        "AK-S1": {
         "audit": {
          "agree": true,
          "reason": "The file is `[]`, a JSON list.",
          "verdict": "agrees"
         },
         "desc": "design-diff.json in the cycle's directory, a JSON list",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-1/design-diff.json: list",
         "kind": "decide",
         "pass": true
        },
        "AK-S10": {
         "audit": {
          "agree": true,
          "reason": "No ask file appears among the added files.",
          "verdict": "agrees"
         },
         "desc": "no ask file written",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AK-S11": {
         "audit": {
          "agree": true,
          "reason": "The title counts 85 characters, over the 80 limit, while the other labels and sections are present, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "each finding file, if any: the labels, trigger conformance, class, title, discovered_by, blocks, no decoration, the three sections (not applicable with none)",
         "detail": "arch-cycle-1-1.md: title of 85 characters",
         "kind": "decide",
         "pass": false
        },
        "AK-S12": {
         "audit": {
          "agree": true,
          "reason": "The final text is the one required line ending in the design-diff path, under 200 characters.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line, conformance <cycle-id>: <f> findings, <a> rows amended; ... ending with design-diff.json's path from the home, at most 200 characters",
         "detail": "conformance cycle-1: 1 findings, 0 rows amended; .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json",
         "kind": "decide",
         "pass": true
        },
        "AK-S13": {
         "audit": {
          "agree": true,
          "reason": "The counts 1 and 0 match one finding file and no rows changed.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's counts are the finding files and the rows changed (not applicable without the line)",
         "detail": "hand-back ('1', '0'); finding files 1, rows changed 0",
         "kind": "decide",
         "pass": true
        },
        "AK-S14": {
         "audit": {
          "agree": true,
          "reason": "The finding's first location is coverage.py:387, not a line in tally/ledger.py 17-22.",
          "verdict": "agrees"
         },
         "desc": "no finding of kind issue located at a line the cycle's merged diff added or changed",
         "detail": "none; the diff's lines: tally/ledger.py 17-22",
         "kind": "decide",
         "pass": true
        },
        "AK-S2": {
         "audit": {
          "agree": true,
          "reason": "No Seams row changed (the code root diff is none), and the list is empty.",
          "verdict": "agrees"
         },
         "desc": "the list is empty, no seam row having changed (not applicable when a row changed)",
         "detail": "rows changed none; list []",
         "kind": "decide",
         "pass": true
        },
        "AK-S3": {
         "audit": {
          "agree": true,
          "reason": "A finding file exists, so not applicable matches AK-E1's n/a.",
          "verdict": "agrees"
         },
         "desc": "with no finding, the journal gains one entry, by the Architect (not applicable with a finding)",
         "detail": "finding files 1; entries added: by=architect: thought: seam/ledger-comments clause '#' has no asserting test: a scratch probe dropping \"#\" from load's startswith left the suite green (8 run, OK); code right",
         "kind": "decide",
         "pass": null
        },
        "AK-S4": {
         "audit": {
          "agree": true,
          "reason": "With a finding written it is not applicable, and the only entry added is a thought line.",
          "verdict": "agrees"
         },
         "desc": "an entry added, not a thought line, says conformance found nothing (not applicable with a finding)",
         "detail": "no entry added but thought lines",
         "kind": "signal",
         "pass": null
        },
        "AK-S5": {
         "audit": {
          "agree": true,
          "reason": "The changed files listed match the evidence's home diff and the clean code root.",
          "verdict": "agrees"
         },
         "desc": "only finding files, design-diff.json, the design, tracked spec files and the state changed",
         "detail": "changed: .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json .orchestrator/changes/json-report/cycles/cycle-1/findings/arch-cycle-1-1.md .orchestrator/changes/json-report/state.json .orchestrator/changes/json-report/state.lock",
         "kind": "decide",
         "pass": true
        },
        "AK-S6": {
         "audit": {
          "agree": true,
          "reason": "The state gained one journal entry and one findings entry, and nothing else changed.",
          "verdict": "agrees"
         },
         "desc": "the state changed only by entries added to the journal and to the findings' index",
         "detail": "other fields changed: none; journal +1, findings index +1; earlier entries kept: True",
         "kind": "decide",
         "pass": true
        },
        "AK-S7": {
         "audit": {
          "agree": true,
          "reason": "The single added journal entry has by=architect.",
          "verdict": "agrees"
         },
         "desc": "each journal entry the run added names the Architect as its writer",
         "detail": "by=architect",
         "kind": "decide",
         "pass": true
        },
        "AK-S8": {
         "audit": {
          "agree": true,
          "reason": "git status shows the worktree clean, no Agent or Task call appears, and tasks/ is untouched.",
          "verdict": "agrees"
         },
         "desc": "no commit, ref or worktree made, no agent spawned, no task file written",
         "detail": "refs and worktrees unchanged: True; spawns: none; task files: none",
         "kind": "decide",
         "pass": true
        },
        "AK-S9": {
         "audit": {
          "agree": true,
          "reason": "Every path named is under the trial's directory <run>/, and there is no mktemp.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AK-E1": "n/a",
        "AK-E2": "pass",
        "AK-E3": "pass",
        "AK-E4": "fail",
        "AK-E5": "pass"
       },
       "judge": {
        "AK-E1": {
         "quotes": [
          "finding_id: arch-cycle-1-1",
          "--- arch-cycle-1-1.md ---",
          "--- arch-cycle-1-1.md ---"
         ],
         "reasons": [
          "The run wrote a finding file, so the clean-journal item does not apply.",
          "The run wrote a finding file, so the clean-journal item does not apply.",
          "The run wrote a finding file, so the clean-conformance journal line does not apply."
         ]
        },
        "AK-E2": {
         "quotes": [
          "conformance cycle-1: 1 findings, 0 rows amended; .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json",
          "conformance cycle-1: 1 findings, 0 rows amended; .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json",
          "conformance cycle-1: 1 findings, 0 rows amended; .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json"
         ],
         "reasons": [
          "design-diff.json is `[]`, and the hand-back is one line under 200 characters of the required form; its count 1 matches the one finding file, which lies outside the diff.",
          "design-diff.json is `[]`, and the hand-back is one line of the required form, well under 200 characters; it counts the one finding file, which lies outside the diff.",
          "design-diff.json is `[]`, and the hand-back is one line of the required form, well under 200 characters. Its count of 1 matches the one finding file, which sits outside the merged diff."
         ]
        },
        "AK-E3": {
         "quotes": [
          "?? .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json\n?? .orchestrator/changes/json-report/cycles/cycle-1/findings/",
          "?? .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json\n?? .orchestrator/changes/json-report/cycles/cycle-1/findings/",
          " M .orchestrator/changes/json-report/state.json\n?? .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json\n?? .orchestrator/changes/json-report/cycles/cycle-1/findings/"
         ],
         "reasons": [
          "Only design-diff.json, the finding file, state.json (journal +1, findings +1) and the lock changed; the code root is clean, there are no commits, Agent calls, tasks or asks, and the probes (probe/, clean/, hash.csv, suite.txt) sit under the scratch directory.",
          "The run changed only the design diff, the finding file, state.json (one journal thought line and one findings entry) and the lock; it made no commit, spawned no agent, wrote no task or ask, and kept its probes (hash.csv, clean/, probe/) under scratch/architect-conformance.",
          "The only writes are design-diff.json, the finding file, and state.json (one journal entry and one findings entry, plus the lock file). There was no commit, Agent call, task or ask, and the probes (hash.csv, probe/, clean/, suite.txt, pytest-v.txt) stayed under scratch/architect-conformance."
         ]
        },
        "AK-E4": {
         "quotes": [
          "title: coverage.py reads this project's unittest run output as \"not run\", pass or fail alike",
          "title: coverage.py reads this project's unittest run output as \"not run\", pass or fail alike",
          "title: coverage.py reads this project's unittest run output as \"not run\", pass or fail alike"
         ],
         "reasons": [
          "The finding carries the labels and the three sections and names a real, reproduced defect in coverage.py (a cleanup went to a thought line), but its title is 85 characters, over the form's 80-character limit.",
          "The finding names a real, reproduced defect in coverage.py (compact and verbose unittest output both read `not run`, while pytest -v reads `pass`), and it carries the labels and three sections; but its title is 85 characters, over the form's 80-character limit.",
          "The finding names a real, reproduced defect: call 9 shows `not run` for both unittest outputs, and call 13 shows `pass` for pytest -v. It also carries the labels and the three sections, and the '#' test gap was kept as a thought line. But its title is 85 characters, over the form's 80-character title limit."
         ]
        },
        "AK-E5": {
         "quotes": [
          "file: skills/orchestrate/scripts/coverage.py:387",
          "- file: skills/orchestrate/scripts/coverage.py:387",
          "- file: skills/orchestrate/scripts/coverage.py:387"
         ],
         "reasons": [
          "The only issue finding is located in the skill's coverage.py, outside the merged diff; the merged load skips both # and ;, and the cited test passes (\"test_semicolon_comment ... ok\").",
          "The only issue finding is located in the skill's coverage.py, outside the merged diff (tally/ledger.py, tests/test_ledger.py); the untested '#' clause was recorded only as a thought line, not as an issue.",
          "No issue-kind finding is located in tally/ledger.py or tests/test_ledger.py. The one finding is in the skill's coverage.py, outside the diff, and the cited scenario was shown passing (`test_semicolon_comment ... ok`)."
         ]
        }
       },
       "milestones": [
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 8 tests in 0.002s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 12,
         "candidates": [
          12
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 8 tests in 0.002s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 18,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Architect's conformance at a cycle's integrate, from its brief to its hand-back",
   "title": "Case architect-conformance-clean: a conformance that finds nothing says so once, writes an empty design diff and names it",
   "trials": 3,
   "validated": "61359a8bd5ab699259b50802504e40b6a34637bd 20261114T123931 t1 (skill-evals)"
  },
  {
   "capability": "architect",
   "id": "architect-conformance-drift",
   "items": [
    {
     "checks": [
      {
       "id": "AC-S4",
       "kind": "decide"
      },
      {
       "id": "AC-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A record correction execute journaled is made by conformance",
      "architect § An amendment never renames a scenario",
      "architect § A row the code has rightly outgrown is amended in place"
     ],
     "id": "AC-E1",
     "question": "Is the stale spec example corrected in place by the test rule, its scenario's title kept?"
    },
    {
     "checks": [
      {
       "id": "AC-S6",
       "kind": "decide"
      },
      {
       "id": "AC-S7",
       "kind": "signal"
      },
      {
       "id": "AC-S22",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § An amendment's finding names the test that pins the code",
      "architect § An amendment cuts implementation detail and never adds it",
      "integrate § A seam row the pinned code has outgrown is corrected in place",
      "integrate § A corrected seam row is cut back to the contract"
     ],
     "id": "AC-E2",
     "question": "Does a row amendment's finding record the amendment and name the pinning test?"
    },
    {
     "checks": [
      {
       "id": "AC-S8",
       "kind": "decide"
      },
      {
       "id": "AC-S9",
       "kind": "decide"
      },
      {
       "id": "AC-S26",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A promise the code does not keep is a finding against the code",
      "integrate § A promise the code does not keep stays in the design"
     ],
     "id": "AC-E3",
     "question": "Is the unkept promise a finding against the code, the promise left in the design?"
    },
    {
     "checks": [
      {
       "id": "AC-S10",
       "kind": "decide"
      },
      {
       "id": "AC-S11",
       "kind": "decide"
      },
      {
       "id": "AC-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Without a pinning test, a disagreement is a finding that puts the design in question",
      "architect § A cited seam the merged code no longer honours is a finding",
      "asks § An unsettled ask is in the change's inbox",
      "integrate § Code that does something else, with no pin, puts the design in question"
     ],
     "id": "AC-E4",
     "question": "Is the untested disagreement a finding that puts the design in question, with no ask and the row left?"
    },
    {
     "checks": [
      {
       "id": "AC-S13",
       "kind": "decide"
      },
      {
       "id": "AC-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A replaced implementation still on a live path is a finding",
      "architect § A finding that breaks a cited seam is of high severity",
      "architect § Every defect is a finding, whatever its likelihood"
     ],
     "id": "AC-E5",
     "question": "Is the replaced rendering on the live path a finding, of high severity when it names the seam it breaks?"
    },
    {
     "checks": [
      {
       "id": "AC-S15",
       "kind": "decide"
      },
      {
       "id": "AC-S16",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A finding is one file in the cycle's findings directory",
      "architect § A finding carries the class of the design checklist",
      "architect § A finding says why the tests missed it and how to resolve it",
      "architect § The Architect writes no route for a finding",
      "reviewer § An Architect's finding is one file in its cycle's directory",
      "reviewer § An Architect's finding carries its labels in its front matter",
      "reviewer § An Architect's finding says where, why the tests missed it and what to do",
      "reviewer § Every finding says what it blocks"
     ],
     "id": "AC-E6",
     "question": "Is each finding one file of the cycle's =findings/=, with a finding's labels and sections, what it blocks, and no route?"
    },
    {
     "checks": [
      {
       "id": "AC-S1",
       "kind": "decide"
      },
      {
       "id": "AC-S2",
       "kind": "decide"
      },
      {
       "id": "AC-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Conformance lists the rows the cycle changed"
     ],
     "id": "AC-E7",
     "question": "Does =design-diff.json= list exactly the rows the cycle changed?"
    },
    {
     "checks": [
      {
       "id": "AC-S17",
       "kind": "decide"
      },
      {
       "id": "AC-S18",
       "kind": "decide"
      },
      {
       "id": "AC-S19",
       "kind": "decide"
      },
      {
       "id": "AC-S20",
       "kind": "decide"
      },
      {
       "id": "AC-S21",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Conformance writes only its findings, its design diff and the text it corrects",
      "architect § No run commits, spawns an agent or writes a task",
      "architect § Probes leave their files in the scratch directory",
      "record § A journal entry names who wrote it"
     ],
     "id": "AC-E8",
     "question": "Does conformance write only its findings, its design diff and the text it corrects, commit nothing, spawn no agent and write no task?"
    },
    {
     "checks": [
      {
       "id": "AC-S23",
       "kind": "decide"
      },
      {
       "id": "AC-S24",
       "kind": "decide"
      },
      {
       "id": "AC-S25",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § Conformance hands back its counts",
      "architect § Conformance's hand-back names its design diff"
     ],
     "id": "AC-E9",
     "question": "Is the hand-back one line of conformance's counts, ending with its design diff?"
    },
    {
     "checks": [],
     "cites": [
      "architect § Style and preference are not reported",
      "architect § A speculative need or a cleanup is a thought line, not a finding"
     ],
     "id": "AC-E10",
     "question": "Is nothing of style or preference reported, and a cleanup only a thought?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "AC-E1": [
       "integrate § A record correction execute journaled is made by conformance",
       "architect § An amendment never renames a scenario",
       "architect § A row the code has rightly outgrown is amended in place"
      ],
      "AC-E10": [
       "architect § Style and preference are not reported",
       "architect § A speculative need or a cleanup is a thought line, not a finding"
      ],
      "AC-E2": [
       "architect § An amendment's finding names the test that pins the code",
       "architect § An amendment cuts implementation detail and never adds it",
       "integrate § A seam row the pinned code has outgrown is corrected in place",
       "integrate § A corrected seam row is cut back to the contract"
      ],
      "AC-E3": [
       "architect § A promise the code does not keep is a finding against the code",
       "integrate § A promise the code does not keep stays in the design"
      ],
      "AC-E4": [
       "architect § Without a pinning test, a disagreement is a finding that puts the design in question",
       "architect § A cited seam the merged code no longer honours is a finding",
       "asks § An unsettled ask is in the change's inbox",
       "integrate § Code that does something else, with no pin, puts the design in question"
      ],
      "AC-E5": [
       "architect § A replaced implementation still on a live path is a finding",
       "architect § A finding that breaks a cited seam is of high severity",
       "architect § Every defect is a finding, whatever its likelihood"
      ],
      "AC-E6": [
       "architect § A finding is one file in the cycle's findings directory",
       "architect § A finding carries the class of the design checklist",
       "architect § A finding says why the tests missed it and how to resolve it",
       "architect § The Architect writes no route for a finding",
       "reviewer § An Architect's finding is one file in its cycle's directory",
       "reviewer § An Architect's finding carries its labels in its front matter",
       "reviewer § An Architect's finding says where, why the tests missed it and what to do",
       "reviewer § Every finding says what it blocks"
      ],
      "AC-E7": [
       "architect § Conformance lists the rows the cycle changed"
      ],
      "AC-E8": [
       "architect § Conformance writes only its findings, its design diff and the text it corrects",
       "architect § No run commits, spawns an agent or writes a task",
       "architect § Probes leave their files in the scratch directory",
       "record § A journal entry names who wrote it"
      ],
      "AC-E9": [
       "architect § Conformance hands back its counts",
       "architect § Conformance's hand-back names its design diff"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AC-E1": [
       "AC-S4",
       "AC-S5"
      ],
      "AC-E10": [],
      "AC-E2": [
       "AC-S22",
       "AC-S6",
       "AC-S7"
      ],
      "AC-E3": [
       "AC-S26",
       "AC-S8",
       "AC-S9"
      ],
      "AC-E4": [
       "AC-S10",
       "AC-S11",
       "AC-S12"
      ],
      "AC-E5": [
       "AC-S13",
       "AC-S14"
      ],
      "AC-E6": [
       "AC-S15",
       "AC-S16"
      ],
      "AC-E7": [
       "AC-S1",
       "AC-S2",
       "AC-S3"
      ],
      "AC-E8": [
       "AC-S17",
       "AC-S18",
       "AC-S19",
       "AC-S20",
       "AC-S21"
      ],
      "AC-E9": [
       "AC-S23",
       "AC-S24",
       "AC-S25"
      ]
     },
     "items_then": [],
     "run": "20261114T122820-1",
     "trials": [
      {
       "calls": {
        "15": {
         "error": false,
         "text": "S=<trial>/scratch/architect-conformance; mkdir -p $S; W=<trial>/home/.worktrees/tally/json-report; C=<trial>/skills/orchestrate/scripts/coverage.py; cd $W && python3 -m unittest discover -s tests -t . -v > $S/verbose.txt 2>&1; cat $S/verbose.txt; python3 $C --root $W --output $S/verbose.txt --scenario 'reporting § The JSON report maps each category to its total' --scenario 'reporting § A comment line is skipped'; python3 -m pytest --version 2>&1 | head -1; git -C $W status --sho … [cut, 602 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "AC-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists and is a JSON list ([]).",
          "verdict": "agrees"
         },
         "desc": "design-diff.json in the cycle's directory, a JSON list",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-1/design-diff.json: list",
         "kind": "decide",
         "pass": true
        },
        "AC-S10": {
         "audit": {
          "agree": true,
          "reason": "As a precondition it correctly finds table-amount findings, including the width one, arch-cycle-1-4.",
          "verdict": "agrees"
         },
         "desc": "a finding about the row the code disagrees with, untested",
         "detail": "arch-cycle-1-2.md arch-cycle-1-3.md arch-cycle-1-4.md arch-cycle-1-7.md",
         "kind": "decide",
         "pass": true
        },
        "AC-S11": {
         "audit": {
          "agree": true,
          "reason": "The table-amount row still says twelve.",
          "verdict": "agrees"
         },
         "desc": "that row is as it was",
         "detail": "| seam/table-amount    | The table writes each amount with a dollar sign before it, right-aligned in a column twelve characters wide       | =tally/report.py=: =format_table=                        |                                                              | =tests/test_report.py= |",
         "kind": "decide",
         "pass": true
        },
        "AC-S12": {
         "audit": {
          "agree": true,
          "reason": "No ask file appears among the changed files.",
          "verdict": "agrees"
         },
         "desc": "no ask file written",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AC-S13": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-1-1, -2 and -3 are located at report.py lines 18 and 26, the _plain lines.",
          "verdict": "agrees"
         },
         "desc": "a finding located at the replaced rendering or its call on the live path",
         "detail": "tally/report.py lines [17, 18, 26]; findings there: arch-cycle-1-1.md arch-cycle-1-2.md arch-cycle-1-3.md",
         "kind": "decide",
         "pass": true
        },
        "AC-S14": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-1-1, the one naming seam/json-report about the second producer, is severity high.",
          "verdict": "agrees"
         },
         "desc": "a finding there that names the JSON report's seam is of high severity (not applicable without one)",
         "detail": "arch-cycle-1-1.md: severity high",
         "kind": "decide",
         "pass": true
        },
        "AC-S15": {
         "audit": {
          "agree": true,
          "reason": "Its fail is borne out by the over-80-character titles of arch-cycle-1-4 and -8, which the item forbids.",
          "verdict": "agrees"
         },
         "desc": "each finding file: the labels, trigger conformance, class from the checklist, title at most 80, discovered_by architect, blocks, no decoration, the three sections",
         "detail": "arch-cycle-1-3.md: a location without a line (- file: docs/changes/json-report/intent.org (G2)); arch-cycle-1-4.md: title of 85 characters; arch-cycle-1-4.md: a location without a line (- file: docs/changes/json-report/design.org (Seams, seam/tab); arch-cycle-1-5.md: a location without a line (- fil … [cut, 507 characters]",
         "kind": "decide",
         "pass": false
        },
        "AC-S16": {
         "audit": {
          "agree": true,
          "reason": "All nine files are named arch-cycle-1-<n>.md with a matching finding_id.",
          "verdict": "agrees"
         },
         "desc": "each finding file is arch-<cycle-id>-<n>.md, its finding_id its name",
         "detail": "arch-cycle-1-1.md arch-cycle-1-2.md arch-cycle-1-3.md arch-cycle-1-4.md arch-cycle-1-5.md arch-cycle-1-6.md arch-cycle-1-7.md arch-cycle-1-8.md arch-cycle-1-9.md",
         "kind": "decide",
         "pass": true
        },
        "AC-S17": {
         "audit": {
          "agree": true,
          "reason": "The changed files are findings, design-diff, state.json, the lock and reporting.org, all allowed.",
          "verdict": "agrees"
         },
         "desc": "only finding files, design-diff.json, the design, tracked spec files and the state changed",
         "detail": "changed: .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json .orchestrator/changes/json-report/cycles/cycle-1/findings/arch-cycle-1-1.md .orchestrator/changes/json-report/cycles/cycle-1/findings/arch-cycle-1-2.md .orchestrator/changes/json-report/cycles/cycle-1/findings/arch-cycle-1-3. … [cut, 902 characters]",
         "kind": "decide",
         "pass": true
        },
        "AC-S18": {
         "audit": {
          "agree": true,
          "reason": "Only journal +2 and findings +9 entries were appended; no other field changed.",
          "verdict": "agrees"
         },
         "desc": "the state changed only by entries added to the journal and to the findings' index",
         "detail": "other fields changed: none; journal +2, findings index +9; earlier entries kept: True",
         "kind": "decide",
         "pass": true
        },
        "AC-S19": {
         "audit": {
          "agree": true,
          "reason": "Both added journal entries have by=architect.",
          "verdict": "agrees"
         },
         "desc": "each journal entry the run added names the Architect as its writer",
         "detail": "by=architect: thought: seam/json-report names cli.main and the scenario's WHEN is 'tally FILE --json', but no test drives the --json f; by=architect: thought: seam/ledger-comments says a line starting with # or ; is a comment; test_ledger asserts only the ; case, and on",
         "kind": "decide",
         "pass": true
        },
        "AC-S2": {
         "audit": {
          "agree": true,
          "reason": "The list has zero elements, so the check passes trivially and correctly.",
          "verdict": "agrees"
         },
         "desc": "each element names a seam row, added, amended or retired, and a finding or ask id",
         "detail": "0 element(s); without the three: []",
         "kind": "decide",
         "pass": true
        },
        "AC-S20": {
         "audit": {
          "agree": true,
          "reason": "The tool calls show no Agent call, no commit and no task-file edits.",
          "verdict": "agrees"
         },
         "desc": "no commit, ref or worktree made, no agent spawned, no task file written",
         "detail": "refs and worktrees unchanged: True; spawns: none; task files: none",
         "kind": "decide",
         "pass": true
        },
        "AC-S21": {
         "audit": {
          "agree": true,
          "reason": "All paths are within the trial directory and no mktemp is used; the probe tempfiles are created in scratch via dir=.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AC-S22": {
         "audit": {
          "agree": true,
          "reason": "No row changed, so not applicable is correct.",
          "verdict": "agrees"
         },
         "desc": "an amended row's statement names no private helper and no test (not applicable with no row amended)",
         "detail": "rows changed []",
         "kind": "decide",
         "pass": null
        },
        "AC-S23": {
         "audit": {
          "agree": true,
          "reason": "The hand-back ends with an absolute path rather than design-diff.json's path from the home, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line, conformance <cycle-id>: <f> findings, <a> rows amended; ... ending with design-diff.json's path from the home, at most 200 characters",
         "detail": "conformance cycle-1: 9 findings, 0 rows amended; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/design-diff.json",
         "kind": "decide",
         "pass": false
        },
        "AC-S24": {
         "audit": {
          "agree": true,
          "reason": "The 9 in the hand-back matches the 9 finding files.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's findings count the cycle's finding files (not applicable without the line)",
         "detail": "hand-back 9; files 9",
         "kind": "decide",
         "pass": true
        },
        "AC-S25": {
         "audit": {
          "agree": true,
          "reason": "The 0 in the hand-back matches the zero amended elements.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's rows amended count the list's amended elements (not applicable without the line)",
         "detail": "hand-back 0; amended elements 0",
         "kind": "signal",
         "pass": true
        },
        "AC-S26": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-1-5 names D2 and the zero total.",
          "verdict": "agrees"
         },
         "desc": "a finding names D2 or a zero total",
         "detail": "arch-cycle-1-3.md arch-cycle-1-5.md",
         "kind": "signal",
         "pass": true
        },
        "AC-S3": {
         "audit": {
          "agree": true,
          "reason": "No row changed in design.org and none is listed.",
          "verdict": "agrees"
         },
         "desc": "the rows the list names are the rows whose line changed in the design",
         "detail": "listed []; changed in design.org []",
         "kind": "decide",
         "pass": true
        },
        "AC-S4": {
         "audit": {
          "agree": true,
          "reason": "The spec diff touches only THEN steps, so every title is unchanged.",
          "verdict": "agrees"
         },
         "desc": "every scenario title in docs/specs as it was",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "AC-S5": {
         "audit": {
          "agree": true,
          "reason": "The amended THEN step contains $900.00.",
          "verdict": "agrees"
         },
         "desc": "the JSON scenario's steps give the amounts as the pinning test has them ($900.00)",
         "detail": "- *WHEN* =tally FILE --json= is run on a ledger with food 13.05 and rent 900.00\n- *THEN* it prints one JSON object ={\"rent\": \"$900.00\", \"food\": \"$13.05\"}=, keys in the table's order\n- *AND* each amount is a string written as the table writes it",
         "kind": "decide",
         "pass": true
        },
        "AC-S6": {
         "audit": {
          "agree": true,
          "reason": "No row changed, so not applicable is correct.",
          "verdict": "agrees"
         },
         "desc": "when a seam row was amended, a finding names the test that pins the code (not applicable with no row amended)",
         "detail": "rows changed []; findings naming tests/test_json.py: arch-cycle-1-1.md arch-cycle-1-6.md",
         "kind": "decide",
         "pass": null
        },
        "AC-S7": {
         "audit": {
          "agree": true,
          "reason": "Not applicable with no row amended fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "a finding names the pinning test (not applicable with no row amended)",
         "detail": "arch-cycle-1-1.md arch-cycle-1-6.md",
         "kind": "signal",
         "pass": null
        },
        "AC-S8": {
         "audit": {
          "agree": true,
          "reason": "design.org is unchanged and still holds D2.",
          "verdict": "agrees"
         },
         "desc": "the promise the code does not keep is still in the design",
         "detail": "- D2. A category whose total is zero",
         "kind": "decide",
         "pass": true
        },
        "AC-S9": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-1-5 is located at report.py:5 and :12, the D2 finding the item wants.",
          "verdict": "agrees"
         },
         "desc": "a finding located at the code that keeps a zero total (the report's functions, the replaced rendering's call aside)",
         "detail": "tally/report.py lines [5, 6, 7, 8, 9, 12, 13, 14, 25, 27, 28, 31, 32, 33]; findings there: arch-cycle-1-1.md arch-cycle-1-4.md arch-cycle-1-5.md",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AC-E1": "pass",
        "AC-E10": "pass",
        "AC-E2": "n/a",
        "AC-E3": "pass",
        "AC-E4": "pass",
        "AC-E5": "pass",
        "AC-E6": "fail",
        "AC-E7": "pass",
        "AC-E8": "pass",
        "AC-E9": "fail"
       },
       "judge": {
        "AC-E1": {
         "quotes": [
          "+- *THEN* it prints one JSON object ={\"rent\": \"$900.00\", \"food\": \"$13.05\"}=, keys in the table's order",
          "+- *THEN* it prints one JSON object ={\"rent\": \"$900.00\", \"food\": \"$13.05\"}=, keys in the table's order",
          "+- *THEN* it prints one JSON object ={\"rent\": \"$900.00\", \"food\": \"$13.05\"}=, keys in the table's order"
         ],
         "reasons": [
          "The JSON scenario's THEN step was amended in place to the table's amounts, which tests/test_json.py pins; no title changed; no seam row had been outgrown under a pinning test, so none needed amending.",
          "The JSON scenario's THEN step was corrected in place to the amounts test_json pins, with the title unchanged; no seam row had a pinning test that called for an amendment.",
          "The JSON scenario's THEN step was corrected in place, and its title is unchanged. `tests/test_json.py::JsonReport::test_scenario_example` pins `$900.00`. No scenario was renamed, and no seam row had a pinning test that called for amendment."
         ]
        },
        "AC-E10": {
         "quotes": [
          "thought: seam/json-report names cli.main ... but no test drives the --json flag through main",
          "thought: seam/ledger-comments says a line starting with # or ; is a comment; test_ledger asserts only the ; case",
          "thought: seam/json-report names cli.main ... no test drives the --json flag through main"
         ],
         "reasons": [
          "No finding or thought is about naming or formatting; the two thought lines note test gaps on code that is right today, and _plain is treated as a behaviour finding.",
          "No finding or thought is about naming or formatting, and the _plain live path is reported as a behaviour defect rather than a cleanup.",
          "No finding or thought is about naming, formatting or equivalent structure. The two thoughts are test gaps where the code is right today, and the findings name behaviour present today."
         ]
        },
        "AC-E2": {
         "quotes": [
          "conformance cycle-1: 9 findings, 0 rows amended",
          "conformance cycle-1: 9 findings, 0 rows amended",
          "rows changed []"
         ],
         "reasons": [
          "No seam row was amended; design.org is unchanged and only spec steps were corrected.",
          "No seam row was amended, since design.org is unchanged and design-diff.json is [].",
          "No seam row was amended; `design.org` is not in the code root's diff."
         ]
        },
        "AC-E3": {
         "quotes": [
          "title: D2 promises zero totals are left out; the table and JSON both list \"$0.00\" ... - file: tally/report.py:5 ... - file: tally/report.py:12",
          "- file: tally/report.py:5 ... - file: tally/report.py:12 ... D2 promises zero totals are left out; the table and JSON both list \"$0.00\"",
          "title: D2 promises zero totals are left out; the table and JSON both list \"$0.00\" ... - file: tally/report.py:5 ... - file: tally/report.py:12"
         ],
         "reasons": [
          "arch-cycle-1-5 is a finding against the code, located in report.py, and design.org (untouched) still holds D2; its recommendation to retire D2 is offered as a decision ask, not made.",
          "arch-cycle-1-5 is a finding located in the report's code, and the design.org diff is empty, so D2 stays in the design.",
          "arch-cycle-1-5 is an issue against the code, located in `tally/report.py`. `design.org` is untouched, so D2 stays in the design."
         ]
        },
        "AC-E4": {
         "quotes": [
          "No test pins either reading, so this is Design in question: the orchestrator writes a =decision= ask",
          "No test pins either reading, so this is Design in question ... overflowing the field and breaking right-alignment",
          "The amount column is ten characters, not the row's twelve ... No test pins either reading, so this is Design in question"
         ],
         "reasons": [
          "arch-cycle-1-4 (kind issue, with probe evidence of the misalignment the seam's right-aligned column is for) puts the width in question, the row still says twelve, and no ask file was written.",
          "arch-cycle-1-4 is an issue with probe evidence and puts the design in question; the seam/table-amount row is left saying twelve, and no ask file was written.",
          "arch-cycle-1-4 is an issue with probe evidence. It names the seam's purpose: right-alignment, with amounts ending in the same column. The row still says twelve, and no ask file was written."
         ]
        },
        "AC-E5": {
         "quotes": [
          "title: A negative total is \"$-13.95\" in the JSON report but \"-13.95\" in the table ... severity: high ... seam: seam/json-report",
          "severity: high ... A negative total is \"$-13.95\" in the JSON report but \"-13.95\" in the table ... seam: seam/json-report",
          "A negative total is \"$-13.95\" in the JSON report but \"-13.95\" in the table ... severity: high ... seam: seam/json-report"
         ],
         "reasons": [
          "The _plain branch on format_table's negative path is a finding naming the broken json-report seam at severity high, with arch-cycle-1-3 also covering it against table-amount.",
          "The _plain path in format_table is a finding (arch-cycle-1-1, also arch-cycle-1-3); the one that says the two reports diverge on seam/json-report is high severity.",
          "The `_plain` branch on the negative path of `format_table` is a finding. The one saying the table and JSON render a refund differently, which breaks `seam/json-report`, has severity high."
         ]
        },
        "AC-E6": {
         "quotes": [
          "title: The amount column is ten characters, not the row's twelve; $100000.00 and up misalign",
          "title: The amount column is ten characters, not the row's twelve; $100000.00 and up misalign",
          "arch-cycle-1-4.md: title of 85 characters; arch-cycle-1-8.md: title of 89 characters"
         ],
         "reasons": [
          "Titles of arch-cycle-1-4 (85 chars) and arch-cycle-1-8 (89 chars) exceed the 80-character limit, and several locations give no line.",
          "The titles of arch-cycle-1-4 (85 characters) and arch-cycle-1-8 (89 characters) exceed the 80-character limit.",
          "Two finding titles exceed the 80-character limit, even though files, ids, labels, sections, `blocks` and the absence of `decoration` are otherwise right."
         ]
        },
        "AC-E7": {
         "quotes": [
          "== design-diff.json in the cycle's directory ==\n[]",
          "== design-diff.json in the cycle's directory ==\n[]",
          "== design-diff.json in the cycle's directory ==\n[]"
         ],
         "reasons": [
          "No seam row was added, amended or retired, and the list is empty.",
          "No seam row changed, and the list is empty.",
          "No seam row was added, amended or retired, and the list is empty."
         ]
        },
        "AC-E8": {
         "quotes": [
          "refs and worktrees unchanged ... journal +2, findings index +9 ... \"by\": \"architect\"",
          "Other files added, changed or removed in the home since the build ==\n--- .orchestrator/changes/json-report/state.lock ---",
          "M docs/specs/reporting.org"
         ],
         "reasons": [
          "Only finding files, design-diff.json, the spec steps, two architect thought lines and nine index entries were written; there was no commit, agent or task, and the probes (probe.py, bom.csv) sit in scratch. One unittest -v run was made in the worktree without PYTHONDONTWRITEBYTECODE, but the evidence does not show it left anything the build lacked.",
          "Only the finding files, design-diff.json, the two spec sentences and state entries (two thought notes by architect, nine finding indexes) changed; there was no commit, no agent and no task, and the probes were in scratch.",
          "Nothing else changed outside the expected writes: finding files, `design-diff.json`, the two spec steps it corrects, two thought lines by architect, and nine index entries. There is no commit, no Agent call, no task file, and probes are in scratch."
         ]
        },
        "AC-E9": {
         "quotes": [
          "conformance cycle-1: 9 findings, 0 rows amended; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/design-diff.json",
          "conformance cycle-1: 9 findings, 0 rows amended; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/design-diff.json",
          "conformance cycle-1: 9 findings, 0 rows amended; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/design-diff.json"
         ],
         "reasons": [
          "The counts are right, but the line ends with an absolute path rather than '; .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json' as the form requires.",
          "The counts are right, but the line ends with an absolute path rather than the required home-relative '; .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json'.",
          "The line ends with the absolute path instead of `; .orchestrator/changes/json-report/cycles/cycle-1/design-diff.json` from the home."
         ]
        }
       },
       "milestones": [
        {
         "call": 15,
         "candidates": [
          15
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 6 tests in 0.002s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 29,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Architect's conformance at a cycle's integrate, from its brief to its hand-back",
   "title": "Case architect-conformance-drift: conformance corrects what a test pins, reports a promise the code does not keep, an untested disagreement and a replaced rendering left live",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T122820-1 t1 (skill-evals)"
  },
  {
   "capability": "architect",
   "id": "architect-ns6",
   "items": [
    {
     "checks": [
      {
       "id": "AR-S1",
       "kind": "decide"
      },
      {
       "id": "AR-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The design round writes the change's design beside its intent",
      "architect § The Context describes the code the change starts from"
     ],
     "id": "AR-E1",
     "question": "Is the change's design written beside its intent, with a Context, Decisions and Seams, and does its Context describe the code at the base?"
    },
    {
     "checks": [
      {
       "id": "AR-S2",
       "kind": "decide"
      },
      {
       "id": "AR-S17",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The Seams table has the design's five columns",
      "architect § A seam row has a unique id and names its scenarios and tests"
     ],
     "id": "AR-E2",
     "question": "Does the Seams table have the design's five columns, one row per seam, each with a unique id and its scenarios and tests?"
    },
    {
     "checks": [
      {
       "id": "AR-S3",
       "kind": "decide"
      },
      {
       "id": "AR-S13",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A seam row's statement names no implementation detail",
      "architect § A Decision names no implementation detail"
     ],
     "id": "AR-E3",
     "question": "Does each seam statement and each Decision state what must hold, naming no guard, error string, encoding, helper or test, nor a transient marker in a row?"
    },
    {
     "checks": [
      {
       "id": "AR-S4",
       "kind": "decide"
      },
      {
       "id": "AR-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Owning symbols are the boundary's entry points",
      "architect § A symbol the design introduces is named without a marker"
     ],
     "id": "AR-E4",
     "question": "Are the owning symbols the boundary's entry points, named without a line number, and a symbol the design introduces named without a marker?"
    },
    {
     "checks": [
      {
       "id": "AR-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A seam row holds the current state only"
     ],
     "id": "AR-E5",
     "question": "Does each seam row hold the current state only?"
    },
    {
     "checks": [],
     "cites": [
      "architect § A seam is a boundary between two parts of the code, not a scenario restated",
      "architect § Each seam serves a scenario the goals need"
     ],
     "id": "AR-E6",
     "question": "Is each seam a boundary between two parts of the code, serving a scenario the goals need?"
    },
    {
     "checks": [
      {
       "id": "AR-S18",
       "kind": "decide"
      },
      {
       "id": "AR-S22",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A Decision that names a seam links to it by id",
      "architect § A Decision that replaces a mechanism accounts for what the old one gave",
      "architect § A property dropped on purpose is marked dropped in its Decision"
     ],
     "id": "AR-E7",
     "question": "Does each Decision that names a seam carry its id, and does a Decision that replaces a mechanism account for what the old one gave?"
    },
    {
     "checks": [
      {
       "id": "AR-S8",
       "kind": "decide"
      },
      {
       "id": "AR-S15",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Every scenario of the change is cited by a test or marked none",
      "architect § A scenario the code lacks gets a pending test that cites it",
      "architect § A scenario with no test worth writing says why"
     ],
     "id": "AR-E8",
     "question": "Is each of the change's scenarios cited by a test, the two the code lacks by pending tests that cite them?"
    },
    {
     "checks": [
      {
       "id": "AR-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A pending test is reported as pending, never as failing"
     ],
     "id": "AR-E9",
     "question": "Are the pending tests reported as pending, and does the suite still pass?"
    },
    {
     "checks": [],
     "cites": [
      "architect § A new test sits with the tests of the module it drives",
      "architect § A pending test drives real code"
     ],
     "id": "AR-E10",
     "question": "Do the new tests sit with the tests of the module they drive, and drive real code?"
    },
    {
     "checks": [
      {
       "id": "AR-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § An open question is an entry in the design's Open Questions",
      "architect § An open question's rows and tests are drafted in the recommended form",
      "architect § A question about a case nobody raised is not an open question",
      "asks § A question for the user is one file, written when it is raised",
      "design-round § The round's phase agent files the asks and hands back stopped"
     ],
     "id": "AR-E11",
     "question": "Is any question only the user can settle an entry of the design's Open Questions, written for the user and never decided, and is no ask file written?"
    },
    {
     "checks": [
      {
       "id": "AR-S10",
       "kind": "decide"
      },
      {
       "id": "AR-S5",
       "kind": "decide"
      },
      {
       "id": "AR-S19",
       "kind": "decide"
      },
      {
       "id": "AR-S23",
       "kind": "decide"
      },
      {
       "id": "AR-S20",
       "kind": "signal"
      },
      {
       "id": "AR-S21",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A defect found at the design round is a finding of the change, outside any cycle",
      "architect § A need the code has today is an issue, never a thought",
      "architect § A finding carries the class of the design checklist",
      "architect § A finding says why the tests missed it and how to resolve it",
      "architect § The Architect writes no route for a finding",
      "reviewer § An Architect's finding carries its labels in its front matter",
      "reviewer § An Architect's finding is one file in its cycle's directory",
      "reviewer § An Architect's finding says where, why the tests missed it and what to do",
      "reviewer § Every finding says what it blocks",
      "record § The change's directory holds what the design round leaves before any cycle"
     ],
     "id": "AR-E12",
     "question": "Is the refund's wrong amount in the existing code, if the Architect finds it, a finding of kind issue in the change's findings, and not an open question?"
    },
    {
     "checks": [],
     "cites": [
      "architect § A design-round thought goes to the file its brief names",
      "architect § Style and preference are not reported"
     ],
     "id": "AR-E13",
     "question": "Is a speculative need or a cleanup the Architect notices a thought line in its scratch directory, and nothing it would only have written differently by taste reported?"
    },
    {
     "checks": [
      {
       "id": "AR-S5",
       "kind": "decide"
      },
      {
       "id": "AR-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The design round writes only the design, the tests and the test properties",
      "design-round § The round writes no task and implements nothing"
     ],
     "id": "AR-E14",
     "question": "Does the round write only the design, the tests and the scenarios' test properties, and implement nothing?"
    },
    {
     "checks": [
      {
       "id": "AR-S6",
       "kind": "decide"
      },
      {
       "id": "AR-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § No run commits, spawns an agent or writes a task",
      "design-round § The Architect makes no commit",
      "design-round § The Architect works in the code root"
     ],
     "id": "AR-E15",
     "question": "Does the Architect work in the code root, commit nothing, spawn no agent and write no task?"
    },
    {
     "checks": [
      {
       "id": "AR-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Probes leave their files in the scratch directory"
     ],
     "id": "AR-E16",
     "question": "Do the Architect's test runs and probes leave their files in the scratch directory its brief names?"
    },
    {
     "checks": [
      {
       "id": "AR-S9",
       "kind": "decide"
      },
      {
       "id": "AR-S16",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The design round hands back its counts"
     ],
     "id": "AR-E17",
     "question": "Does the round hand back one line with its counts?"
    }
   ],
   "runs": {
    "main": {
     "case_commit": "",
     "cites": {},
     "commit": "e356da7",
     "item_checks": {},
     "items_then": [],
     "run": "20261112T120939",
     "trials": [
      {
       "calls": {},
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [],
       "n_calls": 0,
       "trial": 1
      },
      {
       "calls": {},
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [],
       "n_calls": 0,
       "trial": 2
      },
      {
       "calls": {},
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [],
       "n_calls": 0,
       "trial": 3
      }
     ],
     "validation": false
    },
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "AR-E1": [
       "architect § The design round writes the change's design beside its intent",
       "architect § The Context describes the code the change starts from"
      ],
      "AR-E10": [
       "architect § A new test sits with the tests of the module it drives",
       "architect § A pending test drives real code"
      ],
      "AR-E11": [
       "architect § An open question is an entry in the design's Open Questions",
       "architect § An open question's rows and tests are drafted in the recommended form",
       "architect § A question about a case nobody raised is not an open question",
       "asks § A question for the user is one file, written when it is raised",
       "design-round § The round's phase agent files the asks and hands back stopped"
      ],
      "AR-E12": [
       "architect § A defect found at the design round is a finding of the change, outside any cycle",
       "architect § A need the code has today is an issue, never a thought",
       "architect § A finding carries the class of the design checklist",
       "architect § A finding says why the tests missed it and how to resolve it",
       "architect § The Architect writes no route for a finding",
       "reviewer § An Architect's finding carries its labels in its front matter",
       "reviewer § An Architect's finding is one file in its cycle's directory",
       "reviewer § An Architect's finding says where, why the tests missed it and what to do",
       "reviewer § Every finding says what it blocks",
       "record § The change's directory holds what the design round leaves before any cycle"
      ],
      "AR-E13": [
       "architect § A design-round thought goes to the file its brief names",
       "architect § Style and preference are not reported"
      ],
      "AR-E14": [
       "architect § The design round writes only the design, the tests and the test properties",
       "design-round § The round writes no task and implements nothing"
      ],
      "AR-E15": [
       "architect § No run commits, spawns an agent or writes a task",
       "design-round § The Architect makes no commit",
       "design-round § The Architect works in the code root"
      ],
      "AR-E16": [
       "architect § Probes leave their files in the scratch directory"
      ],
      "AR-E17": [
       "architect § The design round hands back its counts"
      ],
      "AR-E2": [
       "architect § The Seams table has the design's five columns",
       "architect § A seam row has a unique id and names its scenarios and tests"
      ],
      "AR-E3": [
       "architect § A seam row's statement names no implementation detail",
       "architect § A Decision names no implementation detail"
      ],
      "AR-E4": [
       "architect § Owning symbols are the boundary's entry points",
       "architect § A symbol the design introduces is named without a marker"
      ],
      "AR-E5": [
       "architect § A seam row holds the current state only"
      ],
      "AR-E6": [
       "architect § A seam is a boundary between two parts of the code, not a scenario restated",
       "architect § Each seam serves a scenario the goals need"
      ],
      "AR-E7": [
       "architect § A Decision that names a seam links to it by id",
       "architect § A Decision that replaces a mechanism accounts for what the old one gave",
       "architect § A property dropped on purpose is marked dropped in its Decision"
      ],
      "AR-E8": [
       "architect § Every scenario of the change is cited by a test or marked none",
       "architect § A scenario the code lacks gets a pending test that cites it",
       "architect § A scenario with no test worth writing says why"
      ],
      "AR-E9": [
       "architect § A pending test is reported as pending, never as failing"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AR-E1": [
       "AR-S1",
       "AR-S12"
      ],
      "AR-E10": [],
      "AR-E11": [
       "AR-S10"
      ],
      "AR-E12": [
       "AR-S10",
       "AR-S19",
       "AR-S20",
       "AR-S21",
       "AR-S23",
       "AR-S5"
      ],
      "AR-E13": [],
      "AR-E14": [
       "AR-S10",
       "AR-S5"
      ],
      "AR-E15": [
       "AR-S10",
       "AR-S6"
      ],
      "AR-E16": [
       "AR-S11"
      ],
      "AR-E17": [
       "AR-S16",
       "AR-S9"
      ],
      "AR-E2": [
       "AR-S17",
       "AR-S2"
      ],
      "AR-E3": [
       "AR-S13",
       "AR-S3"
      ],
      "AR-E4": [
       "AR-S14",
       "AR-S4"
      ],
      "AR-E5": [
       "AR-S14"
      ],
      "AR-E6": [],
      "AR-E7": [
       "AR-S18",
       "AR-S22"
      ],
      "AR-E8": [
       "AR-S15",
       "AR-S8"
      ],
      "AR-E9": [
       "AR-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T123104",
     "trials": [
      {
       "calls": {},
       "checks": {
        "AR-S1": {
         "audit": {
          "agree": true,
          "reason": "It confirms that design.org exists beside intent.org, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "design.org written beside the intent",
         "detail": "docs/changes/json-report/design.org",
         "kind": "decide",
         "pass": true
        },
        "AR-S10": {
         "audit": {
          "agree": true,
          "reason": "Only finding .md files were added under .orchestrator, directly in findings/.",
          "verdict": "agrees"
         },
         "desc": "in the change's directory, only finding files added directly in findings/; the brief as it was",
         "detail": "findings: arch-json-report-1.md arch-json-report-2.md arch-json-report-3.md arch-json-report-4.md arch-json-report-5.md",
         "kind": "decide",
         "pass": true
        },
        "AR-S11": {
         "audit": {
          "agree": true,
          "reason": "No call names /tmp or mktemp; the check scopes out the tempfile written by the tests, which the eval agent judged under AR-E16.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "AR-S12": {
         "audit": {
          "agree": true,
          "reason": "The headings and their order match the design.",
          "verdict": "agrees"
         },
         "desc": "Context, Decisions and Seams sections, Decisions before Seams",
         "detail": "top-level headings: ['Context', 'Decisions', 'Seams']",
         "kind": "decide",
         "pass": true
        },
        "AR-S13": {
         "audit": {
          "agree": true,
          "reason": "None of the wider-net terms appears in the statements or Decisions.",
          "verdict": "agrees"
         },
         "desc": "the wider net: no exit status literal, encoding, mechanism or marker word in a statement or Decision",
         "detail": "",
         "kind": "signal",
         "pass": true
        },
        "AR-S14": {
         "audit": {
          "agree": true,
          "reason": "No row carries (new), 'pending' or a date.",
          "verdict": "agrees"
         },
         "desc": "no row carries a transient marker or a date",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "AR-S15": {
         "audit": {
          "agree": true,
          "reason": "The citing tests are expectedFailure and are reported as expected failures.",
          "verdict": "agrees"
         },
         "desc": "each scenario not marked none has a pending test citing it, reported as an expected failure (not applicable when both are marked none)",
         "detail": "The JSON report maps each category to its total: Report.test_json_has_the_tables_order_and_amounts, Cli.test_json_maps_each_category_to_its_total; An unknown option is refused: Cli.test_unknown_option_is_refused, Cli.test_unknown_option_before_the_file_is_not_read_as_a_file",
         "kind": "decide",
         "pass": true
        },
        "AR-S16": {
         "audit": {
          "agree": true,
          "reason": "The counts match the rows, the expected failures and the uncited count.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's seams, pending tests and uncited scenarios agree with the end state (not applicable without the line)",
         "detail": "hand-back 2 seams, 4 pending, 0 uncited; end state 2 rows, 4 expected failures, 0 uncited",
         "kind": "decide",
         "pass": true
        },
        "AR-S17": {
         "audit": {
          "agree": true,
          "reason": "The ids are unique, the scenario titles are exact and the cited test files exist.",
          "verdict": "agrees"
         },
         "desc": "each row: a unique seam/<name> id, scenarios cited by their exact titles, test files that exist",
         "detail": "2 row(s) well formed",
         "kind": "decide",
         "pass": true
        },
        "AR-S18": {
         "audit": {
          "agree": true,
          "reason": "Each Decision heading carries the seam id its body names; the mechanism-accounting half of AR-E7 is left to the eval agent.",
          "verdict": "agrees"
         },
         "desc": "each Decision that names a seam carries its id in its heading (not applicable with no Decision)",
         "detail": "2 Decision(s)",
         "kind": "decide",
         "pass": true
        },
        "AR-S19": {
         "audit": {
          "agree": true,
          "reason": "arch-json-report-1's title is indeed 90 characters, over the form's 80.",
          "verdict": "agrees"
         },
         "desc": "each finding file carries a finding's labels and sections, and no decoration (not applicable with no finding file)",
         "detail": "arch-json-report-1.md: title of 90 characters",
         "kind": "decide",
         "pass": false
        },
        "AR-S2": {
         "audit": {
          "agree": true,
          "reason": "The header and the seam/ rows match the table.",
          "verdict": "agrees"
         },
         "desc": "Seams table in the design's five columns, one row or more",
         "detail": "columns ['id', 'statement', 'owning symbols', 'scenarios', 'tests']; 2 row(s)",
         "kind": "decide",
         "pass": true
        },
        "AR-S20": {
         "audit": {
          "agree": true,
          "reason": "arch-json-report-1 reports the refund defect; matching file 4 on 'negative' is incidental and does not change the verdict.",
          "verdict": "agrees"
         },
         "desc": "a finding of kind issue names the refund's amount",
         "detail": ".orchestrator/changes/json-report/findings/arch-json-report-1.md .orchestrator/changes/json-report/findings/arch-json-report-4.md",
         "kind": "signal",
         "pass": true
        },
        "AR-S21": {
         "audit": {
          "agree": true,
          "reason": "The design has no Open Questions section.",
          "verdict": "agrees"
         },
         "desc": "the design's Open Questions name nothing of the refund's amount",
         "detail": "no Open Questions section",
         "kind": "signal",
         "pass": true
        },
        "AR-S22": {
         "audit": {
          "agree": true,
          "reason": "The intent is unchanged; no call wrote it.",
          "verdict": "agrees"
         },
         "desc": "the intent as the fixture left it",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "AR-S23": {
         "audit": {
          "agree": true,
          "reason": "The ids match the file names and the trigger is design-round.",
          "verdict": "agrees"
         },
         "desc": "each finding's id is arch-json-report-<n>, its file's name, and its trigger design-round (not applicable with no finding file)",
         "detail": "each as the form says",
         "kind": "decide",
         "pass": true
        },
        "AR-S3": {
         "audit": {
          "agree": true,
          "reason": "No statement or Decision contains the error string, a private helper or a test name.",
          "verdict": "agrees"
         },
         "desc": "no statement names the error string, a helper or a test; no Decision the error string or a helper",
         "detail": "2 statement(s), 2 Decision(s) clean",
         "kind": "decide",
         "pass": true
        },
        "AR-S4": {
         "audit": {
          "agree": true,
          "reason": "The owning symbols include no underscore helper and no line number.",
          "verdict": "agrees"
         },
         "desc": "owning symbols name no private helper and no line number",
         "detail": "seam/report-renderings: =tally/report.py=: =ordered=, =format_table=, =format_json=; seam/cli-options: =tally/cli.py=: =main=",
         "kind": "decide",
         "pass": true
        },
        "AR-S5": {
         "audit": {
          "agree": true,
          "reason": "The changed paths are only the design and test files.",
          "verdict": "agrees"
         },
         "desc": "in the code root, only the design, test files and scenarios' :TEST: properties changed",
         "detail": "changed: tests/test_report.py docs/changes/json-report/design.org tests/test_cli.py",
         "kind": "decide",
         "pass": true
        },
        "AR-S6": {
         "audit": {
          "agree": true,
          "reason": "No commit or worktree appears, and the call list shows no Agent or Task call.",
          "verdict": "agrees"
         },
         "desc": "no commit, no new branch or worktree, no agent spawned",
         "detail": "refs and worktrees unchanged: True; spawns: 0",
         "kind": "decide",
         "pass": true
        },
        "AR-S7": {
         "audit": {
          "agree": true,
          "reason": "The run exits OK with 4 expected failures and no unexpected success.",
          "verdict": "agrees"
         },
         "desc": "the test run passes, each pending test reported as an expected failure",
         "detail": "exit 0; 4 expected failure(s); expectedFailure marks in tests/: 4; Ran 8 tests in 0.067s\n\nOK (expected failures=4)",
         "kind": "decide",
         "pass": true
        },
        "AR-S8": {
         "audit": {
          "agree": true,
          "reason": "Both scenarios are cited by test methods, as the files show.",
          "verdict": "agrees"
         },
         "desc": "each of the change's scenarios cited by a test, or marked :TEST: none: with a reason",
         "detail": "The JSON report maps each category to its total: tests/test_report.py Report.test_json_has_the_tables_order_and_amounts, tests/test_cli.py Cli.test_json_maps_each_category_to_its_total; An unknown option is refused: tests/test_cli.py Cli.test_unknown_option_is_refused, tests/test_cli.py Cli.test_unk … [cut, 349 characters]",
         "kind": "decide",
         "pass": true
        },
        "AR-S9": {
         "audit": {
          "agree": true,
          "reason": "The hand-back is one line in the form.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line in the design round's form, at most 200 characters",
         "detail": "design-round json-report: 2 seams, 4 pending tests, 0 uncited scenarios, 0 open questions",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AR-E1": "pass",
        "AR-E10": "pass",
        "AR-E11": "n/a",
        "AR-E12": "fail",
        "AR-E13": "pass",
        "AR-E14": "pass",
        "AR-E15": "pass",
        "AR-E16": "fail",
        "AR-E17": "pass",
        "AR-E2": "pass",
        "AR-E3": "pass",
        "AR-E4": "pass",
        "AR-E5": "pass",
        "AR-E6": "pass",
        "AR-E7": "fail",
        "AR-E8": "pass",
        "AR-E9": "pass"
       },
       "judge": {
        "AR-E1": {
         "quotes": [
          "=tally/cli.py= =main= takes the first command-line argument as the ledger path, ignores every other argument and prints the table",
          "=tally/cli.py= =main= takes the first command-line argument as the ledger path, ignores every other argument and prints the table",
          "=tally/cli.py= =main= takes the first command-line argument as the ledger path, ignores every other argument and prints the table"
         ],
         "reasons": [
          "The design is at docs/changes/json-report/design.org beside intent.org, has Context, Decisions and Seams in that order, and its Context describes the base code: ledger into entries, totals per category rendered as a table, and a command line that reads only the file.",
          "design.org sits beside intent.org with Context, Decisions and Seams in that order, and its Context describes the base code: the ledger reader, totals per category rendered as a table, and a command line that reads only the file.",
          "design.org sits beside intent.org with Context, Decisions and Seams in that order, and the Context describes the base code: the ledger read into entries, totals per category rendered as a table, and a command line that reads only the file."
         ]
        },
        "AR-E10": {
         "quotes": [
          "return subprocess.run(\n        [sys.executable, \"-m\", \"tally.cli\", *args]",
          "return subprocess.run(\n        [sys.executable, \"-m\", \"tally.cli\", *args]",
          "table = [l.split() for l in report.format_table(t).splitlines()]"
         ],
         "reasons": [
          "The rendering test is in tests/test_report.py and the command-line tests are in a new tests/test_cli.py, since no file drove cli.py; all of them exercise real code and assert behaviour, not module ownership or absence.",
          "The rendering test sits in tests/test_report.py and the CLI tests in a new tests/test_cli.py because no file drove cli.py; both drive real code on real totals and assert neither ownership nor absence.",
          "The rendering test is in test_report.py, and the command-line tests are in a new test_cli.py because none existed; all of them run real code through subprocess or the renderings."
         ]
        },
        "AR-E11": {
         "quotes": [
          "0 open questions",
          "0 open questions",
          "0 open questions"
         ],
         "reasons": [
          "The scenarios fix the behaviour so the round raises no user question; the design has no Open Questions section and no ask file was written.",
          "The round raised no question for the user, the design has no Open Questions section, and no ask file was written.",
          "The round raised no question for the user; the design has no Open Questions section, and only finding files were written, no ask file."
         ]
        },
        "AR-E12": {
         "quotes": [
          "title: A negative total renders one dollar too low and with the wrong cents (-12.05 shows -13.95)",
          "title: A negative total renders one dollar too low and with the wrong cents (-12.05 shows -13.95)",
          "title: A negative total renders one dollar too low and with the wrong cents (-12.05 shows -13.95)"
         ],
         "reasons": [
          "The refund defect is correctly a kind-issue finding (arch-json-report-1.md, trigger design-round) with locations, evidence, blocks, why and a resolution, and no Open Questions entry, but its title is 90 characters, beyond the finding form's 80-character label limit.",
          "The defect is correctly reported as an issue finding (arch-json-report-1.md, trigger design-round, with locations, evidence, blocks, why-missed and a resolution) and not as an open question, but its title is 90 characters, over the form's 80, and the resolution names 'the cents-to-text translation' rather than the function to change.",
          "The refund defect is correctly an issue finding (arch-json-report-1.md, trigger design-round, with locations, evidence, blocks, why-missed and a resolution) and not an open question, but its title is 90 characters, over the front-matter form's 80-character limit, and the resolution names the function only as 'the cents-to-text translation'."
         ]
        },
        "AR-E13": {
         "quotes": [
          "title: tally/cli.py's docstring advertises --json, which tally does not accept at HEAD",
          "thought: a single-dash argument (-j, or - for standard input) is read as a file name; nobody raised short options or stdin",
          "thought: a single-dash argument (-j, or - for standard input) is read as a file name; nobody raised short options or stdin"
         ],
         "reasons": [
          "The speculative short-option/stdin need is correctly a thought line, but the docstring mismatch (a text correction that changes no behaviour, and that this change makes true anyway) was filed as a finding instead of a thought line.",
          "The speculative need is one thought line in the named file; the missing-file need was moved from a thought to a finding, the hand-back is one line, and no finding concerns taste.",
          "The speculative need is one thought line in the named thought file; the missing-file note was moved from a thought to an issue finding because the code has that need today; no finding concerns taste, and the hand-back is one line."
         ]
        },
        "AR-E14": {
         "quotes": [
          "changed: tests/test_report.py docs/changes/json-report/design.org tests/test_cli.py",
          "changed: tests/test_report.py docs/changes/json-report/design.org tests/test_cli.py",
          "changed: tests/test_report.py docs/changes/json-report/design.org tests/test_cli.py"
         ],
         "reasons": [
          "Only the design and test files changed in the code root; tally/ and the intent are untouched, and besides them there are only finding files and the thought file.",
          "Only the design and test files changed in the code root; tally/ and the intent are untouched, and beyond them there are only findings and the thought file.",
          "In the code root only the design and test files changed; tally/ and the intent are untouched; outside it the round wrote only finding files and the thought file, and no task file."
         ]
        },
        "AR-E15": {
         "quotes": [
          "refs and worktrees unchanged: True; spawns: 0",
          "refs and worktrees unchanged: True; spawns: 0",
          "refs and worktrees unchanged: True; spawns: 0"
         ],
         "reasons": [
          "There is no commit, no worktree of its own, no agent spawned and no task file.",
          "There was no commit, worktree, agent or task file.",
          "There was no commit, worktree, spawned agent or task file."
         ]
        },
        "AR-E16": {
         "quotes": [
          "d = tempfile.TemporaryDirectory()",
          "d = tempfile.TemporaryDirectory()",
          "d = tempfile.TemporaryDirectory()"
         ],
         "reasons": [
          "The probes stayed in the scratch directory, but the Architect's own test run (call 15) executed tests/test_cli.py, whose setUp writes the ledger into a tempfile directory under the system temp directory, not the named scratch directory.",
          "The probes stayed in the scratch directory, but the Architect's test run (call 15) executed test_cli.py, whose setUp writes a ledger into a system tempfile directory outside the named scratch directory.",
          "The probes kept their files in the scratch directory, but the Architect's test run (call 15) executed test_cli.py's setUp, which writes a ledger into a tempfile directory under the system temp rather than the named scratch directory."
         ]
        },
        "AR-E17": {
         "quotes": [
          "design-round json-report: 2 seams, 4 pending tests, 0 uncited scenarios, 0 open questions",
          "design-round json-report: 2 seams, 4 pending tests, 0 uncited scenarios, 0 open questions",
          "design-round json-report: 2 seams, 4 pending tests, 0 uncited scenarios, 0 open questions"
         ],
         "reasons": [
          "The hand-back is one line in the required form, and its counts match the two rows, four pending tests, no uncited scenario and no Open Questions entry.",
          "The hand-back is one line in the required form, and its counts match the 2 rows, 4 pending tests, 0 uncited scenarios and 0 Open Questions entries.",
          "The hand-back is one line in the form, and its counts match the 2 rows, 4 pending tests, 0 uncited scenarios and no Open Questions entries."
         ]
        },
        "AR-E2": {
         "quotes": [
          "| id                     | statement | owning symbols | scenarios | tests |",
          "| id                     | statement | owning symbols | scenarios | tests |",
          "| id                     | statement | owning symbols | scenarios | tests |"
         ],
         "reasons": [
          "The table has the five columns and two rows with unique seam/ ids; the scenario cells cite the spec's exact titles as reporting § <title>, and the tests cells name existing test files.",
          "The table has the five columns and two rows with unique seam/ ids; each scenarios cell cites the exact spec titles and each tests cell names existing test files.",
          "Both rows have the five columns and unique seam/ ids, cite scenarios as =reporting § <exact title>=, and name test files that exist."
         ]
        },
        "AR-E3": {
         "quotes": [
          "takes its category order and its amount text from =report='s one order and one cents-to-text translation",
          "one order and one cents-to-text translation",
          "takes its category order and its amount text from =report='s one order and one cents-to-text translation"
         ],
         "reasons": [
          "No statement or Decision names a guard, the error string, an encoding, _money or a test; the amount helper appears only as 'one cents-to-text translation'.",
          "No statement or Decision names the error string, _money, an encoding or a test; the helper is described rather than named, and the '--' prefix rule reads as the contract for what an option is, not as a guard.",
          "No statement or Decision names the error string, a helper such as _money, an encoding or a test; the cents-to-text translation is described, not named."
         ]
        },
        "AR-E4": {
         "quotes": [
          "=tally/report.py=: =ordered=, =format_table=, =format_json=",
          "=tally/report.py=: =ordered=, =format_table=, =format_json=",
          "=tally/report.py=: =ordered=, =format_table=, =format_json=; ... =tally/cli.py=: =main="
         ],
         "reasons": [
          "The owning symbols are public entry points with no line numbers and no private helper, and the new format_json carries no marker.",
          "The owning symbols are public entry points with no line numbers or private helpers, and format_json is named with no 'new' marker.",
          "The owning symbols are the public renderings and order the seam is about, plus the command line's entry point; no private helper or line number appears, and format_json carries no 'new' marker."
         ]
        },
        "AR-E5": {
         "quotes": [
          "| seam/cli-options       | =main= is the one reader of the command line",
          "",
          ""
         ],
         "reasons": [
          "Neither row carries a status, flag, date, history or note to a later role.",
          "No row carries a status, flag, date, history or message to a later role.",
          "Neither row carries a status, flag, date, history or note to a later role."
         ]
        },
        "AR-E6": {
         "quotes": [
          "=main= is the one reader of the command line: an argument beginning with =--= is an option wherever it stands",
          "the JSON rendering is one object mapping each category to the amount text the table shows, in the table's order",
          "=main= is the one reader of the command line: ... =--json= selects the JSON rendering"
         ],
         "reasons": [
          "Each seam states a boundary invariant (renderings share report's order and amount text; main alone reads the command line), serves G1 or G2, and adds no extra format or hook, though each partly echoes its scenario.",
          "Both statements restate scenario behaviour: this clause repeats the JSON scenario, and seam/cli-options adds 'any other option is refused with nothing on standard output'; the criterion requires a pure producer/consumer boundary.",
          "Both seams state what holds between the command line and the renderings, or between the renderings and report's shared order and amount text, and serve G1 and G2 scenarios; no extra format or extension hook is designed."
         ]
        },
        "AR-E7": {
         "quotes": [
          "That holds only if every argument that looks like an option is treated as one wherever it stands, before or after the file, and refused when it is not known.",
          "That holds only if every argument that looks like an option is treated as one wherever it stands, before or after the file, and refused when it is not known.",
          "That holds only if every argument that looks like an option is treated as one wherever it stands, before or after the file"
         ],
         "reasons": [
          "Both Decision headings carry their seam ids, but D2 replaces the take-the-first-argument-and-ignore-the-rest mechanism without saying, for each property the old one gave (which argument is the file, extra non-option arguments being ignored), whether it is kept or dropped.",
          "Both Decisions carry their seam ids and the intent is unchanged, but D2 replaces the 'first argument is the file, the rest are ignored' mechanism without naming what that mechanism gave (extra arguments ignored, any first argument read as a file) or marking those properties kept or dropped.",
          "Both Decision headings carry their seam ids, but D2 replaces a mechanism that took the first argument as the file and ignored the rest, and it never says whether 'first argument is the file' and 'other arguments are ignored' are kept or marked dropped."
         ]
        },
        "AR-E8": {
         "quotes": [
          "@unittest.expectedFailure\n    def test_unknown_option_is_refused(self):\n        \"\"\"Scenario: reporting § An unknown option is refused\"\"\"",
          "@unittest.expectedFailure\n    def test_unknown_option_is_refused(self):\n        \"\"\"Scenario: reporting § An unknown option is refused\"\"\"",
          "@unittest.expectedFailure\n    def test_unknown_option_is_refused(self):\n        \"\"\"Scenario: reporting § An unknown option is refused\"\"\""
         ],
         "reasons": [
          "Each scenario is cited by expectedFailure tests as reporting § <title>, and the contract tests also cite seam/report-renderings and seam/cli-options.",
          "Each scenario has expectedFailure tests citing 'reporting § <title>', and the contract tests also cite seam/report-renderings and seam/cli-options.",
          "Each scenario is cited by expectedFailure tests that drive the command line, and the contract tests also cite their seam ids."
         ]
        },
        "AR-E9": {
         "quotes": [
          "OK (expected failures=4)",
          "OK (expected failures=4)",
          "OK (expected failures=4)"
         ],
         "reasons": [
          "The suite passes and all four pending tests are reported as expected failures, with no failure and no unexpected success.",
          "The suite passes, with all four pending tests reported as expected failures and no unexpected successes.",
          "The suite passes, and all four pending tests are reported as expected failures with no unexpected success."
         ]
        }
       },
       "milestones": [],
       "n_calls": 17,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Architect's design round, from its brief (kept in the change's directory) to its hand-back",
   "title": "Case architect-ns6: the design round's design states contracts only, a defect found is a finding",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T123104 t1 (skill-evals)"
  },
  {
   "capability": "architect",
   "id": "architect-premises",
   "items": [
    {
     "checks": [
      {
       "id": "AP-S1",
       "kind": "decide"
      },
      {
       "id": "AP-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The premise check writes a table of premises per task",
      "record § A cycle's directory holds what its steps wrote, under fixed names"
     ],
     "id": "AP-E1",
     "question": "Is =premise-check.md= in the cycle's directory, with a table of premises for each task?"
    },
    {
     "checks": [
      {
       "id": "AP-S3",
       "kind": "decide"
      },
      {
       "id": "AP-S4",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § The premise check writes a table of premises per task"
     ],
     "id": "AP-E2",
     "question": "Is the wrong claim marked Wrong, and are the claims about the code that hold marked Holds?"
    },
    {
     "checks": [
      {
       "id": "AP-S5",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A defect of the task file itself is a row of its table"
     ],
     "id": "AP-E3",
     "question": "Is the quote cut short a row of its task's table, marked as a defect of the task file?"
    },
    {
     "checks": [
      {
       "id": "AP-S6",
       "kind": "decide"
      },
      {
       "id": "AP-S7",
       "kind": "signal"
      },
      {
       "id": "AP-S17",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A defect outside every task's scope is a finding of the premise check",
      "architect § Every defect is a finding, whatever its likelihood",
      "architect § A need the code has today is an issue, never a thought"
     ],
     "id": "AP-E4",
     "question": "If the run names the refund's wrong amount, is it a finding of the premise check, found from the batch, and never a thought?"
    },
    {
     "checks": [
      {
       "id": "AP-S8",
       "kind": "decide"
      },
      {
       "id": "AP-S9",
       "kind": "signal"
      },
      {
       "id": "AP-S10",
       "kind": "decide"
      },
      {
       "id": "AP-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A stale seam row is an interface-drift finding of the premise check",
      "architect § The premise check changes neither the design nor the tests"
     ],
     "id": "AP-E5",
     "question": "Is the stale row an interface-drift finding, with the row and the tests left as they were?"
    },
    {
     "checks": [
      {
       "id": "AP-S12",
       "kind": "decide"
      },
      {
       "id": "AP-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A finding is one file in the cycle's findings directory",
      "architect § A finding carries the class of the design checklist",
      "architect § A finding says why the tests missed it and how to resolve it",
      "architect § The Architect writes no route for a finding",
      "reviewer § An Architect's finding is one file in its cycle's directory",
      "reviewer § An Architect's finding carries its labels in its front matter",
      "reviewer § An Architect's finding says where, why the tests missed it and what to do",
      "reviewer § Every finding says what it blocks"
     ],
     "id": "AP-E6",
     "question": "Is each finding one file of the cycle's =findings/=, with a finding's labels and sections, what it blocks, and no route?"
    },
    {
     "checks": [
      {
       "id": "AP-S14",
       "kind": "decide"
      },
      {
       "id": "AP-S15",
       "kind": "decide"
      },
      {
       "id": "AP-S16",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A speculative need or a cleanup is a thought line, not a finding",
      "architect § A cleanup a project prior asks for is a thought line naming the prior",
      "reviewer § An Architect's thought is never a finding file",
      "record § A journal entry names who wrote it"
     ],
     "id": "AP-E7",
     "question": "Are the uncalled function and the agreeing second rendering thought lines in the journal, the second naming the prior, and neither a finding?"
    },
    {
     "checks": [],
     "cites": [
      "architect § Style and preference are not reported"
     ],
     "id": "AP-E8",
     "question": "Is nothing of style or preference reported?"
    },
    {
     "checks": [
      {
       "id": "AP-S18",
       "kind": "decide"
      },
      {
       "id": "AP-S19",
       "kind": "decide"
      },
      {
       "id": "AP-S20",
       "kind": "decide"
      },
      {
       "id": "AP-S21",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The premise check writes only its table, its findings and thought lines",
      "architect § No run commits, spawns an agent or writes a task",
      "architect § Probes leave their files in the scratch directory"
     ],
     "id": "AP-E9",
     "question": "Does the run write only its table, its findings and thought lines, commit nothing, spawn no agent and write no task?"
    },
    {
     "checks": [
      {
       "id": "AP-S22",
       "kind": "decide"
      },
      {
       "id": "AP-S23",
       "kind": "decide"
      },
      {
       "id": "AP-S24",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § The premise check hands back its counts and its file"
     ],
     "id": "AP-E10",
     "question": "Is the hand-back one line of the premise check's counts and its file?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "AP-E1": [
       "architect § The premise check writes a table of premises per task",
       "record § A cycle's directory holds what its steps wrote, under fixed names"
      ],
      "AP-E10": [
       "architect § The premise check hands back its counts and its file"
      ],
      "AP-E2": [
       "architect § The premise check writes a table of premises per task"
      ],
      "AP-E3": [
       "architect § A defect of the task file itself is a row of its table"
      ],
      "AP-E4": [
       "architect § A defect outside every task's scope is a finding of the premise check",
       "architect § Every defect is a finding, whatever its likelihood",
       "architect § A need the code has today is an issue, never a thought"
      ],
      "AP-E5": [
       "architect § A stale seam row is an interface-drift finding of the premise check",
       "architect § The premise check changes neither the design nor the tests"
      ],
      "AP-E6": [
       "architect § A finding is one file in the cycle's findings directory",
       "architect § A finding carries the class of the design checklist",
       "architect § A finding says why the tests missed it and how to resolve it",
       "architect § The Architect writes no route for a finding",
       "reviewer § An Architect's finding is one file in its cycle's directory",
       "reviewer § An Architect's finding carries its labels in its front matter",
       "reviewer § An Architect's finding says where, why the tests missed it and what to do",
       "reviewer § Every finding says what it blocks"
      ],
      "AP-E7": [
       "architect § A speculative need or a cleanup is a thought line, not a finding",
       "architect § A cleanup a project prior asks for is a thought line naming the prior",
       "reviewer § An Architect's thought is never a finding file",
       "record § A journal entry names who wrote it"
      ],
      "AP-E8": [
       "architect § Style and preference are not reported"
      ],
      "AP-E9": [
       "architect § The premise check writes only its table, its findings and thought lines",
       "architect § No run commits, spawns an agent or writes a task",
       "architect § Probes leave their files in the scratch directory"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AP-E1": [
       "AP-S1",
       "AP-S2"
      ],
      "AP-E10": [
       "AP-S22",
       "AP-S23",
       "AP-S24"
      ],
      "AP-E2": [
       "AP-S3",
       "AP-S4"
      ],
      "AP-E3": [
       "AP-S5"
      ],
      "AP-E4": [
       "AP-S17",
       "AP-S6",
       "AP-S7"
      ],
      "AP-E5": [
       "AP-S10",
       "AP-S11",
       "AP-S8",
       "AP-S9"
      ],
      "AP-E6": [
       "AP-S12",
       "AP-S13"
      ],
      "AP-E7": [
       "AP-S14",
       "AP-S15",
       "AP-S16"
      ],
      "AP-E8": [],
      "AP-E9": [
       "AP-S18",
       "AP-S19",
       "AP-S20",
       "AP-S21"
      ]
     },
     "items_then": [],
     "run": "20261114T113712",
     "trials": [
      {
       "calls": {
        "7": {
         "error": true,
         "text": "cd <trial>/home/.worktrees/tally/json-report; git diff 5a7e127 HEAD --stat; git log --format='%h %s' -S format_csv; git log --format='%h %s' -S _amount; git log --format='%h %s' -S render_json; S=<trial>/scratch/architect-premise-check; ls $S; python3 -m unittest discover -s tests -t . 2>&1 | tail -4; printf '2026-10-01,food,-12.05\\n' > $S/refund.csv; cd $S && PYTHONPATH=<trial>/home/.worktrees/tally/json-report python3 -B -c \"\nfrom tally import cli, report\ncli.main(['$S/refund.csv']); cli.main(['$S/refund.csv','--json'])\nprint(report.format_json({'zeta':300,'alpha':200,'mid':100}))\nimport con … [cut, 847 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "AP-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists at that path, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "premise-check.md in the cycle's directory",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-2/premise-check.md",
         "kind": "decide",
         "pass": true
        },
        "AP-S10": {
         "audit": {
          "agree": true,
          "reason": "No call edited design.org, consistent with unchanged.",
          "verdict": "agrees"
         },
         "desc": "the design as it was",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "AP-S11": {
         "audit": {
          "agree": true,
          "reason": "No call wrote under tests/.",
          "verdict": "agrees"
         },
         "desc": "no test written or changed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AP-S12": {
         "audit": {
          "agree": true,
          "reason": "Both finding files have the required keys, values and sections, as I read them.",
          "verdict": "agrees"
         },
         "desc": "each finding file: the labels, trigger premise-check, class from the checklist, title at most 80, discovered_by architect, blocks, no decoration, the three sections",
         "detail": "2 finding file(s) well formed",
         "kind": "decide",
         "pass": true
        },
        "AP-S13": {
         "audit": {
          "agree": true,
          "reason": "Each file name matches its finding_id.",
          "verdict": "agrees"
         },
         "desc": "each finding file is arch-<cycle-id>-<n>.md, its finding_id its name",
         "detail": "arch-cycle-2-1.md arch-cycle-2-2.md",
         "kind": "decide",
         "pass": true
        },
        "AP-S14": {
         "audit": {
          "agree": true,
          "reason": "The findings' locations (report.py:17, :29, :27 and design.org:16) are not at the lines of format_csv or _amount.",
          "verdict": "agrees"
         },
         "desc": "no finding file located at the uncalled function or the agreeing second rendering",
         "detail": "tally/report.py:format_csv lines [32, 33, 34]; tally/cli.py:_amount lines [7, 8, 9]; findings there: none",
         "kind": "decide",
         "pass": true
        },
        "AP-S15": {
         "audit": {
          "agree": true,
          "reason": "Both added journal entries have by: architect.",
          "verdict": "agrees"
         },
         "desc": "each journal entry the run added names the Architect as its writer",
         "detail": "by=architect: thought: prior two helpers that do the same thing are merged into one: tally/cli.py _amount is an uncalled copy of tally; by=architect: thought: tally/report.py format_csv is an uncalled producer added by report-json; N1 says no other output format",
         "kind": "decide",
         "pass": true
        },
        "AP-S16": {
         "audit": {
          "agree": true,
          "reason": "The _amount thought mentions the prior.",
          "verdict": "agrees"
         },
         "desc": "a journal entry added names the prior",
         "detail": "thought: prior two helpers that do the same thing are merged into one: tally/cli.py _amount is an uncalled copy of tally/report.py _money (same refund bug); del",
         "kind": "signal",
         "pass": true
        },
        "AP-S17": {
         "audit": {
          "agree": false,
          "reason": "It fails on the word \"refund\" in a parenthetical of the _amount thought, which records the uncalled copy rather than the defect; the defect itself is a finding, so the match does not show the defect was made a thought.",
          "verdict": "suspect"
         },
         "desc": "no journal entry added names the refund's wrong amount",
         "detail": "thought: prior two helpers that do the same thing are merged into one: tally/cli.py _amount is an uncalled copy of tally/report.py _money (same refund bug); del",
         "kind": "signal",
         "pass": false
        },
        "AP-S18": {
         "audit": {
          "agree": true,
          "reason": "The changed files listed are exactly the allowed set.",
          "verdict": "agrees"
         },
         "desc": "in the home and the code root, only premise-check.md, the cycle's finding files and the state changed",
         "detail": "changed: .orchestrator/changes/json-report/cycles/cycle-2/findings/arch-cycle-2-1.md .orchestrator/changes/json-report/cycles/cycle-2/findings/arch-cycle-2-2.md .orchestrator/changes/json-report/cycles/cycle-2/premise-check.md .orchestrator/changes/json-report/state.json .orchestrator/changes/json-r … [cut, 316 characters]",
         "kind": "decide",
         "pass": true
        },
        "AP-S19": {
         "audit": {
          "agree": true,
          "reason": "Only journal +2 and findings index +2 were added, matching the tool calls; the failed call 16 wrote nothing.",
          "verdict": "agrees"
         },
         "desc": "the state changed only by entries added to the journal and to the findings' index",
         "detail": "other fields changed: none; journal +2, findings index +2; earlier entries kept: True",
         "kind": "decide",
         "pass": true
        },
        "AP-S2": {
         "audit": {
          "agree": true,
          "reason": "Each task's section has Holds/Wrong rows (8, 5 and 5), which matches the file.",
          "verdict": "agrees"
         },
         "desc": "each candidate task named, with a table of premises marked Holds or Wrong",
         "detail": "json-unknown-option: named, 8 Holds/Wrong row(s); json-sort-keys: named, 5 Holds/Wrong row(s); json-indent: named, 5 Holds/Wrong row(s)",
         "kind": "decide",
         "pass": true
        },
        "AP-S20": {
         "audit": {
          "agree": true,
          "reason": "No git commit/branch/worktree commands, no Agent call and no task edits appear in the calls.",
          "verdict": "agrees"
         },
         "desc": "no commit, ref or worktree made, no agent spawned, no task file written",
         "detail": "refs and worktrees unchanged: True; spawns: none; task files: none",
         "kind": "decide",
         "pass": true
        },
        "AP-S21": {
         "audit": {
          "agree": true,
          "reason": "All paths are inside the trial directory and no call runs mktemp.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AP-S22": {
         "audit": {
          "agree": true,
          "reason": "The hand-back's path is absolute, not relative to the home, so the fail fits the item's stated form.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line, premise-check <cycle-id>: <n> holds, <m> wrong, <f> findings; <path>, the path premise-check.md's from the home, at most 200 characters",
         "detail": "premise-check cycle-2: 11 holds, 7 wrong, 2 findings; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-2/premise-check.md",
         "kind": "decide",
         "pass": false
        },
        "AP-S23": {
         "audit": {
          "agree": true,
          "reason": "The hand-back says 2 findings and there are 2 files.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's findings count the cycle's finding files (not applicable without the line)",
         "detail": "hand-back 2; files 2",
         "kind": "decide",
         "pass": true
        },
        "AP-S24": {
         "audit": {
          "agree": true,
          "reason": "Counting the rows gives 11 Holds (6+2+3) and 7 Wrong (2+3+2), matching the hand-back.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's holds and wrong count the file's rows marked so",
         "detail": "hand-back 11 holds, 7 wrong; file 11 Holds, 7 Wrong",
         "kind": "signal",
         "pass": true
        },
        "AP-S3": {
         "audit": {
          "agree": true,
          "reason": "json-unknown-option's section has the check_options row marked Wrong, which is the wrong claim the item targets.",
          "verdict": "agrees"
         },
         "desc": "the task with the wrong claim has a premise marked Wrong",
         "detail": "| `tally/cli.py: main` exists and is the owner named by seam/options | **Holds** — `tally/cli.py:12` |; | `main` passes the words after the file name to `check_options(argv[1:])` | **Wrong** — there is no `check_options` anywhere in the tree; `main` checks no options at all, it only tests `\"--json\"  … [cut, 1330 characters]",
         "kind": "decide",
         "pass": true
        },
        "AP-S4": {
         "audit": {
          "agree": true,
          "reason": "json-indent's section marks the format_json claims Holds.",
          "verdict": "agrees"
         },
         "desc": "the third task has a premise marked Holds",
         "detail": "| `format_json` in `tally/report.py` returns `json.dumps` of a dict of the table's categories and amounts | **Holds** — `tally/report.py:27-29` |; | Passing `indent=2` to that call indents the report by two spaces | **Holds** |; | `tests/test_json.py` is where the expected form is asserted | **Holds … [cut, 1071 characters]",
         "kind": "signal",
         "pass": true
        },
        "AP-S5": {
         "audit": {
          "agree": true,
          "reason": "json-sort-keys has a row on the seam quote that says \"truncated\".",
          "verdict": "agrees"
         },
         "desc": "the task with the cut-short quote has a row naming the quote",
         "detail": "| The cited seam quote | **Wrong (task-form defect)** — truncated after \"holds exactly the table's categories,\"; the statement, owning symbols, scenarios and tests columns are missing |",
         "kind": "signal",
         "pass": true
        },
        "AP-S6": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-2-1 is located at tally/report.py:17 (_money) and has trigger premise-check and kind issue.",
          "verdict": "agrees"
         },
         "desc": "a finding located at the amount rendering is the premise check's",
         "detail": "_money at tally/report.py lines [17, 18]; arch-cycle-2-1.md: trigger=premise-check kind=issue",
         "kind": "decide",
         "pass": true
        },
        "AP-S7": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-2-1 mentions the refund and -13.95.",
          "verdict": "agrees"
         },
         "desc": "a finding names the refund's wrong amount",
         "detail": "arch-cycle-2-1.md",
         "kind": "signal",
         "pass": true
        },
        "AP-S8": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-2-2 names render_json and has class interface-drift and seam seam/json-report.",
          "verdict": "agrees"
         },
         "desc": "a finding on the stale row is interface-drift, about the row's seam",
         "detail": "arch-cycle-2-2.md: class=interface-drift seam=seam/json-report",
         "kind": "decide",
         "pass": true
        },
        "AP-S9": {
         "audit": {
          "agree": true,
          "reason": "arch-cycle-2-2 names render_json.",
          "verdict": "agrees"
         },
         "desc": "a finding names the stale row's symbol",
         "detail": "arch-cycle-2-2.md",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "AP-E1": "pass",
        "AP-E10": "fail",
        "AP-E2": "pass",
        "AP-E3": "pass",
        "AP-E4": "pass",
        "AP-E5": "pass",
        "AP-E6": "pass",
        "AP-E7": "pass",
        "AP-E8": "pass",
        "AP-E9": "pass"
       },
       "judge": {
        "AP-E1": {
         "quotes": [
          "## json-unknown-option ... Verdict: **Wrong** — rewrite step 1 ...; ## json-sort-keys ... Verdict: **Wrong** ...; ## json-indent ... Verdict: **Wrong** ...",
          "## json-unknown-option ... Verdict: **Wrong** ... ## json-sort-keys ... Verdict: **Wrong** ... ## json-indent ... Verdict: **Wrong**",
          "## json-unknown-option ... Verdict: **Wrong** — rewrite step 1 ... ## json-sort-keys ... Verdict: **Wrong** ... ## json-indent ... Verdict: **Wrong**"
         ],
         "reasons": [
          "The file is at cycles/cycle-2/premise-check.md and has a `| claim | at HEAD |` table with Holds/Wrong rows and a one-line verdict for each of the three candidate tasks.",
          "The file is at cycles/cycle-2/premise-check.md, and each of the three tasks has a `| claim | at HEAD |` table with Holds or Wrong per row and a one-line verdict.",
          "The file is at the cycle's path, and each of the three tasks has a `| claim | at HEAD |` table with Holds or Wrong on every row and a one-line verdict."
         ]
        },
        "AP-E10": {
         "quotes": [
          "premise-check cycle-2: 11 holds, 7 wrong, 2 findings; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-2/premise-check.md",
          "premise-check cycle-2: 11 holds, 7 wrong, 2 findings; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-2/premise-check.md",
          "premise-check cycle-2: 11 holds, 7 wrong, 2 findings; <trial>/home/.orchestrator/changes/json-report/cycles/cycle-2/premise-check.md"
         ],
         "reasons": [
          "It is one line and its counts match the file (11 Holds, 7 Wrong, 2 findings), but the path is absolute rather than the home-relative `.orchestrator/changes/json-report/cycles/cycle-2/premise-check.md` the item specifies.",
          "The line is one line and its counts are right, but it gives an absolute path where the required form gives the path relative to the home.",
          "The line is one line, under 200 characters, and its counts match the file (11 Holds, 7 Wrong, 2 findings), but its path is absolute rather than the `.orchestrator/...` path the item's form gives."
         ]
        },
        "AP-E2": {
         "quotes": [
          "| `main` passes the words after the file name to `check_options(argv[1:])` | **Wrong** — there is no `check_options` anywhere in the tree",
          "`main` passes the words after the file name to `check_options(argv[1:])` | **Wrong** — there is no `check_options` anywhere in the tree",
          "`main` passes the words after the file name to `check_options(argv[1:])` | **Wrong** — there is no `check_options` anywhere in the tree"
         ],
         "reasons": [
          "The check_options claim is marked Wrong, json-indent's claim about what format_json returns is marked Holds (`tally/report.py:27-29`), and each Wrong verdict follows from a Wrong row in its own table.",
          "The check_options claim is Wrong, and json-indent's claim on what `format_json` returns is Holds; each verdict follows from its table, and json-indent's other Wrong rows are the Architect's reading of the stale quote and of spec fit, which the item excuses.",
          "The missing `check_options` row is marked Wrong, and json-indent's claim about what `format_json` returns is marked Holds; its other Wrong rows are the stale seam symbol and the scenario reading, which the item excuses. Each Wrong verdict follows from at least one Wrong row in its table."
         ]
        },
        "AP-E3": {
         "quotes": [
          "| The cited seam quote | **Wrong (task-form defect)** — truncated after \"holds exactly the table's categories,\"",
          "| The cited seam quote | **Wrong (task-form defect)** — truncated after \"holds exactly the table's categories,\"",
          "| The cited seam quote | **Wrong (task-form defect)** — truncated after \"holds exactly the table's categories,\""
         ],
         "reasons": [
          "json-sort-keys' table has a row for the cut-short quote, flagged as a defect of the task file rather than a premise about the code.",
          "json-sort-keys' table has a row for the cut-short quote, flagged as a defect of the task file.",
          "json-sort-keys' table has a row for the cut quote, flagged as a defect of the task file's own form."
         ]
        },
        "AP-E4": {
         "quotes": [
          "thought: prior two helpers that do the same thing are merged into one: tally/cli.py _amount is an uncalled copy of tally/report.py _money (same refund bug); delete it",
          "finding_id: arch-cycle-2-1\ntrigger: premise-check\nkind: issue ... discovered_from: batch-cycle-2",
          "finding_id: arch-cycle-2-1\ntrigger: premise-check\nkind: issue ... discovered_from: batch-cycle-2"
         ],
         "reasons": [
          "The refund defect is a finding (arch-cycle-2-1: trigger premise-check, discovered_from batch-cycle-2, kind issue), but a thought line also names it (\"same refund bug\"), and the item requires that no thought line name it.",
          "The refund defect is a finding file of kind issue from the premise check and the batch; the only thought touching it is about the uncalled `_amount` copy and mentions the bug in a parenthetical without recording the defect as a thought.",
          "The refund defect is an issue finding of the premise check, discovered from the batch. The only thought line that mentions it is about the uncalled `_amount` copy (\"same refund bug\"), so the defect itself is not recorded as a thought."
         ]
        },
        "AP-E5": {
         "quotes": [
          "class: interface-drift ... seam: seam/json-report ... trigger: premise-check",
          "class: interface-drift ... seam: seam/json-report",
          "class: interface-drift\ntitle: seam/json-report names render_json; the producer at HEAD is format_json ... trigger: premise-check"
         ],
         "reasons": [
          "arch-cycle-2-2 is an interface-drift finding of the premise check on seam/json-report; no call edited design.org or any test (its recommended resolution leaves the row fix to the orchestrator).",
          "arch-cycle-2-2 is an interface-drift finding on seam/json-report with trigger premise-check, and no Write or Edit touched design.org or tests.",
          "The stale row is an interface-drift finding on seam/json-report from the premise check. design.org and tests/ are unchanged, and no Write or Edit touched them."
         ]
        },
        "AP-E6": {
         "quotes": [
          "finding_id: arch-cycle-2-1 / trigger: premise-check / kind: issue / severity: high / class: invariant-gap / ... / discovered_by: architect / ... / blocks: nothing",
          "discovered_by: architect\ndiscovered_at: 2026-11-14T12:00:00Z\nseam: seam/json-report\nevidence: ... blocks: nothing",
          "## Locations ... ## Why tests missed it ... ## Recommended resolution ... blocks: nothing"
         ],
         "reasons": [
          "Each finding is one file named arch-cycle-2-<n>.md with all the required front-matter keys, a checklist class, a title under 80 characters and no decoration, plus Locations with file:line and context, a one-sentence Why tests missed it, and a one-paragraph resolution naming files and functions.",
          "Both files are arch-cycle-2-<n>.md and carry the required front matter: classes from the eight, titles under 80 characters, blocks, no decoration, and the three sections with file:line context, one-sentence reasons and one-paragraph resolutions.",
          "Both files are `arch-cycle-2-<n>.md` with every listed front-matter key and no decoration. Their classes are from the checklist and their titles are under 80 characters. Each has the three sections: file:line locations, a one-sentence why, and a one-paragraph resolution naming files and functions."
         ]
        },
        "AP-E7": {
         "quotes": [
          "thought: tally/report.py format_csv is an uncalled producer added by report-json; N1 says no other output format",
          "thought: prior two helpers that do the same thing are merged into one: tally/cli.py _amount ...; thought: tally/report.py format_csv is an uncalled producer",
          "thought: prior two helpers that do the same thing are merged into one: tally/cli.py _amount is an uncalled copy ...\" / \"thought: tally/report.py format_csv is an uncalled producer"
         ],
         "reasons": [
          "The uncalled format_csv and the duplicate _amount are each one thought line by architect, the _amount line names the duplicate-helpers prior, and neither has a finding.",
          "`format_csv` and `_amount` are each one thought line by architect, the `_amount` one naming the prior, and no finding is located at either.",
          "The uncalled `format_csv` and the duplicate `_amount` are one thought line each, with `by: architect`, and the `_amount` line names the prior. No finding is located at either."
         ]
        },
        "AP-E8": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "The two findings are a behaviour defect and a stale row, and the two thoughts are dead or duplicate code; none is about naming, formatting or an equivalent structure.",
          "Neither finding nor either thought is about naming, formatting or an equivalent structure.",
          "The findings are a behaviour defect and a stale seam, and the thoughts are dead code and a duplicate helper; none is about naming, formatting or an equivalent structure."
         ]
        },
        "AP-E9": {
         "quotes": [
          "changed: .../findings/arch-cycle-2-1.md .../findings/arch-cycle-2-2.md .../premise-check.md .../state.json .../state.lock",
          "changed: ...findings/arch-cycle-2-1.md ...arch-cycle-2-2.md ...premise-check.md ...state.json ...state.lock",
          "--- .orchestrator/changes/json-report/state.lock ---"
         ],
         "reasons": [
          "The run wrote only the table, the finding files, the findings index and thought lines; it made no commit, spawned no agent, wrote no task file, and its probe file is in scratch/architect-premise-check/.",
          "The run wrote only its table, its findings, the findings index and two thought lines; it made no commit, spawned no agent, wrote no task, and its probes wrote only scratch/architect-premise-check/refund.csv.",
          "Outside its premise-check.md and finding files, the run changed only the state (2 journal thoughts, 2 finding index entries) and the lock file. It made no commit, spawned no agent and wrote no task file. Probes ran from the named scratch directory with -B; the call-7 attempt failed before writing anything."
         ]
        }
       },
       "milestones": [
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 17,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Architect's premise check of a cycle's candidate tasks, from its brief to its hand-back",
   "title": "Case architect-premises: the premise check marks each claim, reports a stale row and a defect outside the batch as findings, and a cleanup as a thought",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T113712 t1 (skill-evals)"
  },
  {
   "capability": "architect",
   "id": "architect-replace",
   "items": [
    {
     "checks": [
      {
       "id": "AX-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The design round writes the change's design beside its intent",
      "architect § The Context describes the code the change starts from"
     ],
     "id": "AX-E1",
     "question": "Is the design written beside the intent, with its sections in order?"
    },
    {
     "checks": [
      {
       "id": "AX-S2",
       "kind": "decide"
      },
      {
       "id": "AX-S3",
       "kind": "decide"
      },
      {
       "id": "AX-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The Seams table has the design's five columns",
      "architect § A seam row has a unique id and names its scenarios and tests",
      "architect § A seam row's statement names no implementation detail",
      "architect § Owning symbols are the boundary's entry points",
      "architect § A symbol the design introduces is named without a marker",
      "architect § A seam row holds the current state only"
     ],
     "id": "AX-E2",
     "question": "Is the Seams table in its five columns, each row with a unique id, exact scenario titles and test files that exist, stating no implementation detail?"
    },
    {
     "checks": [
      {
       "id": "AX-S5",
       "kind": "decide"
      },
      {
       "id": "AX-S6",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A Decision that replaces a mechanism accounts for what the old one gave",
      "architect § A Decision names no implementation detail",
      "architect § A Decision that names a seam links to it by id"
     ],
     "id": "AX-E3",
     "question": "Does the Decision that replaces the parser account for each property the old one gave, a kept one being a row?"
    },
    {
     "checks": [
      {
       "id": "AX-S7",
       "kind": "signal"
      },
      {
       "id": "AX-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A property dropped on purpose is marked dropped in its Decision"
     ],
     "id": "AX-E4",
     "question": "Is the whitespace property marked dropped in its Decision, with the intent left as it was?"
    },
    {
     "checks": [
      {
       "id": "AX-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A scenario the code already has gets a plain test"
     ],
     "id": "AX-E5",
     "question": "Does the scenario the code already has get its citation on the existing test, and nothing else there?"
    },
    {
     "checks": [
      {
       "id": "AX-S10",
       "kind": "decide"
      },
      {
       "id": "AX-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A scenario the code lacks gets a pending test that cites it",
      "architect § A pending test is reported as pending, never as failing",
      "architect § A new test sits with the tests of the module it drives",
      "architect § A pending test drives real code"
     ],
     "id": "AX-E6",
     "question": "Does the scenario the code lacks get a pending test that cites it, reported as pending?"
    },
    {
     "checks": [
      {
       "id": "AX-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § Every scenario of the change is cited by a test or marked none",
      "architect § A scenario with no test worth writing says why"
     ],
     "id": "AX-E7",
     "question": "Is every scenario of the change cited by a test or marked none with a reason?"
    },
    {
     "checks": [
      {
       "id": "AX-S13",
       "kind": "decide"
      },
      {
       "id": "AX-S14",
       "kind": "signal"
      },
      {
       "id": "AX-S15",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § An open question is an entry in the design's Open Questions",
      "architect § An open question's rows and tests are drafted in the recommended form"
     ],
     "id": "AX-E8",
     "question": "Is the malformed line's handling an entry of Open Questions, its rows and tests drafted in the recommended form, and no Decision?"
    },
    {
     "checks": [
      {
       "id": "AX-S16",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The intent's open questions join the design's"
     ],
     "id": "AX-E9",
     "question": "Does the intent's open question join the design's, marked (intent)?"
    },
    {
     "checks": [
      {
       "id": "AX-S17",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § A question about a case nobody raised is not an open question",
      "architect § Each seam serves a scenario the goals need"
     ],
     "id": "AX-E10",
     "question": "Is the tab-separated file left out of Open Questions?"
    },
    {
     "checks": [
      {
       "id": "AX-S18",
       "kind": "decide"
      },
      {
       "id": "AX-S19",
       "kind": "decide"
      },
      {
       "id": "AX-S20",
       "kind": "decide"
      },
      {
       "id": "AX-S21",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The design round writes only the design, the tests and the test properties",
      "architect § No run commits, spawns an agent or writes a task",
      "architect § Probes leave their files in the scratch directory",
      "record § The change's directory holds what the design round leaves before any cycle"
     ],
     "id": "AX-E11",
     "question": "Does the round write only the design, the tests and the test properties, commit nothing and spawn no agent?"
    },
    {
     "checks": [
      {
       "id": "AX-S22",
       "kind": "decide"
      },
      {
       "id": "AX-S23",
       "kind": "decide"
      },
      {
       "id": "AX-S24",
       "kind": "signal"
      }
     ],
     "cites": [
      "architect § The design round hands back its counts"
     ],
     "id": "AX-E12",
     "question": "Is the hand-back one line of the round's counts?"
    },
    {
     "checks": [
      {
       "id": "AX-S25",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A defect found at the design round is a finding of the change, outside any cycle",
      "reviewer § An Architect's finding carries its labels in its front matter"
     ],
     "id": "AX-E13",
     "question": "Is a defect the round finds a finding of the change, named for it, with the trigger design-round?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "AX-E1": [
       "architect § The design round writes the change's design beside its intent",
       "architect § The Context describes the code the change starts from"
      ],
      "AX-E10": [
       "architect § A question about a case nobody raised is not an open question",
       "architect § Each seam serves a scenario the goals need"
      ],
      "AX-E11": [
       "architect § The design round writes only the design, the tests and the test properties",
       "architect § No run commits, spawns an agent or writes a task",
       "architect § Probes leave their files in the scratch directory",
       "record § The change's directory holds what the design round leaves before any cycle"
      ],
      "AX-E12": [
       "architect § The design round hands back its counts"
      ],
      "AX-E13": [
       "architect § A defect found at the design round is a finding of the change, outside any cycle",
       "reviewer § An Architect's finding carries its labels in its front matter"
      ],
      "AX-E2": [
       "architect § The Seams table has the design's five columns",
       "architect § A seam row has a unique id and names its scenarios and tests",
       "architect § A seam row's statement names no implementation detail",
       "architect § Owning symbols are the boundary's entry points",
       "architect § A symbol the design introduces is named without a marker",
       "architect § A seam row holds the current state only"
      ],
      "AX-E3": [
       "architect § A Decision that replaces a mechanism accounts for what the old one gave",
       "architect § A Decision names no implementation detail",
       "architect § A Decision that names a seam links to it by id"
      ],
      "AX-E4": [
       "architect § A property dropped on purpose is marked dropped in its Decision"
      ],
      "AX-E5": [
       "architect § A scenario the code already has gets a plain test"
      ],
      "AX-E6": [
       "architect § A scenario the code lacks gets a pending test that cites it",
       "architect § A pending test is reported as pending, never as failing",
       "architect § A new test sits with the tests of the module it drives",
       "architect § A pending test drives real code"
      ],
      "AX-E7": [
       "architect § Every scenario of the change is cited by a test or marked none",
       "architect § A scenario with no test worth writing says why"
      ],
      "AX-E8": [
       "architect § An open question is an entry in the design's Open Questions",
       "architect § An open question's rows and tests are drafted in the recommended form"
      ],
      "AX-E9": [
       "architect § The intent's open questions join the design's"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AX-E1": [
       "AX-S1"
      ],
      "AX-E10": [
       "AX-S17"
      ],
      "AX-E11": [
       "AX-S18",
       "AX-S19",
       "AX-S20",
       "AX-S21"
      ],
      "AX-E12": [
       "AX-S22",
       "AX-S23",
       "AX-S24"
      ],
      "AX-E13": [
       "AX-S25"
      ],
      "AX-E2": [
       "AX-S2",
       "AX-S3",
       "AX-S4"
      ],
      "AX-E3": [
       "AX-S5",
       "AX-S6"
      ],
      "AX-E4": [
       "AX-S7",
       "AX-S8"
      ],
      "AX-E5": [
       "AX-S9"
      ],
      "AX-E6": [
       "AX-S10",
       "AX-S11"
      ],
      "AX-E7": [
       "AX-S12"
      ],
      "AX-E8": [
       "AX-S13",
       "AX-S14",
       "AX-S15"
      ],
      "AX-E9": [
       "AX-S16"
      ]
     },
     "items_then": [],
     "run": "20261114T113714",
     "trials": [
      {
       "calls": {
        "15": {
         "error": false,
         "text": "PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -t . -v 2>&1 | tail -20; echo \"exit=$?\"",
         "tool": "Bash"
        }
       },
       "checks": {
        "AX-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists beside the intent with Context, Decisions and Seams in order.",
          "verdict": "agrees"
         },
         "desc": "design.org written beside the intent, with Context, Decisions and Seams, Decisions before Seams",
         "detail": "docs/changes/csv-ledger/design.org: written; top-level headings: ['Context', 'Decisions', 'Seams', 'Open Questions']",
         "kind": "decide",
         "pass": true
        },
        "AX-S10": {
         "audit": {
          "agree": true,
          "reason": "The quoted-comma test is expectedFailure and the run reports it as an expected failure.",
          "verdict": "agrees"
         },
         "desc": "the scenario the code lacks: a pending test cites it, reported as an expected failure",
         "detail": "scenario: A quoted category holding a comma is read whole; citing: [('ParseLine.test_quoted_category_with_comma_is_read_whole', True, 'expected failure')]",
         "kind": "decide",
         "pass": true
        },
        "AX-S11": {
         "audit": {
          "agree": true,
          "reason": "The run is OK with 5 expected failures, and the five tests that passed before still pass.",
          "verdict": "agrees"
         },
         "desc": "the test run passes, no unexpected success, and every test that passed before still passes",
         "detail": "exit 0; now failing: none; Ran 10 tests in 0.079s\n\nOK (expected failures=5)",
         "kind": "decide",
         "pass": true
        },
        "AX-S12": {
         "audit": {
          "agree": true,
          "reason": "Every one of the three scenarios is cited by a test.",
          "verdict": "agrees"
         },
         "desc": "each of the change's scenarios cited by a test, or marked :TEST: none: with a reason",
         "detail": "scenarios ['A refund is read as a negative amount', 'A quoted category holding a comma is read whole', 'A malformed line is reported']; uncited: none",
         "kind": "decide",
         "pass": true
        },
        "AX-S13": {
         "audit": {
          "agree": false,
          "reason": "The Open Questions section exists with two entries, which is what the check computes.",
          "verdict": "agrees"
         },
         "desc": "the design has an Open Questions section with an entry",
         "detail": "2 entries: Q1 (intent): Should a ledger whose first line is a header (=date,category,amount; Q2: Is a line with three fields whose amount is not dollars and cents (=2026-10-",
         "kind": "decide",
         "pass": true
        },
        "AX-S14": {
         "audit": {
          "agree": true,
          "reason": "It matched Q2 on the word 'malformed', but Q2 asks whether a bad amount is malformed, not how a malformed line is handled.",
          "verdict": "suspect"
         },
         "desc": "an Open Questions entry's heading is about the malformed line",
         "detail": "Q2: Is a line with three fields whose amount is not dollars and cents (=2026-10-",
         "kind": "signal",
         "pass": true
        },
        "AX-S15": {
         "audit": {
          "agree": true,
          "reason": "It flags D2 correctly, but it cannot tell a Decision that settles the matter from the code at HEAD, with its reason, which the item's N/A clause allows and which holds here.",
          "verdict": "suspect"
         },
         "desc": "no Decision heading is about the malformed line",
         "detail": "D2: A malformed line stops the read and is reported once, with its line number i",
         "kind": "signal",
         "pass": false
        },
        "AX-S16": {
         "audit": {
          "agree": true,
          "reason": "Q1 carries (intent) and is the intent's question.",
          "verdict": "agrees"
         },
         "desc": "an Open Questions entry is marked (intent)",
         "detail": "Q1 (intent): Should a ledger whose first line is a header (=date,category,amount",
         "kind": "decide",
         "pass": true
        },
        "AX-S17": {
         "audit": {
          "agree": true,
          "reason": "No Open Questions entry mentions tabs.",
          "verdict": "agrees"
         },
         "desc": "no Open Questions entry is about a tab-separated file",
         "detail": "none",
         "kind": "signal",
         "pass": true
        },
        "AX-S18": {
         "audit": {
          "agree": true,
          "reason": "Only design.org and files under tests/ changed in the code root.",
          "verdict": "agrees"
         },
         "desc": "in the code root, only the design, test files and scenarios' :TEST: properties changed",
         "detail": "changed: tests/test_ledger.py docs/changes/csv-ledger/design.org tests/test_cli.py",
         "kind": "decide",
         "pass": true
        },
        "AX-S19": {
         "audit": {
          "agree": true,
          "reason": "Only the three finding files were added under .orchestrator/.",
          "verdict": "agrees"
         },
         "desc": "in the change's directory, only finding files added directly in findings/; the brief as it was",
         "detail": "none other",
         "kind": "decide",
         "pass": true
        },
        "AX-S2": {
         "audit": {
          "agree": true,
          "reason": "The header is the five columns and there are two rows.",
          "verdict": "agrees"
         },
         "desc": "the Seams table in the design's five columns, one row or more",
         "detail": "columns ['id', 'statement', 'owning symbols', 'scenarios', 'tests']; 2 row(s)",
         "kind": "decide",
         "pass": true
        },
        "AX-S20": {
         "audit": {
          "agree": true,
          "reason": "The tool calls contain no Agent or Task call and no git commit, branch or worktree command.",
          "verdict": "agrees"
         },
         "desc": "no commit, no new branch or worktree, no agent spawned",
         "detail": "refs and worktrees unchanged: True; spawns: 0",
         "kind": "decide",
         "pass": true
        },
        "AX-S21": {
         "audit": {
          "agree": true,
          "reason": "Every path in the calls is under the trial directory and no call runs mktemp.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AX-S22": {
         "audit": {
          "agree": true,
          "reason": "The final text is the one-line form, well under 200 characters.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line in the design round's form, at most 200 characters",
         "detail": "design-round csv-ledger: 2 seams, 5 pending tests, 0 uncited scenarios, 2 open questions",
         "kind": "decide",
         "pass": true
        },
        "AX-S23": {
         "audit": {
          "agree": true,
          "reason": "2 rows, 5 expected failures and 0 uncited match the hand-back.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's seams, pending tests and uncited scenarios agree with the end state (not applicable without the line)",
         "detail": "hand-back ('2', '5', '0'); end state 2 rows, 5 expected failures, 0 uncited",
         "kind": "decide",
         "pass": true
        },
        "AX-S24": {
         "audit": {
          "agree": true,
          "reason": "There are 2 Open Questions entries, matching the hand-back's count.",
          "verdict": "agrees"
         },
         "desc": "the hand-back's open questions count the Open Questions entries (not applicable without the line)",
         "detail": "hand-back 2; entries 2",
         "kind": "signal",
         "pass": true
        },
        "AX-S25": {
         "audit": {
          "agree": true,
          "reason": "All three files have a finding_id matching the file name and trigger design-round.",
          "verdict": "agrees"
         },
         "desc": "each finding file the run added is named arch-<change>-<n>, its id, with trigger design-round (not applicable with none)",
         "detail": "['arch-csv-ledger-1.md', 'arch-csv-ledger-2.md', 'arch-csv-ledger-3.md'] as the form says",
         "kind": "decide",
         "pass": true
        },
        "AX-S3": {
         "audit": {
          "agree": true,
          "reason": "Both ids are unique seam/ ids citing exact spec titles and existing test files.",
          "verdict": "agrees"
         },
         "desc": "each row: a unique seam/<name> id, scenarios cited by their exact titles, test files that exist",
         "detail": "2 row(s) well formed",
         "kind": "decide",
         "pass": true
        },
        "AX-S4": {
         "audit": {
          "agree": true,
          "reason": "No statement names a private helper or a test, and no cell has a line number, marker or date.",
          "verdict": "agrees"
         },
         "desc": "no row's statement names a private helper or a test, no owning symbol a line number, no row a marker or a date",
         "detail": "2 row(s) clean",
         "kind": "decide",
         "pass": true
        },
        "AX-S5": {
         "audit": {
          "agree": true,
          "reason": "D1 and D2 each carry in their headings the seam ids their text names.",
          "verdict": "agrees"
         },
         "desc": "each Decision that names a seam carries its id in its heading (not applicable with no Decision)",
         "detail": "2 Decision(s)",
         "kind": "decide",
         "pass": true
        },
        "AX-S6": {
         "audit": {
          "agree": true,
          "reason": "D1 names the stripped spaces and also the sign, cents, blank lines and comments.",
          "verdict": "agrees"
         },
         "desc": "a Decision names the whitespace property and another of the old parser's",
         "detail": "whitespace: True; another property: cents",
         "kind": "signal",
         "pass": true
        },
        "AX-S7": {
         "audit": {
          "agree": true,
          "reason": "D1 says the space stripping is dropped.",
          "verdict": "agrees"
         },
         "desc": "a Decision names the whitespace property as dropped",
         "detail": "D1: A line is read as comma-separated values in one place (s",
         "kind": "signal",
         "pass": true
        },
        "AX-S8": {
         "audit": {
          "agree": true,
          "reason": "intent.org does not appear in the diff or among the changed files.",
          "verdict": "agrees"
         },
         "desc": "the intent as the fixture left it",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "AX-S9": {
         "audit": {
          "agree": true,
          "reason": "The diff shows only a docstring added to test_refund_is_negative, and the run reports it ok.",
          "verdict": "agrees"
         },
         "desc": "the scenario the code has: the existing test that pins it gains its citation and nothing else, not pending, passing",
         "detail": "scenario: A refund is read as a negative amount; tests citing it: ['ParseLine.test_refund_is_negative']; ParseLine.test_refund_is_negative body and decorators unchanged: True; outcome ok",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AX-E1": "pass",
        "AX-E10": "pass",
        "AX-E11": "pass",
        "AX-E12": "pass",
        "AX-E13": "pass",
        "AX-E2": "pass",
        "AX-E3": "pass",
        "AX-E4": "pass",
        "AX-E5": "pass",
        "AX-E6": "pass",
        "AX-E7": "pass",
        "AX-E8": "n/a",
        "AX-E9": "pass"
       },
       "judge": {
        "AX-E1": {
         "quotes": [
          "* Context\n=tally FILE= prints one total per category ... * Decisions ... * Seams",
          "At the base, a refund line (=2026-10-02,food,-3.50=) is read as -350 cents; a quoted category holding a comma ... splits into four fields and ends in a traceback",
          "* Context ... * Decisions ... * Seams ... * Open Questions; \"=parse_line= splits a line's text on every comma, strips spaces around each field\""
         ],
         "reasons": [
          "The design is at docs/changes/csv-ledger/design.org beside intent.org, with Context, Decisions and Seams in that order, and its Context describes parse_line, load, report and cli as they stand at the base, including what each sample line does there.",
          "docs/changes/csv-ledger/design.org sits beside the intent with Context, Decisions, then Seams, and its Context describes parse_line, load, report and cli as they stand at the base.",
          "The design is at docs/changes/csv-ledger/design.org beside the intent, with Decisions before Seams, and its Context describes parse_line, load, the report reader and cli as they stand at the base."
         ]
        },
        "AX-E10": {
         "quotes": [
          "thought: tab-separated ledgers (intent § Why says the spreadsheet can save them) are no goal; no delimiter option is designed.",
          "thought: tab-separated ledgers (intent § Why says the spreadsheet can save them) are no goal; no delimiter option is designed.",
          "thought: tab-separated ledgers ... are no goal; no delimiter option is designed."
         ],
         "reasons": [
          "Tab-separated files appear only as a thought line; there is no Open Questions entry, seam or option for them.",
          "Tab separation appears only as a thought line; it has no Open Questions entry, seam or option.",
          "No Open Questions entry, seam or option is about tab-separated files; the case appears only as a thought line."
         ]
        },
        "AX-E11": {
         "quotes": [
          "changed: tests/test_ledger.py docs/changes/csv-ledger/design.org tests/test_cli.py",
          "changed: tests/test_ledger.py docs/changes/csv-ledger/design.org tests/test_cli.py",
          "changed: tests/test_ledger.py docs/changes/csv-ledger/design.org tests/test_cli.py"
         ],
         "reasons": [
          "The code root changed only in the design and test files, the change's directory gained only findings, the probe files and the thought file stayed in the scratch directory as the brief allows, and there was no commit, ref change or agent call.",
          "The code root changed only the design and test files, and the change's directory gained only findings files; the scratch directory holds the thought file plus probe files (which the brief allows there), with no commit, no agent and no lasting file outside the trial directory.",
          "The code root gained only the design and test files, the change's directory gained only findings/ files, and scratch holds only the thought file and probe files; there is no commit, no agent and no path outside the trial directory."
         ]
        },
        "AX-E12": {
         "quotes": [
          "design-round csv-ledger: 2 seams, 5 pending tests, 0 uncited scenarios, 2 open questions",
          "design-round csv-ledger: 2 seams, 5 pending tests, 0 uncited scenarios, 2 open questions",
          "design-round csv-ledger: 2 seams, 5 pending tests, 0 uncited scenarios, 2 open questions"
         ],
         "reasons": [
          "The hand-back is one line of the required form, and its counts match the 2 rows, the 5 expected failures, 0 uncited scenarios and 2 Open Questions entries.",
          "The hand-back is one line in the required form, and its counts match the 2 rows, 5 expected failures, 0 uncited scenarios and 2 entries.",
          "The hand-back is a single line in the required form, and its counts match the 2 rows, 5 expected failures, 0 uncited scenarios and 2 Open Questions entries."
         ]
        },
        "AX-E13": {
         "quotes": [
          "finding_id: arch-csv-ledger-1\ntrigger: design-round",
          "finding_id: arch-csv-ledger-1\ntrigger: design-round",
          "finding_id: arch-csv-ledger-1\ntrigger: design-round"
         ],
         "reasons": [
          "Each of the three defects (negative money formatting, the --json docstring, coverage.py's unittest blind spot) is one file directly in the change's findings/, its finding_id matching its file name, its trigger design-round, and none has an Open Questions entry.",
          "Each of the three findings is a defect in existing code or text, sits directly in the change's findings/ under its finding_id name with trigger design-round, and has no Open Questions entry.",
          "Each of the three defects is its own file in the change's findings/, named arch-csv-ledger-<n>.md, with a matching finding_id and trigger design-round, and none has an Open Questions entry."
         ]
        },
        "AX-E2": {
         "quotes": [
          "| id | statement | owning symbols | scenarios | tests |",
          "| seam/malformed-line | =load= stops at the first line that is not three fields ... | =tally/ledger.py=: =load=, =MalformedLine=; =tally/cli.py=: =main= | ledger § A malformed line is reported | =tests/test_cli.py= |",
          "| id | statement | owning symbols | scenarios | tests |"
         ],
         "reasons": [
          "The table has the five columns and two unique seam/ ids; the scenario titles match the spec exactly; the test files exist; the statements name no guard, error string, encoding, helper or test; the owning symbols have no line numbers or markers.",
          "The table has the five columns and two unique seam/ ids, cites exact spec titles, and names test files that exist; no statement names a guard, an error string, an encoding, a helper or a test, and no row carries a line number, marker or date.",
          "There are two unique seam/ ids, their scenario titles match the spec exactly, tests/test_ledger.py and tests/test_cli.py exist, no statement names a guard, error string, encoding, helper or test, owning symbols carry no line numbers, and no row has a marker, status or date."
         ]
        },
        "AX-E3": {
         "quotes": [
          "- A leading minus making the amount negative: kept (intent G2).\n- Reading the amount as dollars and cents: kept.\n- Stripping spaces around each field: dropped (intent N1).",
          "- Splitting on commas: kept, with a quoted field read whole.\n- A leading minus making the amount negative: kept (intent G2).\n- Reading the amount as dollars and cents: kept.\n- Stripping spaces around each field: dropped (intent N1).\n- =load= passing over blank lines and lines beginning with =#=: kep … [cut, 322 characters]",
          "What the hand-written split provided, and whether the replacement keeps it: ... A leading minus making the amount negative: kept ... Stripping spaces around each field: dropped"
         ],
         "reasons": [
          "D1 lists each property of the old parser and of load and says whether the CSV reader keeps it; seam/ledger-line states every kept property; D1 says why, names no guard or helper, and carries the seam id in its heading.",
          "D1 accounts for each property of the old parser, including blank and comment lines; every kept property is stated in seam/ledger-line, and the heading carries that seam's id.",
          "D1 lists the comma split, the sign, dollars and cents, whitespace stripping and the passing over of blank and comment lines with kept or dropped for each; the kept ones are in seam/ledger-line's statement, and the heading carries that seam's id."
         ]
        },
        "AX-E4": {
         "quotes": [
          "Stripping spaces around each field: dropped (intent N1).",
          "Stripping spaces around each field: dropped (intent N1).",
          "Stripping spaces around each field: dropped (intent N1)."
         ],
         "reasons": [
          "D1 marks the whitespace property dropped, and intent.org is not in the diff (S8 reports it unchanged).",
          "D1 marks the whitespace property dropped on purpose, and no tool call writes intent.org.",
          "D1 marks whitespace stripping as dropped on purpose, and the intent is not in the diff or among the changed files."
         ]
        },
        "AX-E5": {
         "quotes": [
          "+        \"\"\"Scenario: ledger § A refund is read as a negative amount\"\"\"",
          "     def test_refund_is_negative(self):\n+        \"\"\"Scenario: ledger § A refund is read as a negative amount\"\"\"",
          "+        \"\"\"Scenario: ledger § A refund is read as a negative amount\"\"\""
         ],
         "reasons": [
          "The only change to test_refund_is_negative is the docstring citation; it is not marked pending and the run reports it ok.",
          "The only change to the test is the citation docstring; it is not pending, and the run reports it ok.",
          "The only change to test_refund_is_negative is the citation docstring; it is not marked pending, and the run reports it ok."
         ]
        },
        "AX-E6": {
         "quotes": [
          "@unittest.expectedFailure\n    def test_quoted_category_with_comma_is_read_whole ... parse_line('2026-10-01,\"rent, garage\",900.00')",
          "test_quoted_category_with_comma_is_read_whole (tests.test_ledger.ParseLine...) ... expected failure",
          "@unittest.expectedFailure\n    def test_quoted_category_with_comma_is_read_whole ... parse_line('2026-10-01,\"rent, garage\",900.00')"
         ],
         "reasons": [
          "The test is in tests/test_ledger.py, drives parse_line, cites the scenario and is marked expectedFailure; the run reports it as an expected failure, ends OK, and the earlier tests stay ok.",
          "The test is in tests/test_ledger.py, is marked expectedFailure, drives parse_line directly and is reported as an expected failure; the run is OK and every earlier test is still ok.",
          "The pending test is in tests/test_ledger.py, drives parse_line, cites the comma scenario and is reported as an expected failure; the run is OK and every earlier test still passes."
         ]
        },
        "AX-E7": {
         "quotes": [
          "\"\"\"Scenario: ledger § A malformed line is reported\"\"\"",
          "def test_malformed_line_is_named(self):\n        \"\"\"Scenario: ledger § A malformed line is reported\"\"\"",
          "\"\"\"Scenario: ledger § A malformed line is reported\"\"\""
         ],
         "reasons": [
          "Each of the three scenarios is cited by an existing test: refund and quoted comma in tests/test_ledger.py, malformed line in tests/test_cli.py.",
          "All three scenarios are cited by tests that exist: refund in test_ledger, quoted comma in test_ledger, and malformed line in test_cli.",
          "test_refund_is_negative, test_quoted_category_with_comma_is_read_whole and test_cli.py::test_malformed_line_is_named cite the three scenarios, one each."
         ]
        },
        "AX-E8": {
         "quotes": [
          "The run stops with no table, as it stops at the base: a table printed without the line would show totals that look whole and are not.",
          "The run stops with no table, as it stops at the base: a table printed without the line would show totals that look whole and are not.",
          "D2: A malformed line stops the read and is reported once ... The run stops with no table, as it stops at the base"
         ],
         "reasons": [
          "D2 settles stop-or-skip from the code at HEAD and says why, and the reason holds: the design's Context and the step-11 probe both show a two-field line already ends the run at the base.",
          "D2 settles stop-versus-skip from the code at HEAD, where a two-field line already ends the run with no table (shown in the Context and the probe), and says so; that reason holds, so the N/A clause applies.",
          "D2 settles stop-versus-skip from the code at HEAD, which ends the run on a two-field line (the Context confirms the traceback), and says why; the N/A condition holds and the reason is sound."
         ]
        },
        "AX-E9": {
         "quotes": [
          "** Q1 (intent): Should a ledger whose first line is a header (=date,category,amount=) be read?",
          "** Q1 (intent): Should a ledger whose first line is a header (=date,category,amount=) be read?",
          "** Q1 (intent): Should a ledger whose first line is a header (=date,category,amount=) be read?"
         ],
         "reasons": [
          "The intent's question is copied with its wording and marked (intent), and it names the drafted seam clause and the pending test test_header_first_line_is_passed_over.",
          "The intent's question is carried with its own wording and options and marked (intent); its recommended form is drafted as seam/ledger-line's clause and a pending test, and the entry says so.",
          "The intent's question, options and recommendation appear word for word, marked (intent), with the seam clause and the pending test test_header_first_line_is_passed_over drafted in the recommended form."
         ]
        }
       },
       "milestones": [
        {
         "call": 15,
         "candidates": [
          15
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 10 tests in 0.087s\nOK (expected failures=5)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 25,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Architect's design round, from its brief (kept in the change's directory) to its hand-back",
   "title": "Case architect-replace: the design round on a change that replaces a mechanism, drops a property and leaves a question open",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T113714 t1 (skill-evals)"
  },
  {
   "capability": "architect",
   "id": "architect-rerun",
   "items": [
    {
     "checks": [
      {
       "id": "AU-S1",
       "kind": "decide"
      },
      {
       "id": "AU-S2",
       "kind": "decide"
      },
      {
       "id": "AU-S3",
       "kind": "decide"
      },
      {
       "id": "AU-S4",
       "kind": "decide"
      },
      {
       "id": "AU-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A run for one seam leaves the rest of the design as it was",
      "design-round § A re-run changes only its seam"
     ],
     "id": "AU-E1",
     "question": "Does the re-run leave every other row, the rest of the design and every other test as it was?"
    },
    {
     "checks": [
      {
       "id": "AU-S7",
       "kind": "decide"
      },
      {
       "id": "AU-S8",
       "kind": "decide"
      },
      {
       "id": "AU-S9",
       "kind": "decide"
      },
      {
       "id": "AU-S10",
       "kind": "signal"
      }
     ],
     "cites": [
      "design-round § A re-run changes only its seam",
      "architect § A seam row's statement names no implementation detail",
      "architect § A pending test is reported as pending, never as failing",
      "architect § Every scenario of the change is cited by a test or marked none"
     ],
     "id": "AU-E2",
     "question": "Does the re-run change the seam's row and its pending test as the answer implies, keeping the row a contract?"
    },
    {
     "checks": [
      {
       "id": "AU-S6",
       "kind": "decide"
      },
      {
       "id": "AU-S11",
       "kind": "decide"
      },
      {
       "id": "AU-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The design round writes only the design, the tests and the test properties",
      "architect § No run commits, spawns an agent or writes a task",
      "architect § Probes leave their files in the scratch directory"
     ],
     "id": "AU-E3",
     "question": "Does the re-run write only the design, the tests and the test properties, commit nothing and spawn no agent?"
    },
    {
     "checks": [
      {
       "id": "AU-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § The design round hands back its counts"
     ],
     "id": "AU-E4",
     "question": "Is the hand-back the design round's one line?"
    },
    {
     "checks": [
      {
       "id": "AU-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "architect § A defect found at the design round is a finding of the change, outside any cycle",
      "reviewer § An Architect's finding carries its labels in its front matter"
     ],
     "id": "AU-E5",
     "question": "Is a defect the round finds a finding of the change, named for it, with the trigger design-round?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "37adbe1",
     "cites": {
      "AU-E1": [
       "architect § A run for one seam leaves the rest of the design as it was",
       "design-round § A re-run changes only its seam"
      ],
      "AU-E2": [
       "design-round § A re-run changes only its seam",
       "architect § A seam row's statement names no implementation detail",
       "architect § A pending test is reported as pending, never as failing",
       "architect § Every scenario of the change is cited by a test or marked none"
      ],
      "AU-E3": [
       "architect § The design round writes only the design, the tests and the test properties",
       "architect § No run commits, spawns an agent or writes a task",
       "architect § Probes leave their files in the scratch directory"
      ],
      "AU-E4": [
       "architect § The design round hands back its counts"
      ],
      "AU-E5": [
       "architect § A defect found at the design round is a finding of the change, outside any cycle",
       "reviewer § An Architect's finding carries its labels in its front matter"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "AU-E1": [
       "AU-S1",
       "AU-S2",
       "AU-S3",
       "AU-S4",
       "AU-S5"
      ],
      "AU-E2": [
       "AU-S10",
       "AU-S7",
       "AU-S8",
       "AU-S9"
      ],
      "AU-E3": [
       "AU-S11",
       "AU-S12",
       "AU-S6"
      ],
      "AU-E4": [
       "AU-S13"
      ],
      "AU-E5": [
       "AU-S14"
      ]
     },
     "items_then": [],
     "run": "20261114T102118",
     "trials": [
      {
       "calls": {
        "11": {
         "error": false,
         "text": "cd <trial>/home/.worktrees/tally/json-report; cat tests/test_report.py; python3 -m unittest discover -s tests -t . 2>&1 | tail -5; python3 -c \"from tally import report, ledger; print(repr(report._money(-1205)), ledger.parse_line('2026-10-01,food,-12.05'))\"; S=<trial>/skills/orchestrate; python3 $S/scripts/coverage.py --help 2>&1 | head -20",
         "tool": "Bash"
        },
        "14": {
         "error": false,
         "text": "cd <trial>/home/.worktrees/tally/json-report; python3 - <<'EOF'\np=\"tests/test_cli.py\"; s=open(p).read()\ns=s.replace('    \"\"\"Run tally on argv; return (status, stdout, stderr).\"\"\"','    \"\"\"Run tally on argv; return (status, stdout, stderr), the streams stripped.\"\"\"')\ns=s.replace('    return status, out.getvalue(), err.getvalue()','    return status, out.getvalue().strip(), err.getvalue().strip()')\ns=s.replace('\"tally: unknown option --yaml\\\\n\"))','\"tally: unknown option --yaml\"))')\ns=s.replace('\"tally: unknown option --food.csv\\\\n\"))','\"tally: unknown option --food.csv\"))')\ns=s.replace(\"\"\"'{\"re … [cut, 841 characters]",
         "tool": "Bash"
        },
        "15": {
         "error": false,
         "text": "SC=<trial>/scratch/architect-design-round; mkdir -p $SC/probe && cd <trial>/home/.worktrees/tally/json-report && cp -R tally tests $SC/probe/ && cd $SC/probe && cat >> tally/report.py <<'EOF'\n\n\ndef format_json(totals):\n    import json\n    return json.dumps({c: _money(totals[c]) for c in ordered(totals)})\nEOF\ncat > tally/cli.py <<'EOF'\nimport sys\nfrom tally import ledger, report\n\ndef main(argv=None):\n    argv = sys.argv[1:] if argv is None else argv\n    as_json, files, rest = False, [], False\n    for a in argv:\n        if rest: files.append(a)\n        elif a == \"--\": rest = True\n        elif a  … [cut, 1101 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "AU-S1": {
         "audit": {
          "agree": true,
          "reason": "The design diff shows the row ids are unchanged and seam/json-report's row is untouched, which matches the pass.",
          "verdict": "agrees"
         },
         "desc": "every row but the seam's as it was, none added or removed",
         "detail": "rows before ['seam/json-report', 'seam/options'], now ['seam/json-report', 'seam/options']; other rows changed: none",
         "kind": "decide",
         "pass": true
        },
        "AU-S10": {
         "audit": {
          "agree": true,
          "reason": "Both the row and test_cli.py changed, as the diffs show.",
          "verdict": "agrees"
         },
         "desc": "the seam's row or its tests changed",
         "detail": "row changed: True; test files changed: ['test_cli.py']",
         "kind": "signal",
         "pass": true
        },
        "AU-S11": {
         "audit": {
          "agree": true,
          "reason": "The tool calls include no Agent or Task call and no git commit, branch or worktree command.",
          "verdict": "agrees"
         },
         "desc": "no commit, ref or worktree made, no agent spawned",
         "detail": "refs and worktrees unchanged: True; spawns: none",
         "kind": "decide",
         "pass": true
        },
        "AU-S12": {
         "audit": {
          "agree": true,
          "reason": "Every path the calls name is under <run>/, and no call runs mktemp.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AU-S13": {
         "audit": {
          "agree": true,
          "reason": "The final text is exactly the one line of the required form.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line in the design round's form, at most 200 characters",
         "detail": "design-round json-report: 2 seams, 6 pending tests, 0 uncited scenarios, 0 open questions",
         "kind": "decide",
         "pass": true
        },
        "AU-S14": {
         "audit": {
          "agree": true,
          "reason": "Both finding files have a finding_id equal to the file name and trigger: design-round in their front matter.",
          "verdict": "agrees"
         },
         "desc": "each finding file the run added is named arch-<change>-<n>, its id, with trigger design-round (not applicable with none)",
         "detail": "['arch-json-report-1.md', 'arch-json-report-2.md'] as the form says",
         "kind": "decide",
         "pass": true
        },
        "AU-S2": {
         "audit": {
          "agree": true,
          "reason": "The seam/options row is present under its id in the design after the session.",
          "verdict": "agrees"
         },
         "desc": "the seam's row still there under its id",
         "detail": "| seam/options     | =tally='s command line is split into options and the file name in one place, before the ledger is read: an option counts wherever it stands, before or after the file name, and every word after =--= is the file name. An option =tally= does not know ends the run with the usage sta … [cut, 514 characters]",
         "kind": "decide",
         "pass": true
        },
        "AU-S3": {
         "audit": {
          "agree": true,
          "reason": "The design diff has a single hunk, confined to seam/options' row.",
          "verdict": "agrees"
         },
         "desc": "the design outside the seam's row as it was",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "AU-S4": {
         "audit": {
          "agree": true,
          "reason": "Only tests/test_cli.py changed, and it is the file the seam's tests cell names.",
          "verdict": "agrees"
         },
         "desc": "only the seam's test files changed under tests/",
         "detail": "changed ['test_cli.py']; the seam's test files ['test_cli.py']",
         "kind": "decide",
         "pass": true
        },
        "AU-S5": {
         "audit": {
          "agree": true,
          "reason": "The docs/specs/ diff is empty, so no line outside :TEST: properties changed.",
          "verdict": "agrees"
         },
         "desc": "in docs/specs, only :TEST: properties of the seam's scenarios changed",
         "detail": "none other",
         "kind": "decide",
         "pass": true
        },
        "AU-S6": {
         "audit": {
          "agree": true,
          "reason": "The changed files listed are exactly design.org, test_cli.py and two finding files, and the scratch writes the item allows are rightly outside the home and code root.",
          "verdict": "agrees"
         },
         "desc": "only the design, test files, spec files and finding files changed in the home and the code root",
         "detail": "changed: .orchestrator/changes/json-report/findings/arch-json-report-1.md .orchestrator/changes/json-report/findings/arch-json-report-2.md .worktrees/tally/json-report/docs/changes/json-report/design.org .worktrees/tally/json-report/tests/test_cli.py",
         "kind": "decide",
         "pass": true
        },
        "AU-S7": {
         "audit": {
          "agree": true,
          "reason": "The verbose run shows exit OK with 6 expected failures, no unexpected success, and the four earlier ok tests still ok.",
          "verdict": "agrees"
         },
         "desc": "the test run passes, no unexpected success, every test that passed before still passes",
         "detail": "exit 0; now failing: none; Ran 10 tests in 0.004s\n\nOK (expected failures=6)",
         "kind": "decide",
         "pass": true
        },
        "AU-S8": {
         "audit": {
          "agree": true,
          "reason": "Several test methods carry 'Scenario: reporting § An unknown option is refused', so the seam's one scenario is cited.",
          "verdict": "agrees"
         },
         "desc": "each scenario the seam serves is cited by a test",
         "detail": "scenarios ['reporting § An unknown option is refused']; uncited none",
         "kind": "decide",
         "pass": true
        },
        "AU-S9": {
         "audit": {
          "agree": true,
          "reason": "The statement contains neither the literal error message nor any _name helper. The check is narrower than the item (it does not look for guards or encodings), but its verdict fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the seam's statement names no error string and no private helper",
         "detail": "\"=tally='s command line is split into options and the file name in one place, before the ledger is read: an option counts wherever it stands, before or after the file name, and every word after =--= is the file name. An option =tally= does not know ends the run with the usage status and a message on … [cut, 314 characters]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AU-E1": "pass",
        "AU-E2": "pass",
        "AU-E3": "pass",
        "AU-E4": "pass",
        "AU-E5": "pass"
       },
       "judge": {
        "AU-E1": {
         "quotes": [
          "-| seam/options     | An option =tally= does not know ends the run ... +| seam/options     | =tally='s command line is split into options and the file name in one place",
          "-| seam/options ... +| seam/options ...  (only hunk in design.org); tests diff: only tests/test_cli.py, additions after line 21; docs/specs/: (none)",
          "-| seam/options     | An option =tally= does not know ends the run ... +| seam/options     | =tally='s command line is split into options and the file name in one place ..."
         ],
         "reasons": [
          "The design diff touches only seam/options' row, no row was added or removed, only tests/test_cli.py (the seam's test file) changed, and docs/specs/ shows no diff.",
          "The design diff touches only seam/options's row, the seam/json-report row and every other line are unchanged, no row was added or removed, the only test file changed is the seam's own test_cli.py (its existing test untouched), and nothing in docs/specs/ changed.",
          "The design diff touches only seam/options' row and no row is added or removed; the tests diff only appends to tests/test_cli.py (the seam's test file), leaving the existing Options test unchanged; docs/specs/ is unchanged."
         ]
        },
        "AU-E2": {
         "quotes": [
          "an option counts wherever it stands, before or after the file name, and every word after =--= is the file name",
          "an option counts wherever it stands, before or after the file name, and every word after =--= is the file name ... OK (expected failures=6)",
          "an option counts wherever it stands, before or after the file name, and every word after =--= is the file name ... OK (expected failures=6)"
         ],
         "reasons": [
          "The row now states the placement contract D4 implies and names no guard, error string, encoding or helper; the new tests are expectedFailure; the run reports 'OK (expected failures=6)' with the four previously passing tests still ok; the scenario is still cited.",
          "The row now states the D4 contract (option placement and the -- rule) with no guard, error string, encoding or helper. The new expectedFailure tests pin --json before or after the file and the -- file-name rule. The suite exits OK with all previously-ok tests still ok and the pending ones reported as expected failures. The seam's scenario is still cited, by test_unknown_option_refused and the two new refusal tests.",
          "The row now states the D4 contract and names no guard, error string, encoding or helper; the new expectedFailure tests pin --json before or after the file and the -- rule; the suite exits OK with the 4 previously passing tests still ok and the scenario still cited."
         ]
        },
        "AU-E3": {
         "quotes": [
          "changed: .orchestrator/changes/json-report/findings/arch-json-report-1.md ... design.org .worktrees/tally/json-report/tests/test_cli.py",
          "changed: .../findings/arch-json-report-1.md .../findings/arch-json-report-2.md .../design.org .../tests/test_cli.py",
          "changed: .../findings/arch-json-report-1.md .../findings/arch-json-report-2.md .../design.org .../tests/test_cli.py"
         ],
         "reasons": [
          "Only the design, test_cli.py and two finding files changed in the home and code root. Outside them the session wrote only the thought file and probe files in the scratch directory the brief gives for probes, made no commit, spawned no agent and touched no path outside the trial directory.",
          "In the home and code root only design.org, the test file and two finding files changed. Besides them only the thought file and probe copies were written, all in the brief's scratch directory, which the brief gives for probes. No commit, branch, worktree or Agent call was made, and no path outside the trial's directory was touched.",
          "In the home and code root only design.org, test_cli.py and two finding files changed; outside them only the thought file and probe copies in the scratch directory the brief gives for probes changed; there is no commit, no Agent call and no path outside the trial directory."
         ]
        },
        "AU-E4": {
         "quotes": [
          "design-round json-report: 2 seams, 6 pending tests, 0 uncited scenarios, 0 open questions",
          "design-round json-report: 2 seams, 6 pending tests, 0 uncited scenarios, 0 open questions",
          "design-round json-report: 2 seams, 6 pending tests, 0 uncited scenarios, 0 open questions"
         ],
         "reasons": [
          "The hand-back is a single line of the required form, well under 200 characters, and its counts match the design (2 rows) and the test run (6 expected failures).",
          "The final text is the single line of the required form, well under 200 characters, with nothing else; its counts match the suite's 6 expected failures and the design's 2 seams.",
          "The hand-back is one line in the required form; its counts match the design (2 seams), the run (expected failures=6), the coverage (both scenarios cited) and the absence of open questions."
         ]
        },
        "AU-E5": {
         "quotes": [
          "finding_id: arch-json-report-2\ntrigger: design-round",
          "finding_id: arch-json-report-2\ntrigger: design-round ... report._money(-1205) -> '-13.95'",
          "finding_id: arch-json-report-2\ntrigger: design-round ... report._money(-1205) -> '-13.95'"
         ],
         "reasons": [
          "Both defects are real: the spec has no scenario for D4, and _money(-1205) gives '-13.95'. Each is one file directly in the change's findings/, named arch-json-report-<n>.md with a matching finding_id and trigger design-round, and no Open Questions entry was added.",
          "Both defects (the D4 answer missing from the spec, and the negative-amount rendering bug, which the probe confirms) are files directly in the change's findings/. Each is named arch-json-report-<n>.md with a matching finding_id and trigger design-round, and no Open Questions entry was written.",
          "Both findings sit directly in the change's findings/, are named arch-json-report-<n>.md with matching finding_id and trigger design-round, describe defects in existing code or text (the -13.95 rendering is real), and have no Open Questions entry."
         ]
        }
       },
       "milestones": [
        {
         "call": 11,
         "candidates": [
          11
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=2)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 10 tests in 0.005s\nOK (expected failures=6)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 15,
         "candidates": [
          15
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 10 tests in 0.003s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 21,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Architect's design round run again for one seam, from its scoped brief to its hand-back",
   "title": "Case architect-rerun: the design round run again for one seam changes that seam alone",
   "trials": 3,
   "validated": "37adbe17f1918f495ab9abeeb6f70b762081e31d 20261114T102118 t1 (skill-evals)"
  },
  {
   "capability": "design-round",
   "id": "design-round-recheck",
   "items": [
    {
     "checks": [
      {
       "id": "DR-S4",
       "kind": "decide"
      },
      {
       "id": "DR-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "design-round § The Architect runs at its role default and the record keeps it",
      "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check",
      "design-round § The Architect works in the code root"
     ],
     "id": "DR-E3",
     "question": "Is the re-spawned Architect at its role default, in the code root?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "de7099e",
     "cites": {
      "DR-E3": [
       "design-round § The Architect runs at its role default and the record keeps it",
       "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check",
       "design-round § The Architect works in the code root"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "DR-E3": [
       "DR-S4",
       "DR-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T102951",
     "trials": [
      {
       "calls": {
        "10": {
         "error": true,
         "text": "Architect design round",
         "tool": "Agent"
        },
        "11": {
         "error": true,
         "text": "cd <trial>/home && export ORCH_CHANGE=json-report && python3 <trial>/skills/orchestrate/scripts/state.py record add agents '{\"agent_id\": \"afc02062b3a81141f\", \"role\": \"architect-design\", \"task\": \"design round\", \"model\": \"opus\", \"effort\": \"high\"}'; cd .worktrees/tally/json-report; git status --short; python3 <trial>/skills/orchestrate/scripts/coverage.py --intent docs/changes/json-report/intent.org; python3 -m unittest discover -s tests -t . 2>&1 | tail -5; ls ../../../.orchestrator/changes/json-report/findings 2>&1",
         "tool": "Bash"
        },
        "14": {
         "error": true,
         "text": "Architect design re-run",
         "tool": "Agent"
        },
        "15": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && python3 <trial>/skills/orchestrate/scripts/state.py record add agents '{\"agent_id\": \"a25ab4684841eea59\", \"role\": \"architect-design\", \"task\": \"design round re-run\", \"model\": \"opus\", \"effort\": \"high\"}' >/dev/null; cd .worktrees/tally/json-report; git status --short; python3 <trial>/skills/orchestrate/scripts/coverage.py --intent docs/changes/json-report/intent.org; python3 -m unittest discover -s tests -t . 2>&1 | tail -4; ls ../../../.orchestrator/changes/json-report/findings; cat ../../../.orchestrator/changes/json-report/findings/*; git diff; sed -n '/Seams/, … [cut, 688 characters]",
         "tool": "Bash"
        },
        "7": {
         "error": false,
         "text": "cd <trial>/home/.worktrees/tally/json-report && python3 <trial>/skills/orchestrate/scripts/coverage.py --intent docs/changes/json-report/intent.org; python3 -m unittest discover -s tests -t . 2>&1 | tail -3; mkdir -p <trial>/home/.orchestrator/changes/json-report/briefs <trial>/scratch/phase-design-json-report/architect-design-round",
         "tool": "Bash"
        }
       },
       "checks": {
        "DR-S4": {
         "audit": {
          "agree": true,
          "reason": "The second Agent input gives model opus and effort high, which is the role default the item expects.",
          "verdict": "agrees"
         },
         "desc": "the re-spawn at opus, and at high when an effort is given",
         "detail": "model opus, effort high",
         "kind": "decide",
         "pass": true
        },
        "DR-S5": {
         "audit": {
          "agree": true,
          "reason": "The call carries no isolation field, and no worktree was added, which matches working in the code root.",
          "verdict": "agrees"
         },
         "desc": "no worktree of the Architect's own: the call asks none and none is added",
         "detail": "isolation None; worktrees added since the build: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "DR-E3": "pass"
       },
       "judge": {
        "DR-E3": {
         "quotes": [
          "\"description\": \"Architect design re-run\", \"subagent_type\": \"claude\", \"model\": \"opus\", \"effort\": \"high\", \"run_in_background\": true",
          "\"description\": \"Architect design re-run\", \"subagent_type\": \"claude\", \"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\", ... You work in the code root `.../.worktrees/tally/json-report`"
         ],
         "reasons": [
          "The re-spawn is at opus/high with no isolation, its brief says it works in the code root, and the evidence shows no worktree added.",
          "The re-spawn gives opus at high with no isolation, its brief places it in the code root, and no worktree was added.",
          "The re-spawn is at opus/high, gives no isolation, points the Architect at the existing code root, and no worktree was added."
         ]
        }
       },
       "milestones": [
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 4 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 10,
         "candidates": [
          10
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Architect design round",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Architect design round",
         "scenarios": [],
         "sig": "spawn agent None"
        },
        {
         "call": 11,
         "candidates": [
          11
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=2))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.037s\nOK (expected failures=2)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Architect design re-run",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Architect design re-run",
         "scenarios": [],
         "sig": "spawn agent None"
        },
        {
         "call": 15,
         "candidates": [
          15
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=3))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.062s\nOK (expected failures=3)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 17,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the design round's phase agent, from its brief through the Architect's run, the check that fails and the re-spawn (stand-ins), to the round's commit and hand-back",
   "title": "Case design-round-recheck: the Architect's output leaves a scenario uncited, so the round re-spawns it with that failure as its scope, commits nothing until the second run passes, then commits",
   "trials": 3,
   "validated": "de7099e1c6e2d3da0ae5780f3d1247fefadd8715 20261114T102951 t1 (skill-evals)"
  },
  {
   "capability": "design-round",
   "id": "design-round-spawn",
   "items": [
    {
     "checks": [
      {
       "id": "DR-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "design-round § The Architect runs at its role default and the record keeps it",
      "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check"
     ],
     "id": "DR-E2",
     "question": "Is the Architect spawned at its role default for the design round?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "DR-E2": [
       "design-round § The Architect runs at its role default and the record keeps it",
       "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "DR-E2": [
       "DR-S2"
      ]
     },
     "items_then": [],
     "run": "20261114T102957",
     "trials": [
      {
       "calls": {
        "7": {
         "error": false,
         "text": "cd <trial>/home/.worktrees/tally/json-report && python3 <trial>/skills/orchestrate/scripts/coverage.py --intent docs/changes/json-report/intent.org; git diff 5a7e127 -- docs/specs/ | head -5; python3 -m unittest discover -s tests -t . 2>&1 | tail -3; git log --oneline main -3 2>&1; mkdir -p <trial>/scratch/phase-design-json-report/architect-design-round",
         "tool": "Bash"
        },
        "9": {
         "error": false,
         "text": "Architect design round",
         "tool": "Agent"
        }
       },
       "checks": {
        "DR-S2": {
         "audit": {
          "agree": true,
          "reason": "It reads model opus and effort high from the call, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the spawn at opus, and at high when an effort is given",
         "detail": "model opus, effort high",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "DR-E2": "pass"
       },
       "judge": {
        "DR-E2": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\""
         ],
         "reasons": [
          "The spawn gives opus at high, the Architect's design-round default.",
          "The spawn gives opus at high, the Architect's role default for the design round.",
          "The spawn gives model opus at effort high, the Architect's design-round default."
         ]
        }
       },
       "milestones": [
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 4 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 9,
         "candidates": [
          9
         ],
         "checks": [
          "DR-S1"
         ],
         "hits": {
          "DR-S1": [
           "Architect design round"
          ]
         },
         "label": "spawn: Architect design round",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Architect design round",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 9,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the design round's phase agent, from the dispatcher's brief to its first spawn (the Architect)",
   "title": "Case design-round-spawn: the design round's phase agent spawns the Architect with the whole brief, a not-met scenario among the change's",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T102957 t1 (skill-evals)"
  },
  {
   "capability": "design-round",
   "id": "design-round-user-design",
   "items": [
    {
     "checks": [
      {
       "id": "DR-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "design-round § The Architect runs at its role default and the record keeps it",
      "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check"
     ],
     "id": "DR-E2",
     "question": "Is the Architect spawned at its role default for the design round?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "6a8c2b2",
     "cites": {
      "DR-E2": [
       "design-round § The Architect runs at its role default and the record keeps it",
       "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "DR-E2": [
       "DR-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T103413-1",
     "trials": [
      {
       "calls": {
        "9": {
         "error": false,
         "text": "Architect design round",
         "tool": "Agent"
        }
       },
       "checks": {
        "DR-S4": {
         "audit": {
          "agree": true,
          "reason": "The first Agent call's input shows model opus and effort high, which is what the item expects.",
          "verdict": "agrees"
         },
         "desc": "the spawn at opus, and at high when an effort is given",
         "detail": "model opus, effort high",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "DR-E2": "pass"
       },
       "judge": {
        "DR-E2": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\""
         ],
         "reasons": [
          "The only Agent call gives model opus and effort high, the Architect's role default for the design round.",
          "The Agent call sets model opus and effort high, the Architect's role default for the design round.",
          "The Agent call gives model opus and effort high, the Architect's role default for the design round."
         ]
        }
       },
       "milestones": [
        {
         "call": 9,
         "candidates": [
          9
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Architect design round",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Architect design round",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 9,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the design round's phase agent, from the dispatcher's brief to its first spawn (the Architect), on a change with a design the user wrote",
   "title": "Case design-round-user-design: a design the user wrote before the round goes whole into the Architect's brief",
   "trials": 3,
   "validated": "6a8c2b2b9791dfa4135539ebc540ba532d1447c1 20261114T103413-1 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-start",
   "items": [
    {
     "checks": [
      {
       "id": "IS-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Each agent integrate spawns runs at its role default",
      "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check",
      "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
     ],
     "id": "IS-E5",
     "question": "Is the first spawn at its role's default?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "IS-E5": [
       "integrate § Each agent integrate spawns runs at its role default",
       "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check",
       "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "IS-E5": [
       "IS-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T114833",
     "trials": [
      {
       "calls": {
        "10": {
         "error": true,
         "text": "Architect conformance, cycle-1",
         "tool": "Agent"
        }
       },
       "checks": {
        "IS-S6": {
         "audit": {
          "agree": true,
          "reason": "Conformance at opus/high matches the Architect's conformance default.",
          "verdict": "agrees"
         },
         "desc": "the first spawn at its role's default",
         "detail": "conformance: ('opus', 'high'), its default ('opus', 'high')",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IS-E5": "pass"
       },
       "judge": {
        "IS-E5": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\""
         ],
         "reasons": [
          "Conformance's Architect was spawned at its role default of opus, high.",
          "Conformance's Architect is spawned at its role default, opus at high.",
          "Conformance's Architect was spawned at opus, high, which is its role default."
         ]
        }
       },
       "milestones": [
        {
         "call": 10,
         "candidates": [
          10
         ],
         "checks": [
          "IS-S7"
         ],
         "hits": {
          "IS-S7": [
           "Architect conformance, cycle-1"
          ]
         },
         "label": "spawn: Architect conformance, cycle-1",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Architect conformance, cycle-1",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 10,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent from the fixed brief (no handover), to its first spawn",
   "title": "Case integrate-start: the integrate phase agent's start, an answered ask first, then conformance or the whole-change review",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114833 t1 (skill-evals)"
  }
 ],
 "source": {
  "evals_head": "589177f",
  "spec": "orchestrate/docs/specs/architect.org",
  "spec_dirty": false,
  "worktree_head": "73b676c"
 },
 "spec": {
  "gaps": "No gap open. The nine this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/architect.org= § Gap decisions).",
  "purpose": "The Architect is the role agent that writes the change's design and keeps it true to the code. It runs three times. At the design round it writes the change's design, the pending tests and the scenarios' test properties. At a plan's premise check it checks what the tasks it is given claim about the code and writes a verdict on each claim. At an integrate's conformance it checks the merged code against the seam rows the cycle's tasks cited, amends a row the code has rightly outgrown, and lists the rows the cycle changed. In every run it reports each defect it finds in code or text that exists as a finding and routes none. Each run hands back one line. This spec states what each run writes. When a run happens, what it is given and what is done with its output are stated by =design-round=, =plan= and =integrate=; the labels a finding carries are stated by =reviewer=. The Architect commits nothing, spawns no agent and writes no task.",
  "requirements": [
   {
    "goal": "G5 (inferred; =outcomes.org= O-DR-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect's design round hands back"
       },
       {
        "kw": "THEN",
        "text": "the change's design is =docs/changes/<change>/design.org= in the code root, in the same directory as the intent"
       },
       {
        "kw": "AND",
        "text": "it has a Context, Decisions and Seams section, Decisions before Seams"
       }
      ],
      "test": null,
      "title": "The design round writes the change's design beside its intent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round writes the change's design"
       },
       {
        "kw": "THEN",
        "text": "its Context describes the code as it stands at the change's base, as far as the change needs it"
       }
      ],
      "test": null,
      "title": "The Context describes the code the change starts from"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round writes the Seams table"
       },
       {
        "kw": "THEN",
        "text": "its columns are =id=, =statement=, =owning symbols=, =scenarios= and =tests="
       },
       {
        "kw": "AND",
        "text": "each seam has one row"
       }
      ],
      "test": null,
      "title": "The Seams table has the design's five columns"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round hands back"
       },
       {
        "kw": "THEN",
        "text": "the only files it changed are the change's design, files of the test tree, and the spec files, where only scenarios' =:TEST:= properties changed"
       },
       {
        "kw": "AND",
        "text": "besides them it has written only a finding file for each defect it found (§ A defect found at the design round is a finding of the change, outside any cycle) and the thought file its brief names"
       }
      ],
      "test": null,
      "title": "The design round writes only the design, the tests and the test properties"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round is run again for one seam"
       },
       {
        "kw": "THEN",
        "text": "it changes that seam's row, the =:TEST:= properties of the scenarios the row serves and that seam's pending tests"
       },
       {
        "kw": "AND",
        "text": "every other row and every other test is as it was"
       }
      ],
      "test": null,
      "title": "A run for one seam leaves the rest of the design as it was"
     }
    ],
    "text": "The Architect's design round SHALL write the change's design, =docs/changes/<change>/design.org= in the code root, with a Context, Decisions, a Seams table and, while any question is open, Open Questions; and SHALL write the pending tests and the scenarios' test properties. It SHALL change no other file. A run for one seam SHALL change only that seam's row, the test properties of the scenarios it serves and its pending tests.",
    "title": "The design round writes the change's design and its tests, and nothing else"
   },
   {
    "goal": "G6 (=outcomes.org= O-NS-6)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes or amends a seam row"
       },
       {
        "kw": "THEN",
        "text": "its statement names none of these: a guard, an error string, an encoding, a helper's name, a test's name, a transient marker such as \"(new)\" or \"pending\""
       }
      ],
      "test": null,
      "title": "A seam row's statement names no implementation detail"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a seam row"
       },
       {
        "kw": "THEN",
        "text": "its statement says, in one or two sentences, what holds between the part of the code that produces a thing and the part that consumes it"
       },
       {
        "kw": "AND",
        "text": "it does not restate a scenario's behaviour"
       }
      ],
      "test": null,
      "title": "A seam is a boundary between two parts of the code, not a scenario restated"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a seam row"
       },
       {
        "kw": "THEN",
        "text": "its owning symbols name the file and the symbols another part of the code calls or reads at that boundary"
       },
       {
        "kw": "AND",
        "text": "not the helpers behind them, and no line number"
       }
      ],
      "test": null,
      "title": "Owning symbols are the boundary's entry points"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a seam's entry point does not yet exist at the code root's HEAD"
       },
       {
        "kw": "THEN",
        "text": "the row names the symbol the design introduces, with nothing marking it as new"
       }
      ],
      "test": null,
      "title": "A symbol the design introduces is named without a marker"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a seam row"
       },
       {
        "kw": "THEN",
        "text": "its id is =seam/<name>=, used by no other row of the change"
       },
       {
        "kw": "AND",
        "text": "its scenarios cell cites each scenario the seam serves as =<capability> § <title>=, with the title exactly as the spec has it"
       },
       {
        "kw": "AND",
        "text": "its tests cell names the test files that pin the seam"
       }
      ],
      "test": null,
      "title": "A seam row has a unique id and names its scenarios and tests"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes or amends a seam row"
       },
       {
        "kw": "THEN",
        "text": "the row carries no status, importance flag, dated note, history or message to a later role"
       }
      ],
      "test": null,
      "title": "A seam row holds the current state only"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round writes the Seams table"
       },
       {
        "kw": "THEN",
        "text": "each seam serves a scenario of the change that the intent's goals need, or has a demonstrated reason"
       },
       {
        "kw": "AND",
        "text": "no second implementation, configuration point or extension hook is designed for a case nobody raised"
       }
      ],
      "test": null,
      "title": "Each seam serves a scenario the goals need"
     }
    ],
    "text": "Each seam row the Architect writes or amends SHALL state, in one or two sentences, what must hold where one part of the code hands something to another; SHALL name as its owning symbols the boundary's entry points; SHALL cite the scenarios it serves by their titles and name the tests that pin it; and SHALL hold the current state only. It SHALL name no guard, error string, encoding, helper, test or transient marker. Each seam SHALL serve a scenario the change's goals need, or have a demonstrated reason.",
    "title": "A seam row states only what must hold at the boundary"
   },
   {
    "goal": "G6 (=outcomes.org= O-NS-6)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes or amends a Decision"
       },
       {
        "kw": "THEN",
        "text": "it states what must hold and why"
       },
       {
        "kw": "AND",
        "text": "it names none of these: a guard, an error string, an encoding, a helper's name"
       }
      ],
      "test": null,
      "title": "A Decision names no implementation detail"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Decision names a seam"
       },
       {
        "kw": "THEN",
        "text": "the Decision's heading carries the seam's id"
       }
      ],
      "test": null,
      "title": "A Decision that names a seam links to it by id"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Decision replaces a mechanism"
       },
       {
        "kw": "THEN",
        "text": "it names each property the old mechanism provided and whether the new one keeps it"
       },
       {
        "kw": "AND",
        "text": "each property kept is a seam row a Reviewer can cite"
       }
      ],
      "test": null,
      "title": "A Decision that replaces a mechanism accounts for what the old one gave"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Decision replaces a mechanism and drops one of its properties on purpose"
       },
       {
        "kw": "THEN",
        "text": "the Decision names that property as dropped"
       },
       {
        "kw": "AND",
        "text": "the Architect does not write the intent"
       }
      ],
      "test": null,
      "title": "A property dropped on purpose is marked dropped in its Decision"
     }
    ],
    "text": "Each Decision the Architect writes or amends SHALL record what must hold and why, SHALL name by id the seam it concerns, and SHALL name no guard, error string, encoding or helper. A Decision that replaces a mechanism SHALL name each property the old one provided and whether the new one keeps it.",
    "title": "A Decision states what must hold and why"
   },
   {
    "goal": "G5 (inferred; =outcomes.org= O-DR-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round hands back"
       },
       {
        "kw": "THEN",
        "text": "each scenario of the change is cited by a test that exists, pending or not, or carries =:TEST: none:= with a reason"
       }
      ],
      "test": null,
      "title": "Every scenario of the change is cited by a test or marked none"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a test can pin a scenario of the change and the code at HEAD does not yet have its behaviour"
       },
       {
        "kw": "THEN",
        "text": "the Architect writes a test for it that cites the scenario as =<capability> § <title>="
       },
       {
        "kw": "AND",
        "text": "the test is marked pending in the project's pending style"
       },
       {
        "kw": "AND",
        "text": "a test that stands in for a seam also cites the seam's id"
       }
      ],
      "test": null,
      "title": "A scenario the code lacks gets a pending test that cites it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the project's test command runs in the code root after the design round, before any task"
       },
       {
        "kw": "THEN",
        "text": "each pending test is reported as pending, not as a failure"
       },
       {
        "kw": "AND",
        "text": "a run that passed before the round still passes"
       }
      ],
      "test": null,
      "title": "A pending test is reported as pending, never as failing"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the code at HEAD already has a scenario's behaviour"
       },
       {
        "kw": "THEN",
        "text": "the test that cites it is not marked pending and passes at HEAD"
       },
       {
        "kw": "AND",
        "text": "when an existing test already pins it, that test gains the scenario's citation and nothing else in it changes"
       }
      ],
      "test": null,
      "title": "A scenario the code already has gets a plain test"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a test"
       },
       {
        "kw": "THEN",
        "text": "it is in the file that already tests the seam's owning module, or the module of the surface an acceptance test drives"
       },
       {
        "kw": "AND",
        "text": "it is in a new file only when no such file exists"
       }
      ],
      "test": null,
      "title": "A new test sits with the tests of the module it drives"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a pending test"
       },
       {
        "kw": "THEN",
        "text": "the test exercises the code itself"
       },
       {
        "kw": "AND",
        "text": "it asserts neither which module owns a behaviour nor that a symbol is absent"
       }
      ],
      "test": null,
      "title": "A pending test drives real code"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "no test is worth writing for a scenario: nothing a test can drive and no seam whose test would pin it, or a cost the Architect states"
       },
       {
        "kw": "THEN",
        "text": "the scenario carries =:TEST: none:= with that reason"
       },
       {
        "kw": "AND",
        "text": "the reason names the seam that stands in for a test, when there is one"
       }
      ],
      "test": null,
      "title": "A scenario with no test worth writing says why"
     }
    ],
    "text": "At the design round the Architect SHALL leave each scenario of the change cited by a test, pending or not, or carrying a =:TEST: none:= property with its reason. It SHALL write a pending test, citing its scenario and marked in the project's pending style, for each scenario a test can pin whose behaviour the code at HEAD does not yet have; and a plain test, or a citation added to an existing test, for each scenario whose behaviour the code already has.",
    "title": "Every scenario of the change is cited by a test or says why it has none"
   },
   {
    "goal": "G7 (=outcomes.org= O-DR-2)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round raises a question only the user can settle, which an intent goal or a scenario of the change needs answered"
       },
       {
        "kw": "THEN",
        "text": "the design has an Open Questions section with one entry for it"
       },
       {
        "kw": "AND",
        "text": "the entry gives the question in plain terms, two to four options each with the consequence the user would notice, the recommendation and its reason, and the seam rows and pending tests that depend on the answer"
       }
      ],
      "test": null,
      "title": "An open question is an entry in the design's Open Questions"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round writes an open question"
       },
       {
        "kw": "THEN",
        "text": "the seam rows and pending tests that depend on it are written as the recommendation implies"
       },
       {
        "kw": "AND",
        "text": "the entry says that they are"
       },
       {
        "kw": "AND",
        "text": "no Decision is written for the question"
       }
      ],
      "test": null,
      "title": "An open question's rows and tests are drafted in the recommended form"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the intent has open questions"
       },
       {
        "kw": "THEN",
        "text": "each is an entry of the design's Open Questions, as the intent words it and marked =(intent)="
       },
       {
        "kw": "AND",
        "text": "the seam rows and pending tests its recommendation implies are written with it"
       }
      ],
      "test": null,
      "title": "The intent's open questions join the design's"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round meets a question that neither an intent goal nor a scenario of the change needs answered"
       },
       {
        "kw": "THEN",
        "text": "the design's Open Questions has no entry for it"
       }
      ],
      "test": null,
      "title": "A question about a case nobody raised is not an open question"
     }
    ],
    "text": "Each question only the user can settle that the design round raises SHALL be an entry in the design's Open Questions, with its options, its recommendation and the rows and tests that depend on it; those rows and tests SHALL be drafted in the form the recommendation implies, and the entry SHALL say so; no Decision SHALL be written for an open question. The intent's open questions SHALL join them as written. A question about a case nobody raised SHALL not be an entry.",
    "title": "A design-round question is written for the user, never decided"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check runs on a cycle's candidate tasks"
       },
       {
        "kw": "THEN",
        "text": "the cycle's directory holds =premise-check.md="
       },
       {
        "kw": "AND",
        "text": "for each task it holds a table of the premises the task makes about the code at the code root's HEAD, each marked Holds or Wrong"
       },
       {
        "kw": "AND",
        "text": "a one-line verdict for each task"
       }
      ],
      "test": null,
      "title": "The premise check writes a table of premises per task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task file has a defect of its own form, such as a quote of a seam row cut short"
       },
       {
        "kw": "THEN",
        "text": "the defect is a row of that task's table, marked as a defect of the task file"
       }
      ],
      "test": null,
      "title": "A defect of the task file itself is a row of its table"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check finds a defect in code or text that exists, outside the scope of every task it was given"
       },
       {
        "kw": "THEN",
        "text": "it writes a finding for it, marked as found by the premise check, found from the cycle's batch"
       }
      ],
      "test": null,
      "title": "A defect outside every task's scope is a finding of the premise check"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check finds a seam row stale: the code or the test tree is right and the row is not"
       },
       {
        "kw": "THEN",
        "text": "it writes a finding of class =interface-drift= for the row, found by the premise check"
       },
       {
        "kw": "AND",
        "text": "it leaves the row as it is"
       }
      ],
      "test": null,
      "title": "A stale seam row is an interface-drift finding of the premise check"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check hands back, whether or not it found a seam row stale"
       },
       {
        "kw": "THEN",
        "text": "the change's design is as it was before the check"
       },
       {
        "kw": "AND",
        "text": "it has written no test"
       }
      ],
      "test": null,
      "title": "The premise check changes neither the design nor the tests"
     }
    ],
    "text": "The premise check SHALL write, in the cycle's directory, a table for each task it is given, with each premise the task makes about the code at the code root's HEAD marked as holding or wrong, and a one-line verdict for each task. A defect it finds outside every task's scope SHALL be a finding. It SHALL change neither the design nor any test.",
    "title": "The premise check writes a verdict on each premise and leaves the design alone"
   },
   {
    "goal": "G6 (inferred; =outcomes.org= O-IN-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cited seam row disagrees with the merged code and a test pins the code's behaviour on that point"
       },
       {
        "kw": "THEN",
        "text": "the Architect amends the row in place in the change's design"
       },
       {
        "kw": "AND",
        "text": "in the same edit it amends the Decision and the spec scenario steps that state the same thing"
       }
      ],
      "test": null,
      "title": "A row the code has rightly outgrown is amended in place"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect amends a seam row at conformance"
       },
       {
        "kw": "THEN",
        "text": "its finding of the drift records that the design was amended"
       },
       {
        "kw": "AND",
        "text": "names the test that pins the code's behaviour"
       }
      ],
      "test": null,
      "title": "An amendment's finding names the test that pins the code"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cited row is stale because the code gained a guard, an error string, an encoding or a helper"
       },
       {
        "kw": "THEN",
        "text": "the amended row names none of them, and a row that named one is cut back"
       },
       {
        "kw": "AND",
        "text": "no Decision gains one"
       }
      ],
      "test": null,
      "title": "An amendment cuts implementation detail and never adds it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design promises something the merged code does not do"
       },
       {
        "kw": "THEN",
        "text": "the Architect writes a finding against the code"
       },
       {
        "kw": "AND",
        "text": "the promise is still in the design"
       }
      ],
      "test": null,
      "title": "A promise the code does not keep is a finding against the code"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cited seam row and the merged code disagree and no test pins the code's behaviour on that point"
       },
       {
        "kw": "THEN",
        "text": "the Architect writes a finding that says what the seam is for, with its kind and its evidence"
       },
       {
        "kw": "AND",
        "text": "the Architect writes no ask"
       }
      ],
      "test": null,
      "title": "Without a pinning test, a disagreement is a finding that puts the design in question"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect amends a spec scenario's steps at conformance"
       },
       {
        "kw": "THEN",
        "text": "the scenario's title is as it was"
       }
      ],
      "test": null,
      "title": "An amendment never renames a scenario"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "at conformance a cited seam's owning symbols are gone or no longer the one producer or mapping, or a scenario the cycle's tasks cited does not pass"
       },
       {
        "kw": "THEN",
        "text": "the Architect writes a finding that names the seam"
       },
       {
        "kw": "AND",
        "text": "the seam row is not amended to match unless a test pins the code's behaviour"
       }
      ],
      "test": null,
      "title": "A cited seam the merged code no longer honours is a finding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a diff the cycle merged leaves a replaced implementation on a live path"
       },
       {
        "kw": "THEN",
        "text": "the Architect writes a finding for it"
       }
      ],
      "test": null,
      "title": "A replaced implementation still on a live path is a finding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "conformance hands back"
       },
       {
        "kw": "THEN",
        "text": "the cycle's directory holds =design-diff.json=, a list with one element for each seam row this cycle added, amended or retired"
       },
       {
        "kw": "AND",
        "text": "each element names the row, whether it was added, amended or retired, and the finding or ask behind the change"
       },
       {
        "kw": "AND",
        "text": "the list is empty when no row changed"
       }
      ],
      "test": null,
      "title": "Conformance lists the rows the cycle changed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "conformance finds no defect"
       },
       {
        "kw": "THEN",
        "text": "the change's journal gains one line saying conformance found nothing"
       }
      ],
      "test": null,
      "title": "A clean conformance says so in the journal"
     }
    ],
    "text": "At conformance the Architect SHALL check each seam row the cycle's tasks cited, and each diff the cycle merged, against the merged code. Where a test pins the code's behaviour and the row is stale, it SHALL amend the row in place, with the Decision and the scenario steps that state the same thing, keeping each to what must hold at the boundary. Where the code does not keep a promise of the design, or no test decides which side is right, it SHALL write a finding and leave the promise in the design. It SHALL list every row the cycle added, amended or retired.",
    "title": "Conformance makes the design say what the merged code does"
   },
   {
    "goal": "none stated (=outcomes.org= O-NS-12; =north-star.org= * Disposition states it, no goal does)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check or conformance finds a defect"
       },
       {
        "kw": "THEN",
        "text": "one file for that defect is in the =findings/= directory of the cycle's directory"
       },
       {
        "kw": "AND",
        "text": "it carries the labels the =reviewer= spec states for a finding"
       },
       {
        "kw": "AND",
        "text": "it names the run that found it, premise check or conformance, the task or the batch it was found from, and the seam it concerns when there is one"
       }
      ],
      "test": null,
      "title": "A finding is one file in the cycle's findings directory"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "at the design round the Architect finds a defect in code or text that exists"
       },
       {
        "kw": "THEN",
        "text": "one finding file for it is in the =findings/= directory of the change's directory, outside any cycle"
       },
       {
        "kw": "AND",
        "text": "its id is =arch-<change>-<n>= and its =trigger= is =design-round="
       },
       {
        "kw": "AND",
        "text": "the design's Open Questions has no entry for it"
       }
      ],
      "test": null,
      "title": "A defect found at the design round is a finding of the change, outside any cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a finding"
       },
       {
        "kw": "THEN",
        "text": "its class is one of =shape-fragmentation=, =vocabulary-mismatch=, =responsibility-leakage=, =dead-branch=, =interface-drift=, =mutation=, =invariant-gap= and =duplication="
       }
      ],
      "test": null,
      "title": "A finding carries the class of the design checklist"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a finding"
       },
       {
        "kw": "THEN",
        "text": "it says in one sentence why the tests missed the defect"
       },
       {
        "kw": "AND",
        "text": "it gives a recommended resolution in one paragraph that names the files and functions to change"
       },
       {
        "kw": "AND",
        "text": "when the Architect amended a seam row, the resolution names the row and how it was amended"
       }
      ],
      "test": null,
      "title": "A finding says why the tests missed it and how to resolve it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "at the premise check or conformance the Architect finds a defect in code or text that exists, however unlikely it is to trigger"
       },
       {
        "kw": "THEN",
        "text": "it writes a finding for it"
       }
      ],
      "test": null,
      "title": "Every defect is a finding, whatever its likelihood"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the code needs something today, such as a variable read where nothing on its load path defines it, or a guard that lets through what it should refuse"
       },
       {
        "kw": "THEN",
        "text": "the Architect writes a finding of kind =issue= for it, whatever its severity"
       },
       {
        "kw": "AND",
        "text": "the journal has no thought line for it"
       }
      ],
      "test": null,
      "title": "A need the code has today is an issue, never a thought"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "at the premise check or conformance the Architect notices a need that would arise only after a future change, or a cleanup that changes no behaviour today, and no project prior asks for that kind of work"
       },
       {
        "kw": "THEN",
        "text": "the change's journal gains one thought line for it"
       },
       {
        "kw": "AND",
        "text": "no finding file is written for it"
       }
      ],
      "test": null,
      "title": "A speculative need or a cleanup is a thought line, not a finding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "at the design round the Architect notices a speculative need or a cleanup that changes no behaviour today"
       },
       {
        "kw": "THEN",
        "text": "it writes one thought line for it in the file of its scratch directory that its brief names"
       },
       {
        "kw": "AND",
        "text": "its hand-back stays one line"
       }
      ],
      "test": null,
      "title": "A design-round thought goes to the file its brief names"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect notices a cleanup that changes no behaviour today, and one of the project's priors asks for that kind of work"
       },
       {
        "kw": "THEN",
        "text": "the change's journal gains one thought line for it that names the prior"
       },
       {
        "kw": "AND",
        "text": "no finding file is written for it"
       }
      ],
      "test": null,
      "title": "A cleanup a project prior asks for is a thought line naming the prior"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect writes a finding"
       },
       {
        "kw": "THEN",
        "text": "the finding carries no decoration, neither blocking nor hardening"
       }
      ],
      "test": null,
      "title": "The Architect writes no route for a finding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect finds that merged code breaks a cited seam's promise, by a second producer, an inlined mapping or a promoted test that fails, or that it loses data"
       },
       {
        "kw": "THEN",
        "text": "the finding's severity is =high="
       }
      ],
      "test": null,
      "title": "A finding that breaks a cited seam is of high severity"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect would only have written the code differently by taste, in naming, formatting or an equivalent structure"
       },
       {
        "kw": "THEN",
        "text": "there is neither a finding nor a thought line for it"
       }
      ],
      "test": null,
      "title": "Style and preference are not reported"
     }
    ],
    "text": "At each of its runs the Architect SHALL report each defect it finds in code or text that exists, whatever its likelihood, as a finding: one file per finding in the cycle's directory, carrying the labels the =reviewer= spec states for a finding, the run that found it, its class, the seam it concerns when there is one, and a recommended resolution. It SHALL write no route for a finding. A speculative need, or a cleanup that changes no behaviour today, SHALL be one thought line in the change's journal and not a finding. Style and preference SHALL not be reported.",
    "title": "Every defect the Architect finds is a finding, and it routes none"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design round ends"
       },
       {
        "kw": "THEN",
        "text": "it hands back one line, =design-round <change>: <n> seams, <k> pending tests, <u> uncited scenarios, <q> open questions=, and nothing else"
       },
       {
        "kw": "AND",
        "text": "the line is at most 200 characters"
       }
      ],
      "test": null,
      "title": "The design round hands back its counts"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check ends"
       },
       {
        "kw": "THEN",
        "text": "it hands back one line, =premise-check <cycle-id>: <n> holds, <m> wrong, <f> findings; <path>=, and nothing else"
       },
       {
        "kw": "AND",
        "text": "=<path>= is the path of its =premise-check.md=, relative to the home"
       },
       {
        "kw": "AND",
        "text": "the line is at most 200 characters"
       }
      ],
      "test": null,
      "title": "The premise check hands back its counts and its file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "conformance ends"
       },
       {
        "kw": "THEN",
        "text": "it hands back one line that starts =conformance <cycle-id>: <f> findings, <a> rows amended;= and nothing else"
       },
       {
        "kw": "AND",
        "text": "the line is at most 200 characters"
       }
      ],
      "test": null,
      "title": "Conformance hands back its counts"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "conformance hands back"
       },
       {
        "kw": "THEN",
        "text": "its line ends =; <path>=, the path of the cycle's =design-diff.json= relative to the home, which every conformance run writes"
       }
      ],
      "test": null,
      "title": "Conformance's hand-back names its design diff"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check hands back"
       },
       {
        "kw": "THEN",
        "text": "the files it has written or changed are =premise-check.md= and its finding files in the cycle's directory"
       },
       {
        "kw": "AND",
        "text": "besides them it has added only thought lines to the change's journal and the state's entry for each finding it wrote (=record= § A finding in a file of its own is indexed in the state)"
       }
      ],
      "test": null,
      "title": "The premise check writes only its table, its findings and thought lines"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "conformance hands back"
       },
       {
        "kw": "THEN",
        "text": "the files it has written or changed are its finding files and =design-diff.json= in the cycle's directory, and the seam rows and spec sentences it amended"
       },
       {
        "kw": "AND",
        "text": "besides them it has added only thought lines to the change's journal and the state's entry for each finding it wrote (=record= § A finding in a file of its own is indexed in the state)"
       }
      ],
      "test": null,
      "title": "Conformance writes only its findings, its design diff and the text it corrects"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "any run of the Architect hands back"
       },
       {
        "kw": "THEN",
        "text": "it has made no commit, in the code root or in the home"
       },
       {
        "kw": "AND",
        "text": "it has spawned no agent"
       },
       {
        "kw": "AND",
        "text": "it has written no task file"
       }
      ],
      "test": null,
      "title": "No run commits, spawns an agent or writes a task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect runs the test suite or a probe"
       },
       {
        "kw": "THEN",
        "text": "it does so in the scratch directory its brief names"
       },
       {
        "kw": "AND",
        "text": "the probe changes no file of the code root or the home"
       }
      ],
      "test": null,
      "title": "Probes leave their files in the scratch directory"
     }
    ],
    "text": "Each run of the Architect SHALL hand back one line of at most 200 characters that gives its counts, its detail being in the files it wrote. No run SHALL commit, spawn an agent or write a task. The Architect SHALL run the test suite and its probes only read-only, in the scratch directory its brief names.",
    "title": "Each run hands back one line and leaves the rest to its spawner"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect is spawned"
       },
       {
        "kw": "THEN",
        "text": "it runs at =opus=, at =high= effort for the design round and for conformance"
       },
       {
        "kw": "AND",
        "text": "at =medium= for the premise check"
       }
      ],
      "test": null,
      "title": "The Architect's role default is opus, at high for the design round and conformance and medium for the premise check"
     }
    ],
    "text": "Each run of the Architect SHALL be at its role default; a task override SHALL NOT reach it.",
    "title": "The Architect's role default"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); nine gaps open; at the hand-off review 2026-11-13: G2, G5, G7, G8 and G11 applied, its role default added; the dropped property, what each run writes and the prior's cleanup filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-14 after acceptance (what the design round writes besides the design; a design-round finding's id and trigger; the state's entries for the findings a run writes), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning)",
  "title": "architect"
 },
 "summary": {
  "cases": 10,
  "cited": 63,
  "fail": 21,
  "pass": 42,
  "scenarios": 63,
  "title": "architect",
  "tree": "skill-evals",
  "validation": 10
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
