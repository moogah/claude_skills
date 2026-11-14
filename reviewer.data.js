window.SPEC_PAGE = {
 "capability": "reviewer",
 "cases": [
  {
   "capability": "architect",
   "id": "architect-conformance-clean",
   "items": [
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
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "61359a8",
     "cites": {
      "AK-E4": [
       "reviewer § An Architect's finding carries its labels in its front matter",
       "reviewer § An Architect's finding says where, why the tests missed it and what to do",
       "architect § Style and preference are not reported",
       "architect § A speculative need or a cleanup is a thought line, not a finding"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AK-E4": [
       "AK-S11"
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
        }
       },
       "items": {
        "AK-E4": "fail"
       },
       "judge": {
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
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "AC-E6": [
       "architect § A finding is one file in the cycle's findings directory",
       "architect § A finding carries the class of the design checklist",
       "architect § A finding says why the tests missed it and how to resolve it",
       "architect § The Architect writes no route for a finding",
       "reviewer § An Architect's finding is one file in its cycle's directory",
       "reviewer § An Architect's finding carries its labels in its front matter",
       "reviewer § An Architect's finding says where, why the tests missed it and what to do",
       "reviewer § Every finding says what it blocks"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AC-E6": [
       "AC-S15",
       "AC-S16"
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
        }
       },
       "items": {
        "AC-E6": "fail"
       },
       "judge": {
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
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AR-E12": [
       "AR-S10",
       "AR-S19",
       "AR-S20",
       "AR-S21",
       "AR-S23",
       "AR-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T123104",
     "trials": [
      {
       "calls": {},
       "checks": {
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
        }
       },
       "items": {
        "AR-E12": "fail"
       },
       "judge": {
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
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
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
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AP-E6": [
       "AP-S12",
       "AP-S13"
      ],
      "AP-E7": [
       "AP-S14",
       "AP-S15",
       "AP-S16"
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
        }
       },
       "items": {
        "AP-E6": "pass",
        "AP-E7": "pass"
       },
       "judge": {
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
      "AX-E13": [
       "architect § A defect found at the design round is a finding of the change, outside any cycle",
       "reviewer § An Architect's finding carries its labels in its front matter"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AX-E13": [
       "AX-S25"
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
        }
       },
       "items": {
        "AX-E13": "pass"
       },
       "judge": {
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
      "AU-E5": [
       "architect § A defect found at the design round is a finding of the change, outside any cycle",
       "reviewer § An Architect's finding carries its labels in its front matter"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
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
        }
       },
       "items": {
        "AU-E5": "pass"
       },
       "judge": {
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
   "capability": "execute",
   "id": "execute-ex2",
   "items": [
    {
     "checks": [
      {
       "id": "EX-S28",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § A Reviewer runs at its role default or the task's raised setting",
      "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
     ],
     "id": "EX-E17",
     "question": "Is a task's Reviewer spawned at the Reviewer's role default?"
    }
   ],
   "runs": {
    "main": {
     "case_commit": "717a75b",
     "cites": {},
     "commit": "e356da7",
     "item_checks": {},
     "items_then": [],
     "run": "20261112T215928",
     "trials": [
      {
       "calls": {
        "13": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && S=<trial>/skills/orchestrate/scripts/state.py\npython3 <trial>/skills/orchestrate/scripts/state.py task set dollar-table blocked blocker_note=ask-cycle-1-1\npython3 <trial>/skills/orchestrate/scripts/state.py task set ledger-comments blocked blocker_note=ask-cycle-1-1\npython3 <trial>/skills/orchestrate/scripts/state.py note note \"regression: dollar-table merge 4986ad8 breaks tests.test_json test_scenario_example (shared _money helper); merges stopped\" --ref dollar-table\ngit add .orchestrator/changes/json-report/inbox/ask-cycle-1-1.org && git commit -q -m \"Ask as … [cut, 821 characters]",
         "tool": "Bash"
        },
        "8": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TASK_NAME=dollar-table; BRANCH_NAME=task-dollar-table-1793000002; TS=1793000002\ngit -C \"$CODE_ROOT\" merge --no-ff \"$BRANCH_NAME\" -m \"Merge task $TASK_NAME: Show amounts in the table with a dollar sign\" 2>&1 | tail -5\nMERGE_COMMIT=$(git -C \"$CODE_ROOT\" rev-parse HEAD); echo $MERGE_COMMIT\n(cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-${TASK_NAME}-${TS}.txt\" 2>&1; echo status=$?; tail -5 \"$ORCH_CHANGE_DIR/after-${TASK_NAME}-${TS}.txt\"",
         "tool": "Bash"
        }
       },
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [
          "EX-S1"
         ],
         "hits": {
          "EX-S1": [
           "4986ad8"
          ]
         },
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its sha in the call's output",
         "scenarios": [
          "execute § Each task arrives as a merge commit of its own",
          "execute § Tasks merge in the order they landed"
         ],
         "sig": "merge dollar-table"
        },
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [],
         "hits": {},
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nFAILED (failures=1)",
         "scenarios": [],
         "sig": "test run after merge dollar-table: red"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [],
         "hits": {},
         "label": "journal, note: regression: dollar-table merge 4986ad8 breaks tests.test_json test_scenario_example (shared _money helper); merges stopped",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "regression: dollar-t",
         "scenarios": [],
         "sig": "journal note on dollar-table"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [
          "EX-S5"
         ],
         "hits": {
          "EX-S5": [
           ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org"
          ]
         },
         "label": "ask filed: ask-cycle-1-1.org (environment): Should the JSON report show amounts with a dollar sign too, now that the table does?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-1.org",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect"
         ],
         "sig": "ask, environment"
        }
       ],
       "n_calls": 13,
       "trial": 1
      },
      {
       "calls": {
        "12": {
         "error": true,
         "text": "export ORCH_CHANGE=json-report && S=\"python3 <trial>/skills/orchestrate/scripts/state.py\" && $S task set dollar-table regression_detected=true after_snapshot=.orchestrator/changes/json-report/after-dollar-table-1793000002.txt && $S task set dollar-table blocked blocker_note=ask-cycle-1-1 && $S task set ledger-comments blocked blocker_note=ask-cycle-1-1 && git add .orchestrator/changes/json-report/inbox/ask-cycle-1-1.org && git commit -q -m \"Ask ask-cycle-1-1: dollar sign leaks into the JSON report after merging dollar-table\" -m \"Co-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\" && $S  … [cut, 629 characters]",
         "tool": "Bash"
        },
        "13": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && python3 <trial>/skills/orchestrate/scripts/state.py task set dollar-table regression_detected=true after_snapshot=.orchestrator/changes/json-report/after-dollar-table-1793000002.txt && python3 <trial>/skills/orchestrate/scripts/state.py task set dollar-table blocked blocker_note=ask-cycle-1-1 && python3 <trial>/skills/orchestrate/scripts/state.py task set ledger-comments blocked blocker_note=ask-cycle-1-1 && git add .orchestrator/changes/json-report/inbox/ask-cycle-1-1.org && git commit -q -m \"Ask ask-cycle-1-1: dollar sign leaks into the JSON report after mer … [cut, 755 characters]",
         "tool": "Bash"
        },
        "14": {
         "error": false,
         "text": "cat > .orchestrator/changes/json-report/cycles/cycle-1/execute-handover.md <<'EOF'\n# execute cycle-1: handover\n\n## Merged\n\n- report-json: `Merge task report-json` 791af18af367; tests passed; review done, clean (`cycles/cycle-1/reviews/report-json.md`); task done. Its worktree `task-report-json-1793000001` is still present (§ 6 removal not done).\n- dollar-table: merged as 35e5de5 on the change branch; the after-test FAILED (regression: `tests.test_json.JsonReport.test_scenario_example`, `_money` now adds `$`, which leaks into `format_json`). After-file `after-dollar-table-1793000002.txt`. `regr … [cut, 1811 characters]",
         "tool": "Bash"
        },
        "6": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\" && TS=1793000002 TASK_NAME=dollar-table BRANCH_NAME=task-dollar-table-1793000002 && git -C \"$CODE_ROOT\" merge --no-ff \"$BRANCH_NAME\" -m \"Merge task $TASK_NAME: Show amounts in the table with a dollar sign\" && git -C \"$CODE_ROOT\" rev-parse HEAD && (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-${TASK_NAME}-${TS}.txt\" 2>&1; echo status=$?; tail -5 \"$ORCH_CHANGE_DIR/after-${TASK_NAME}-${TS}.txt\"",
         "tool": "Bash"
        }
       },
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [
        {
         "call": 6,
         "candidates": [
          6
         ],
         "checks": [
          "EX-S1"
         ],
         "hits": {
          "EX-S1": [
           "35e5de5"
          ]
         },
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its sha in the call's output",
         "scenarios": [
          "execute § Each task arrives as a merge commit of its own",
          "execute § Tasks merge in the order they landed"
         ],
         "sig": "merge dollar-table"
        },
        {
         "call": 6,
         "candidates": [
          6
         ],
         "checks": [],
         "hits": {},
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nFAILED (failures=1)",
         "scenarios": [],
         "sig": "test run after merge dollar-table: red"
        },
        {
         "call": 12,
         "candidates": [
          12,
          13,
          14
         ],
         "checks": [
          "EX-S5"
         ],
         "hits": {
          "EX-S5": [
           ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org"
          ]
         },
         "label": "ask filed: ask-cycle-1-1.org (environment): Should amounts in the JSON report stay without a dollar sign now that the table shows one?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-1.org",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect"
         ],
         "sig": "ask, environment"
        }
       ],
       "n_calls": 14,
       "trial": 2
      },
      {
       "calls": {
        "13": {
         "error": true,
         "text": "export ORCH_CHANGE=json-report; S=<trial>/skills/orchestrate/scripts/state.py\npython3 $S task set ledger-comments blocked blocker_note=ask-cycle-1-1 | tail -1\npython3 $S task set dollar-table blocked blocker_note=ask-cycle-1-1 | tail -1\nrm -f /tmp/ask-draft-*.org\ngit add .orchestrator/changes/json-report/inbox/ask-cycle-1-1.org && git commit -q -m \"Ask ask-cycle-1-1: dollar sign in the table broke the JSON report test\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\" && git log --oneline -1; python3 $S gate check execute | head -20",
         "tool": "Bash"
        },
        "14": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report; S=<trial>/skills/orchestrate/scripts/state.py\npython3 <trial>/skills/orchestrate/scripts/state.py task set ledger-comments blocked blocker_note=ask-cycle-1-1 | tail -1\npython3 <trial>/skills/orchestrate/scripts/state.py task set dollar-table blocked blocker_note=ask-cycle-1-1 | tail -1\ngit add .orchestrator/changes/json-report/inbox/ask-cycle-1-1.org && git commit -q -m \"Ask ask-cycle-1-1: dollar sign in the table broke the JSON report test\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\" && git log --oneline -1\npython3 <trial>/skills/orchestrate/scri … [cut, 642 characters]",
         "tool": "Bash"
        },
        "8": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report; eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TASK_NAME=dollar-table; BRANCH_NAME=task-dollar-table-1793000002; TS=1793000002\ngit -C \"$CODE_ROOT\" merge --no-ff \"$BRANCH_NAME\" -m \"Merge task $TASK_NAME: Show amounts in the table with a dollar sign\" 2>&1 | tail -5\nMERGE_COMMIT=$(git -C \"$CODE_ROOT\" rev-parse HEAD); echo $MERGE_COMMIT\n(cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-${TASK_NAME}-${TS}.txt\" 2>&1; echo status=$?; tail -5 \"$ORCH_CHANGE_DIR/after-${TASK_NAME}-${TS}.txt\"",
         "tool": "Bash"
        }
       },
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [
          "EX-S1"
         ],
         "hits": {
          "EX-S1": [
           "8b9f3c8"
          ]
         },
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its sha in the call's output",
         "scenarios": [
          "execute § Each task arrives as a merge commit of its own",
          "execute § Tasks merge in the order they landed"
         ],
         "sig": "merge dollar-table"
        },
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [],
         "hits": {},
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nFAILED (failures=1)",
         "scenarios": [],
         "sig": "test run after merge dollar-table: red"
        },
        {
         "call": 13,
         "candidates": [
          13,
          14
         ],
         "checks": [
          "EX-S5"
         ],
         "hits": {
          "EX-S5": [
           ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org"
          ]
         },
         "label": "ask filed: ask-cycle-1-1.org (environment): Should the dollar sign show only in the table, with the JSON report keeping plain amounts like 900.00?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-1.org",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect"
         ],
         "sig": "ask, environment"
        }
       ],
       "n_calls": 14,
       "trial": 3
      }
     ],
     "validation": false
    },
    "skill-evals": {
     "case_commit": "717a75b",
     "cites": {},
     "commit": "c7a8b4d",
     "item_checks": {},
     "items_then": [],
     "run": "20261112T215928",
     "trials": [
      {
       "calls": {
        "16": {
         "error": false,
         "text": "python3 -m unittest discover -s tests -t . 2>&1 | tail -4; git add tests/test_json.py && git commit -q -m \"Inline fix after merging task dollar-table: the JSON test expects the table's dollar-sign amounts\n\nAnswers the regression in after-dollar-table-1793000002.txt: tests.test_json\ntest_scenario_example failed after the merge (green baseline) because it\nexpected amounts without a dollar sign. G1 (JSON uses the table's amounts) and\nG2 (the table shows a dollar sign) settle it. The test itself is the pin: it\nfailed before this change and passes after.\" && git log --oneline | head -3",
         "tool": "Bash"
        },
        "17": {
         "error": false,
         "text": "cd <trial>/home && export ORCH_CHANGE=json-report && S=<trial>/skills/orchestrate/scripts/state.py && eval \"$(python3 $S where --sh)\" && cp $ORCH_CHANGE_DIR/after-dollar-table-1793000002.txt $ORCH_CHANGE_DIR/after-dollar-table-1793000002-regression.txt; (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-dollar-table-1793000002.txt\" 2>&1; echo status=$?\npython3 <trial>/skills/orchestrate/scripts/state.py note discovery \"regression: dollar-table: tests.test_json expected amounts without \\$ after the table gained one; clash of report-json and dollar-table; f … [cut, 1163 characters]",
         "tool": "Bash"
        },
        "19": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && S=<trial>/skills/orchestrate/scripts/state.py; python3 <trial>/skills/orchestrate/scripts/state.py task set dollar-table after_snapshot=.orchestrator/changes/json-report/after-dollar-table-1793000002.txt regression_detected=false; M=$(git -C .worktrees/tally/json-report rev-parse HEAD); python3 <trial>/skills/orchestrate/scripts/state.py task set dollar-table needs_review merge_commit=$M; git -C .worktrees/tally/json-report worktree remove .worktrees/tally/task-dollar-table-1793000002 2>&1; git -C .worktrees/tally/json-report branch -D task-dollar-table-179300 … [cut, 881 characters]",
         "tool": "Bash"
        },
        "25": {
         "error": false,
         "text": "Review task dollar-table",
         "tool": "Agent"
        },
        "7": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\" && git -C \"$CODE_ROOT\" merge --no-ff task-dollar-table-1793000002 -m \"Merge task dollar-table: Show amounts in the table with a dollar sign\" 2>&1 | tail -8; git -C \"$CODE_ROOT\" status --short | head",
         "tool": "Bash"
        },
        "8": {
         "error": true,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\" && MC=$(git -C \"$CODE_ROOT\" rev-parse HEAD); echo $MC; (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-dollar-table-1793000002.txt\" 2>&1; echo status=$?; tail -6 \"$ORCH_CHANGE_DIR/after-dollar-table-1793000002.txt\"; ls $ORCH_CHANGE_DIR/cycles/cycle-1/reports",
         "tool": "Bash"
        }
       },
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [
          "EX-S1"
         ],
         "hits": {
          "EX-S1": [
           "f521b36"
          ]
         },
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task dollar-table: Show amounts in",
         "scenarios": [
          "execute § Each task arrives as a merge commit of its own",
          "execute § Tasks merge in the order they landed"
         ],
         "sig": "merge dollar-table"
        },
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [
          "EX-S2",
          "EX-S7",
          "EX-S9"
         ],
         "hits": {
          "EX-S2": [
           "after-dollar-table-1793000002.txt"
          ],
          "EX-S7": [
           "after-dollar-table-1793000002.txt"
          ],
          "EX-S9": [
           "after-dollar-table-1793000002.txt"
          ]
         },
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nFAILED (failures=1)",
         "scenarios": [
          "execute § Against a green baseline, any failing run is a regression",
          "execute § An inline fix's commit names its task, its cause and its pin",
          "execute § Every merge is followed by a test run whose output is kept",
          "execute § Merging resumes only after a run shows no regression"
         ],
         "sig": "test run after merge dollar-table: red"
        },
        {
         "call": 16,
         "candidates": [
          16
         ],
         "checks": [
          "EX-S11",
          "EX-S6",
          "EX-S7"
         ],
         "hits": {
          "EX-S11": [
           "8e02321"
          ],
          "EX-S6": [
           "Inline fix after merging task dollar-tab"
          ],
          "EX-S7": [
           "Inline fix after merging task dollar-tab"
          ]
         },
         "label": "Inline fix after merging task dollar-table: the JSON test expects the table's dollar-sign amounts",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Inline fix after merging task dollar-tab",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect",
          "execute § An inline fix's commit names its task, its cause and its pin",
          "execute § The phase agent finds the regression's cause"
         ],
         "sig": "inline fix after dollar-table"
        },
        {
         "call": 16,
         "candidates": [
          16
         ],
         "checks": [
          "EX-S2",
          "EX-S7",
          "EX-S9"
         ],
         "hits": {
          "EX-S2": [
           "after-dollar-table-1793000002.txt"
          ],
          "EX-S7": [
           "after-dollar-table-1793000002.txt"
          ],
          "EX-S9": [
           "after-dollar-table-1793000002.txt"
          ]
         },
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nOK",
         "scenarios": [
          "execute § Against a green baseline, any failing run is a regression",
          "execute § An inline fix's commit names its task, its cause and its pin",
          "execute § Every merge is followed by a test run whose output is kept",
          "execute § Merging resumes only after a run shows no regression"
         ],
         "sig": "test run after inline fix after dollar-table: green"
        },
        {
         "call": 17,
         "candidates": [
          17
         ],
         "checks": [
          "EX-S11"
         ],
         "hits": {
          "EX-S11": [
           "regression: dollar-table: tests.test_jso"
          ]
         },
         "label": "journal, discovery: regression: dollar-table: tests.test_json expected amounts without $ after the table gained one; clash of report-json and dollar-table; fixed inline 8e02321 (G1+G2 settle it)",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "regression: dollar-t",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect",
          "execute § The phase agent finds the regression's cause"
         ],
         "sig": "journal discovery on dollar-table"
        },
        {
         "call": 17,
         "candidates": [
          17
         ],
         "checks": [
          "EX-S12"
         ],
         "hits": {
          "EX-S12": [
           "record-correction for integrate: spec sc"
          ]
         },
         "label": "journal, note: record-correction for integrate: spec scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", \"food\": \"13.05\"}; with G2 it is \"$900.00\", \"$13.05\"",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "record-correction fo",
         "scenarios": [
          "execute § A spec statement the goals overrule is left for integrate"
         ],
         "sig": "journal note on dollar-table"
        },
        {
         "call": 17,
         "candidates": [
          17
         ],
         "checks": [],
         "hits": {},
         "label": "journal, inline-fix: test_json expects the table's dollar-sign amounts (8e02321)",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "test_json expects th",
         "scenarios": [],
         "sig": "journal inline-fix on dollar-table"
        },
        {
         "call": 19,
         "candidates": [
          19
         ],
         "checks": [],
         "hits": {},
         "label": "journal, note: scanned dollar-table merge for latent bugs and discoveries; nothing beyond the clash regression",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "scanned dollar-table",
         "scenarios": [],
         "sig": "journal note on dollar-table"
        },
        {
         "call": 25,
         "candidates": [
          25
         ],
         "checks": [
          "EX-S14"
         ],
         "hits": {
          "EX-S14": [
           "Reviewer spawn(s) for dollar-table"
          ]
         },
         "label": "spawn: Review task dollar-table",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Review task dollar-table",
         "scenarios": [
          "execute § The task whose merge broke a test is reviewed after the fix"
         ],
         "sig": "spawn Reviewer dollar-table"
        }
       ],
       "n_calls": 25,
       "trial": 1
      },
      {
       "calls": {
        "11": {
         "error": true,
         "text": "sed -i 's/{\"rent\": \"900.00\", \"food\": \"13.05\"}/{\"rent\": \"$900.00\", \"food\": \"$13.05\"}/' tests/test_json.py && python3 -m unittest discover -s tests -t . 2>&1 | tail -4; git status --short; git add tests/test_json.py && git commit -q -m \"Inline fix after merging task dollar-table: JSON amounts follow the table's dollar sign\n\nAnswers the regression in after-dollar-table-1793000002.txt (tests.test_json.JsonReport.test_scenario_example): the table's amount rendering gained a dollar sign (G2) and the JSON report reuses it (G1, D1), so its expected amounts carry the sign. Pinned by tests/test_json.py, … [cut, 731 characters]",
         "tool": "Bash"
        },
        "12": {
         "error": false,
         "text": "python3 -I - <<'EOF'\np=\"tests/test_json.py\"\ns=open(p).read()\ns=s.replace('{\"rent\": \"900.00\", \"food\": \"13.05\"}','{\"rent\": \"$900.00\", \"food\": \"$13.05\"}')\nopen(p,\"w\").write(s)\nEOF\ngit diff --stat; python3 -m unittest discover -s tests -t . 2>&1 | tail -4; git status --short",
         "tool": "Bash"
        },
        "13": {
         "error": false,
         "text": "cd <trial>/home && export ORCH_CHANGE=json-report && S=<trial>/skills/orchestrate/scripts/state.py\ngit -C .worktrees/tally/json-report commit -q -am \"Inline fix after merging task dollar-table: JSON amounts follow the table's dollar sign\n\nAnswers the regression in after-dollar-table-1793000002.txt (tests.test_json.JsonReport.test_scenario_example): the table's amount rendering gained a dollar sign (G2) and the JSON report reuses it (G1, D1), so its expected amounts carry the sign. Pinned by tests/test_json.py, which failed before the fix and passes after it.\n\nCo-Authored-By: Claude Sonnet 5.5  … [cut, 1404 characters]",
         "tool": "Bash"
        },
        "14": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && S=<trial>/skills/orchestrate/scripts/state.py\npython3 $S note inline-fix \"test_json expects the table's amounts with the dollar sign (c923eed)\" --ref dollar-table >/dev/null\nCODE_ROOT=$PWD/.worktrees/tally/json-report\n(cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > .orchestrator/changes/json-report/after-dollar-table-1793000002.txt 2>&1; echo status=$?; tail -3 .orchestrator/changes/json-report/after-dollar-table-1793000002.txt\npython3 $S task set dollar-table needs_review merge_commit=$(git -C $CODE_ROOT rev-parse HEAD) regression_detected=f … [cut, 604 characters]",
         "tool": "Bash"
        },
        "24": {
         "error": false,
         "text": "Review task dollar-table",
         "tool": "Agent"
        },
        "7": {
         "error": false,
         "text": "cd <trial>/home && export ORCH_CHANGE=json-report && CODE_ROOT=$PWD/.worktrees/tally/json-report && git -C \"$CODE_ROOT\" merge --no-ff task-dollar-table-1793000002 -m \"Merge task dollar-table: Show amounts in the table with a dollar sign\" 2>&1 | tail -5; git -C \"$CODE_ROOT\" rev-parse HEAD; (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > .orchestrator/changes/json-report/after-dollar-table-1793000002.txt 2>&1; echo status=$?; tail -5 .orchestrator/changes/json-report/after-dollar-table-1793000002.txt",
         "tool": "Bash"
        }
       },
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [
          "EX-S1"
         ],
         "hits": {
          "EX-S1": [
           "a5c2190"
          ]
         },
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task dollar-table: Show amounts in",
         "scenarios": [
          "execute § Each task arrives as a merge commit of its own",
          "execute § Tasks merge in the order they landed"
         ],
         "sig": "merge dollar-table"
        },
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [
          "EX-S2",
          "EX-S7"
         ],
         "hits": {
          "EX-S2": [
           "after-dollar-table-1793000002.txt"
          ],
          "EX-S7": [
           "after-dollar-table-1793000002.txt"
          ]
         },
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nFAILED (failures=1)",
         "scenarios": [
          "execute § Against a green baseline, any failing run is a regression",
          "execute § An inline fix's commit names its task, its cause and its pin",
          "execute § Every merge is followed by a test run whose output is kept"
         ],
         "sig": "test run after merge dollar-table: red"
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
         "result": "Ran 5 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after merge dollar-table: green"
        },
        {
         "call": 13,
         "candidates": [
          11,
          13
         ],
         "checks": [
          "EX-S6",
          "EX-S7"
         ],
         "hits": {
          "EX-S6": [
           "Inline fix after merging task dollar-tab"
          ],
          "EX-S7": [
           "Inline fix after merging task dollar-tab"
          ]
         },
         "label": "Inline fix after merging task dollar-table: JSON amounts follow the table's dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Inline fix after merging task dollar-tab",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect",
          "execute § An inline fix's commit names its task, its cause and its pin"
         ],
         "sig": "inline fix after dollar-table"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [
          "EX-S11"
         ],
         "hits": {
          "EX-S11": [
           "regression: dollar-table: the shared _mo"
          ]
         },
         "label": "journal, discovery: regression: dollar-table: the shared _money gained a $ (G2) and format_json reuses it (G1, D1), so test_json's literal 900.00 failed; two tasks clashing, goals settle it: JSON carries the $. fixed: inline fix",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "regression: dollar-t",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect",
          "execute § The phase agent finds the regression's cause"
         ],
         "sig": "journal discovery on dollar-table"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [
          "EX-S12"
         ],
         "hits": {
          "EX-S12": [
           "record-correction for integrate: docs/sp"
          ]
         },
         "label": "journal, note: record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", \"food\": \"13.05\"}; per G1+G2 the amounts carry a dollar sign",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "record-correction fo",
         "scenarios": [
          "execute § A spec statement the goals overrule is left for integrate"
         ],
         "sig": "journal note on dollar-table"
        },
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "journal, inline-fix: test_json expects the table's amounts with the dollar sign (c923eed)",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "test_json expects th",
         "scenarios": [],
         "sig": "journal inline-fix on dollar-table"
        },
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [
          "EX-S2",
          "EX-S7"
         ],
         "hits": {
          "EX-S2": [
           "after-dollar-table-1793000002.txt"
          ],
          "EX-S7": [
           "after-dollar-table-1793000002.txt"
          ]
         },
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nOK",
         "scenarios": [
          "execute § Against a green baseline, any failing run is a regression",
          "execute § An inline fix's commit names its task, its cause and its pin",
          "execute § Every merge is followed by a test run whose output is kept"
         ],
         "sig": "test run after inline fix after dollar-table: green"
        },
        {
         "call": 24,
         "candidates": [
          24
         ],
         "checks": [
          "EX-S14"
         ],
         "hits": {
          "EX-S14": [
           "Reviewer spawn(s) for dollar-table"
          ]
         },
         "label": "spawn: Review task dollar-table",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Review task dollar-table",
         "scenarios": [
          "execute § The task whose merge broke a test is reviewed after the fix"
         ],
         "sig": "spawn Reviewer dollar-table"
        }
       ],
       "n_calls": 24,
       "trial": 2
      },
      {
       "calls": {
        "13": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && S=\"python3 <trial>/skills/orchestrate/scripts/state.py\" && sed -i 's/{\"rent\": \"900.00\", \"food\": \"13.05\"}/{\"rent\": \"$900.00\", \"food\": \"$13.05\"}/' tests/test_json.py && python3 -m unittest discover -s tests -t . 2>&1 | tail -4 && git add tests/test_json.py && git commit -q -m \"Inline fix after merging task dollar-table: JSON amounts carry the table's dollar sign\n\nAnswers the regression in after-dollar-table-1793000002.txt (test_json.test_scenario_example). Goal G1 and design D1 make the JSON amounts the table's amounts, so the test follows the table. Pinned by t … [cut, 1527 characters]",
         "tool": "Bash"
        },
        "15": {
         "error": false,
         "text": "cd <trial>/home/.worktrees/tally/json-report && (python3 -m unittest discover -s tests -t . 2>&1 | tail -4) && git add tests/test_json.py && git commit -q -m \"Inline fix after merging task dollar-table: JSON amounts carry the table's dollar sign\n\nAnswers the regression in after-dollar-table-1793000002.txt (test_json.test_scenario_example). Goal G1 and design D1 make the JSON amounts the table's amounts, so the test follows the table. Pinned by that test: failed after the merge, passes now.\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\" && git rev-parse HEAD",
         "tool": "Bash"
        },
        "18": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; git -C \"$CODE_ROOT\" merge --no-ff task-ledger-comments-1793000003 -m \"Merge task ledger-comments: ; starts a comment line\" 2>&1 | tail -5; git -C \"$CODE_ROOT\" rev-parse HEAD; TS=1793000003; (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-ledger-comments-$TS.txt\" 2>&1; echo status $?; tail -4 \"$ORCH_CHANGE_DIR/after-ledger-comments-$TS.txt\"",
         "tool": "Bash"
        },
        "25": {
         "error": false,
         "text": "Review dollar-table task",
         "tool": "Agent"
        },
        "6": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; grep -n \"test\" .orchestrator/changes/json-report/change.json | head; ls .orchestrator/changes/json-report/cycles/cycle-1 .orchestrator/changes/json-report/cycles/cycle-1/briefs; cat .orchestrator/changes/json-report/baseline-cycle-1.txt | tail -5; git -C $CODE_ROOT merge --no-ff task-dollar-table-1793000002 -m \"Merge task dollar-table: amounts with a dollar sign in the table\"; git -C $CODE_ROOT rev-parse HEAD",
         "tool": "Bash"
        },
        "9": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TS=1793000002; (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-dollar-table-$TS.txt\" 2>&1; echo status $?; tail -6 \"$ORCH_CHANGE_DIR/after-dollar-table-$TS.txt\"",
         "tool": "Bash"
        }
       },
       "checks": {},
       "items": {},
       "judge": {},
       "milestones": [
        {
         "call": 6,
         "candidates": [
          6
         ],
         "checks": [
          "EX-S1"
         ],
         "hits": {
          "EX-S1": [
           "628e7eb"
          ]
         },
         "label": "Merge task dollar-table: amounts with a dollar sign in the table",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task dollar-table: amounts with a ",
         "scenarios": [
          "execute § Each task arrives as a merge commit of its own",
          "execute § Tasks merge in the order they landed"
         ],
         "sig": "merge dollar-table"
        },
        {
         "call": 9,
         "candidates": [
          9
         ],
         "checks": [],
         "hits": {},
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nFAILED (failures=1)",
         "scenarios": [],
         "sig": "test run after merge dollar-table: red"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [
          "EX-S11"
         ],
         "hits": {
          "EX-S11": [
           "regression: dollar-table: format_json sh"
          ]
         },
         "label": "journal, discovery: regression: dollar-table: format_json shares _money, so the table's $ reaches the JSON; test_json expected no $. G1/D1 say the table's amounts: test fixed inline",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "regression: dollar-t",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect",
          "execute § The phase agent finds the regression's cause"
         ],
         "sig": "journal discovery on dollar-table"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [
          "EX-S12"
         ],
         "hits": {
          "EX-S12": [
           "record-correction for integrate: docs/sp"
          ]
         },
         "label": "journal, note: record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows amounts without $; the table now has $ and G1 makes JSON follow it",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "record-correction fo",
         "scenarios": [
          "execute § A spec statement the goals overrule is left for integrate"
         ],
         "sig": "journal note on dollar-table"
        },
        {
         "call": 15,
         "candidates": [
          13,
          15
         ],
         "checks": [
          "EX-S6",
          "EX-S7"
         ],
         "hits": {
          "EX-S6": [
           "Inline fix after merging task dollar-tab"
          ],
          "EX-S7": [
           "Inline fix after merging task dollar-tab"
          ]
         },
         "label": "Inline fix after merging task dollar-table: JSON amounts carry the table's dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Inline fix after merging task dollar-tab",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect",
          "execute § An inline fix's commit names its task, its cause and its pin"
         ],
         "sig": "inline fix after dollar-table"
        },
        {
         "call": 15,
         "candidates": [
          15
         ],
         "checks": [
          "EX-S2",
          "EX-S7"
         ],
         "hits": {
          "EX-S2": [
           "after-dollar-table-1793000002.txt"
          ],
          "EX-S7": [
           "after-dollar-table-1793000002.txt"
          ]
         },
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nOK",
         "scenarios": [
          "execute § Against a green baseline, any failing run is a regression",
          "execute § An inline fix's commit names its task, its cause and its pin",
          "execute § Every merge is followed by a test run whose output is kept"
         ],
         "sig": "test run after inline fix after dollar-table: green"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [],
         "hits": {},
         "label": "Merge task ledger-comments: ; starts a comment line",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task ledger-comments: ; starts a c",
         "scenarios": [],
         "sig": "merge ledger-comments"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 6 tests in 0.003s\nOK",
         "scenarios": [],
         "sig": "test run after merge ledger-comments: green"
        },
        {
         "call": 25,
         "candidates": [
          25
         ],
         "checks": [
          "EX-S14"
         ],
         "hits": {
          "EX-S14": [
           "Reviewer spawn(s) for dollar-table"
          ]
         },
         "label": "spawn: Review dollar-table task",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Review dollar-table task",
         "scenarios": [
          "execute § The task whose merge broke a test is reviewed after the fix"
         ],
         "sig": "spawn Reviewer dollar-table"
        }
       ],
       "n_calls": 25,
       "trial": 3
      }
     ],
     "validation": false
    }
   },
   "step": "the execute phase agent's merge chain, from a handover to its first spawn (a Reviewer) or its hand-back",
   "title": "Case execute-ex2: a merge that breaks a passing test, fixed in the change because the goals settle it",
   "trials": 3,
   "validated": "9a55ddae3c987e8f4d84b3c15bcf934e81100290 20261114T103108 t1 (skill-evals)"
  },
  {
   "capability": "execute",
   "id": "execute-merge-chain",
   "items": [
    {
     "checks": [
      {
       "id": "MC-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § A Reviewer runs at its role default or the task's raised setting",
      "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
     ],
     "id": "MC-E6",
     "question": "Is a task's Reviewer spawned at the Reviewer's role default?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "56af72c",
     "cites": {
      "MC-E6": [
       "execute § A Reviewer runs at its role default or the task's raised setting",
       "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
      ]
     },
     "commit": "5dd6a8c",
     "item_checks": {
      "MC-E6": [
       "MC-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T111932",
     "trials": [
      {
       "calls": {
        "11": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TS=$(cat <trial>/scratch/phase-execute-cycle-1/ts)\nS=<trial>/skills/orchestrate/scripts/state.py\npython3 $S task set report-json after_snapshot=.orchestrator/changes/json-report/after-report-json-$TS.txt\npython3 $S note note \"scanned report-json discoveries/observations; disc-report-json-1 is a no-action question (cli docstring now true), no defect\" --ref report-json\npython3 $S task set report-json needs_review merge_commit=cebab626d8634d146e97a1f8b1bf38bc299c305d implementor_report_path … [cut, 1203 characters]",
         "tool": "Bash"
        },
        "12": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TS=$(date +%s); echo $TS > <trial>/scratch/phase-execute-cycle-1/ts2\nsed -n 1,12p .orchestrator/changes/json-report/tasks/open/ledger-comments.md\ngit -C \"$CODE_ROOT\" merge --no-ff task-ledger-comments-1793000003 -m \"Merge task ledger-comments: ; starts a comment line\"; echo merge=$?; git -C \"$CODE_ROOT\" rev-parse HEAD; git -C \"$CODE_ROOT\" status --short",
         "tool": "Bash"
        },
        "13": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TS=$(cat <trial>/scratch/phase-execute-cycle-1/ts2)\n(cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-ledger-comments-$TS.txt\" 2>&1; echo after=$?; tail -4 \"$ORCH_CHANGE_DIR/after-ledger-comments-$TS.txt\"\npython3 <trial>/skills/orchestrate/scripts/state.py task set ledger-comments after_snapshot=.orchestrator/changes/json-report/after-ledger-comments-$TS.txt\nsed -n '/^## Observations/,$p' .orchestrator/changes/json-report/tasks/open/ledger-comments … [cut, 694 characters]",
         "tool": "Bash"
        },
        "14": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"\npython3 <trial>/skills/orchestrate/scripts/state.py note note \"scanned ledger-comments report and task file; nothing\" --ref ledger-comments\npython3 <trial>/skills/orchestrate/scripts/state.py task set ledger-comments needs_review merge_commit=468375a324b2b48cc802a520f7bf194be944df2a implementor_report_path=.orchestrator/changes/json-report/cycles/cycle-1/reports/ledger-comments.md\ngit -C \"$CODE_ROOT\" worktree remove <trial>/home/.worktrees/tally/task-ledger-comments-1793000003 && git -C \" … [cut, 1241 characters]",
         "tool": "Bash"
        },
        "22": {
         "error": true,
         "text": "Implement dollar-table",
         "tool": "Agent"
        },
        "31": {
         "error": true,
         "text": "Review report-json",
         "tool": "Agent"
        },
        "32": {
         "error": true,
         "text": "Review ledger-comments",
         "tool": "Agent"
        },
        "35": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TS=$(date +%s)\ngit -C \"$CODE_ROOT\" merge --no-ff task-dollar-table-1793000002 -m \"Merge task dollar-table: Show amounts in the table with a dollar sign\"; echo merge=$?; git -C \"$CODE_ROOT\" rev-parse HEAD\n(cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-dollar-table-$TS.txt\" 2>&1; echo after=$?; cat \"$ORCH_CHANGE_DIR/after-dollar-table-$TS.txt\" | tail -30; echo $TS > <trial>/scratch/phase-execute-cycle-1/ts3\ngit -C \"$CODE_ROOT\" diff 468375a HEAD; c … [cut, 675 characters]",
         "tool": "Bash"
        },
        "36": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; TS=$(cat <trial>/scratch/phase-execute-cycle-1/ts3); S=<trial>/skills/orchestrate/scripts/state.py\npython3 $S task set dollar-table after_snapshot=.orchestrator/changes/json-report/after-dollar-table-$TS.txt\npython3 $S note note \"scanned dollar-table report and task file; no discoveries or observations recorded. Diff changes _money (shared with format_json) and edits tests/test_json.py: left to its Reviewer\" --ref dollar-table\npython3 $S task set dollar-table needs_review merge_commit=eb … [cut, 1520 characters]",
         "tool": "Bash"
        },
        "39": {
         "error": true,
         "text": "Review dollar-table",
         "tool": "Agent"
        },
        "42": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"\nS=<trial>/skills/orchestrate/scripts/state.py; CY=.orchestrator/changes/json-report/cycles/cycle-1\npython3 $S note note \"record-correction for integrate: docs/specs/reporting.org § The JSON report maps each category to its total shows {\\\"rent\\\": \\\"900.00\\\", \\\"food\\\": \\\"13.05\\\"}; after dollar-table the JSON carries the table's amounts, \\\"\\$900.00\\\" (G1, D1, G2). Probed on the CLI. Review of dollar-table did not flag it.\" --ref dollar-table\npython3 $S note note \"dollar-table review prompt h … [cut, 1309 characters]",
         "tool": "Bash"
        },
        "43": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && python3 <trial>/skills/orchestrate/scripts/state.py note note \"record-correction for integrate: reporting.org JSON scenario shows \\\"900.00\\\"; after dollar-table the JSON carries the table's amounts, \\\"\\$900.00\\\" (G1, D1, G2; probed on the CLI). Its review did not flag it.\" --ref dollar-table",
         "tool": "Bash"
        },
        "7": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; git -C \"$CODE_ROOT\" status --short; (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/baseline-cycle-1.txt\" 2>&1; echo status=$?; tail -8 \"$ORCH_CHANGE_DIR/baseline-cycle-1.txt\"",
         "tool": "Bash"
        },
        "9": {
         "error": false,
         "text": "cd <trial>/home && export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\"; head -20 .orchestrator/changes/json-report/tasks/open/report-json.md; TS=$(date +%s); echo $TS > <trial>/scratch/phase-execute-cycle-1/ts\ngit -C \"$CODE_ROOT\" merge --no-ff task-report-json-1793000001 -m \"Merge task report-json: format_json and --json\"; git -C \"$CODE_ROOT\" rev-parse HEAD; (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-report-json-$TS.txt\" 2>&1; echo after=$?; tail -5 \"$ORCH_CHANGE_DIR/after-report-json-$TS.txt\"",
         "tool": "Bash"
        }
       },
       "checks": {
        "MC-S7": {
         "audit": {
          "agree": true,
          "reason": "All three Reviewer spawns are opus/high.",
          "verdict": "agrees"
         },
         "desc": "each Reviewer spawned at opus, and at high when an effort is given",
         "detail": "report-json: opus high | ledger-comments: opus high | dollar-table: opus high",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "MC-E6": "pass"
       },
       "judge": {
        "MC-E6": {
         "quotes": [
          "\"description\": \"Review dollar-table\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"high\"",
          "\"description\": \"Review dollar-table\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"high\"",
          "\"description\": \"Review dollar-table\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"high\""
         ],
         "reasons": [
          "All three Reviewers were spawned at opus, high.",
          "All three Reviewers were spawned at opus, high.",
          "All three Reviewers were spawned at opus, high."
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
         "call": 9,
         "candidates": [
          9
         ],
         "checks": [
          "MC-S2"
         ],
         "hits": {
          "MC-S2": [
           "Merge task report-json: format_json and "
          ]
         },
         "label": "Merge task report-json: format_json and --json",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task report-json: format_json and ",
         "scenarios": [],
         "sig": "merge report-json"
        },
        {
         "call": 9,
         "candidates": [
          9
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 5 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after merge report-json: green"
        },
        {
         "call": 11,
         "candidates": [
          11
         ],
         "checks": [],
         "hits": {},
         "label": "journal, note: scanned report-json discoveries/observations; disc-report-json-1 is a no-action question (cli docstring now true), no defect",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "scanned report-json ",
         "scenarios": [],
         "sig": "journal note on report-json"
        },
        {
         "call": 12,
         "candidates": [
          12
         ],
         "checks": [
          "MC-S2"
         ],
         "hits": {
          "MC-S2": [
           "Merge task ledger-comments: ; starts a c"
          ]
         },
         "label": "Merge task ledger-comments: ; starts a comment line",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task ledger-comments: ; starts a c",
         "scenarios": [],
         "sig": "merge ledger-comments"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.002s\nOK",
         "scenarios": [],
         "sig": "test run after merge ledger-comments: green"
        },
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "journal, note: scanned ledger-comments report and task file; nothing",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "scanned ledger-comme",
         "scenarios": [],
         "sig": "journal note on ledger-comments"
        },
        {
         "call": 22,
         "candidates": [
          22
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Implement dollar-table",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Implement dollar-table",
         "scenarios": [],
         "sig": "spawn Implementor dollar-table"
        },
        {
         "call": 31,
         "candidates": [
          31
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Review report-json",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Review report-json",
         "scenarios": [],
         "sig": "spawn Reviewer report-json"
        },
        {
         "call": 32,
         "candidates": [
          32
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Review ledger-comments",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Review ledger-comments",
         "scenarios": [],
         "sig": "spawn Reviewer ledger-comments"
        },
        {
         "call": 35,
         "candidates": [
          35
         ],
         "checks": [
          "MC-S2"
         ],
         "hits": {
          "MC-S2": [
           "Merge task dollar-table: Show amounts in"
          ]
         },
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task dollar-table: Show amounts in",
         "scenarios": [],
         "sig": "merge dollar-table"
        },
        {
         "call": 35,
         "candidates": [
          35
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.002s\nOK",
         "scenarios": [],
         "sig": "test run after merge dollar-table: green"
        },
        {
         "call": 36,
         "candidates": [
          36
         ],
         "checks": [],
         "hits": {},
         "label": "journal, note: scanned dollar-table report and task file; no discoveries or observations recorded. Diff changes _money (shared with format_json) and edits tests/test_json.py: left to its Reviewer",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "scanned dollar-table",
         "scenarios": [],
         "sig": "journal note on dollar-table"
        },
        {
         "call": 39,
         "candidates": [
          39
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Review dollar-table",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Review dollar-table",
         "scenarios": [],
         "sig": "spawn Reviewer dollar-table"
        },
        {
         "call": 42,
         "candidates": [
          42
         ],
         "checks": [],
         "hits": {},
         "label": "journal, note: dollar-table review prompt held one mistyped line in the test_report.py hunk (the removed line showed $); the saved brief and the checkout were right; review file reads clean",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "dollar-table review ",
         "scenarios": [],
         "sig": "journal note on dollar-table"
        },
        {
         "call": 42,
         "candidates": [
          42,
          43
         ],
         "checks": [],
         "hits": {},
         "label": "journal, note: record-correction for integrate: reporting.org JSON scenario shows \"900.00\"; after dollar-table the JSON carries the table's amounts, \"$900.00\" (G1, D1, G2; probed on the CLI). Its review did not flag it.",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "record-correction fo",
         "scenarios": [],
         "sig": "journal note on dollar-table"
        }
       ],
       "n_calls": 44,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the execute phase agent's merge chain from its start (no baseline yet), from a handover to its hand-back, its spawns stood in by the host",
   "title": "Case execute-merge-chain: the merge chain from its start to the phase's close, its spawns stood in",
   "trials": 3,
   "validated": "56af72c0816f45acf4d08c2fe5d37dd6d1c7bb4d 20261114T111932 t1 (skill-evals)"
  },
  {
   "capability": "execute",
   "id": "execute-route-discoveries",
   "items": [
    {
     "checks": [
      {
       "id": "DS-S16",
       "kind": "decide"
      },
      {
       "id": "DS-S17",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § A merged task is not done until it is reviewed",
      "execute § A Reviewer runs at its role default or the task's raised setting",
      "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
     ],
     "id": "DS-E11",
     "question": "Is report-json reviewed by a Reviewer of its own, at its role default, before it is done?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "DS-E11": [
       "execute § A merged task is not done until it is reviewed",
       "execute § A Reviewer runs at its role default or the task's raised setting",
       "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "DS-E11": [
       "DS-S16",
       "DS-S17"
      ]
     },
     "items_then": [],
     "run": "20261114T112643-1",
     "trials": [
      {
       "calls": {
        "10": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && eval \"$(python3 <trial>/skills/orchestrate/scripts/state.py where --sh)\" && BR=task-report-json-1793000001 && git -C \"$CODE_ROOT\" rev-parse $BR && git -C \"$CODE_ROOT\" merge --no-ff \"$BR\" -m \"Merge task report-json: Add the JSON report (format_json and the --json flag)\" && git -C \"$CODE_ROOT\" rev-parse HEAD && TS=1793000100 && (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-report-json-$TS.txt\" 2>&1; echo status=$?; tail -8 \"$ORCH_CHANGE_DIR/after-report-json-$TS.txt\"",
         "tool": "Bash"
        },
        "14": {
         "error": false,
         "text": "python3 - <<'EOF'\np='tests/test_ledger.py'\ns=open(p).read()\ns=s.replace('''\n\nif __name__''','''\n    def test_one_cents_digit_is_tens_of_cents(self):\n        self.assertEqual(parse_line(\"2026-10-01,food,13.5\")[2], 1350)\n\n\nif __name__''')\nopen(p,'w').write(s)\nEOF\npython3 -m unittest discover -s tests -t . 2>&1 | tail -12",
         "tool": "Bash"
        },
        "15": {
         "error": false,
         "text": "sed -i 's/int(cents.zfill(2)\\[:2\\])/int((cents + \"00\")[:2])/' tally/ledger.py && python3 -m unittest discover -s tests -t . 2>&1 | tail -4 && git add tally/ledger.py tests/test_ledger.py && git commit -q -F - <<'EOF'\nInline fix after merging task report-json: restore right-padded cents in parse_line\n\nAnswers disc-report-json-1 (cycles/cycle-1/reports/report-json.md). The task's\ncommit changed parse_line from (cents + \"00\")[:2] to cents.zfill(2)[:2], which\nreads 13.5 as 13.05 (the base read it as 13.50); the change was outside the\ntask's steps. Pinned by tests/test_ledger.py::ParseLine::test_on … [cut, 1127 characters]",
         "tool": "Bash"
        },
        "16": {
         "error": false,
         "text": "python3 - <<'EOF'\ndef sub(p,a,b):\n    s=open(p).read(); assert a in s,(p,a); open(p,'w').write(s.replace(a,b))\nsub('tally/ledger.py','int(cents.zfill(2)[:2])','int((cents + \"00\")[:2])')\nEOF\npython3 -m unittest discover -s tests -t . 2>&1 | tail -3 && git add tally/ledger.py tests/test_ledger.py && git commit -q -F - <<'EOF'\nInline fix after merging task report-json: restore right-padded cents in parse_line\n\nAnswers disc-report-json-1 (cycles/cycle-1/reports/report-json.md). The task's\ncommit changed parse_line from (cents + \"00\")[:2] to cents.zfill(2)[:2], which\nreads 13.5 as 13.05 (the base r … [cut, 1292 characters]",
         "tool": "Bash"
        },
        "17": {
         "error": false,
         "text": "git add tally && git commit -q -F - <<'EOF'\nText correction: docstrings of cli.py and format_json after task report-json\n\nThe cli.py module docstring named a --csv option and a CSV output that do not\nexist (the change's non-goal N1); the format_json docstring said keys are\nsorted by name, but they are in the table's order (ordered, D1). Prose only.\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\nEOF\ngit rev-parse HEAD; git status --short",
         "tool": "Bash"
        },
        "18": {
         "error": false,
         "text": "cd <trial>/home && export ORCH_CHANGE=json-report && S=<trial>/skills/orchestrate/scripts/state.py && eval \"$(python3 $S where --sh)\" && (cd \"$CODE_ROOT\" && python3 -m unittest discover -s tests -t .) > \"$ORCH_CHANGE_DIR/after-report-json-1793000100-fix-1.txt\" 2>&1; echo $?; tail -3 \"$ORCH_CHANGE_DIR/after-report-json-1793000100-fix-1.txt\"\npython3 <trial>/skills/orchestrate/scripts/state.py task set report-json after_snapshot=.orchestrator/changes/json-report/after-report-json-1 … [cut, 1569 characters]",
         "tool": "Bash"
        },
        "19": {
         "error": false,
         "text": "export ORCH_CHANGE=json-report && S=<trial>/skills/orchestrate/scripts/state.py\ncat > .orchestrator/changes/json-report/hardening.org <<'EOF'\n#+TITLE: Hardening list: json-report\n#+OPTIONS: ^:{} -:nil\n\n* issue (hardening): A refund total is shown with the wrong cents, in the table and in the JSON report\n:PROPERTIES:\n:REF: cycles/cycle-1/reports/report-json.md (Observations)\n:SEVERITY: medium\n:FILES: tally/report.py:_money\n:END:\nOutside the change: =_money= formats a negative total with floor division, so a total of -12.05 shows as -13.95, against the spec … [cut, 2088 characters]",
         "tool": "Bash"
        },
        "26": {
         "error": true,
         "text": "Review task report-json",
         "tool": "Agent"
        }
       },
       "checks": {
        "DS-S16": {
         "audit": {
          "agree": true,
          "reason": "The task is done after one Reviewer spawn whose input holds the Summary.",
          "verdict": "agrees"
         },
         "desc": "report-json done only once its Reviewer was spawned",
         "detail": "status done; Reviewer spawns 1",
         "kind": "decide",
         "pass": true
        },
        "DS-S17": {
         "audit": {
          "agree": true,
          "reason": "The Reviewer was spawned at opus, high.",
          "verdict": "agrees"
         },
         "desc": "the Reviewer spawned at opus, and at high when an effort is given",
         "detail": "report-json: opus, high",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "DS-E11": "pass"
       },
       "judge": {
        "DS-E11": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"high\"",
          "\"model\": \"opus\", \"effort\": \"high\", \"prompt\": \"You are the Reviewer for task `report-json`",
          "\"description\": \"Review task report-json\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"high\""
         ],
         "reasons": [
          "A Reviewer was spawned at opus/high after the merge and test run, and the task was set done only afterwards (call 28).",
          "After the merge and test run, a Reviewer was spawned at opus/high, and the task was set done only after its clean review.",
          "After the merge and test run, a Reviewer was spawned at opus/high, and the task was set to done only after the review file came back (call 28)."
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
          "DS-S3"
         ],
         "hits": {
          "DS-S3": [
           "Merge task report-json: Add the JSON rep"
          ]
         },
         "label": "Merge task report-json: Add the JSON report (format_json and the --json flag)",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Merge task report-json: Add the JSON rep",
         "scenarios": [],
         "sig": "merge report-json"
        },
        {
         "call": 10,
         "candidates": [
          10
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after merge report-json: green"
        },
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "test run: fails (FAILED (failures=1, expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.001s\nFAILED (failures=1, expected failures=1)",
         "scenarios": [],
         "sig": "test run after merge report-json: red"
        },
        {
         "call": 15,
         "candidates": [
          15
         ],
         "checks": [],
         "hits": {},
         "label": "test run: fails (FAILED (failures=1, expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.001s\nFAILED (failures=1, expected failures=1)",
         "scenarios": [],
         "sig": "test run after merge report-json: red"
        },
        {
         "call": 16,
         "candidates": [
          15,
          16
         ],
         "checks": [
          "DS-S4"
         ],
         "hits": {
          "DS-S4": [
           "745fa59"
          ]
         },
         "label": "Inline fix after merging task report-json: restore right-padded cents in parse_line",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Inline fix after merging task report-jso",
         "scenarios": [],
         "sig": "inline fix after report-json"
        },
        {
         "call": 16,
         "candidates": [
          16
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.001s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after inline fix after report-json: green"
        },
        {
         "call": 16,
         "candidates": [
          16
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.001s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after inline fix after report-json: green"
        },
        {
         "call": 17,
         "candidates": [
          17
         ],
         "checks": [
          "DS-S9"
         ],
         "hits": {
          "DS-S9": [
           "221d3c1"
          ]
         },
         "label": "Text correction: docstrings of cli.py and format_json after task report-json",
         "lane": "git",
         "linked_by": "its text in the call",
         "match": "Text correction: docstrings of cli.py an",
         "scenarios": [],
         "sig": "commit: Text correction: docstrings of cli.py and format_json after …"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [
          "DS-S9"
         ],
         "hits": {
          "DS-S9": [
           "parse_line: restored (cents+'00')[:2]; t"
          ]
         },
         "label": "journal, inline-fix: parse_line: restored (cents+'00')[:2]; the task's zfill edit read 13.5 as 13.05 (disc-1; pinned by test_one_cents_digit_is_tens_of_cents, failed before)",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "parse_line: restored",
         "scenarios": [],
         "sig": "journal inline-fix on report-json"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [
          "DS-S9"
         ],
         "hits": {
          "DS-S9": [
           "text correction: cli.py docstring named "
          ]
         },
         "label": "journal, inline-fix: text correction: cli.py docstring named a nonexistent --csv/CSV output; format_json docstring said keys sorted by name (they are in the table's order)",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "text correction: cli",
         "scenarios": [],
         "sig": "journal inline-fix on report-json"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [
          "DS-S9"
         ],
         "hits": {
          "DS-S9": [
           "report-json's commit edited ledger.py an"
          ]
         },
         "label": "journal, discovery: report-json's commit edited ledger.py and cli.py's docstring beyond its steps and its report's deviation_summary was empty; disc-1's evidence described its own edit. fixed: inline fix 221d3c1^",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "report-json's commit",
         "scenarios": [],
         "sig": "journal discovery on report-json"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.001s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after commit: Text correction: docstrings of cli.py and format_json after …: green"
        },
        {
         "call": 19,
         "candidates": [
          19
         ],
         "checks": [],
         "hits": {},
         "label": "journal, discovery: disc-report-json-3 (4th ledger field refused) and the _money refund observation: off the main path, routed hardening (hardening.org)",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "disc-report-json-3 (",
         "scenarios": [],
         "sig": "journal discovery on report-json"
        },
        {
         "call": 19,
         "candidates": [
          19
         ],
         "checks": [],
         "hits": {},
         "label": "journal, discovery: disc-report-json-2 and -5 already settled by design.org D4 and D3 (user answers ask-json-report-2, -1); no work, no ask",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "disc-report-json-2 a",
         "scenarios": [],
         "sig": "journal discovery on report-json"
        },
        {
         "call": 19,
         "candidates": [
          19
         ],
         "checks": [
          "DS-S15"
         ],
         "hits": {
          "DS-S15": [
           "record-correction for integrate: design."
          ]
         },
         "label": "journal, note: record-correction for integrate: design.org Context says tally/cli.py 'reads no options'; it reads --json after this change (disc-report-json-4)",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "record-correction fo",
         "scenarios": [],
         "sig": "journal note on report-json"
        },
        {
         "call": 26,
         "candidates": [
          26
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Review task report-json",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Review task report-json",
         "scenarios": [],
         "sig": "spawn Reviewer report-json"
        }
       ],
       "n_calls": 29,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the execute phase agent routing an Implementor's report and merging its task while another Implementor runs, from a handover to the end of its turn, its spawns stood in by the host",
   "title": "Case execute-route-discoveries: an Implementor's report routed discovery by discovery, its notes kept from the diff",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T112643-1 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-recordonly-mvp",
   "items": [
    {
     "checks": [
      {
       "id": "RM-S3",
       "kind": "decide"
      },
      {
       "id": "RM-S4",
       "kind": "decide"
      },
      {
       "id": "RM-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The whole-change review reads the change from its base",
      "integrate § The whole-change review is author-blind",
      "integrate § Each agent integrate spawns runs at its role default",
      "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
     ],
     "id": "RM-E2",
     "question": "Does the whole-change Reviewer read the change from its base, author-blind, at its role default?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "RM-E2": [
       "integrate § The whole-change review reads the change from its base",
       "integrate § The whole-change review is author-blind",
       "integrate § Each agent integrate spawns runs at its role default",
       "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RM-E2": [
       "RM-S3",
       "RM-S4",
       "RM-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103835",
     "trials": [
      {
       "calls": {
        "11": {
         "error": false,
         "text": "S=<trial>/scratch/phase-integrate-cycle-3; mkdir -p $S/review-whole-change; python3 <trial>/skills/orchestrate/scripts/coverage.py --intent docs/changes/json-report/intent.org > $S/coverage-intent.txt; git rev-parse HEAD; python3 -m unittest discover -s tests -t . 2>&1 | tail -4",
         "tool": "Bash"
        },
        "13": {
         "error": true,
         "text": "Whole-change review json-report",
         "tool": "Agent"
        }
       },
       "checks": {
        "RM-S3": {
         "audit": {
          "agree": true,
          "reason": "The prompt names the base commit 5a7e127…, which matches intent.org's #+BASE. The seam cells are not quoted in the prompt text but are held through the design.org it names, which the item's 'files it names' allows, so 'all held' fits.",
          "verdict": "agrees"
         },
         "desc": "the Reviewer's input holds the base commit and every seam row",
         "detail": "all held",
         "kind": "decide",
         "pass": true
        },
        "RM-S4": {
         "audit": {
          "agree": true,
          "reason": "The prompt names no reports/<task>.md or per-task review file and has no Observations/Discoveries heading; it only forbids reading them.",
          "verdict": "agrees"
         },
         "desc": "the Reviewer's input holds no report, Observations or task review",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RM-S5": {
         "audit": {
          "agree": true,
          "reason": "The Agent call's input carries \"model\": \"opus\" and \"effort\": \"xhigh\".",
          "verdict": "agrees"
         },
         "desc": "the whole-change Reviewer at opus, xhigh",
         "detail": "('opus', 'xhigh')",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RM-E2": "pass"
       },
       "judge": {
        "RM-E2": {
         "quotes": [
          "the change's whole diff: `git -C <checkout> diff 5a7e127a2a39b2991f0cb7b26e7959ef8456a430..HEAD` ... intent.org and every Seams row of docs/changes/json-report/design.org ... coverage output for the intent ... the priors file ... Do not read implementor reports, task files' Observations/Discoveries, … [cut, 399 characters]",
          "the change's whole diff: `git -C <checkout> diff 5a7e127a2a39b2991f0cb7b26e7959ef8456a430..HEAD` (the base is intent.org's #+BASE:) ... Do not read implementor reports, task files' Observations/Discoveries, or any file under .orchestrator/ (no prior reviews either).",
          "`git -C <checkout> diff 5a7e127a2a39b2991f0cb7b26e7959ef8456a430..HEAD` (the base is intent.org's #+BASE:) ... \"model\": \"opus\", \"effort\": \"xhigh\""
         ],
         "reasons": [
          "The diff runs from the intent's #+BASE commit (5a7e127…) to HEAD. The input names the intent, the seam rows, the coverage, reviewer.md and the priors, and excludes reports, Observations and per-task reviews. The call passes opus, xhigh.",
          "The input gives the diff from the intent's #+BASE (5a7e127…) to HEAD, plus the intent, every Seams row of design.org, the coverage output, reviewer.md and the priors; it excludes reports, Observations and prior reviews, and passes opus and xhigh.",
          "The input holds the diff from the intent's #+BASE to HEAD, the intent, every Seams row, the intent's coverage output, reviewer.md and the priors, forbids implementor reports, Observations/Discoveries and prior reviews, and passes opus at xhigh."
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
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Whole-change review json-report",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Whole-change review json-report",
         "scenarios": [],
         "sig": "spawn Reviewer json-report"
        }
       ],
       "n_calls": 13,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after conformance (zero findings), to its first spawn or its hand-back",
   "title": "Case integrate-recordonly-mvp: a cycle that would file the MVP decision runs the whole-change review, whatever it merged",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103835 t1 (skill-evals)"
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
  },
  {
   "capability": "integrate",
   "id": "integrate-takein-clean",
   "items": [
    {
     "checks": [
      {
       "id": "TC-S4",
       "kind": "decide"
      },
      {
       "id": "TC-S5",
       "kind": "decide"
      },
      {
       "id": "TC-S6",
       "kind": "decide"
      },
      {
       "id": "TC-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A cycle that merged code gets a whole-change review",
      "integrate § The whole-change review reads the change from its base",
      "integrate § The whole-change review is author-blind",
      "integrate § Each agent integrate spawns runs at its role default",
      "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
     ],
     "id": "TC-E2",
     "question": "Does the cycle that merged code get a whole-change review, read from the change's base, author-blind, at its role default?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "TC-E2": [
       "integrate § A cycle that merged code gets a whole-change review",
       "integrate § The whole-change review reads the change from its base",
       "integrate § The whole-change review is author-blind",
       "integrate § Each agent integrate spawns runs at its role default",
       "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "TC-E2": [
       "TC-S4",
       "TC-S5",
       "TC-S6",
       "TC-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T114238",
     "trials": [
      {
       "calls": {
        "10": {
         "error": false,
         "text": "git merge --no-edit ws/json-report 2>&1 | tail -5; git status --short; python3 -m unittest discover -s tests -t . 2>&1 | tail -8",
         "tool": "Bash"
        },
        "14": {
         "error": true,
         "text": "Whole-change review",
         "tool": "Agent"
        }
       },
       "checks": {
        "TC-S4": {
         "audit": {
          "agree": true,
          "reason": "The single Agent call is described and prompted as the whole-change review.",
          "verdict": "agrees"
         },
         "desc": "the first spawn is the whole-change Reviewer",
         "detail": "1 spawn(s); the first: whole-change at ('opus', 'xhigh')",
         "kind": "decide",
         "pass": true
        },
        "TC-S5": {
         "audit": {
          "agree": true,
          "reason": "The spawn input carries model opus and effort xhigh.",
          "verdict": "agrees"
         },
         "desc": "the whole-change Reviewer at opus, xhigh",
         "detail": "('opus', 'xhigh')",
         "kind": "decide",
         "pass": true
        },
        "TC-S6": {
         "audit": {
          "agree": true,
          "reason": "The prompt names the base sha and points to design.org, which holds both Seams rows in full; nothing in the evidence contradicts the base being the intent's.",
          "verdict": "agrees"
         },
         "desc": "the Reviewer's input holds the base commit and every seam row",
         "detail": "all held",
         "kind": "decide",
         "pass": true
        },
        "TC-S7": {
         "audit": {
          "agree": true,
          "reason": "The prompt names no reports/<task>.md or task review file and has no Observations or Discoveries heading.",
          "verdict": "agrees"
         },
         "desc": "the Reviewer's input holds no report, Observations or task review",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "TC-E2": "pass"
       },
       "judge": {
        "TC-E2": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"xhigh\" … Diff: `git -C <code root> diff 5a7e127a2a39b2991f0cb7b26e7959ef8456a430..HEAD` (the change's #+BASE: to HEAD) … You are author-blind: do not look for implementor reports, observations, or other review files.",
          "\"model\": \"opus\", \"effort\": \"xhigh\" … Diff: `git -C <code root> diff 5a7e127a2a39b2991f0cb7b26e7959ef8456a430..HEAD` (the change's #+BASE: to HEAD) … Design (every Seams row) … You are author-blind: do not look for implementor reports, observations, or other review files.",
          "\"model\": \"opus\", \"effort\": \"xhigh\" ... Diff: `git -C <code root> diff 5a7e127a2a39b2991f0cb7b26e7959ef8456a430..HEAD` (the change's #+BASE: to HEAD) ... Design (every Seams row) ... Coverage: run `python3 .../coverage.py` ... You are author-blind: do not look for implementor reports, observations, o … [cut, 321 characters]"
         ],
         "reasons": [
          "The first and only spawn is one whole-change Reviewer at opus/xhigh, with these inputs: the base-to-HEAD diff after the take-in, the intent, the design with every Seams row, the coverage run, and the reviewer role brief (priors were checked for and found absent); no report, Observations or per-task review is named. The json-report intent's #+BASE value is not shown directly, but nothing contradicts the cited sha.",
          "The first and only spawn is one whole-change Reviewer at opus/xhigh; its input gives the base-to-HEAD diff (HEAD includes the take-in), the intent, the design with every seam row, the coverage run, the reviewer role brief, and states there is no priors file; it names no implementor report, Observations or per-task review.",
          "The first spawn is one whole-change Reviewer at opus/xhigh. It reads the diff from the base to HEAD after the take-in, plus the intent, the design's seams, coverage, the reviewer role brief, and specs. It states that there are no priors or overlay. It names no implementor report, Observations, or per-task review."
         ]
        }
       },
       "milestones": [
        {
         "call": 10,
         "candidates": [
          10
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=1)",
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
         "label": "spawn: Whole-change review",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Whole-change review",
         "scenarios": [],
         "sig": "spawn Reviewer None"
        }
       ],
       "n_calls": 14,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after conformance, to its first spawn or its hand-back",
   "title": "Case integrate-takein-clean: the workspace integration branch taken in, then the whole-change review",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114238 t1 (skill-evals)"
  },
  {
   "capability": "reviewer",
   "id": "reviewer-clean",
   "items": [
    {
     "checks": [
      {
       "id": "RC-S1",
       "kind": "decide"
      },
      {
       "id": "RC-S2",
       "kind": "decide"
      },
      {
       "id": "RC-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The review file is at the path its brief names",
      "reviewer § The review file opens with what was reviewed and what was found",
      "reviewer § The review file holds a summary, the numbered findings and the thoughts",
      "record § A cycle's directory holds what its steps wrote, under fixed names"
     ],
     "id": "RC-E1",
     "question": "Is the review file at the path the brief names, in its form?"
    },
    {
     "checks": [
      {
       "id": "RC-S3",
       "kind": "decide"
      },
      {
       "id": "RC-S8",
       "kind": "decide"
      },
      {
       "id": "RC-S9",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § A review with no finding says what was looked for",
      "reviewer § The finding count and the clean mark agree with the findings"
     ],
     "id": "RC-E2",
     "question": "With nothing found, does the file say so in its count and mark, and say what was looked for and why each was ruled out?"
    },
    {
     "checks": [
      {
       "id": "RC-S5",
       "kind": "decide"
      },
      {
       "id": "RC-S6",
       "kind": "decide"
      },
      {
       "id": "RC-S7",
       "kind": "decide"
      },
      {
       "id": "RC-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § Style and preference are not findings",
      "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
      "reviewer § A recommended action is an inline fix, a rework or the user's",
      "reviewer § Every finding says what it blocks",
      "reviewer § The reporter leaves the route to the routing"
     ],
     "id": "RC-E3",
     "question": "Is nothing of style or preference a finding, and does each finding, if any, carry its labels?"
    },
    {
     "checks": [
      {
       "id": "RC-S11",
       "kind": "decide"
      },
      {
       "id": "RC-S12",
       "kind": "decide"
      },
      {
       "id": "RC-S13",
       "kind": "decide"
      },
      {
       "id": "RC-S14",
       "kind": "decide"
      },
      {
       "id": "RC-S15",
       "kind": "decide"
      },
      {
       "id": "RC-S16",
       "kind": "decide"
      },
      {
       "id": "RC-S17",
       "kind": "decide"
      },
      {
       "id": "RC-S18",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The Reviewer applies no fix and moves no ref",
      "reviewer § The Reviewer leaves the task file alone",
      "reviewer § A test run leaves the checkout as git sees it",
      "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
      "reviewer § Every run ends before the hand-back",
      "reviewer § The Reviewer leaves what other agents share alone"
     ],
     "id": "RC-E4",
     "question": "Does the Reviewer change nothing it reviews, probe in its scratch directory and leave what others share alone?"
    },
    {
     "checks": [
      {
       "id": "RC-S19",
       "kind": "decide"
      },
      {
       "id": "RC-S20",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The hand-back line counts the findings",
      "reviewer § The hand-back carries no path and no summary"
     ],
     "id": "RC-E5",
     "question": "Is the hand-back one line of counts, zero here, naming no path?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "RC-E1": [
       "reviewer § The review file is at the path its brief names",
       "reviewer § The review file opens with what was reviewed and what was found",
       "reviewer § The review file holds a summary, the numbered findings and the thoughts",
       "record § A cycle's directory holds what its steps wrote, under fixed names"
      ],
      "RC-E2": [
       "reviewer § A review with no finding says what was looked for",
       "reviewer § The finding count and the clean mark agree with the findings"
      ],
      "RC-E3": [
       "reviewer § Style and preference are not findings",
       "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
       "reviewer § A recommended action is an inline fix, a rework or the user's",
       "reviewer § Every finding says what it blocks",
       "reviewer § The reporter leaves the route to the routing"
      ],
      "RC-E4": [
       "reviewer § The Reviewer applies no fix and moves no ref",
       "reviewer § The Reviewer leaves the task file alone",
       "reviewer § A test run leaves the checkout as git sees it",
       "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
       "reviewer § Every run ends before the hand-back",
       "reviewer § The Reviewer leaves what other agents share alone"
      ],
      "RC-E5": [
       "reviewer § The hand-back line counts the findings",
       "reviewer § The hand-back carries no path and no summary"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "RC-E1": [
       "RC-S1",
       "RC-S2",
       "RC-S4"
      ],
      "RC-E2": [
       "RC-S3",
       "RC-S8",
       "RC-S9"
      ],
      "RC-E3": [
       "RC-S10",
       "RC-S5",
       "RC-S6",
       "RC-S7"
      ],
      "RC-E4": [
       "RC-S11",
       "RC-S12",
       "RC-S13",
       "RC-S14",
       "RC-S15",
       "RC-S16",
       "RC-S17",
       "RC-S18"
      ],
      "RC-E5": [
       "RC-S19",
       "RC-S20"
      ]
     },
     "items_then": [],
     "run": "20261114T123155",
     "trials": [
      {
       "calls": {
        "4": {
         "error": false,
         "text": "cat docs/changes/json-report/design.org docs/changes/json-report/intent.org tests/test_json.py; grep -rn -i 'comment\\|semicolon' tally tests docs; python3 -m unittest discover -s tests -t . 2>&1 | tail -15",
         "tool": "Bash"
        }
       },
       "checks": {
        "RC-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists at the named path.",
          "verdict": "agrees"
         },
         "desc": "the review file at the path the brief names",
         "detail": "<trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/reviews/ledger-comments.md",
         "kind": "decide",
         "pass": true
        },
        "RC-S10": {
         "audit": {
          "agree": true,
          "reason": "No ask file was added.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RC-S11": {
         "audit": {
          "agree": true,
          "reason": "Refs are unchanged and the checkout HEAD is the merge.",
          "verdict": "agrees"
         },
         "desc": "no ref moved; the checkout at the reviewed commit",
         "detail": "refs unchanged: True; checkout HEAD adca26f04788a6dd535629187763a9700731d710, reviewed adca26f04788a6dd535629187763a9700731d710",
         "kind": "decide",
         "pass": true
        },
        "RC-S12": {
         "audit": {
          "agree": true,
          "reason": "The checkout and code root are clean, and the home shows only the review file's line, which the check sets aside.",
          "verdict": "agrees"
         },
         "desc": "the checkout, the code root and the home as git sees them: as after the build (the checkout clean)",
         "detail": "checkout: clean | code root: clean | home: clean",
         "kind": "decide",
         "pass": true
        },
        "RC-S13": {
         "audit": {
          "agree": true,
          "reason": "Nothing in the calls touches the task file, and the evidence shows no other home change.",
          "verdict": "agrees"
         },
         "desc": "the task file as the fixture wrote it",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "RC-S14": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows the scratch directory is gone.",
          "verdict": "agrees"
         },
         "desc": "the scratch directory gone",
         "detail": "<trial>/scratch/review-ledger-comments",
         "kind": "decide",
         "pass": true
        },
        "RC-S15": {
         "audit": {
          "agree": true,
          "reason": "Every path named is inside the trial directory, and no mktemp was run.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RC-S16": {
         "audit": {
          "agree": true,
          "reason": "No other file in the home was added, changed or removed.",
          "verdict": "agrees"
         },
         "desc": "the review file the only file added or changed in the home",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RC-S17": {
         "audit": {
          "agree": true,
          "reason": "No call ran in the background.",
          "verdict": "agrees"
         },
         "desc": "no run in the background",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RC-S18": {
         "audit": {
          "agree": true,
          "reason": "No stash, worktree prune or remove, or gc appears.",
          "verdict": "agrees"
         },
         "desc": "no git stash, worktree prune or remove, or gc",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RC-S19": {
         "audit": {
          "agree": true,
          "reason": "The line has the form and its counts match the file; whether zeros were due is explicitly left to the eval agent.",
          "verdict": "agrees"
         },
         "desc": "the hand-back one line of counts matching the file",
         "detail": "'reviewed ledger-comments issues=2 questions=0 spec-signal=0 thoughts=1'; the file: issues=2 questions=0 spec-signal=0",
         "kind": "decide",
         "pass": true
        },
        "RC-S2": {
         "audit": {
          "agree": true,
          "reason": "All five keys are present and merge_commit is adca26f…, the merge reviewed.",
          "verdict": "agrees"
         },
         "desc": "front matter with the five fields, merge_commit the reviewed commit",
         "detail": "missing []; merge_commit='adca26f04788a6dd535629187763a9700731d710'; reviewed adca26f04788a6dd535629187763a9700731d710",
         "kind": "decide",
         "pass": true
        },
        "RC-S20": {
         "audit": {
          "agree": true,
          "reason": "The final text contains no slash.",
          "verdict": "agrees"
         },
         "desc": "the hand-back names no path",
         "detail": "reviewed ledger-comments issues=2 questions=0 spec-signal=0 thoughts=1",
         "kind": "decide",
         "pass": true
        },
        "RC-S3": {
         "audit": {
          "agree": true,
          "reason": "The count of 2 and clean_review false are consistent with the two headings, which is all this check reads.",
          "verdict": "agrees"
         },
         "desc": "finding_count and clean_review agree with the findings",
         "detail": "finding_count='2' clean_review='false'; 2 finding heading(s)",
         "kind": "decide",
         "pass": true
        },
        "RC-S4": {
         "audit": {
          "agree": true,
          "reason": "It reads section order only, and Thoughts does come after Findings; whether Thoughts is last is left to the eval agent by design.",
          "verdict": "agrees"
         },
         "desc": "a summary, the findings numbered from 1, thoughts after them when present",
         "detail": "sections: ['Summary', 'Findings', 'Thoughts', 'What I looked for and ruled out']; finding numbers [1, 2]; thoughts present: True",
         "kind": "decide",
         "pass": true
        },
        "RC-S5": {
         "audit": {
          "agree": true,
          "reason": "Both findings carry kind, severity, direction, a location with a line, and evidence.",
          "verdict": "agrees"
         },
         "desc": "every finding: kind, severity, direction, a location with a line, evidence",
         "detail": "#1 kind=issue severity=low direction=design-drift location=True evidence=True; #2 kind=issue severity=low direction=sub-par-code location=True evidence=True",
         "kind": "decide",
         "pass": true
        },
        "RC-S6": {
         "audit": {
          "agree": true,
          "reason": "Both recommended actions are inline-fix.",
          "verdict": "agrees"
         },
         "desc": "every recommended action is inline-fix, rework or route-to-user",
         "detail": "#1 inline-fix; #2 inline-fix",
         "kind": "decide",
         "pass": true
        },
        "RC-S7": {
         "audit": {
          "agree": true,
          "reason": "Both findings have a blocks value.",
          "verdict": "agrees"
         },
         "desc": "every finding says what it blocks",
         "detail": "#1 blocks='done'; #2 blocks='nothing'",
         "kind": "decide",
         "pass": true
        },
        "RC-S8": {
         "audit": {
          "agree": false,
          "reason": "It returns n/a because finding headings exist, but those findings are false positives and the premise holds, so a clean count and mark were due and the check misses that.",
          "verdict": "suspect"
         },
         "desc": "no finding: finding_count 0 and clean_review true (not applicable when the file has findings: the premise is reread)",
         "detail": "2 finding(s); finding_count='2' clean_review='false'",
         "kind": "decide",
         "pass": null
        },
        "RC-S9": {
         "audit": {
          "agree": false,
          "reason": "Its not-applicable verdict changes nothing, since the file does have a section on what was looked for and ruled out; the item fails on the count and the mark, not on this.",
          "verdict": "agrees"
         },
         "desc": "with no finding, the file says what was looked for (not applicable with a finding)",
         "detail": "I read the diff in `tally/ledger.py` and `tests/test_ledger.py`. I checked it against the `seam/ledger-comments` row, the scenario \"reporting § A comment line is skipped\", and the requirement text in `docs/specs/reporting.org`. I ran the test suite at the merge commit: 8 tests pass, with 1 expected  … [cut, 400 characters]",
         "kind": "signal",
         "pass": null
        }
       },
       "items": {
        "RC-E1": "fail",
        "RC-E2": "fail",
        "RC-E3": "fail",
        "RC-E4": "pass",
        "RC-E5": "fail"
       },
       "judge": {
        "RC-E1": {
         "quotes": [
          "## Thoughts ... ## What I looked for and ruled out",
          "sections: ['Summary', 'Findings', 'Thoughts', 'What I looked for and ruled out']",
          "sections: ['Summary', 'Findings', 'Thoughts', 'What I looked for and ruled out']"
         ],
         "reasons": [
          "The path, the five front-matter keys with the reviewed merge, the one-paragraph summary and the Findings section are all right, but Thoughts is followed by another section, so it is not at the end.",
          "The path, the five front-matter keys, the merge commit, the one-paragraph summary and the Findings section are all right, but `## Thoughts` is followed by another section, so it is not at the end.",
          "The path, the five front-matter keys, the merge_commit, the one-paragraph summary and the Findings section are all right, but the item requires `## Thoughts` to be at the end and here another section follows it."
         ]
        },
        "RC-E2": {
         "quotes": [
          "finding_count: 2\nclean_review: false",
          "finding_count: 2\nclean_review: false",
          "finding_count: 2\nclean_review: false"
         ],
         "reasons": [
          "On rereading, the premise holds and neither finding is a real defect of the change, so the file should have read finding_count 0 and clean_review true. Finding 1's Context sentence (\"=#= today, and =;= too once =seam/ledger-comments= is built\") stays true once the seam is built, as the build confirmed. Finding 2's BOM behaviour predates the diff, applies equally to #, and matches the spec's literal \"first character\".",
          "Neither finding is a real defect of the diff or its neighbours, so the premise stands and the file should read 0 and true. Finding 1 calls the Context untrue, but its conditional wording ('=;= too once =seam/ledger-comments= is built') stays true after the build, as the case confirmed. Finding 2's BOM handling predates the change, is outside the spec and the scenario, and the Reviewer itself says the same defect existed before for `#`.",
          "Neither finding is a real defect, so the premise holds and the review should have been clean: Finding 1 calls stale a Context sentence (\"=;= too once =seam/ledger-comments= is built\") that stays true once the seam is built, as the case confirms; Finding 2 is pre-existing behaviour the spec does not cover (\"Neither covers a BOM\"), since a BOM-prefixed line's first character is not `;`."
         ]
        },
        "RC-E3": {
         "quotes": [
          "The fix belongs to the orchestrator or the Architect.",
          "The fix belongs to the orchestrator or the Architect.",
          "The fix belongs to the orchestrator or the Architect."
         ],
         "reasons": [
          "Finding 1 objects to the wording of a sentence that is still true, which is a preference rather than a defect, and it names a route, which the reporter must leave to the routing; the labels are otherwise present.",
          "Both findings carry all their labels and no ask was filed, but Finding 1 names a route. Finding 1 also concerns the wording of a sentence that is still true, which is a preference rather than a defect.",
          "The findings carry all their labels and none is about style, but Finding 1 names a route, which the item forbids."
         ]
        },
        "RC-E4": {
         "quotes": [
          "rm -rf .../scratch/review-ledger-comments ... !! tally/__pycache__/",
          "rm -rf <trial>/scratch/review-ledger-comments ... git status --short --ignored",
          "rm -rf <trial>/scratch/review-ledger-comments ... !! tally/__pycache__/"
         ],
         "reasons": [
          "Probes ran in the scratch directory, which was then removed. The checkout is clean apart from ignored caches, the review file is the only change in the home, no ref moved, and there was no background run, stash, prune or gc.",
          "Probes ran in the scratch directory, which is gone. The checkout is clean apart from ignored caches, the home changed only in the review file, and no ref, stash, prune, gc or background run appears.",
          "No ref moved, the checkout is clean at the merge with only ignored caches, the home's only change is the review file, the scratch directory is gone, nothing ran in the background and there was no stash, prune or gc."
         ]
        },
        "RC-E5": {
         "quotes": [
          "reviewed ledger-comments issues=2 questions=0 spec-signal=0 thoughts=1",
          "reviewed ledger-comments issues=2 questions=0 spec-signal=0 thoughts=1",
          "reviewed ledger-comments issues=2 questions=0 spec-signal=0 thoughts=1"
         ],
         "reasons": [
          "The line has the right form, names no path and matches the file's counts, but the premise holds, so issues should have been 0.",
          "The line is one line, names no path and matches the file's counts, but no real defect was found, so it should read issues=0.",
          "The line's form and counts match the file, but the premise holds (no real defect), so the item's zero issue count was due and the line reports 2."
         ]
        }
       },
       "milestones": [
        {
         "call": 4,
         "candidates": [
          4
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
       "n_calls": 7,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Reviewer's review of one merged task, from its brief to its hand-back",
   "title": "Case reviewer-clean: a review that finds nothing says what it looked for, with a zero count and a clean mark",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T123155 t1 (skill-evals)"
  },
  {
   "capability": "reviewer",
   "id": "reviewer-flawed-diff",
   "items": [
    {
     "checks": [
      {
       "id": "RD-S1",
       "kind": "decide"
      },
      {
       "id": "RD-S2",
       "kind": "decide"
      },
      {
       "id": "RD-S3",
       "kind": "decide"
      },
      {
       "id": "RD-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The review file is at the path its brief names",
      "reviewer § The review file opens with what was reviewed and what was found",
      "reviewer § The finding count and the clean mark agree with the findings",
      "reviewer § The review file holds a summary, the numbered findings and the thoughts",
      "record § A cycle's directory holds what its steps wrote, under fixed names"
     ],
     "id": "RD-E1",
     "question": "Is the review file at the path the brief names, in its form?"
    },
    {
     "checks": [
      {
       "id": "RD-S8",
       "kind": "decide"
      },
      {
       "id": "RD-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § A pending marker left, a cited scenario's test untouched or an edit under docs is a finding"
     ],
     "id": "RD-E2",
     "question": "Are the left marker and the edit of the change's design each a finding?"
    },
    {
     "checks": [
      {
       "id": "RD-S10",
       "kind": "decide"
      },
      {
       "id": "RD-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § A document the diff makes untrue is a finding",
      "reviewer § A finding about the change's work names the direction it looks in"
     ],
     "id": "RD-E3",
     "question": "Is the docstring the diff made untrue a finding of kind issue?"
    },
    {
     "checks": [
      {
       "id": "RD-S12",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § Style and preference are not findings"
     ],
     "id": "RD-E4",
     "question": "Is the variable's naming left out of the findings?"
    },
    {
     "checks": [
      {
       "id": "RD-S5",
       "kind": "decide"
      },
      {
       "id": "RD-S6",
       "kind": "decide"
      },
      {
       "id": "RD-S7",
       "kind": "decide"
      },
      {
       "id": "RD-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
      "reviewer § A recommended action is an inline fix, a rework or the user's",
      "reviewer § Every finding says what it blocks",
      "reviewer § The reporter leaves the route to the routing",
      "reviewer § Severity is the harm when the defect triggers",
      "reviewer § An issue's evidence shows the defect and who it harms"
     ],
     "id": "RD-E5",
     "question": "Does every finding carry its labels, what it blocks and a recommended action of the three kinds, leaving the route to the routing?"
    },
    {
     "checks": [
      {
       "id": "RD-S14",
       "kind": "decide"
      },
      {
       "id": "RD-S15",
       "kind": "decide"
      },
      {
       "id": "RD-S16",
       "kind": "decide"
      },
      {
       "id": "RD-S17",
       "kind": "decide"
      },
      {
       "id": "RD-S18",
       "kind": "decide"
      },
      {
       "id": "RD-S19",
       "kind": "decide"
      },
      {
       "id": "RD-S20",
       "kind": "decide"
      },
      {
       "id": "RD-S21",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The Reviewer applies no fix and moves no ref",
      "reviewer § The Reviewer leaves the task file alone",
      "reviewer § A test run leaves the checkout as git sees it",
      "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
      "reviewer § Every run ends before the hand-back",
      "reviewer § The Reviewer leaves what other agents share alone"
     ],
     "id": "RD-E6",
     "question": "Does the Reviewer change nothing it reviews, probe in its scratch directory and leave what others share alone?"
    },
    {
     "checks": [
      {
       "id": "RD-S22",
       "kind": "decide"
      },
      {
       "id": "RD-S23",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The hand-back line counts the findings",
      "reviewer § The hand-back carries no path and no summary"
     ],
     "id": "RD-E7",
     "question": "Is the hand-back one line of counts, naming no path?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0abfdc5",
     "cites": {
      "RD-E1": [
       "reviewer § The review file is at the path its brief names",
       "reviewer § The review file opens with what was reviewed and what was found",
       "reviewer § The finding count and the clean mark agree with the findings",
       "reviewer § The review file holds a summary, the numbered findings and the thoughts",
       "record § A cycle's directory holds what its steps wrote, under fixed names"
      ],
      "RD-E2": [
       "reviewer § A pending marker left, a cited scenario's test untouched or an edit under docs is a finding"
      ],
      "RD-E3": [
       "reviewer § A document the diff makes untrue is a finding",
       "reviewer § A finding about the change's work names the direction it looks in"
      ],
      "RD-E4": [
       "reviewer § Style and preference are not findings"
      ],
      "RD-E5": [
       "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
       "reviewer § A recommended action is an inline fix, a rework or the user's",
       "reviewer § Every finding says what it blocks",
       "reviewer § The reporter leaves the route to the routing",
       "reviewer § Severity is the harm when the defect triggers",
       "reviewer § An issue's evidence shows the defect and who it harms"
      ],
      "RD-E6": [
       "reviewer § The Reviewer applies no fix and moves no ref",
       "reviewer § The Reviewer leaves the task file alone",
       "reviewer § A test run leaves the checkout as git sees it",
       "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
       "reviewer § Every run ends before the hand-back",
       "reviewer § The Reviewer leaves what other agents share alone"
      ],
      "RD-E7": [
       "reviewer § The hand-back line counts the findings",
       "reviewer § The hand-back carries no path and no summary"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "RD-E1": [
       "RD-S1",
       "RD-S2",
       "RD-S3",
       "RD-S4"
      ],
      "RD-E2": [
       "RD-S8",
       "RD-S9"
      ],
      "RD-E3": [
       "RD-S10",
       "RD-S11"
      ],
      "RD-E4": [
       "RD-S12"
      ],
      "RD-E5": [
       "RD-S13",
       "RD-S5",
       "RD-S6",
       "RD-S7"
      ],
      "RD-E6": [
       "RD-S14",
       "RD-S15",
       "RD-S16",
       "RD-S17",
       "RD-S18",
       "RD-S19",
       "RD-S20",
       "RD-S21"
      ],
      "RD-E7": [
       "RD-S22",
       "RD-S23"
      ]
     },
     "items_then": [],
     "run": "20261114T120932",
     "trials": [
      {
       "calls": {
        "4": {
         "error": false,
         "text": "python3 -m unittest discover -s tests -t . -v 2>&1; echo \"exit=$?\"; mkdir -p <trial>/scratch/review-report-json && cd <trial>/scratch/review-report-json && printf '2026-10-01,food,13.05\\n2026-10-02,rent,900.00\\n' > ex.csv && printf '2026-10-01,zeta,5.00\\n2026-10-02,alpha,5.00\\n' > tie.csv && printf '2026-10-01,food,-12.05\\n' > refund.csv && R=<trial>/checkouts/review-report-json && for a in \"ex.csv --json\" \"ex.csv\" \"tie.csv\" \"tie.csv --json\" \"refund.csv\" \"refund.csv --json\" \"ex.csv --yaml\" \"--json ex.csv\"; do echo \"== tally $a\"; PYTHONPATH=$R python3 -m tally.cli $a 2>&1 | tail -2; echo \"exit= … [cut, 684 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "RD-S1": {
         "audit": {
          "agree": true,
          "reason": "The review file exists at the named path, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the review file at the path the brief names",
         "detail": "<trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/reviews/report-json.md",
         "kind": "decide",
         "pass": true
        },
        "RD-S10": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 is located at report.py:12-14, the lines of ordered.",
          "verdict": "agrees"
         },
         "desc": "a finding located at ordered, whose docstring the diff makes untrue",
         "detail": "tally/report.py ordered lines [12, 13, 14]; findings there: #1",
         "kind": "decide",
         "pass": true
        },
        "RD-S11": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 is of kind issue.",
          "verdict": "agrees"
         },
         "desc": "a finding located at ordered is of kind issue (not applicable without one)",
         "detail": "#1 kind=issue",
         "kind": "decide",
         "pass": true
        },
        "RD-S12": {
         "audit": {
          "agree": true,
          "reason": "No heading mentions style or naming.",
          "verdict": "agrees"
         },
         "desc": "no finding's heading is about naming or style",
         "detail": "none",
         "kind": "signal",
         "pass": true
        },
        "RD-S13": {
         "audit": {
          "agree": true,
          "reason": "No files other than the review file were added in the home.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RD-S14": {
         "audit": {
          "agree": true,
          "reason": "No call moved a ref, and the checkout HEAD is the merge.",
          "verdict": "agrees"
         },
         "desc": "no ref moved; the checkout at the reviewed commit",
         "detail": "refs unchanged: True; checkout HEAD e5430070052536236edba4bd1f3343cae07f58ca, reviewed e5430070052536236edba4bd1f3343cae07f58ca",
         "kind": "decide",
         "pass": true
        },
        "RD-S15": {
         "audit": {
          "agree": true,
          "reason": "All three git statuses are clean, as shown.",
          "verdict": "agrees"
         },
         "desc": "the checkout, the code root and the home as git sees them: as after the build (the checkout clean)",
         "detail": "checkout: clean | code root: clean | home: clean",
         "kind": "decide",
         "pass": true
        },
        "RD-S16": {
         "audit": {
          "agree": true,
          "reason": "No call touched the task file.",
          "verdict": "agrees"
         },
         "desc": "the task file as the fixture wrote it",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "RD-S17": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows the scratch directory gone after the rm -rf.",
          "verdict": "agrees"
         },
         "desc": "the scratch directory gone",
         "detail": "<trial>/scratch/review-report-json",
         "kind": "decide",
         "pass": true
        },
        "RD-S18": {
         "audit": {
          "agree": true,
          "reason": "All paths in the calls are under the trial's directory, and no mktemp was run.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RD-S19": {
         "audit": {
          "agree": true,
          "reason": "The evidence lists no other files changed in the home.",
          "verdict": "agrees"
         },
         "desc": "the review file the only file added or changed in the home",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RD-S2": {
         "audit": {
          "agree": true,
          "reason": "The front matter has the five keys and merge_commit equals the reviewed merge.",
          "verdict": "agrees"
         },
         "desc": "front matter with the five fields, merge_commit the reviewed commit",
         "detail": "missing []; merge_commit='e5430070052536236edba4bd1f3343cae07f58ca'; reviewed e5430070052536236edba4bd1f3343cae07f58ca",
         "kind": "decide",
         "pass": true
        },
        "RD-S20": {
         "audit": {
          "agree": true,
          "reason": "No call ran in the background.",
          "verdict": "agrees"
         },
         "desc": "no run in the background",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RD-S21": {
         "audit": {
          "agree": true,
          "reason": "No stash, worktree prune or remove, or gc appears in the commands.",
          "verdict": "agrees"
         },
         "desc": "no git stash, worktree prune or remove, or gc",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RD-S22": {
         "audit": {
          "agree": true,
          "reason": "The final line has the required form and its counts match the file.",
          "verdict": "agrees"
         },
         "desc": "the hand-back one line of counts matching the file",
         "detail": "'reviewed report-json issues=6 questions=0 spec-signal=0 thoughts=0'; the file: issues=6 questions=0 spec-signal=0",
         "kind": "decide",
         "pass": true
        },
        "RD-S23": {
         "audit": {
          "agree": true,
          "reason": "The final text contains no slash.",
          "verdict": "agrees"
         },
         "desc": "the hand-back names no path",
         "detail": "reviewed report-json issues=6 questions=0 spec-signal=0 thoughts=0",
         "kind": "decide",
         "pass": true
        },
        "RD-S3": {
         "audit": {
          "agree": true,
          "reason": "finding_count 6 matches the six headings and clean_review is false.",
          "verdict": "agrees"
         },
         "desc": "finding_count and clean_review agree with the findings",
         "detail": "finding_count='6' clean_review='false'; 6 finding heading(s)",
         "kind": "decide",
         "pass": true
        },
        "RD-S4": {
         "audit": {
          "agree": true,
          "reason": "Summary, findings numbered 1-6 in order, and no Thoughts section match the file.",
          "verdict": "agrees"
         },
         "desc": "a summary, the findings numbered from 1, thoughts after them when present",
         "detail": "sections: ['Summary', 'Findings', 'What I looked for and ruled out']; finding numbers [1, 2, 3, 4, 5, 6]; thoughts present: False",
         "kind": "decide",
         "pass": true
        },
        "RD-S5": {
         "audit": {
          "agree": true,
          "reason": "Each finding's labels, location and evidence are present, as the file shows.",
          "verdict": "agrees"
         },
         "desc": "every finding: kind, severity, direction, a location with a line, evidence",
         "detail": "#1 kind=issue severity=high direction=sub-par-code location=True evidence=True; #2 kind=issue severity=high direction=design-drift location=True evidence=True; #3 kind=issue severity=medium direction=design-drift location=True evidence=True; #4 kind=issue severity=medium direction=sub-par-code locat … [cut, 481 characters]",
         "kind": "decide",
         "pass": true
        },
        "RD-S6": {
         "audit": {
          "agree": true,
          "reason": "All six actions are inline-fix.",
          "verdict": "agrees"
         },
         "desc": "every recommended action is inline-fix, rework or route-to-user",
         "detail": "#1 inline-fix; #2 inline-fix; #3 inline-fix; #4 inline-fix; #5 inline-fix; #6 inline-fix",
         "kind": "decide",
         "pass": true
        },
        "RD-S7": {
         "audit": {
          "agree": true,
          "reason": "Every finding carries a blocks value.",
          "verdict": "agrees"
         },
         "desc": "every finding says what it blocks",
         "detail": "#1 blocks='task report-json'; #2 blocks='merge'; #3 blocks='task report-json'; #4 blocks='task report-json'; #5 blocks='done'; #6 blocks='nothing'",
         "kind": "decide",
         "pass": true
        },
        "RD-S8": {
         "audit": {
          "agree": true,
          "reason": "Finding 2 is located at tests/test_json.py:7, the decorator line.",
          "verdict": "agrees"
         },
         "desc": "a finding located at the pending marker left on the promoted test",
         "detail": "tests/test_json.py lines [7, 8]; findings there: #2",
         "kind": "decide",
         "pass": true
        },
        "RD-S9": {
         "audit": {
          "agree": true,
          "reason": "Finding 3 is located at design.org:16.",
          "verdict": "agrees"
         },
         "desc": "a finding located in the change's design.org, which the diff edits",
         "detail": "#3",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RD-E1": "pass",
        "RD-E2": "pass",
        "RD-E3": "pass",
        "RD-E4": "pass",
        "RD-E5": "pass",
        "RD-E6": "pass",
        "RD-E7": "pass"
       },
       "judge": {
        "RD-E1": {
         "quotes": [
          "finding_count: 6\nclean_review: false",
          "finding_count: 6\nclean_review: false",
          "finding_count: 6\nclean_review: false"
         ],
         "reasons": [
          "The file is at the brief's path with all five front-matter keys and merge_commit e5430070…; the count matches the six numbered findings and clean_review is false; the summary is one paragraph; there is no Thoughts section, and none is needed with no thought.",
          "The file is at the named path. Its front matter has the five keys and merge_commit e5430070…, and the count of 6 matches six findings with clean_review false. It has a one-paragraph Summary, findings numbered 1–6, and no Thoughts section.",
          "The file is at the named path in the home. Its front matter has the five keys, with merge_commit e5430070…, a count of 6 matching six numbered findings and clean_review false. It has a one-paragraph summary, findings numbered 1–6 and no thoughts section."
         ]
        },
        "RD-E2": {
         "quotes": [
          "### Finding 2: The promoted test keeps its `expectedFailure` marker … - file: tests/test_json.py:7 … ### Finding 3: The diff rewrites the seam/json-report row in docs/ … - file: docs/changes/json-report/design.org:16",
          "Finding 2 ... - file: tests/test_json.py:7; Finding 3 ... - file: docs/changes/json-report/design.org:16",
          "Finding 2 ... file: tests/test_json.py:7 ... Finding 3 ... file: docs/changes/json-report/design.org:16"
         ],
         "reasons": [
          "Finding 2 covers the left marker and is located at its decorator line; Finding 3 covers the seam row edit and is located in the change's design.org.",
          "The left expectedFailure marker is Finding 2, located at the decorator line, and the seam row edit is Finding 3, located in the change's design.org.",
          "The left expectedFailure marker is a finding located at the promoted test's decorator, and the seam row edit is a finding located in the change's design.org."
         ]
        },
        "RD-E3": {
         "quotes": [
          "The docstring directly above still says \"ties by name\", which is now untrue.",
          "The docstring directly above still says \"ties by name\", which is now untrue.",
          "The docstring directly above still says \"ties by name\", which is now untrue."
         ],
         "reasons": [
          "Finding 1 is of kind issue with direction sub-par-code, is located at tally/report.py:12-14 (ordered), and names the docstring that is now untrue.",
          "Finding 1 is kind issue with direction sub-par-code, is located at tally/report.py:12-14 (ordered), and names the now-untrue docstring.",
          "Finding 1 is kind issue and direction sub-par-code, located at tally/report.py:12-14 on the dropped tie-break, and it names the docstring as untrue."
         ]
        },
        "RD-E4": {
         "quotes": [
          "",
          "I found no error handling, mocking or duplicated logic in the diff.",
          ""
         ],
         "reasons": [
          "None of the six findings concerns the jsonDict name, formatting, or an equivalent restructuring.",
          "No finding concerns the jsonDict name, formatting, or an equivalent restructuring.",
          "None of the six findings, and nothing in the ruled-out section, mentions the jsonDict name or any style or formatting point."
         ]
        },
        "RD-E5": {
         "quotes": [
          "- blocks: task report-json\n- recommended_action:\n  kind: inline-fix\n  detail: Restore `key=lambda c: (-totals[c], c)` and add a tie-order test to tests/test_report.py.",
          "- blocks: task report-json\n- recommended_action:\n  kind: inline-fix\n  detail: Restore `key=lambda c: (-totals[c], c)` and add a tie-order test to tests/test_report.py.",
          "- blocks: task report-json\n- recommended_action:\n  kind: inline-fix"
         ],
         "reasons": [
          "Every finding has kind, severity, direction, a located line, evidence of the defect and who it harms, blocks, and a one-line inline-fix action; none names a route, and no ask was filed (only the review file was written).",
          "All six findings carry kind, severity, direction, a located line, evidence of the defect and its harm, blocks, and an inline-fix action with a one-line detail. None names a route, and no ask was filed.",
          "Every finding has kind, severity, direction, a line-numbered location, evidence of the defect and its harm, a blocks value, and an inline-fix action with a one-line detail; none names a route and no ask was filed."
         ]
        },
        "RD-E6": {
         "quotes": [
          "rm -rf <trial>/scratch/review-report-json && cd … && git status --short",
          "git status --porcelain ... clean / clean / clean; scratch directory ... gone",
          "rm -rf <trial>/scratch/review-report-json && cd ... && git status --short"
         ],
         "reasons": [
          "Probes ran in the scratch directory, which is gone afterwards; refs and checkout HEAD are unchanged; the checkout, code root and home are clean apart from the review file; no background runs, stash, prune or gc, and no paths outside the trial's directory.",
          "Refs, checkout, home and task file are unchanged apart from the review file. Probes ran in the named scratch directory, which was removed, and nothing ran in the background or used stash, prune or gc.",
          "The checkout, code root and home are clean, the only file added is the review file, probes ran in the scratch directory and it is gone, and no call ran in the background, stashed, pruned or wrote outside the trial directory."
         ]
        },
        "RD-E7": {
         "quotes": [
          "reviewed report-json issues=6 questions=0 spec-signal=0 thoughts=0",
          "reviewed report-json issues=6 questions=0 spec-signal=0 thoughts=0",
          "reviewed report-json issues=6 questions=0 spec-signal=0 thoughts=0"
         ],
         "reasons": [
          "The hand-back is one short line in the required form, its counts match the file, and it names no path.",
          "The hand-back is one line in the required form, its counts match the file, and it names no path.",
          "The hand-back is one line in the required form, its counts match the file and it names no path."
         ]
        }
       },
       "milestones": [
        {
         "call": 4,
         "candidates": [
          4
         ],
         "checks": [],
         "hits": {},
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 6 tests in 0.001s\nFAILED (expected failures=1, unexpected successes=1)",
         "scenarios": [],
         "sig": "test run after the handover: red"
        }
       ],
       "n_calls": 8,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Reviewer's review of one merged task, from its brief to its hand-back",
   "title": "Case reviewer-flawed-diff: a pending marker left, an edit of the change's design and a docstring made untrue are each a finding; style is not",
   "trials": 3,
   "validated": "0abfdc5fc6c46cfbd1a9565c88d9270479fd385f 20261114T120932 t1 (skill-evals)"
  },
  {
   "capability": "reviewer",
   "id": "reviewer-ns12",
   "items": [
    {
     "checks": [
      {
       "id": "RV-S1",
       "kind": "decide"
      },
      {
       "id": "RV-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The review file is at the path its brief names",
      "reviewer § The review file opens with what was reviewed and what was found",
      "record § A cycle's directory holds what its steps wrote, under fixed names"
     ],
     "id": "RV-E1",
     "question": "Is the review file at the path the brief names, opening with what was reviewed and what was found?"
    },
    {
     "checks": [
      {
       "id": "RV-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The finding count and the clean mark agree with the findings"
     ],
     "id": "RV-E2",
     "question": "Do the finding count and the clean mark agree with the findings?"
    },
    {
     "checks": [
      {
       "id": "RV-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The review file holds a summary, the numbered findings and the thoughts",
      "reviewer § A speculative need or a cleanup is one thought line"
     ],
     "id": "RV-E3",
     "question": "Does the file hold a summary, the numbered findings and, at the end, its thoughts only when it has any?"
    },
    {
     "checks": [
      {
       "id": "RV-S3",
       "kind": "decide"
      },
      {
       "id": "RV-S10",
       "kind": "decide"
      },
      {
       "id": "RV-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
      "reviewer § A recommended action is an inline fix, a rework or the user's",
      "reviewer § Every finding says what it blocks",
      "reviewer § A finding about the change's work names the direction it looks in"
     ],
     "id": "RV-E4",
     "question": "Does every finding carry a kind, a severity, a direction, a location, its evidence, what it blocks and a recommended action of the three kinds?"
    },
    {
     "checks": [
      {
       "id": "RV-S6",
       "kind": "decide"
      },
      {
       "id": "RV-S12",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § A defect the Reviewer can point at is an issue",
      "reviewer § A defect in code the diff does not change is still reported",
      "reviewer § An issue's evidence shows the defect and who it harms",
      "reviewer § Severity is the harm when the defect triggers"
     ],
     "id": "RV-E5",
     "question": "If the file names the defect in =_money=, is it a finding of kind issue with a severity and evidence, reported though the diff does not change that code?"
    },
    {
     "checks": [
      {
       "id": "RV-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § A defect in code the change did not write is sub-par code"
     ],
     "id": "RV-E6",
     "question": "Is the finding on =_money= in the sub-par-code direction?"
    },
    {
     "checks": [
      {
       "id": "RV-S14",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § A present defect is never only a thought",
      "reviewer § A defect the file names anywhere is among its findings"
     ],
     "id": "RV-E7",
     "question": "Is every defect the file names, in its summary, thoughts or what it ruled out, also among its findings?"
    },
    {
     "checks": [],
     "cites": [
      "reviewer § A suspected defect the Reviewer cannot settle is a question",
      "reviewer § A probe that shows a suspected defect makes it an issue",
      "reviewer § A probe that shows the code right leaves no finding"
     ],
     "id": "RV-E8",
     "question": "Does each finding's kind fit what the Reviewer could settle?"
    },
    {
     "checks": [],
     "cites": [
      "reviewer § Style and preference are not findings",
      "reviewer § A review with no finding says what was looked for"
     ],
     "id": "RV-E9",
     "question": "Are the findings free of style and preference, and does a review with no finding say what was looked for?"
    },
    {
     "checks": [
      {
       "id": "RV-S15",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The reporter leaves the route to the routing",
      "asks § An unsettled ask is in the change's inbox",
      "asks § A settled ask is in its cycle's asks directory"
     ],
     "id": "RV-E10",
     "question": "Does the Reviewer leave the route to the routing?"
    },
    {
     "checks": [],
     "cites": [
      "reviewer § A finding for the user says what the thing is for and what it blocks",
      "reviewer § A spec signal says what the thing is for and what it blocks",
      "reviewer § A spec signal of kind issue carries a stopgap"
     ],
     "id": "RV-E11",
     "question": "Does a finding for the user, or a spec signal, say what the thing is for and what it blocks, with a stopgap on a spec signal of kind issue?"
    },
    {
     "checks": [
      {
       "id": "RV-S7",
       "kind": "decide"
      },
      {
       "id": "RV-S17",
       "kind": "decide"
      },
      {
       "id": "RV-S18",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The Reviewer applies no fix and moves no ref",
      "reviewer § The Reviewer leaves the task file alone",
      "reviewer § A test run leaves the checkout as git sees it"
     ],
     "id": "RV-E12",
     "question": "Does the Reviewer change nothing it reviews?"
    },
    {
     "checks": [
      {
       "id": "RV-S8",
       "kind": "decide"
      },
      {
       "id": "RV-S19",
       "kind": "decide"
      },
      {
       "id": "RV-S20",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards"
     ],
     "id": "RV-E13",
     "question": "Do the probes run in the scratch directory the brief names, which is gone afterwards, leaving nothing else behind?"
    },
    {
     "checks": [
      {
       "id": "RV-S21",
       "kind": "decide"
      },
      {
       "id": "RV-S22",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § Every run ends before the hand-back",
      "reviewer § The Reviewer leaves what other agents share alone"
     ],
     "id": "RV-E14",
     "question": "Does every run end before the hand-back, leaving what other agents share alone?"
    },
    {
     "checks": [
      {
       "id": "RV-S5",
       "kind": "decide"
      },
      {
       "id": "RV-S16",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The hand-back line counts the findings",
      "reviewer § The hand-back carries no path and no summary"
     ],
     "id": "RV-E15",
     "question": "Is the hand-back one line of counts, naming no path?"
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
     "case_commit": "fc17932",
     "cites": {
      "RV-E1": [
       "reviewer § The review file is at the path its brief names",
       "reviewer § The review file opens with what was reviewed and what was found",
       "record § A cycle's directory holds what its steps wrote, under fixed names"
      ],
      "RV-E10": [
       "reviewer § The reporter leaves the route to the routing",
       "asks § An unsettled ask is in the change's inbox",
       "asks § A settled ask is in its cycle's asks directory"
      ],
      "RV-E11": [
       "reviewer § A finding for the user says what the thing is for and what it blocks",
       "reviewer § A spec signal says what the thing is for and what it blocks",
       "reviewer § A spec signal of kind issue carries a stopgap"
      ],
      "RV-E12": [
       "reviewer § The Reviewer applies no fix and moves no ref",
       "reviewer § The Reviewer leaves the task file alone",
       "reviewer § A test run leaves the checkout as git sees it"
      ],
      "RV-E13": [
       "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards"
      ],
      "RV-E14": [
       "reviewer § Every run ends before the hand-back",
       "reviewer § The Reviewer leaves what other agents share alone"
      ],
      "RV-E15": [
       "reviewer § The hand-back line counts the findings",
       "reviewer § The hand-back carries no path and no summary"
      ],
      "RV-E2": [
       "reviewer § The finding count and the clean mark agree with the findings"
      ],
      "RV-E3": [
       "reviewer § The review file holds a summary, the numbered findings and the thoughts",
       "reviewer § A speculative need or a cleanup is one thought line"
      ],
      "RV-E4": [
       "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
       "reviewer § A recommended action is an inline fix, a rework or the user's",
       "reviewer § Every finding says what it blocks",
       "reviewer § A finding about the change's work names the direction it looks in"
      ],
      "RV-E5": [
       "reviewer § A defect the Reviewer can point at is an issue",
       "reviewer § A defect in code the diff does not change is still reported",
       "reviewer § An issue's evidence shows the defect and who it harms",
       "reviewer § Severity is the harm when the defect triggers"
      ],
      "RV-E6": [
       "reviewer § A defect in code the change did not write is sub-par code"
      ],
      "RV-E7": [
       "reviewer § A present defect is never only a thought",
       "reviewer § A defect the file names anywhere is among its findings"
      ],
      "RV-E8": [
       "reviewer § A suspected defect the Reviewer cannot settle is a question",
       "reviewer § A probe that shows a suspected defect makes it an issue",
       "reviewer § A probe that shows the code right leaves no finding"
      ],
      "RV-E9": [
       "reviewer § Style and preference are not findings",
       "reviewer § A review with no finding says what was looked for"
      ]
     },
     "commit": "db6ad9d",
     "item_checks": {
      "RV-E1": [
       "RV-S1",
       "RV-S2"
      ],
      "RV-E10": [
       "RV-S15"
      ],
      "RV-E11": [],
      "RV-E12": [
       "RV-S17",
       "RV-S18",
       "RV-S7"
      ],
      "RV-E13": [
       "RV-S19",
       "RV-S20",
       "RV-S8"
      ],
      "RV-E14": [
       "RV-S21",
       "RV-S22"
      ],
      "RV-E15": [
       "RV-S16",
       "RV-S5"
      ],
      "RV-E2": [
       "RV-S4"
      ],
      "RV-E3": [
       "RV-S9"
      ],
      "RV-E4": [
       "RV-S10",
       "RV-S11",
       "RV-S3"
      ],
      "RV-E5": [
       "RV-S12",
       "RV-S6"
      ],
      "RV-E6": [
       "RV-S13"
      ],
      "RV-E7": [
       "RV-S14"
      ],
      "RV-E8": [],
      "RV-E9": []
     },
     "items_then": [],
     "run": "20261113T231729",
     "trials": [
      {
       "calls": {
        "4": {
         "error": false,
         "text": "git show 5a7e127:tally/cli.py | head -3; git show HEAD^1:tally/cli.py | head -3; ls -R <trial>/home/.orchestrator/changes/json-report | head -40; python3 -m unittest discover -s tests -t . 2>&1 | tail -5",
         "tool": "Bash"
        }
       },
       "checks": {
        "RV-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists at the named path, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the review file at the path the brief names",
         "detail": "<trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/reviews/report-json.md",
         "kind": "decide",
         "pass": true
        },
        "RV-S10": {
         "audit": {
          "agree": true,
          "reason": "The action kinds inline-fix, rework, inline-fix, inline-fix match the file.",
          "verdict": "agrees"
         },
         "desc": "every recommended action is inline-fix, rework or route-to-user",
         "detail": "#1 inline-fix; #2 rework; #3 inline-fix; #4 inline-fix",
         "kind": "decide",
         "pass": true
        },
        "RV-S11": {
         "audit": {
          "agree": true,
          "reason": "Every finding has a blocks value (done or nothing).",
          "verdict": "agrees"
         },
         "desc": "every finding says what it blocks",
         "detail": "#1 blocks='done'; #2 blocks='done'; #3 blocks='done'; #4 blocks='nothing'",
         "kind": "decide",
         "pass": true
        },
        "RV-S12": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 mentions `_money`, negative amounts and -13.95.",
          "verdict": "agrees"
         },
         "desc": "a finding names the defect in words",
         "detail": "findings naming it: [1]",
         "kind": "signal",
         "pass": true
        },
        "RV-S13": {
         "audit": {
          "agree": true,
          "reason": "Finding 1's direction is sub-par-code.",
          "verdict": "agrees"
         },
         "desc": "the finding at _money is sub-par-code",
         "detail": "#1 direction=sub-par-code",
         "kind": "decide",
         "pass": true
        },
        "RV-S14": {
         "audit": {
          "agree": true,
          "reason": "The summary mentions `_money`, and Finding 1 covers it.",
          "verdict": "agrees"
         },
         "desc": "the defect named outside the findings is among them too",
         "detail": "outside the findings: _money; in a finding: True",
         "kind": "signal",
         "pass": true
        },
        "RV-S15": {
         "audit": {
          "agree": true,
          "reason": "No ask file appears among the home's changes. The item's failure comes from the route named in a recommended action's text, which this check does not observe.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RV-S16": {
         "audit": {
          "agree": true,
          "reason": "The final text contains no slash.",
          "verdict": "agrees"
         },
         "desc": "the hand-back names no path",
         "detail": "reviewed report-json issues=4 questions=0 spec-signal=0 thoughts=1",
         "kind": "decide",
         "pass": true
        },
        "RV-S17": {
         "audit": {
          "agree": true,
          "reason": "Both porcelain outputs are clean in the evidence.",
          "verdict": "agrees"
         },
         "desc": "the checkout and the home as git sees them",
         "detail": "checkout: clean | home: clean",
         "kind": "decide",
         "pass": true
        },
        "RV-S18": {
         "audit": {
          "agree": true,
          "reason": "No call wrote the task file, and the check reports it unchanged.",
          "verdict": "agrees"
         },
         "desc": "the task file as the fixture wrote it",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "RV-S19": {
         "audit": {
          "agree": true,
          "reason": "Every path used is inside the trial directory, and no call runs mktemp.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RV-S2": {
         "audit": {
          "agree": true,
          "reason": "The front matter holds all five keys, and merge_commit equals the reviewed commit.",
          "verdict": "agrees"
         },
         "desc": "front matter with the five fields, merge_commit the reviewed commit",
         "detail": "missing []; merge_commit='515790212d3cca16fb8f0c5c6663402ecb40b2bf'; reviewed 515790212d3cca16fb8f0c5c6663402ecb40b2bf",
         "kind": "decide",
         "pass": true
        },
        "RV-S20": {
         "audit": {
          "agree": true,
          "reason": "The evidence lists no file in the home added or changed other than the review file.",
          "verdict": "agrees"
         },
         "desc": "the review file the only file added or changed in the home",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RV-S21": {
         "audit": {
          "agree": true,
          "reason": "No call ran in the background.",
          "verdict": "agrees"
         },
         "desc": "no run in the background",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RV-S22": {
         "audit": {
          "agree": true,
          "reason": "No command runs stash, worktree prune or remove, or gc.",
          "verdict": "agrees"
         },
         "desc": "no git stash, worktree prune or remove, or gc",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RV-S3": {
         "audit": {
          "agree": true,
          "reason": "Each finding's kind, severity, direction, location and evidence match what the file shows.",
          "verdict": "agrees"
         },
         "desc": "every finding: kind, severity, direction, a location with a line, evidence",
         "detail": "#1 kind=issue severity=high direction=sub-par-code location=True evidence=True; #2 kind=issue severity=medium direction=design-drift location=True evidence=True; #3 kind=issue severity=medium direction=sub-par-code location=True evidence=True; #4 kind=issue severity=low direction=sub-par-code locati … [cut, 321 characters]",
         "kind": "decide",
         "pass": true
        },
        "RV-S4": {
         "audit": {
          "agree": true,
          "reason": "The check compares the count with four headings and clean_review false, which matches the file.",
          "verdict": "agrees"
         },
         "desc": "finding_count and clean_review agree with the findings",
         "detail": "finding_count='4' clean_review='false'; 4 finding heading(s)",
         "kind": "decide",
         "pass": true
        },
        "RV-S5": {
         "audit": {
          "agree": true,
          "reason": "The hand-back line's form and counts match the file.",
          "verdict": "agrees"
         },
         "desc": "the hand-back one line of counts matching the file",
         "detail": "'reviewed report-json issues=4 questions=0 spec-signal=0 thoughts=1'; the file: issues=4 questions=0 spec-signal=0",
         "kind": "decide",
         "pass": true
        },
        "RV-S6": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 is located at tally/report.py:17, which is a line of `_money`, and it is an issue of high severity.",
          "verdict": "agrees"
         },
         "desc": "a finding located at _money is an issue with a severity",
         "detail": "_money at tally/report.py lines [17, 18]; #1 kind=issue severity=high",
         "kind": "decide",
         "pass": true
        },
        "RV-S7": {
         "audit": {
          "agree": true,
          "reason": "No ref moved, and the checkout's HEAD is the merge commit.",
          "verdict": "agrees"
         },
         "desc": "no ref moved; the checkout at the merge commit",
         "detail": "refs changed: none; checkout HEAD 515790212d3cca16fb8f0c5c6663402ecb40b2bf, merge 515790212d3cca16fb8f0c5c6663402ecb40b2bf",
         "kind": "decide",
         "pass": true
        },
        "RV-S8": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows the scratch directory is gone.",
          "verdict": "agrees"
         },
         "desc": "the scratch directory gone",
         "detail": "<trial>/scratch/review-report-json",
         "kind": "decide",
         "pass": true
        },
        "RV-S9": {
         "audit": {
          "agree": true,
          "reason": "The check computes the structure it describes correctly and explicitly leaves to the eval agent whether Thoughts is last, which is where the item fails.",
          "verdict": "agrees"
         },
         "desc": "a summary, the findings numbered from 1, thoughts after them when present",
         "detail": "sections: ['Summary', 'Findings', 'Thoughts', 'What I looked for and ruled out']; finding numbers [1, 2, 3, 4]; thoughts present: True",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RV-E1": "pass",
        "RV-E10": "fail",
        "RV-E11": "n/a",
        "RV-E12": "pass",
        "RV-E13": "pass",
        "RV-E14": "pass",
        "RV-E15": "pass",
        "RV-E2": "pass",
        "RV-E3": "fail",
        "RV-E4": "pass",
        "RV-E5": "pass",
        "RV-E6": "pass",
        "RV-E7": "pass",
        "RV-E8": "pass",
        "RV-E9": "pass"
       },
       "judge": {
        "RV-E1": {
         "quotes": [
          "task_name: report-json\nmerge_commit: 515790212d3cca16fb8f0c5c6663402ecb40b2bf\nreviewed_at: 2026-11-14T04:18:22Z\nfinding_count: 4\nclean_review: false",
          "task_name: report-json\nmerge_commit: 515790212d3cca16fb8f0c5c6663402ecb40b2bf\nreviewed_at: 2026-11-14T04:18:22Z\nfinding_count: 4\nclean_review: false",
          "task_name: report-json\nmerge_commit: 515790212d3cca16fb8f0c5c6663402ecb40b2bf\nreviewed_at: 2026-11-14T04:18:22Z\nfinding_count: 4\nclean_review: false"
         ],
         "reasons": [
          "The file is at the path the brief names, has all five front-matter keys, and its merge_commit is the commit the brief named.",
          "The file is at the brief's path, and its front matter holds all five keys with the merge commit the brief named.",
          "The file was written to the brief's path, its front matter holds all five keys, and its merge_commit is the commit the brief named."
         ]
        },
        "RV-E10": {
         "quotes": [
          "Alternatively, the orchestrator plans a task citing seam/options; the change cannot be done without one.",
          "Alternatively, the orchestrator plans a task citing seam/options; the change cannot be done without one.",
          "Alternatively, the orchestrator plans a task citing seam/options; the change cannot be done without one."
         ],
         "reasons": [
          "Finding 2's recommended action names a route (planning a follow-up task) instead of only saying what should be done were the defect fixed now.",
          "Finding 2's recommended action names a route (a follow-up task to plan) instead of only saying what the fix now would be.",
          "Finding 2's recommended action names a route, a follow-up task, rather than only saying what should be done were the defect fixed now."
         ]
        },
        "RV-E11": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "No finding goes to the user, and none is in the spec-signal direction.",
          "No finding is routed to the user and none is in the spec-signal direction.",
          "No finding is route-to-user and none has direction spec-signal."
         ]
        },
        "RV-E12": {
         "quotes": [
          "refs changed: none; checkout HEAD 515790212d3cca16fb8f0c5c6663402ecb40b2bf ... checkout: clean | home: clean ... unchanged",
          "git status --porcelain of the merge's checkout, then of the home ==\nclean\nclean",
          "== git status --porcelain of the merge's checkout, then of the home ==\nclean\nclean"
         ],
         "reasons": [
          "No ref moved, both work trees are clean, and the task file is unchanged.",
          "No call commits, checks out or writes outside the review file, both trees are clean and no other home file changed.",
          "The checkout and the home are both clean. Refs are unchanged and the task file is unchanged, per the collected state."
         ]
        },
        "RV-E13": {
         "quotes": [
          "rm -rf $S; ls $S 2>&1 ... ls: ... review-report-json: No such file or directory",
          "mkdir -p $S && cd $S ... rm -rf $S; ls $S 2>&1 -> No such file or directory",
          "S=<trial>/scratch/review-report-json; mkdir -p $S && cd $S ... rm -rf $S; ls $S 2>&1"
         ],
         "reasons": [
          "Probe ledgers were written in the brief's scratch directory, and the CLI was run against the checkout. The directory was removed before the hand-back, nothing was written elsewhere under a temp directory, and the review file is the only change in the home.",
          "Probe ledgers were written in the brief's scratch directory and run against the checkout, the directory was removed, and no other temp path was used.",
          "The probes ran against the checkout from the brief's scratch directory, which is gone at the end. Nothing else in the home changed apart from the review file."
         ]
        },
        "RV-E14": {
         "quotes": [
          "reviewed report-json issues=4 questions=0 spec-signal=0 thoughts=1",
          "reviewed report-json issues=4 questions=0 spec-signal=0 thoughts=1",
          "Ran 5 tests in 0.000s\n\nOK"
         ],
         "reasons": [
          "Every run was in the foreground and finished before the single hand-back. No stash, worktree pruning, server or process kill appears.",
          "All runs were foreground and finished, there was no stash, prune or worktree removal, and one hand-back was given.",
          "All runs were foreground Bash calls that finished before the single hand-back, with no stash, prune or worktree removal."
         ]
        },
        "RV-E15": {
         "quotes": [
          "reviewed report-json issues=4 questions=0 spec-signal=0 thoughts=1",
          "reviewed report-json issues=4 questions=0 spec-signal=0 thoughts=1",
          "reviewed report-json issues=4 questions=0 spec-signal=0 thoughts=1"
         ],
         "reasons": [
          "The hand-back is one line in the required form, names no path, and its counts match the file (four issues, no questions, no spec signals, one thought).",
          "The hand-back is one line in the required form, its counts match the file's four issues, zero questions, zero spec-signal and one thought, and it names no path.",
          "The hand-back is one line of counts that match the file, and it names no path."
         ]
        },
        "RV-E2": {
         "quotes": [
          "finding_count: 4\nclean_review: false",
          "finding_count: 4\nclean_review: false",
          "finding_count: 4\nclean_review: false"
         ],
         "reasons": [
          "There are four `### Finding` headings, and clean_review is false, as it should be when there are findings.",
          "There are four `### Finding` headings, and `clean_review` is false, as it should be with findings present.",
          "There are four findings, Finding 1 to Finding 4, and clean_review is false, as it should be when there is a finding."
         ]
        },
        "RV-E3": {
         "quotes": [
          "## Thoughts ... ## What I looked for and ruled out",
          "## Thoughts ... ## What I looked for and ruled out",
          "## Thoughts\n\n- thought: JSON objects are unordered by RFC 8259. ... \n\n## What I looked for and ruled out"
         ],
         "reasons": [
          "The `## Thoughts` section is not at the end: a `## What I looked for and ruled out` section follows it. The Summary is also a paragraph plus a numbered list, not one paragraph.",
          "The Thoughts section is not at the end because a further section follows it, and the Summary is a paragraph followed by a numbered list rather than one paragraph.",
          "The Thoughts section is not at the end because a further section follows it, and the Summary is a paragraph plus a numbered list rather than one paragraph."
         ]
        },
        "RV-E4": {
         "quotes": [
          "- kind: issue\n- severity: high\n- direction: sub-par-code\n- locations:\n  - file: tally/report.py:17 ... - blocks: done\n- recommended_action:\n  kind: inline-fix",
          "- kind: issue\n- severity: high\n- direction: sub-par-code\n- locations:\n  - file: tally/report.py:17 ... - blocks: done\n- recommended_action:\n  kind: inline-fix",
          "- kind: issue\n- severity: high\n- direction: sub-par-code\n- locations:\n  - file: tally/report.py:17 ... - blocks: done\n- recommended_action:\n  kind: inline-fix"
         ],
         "reasons": [
          "All four findings carry every required field: a valid kind, severity and direction, a file:line location, evidence, a blocks value, and an action kind of inline-fix or rework with a detail line.",
          "Each of the four findings has a valid kind, severity, direction, file:line location, evidence, a blocks value (done or nothing) and a recommended action of an allowed kind with its detail.",
          "All four findings carry these fields:\n- an allowed kind, severity and direction\n- a file:line location\n- evidence\n- a blocks value (done or nothing)\n- a recommended action of an allowed kind, with one line of detail."
         ]
        },
        "RV-E5": {
         "quotes": [
          "Probe at merge_commit: `[_money(c) for c in (-1205, -50, -100, -1)]` returns `['-13.95', '-1.50', '-1.00', '-1.99']` ... any script reading a refund now gets a wrong number that looks valid.",
          "returns `['-13.95', '-1.50', '-1.00', '-1.99']` ... prints `food -13.95` as the table and `{\"food\": \"-13.95\"}` with `--json` ... any script reading a refund now gets a wrong number that looks valid",
          "a ledger whose only line is `2026-10-01,food,-12.05` prints `food -13.95` as the table and `{\"food\": \"-13.95\"}` with `--json` ... any script reading a refund now gets a wrong number that looks valid."
         ],
         "reasons": [
          "The `_money` defect is reported as an issue of high severity even though the diff does not change that code. Its evidence includes probe output and says who is harmed (refunds shown wrong in the table and in the JSON).",
          "The defect is an issue of severity high, with probe output and the harm to readers of the table and to scripts reading the JSON, reported even though the diff does not change `_money`.",
          "The _money defect is reported as an issue with severity high. The evidence includes probe and CLI output and names the harm, a refund shown wrong in both the table and the JSON report, even though the diff does not change _money."
         ]
        },
        "RV-E6": {
         "quotes": [
          "### Finding 1: `_money` renders negative amounts wrong ... - direction: sub-par-code",
          "### Finding 1 ... direction: sub-par-code",
          "### Finding 1: `_money` renders negative amounts wrong ... - direction: sub-par-code"
         ],
         "reasons": [
          "The finding on `_money` is in the sub-par-code direction.",
          "The `_money` finding is in the sub-par-code direction.",
          "The finding on _money has direction sub-par-code."
         ]
        },
        "RV-E7": {
         "quotes": [
          "1. `_money` renders every negative amount wrong, and the JSON report now passes those wrong amounts to scripts.",
          "The gaps found are implementation and planning gaps (Findings 1 and 2)",
          "A non-ASCII category is escaped (`\"caf\\u00e9\"`); that is valid JSON and parses back to the same key, so it is not a defect."
         ],
         "reasons": [
          "Every defect the summary names is one of the four findings. The thought (RFC 8259 ordering) and the ruled-out notes name no defect present at the merge commit.",
          "Every defect named in the summary is one of the findings; the thought (unordered JSON maps for hypothetical consumers) and the ruled-out items are not present defects.",
          "Every defect the summary names is among the findings. What the file ruled out and its one thought are not defects present at the merge commit."
         ]
        },
        "RV-E8": {
         "quotes": [
          "Probe at merge_commit: `tally FILE --yaml` prints the table and exits 0",
          "Probe at merge_commit: `tally FILE --yaml` prints the table and exits 0",
          "Probe at merge_commit: `tally FILE --yaml` prints the table and exits 0"
         ],
         "reasons": [
          "Every finding is an issue the Reviewer settled, by a probe or by reading code and tests, with that output or reading in its evidence. No probe that showed correct behaviour was turned into a finding.",
          "Findings 1, 2 and 4 carry probe output, Finding 3 is a test gap visible from the test's text, and nothing the probes showed right (key order, empty ledger, non-ASCII) is made a finding.",
          "Each issue rests on probe output or plain reading of the code. No finding is something a probe showed to be right."
         ]
        },
        "RV-E9": {
         "quotes": [
          "A script that asks for JSON with a mistyped flag therefore gets table text with a success status.",
          "A non-ASCII category is escaped ... so it is not a defect.",
          "### Finding 4: A misplaced flag or a missing argument ends in an uncaught traceback"
         ],
         "reasons": [
          "Every finding is a behavioural or coverage defect, not a matter of naming, formatting or a settled decision.",
          "No finding concerns naming, formatting or a settled decision; each concerns behaviour or test coverage.",
          "All findings concern behaviour (wrong amounts, silent acceptance of unknown options, an untested CLI path, tracebacks), not style or a settled decision."
         ]
        }
       },
       "milestones": [
        {
         "call": 4,
         "candidates": [
          4
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 5 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 8,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Reviewer's review of one merged task, from its brief to its hand-back",
   "title": "Case reviewer-ns12: the Reviewer reports a defect beside the diff as a finding",
   "trials": 3,
   "validated": "fc1793274ee636b5cf406edb5848677b34dd56cd 20261113T231729 t1 (skill-evals)"
  },
  {
   "capability": "reviewer",
   "id": "reviewer-scope-signal",
   "items": [
    {
     "checks": [
      {
       "id": "RS-S1",
       "kind": "decide"
      },
      {
       "id": "RS-S2",
       "kind": "decide"
      },
      {
       "id": "RS-S3",
       "kind": "decide"
      },
      {
       "id": "RS-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The review file is at the path its brief names",
      "reviewer § The review file opens with what was reviewed and what was found",
      "reviewer § The finding count and the clean mark agree with the findings",
      "reviewer § The review file holds a summary, the numbered findings and the thoughts",
      "record § A cycle's directory holds what its steps wrote, under fixed names"
     ],
     "id": "RS-E1",
     "question": "Is the review file at the path the brief names, opening with what was reviewed and what was found?"
    },
    {
     "checks": [
      {
       "id": "RS-S8",
       "kind": "decide"
      },
      {
       "id": "RS-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § A document the diff makes untrue is a finding",
      "reviewer § A defect the Reviewer can point at is an issue"
     ],
     "id": "RS-E2",
     "question": "Is the example the diff made untrue a finding of kind issue, located at the scenario?"
    },
    {
     "checks": [
      {
       "id": "RS-S10",
       "kind": "signal"
      },
      {
       "id": "RS-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § A wrong task scope is a spec signal, not a change of scope",
      "reviewer § A spec signal says what the thing is for and what it blocks",
      "reviewer § A spec signal of kind issue carries a stopgap",
      "reviewer § A finding about the change's work names the direction it looks in"
     ],
     "id": "RS-E3",
     "question": "Is the wrong scope a spec signal, not a change of scope, saying what the rule is for and what it blocks, with a stopgap?"
    },
    {
     "checks": [],
     "cites": [
      "reviewer § A finding for the user says what the thing is for and what it blocks"
     ],
     "id": "RS-E4",
     "question": "Does a finding for the user say what the thing is for and what it blocks?"
    },
    {
     "checks": [
      {
       "id": "RS-S5",
       "kind": "decide"
      },
      {
       "id": "RS-S6",
       "kind": "decide"
      },
      {
       "id": "RS-S7",
       "kind": "decide"
      },
      {
       "id": "RS-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
      "reviewer § A recommended action is an inline fix, a rework or the user's",
      "reviewer § Every finding says what it blocks",
      "reviewer § The reporter leaves the route to the routing"
     ],
     "id": "RS-E5",
     "question": "Does every finding carry its labels, what it blocks and a recommended action of the three kinds, leaving the route to the routing?"
    },
    {
     "checks": [
      {
       "id": "RS-S12",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § A defect the file names anywhere is among its findings",
      "reviewer § Severity is the harm when the defect triggers",
      "reviewer § Style and preference are not findings"
     ],
     "id": "RS-E6",
     "question": "Is every defect the review names a finding, the refund's among them if named, and nothing of style?"
    },
    {
     "checks": [
      {
       "id": "RS-S14",
       "kind": "decide"
      },
      {
       "id": "RS-S15",
       "kind": "decide"
      },
      {
       "id": "RS-S16",
       "kind": "decide"
      },
      {
       "id": "RS-S17",
       "kind": "decide"
      },
      {
       "id": "RS-S18",
       "kind": "decide"
      },
      {
       "id": "RS-S19",
       "kind": "decide"
      },
      {
       "id": "RS-S20",
       "kind": "decide"
      },
      {
       "id": "RS-S21",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The Reviewer applies no fix and moves no ref",
      "reviewer § The Reviewer leaves the task file alone",
      "reviewer § A test run leaves the checkout as git sees it",
      "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
      "reviewer § Every run ends before the hand-back",
      "reviewer § The Reviewer leaves what other agents share alone"
     ],
     "id": "RS-E7",
     "question": "Does the Reviewer change nothing it reviews, probe in its scratch directory and leave what others share alone?"
    },
    {
     "checks": [
      {
       "id": "RS-S22",
       "kind": "decide"
      },
      {
       "id": "RS-S23",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The hand-back line counts the findings",
      "reviewer § The hand-back carries no path and no summary"
     ],
     "id": "RS-E8",
     "question": "Is the hand-back one line of counts, its spec-signal count the file's, naming no path?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "RS-E1": [
       "reviewer § The review file is at the path its brief names",
       "reviewer § The review file opens with what was reviewed and what was found",
       "reviewer § The finding count and the clean mark agree with the findings",
       "reviewer § The review file holds a summary, the numbered findings and the thoughts",
       "record § A cycle's directory holds what its steps wrote, under fixed names"
      ],
      "RS-E2": [
       "reviewer § A document the diff makes untrue is a finding",
       "reviewer § A defect the Reviewer can point at is an issue"
      ],
      "RS-E3": [
       "reviewer § A wrong task scope is a spec signal, not a change of scope",
       "reviewer § A spec signal says what the thing is for and what it blocks",
       "reviewer § A spec signal of kind issue carries a stopgap",
       "reviewer § A finding about the change's work names the direction it looks in"
      ],
      "RS-E4": [
       "reviewer § A finding for the user says what the thing is for and what it blocks"
      ],
      "RS-E5": [
       "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
       "reviewer § A recommended action is an inline fix, a rework or the user's",
       "reviewer § Every finding says what it blocks",
       "reviewer § The reporter leaves the route to the routing"
      ],
      "RS-E6": [
       "reviewer § A defect the file names anywhere is among its findings",
       "reviewer § Severity is the harm when the defect triggers",
       "reviewer § Style and preference are not findings"
      ],
      "RS-E7": [
       "reviewer § The Reviewer applies no fix and moves no ref",
       "reviewer § The Reviewer leaves the task file alone",
       "reviewer § A test run leaves the checkout as git sees it",
       "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
       "reviewer § Every run ends before the hand-back",
       "reviewer § The Reviewer leaves what other agents share alone"
      ],
      "RS-E8": [
       "reviewer § The hand-back line counts the findings",
       "reviewer § The hand-back carries no path and no summary"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RS-E1": [
       "RS-S1",
       "RS-S2",
       "RS-S3",
       "RS-S4"
      ],
      "RS-E2": [
       "RS-S8",
       "RS-S9"
      ],
      "RS-E3": [
       "RS-S10",
       "RS-S11"
      ],
      "RS-E4": [],
      "RS-E5": [
       "RS-S13",
       "RS-S5",
       "RS-S6",
       "RS-S7"
      ],
      "RS-E6": [
       "RS-S12"
      ],
      "RS-E7": [
       "RS-S14",
       "RS-S15",
       "RS-S16",
       "RS-S17",
       "RS-S18",
       "RS-S19",
       "RS-S20",
       "RS-S21"
      ],
      "RS-E8": [
       "RS-S22",
       "RS-S23"
      ]
     },
     "items_then": [],
     "run": "20261114T102112",
     "trials": [
      {
       "calls": {
        "4": {
         "error": false,
         "text": "S=<trial>/scratch/review-dollar-table && mkdir -p $S && python3 -m unittest discover -s tests -t . 2>&1 | tail -4; cat tally/ledger.py; printf '2026-01-01 food -12.05\\n2026-01-02 rent 900.00\\n2026-01-03 misc 0.05\\n' > $S/l.txt; head -3 tests/test_ledger.py; PYTHONDONTWRITEBYTECODE=1 python3 -c \"\nimport sys; sys.argv=['x']\nfrom tally import report, cli\nprint(repr(report._money(-1205)), repr(report._money(-5)), repr(report._money(5)))\ncli.main(['$S/l.txt']); cli.main(['$S/l.txt','--json'])\n\"; git status --short",
         "tool": "Bash"
        }
       },
       "checks": {
        "RS-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists at the brief's path, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the review file at the path the brief names",
         "detail": "<trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/reviews/dollar-table.md",
         "kind": "decide",
         "pass": true
        },
        "RS-S10": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 has direction: spec-signal.",
          "verdict": "agrees"
         },
         "desc": "a finding in the spec-signal direction",
         "detail": "#1",
         "kind": "signal",
         "pass": true
        },
        "RS-S11": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 carries a stopgap line (none, with a reason).",
          "verdict": "agrees"
         },
         "desc": "each spec signal of kind issue names a stopgap (not applicable without one)",
         "detail": "#1 stopgap named: True",
         "kind": "signal",
         "pass": true
        },
        "RS-S12": {
         "audit": {
          "agree": true,
          "reason": "Findings mention the refund and the negative amounts, Finding 2 most directly.",
          "verdict": "agrees"
         },
         "desc": "a finding names the refund's amount",
         "detail": "#1 #2 #3",
         "kind": "signal",
         "pass": true
        },
        "RS-S13": {
         "audit": {
          "agree": true,
          "reason": "No ask file appears among the home's changes.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RS-S14": {
         "audit": {
          "agree": true,
          "reason": "No call moves a ref, and the checkout HEAD is the reviewed commit.",
          "verdict": "agrees"
         },
         "desc": "no ref moved; the checkout at the reviewed commit",
         "detail": "refs unchanged: True; checkout HEAD 3c51fbbf3495f3b94ba1bc5606309ba7372527b5, reviewed 3c51fbbf3495f3b94ba1bc5606309ba7372527b5",
         "kind": "decide",
         "pass": true
        },
        "RS-S15": {
         "audit": {
          "agree": true,
          "reason": "The porcelain output shows a clean checkout and code root, and only the review file's line in the home.",
          "verdict": "agrees"
         },
         "desc": "the checkout, the code root and the home as git sees them: as after the build (the checkout clean)",
         "detail": "checkout: clean | code root: clean | home: clean",
         "kind": "decide",
         "pass": true
        },
        "RS-S16": {
         "audit": {
          "agree": true,
          "reason": "No call wrote to the task file, and the home shows no other change.",
          "verdict": "agrees"
         },
         "desc": "the task file as the fixture wrote it",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "RS-S17": {
         "audit": {
          "agree": true,
          "reason": "The evidence reports the scratch directory as gone.",
          "verdict": "agrees"
         },
         "desc": "the scratch directory gone",
         "detail": "<trial>/scratch/review-dollar-table",
         "kind": "decide",
         "pass": true
        },
        "RS-S18": {
         "audit": {
          "agree": true,
          "reason": "Every path in the calls lies under the trial directory, and none uses mktemp.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RS-S19": {
         "audit": {
          "agree": true,
          "reason": "Other changed files in the home: none, beyond the review file.",
          "verdict": "agrees"
         },
         "desc": "the review file the only file added or changed in the home",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RS-S2": {
         "audit": {
          "agree": true,
          "reason": "The front matter has the five keys and merge_commit equals the reviewed commit.",
          "verdict": "agrees"
         },
         "desc": "front matter with the five fields, merge_commit the reviewed commit",
         "detail": "missing []; merge_commit='3c51fbbf3495f3b94ba1bc5606309ba7372527b5'; reviewed 3c51fbbf3495f3b94ba1bc5606309ba7372527b5",
         "kind": "decide",
         "pass": true
        },
        "RS-S20": {
         "audit": {
          "agree": true,
          "reason": "No call uses run_in_background or a trailing &.",
          "verdict": "agrees"
         },
         "desc": "no run in the background",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RS-S21": {
         "audit": {
          "agree": true,
          "reason": "No command runs stash, worktree prune/remove or gc.",
          "verdict": "agrees"
         },
         "desc": "no git stash, worktree prune or remove, or gc",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RS-S22": {
         "audit": {
          "agree": true,
          "reason": "The final line has the required form, and its counts match the file.",
          "verdict": "agrees"
         },
         "desc": "the hand-back one line of counts matching the file",
         "detail": "'reviewed dollar-table issues=3 questions=0 spec-signal=1 thoughts=1'; the file: issues=3 questions=0 spec-signal=1",
         "kind": "decide",
         "pass": true
        },
        "RS-S23": {
         "audit": {
          "agree": true,
          "reason": "The final text contains no slash.",
          "verdict": "agrees"
         },
         "desc": "the hand-back names no path",
         "detail": "reviewed dollar-table issues=3 questions=0 spec-signal=1 thoughts=1",
         "kind": "decide",
         "pass": true
        },
        "RS-S3": {
         "audit": {
          "agree": true,
          "reason": "Three finding headings match finding_count 3, and clean_review is false.",
          "verdict": "agrees"
         },
         "desc": "finding_count and clean_review agree with the findings",
         "detail": "finding_count='3' clean_review='false'; 3 finding heading(s)",
         "kind": "decide",
         "pass": true
        },
        "RS-S4": {
         "audit": {
          "agree": true,
          "reason": "There is a non-empty Summary, findings numbered 1–3, and Thoughts after Findings with a thought line; the check doesn't test 'at the end', but the item's verdict doesn't turn on that here.",
          "verdict": "agrees"
         },
         "desc": "a summary, the findings numbered from 1, thoughts after them when present",
         "detail": "sections: ['Summary', 'Findings', 'Thoughts', 'What I looked for and ruled out']; finding numbers [1, 2, 3]; thoughts present: True",
         "kind": "decide",
         "pass": true
        },
        "RS-S5": {
         "audit": {
          "agree": true,
          "reason": "Every finding carries a valid kind, severity, direction, a path:line location and evidence.",
          "verdict": "agrees"
         },
         "desc": "every finding: kind, severity, direction, a location with a line, evidence",
         "detail": "#1 kind=issue severity=high direction=spec-signal location=True evidence=True; #2 kind=issue severity=high direction=sub-par-code location=True evidence=True; #3 kind=issue severity=low direction=design-drift location=True evidence=True",
         "kind": "decide",
         "pass": true
        },
        "RS-S6": {
         "audit": {
          "agree": true,
          "reason": "The actions are route-to-user, inline-fix and inline-fix, all allowed kinds.",
          "verdict": "agrees"
         },
         "desc": "every recommended action is inline-fix, rework or route-to-user",
         "detail": "#1 route-to-user; #2 inline-fix; #3 inline-fix",
         "kind": "decide",
         "pass": true
        },
        "RS-S7": {
         "audit": {
          "agree": true,
          "reason": "Each finding has blocks: done.",
          "verdict": "agrees"
         },
         "desc": "every finding says what it blocks",
         "detail": "#1 blocks='done'; #2 blocks='done'; #3 blocks='done'",
         "kind": "decide",
         "pass": true
        },
        "RS-S8": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 is located at reporting.org:24, inside the JSON scenario's lines.",
          "verdict": "agrees"
         },
         "desc": "a finding located at the JSON scenario in the reporting spec",
         "detail": "docs/specs/reporting.org lines [22, 23, 24, 25, 26]; findings there: #1",
         "kind": "decide",
         "pass": true
        },
        "RS-S9": {
         "audit": {
          "agree": true,
          "reason": "The finding at the scenario is of kind issue.",
          "verdict": "agrees"
         },
         "desc": "a finding located there is of kind issue (not applicable without one)",
         "detail": "#1 kind=issue",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RS-E1": "pass",
        "RS-E2": "pass",
        "RS-E3": "pass",
        "RS-E4": "pass",
        "RS-E5": "pass",
        "RS-E6": "pass",
        "RS-E7": "pass",
        "RS-E8": "pass"
       },
       "judge": {
        "RS-E1": {
         "quotes": [
          "finding_count: 3\nclean_review: false",
          "task_name: dollar-table\nmerge_commit: 3c51fbbf3495f3b94ba1bc5606309ba7372527b5\nreviewed_at: 2026-11-14T10:40:00Z\nfinding_count: 3\nclean_review: false",
          "task_name: dollar-table\nmerge_commit: 3c51fbbf3495f3b94ba1bc5606309ba7372527b5\nreviewed_at: 2026-11-14T10:40:00Z\nfinding_count: 3\nclean_review: false"
         ],
         "reasons": [
          "The file is at the named path with all five front-matter keys and merge_commit 3c51fbb…, a one-paragraph Summary, findings numbered 1–3 matching the count, and a Thoughts section holding one thought; a 'What I looked for and ruled out' section follows Thoughts, and the role text that might define it is cut from the evidence.",
          "The file is at the named path and has the five keys, with merge_commit equal to the reviewed commit and counts that match three findings; it has a one-paragraph Summary, findings numbered 1–3 and a Thoughts section holding a thought after the findings, though a 'What I looked for and ruled out' section follows Thoughts (the role text that might prescribe that section is cut).",
          "The file is at the named path; its five front-matter keys agree with three findings and the reviewed commit; it has a one-paragraph Summary, Findings numbered 1–3, and a Thoughts section with a thought after the findings (a 'What I looked for and ruled out' section follows it)."
         ]
        },
        "RS-E2": {
         "quotes": [
          "file: docs/specs/reporting.org:24 … gives the object literally as `{\"rent\": \"900.00\", \"food\": \"13.05\"}` … Probe: `tally ledger --json` prints `{\"rent\": \"$900.00\", …}`",
          "- kind: issue ... - file: docs/specs/reporting.org:24 ... that scenario's THEN line (reporting.org:24) gives the object literally as `{\"rent\": \"900.00\", \"food\": \"13.05\"}`",
          "- file: docs/specs/reporting.org:24 ... Probe: `tally ledger --json` prints `{\"rent\": \"$900.00\", ...}` ... that scenario's THEN line (reporting.org:24) gives the object literally as `{\"rent\": \"900.00\", \"food\": \"13.05\"}`"
         ],
         "reasons": [
          "Finding 1 is of kind issue, is located at the JSON scenario's THEN line, and sets the example's amounts against the probe's actual JSON output.",
          "Finding 1 is of kind issue, is located at the JSON scenario's line, and its evidence sets the example's bare amounts against the probed output `{\"rent\": \"$900.00\", ...}`.",
          "Finding 1 is of kind issue, is located at the JSON scenario's THEN line, and sets the example's bare amounts against the `$` amounts the report now prints."
         ]
        },
        "RS-E3": {
         "quotes": [
          "about: The `--json` report exists so that scripts can read tally's per-category totals without parsing the table … stopgap: \"none: both answers produce a well-formed, deliberate output …\"",
          "direction: spec-signal ... about: The `--json` report exists so that scripts can read tally's per-category totals without parsing the table ... stopgap: \"none: both answers produce a well-formed, deliberate output ... so no error or refusal can stand in for the choice.",
          "direction: spec-signal ... The diff adds `$` in `_money` (report.py:17), not in `format_table`, which is the owning symbol of seam/table-amount ... stopgap: \"none: both answers produce a well-formed, deliberate output ... so no error or refusal can stand in for the choice.\""
         ],
         "reasons": [
          "Finding 1 is a spec-signal on the table-only scope against the shared `_money` changing the JSON. It says the JSON is for scripts, notes that consumers must now strip `$`, and has blocks: done. It gives a stopgap of none with a reason, and the Reviewer changed nothing.",
          "Finding 1 is a spec-signal that sets the table-only seam/G2 against what the shared `_money` and D1 do to the JSON report; it states the JSON report's purpose in product terms, has blocks: done and a reasoned 'none' stopgap, and routes the decision to the user without changing scope.",
          "Finding 1 is a spec-signal on the table-only scope against what the shared renderer does to the JSON report; it states that the JSON is for scripts reading the totals and that it blocks done; it gives a stopgap of none with a reason; and it routes the choice to the user without changing the scope."
         ]
        },
        "RS-E4": {
         "quotes": [
          "about: The `--json` report exists so that scripts can read tally's per-category totals without parsing the table; the question is whether its amount strings follow the table's human `$` format or stay bare decimal numbers.",
          "about: The `--json` report exists so that scripts can read tally's per-category totals without parsing the table; ... blocks: done",
          "about: The `--json` report exists so that scripts can read tally's per-category totals without parsing the table; the question is whether its amount strings follow the table's human `$` format or stay bare decimal numbers."
         ],
         "reasons": [
          "The only route-to-user finding states in product terms what the JSON report is for and what is at stake for its users, with blocks: done.",
          "The only route-to-user finding says in one product-terms sentence what the JSON report is for, and states what it blocks.",
          "The only route-to-user finding says in product terms what the JSON report is for, and it carries what it blocks (`blocks: done`, plus consumers having to strip `$`)."
         ]
        },
        "RS-E5": {
         "quotes": [
          "- kind: issue\n- severity: high\n- direction: sub-par-code … - blocks: done\n- recommended_action:\n  kind: inline-fix",
          "#1 route-to-user; #2 inline-fix; #3 inline-fix",
          "#1 route-to-user; #2 inline-fix; #3 inline-fix ... blocks: done"
         ],
         "reasons": [
          "Each of the three findings has a kind, severity, direction, a path:line location, evidence, blocks, and a one-line action of an allowed kind; none names a route and no ask was filed.",
          "Each finding has a kind, severity, direction, a path:line location, evidence, blocks and one of the three action kinds with a one-line detail; none names a route, and the only Write is the review file, so no ask was filed.",
          "Each finding has kind, severity, direction, line locations, evidence, blocks and a recommended action of the three kinds with one line of detail; none names a route, and no ask was filed."
         ]
        },
        "RS-E6": {
         "quotes": [
          "### Finding 2: `_money` renders every negative amount wrong (a refund of -12.05 prints as `$-13.95`) … severity: high",
          "- thought: `test_table_order_and_amounts` compares `l.split()`, so it never checks the right-aligned amount column. A `$` that pushes an amount past 10 characters (≥ $1,000,000.00) would shift the column silently.",
          "### Finding 2: `_money` renders every negative amount wrong (a refund of -12.05 prints as `$-13.95`) ... severity: high"
         ],
         "reasons": [
          "The refund defect the probe showed is Finding 2, at a severity fitting a wrong amount in both outputs; the stale spec literals are Finding 3, and the only non-finding remark is a test-coverage thought, not a style preference.",
          "The refund rendering is properly Finding 2 at high severity, but the file names a further defect in existing code only under Thoughts: the `$` lowers the overflow threshold of the 10-wide column, and an untested misalignment triggers at real inputs. That should have been a low-severity finding.",
          "The refund defect the review names is a finding with severity set by its harm; the other defects named (the JSON leak and the stale literals) are findings; the one thought is a test-coverage observation about a spec-unrequired alignment, not a named defect; no finding is a matter of taste."
         ]
        },
        "RS-E7": {
         "quotes": [
          "rm -rf <trial>/scratch/review-dollar-table && cd … && git status --short",
          "checkout: clean | code root: clean | home: clean ... scratch/review-dollar-table: gone",
          "rm -rf <trial>/scratch/review-dollar-table && cd ... && git status --short"
         ],
         "reasons": [
          "Probes ran in the scratch directory, which is gone. The checkout, code root and home are clean apart from the review file, and no ref moved. No call ran in the background, used git stash or gc, or touched paths outside the trial.",
          "Refs are unchanged, the checkout is clean at the reviewed commit, the review file is the only change in the home, probes ran in the scratch directory, which was removed, and nothing ran in the background or used stash, prune or gc.",
          "Probes ran in the scratch directory, which is gone; the checkout, code root and home are clean apart from the review file; no other files changed; and no background run, stash, prune or gc appears."
         ]
        },
        "RS-E8": {
         "quotes": [
          "reviewed dollar-table issues=3 questions=0 spec-signal=1 thoughts=1",
          "reviewed dollar-table issues=3 questions=0 spec-signal=1 thoughts=1",
          "reviewed dollar-table issues=3 questions=0 spec-signal=1 thoughts=1"
         ],
         "reasons": [
          "The hand-back is one line of the required form, its counts match the file (one spec-signal finding), and it names no path.",
          "The hand-back is one line of the required form with counts matching the file (one spec-signal finding) and no path.",
          "The hand-back is one line of the required form; its counts match the file (3 issues, 1 spec-signal, 1 thought) and it names no path."
         ]
        }
       },
       "milestones": [
        {
         "call": 4,
         "candidates": [
          4
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "result": "Ran 6 tests in 0.002s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 7,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the Reviewer's review of one merged task, from its brief to its hand-back",
   "title": "Case reviewer-scope-signal: a task whose stated scope is wrong is a spec signal, and the example it made untrue a finding",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T102112 t1 (skill-evals)"
  },
  {
   "capability": "reviewer",
   "id": "reviewer-whole-change-tasks",
   "items": [
    {
     "checks": [
      {
       "id": "RW-S1",
       "kind": "decide"
      },
      {
       "id": "RW-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The whole-change review is one review file of its cycle",
      "reviewer § The whole-change review names the whole change in place of a task",
      "reviewer § The review file opens with what was reviewed and what was found",
      "record § A cycle's directory holds what its steps wrote, under fixed names",
      "integrate § The whole-change review is kept in the cycle's directory"
     ],
     "id": "RW-E1",
     "question": "Is the whole-change review one review file of the cycle, naming the whole change and the HEAD reviewed?"
    },
    {
     "checks": [
      {
       "id": "RW-S3",
       "kind": "decide"
      },
      {
       "id": "RW-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The finding count and the clean mark agree with the findings",
      "reviewer § The review file holds a summary, the numbered findings and the thoughts"
     ],
     "id": "RW-E2",
     "question": "Does the file hold the count, the clean mark, a summary, the numbered findings and the thoughts as a task's does?"
    },
    {
     "checks": [
      {
       "id": "RW-S8",
       "kind": "decide"
      },
      {
       "id": "RW-S9",
       "kind": "decide"
      },
      {
       "id": "RW-S10",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § The whole-change review reports what no task's review could see",
      "reviewer § A document the diff makes untrue is a finding"
     ],
     "id": "RW-E3",
     "question": "Does the review report what no task's review could see: the example the tasks together made untrue?"
    },
    {
     "checks": [
      {
       "id": "RW-S5",
       "kind": "decide"
      },
      {
       "id": "RW-S6",
       "kind": "decide"
      },
      {
       "id": "RW-S7",
       "kind": "decide"
      },
      {
       "id": "RW-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
      "reviewer § A recommended action is an inline fix, a rework or the user's",
      "reviewer § Every finding says what it blocks",
      "reviewer § A finding about the change's work names the direction it looks in",
      "reviewer § The reporter leaves the route to the routing"
     ],
     "id": "RW-E4",
     "question": "Does every finding carry its labels, its direction, what it blocks and a recommended action of the three kinds, with no route?"
    },
    {
     "checks": [
      {
       "id": "RW-S22",
       "kind": "signal"
      }
     ],
     "cites": [
      "reviewer § A defect the Reviewer can point at is an issue",
      "reviewer § A defect the file names anywhere is among its findings",
      "reviewer § Style and preference are not findings",
      "reviewer § Severity is the harm when the defect triggers",
      "reviewer § An issue's evidence shows the defect and who it harms"
     ],
     "id": "RW-E5",
     "question": "Is every defect the review finds a finding, whatever its size or place, and nothing of style?"
    },
    {
     "checks": [
      {
       "id": "RW-S12",
       "kind": "decide"
      },
      {
       "id": "RW-S13",
       "kind": "decide"
      },
      {
       "id": "RW-S14",
       "kind": "decide"
      },
      {
       "id": "RW-S15",
       "kind": "decide"
      },
      {
       "id": "RW-S16",
       "kind": "decide"
      },
      {
       "id": "RW-S17",
       "kind": "decide"
      },
      {
       "id": "RW-S18",
       "kind": "decide"
      },
      {
       "id": "RW-S19",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The Reviewer applies no fix and moves no ref",
      "reviewer § The Reviewer leaves the task file alone",
      "reviewer § A test run leaves the checkout as git sees it",
      "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
      "reviewer § Every run ends before the hand-back",
      "reviewer § The Reviewer leaves what other agents share alone"
     ],
     "id": "RW-E6",
     "question": "Does the Reviewer change nothing it reviews, probe in its scratch directory and leave what others share alone?"
    },
    {
     "checks": [
      {
       "id": "RW-S20",
       "kind": "decide"
      },
      {
       "id": "RW-S21",
       "kind": "decide"
      }
     ],
     "cites": [
      "reviewer § The whole-change review names the whole change in place of a task",
      "reviewer § The hand-back line counts the findings",
      "reviewer § The hand-back carries no path and no summary"
     ],
     "id": "RW-E7",
     "question": "Is the hand-back one line of counts, =reviewed whole-change …=, naming no path?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "RW-E1": [
       "reviewer § The whole-change review is one review file of its cycle",
       "reviewer § The whole-change review names the whole change in place of a task",
       "reviewer § The review file opens with what was reviewed and what was found",
       "record § A cycle's directory holds what its steps wrote, under fixed names",
       "integrate § The whole-change review is kept in the cycle's directory"
      ],
      "RW-E2": [
       "reviewer § The finding count and the clean mark agree with the findings",
       "reviewer § The review file holds a summary, the numbered findings and the thoughts"
      ],
      "RW-E3": [
       "reviewer § The whole-change review reports what no task's review could see",
       "reviewer § A document the diff makes untrue is a finding"
      ],
      "RW-E4": [
       "reviewer § Each finding carries a kind, a severity, its locations, its evidence and a recommended action",
       "reviewer § A recommended action is an inline fix, a rework or the user's",
       "reviewer § Every finding says what it blocks",
       "reviewer § A finding about the change's work names the direction it looks in",
       "reviewer § The reporter leaves the route to the routing"
      ],
      "RW-E5": [
       "reviewer § A defect the Reviewer can point at is an issue",
       "reviewer § A defect the file names anywhere is among its findings",
       "reviewer § Style and preference are not findings",
       "reviewer § Severity is the harm when the defect triggers",
       "reviewer § An issue's evidence shows the defect and who it harms"
      ],
      "RW-E6": [
       "reviewer § The Reviewer applies no fix and moves no ref",
       "reviewer § The Reviewer leaves the task file alone",
       "reviewer § A test run leaves the checkout as git sees it",
       "reviewer § Probes run in the scratch directory the brief names, which is gone afterwards",
       "reviewer § Every run ends before the hand-back",
       "reviewer § The Reviewer leaves what other agents share alone"
      ],
      "RW-E7": [
       "reviewer § The whole-change review names the whole change in place of a task",
       "reviewer § The hand-back line counts the findings",
       "reviewer § The hand-back carries no path and no summary"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RW-E1": [
       "RW-S1",
       "RW-S2"
      ],
      "RW-E2": [
       "RW-S3",
       "RW-S4"
      ],
      "RW-E3": [
       "RW-S10",
       "RW-S8",
       "RW-S9"
      ],
      "RW-E4": [
       "RW-S11",
       "RW-S5",
       "RW-S6",
       "RW-S7"
      ],
      "RW-E5": [
       "RW-S22"
      ],
      "RW-E6": [
       "RW-S12",
       "RW-S13",
       "RW-S14",
       "RW-S15",
       "RW-S16",
       "RW-S17",
       "RW-S18",
       "RW-S19"
      ],
      "RW-E7": [
       "RW-S20",
       "RW-S21"
      ]
     },
     "items_then": [],
     "run": "20261114T102110",
     "trials": [
      {
       "calls": {
        "13": {
         "error": false,
         "text": "python3 -m unittest discover -s tests -t . -v 2>&1; echo \"exit=$?\"; echo ---; python3 <trial>/skills/orchestrate/scripts/coverage.py --root . --intent docs/changes/json-report/intent.org; echo ---; python3 <trial>/skills/orchestrate/scripts/coverage.py --root .; echo; git status --porcelain --ignored",
         "tool": "Bash"
        }
       },
       "checks": {
        "RW-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists at the expected path, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the review file at cycles/<cycle-id>/reviews/whole-change.md",
         "detail": "<trial>/home/.orchestrator/changes/json-report/cycles/cycle-1/reviews/whole-change.md",
         "kind": "decide",
         "pass": true
        },
        "RW-S10": {
         "audit": {
          "agree": true,
          "reason": "Findings 1 and 3 mention `900.00`.",
          "verdict": "agrees"
         },
         "desc": "a finding names the example's amount",
         "detail": "#1 #3",
         "kind": "signal",
         "pass": true
        },
        "RW-S11": {
         "audit": {
          "agree": true,
          "reason": "No ask file was added, and the only added file is the review.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RW-S12": {
         "audit": {
          "agree": true,
          "reason": "The refs are unchanged and the checkout's HEAD equals the reviewed commit.",
          "verdict": "agrees"
         },
         "desc": "no ref moved; the checkout at the change's HEAD reviewed",
         "detail": "refs unchanged: True; checkout HEAD e3bda570a18405d6411845c682c544c9d892d672, reviewed e3bda570a18405d6411845c682c544c9d892d672",
         "kind": "decide",
         "pass": true
        },
        "RW-S13": {
         "audit": {
          "agree": true,
          "reason": "The porcelain status shows only the review file's line.",
          "verdict": "agrees"
         },
         "desc": "the checkout, the code root and the home as git sees them: as after the build (the checkout clean)",
         "detail": "checkout: clean | code root: clean | home: clean",
         "kind": "decide",
         "pass": true
        },
        "RW-S14": {
         "audit": {
          "agree": true,
          "reason": "No tool call touched `tasks/`.",
          "verdict": "agrees"
         },
         "desc": "the task files as the fixture wrote them",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "RW-S15": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows the scratch directory gone after the `rm -rf` in call 24.",
          "verdict": "agrees"
         },
         "desc": "the scratch directory gone",
         "detail": "<trial>/scratch/review-whole-change",
         "kind": "decide",
         "pass": true
        },
        "RW-S16": {
         "audit": {
          "agree": true,
          "reason": "All paths in the calls are under the trial directory, and no call runs `mktemp`.",
          "verdict": "agrees"
         },
         "desc": "no working file outside the trial's directory",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RW-S17": {
         "audit": {
          "agree": true,
          "reason": "No other home files were added, changed or removed.",
          "verdict": "agrees"
         },
         "desc": "the review file the only file added or changed in the home",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RW-S18": {
         "audit": {
          "agree": true,
          "reason": "No call has `run_in_background` or a trailing `&`.",
          "verdict": "agrees"
         },
         "desc": "no run in the background",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RW-S19": {
         "audit": {
          "agree": true,
          "reason": "No Bash command runs stash, worktree prune or remove, or gc.",
          "verdict": "agrees"
         },
         "desc": "no git stash, worktree prune or remove, or gc",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RW-S2": {
         "audit": {
          "agree": true,
          "reason": "The front matter holds the five keys, with `task_name` whole-change and `merge_commit` equal to HEAD.",
          "verdict": "agrees"
         },
         "desc": "front matter with the five fields, task_name whole-change, merge_commit the change's HEAD reviewed",
         "detail": "missing []; task_name='whole-change'; merge_commit='e3bda570a18405d6411845c682c544c9d892d672'; HEAD e3bda570a18405d6411845c682c544c9d892d672",
         "kind": "decide",
         "pass": true
        },
        "RW-S20": {
         "audit": {
          "agree": true,
          "reason": "The line has the required form and its counts match the file's findings.",
          "verdict": "agrees"
         },
         "desc": "the hand-back one line, reviewed whole-change with counts matching the file",
         "detail": "'reviewed whole-change issues=6 questions=1 spec-signal=1 thoughts=3'; the file: issues=6 questions=1 spec-signal=1",
         "kind": "decide",
         "pass": true
        },
        "RW-S21": {
         "audit": {
          "agree": true,
          "reason": "The final text contains no `/`.",
          "verdict": "agrees"
         },
         "desc": "the hand-back names no path",
         "detail": "reviewed whole-change issues=6 questions=1 spec-signal=1 thoughts=3",
         "kind": "decide",
         "pass": true
        },
        "RW-S22": {
         "audit": {
          "agree": true,
          "reason": "Finding 2 is about the refund rendering.",
          "verdict": "agrees"
         },
         "desc": "a finding names the refund's amount",
         "detail": "#1 #2 #3",
         "kind": "signal",
         "pass": true
        },
        "RW-S3": {
         "audit": {
          "agree": true,
          "reason": "`finding_count` is 7, there are 7 headings, and `clean_review` is false.",
          "verdict": "agrees"
         },
         "desc": "finding_count and clean_review agree with the findings",
         "detail": "finding_count='7' clean_review='false'; 7 finding heading(s)",
         "kind": "decide",
         "pass": true
        },
        "RW-S4": {
         "audit": {
          "agree": true,
          "reason": "The check verifies the summary, the numbering and that Thoughts comes after Findings; it explicitly leaves the last-section question to the eval agent.",
          "verdict": "agrees"
         },
         "desc": "a summary, the findings numbered from 1, thoughts after them when present",
         "detail": "sections: ['Summary', 'Findings', 'Thoughts', 'What I looked for and ruled out']; finding numbers [1, 2, 3, 4, 5, 6, 7]; thoughts present: True",
         "kind": "decide",
         "pass": true
        },
        "RW-S5": {
         "audit": {
          "agree": true,
          "reason": "Every finding has a valid kind, severity, direction, located line and evidence.",
          "verdict": "agrees"
         },
         "desc": "every finding: kind, severity, direction, a location with a line, evidence",
         "detail": "#1 kind=issue severity=high direction=spec-signal location=True evidence=True; #2 kind=issue severity=high direction=sub-par-code location=True evidence=True; #3 kind=issue severity=medium direction=design-drift location=True evidence=True; #4 kind=issue severity=low direction=sub-par-code location= … [cut, 558 characters]",
         "kind": "decide",
         "pass": true
        },
        "RW-S6": {
         "audit": {
          "agree": true,
          "reason": "The action kinds are `route-to-user` and `inline-fix`, both allowed.",
          "verdict": "agrees"
         },
         "desc": "every recommended action is inline-fix, rework or route-to-user",
         "detail": "#1 route-to-user; #2 inline-fix; #3 inline-fix; #4 inline-fix; #5 inline-fix; #6 inline-fix; #7 inline-fix",
         "kind": "decide",
         "pass": true
        },
        "RW-S7": {
         "audit": {
          "agree": true,
          "reason": "Every finding has a `blocks` value.",
          "verdict": "agrees"
         },
         "desc": "every finding says what it blocks",
         "detail": "#1 blocks='done'; #2 blocks='done'; #3 blocks='done'; #4 blocks='nothing'; #5 blocks='nothing'; #6 blocks='nothing'; #7 blocks='nothing'",
         "kind": "decide",
         "pass": true
        },
        "RW-S8": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 is located at reporting.org:24, inside the JSON scenario's lines 22–26.",
          "verdict": "agrees"
         },
         "desc": "a finding located at the JSON scenario in the reporting spec",
         "detail": "docs/specs/reporting.org lines [22, 23, 24, 25, 26]; findings there: #1",
         "kind": "decide",
         "pass": true
        },
        "RW-S9": {
         "audit": {
          "agree": true,
          "reason": "Finding 1 is of kind issue.",
          "verdict": "agrees"
         },
         "desc": "a finding located there is of kind issue (not applicable without one)",
         "detail": "#1 kind=issue",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RW-E1": "pass",
        "RW-E2": "fail",
        "RW-E3": "pass",
        "RW-E4": "pass",
        "RW-E5": "pass",
        "RW-E6": "pass",
        "RW-E7": "pass"
       },
       "judge": {
        "RW-E1": {
         "quotes": [
          "task_name: whole-change\nmerge_commit: e3bda570a18405d6411845c682c544c9d892d672\nreviewed_at: 2026-11-14T15:24:44Z\nfinding_count: 7\nclean_review: false",
          "task_name: whole-change\nmerge_commit: e3bda570a18405d6411845c682c544c9d892d672\nreviewed_at: 2026-11-14T15:24:44Z\nfinding_count: 7\nclean_review: false",
          "task_name: whole-change\nmerge_commit: e3bda570a18405d6411845c682c544c9d892d672\nreviewed_at: 2026-11-14T15:24:44Z\nfinding_count: 7\nclean_review: false"
         ],
         "reasons": [
          "The review file is at cycles/cycle-1/reviews/whole-change.md in the home, and its front matter has all five keys, naming the whole change and the HEAD that was reviewed.",
          "The file sits at cycles/cycle-1/reviews/whole-change.md in the home, and its front matter holds all five keys, naming the whole change and the HEAD that was reviewed.",
          "The file is at the cycle's reviews/whole-change.md, and its front matter holds all five keys, with the change's full HEAD as merge_commit."
         ]
        },
        "RW-E2": {
         "quotes": [
          "## Thoughts ... ## What I looked for and ruled out",
          "## Thoughts ... ## What I looked for and ruled out",
          "## Thoughts ... ## What I looked for and ruled out"
         ],
         "reasons": [
          "The count (7), the clean mark (false), the one-paragraph Summary and the findings numbered 1–7 are all correct, but `## Thoughts` is not the last section: a `## What I looked for and ruled out` section follows it.",
          "The count, clean mark, one-paragraph summary and findings numbered 1–7 are all right, but `## Thoughts` is not at the end: a further section, 'What I looked for and ruled out', follows it.",
          "The count (7), the clean mark (false), the one-paragraph Summary and the findings numbered 1–7 all hold, but Thoughts is not at the end: a further section follows it."
         ]
        },
        "RW-E3": {
         "quotes": [
          "dollar-table then changed the shared `_money` (report.py:17)... Because D1 has `format_json` reuse `_money`, the `$` reached the JSON too. The inline fix 3c51fbb then rewrote the scenario's citing test",
          "kind: issue ... file: docs/specs/reporting.org:24 ... dollar-table then changed the shared `_money` ... Because D1 has `format_json` reuse `_money`, the `$` reached the JSON too",
          "dollar-table then changed the shared `_money` ... Because D1 has `format_json` reuse `_money`, the `$` reached the JSON too ... the scenario's literal is untrue at HEAD"
         ],
         "reasons": [
          "Finding 1 is an issue located at docs/specs/reporting.org:24. It says the scenario's `900.00` example no longer matches what `--json` prints, and it names report-json, dollar-table and the inline fix as the causes.",
          "Finding 1 is an issue located at the JSON scenario's example line; it says the example's `900.00` no longer matches the printed `$900.00` and traces this to report-json and dollar-table together, plus the inline fix.",
          "Finding 1 is an issue located at docs/specs/reporting.org:24; it says the example's `900.00` no longer matches the printed `$900.00` and names report-json and dollar-table, with the inline fix, as the cause."
         ]
        },
        "RW-E4": {
         "quotes": [
          "#1 route-to-user; #2–#7 inline-fix; blocks done/nothing; directions spec-signal, sub-par-code, design-drift",
          "#1 route-to-user; #2–#7 inline-fix; blocks: done / nothing",
          "kind: route-to-user ... kind: inline-fix"
         ],
         "reasons": [
          "Every finding carries a kind, severity, one of the three directions, a path:line location, evidence, blocks, and an action of the three kinds with a detail line; no finding names a route and no ask was filed.",
          "Every finding carries kind, severity, one of the three directions, a location with a line, evidence, a blocks value and a one-line action of an allowed kind; none names a route, and no ask was filed.",
          "All seven findings carry kind, severity, one of the three directions, a file:line location, evidence, blocks and a one-line recommended action of an allowed kind; none names a route, and no ask was filed."
         ]
        },
        "RW-E5": {
         "quotes": [
          "### Finding 2: Negative totals are rendered wrong in the table and in the JSON (`-12.05` shows as `$-13.95`)",
          "### Finding 2: Negative totals are rendered wrong in the table and in the JSON (`-12.05` shows as `$-13.95`)",
          "### Finding 2: Negative totals are rendered wrong in the table and in the JSON (`-12.05` shows as `$-13.95`)"
         ],
         "reasons": [
          "The refund bug and every other defect the file names are findings with harm-based severities and probe evidence. The thoughts record test gaps where the probes showed the behaviour works, and no finding is a matter of taste.",
          "Every defect the file names (refund rendering, stale literals, column overflow, CLI arg handling, BOM/indent crash, intent template) is a finding with severity and probe evidence of its harm; the thoughts note test gaps rather than defects, and nothing is a matter of taste.",
          "Each defect the file names, the refund rendering among them, is a finding whose severity follows its harm and whose evidence comes from probes; the thoughts note test gaps rather than defects, and no finding is a matter of taste."
         ]
        },
        "RW-E6": {
         "quotes": [
          "checkout: clean | code root: clean | home: ?? ...reviews/whole-change.md; scratch directory: gone",
          "rm -rf .../scratch/review-whole-change ... git status --porcelain",
          "rm -rf .../scratch/review-whole-change ... git status --porcelain"
         ],
         "reasons": [
          "No refs moved, the checkout stayed clean at HEAD, the review file is the only addition, probes ran only in the scratch directory, which was then removed, and no run was in the background and none was a stash, prune or gc.",
          "All probes ran in the named scratch directory, which was removed; the checkout and code root are clean; the review file is the only addition to the home; every call stayed in the trial directory and ran in the foreground; no stash, prune or gc was run.",
          "Refs, the checkout and the code root are unchanged and clean, the review file is the only addition to the home, probes ran in the scratch directory and it was removed, and every call ran in the foreground with no stash, prune or gc."
         ]
        },
        "RW-E7": {
         "quotes": [
          "reviewed whole-change issues=6 questions=1 spec-signal=1 thoughts=3",
          "reviewed whole-change issues=6 questions=1 spec-signal=1 thoughts=3",
          "reviewed whole-change issues=6 questions=1 spec-signal=1 thoughts=3"
         ],
         "reasons": [
          "The hand-back is one line of the required form, its counts match the file (6 issues, 1 question, 1 spec-signal, 3 thoughts), and it names no path.",
          "The hand-back is one line of the required form, naming no path, and its counts match the file: 6 issues, 1 question, 1 spec-signal and 3 thoughts.",
          "The hand-back is one line of the required form, its counts match the file (6 issues, 1 question, 1 spec-signal, 3 thoughts), and it names no path."
         ]
        }
       },
       "milestones": [
        {
         "call": 13,
         "candidates": [
          13
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
       "n_calls": 24,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the whole-change Reviewer at a cycle's integrate, from its brief to its hand-back",
   "title": "Case reviewer-whole-change-tasks: the whole-change review reports what two tasks together made untrue",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T102110 t1 (skill-evals)"
  }
 ],
 "source": {
  "evals_head": "589177f",
  "spec": "orchestrate/docs/specs/reviewer.org",
  "spec_dirty": false,
  "worktree_head": "73b676c"
 },
 "spec": {
  "gaps": "No gap open. The four this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/reviewer.org= § Gap decisions).",
  "purpose": "A Reviewer reads one merged task, or at integrate the whole change, and writes what it found to a review file, then hands back one line. This spec states what that file must carry, what makes a finding (its kind, severity, direction, locations, evidence and recommended action, whoever writes it, the Architect included), what a Reviewer may and may not touch while it probes the merge, and the form of its hand-back line. It does not state what a Reviewer is given, when it is spawned, at which agent setting, or what becomes of its findings: =execute= states those for a task's review and =integrate= for the whole-change review.",
  "requirements": [
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer reviews a task"
       },
       {
        "kw": "THEN",
        "text": "it writes its review file at =.orchestrator/changes/<change>/cycles/<cycle-id>/reviews/<task>.md= in the home, the path its brief names"
       }
      ],
      "test": null,
      "title": "The review file is at the path its brief names"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review file is written"
       },
       {
        "kw": "THEN",
        "text": "it opens with front matter holding =task_name=, =merge_commit=, =reviewed_at=, =finding_count= and =clean_review="
       },
       {
        "kw": "AND",
        "text": "=merge_commit= is the commit the Reviewer reviewed"
       }
      ],
      "test": null,
      "title": "The review file opens with what was reviewed and what was found"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review file is written"
       },
       {
        "kw": "THEN",
        "text": "its =finding_count= is the number of findings in it"
       },
       {
        "kw": "AND",
        "text": "its =clean_review= is true when it has no finding and false otherwise"
       }
      ],
      "test": null,
      "title": "The finding count and the clean mark agree with the findings"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review file is written"
       },
       {
        "kw": "THEN",
        "text": "it has a =## Summary= of one paragraph: what the Reviewer looked at, what it ruled out and what it found"
       },
       {
        "kw": "AND",
        "text": "a =## Findings= section, each finding under its own heading =### Finding <n>: <one line>=, numbered from 1"
       },
       {
        "kw": "AND",
        "text": "a =## Thoughts= section at the end only when the Reviewer has a thought"
       }
      ],
      "test": null,
      "title": "The review file holds a summary, the numbered findings and the thoughts"
     }
    ],
    "text": "The Reviewer SHALL write its review of a task as one review file, at the path its brief names in the cycle's directory, opening with what it reviewed and how many findings it wrote, followed by a summary, the findings numbered in order, and its thoughts when it has any.",
    "title": "The Reviewer writes one review file per review"
   },
   {
    "goal": "none stated (=north-star.org= * Disposition states it; no goal does)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer finds a defect it can point at in code or text that exists"
       },
       {
        "kw": "THEN",
        "text": "the review file has a finding of kind =issue= for it, with a severity and its evidence"
       }
      ],
      "test": null,
      "title": "A defect the Reviewer can point at is an issue"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer finds a defect in code the diff calls or is called by, or in the tests, and the diff does not change that code"
       },
       {
        "kw": "THEN",
        "text": "it is a finding of kind =issue="
       },
       {
        "kw": "AND",
        "text": "it is reported though the Reviewer judges it unlikely, small, or off the change's main path"
       }
      ],
      "test": null,
      "title": "A defect in code the diff does not change is still reported"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a line the Reviewer would write names a defect present at the reviewed merge commit"
       },
       {
        "kw": "THEN",
        "text": "it is a finding of kind =issue=, whatever its severity"
       },
       {
        "kw": "AND",
        "text": "it is not a line among the thoughts"
       }
      ],
      "test": null,
      "title": "A present defect is never only a thought"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the review file's summary, its thoughts or what it ruled out names a defect in code or text that exists"
       },
       {
        "kw": "THEN",
        "text": "that defect is also a finding of kind =issue= or =question= in the file's findings"
       }
      ],
      "test": null,
      "title": "A defect the file names anywhere is among its findings"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the diff makes a sentence untrue in a document (a README, a help text, a comment, a cited seam row or scenario), whether or not the diff touches the document"
       },
       {
        "kw": "THEN",
        "text": "the sentence is a finding of kind =issue="
       }
      ],
      "test": null,
      "title": "A document the diff makes untrue is a finding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer suspects a defect and cannot settle whether it exists"
       },
       {
        "kw": "THEN",
        "text": "it is a finding of kind =question="
       },
       {
        "kw": "AND",
        "text": "its evidence gives the reasoning, and the reproduction that would settle it"
       }
      ],
      "test": null,
      "title": "A suspected defect the Reviewer cannot settle is a question"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer settles a suspected defect with a probe and the probe shows the defect"
       },
       {
        "kw": "THEN",
        "text": "it is a finding of kind =issue="
       },
       {
        "kw": "AND",
        "text": "the probe's output is in its evidence"
       }
      ],
      "test": null,
      "title": "A probe that shows a suspected defect makes it an issue"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer settles a suspected defect with a probe and the probe shows the code right"
       },
       {
        "kw": "THEN",
        "text": "the review file has no finding for it"
       }
      ],
      "test": null,
      "title": "A probe that shows the code right leaves no finding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "what the Reviewer would say is a matter of naming taste, formatting, a structure it would have written differently to the same effect, or a decision already settled"
       },
       {
        "kw": "THEN",
        "text": "the review file has no finding for it"
       }
      ],
      "test": null,
      "title": "Style and preference are not findings"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer finds nothing"
       },
       {
        "kw": "THEN",
        "text": "the review file says what the Reviewer looked for and why it ruled each thing out"
       }
      ],
      "test": null,
      "title": "A review with no finding says what was looked for"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the diff leaves a test it promotes still marked pending, or does not touch the test of a scenario the task cites, or edits the change's documents under =docs/="
       },
       {
        "kw": "THEN",
        "text": "each of these is a finding"
       }
      ],
      "test": null,
      "title": "A pending marker left, a cited scenario's test untouched or an edit under docs is a finding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer judges that the scope the task states is wrong"
       },
       {
        "kw": "THEN",
        "text": "it writes a finding whose direction is =spec-signal="
       }
      ],
      "test": null,
      "title": "A wrong task scope is a spec signal, not a change of scope"
     }
    ],
    "text": "The Reviewer SHALL write every defect it finds in code or text that exists as a finding: an =issue= when it can point at it, a =question= when it suspects it and cannot settle it. It SHALL report a defect whatever it judges of its likelihood, its size or its place, leaving the weighing to the routing; it SHALL report no matter of style or preference; and a review with no finding SHALL say what was looked for.",
    "title": "Every defect the Reviewer finds is a finding, whatever its size, likelihood or place"
   },
   {
    "goal": "none stated (=north-star.org= * Disposition states it; no goal does)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding is written in a review file"
       },
       {
        "kw": "THEN",
        "text": "it carries a =kind=, =issue= or =question="
       },
       {
        "kw": "AND",
        "text": "a =severity=, =high=, =medium= or =low="
       },
       {
        "kw": "AND",
        "text": "at least one location, a file and a line"
       },
       {
        "kw": "AND",
        "text": "its evidence"
       },
       {
        "kw": "AND",
        "text": "a recommended action: a kind, and one line saying what should happen"
       }
      ],
      "test": null,
      "title": "Each finding carries a kind, a severity, its locations, its evidence and a recommended action"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding's recommended action is written"
       },
       {
        "kw": "THEN",
        "text": "its kind is =inline-fix=, =rework= or =route-to-user="
       }
      ],
      "test": null,
      "title": "A recommended action is an inline fix, a rework or the user's"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding is written, in a review file or in a file of its own"
       },
       {
        "kw": "THEN",
        "text": "it carries =blocks=: the task, a merge, the next plan or the change's done that waits on it, or =nothing="
       }
      ],
      "test": null,
      "title": "Every finding says what it blocks"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding is about the diff's own code, its drift from the change's design, or a spec or design the work shows may be wrong"
       },
       {
        "kw": "THEN",
        "text": "its =direction= is =sub-par-code=, =design-drift= or =spec-signal= respectively"
       }
      ],
      "test": null,
      "title": "A finding about the change's work names the direction it looks in"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding is about code or tests that exist and that the diff does not change"
       },
       {
        "kw": "THEN",
        "text": "its =direction= is =sub-par-code="
       }
      ],
      "test": null,
      "title": "A defect in code the change did not write is sub-par code"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding's severity is set"
       },
       {
        "kw": "THEN",
        "text": "it is =high= for wrong or lost data, a breach of security or isolation, or a failure on a use the change exists for"
       },
       {
        "kw": "AND",
        "text": "=medium= for a wrong result or a misleading message that has a workaround"
       },
       {
        "kw": "AND",
        "text": "=low= for a minor wrong output or an untrue comment"
       }
      ],
      "test": null,
      "title": "Severity is the harm when the defect triggers"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding is of kind =issue="
       },
       {
        "kw": "THEN",
        "text": "its evidence names the line, the run or the probe's output, and what goes wrong for whom"
       }
      ],
      "test": null,
      "title": "An issue's evidence shows the defect and who it harms"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding recommends taking it to the user"
       },
       {
        "kw": "THEN",
        "text": "it says in one sentence, in product terms, what the feature, flag or rule in question is for"
       },
       {
        "kw": "AND",
        "text": "what it blocks: a task, a merge, the next plan, the change's done, or nothing"
       }
      ],
      "test": null,
      "title": "A finding for the user says what the thing is for and what it blocks"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding's direction is =spec-signal="
       },
       {
        "kw": "THEN",
        "text": "it says what the feature, flag or rule in question is for, and what it blocks"
       }
      ],
      "test": null,
      "title": "A spec signal says what the thing is for and what it blocks"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding with the =spec-signal= direction is of kind =issue="
       },
       {
        "kw": "THEN",
        "text": "it carries a stopgap: the least change that turns the defect's wrong success into a loud failure, whichever option the ask would offer"
       },
       {
        "kw": "AND",
        "text": "when no change holds under every option, the stopgap is =none= with the reason"
       }
      ],
      "test": null,
      "title": "A spec signal of kind issue carries a stopgap"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer writes a finding"
       },
       {
        "kw": "THEN",
        "text": "the finding names no route, neither blocking nor hardening"
       },
       {
        "kw": "AND",
        "text": "its recommended action says what should be done were the defect fixed now"
       }
      ],
      "test": null,
      "title": "The reporter leaves the route to the routing"
     }
    ],
    "text": "Each finding SHALL carry its kind, its severity, its locations, its evidence and a recommended action, and a Reviewer's finding SHALL name the direction it looks in. A finding the user must settle SHALL say what the thing in question is for and what it blocks, and a spec signal of kind =issue= SHALL carry a stopgap. The reporter SHALL NOT route its own finding.",
    "title": "A finding carries its labels, its place, its evidence and a recommended action"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer sees a need that would arise only after some future change, or cleanup that changes no behaviour today"
       },
       {
        "kw": "THEN",
        "text": "it is one line labelled =thought= under the file's =## Thoughts="
       },
       {
        "kw": "AND",
        "text": "it is not a finding"
       }
      ],
      "test": null,
      "title": "A speculative need or a cleanup is one thought line"
     }
    ],
    "text": "The Reviewer SHALL write a speculative need, or cleanup that changes no behaviour today, as one line labelled =thought= at the end of the review file, never as a finding.",
    "title": "A thought is one line at the end of the review file"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer hands back"
       },
       {
        "kw": "THEN",
        "text": "no branch or other ref of the repository or of the home has moved"
       },
       {
        "kw": "AND",
        "text": "the merge's checkout is clean at the reviewed merge commit"
       }
      ],
      "test": null,
      "title": "The Reviewer applies no fix and moves no ref"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer hands back"
       },
       {
        "kw": "THEN",
        "text": "the task file is as it was when the Reviewer was spawned"
       }
      ],
      "test": null,
      "title": "The Reviewer leaves the task file alone"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer runs a probe"
       },
       {
        "kw": "THEN",
        "text": "the probe runs in the scratch directory its brief names, against the merge's checkout"
       },
       {
        "kw": "AND",
        "text": "that directory is gone when the Reviewer hands back"
       }
      ],
      "test": null,
      "title": "Probes run in the scratch directory the brief names, which is gone afterwards"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer has run the project's tests or its probes against the merge's checkout"
       },
       {
        "kw": "THEN",
        "text": "no tracked file in the checkout has changed"
       },
       {
        "kw": "AND",
        "text": "nothing new is in it that the repository does not ignore"
       }
      ],
      "test": null,
      "title": "A test run leaves the checkout as git sees it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer runs the project's tests or a probe"
       },
       {
        "kw": "THEN",
        "text": "the run is in the foreground and has ended before the Reviewer hands back"
       },
       {
        "kw": "AND",
        "text": "the Reviewer hands back once"
       }
      ],
      "test": null,
      "title": "Every run ends before the hand-back"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer probes on a machine other agents share"
       },
       {
        "kw": "THEN",
        "text": "it uses no =git stash=, and prunes, collects or removes no worktree it did not create"
       },
       {
        "kw": "AND",
        "text": "any server or container it starts carries the name of the review's checkout, not a fixed one"
       },
       {
        "kw": "AND",
        "text": "it starts no GUI or interactive program and stops no process it did not start"
       }
      ],
      "test": null,
      "title": "The Reviewer leaves what other agents share alone"
     }
    ],
    "text": "The Reviewer SHALL be read-only: it SHALL apply no fix, commit nothing and leave the task file as it found it. It SHALL run its probes and the project's tests in the foreground, in the scratch directory its brief names, against the merge's checkout, leave nothing behind, and leave alone what other agents on the machine share.",
    "title": "The Reviewer changes nothing it reviews"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer has written its review file"
       },
       {
        "kw": "THEN",
        "text": "it hands back one line, =reviewed <task> issues=<n> questions=<n> spec-signal=<n> thoughts=<n>=, of at most 200 characters"
       },
       {
        "kw": "AND",
        "text": "=issues= and =questions= count the file's findings of each kind, and =spec-signal= those among them whose direction is =spec-signal="
       }
      ],
      "test": null,
      "title": "The hand-back line counts the findings"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Reviewer hands back"
       },
       {
        "kw": "THEN",
        "text": "the line names no path"
       },
       {
        "kw": "AND",
        "text": "the hand-back holds nothing but that line"
       }
      ],
      "test": null,
      "title": "The hand-back carries no path and no summary"
     }
    ],
    "text": "The Reviewer SHALL hand back one line of at most 200 characters, naming the task and counting its findings by kind and its spec signals and thoughts, with no path and nothing else.",
    "title": "The Reviewer hands back one line of counts"
   },
   {
    "goal": "G11",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review runs"
       },
       {
        "kw": "THEN",
        "text": "its review file is =.orchestrator/changes/<change>/cycles/<cycle-id>/reviews/whole-change.md= in the home, in the form of a task's review file"
       },
       {
        "kw": "AND",
        "text": "the Reviewer hands back one line"
       }
      ],
      "test": null,
      "title": "The whole-change review is one review file of its cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review file is written"
       },
       {
        "kw": "THEN",
        "text": "its =task_name= is =whole-change= and its =merge_commit= the change's HEAD it reviewed"
       },
       {
        "kw": "AND",
        "text": "its hand-back line is =reviewed whole-change issues=<n> questions=<n> spec-signal=<n> thoughts=<n>="
       }
      ],
      "test": null,
      "title": "The whole-change review names the whole change in place of a task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "tasks of the change interact wrongly, or a change bypassed review, or a scenario's test passes while the surface the user would use does not"
       },
       {
        "kw": "THEN",
        "text": "the whole-change review file has a finding for it"
       }
      ],
      "test": null,
      "title": "The whole-change review reports what no task's review could see"
     }
    ],
    "text": "The Reviewer of the whole change SHALL write one review file for the cycle in the form of a task's, report what no task's review could see, and hand back one line.",
    "title": "The whole-change review is written in the same form"
   },
   {
    "goal": "none stated (=north-star.org= * Disposition states it; no goal does)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect reports a finding"
       },
       {
        "kw": "THEN",
        "text": "it is one file, =.orchestrator/changes/<change>/cycles/<cycle-id>/findings/<finding-id>.md= in the home"
       },
       {
        "kw": "AND",
        "text": "its id is =arch-<cycle-id>-<n>="
       },
       {
        "kw": "AND",
        "text": "a finding of the design round, before any cycle, is in the change's directory instead, with id =arch-<change>-<n>= (=architect= § A defect found at the design round is a finding of the change, outside any cycle)"
       }
      ],
      "test": null,
      "title": "An Architect's finding is one file in its cycle's directory"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Architect's finding file is written"
       },
       {
        "kw": "THEN",
        "text": "its front matter holds =finding_id=, =trigger= (=premise-check=, =conformance= or =design-round=), =kind=, =severity=, =class=, a =title= of at most 80 characters, =discovered_from=, =discovered_by: architect=, =discovered_at=, the =seam= it is about when there is one, and =evidence="
       },
       {
        "kw": "AND",
        "text": "its =class= is one of =shape-fragmentation=, =vocabulary-mismatch=, =responsibility-leakage=, =dead-branch=, =interface-drift=, =mutation=, =invariant-gap= or =duplication="
       },
       {
        "kw": "AND",
        "text": "the Architect writes no =decoration="
       }
      ],
      "test": null,
      "title": "An Architect's finding carries its labels in its front matter"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Architect's finding file is written"
       },
       {
        "kw": "THEN",
        "text": "it has a =## Locations= section, each a file and line with its context"
       },
       {
        "kw": "AND",
        "text": "a =## Why tests missed it= section of one sentence"
       },
       {
        "kw": "AND",
        "text": "a =## Recommended resolution= section of one paragraph that names files and functions, and names the seam row amended and how when the code is right and the row was stale"
       }
      ],
      "test": null,
      "title": "An Architect's finding says where, why the tests missed it and what to do"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect has a thought"
       },
       {
        "kw": "THEN",
        "text": "no finding file is written for it"
       }
      ],
      "test": null,
      "title": "An Architect's thought is never a finding file"
     }
    ],
    "text": "Each finding the Architect reports SHALL be one file in its cycle's directory, carrying the kind and severity every finding carries, its class and trigger, where it was found and what it is about, why the tests missed it and a recommended resolution; the Architect SHALL write no decoration, and a thought SHALL never be a finding file.",
    "title": "An Architect's finding is a file of its own, with the same labels"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer is spawned at its role default"
       },
       {
        "kw": "THEN",
        "text": "a task's Reviewer runs at =opus=, =high="
       },
       {
        "kw": "AND",
        "text": "the whole-change Reviewer at =opus=, =xhigh="
       }
      ],
      "test": null,
      "title": "The Reviewer's role default is opus, high for a task and xhigh for the whole change"
     }
    ],
    "text": "The Reviewer SHALL run at its role default unless a task override raises it.",
    "title": "The Reviewer's role default"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); not yet reviewed; at the hand-off review 2026-11-13: G8 and G11 applied, its role default added; the recommended actions and the direction of a defect outside the work and read-only filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-14 after acceptance (a design-round finding's id and trigger), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning)",
  "title": "reviewer"
 },
 "summary": {
  "cases": 17,
  "cited": 44,
  "fail": 19,
  "pass": 25,
  "scenarios": 44,
  "title": "reviewer",
  "tree": "skill-evals",
  "validation": 16
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
