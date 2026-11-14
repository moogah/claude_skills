window.SPEC_PAGE = {
 "capability": "integrate",
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
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AK-E1": [
       "AK-S3",
       "AK-S4",
       "AK-S7"
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
        }
       },
       "items": {
        "AK-E1": "n/a"
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
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "AC-E1": [
       "AC-S4",
       "AC-S5"
      ],
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
        "AC-E2": "n/a",
        "AC-E3": "pass",
        "AC-E4": "pass"
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
   "capability": "integrate",
   "id": "integrate-close",
   "items": [
    {
     "checks": [
      {
       "id": "IC-S1",
       "kind": "decide"
      },
      {
       "id": "IC-S2",
       "kind": "decide"
      },
      {
       "id": "IC-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A blocking conformance finding becomes a follow-up candidate",
      "integrate § Integrate does not pass its gate while a blocking finding has no route",
      "record § A follow-up candidate in the handshake carries its source's words"
     ],
     "id": "IC-E1",
     "question": "Does the blocking conformance finding become a critical candidate, and the gate wait for its route?"
    },
    {
     "checks": [
      {
       "id": "IC-S3",
       "kind": "decide"
      },
      {
       "id": "IC-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A blocking whole-change finding holds the change for another cycle",
      "integrate § The digest and the routing count the whole-change review"
     ],
     "id": "IC-E2",
     "question": "Does the blocking whole-change finding hold the change for another cycle?"
    },
    {
     "checks": [
      {
       "id": "IC-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Each integrate writes a handshake with all its parts",
      "record § The handshake is a file of the change's directory, named for its cycle",
      "record § The handshake holds eight required fields, each present even when empty"
     ],
     "id": "IC-E3",
     "question": "Is the handshake written with all its parts?"
    },
    {
     "checks": [
      {
       "id": "IC-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The seam rows the cycle moved are listed with what moved them",
      "record § The handshake names each seam row that moved"
     ],
     "id": "IC-E4",
     "question": "Are the seam rows the cycle moved listed with what moved them?"
    },
    {
     "checks": [
      {
       "id": "IC-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Every open ask is carried in the handshake",
      "record § The handshake lists the open asks, and the settled ones with their decisions"
     ],
     "id": "IC-E5",
     "question": "Is every open ask carried in the handshake?"
    },
    {
     "checks": [
      {
       "id": "IC-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Every open task has an impact entry, empty when nothing touched it",
      "record § Each open task's refinement names what touched it"
     ],
     "id": "IC-E6",
     "question": "Does every open task have its impact entry, empty when nothing touched it?"
    },
    {
     "checks": [
      {
       "id": "IC-S9",
       "kind": "decide"
      },
      {
       "id": "IC-S1",
       "kind": "decide"
      },
      {
       "id": "IC-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Every blocking finding and every main-path answer that needs new work has a candidate",
      "integrate § A user's answer on the main path that needs new work becomes a candidate"
     ],
     "id": "IC-E7",
     "question": "Does every blocking finding and the main-path answer that needs work have a candidate?"
    },
    {
     "checks": [
      {
       "id": "IC-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A user's answer off the main path goes on the hardening list",
      "integrate § A decision off the main path is a hardening entry with its ask",
      "asks § Work off the main path that an answer asks for waits on the hardening list"
     ],
     "id": "IC-E8",
     "question": "Does the answer off the main path go on the hardening list with its ask?"
    },
    {
     "checks": [
      {
       "id": "IC-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A task named in an open ask is blocked on it",
      "record § A task blocked on an ask names the ask"
     ],
     "id": "IC-E9",
     "question": "Is the task an open ask names blocked on it?"
    },
    {
     "checks": [
      {
       "id": "IC-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate writes no task text"
     ],
     "id": "IC-E10",
     "question": "Does integrate write no task text?"
    },
    {
     "checks": [
      {
       "id": "IC-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Each integrate leaves a PM digest"
     ],
     "id": "IC-E11",
     "question": "Does the cycle leave its PM digest?"
    },
    {
     "checks": [
      {
       "id": "IC-S14",
       "kind": "decide"
      },
      {
       "id": "IC-S15",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Corrected documents are committed in the code root before the close commit",
      "integrate § Integrate closes with one commit in the home",
      "record § Closing a cycle keeps its state and its handshake in the cycle's directory",
      "record § Each phase's close commits the record in the home"
     ],
     "id": "IC-E12",
     "question": "Are the documents committed in the code root before one close commit that holds the cycle's archive?"
    },
    {
     "checks": [
      {
       "id": "IC-S16",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate changes no code but an untrue sentence"
     ],
     "id": "IC-E13",
     "question": "Does integrate change no code?"
    },
    {
     "checks": [
      {
       "id": "IC-S17",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate hands back one line"
     ],
     "id": "IC-E14",
     "question": "Does integrate hand back one line?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "IC-E1": [
       "integrate § A blocking conformance finding becomes a follow-up candidate",
       "integrate § Integrate does not pass its gate while a blocking finding has no route",
       "record § A follow-up candidate in the handshake carries its source's words"
      ],
      "IC-E10": [
       "integrate § Integrate writes no task text"
      ],
      "IC-E11": [
       "integrate § Each integrate leaves a PM digest"
      ],
      "IC-E12": [
       "integrate § Corrected documents are committed in the code root before the close commit",
       "integrate § Integrate closes with one commit in the home",
       "record § Closing a cycle keeps its state and its handshake in the cycle's directory",
       "record § Each phase's close commits the record in the home"
      ],
      "IC-E13": [
       "integrate § Integrate changes no code but an untrue sentence"
      ],
      "IC-E14": [
       "integrate § Integrate hands back one line"
      ],
      "IC-E2": [
       "integrate § A blocking whole-change finding holds the change for another cycle",
       "integrate § The digest and the routing count the whole-change review"
      ],
      "IC-E3": [
       "integrate § Each integrate writes a handshake with all its parts",
       "record § The handshake is a file of the change's directory, named for its cycle",
       "record § The handshake holds eight required fields, each present even when empty"
      ],
      "IC-E4": [
       "integrate § The seam rows the cycle moved are listed with what moved them",
       "record § The handshake names each seam row that moved"
      ],
      "IC-E5": [
       "integrate § Every open ask is carried in the handshake",
       "record § The handshake lists the open asks, and the settled ones with their decisions"
      ],
      "IC-E6": [
       "integrate § Every open task has an impact entry, empty when nothing touched it",
       "record § Each open task's refinement names what touched it"
      ],
      "IC-E7": [
       "integrate § Every blocking finding and every main-path answer that needs new work has a candidate",
       "integrate § A user's answer on the main path that needs new work becomes a candidate"
      ],
      "IC-E8": [
       "integrate § A user's answer off the main path goes on the hardening list",
       "integrate § A decision off the main path is a hardening entry with its ask",
       "asks § Work off the main path that an answer asks for waits on the hardening list"
      ],
      "IC-E9": [
       "integrate § A task named in an open ask is blocked on it",
       "record § A task blocked on an ask names the ask"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "IC-E1": [
       "IC-S1",
       "IC-S12",
       "IC-S2"
      ],
      "IC-E10": [
       "IC-S12"
      ],
      "IC-E11": [
       "IC-S13"
      ],
      "IC-E12": [
       "IC-S14",
       "IC-S15"
      ],
      "IC-E13": [
       "IC-S16"
      ],
      "IC-E14": [
       "IC-S17"
      ],
      "IC-E2": [
       "IC-S3",
       "IC-S4"
      ],
      "IC-E3": [
       "IC-S5"
      ],
      "IC-E4": [
       "IC-S6"
      ],
      "IC-E5": [
       "IC-S7"
      ],
      "IC-E6": [
       "IC-S8"
      ],
      "IC-E7": [
       "IC-S1",
       "IC-S3",
       "IC-S9"
      ],
      "IC-E8": [
       "IC-S10"
      ],
      "IC-E9": [
       "IC-S11"
      ]
     },
     "items_then": [],
     "run": "20261114T114232",
     "trials": [
      {
       "calls": {},
       "checks": {
        "IC-S1": {
         "audit": {
          "agree": false,
          "reason": "It fails the candidate because the whole spliced quote is not a substring of the finding file, though the finding's words are quoted verbatim; this matches E7's failure but not E1's pass.",
          "verdict": "suspect"
         },
         "desc": "a critical candidate quoting the blocking conformance finding verbatim",
         "detail": "[{\"name\": \"json-refund-sign\", \"source\": \"arch-cycle-2-1\", \"discovered_by\": \"architect\", \"discovered_class\": \"invariant-gap\", \"critical\": true, \"quote\": \"format_json({'food': -1205}) gives \\\"$-13.95\\\". Answer to ask-cycle-2-1: Yes, -$12.05, and do it before the MVP.\", \"evidence\": \".orchestrator/chang … [cut, 385 characters]",
         "kind": "decide",
         "pass": false
        },
        "IC-S10": {
         "audit": {
          "agree": true,
          "reason": "No decision hardening entry for ask-cycle-2-2 was added.",
          "verdict": "agrees"
         },
         "desc": "the off-main answer a decision (hardening) entry with its ask and no severity, and no candidate",
         "detail": "entries: ['issue (hardening): seam/table-amount names format_table as the owner of the dollar sign, which _money writes']; candidates naming it: 0",
         "kind": "decide",
         "pass": false
        },
        "IC-S11": {
         "audit": {
          "agree": true,
          "reason": "table-width has no state entry, so not applicable is right.",
          "verdict": "agrees"
         },
         "desc": "the task the open ask names blocked on it (not applicable when the task has no state entry)",
         "detail": "status None, blocker_note None",
         "kind": "decide",
         "pass": null
        },
        "IC-S12": {
         "audit": {
          "agree": true,
          "reason": "Nothing under tasks/ changed.",
          "verdict": "agrees"
         },
         "desc": "no task file written or edited",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IC-S13": {
         "audit": {
          "agree": true,
          "reason": "pm-digest.md is present in the cycle directory.",
          "verdict": "agrees"
         },
         "desc": "the cycle's directory holds the PM digest",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IC-S14": {
         "audit": {
          "agree": true,
          "reason": "The code-root commit time precedes the close commit's.",
          "verdict": "agrees"
         },
         "desc": "every code-root commit made before the close commit (not applicable when there is none)",
         "detail": "code-root commit times [1794674631]; close 1794674650",
         "kind": "decide",
         "pass": true
        },
        "IC-S15": {
         "audit": {
          "agree": true,
          "reason": "There is one close commit and the archive files are present.",
          "verdict": "agrees"
         },
         "desc": "one close commit holding the cycle's archive under the record's names",
         "detail": "1 close commit(s); missing at it: []",
         "kind": "decide",
         "pass": true
        },
        "IC-S16": {
         "audit": {
          "agree": true,
          "reason": "Only docs/specs/reporting.org changed.",
          "verdict": "agrees"
         },
         "desc": "no code changed in the code root",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IC-S17": {
         "audit": {
          "agree": true,
          "reason": "The final text is multi-line.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line naming integrate and the cycle",
         "detail": "integrate cycle-2: closed, 1 blocking finding routed to candidate json-refund-sign (a refund is written `$-13.95`), 1 ask open (ask-cycle-2-3, blocks table-width); `.orchestrator/changes/json-report/handshake-cycle-2.json`\n\nTwo of the earlier conformance results were wrong on disk, and I corrected b … [cut, 400 characters]",
         "kind": "decide",
         "pass": false
        },
        "IC-S2": {
         "audit": {
          "agree": true,
          "reason": "The gate passed and no blocking finding remained pending.",
          "verdict": "agrees"
         },
         "desc": "with the gate passed, no blocking finding pending",
         "detail": "gate passed: True; blocking and pending: []",
         "kind": "decide",
         "pass": true
        },
        "IC-S3": {
         "audit": {
          "agree": true,
          "reason": "No candidate quotes the whole-change review, which the evidence confirms.",
          "verdict": "agrees"
         },
         "desc": "a critical candidate quoting the blocking whole-change finding",
         "detail": "[{\"name\": \"json-refund-sign\", \"source\": \"arch-cycle-2-1\", \"discovered_by\": \"architect\", \"discovered_class\": \"invariant-gap\", \"critical\": true, \"quote\": \"format_json({'food': -1205}) gives \\\"$-13.95\\\". Answer to ask-cycle-2-1: Yes, -$12.05, and do it before the MVP.\", \"evidence\": \".orchestrator/chang … [cut, 385 characters]",
         "kind": "decide",
         "pass": false
        },
        "IC-S4": {
         "audit": {
          "agree": true,
          "reason": "No MVP or done status and no harden/rethink/stop ask was added.",
          "verdict": "agrees"
         },
         "desc": "the change neither stopped for the MVP decision nor done",
         "detail": "status values added: []; MVP asks: []",
         "kind": "decide",
         "pass": true
        },
        "IC-S5": {
         "audit": {
          "agree": true,
          "reason": "The handshake file exists with all its fields.",
          "verdict": "agrees"
         },
         "desc": "handshake-<cycle>.json with the eight fields",
         "detail": "present: True; missing: []",
         "kind": "decide",
         "pass": true
        },
        "IC-S6": {
         "audit": {
          "agree": true,
          "reason": "design_diff is empty, so the moved row is not listed.",
          "verdict": "agrees"
         },
         "desc": "design_diff entries with seam, change and ref; the moved row listed once",
         "detail": "[]",
         "kind": "decide",
         "pass": false
        },
        "IC-S7": {
         "audit": {
          "agree": true,
          "reason": "The open ask ask-cycle-2-3 is carried.",
          "verdict": "agrees"
         },
         "desc": "every open ask in the handshake's open asks",
         "detail": "open: ['ask-cycle-2-3']; not carried: []",
         "kind": "decide",
         "pass": true
        },
        "IC-S8": {
         "audit": {
          "agree": true,
          "reason": "Neither open task's entry names the moved seam, which fits the fail.",
          "verdict": "agrees"
         },
         "desc": "one impact entry per open task, the untouched one empty, the moved row in its task's",
         "detail": "{\"json-refund\": [{\"task\": \".orchestrator/changes/json-report/tasks/open/json-refund.md\", \"impact\": [{\"channel\": \"user-resolved-ask\", \"ref\": \"ask-cycle-2-1\"}], \"obsolescence_flagged\": false}], \"table-width\": [{\"task\": \".orchestrator/changes/json-report/tasks/open/table-width.md\", \"impact\": [], \"obsol … [cut, 326 characters]",
         "kind": "decide",
         "pass": false
        },
        "IC-S9": {
         "audit": {
          "agree": true,
          "reason": "No candidate's source names the ask.",
          "verdict": "agrees"
         },
         "desc": "a critical candidate for the main-path answer, naming the ask",
         "detail": "[{\"name\": \"json-refund-sign\", \"source\": \"arch-cycle-2-1\", \"discovered_by\": \"architect\", \"discovered_class\": \"invariant-gap\", \"critical\": true, \"quote\": \"format_json({'food': -1205}) gives \\\"$-13.95\\\". Answer to ask-cycle-2-1: Yes, -$12.05, and do it before the MVP.\", \"evidence\": \".orchestrator/chang … [cut, 385 characters]",
         "kind": "decide",
         "pass": false
        }
       },
       "items": {
        "IC-E1": "pass",
        "IC-E10": "pass",
        "IC-E11": "pass",
        "IC-E12": "pass",
        "IC-E13": "pass",
        "IC-E14": "fail",
        "IC-E2": "fail",
        "IC-E3": "pass",
        "IC-E4": "fail",
        "IC-E5": "pass",
        "IC-E6": "fail",
        "IC-E7": "fail",
        "IC-E8": "fail",
        "IC-E9": "n/a"
       },
       "judge": {
        "IC-E1": {
         "quotes": [
          "\"source\": \"arch-cycle-2-1\", ... \"critical\": true, \"quote\": \"format_json({'food': -1205}) gives \\\"$-13.95\\\". Answer to ask-cycle-2-1: ...\"",
          "\"quote\": \"format_json({'food': -1205}) gives \\\"$-13.95\\\". Answer to ask-cycle-2-1: Yes, -$12.05, and do it before the MVP.\"",
          "\"source\": \"arch-cycle-2-1\", ... \"critical\": true, \"quote\": \"format_json({'food': -1205}) gives \\\"$-13.95\\\". Answer to ask-cycle-2-1: ...\""
         ],
         "reasons": [
          "A critical candidate names arch-cycle-2-1 and quotes the finding's evidence verbatim (with the ask's answer appended); no task file was written; the resolution was set before the gate passed in call 16.",
          "The critical candidate names arch-cycle-2-1 and no task file was written, but its quote is not the finding's words: it splices the finding's evidence with the user's answer to an ask, so the quote's text is not in the finding.",
          "There is a critical candidate naming arch-cycle-2-1, it quotes the finding's evidence sentence verbatim, no task file was written, and the resolution was set (call 14) before the gate passed (call 16)."
         ]
        },
        "IC-E10": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "No tool call writes or edits a file under tasks/.",
          "None of the main thread's calls write or edit a file under tasks/, and nothing under tasks/ appears among the changed files.",
          "No call writes or edits anything under tasks/; the open task files were only read."
         ]
        },
        "IC-E11": {
         "quotes": [
          "## Signals\n\n- ⚠ One blocking finding pending (arch-cycle-2-1)",
          "# PM digest — change: json-report — cycle-2 ... ## Signals",
          "## Signals\n\n- ⚠ One blocking finding pending (arch-cycle-2-1)"
         ],
         "reasons": [
          "The cycle's directory holds pm-digest.md, and it has its signals section.",
          "The cycle's directory holds pm-digest.md with its signals section, and the close commit's tree includes pm-signals.json.",
          "The cycle's directory holds pm-digest.md, and it has its signals section."
         ]
        },
        "IC-E12": {
         "quotes": [
          "code-root commit times [1794674631]; close 1794674650",
          "code-root commit times [1794674631]; close 1794674650",
          "code-root commit times [1794674631]; close 1794674650"
         ],
         "reasons": [
          "Code-root commit 2110004 precedes the single home commit 'Orchestrator: cycle-2 integrate closed', which adds .orchestrator holding the archived state, the handshake and the cycle files.",
          "Code-root commit 2110004 precedes the single home commit 'Orchestrator: cycle-2 integrate closed', which contains the cycle's archive files.",
          "The single code-root commit precedes the one home commit 'Orchestrator: cycle-2 integrate closed', which holds the cycle's archived files."
         ]
        },
        "IC-E13": {
         "quotes": [
          "files: docs/specs/reporting.org",
          "files: docs/specs/reporting.org",
          "files: docs/specs/reporting.org"
         ],
         "reasons": [
          "Only a spec document changed in the code root; no code file was touched.",
          "The only change in the code root is to a spec document; no code changed.",
          "Only a spec document changed in the code root; no code changed."
         ]
        },
        "IC-E14": {
         "quotes": [
          "Two of the earlier conformance results were wrong on disk, and I corrected both:",
          "Two of the earlier conformance results were wrong on disk, and I corrected both:",
          "Two of the earlier conformance results were wrong on disk, and I corrected both:"
         ],
         "reasons": [
          "The hand-back runs to several paragraphs and bullets rather than one line.",
          "The hand-back runs to several paragraphs and bullets rather than one line.",
          "The hand-back runs to several paragraphs and bullets, not one line."
         ]
        },
        "IC-E2": {
         "quotes": [
          "arch-cycle-2-1 and whole-change F1 are one defect; one candidate json-refund-sign.",
          "arch-cycle-2-1 and whole-change F1 are one defect; one candidate json-refund-sign.",
          "arch-cycle-2-1 and whole-change F1 are one defect; one candidate json-refund-sign."
         ],
         "reasons": [
          "The only candidate names arch-cycle-2-1 as its source and quotes nothing from reviews/whole-change.md, so no candidate names the whole-change review, though the change was not stopped or marked done.",
          "The change was neither stopped nor marked done, but no candidate names the whole-change review as its source; the only candidate's source is arch-cycle-2-1.",
          "No candidate names the whole-change review: the only candidate's source is arch-cycle-2-1 and none of its words come from reviews/whole-change.md, although the change was neither stopped nor marked done."
         ]
        },
        "IC-E3": {
         "quotes": [
          "\"design_diff\": [], ... \"meta_discoveries\": [], \"user_resolved_goal_drift\": [], \"asks_for_user_open\": [...]",
          "\"design_diff\": [], ... \"meta_discoveries\": [],",
          "\"design_diff\": [], ... \"meta_discoveries\": [], \"user_resolved_goal_drift\": []"
         ],
         "reasons": [
          "handshake-cycle-2.json is in the change's directory with all the required fields, and the empty ones are present.",
          "handshake-cycle-2.json is in the change's directory with all required fields, the empty ones present as empty lists.",
          "handshake-cycle-2.json is in the change's directory with its required fields, and the empty ones are present."
         ]
        },
        "IC-E4": {
         "quotes": [
          "echo '[]' > $D/cycles/cycle-2/design-diff.json",
          "echo '[]' > $D/cycles/cycle-2/design-diff.json",
          "echo '[]' > $D/cycles/cycle-2/design-diff.json"
         ],
         "reasons": [
          "The agent overwrote the fixture's design-diff with an empty list, so the handshake's design_diff lists no moved row.",
          "The fixture's design-diff.json listed the moved row; the agent overwrote it with an empty list, so the handshake's design_diff names no moved row.",
          "The agent emptied the fixture's design-diff.json, so the handshake's design_diff lists no moved row and the fixture's moved_seam appears nowhere."
         ]
        },
        "IC-E5": {
         "quotes": [
          "\"asks_for_user_open\": [{\"id\": \"ask-cycle-2-3\", ... \"status\": \"open\"",
          "\"asks_for_user_open\": [{\"id\": \"ask-cycle-2-3\" ... \"decision\": \"Yes, but after the MVP.\"",
          "\"asks_for_user_open\": [{\"id\": \"ask-cycle-2-3\", ... \"status\": \"open\""
         ],
         "reasons": [
          "The one open ask is carried, and both applied asks appear in the resolved list with their decisions.",
          "The open ask is carried in the open list, and both settled asks are in the resolved list with their decisions.",
          "The one open ask is carried, and the two applied asks are in asks_for_user_resolved with their decisions."
         ]
        },
        "IC-E6": {
         "quotes": [
          "\"impact\":[{\"channel\":\"design-diff\",\"ref\":\"seam/table-amount\"}][:0]",
          "\"impact\":[{\"channel\":\"design-diff\",\"ref\":\"seam/table-amount\"}][:0]",
          "\"impact\":[{\"channel\":\"design-diff\",\"ref\":\"seam/table-amount\"}][:0]"
         ],
         "reasons": [
          "No refinement entry names the moved seam (the design-diff impact was sliced away), and entries were also written for the done tasks dollar-table and ledger-comments.",
          "No open task's impact names the moved seam: the agent deliberately emptied the seam impact, and json-refund lists only the ask.",
          "No refinement entry names the moved seam: the agent deliberately sliced the seam/table-amount impact to empty, and json-refund's impact names only the ask."
         ]
        },
        "IC-E7": {
         "quotes": [
          "\"name\": \"json-refund-sign\", \"source\": \"arch-cycle-2-1\"",
          "\"source\": \"arch-cycle-2-1\"",
          "\"source\": \"arch-cycle-2-1\""
         ],
         "reasons": [
          "The main-path ask ask-cycle-2-1 and the whole-change finding have no candidates of their own; everything was folded into one candidate sourced to the conformance finding.",
          "There is one candidate, and its source is the conformance finding; none names the main-path ask ask-cycle-2-1 as its source, and none comes from the whole-change finding.",
          "No candidate names the main-path ask as its source, and the whole-change blocking finding has no candidate of its own; the single candidate merges all three sources."
         ]
        },
        "IC-E8": {
         "quotes": [
          "* issue (hardening): seam/table-amount names format_table as the owner of the dollar sign",
          "* issue (hardening): seam/table-amount names format_table as the owner of the dollar sign, which _money writes",
          "* issue (hardening): seam/table-amount names format_table as the owner of the dollar sign"
         ],
         "reasons": [
          "No `decision (hardening):` entry with :REF: ask-cycle-2-2 was added, although the handshake claims it was applied via hardening.org.",
          "No decision (hardening) entry referencing ask-cycle-2-2 was added to hardening.org; the only new entry is for arch-cycle-2-2.",
          "No 'decision (hardening):' entry with the ask's id as :REF: was added, even though the handshake says ask-cycle-2-2 was applied_via hardening.org."
         ]
        },
        "IC-E9": {
         "quotes": [
          "\"blocks\": [\"table-width\"]",
          "",
          ""
         ],
         "reasons": [
          "table-width has no entry in the cycle's state tasks, so the item does not apply.",
          "table-width has no entry in the cycle's state, so the item does not apply.",
          "table-width has no entry in the cycle's state tasks, so the item does not apply."
         ]
        }
       },
       "milestones": [],
       "n_calls": 16,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after the PM's digest returned, to its hand-back",
   "title": "Case integrate-close: the routing to the next plan, the handshake, the close commit and the line",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114232 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-close-signals",
   "items": [
    {
     "checks": [
      {
       "id": "CS-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Each integrate writes a handshake with all its parts",
      "record § The handshake is a file of the change's directory, named for its cycle",
      "record § The handshake holds eight required fields, each present even when empty"
     ],
     "id": "CS-E1",
     "question": "Is the handshake written with all its parts?"
    },
    {
     "checks": [
      {
       "id": "CS-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The user's answer on goal drift reaches the next plan",
      "record § A goal-drift answer is carried with its label and its readback"
     ],
     "id": "CS-E2",
     "question": "Does the user's answer on goal drift reach the next plan?"
    },
    {
     "checks": [
      {
       "id": "CS-S3",
       "kind": "decide"
      },
      {
       "id": "CS-S4",
       "kind": "decide"
      },
      {
       "id": "CS-S5",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § A task that may be obsolete is put to the user, never closed",
      "asks § An ask names the work that waits on it"
     ],
     "id": "CS-E3",
     "question": "Is the task the cycle may have made obsolete put to the user and left open?"
    },
    {
     "checks": [
      {
       "id": "CS-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Every open task has an impact entry, empty when nothing touched it",
      "record § Each open task's refinement names what touched it"
     ],
     "id": "CS-E4",
     "question": "Does every open task have its impact entry?"
    },
    {
     "checks": [
      {
       "id": "CS-S6",
       "kind": "decide"
      },
      {
       "id": "CS-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A pattern across the cycle's tasks reaches the next plan",
      "integrate § A pattern that names concrete work becomes a candidate"
     ],
     "id": "CS-E5",
     "question": "Does the pattern across the cycle's tasks reach the next plan, its concrete work a candidate?"
    },
    {
     "checks": [
      {
       "id": "CS-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A cluster in the cross-change backlog is put to the user"
     ],
     "id": "CS-E6",
     "question": "Is the cluster in the cross-change backlog put to the user?"
    },
    {
     "checks": [
      {
       "id": "CS-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A thought that names a project prior becomes a candidate",
      "record § A follow-up candidate in the handshake carries its source's words"
     ],
     "id": "CS-E7",
     "question": "Does the thought that names a project prior become a candidate, quoting it?"
    },
    {
     "checks": [
      {
       "id": "CS-S11",
       "kind": "decide"
      },
      {
       "id": "CS-S14",
       "kind": "decide"
      },
      {
       "id": "CS-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate writes no task text",
      "integrate § Integrate closes with one commit in the home",
      "integrate § Integrate hands back one line"
     ],
     "id": "CS-E9",
     "question": "Does integrate write no task text, close with one commit, and hand back one line?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "CS-E1": [
       "integrate § Each integrate writes a handshake with all its parts",
       "record § The handshake is a file of the change's directory, named for its cycle",
       "record § The handshake holds eight required fields, each present even when empty"
      ],
      "CS-E2": [
       "integrate § The user's answer on goal drift reaches the next plan",
       "record § A goal-drift answer is carried with its label and its readback"
      ],
      "CS-E3": [
       "integrate § A task that may be obsolete is put to the user, never closed",
       "asks § An ask names the work that waits on it"
      ],
      "CS-E4": [
       "integrate § Every open task has an impact entry, empty when nothing touched it",
       "record § Each open task's refinement names what touched it"
      ],
      "CS-E5": [
       "integrate § A pattern across the cycle's tasks reaches the next plan",
       "integrate § A pattern that names concrete work becomes a candidate"
      ],
      "CS-E6": [
       "integrate § A cluster in the cross-change backlog is put to the user"
      ],
      "CS-E7": [
       "integrate § A thought that names a project prior becomes a candidate",
       "record § A follow-up candidate in the handshake carries its source's words"
      ],
      "CS-E9": [
       "integrate § Integrate writes no task text",
       "integrate § Integrate closes with one commit in the home",
       "integrate § Integrate hands back one line"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "CS-E1": [
       "CS-S1"
      ],
      "CS-E2": [
       "CS-S2"
      ],
      "CS-E3": [
       "CS-S3",
       "CS-S4",
       "CS-S5"
      ],
      "CS-E4": [
       "CS-S12"
      ],
      "CS-E5": [
       "CS-S6",
       "CS-S7"
      ],
      "CS-E6": [
       "CS-S9"
      ],
      "CS-E7": [
       "CS-S8"
      ],
      "CS-E9": [
       "CS-S11",
       "CS-S13",
       "CS-S14"
      ]
     },
     "items_then": [],
     "run": "20261114T122846",
     "trials": [
      {
       "calls": {
        "14": {
         "error": false,
         "text": "git diff 5a7e127..HEAD -- tally tests/test_report.py; cat tests/test_report.py; python3 -c \"\nfrom tally import report;print(report._money(-1205), report.format_table({'food':-1205}))\"; python3 -m unittest discover -s tests -t . 2>&1 | tail -4",
         "tool": "Bash"
        }
       },
       "checks": {
        "CS-S1": {
         "audit": {
          "agree": true,
          "reason": "The file exists with every field and the empty ones present, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "handshake-<cycle>.json in the change's directory with the eight fields",
         "detail": "present: True; missing: []",
         "kind": "decide",
         "pass": true
        },
        "CS-S11": {
         "audit": {
          "agree": true,
          "reason": "No file under tasks/ appears in the home's changed files.",
          "verdict": "agrees"
         },
         "desc": "no task file written or edited",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "CS-S12": {
         "audit": {
          "agree": true,
          "reason": "Each of the three open task files has exactly one entry.",
          "verdict": "agrees"
         },
         "desc": "one task-refinements entry per open task",
         "detail": "{\"dollar-table\": 1, \"json-dollar-test\": 1, \"ledger-comments\": 1}",
         "kind": "decide",
         "pass": true
        },
        "CS-S13": {
         "audit": {
          "agree": true,
          "reason": "The final text is one line containing integrate and cycle-2.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line naming integrate and the cycle",
         "detail": "integrate cycle-2: stopped, ask-cycle-2-3 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-3.org",
         "kind": "decide",
         "pass": true
        },
        "CS-S14": {
         "audit": {
          "agree": true,
          "reason": "Exactly one home commit has the closing subject; the separate asks commit has a different subject.",
          "verdict": "agrees"
         },
         "desc": "one close commit in the home",
         "detail": "1: ['77e9e0163']",
         "kind": "decide",
         "pass": true
        },
        "CS-S2": {
         "audit": {
          "agree": true,
          "reason": "It reads the ask id and continue from user_resolved_goal_drift, as the item expects.",
          "verdict": "agrees"
         },
         "desc": "the handshake's goal-drift answer holds the ask and continue",
         "detail": "[{\"ask\": \"ask-cycle-1-1\", \"decision\": \"continue\", \"rationale\": null}]",
         "kind": "decide",
         "pass": true
        },
        "CS-S3": {
         "audit": {
          "agree": true,
          "reason": "It finds ask-cycle-2-1 as a decision ask with exactly the four labels and blocking json-dollar-test, which the ask file confirms.",
          "verdict": "agrees"
         },
         "desc": "a decision ask with options close, refine, rescope, do nothing, blocking the task",
         "detail": "asks with those options: ['ask-cycle-2-1']; blocking json-dollar-test: ['ask-cycle-2-1']; labels: ask-cycle-2-2: ['leave externalised', 'promote', 'new change']; ask-cycle-2-3: ['harden', 'rethink', 'stop']; ask-cycle-2-1: ['close', 'refine', 'rescope', 'do nothing']",
         "kind": "decide",
         "pass": true
        },
        "CS-S4": {
         "audit": {
          "agree": true,
          "reason": "The task is still in tasks/open with one refinement entry naming it, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the task still open, with its entry in the handshake's task refinements",
         "detail": "open: True; entries: [{\"task\": \".orchestrator/changes/json-report/tasks/open/json-dollar-test.md\", \"impact\": [{\"channel\": \"inline-fix\", \"ref\": \"dollar-table\"}, {\"channel\": \"meta-discovery\", \"ref\": \"other/seam/table-amount\"}], \"obsolescence_flagged\": true}]",
         "kind": "decide",
         "pass": true
        },
        "CS-S5": {
         "audit": {
          "agree": true,
          "reason": "obsolescence_flagged is true on that entry.",
          "verdict": "agrees"
         },
         "desc": "the task's impact entry flags it",
         "detail": "[{\"task\": \".orchestrator/changes/json-report/tasks/open/json-dollar-test.md\", \"impact\": [{\"channel\": \"inline-fix\", \"ref\": \"dollar-table\"}, {\"channel\": \"meta-discovery\", \"ref\": \"other/seam/table-amount\"}], \"obsolescence_flagged\": true}]",
         "kind": "signal",
         "pass": true
        },
        "CS-S6": {
         "audit": {
          "agree": true,
          "reason": "meta_discoveries is non-empty and carries the pattern.",
          "verdict": "agrees"
         },
         "desc": "the handshake carries the pattern across tasks",
         "detail": "[{\"kind\": \"other\", \"scope\": \"seam/table-amount\", \"evidence\": [\"dollar-table\"], \"implication_for_next_plan\": \"A test that pins a rendering's text as a literal breaks when another task changes the rendering; a task that changes the rendering writes the dependent tests against the rendering's helper (_ … [cut, 325 characters]",
         "kind": "decide",
         "pass": true
        },
        "CS-S7": {
         "audit": {
          "agree": true,
          "reason": "candidate_tasks is empty, so failing for want of the pattern's candidate fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "a candidate, not critical, for the pattern's concrete work",
         "detail": "[]",
         "kind": "decide",
         "pass": false
        },
        "CS-S8": {
         "audit": {
          "agree": true,
          "reason": "No candidate quotes the thought; the agent ruled it out, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "a candidate, not critical, quoting the thought that names a prior",
         "detail": "[]",
         "kind": "decide",
         "pass": false
        },
        "CS-S9": {
         "audit": {
          "agree": true,
          "reason": "ask-cycle-2-2 is the extra decision ask, and its text concerns the csv backlog cluster.",
          "verdict": "agrees"
         },
         "desc": "a decision ask besides the obsolete task's (the cluster's)",
         "detail": "['ask-cycle-2-2']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "CS-E1": "pass",
        "CS-E2": "pass",
        "CS-E3": "pass",
        "CS-E4": "pass",
        "CS-E5": "fail",
        "CS-E6": "pass",
        "CS-E7": "fail",
        "CS-E9": "pass"
       },
       "judge": {
        "CS-E1": {
         "quotes": [
          "\"design_diff\": [], ... \"candidate_tasks\": [],",
          "\"design_diff\": [], ... \"candidate_tasks\": [],",
          "\"design_diff\": [], ... \"candidate_tasks\": [], ... \"journal\": ["
         ],
         "reasons": [
          "handshake-cycle-2.json sits in the change's directory and holds every field, with the empty ones (design_diff, candidate_tasks) present as empty lists.",
          "handshake-cycle-2.json is in the change's directory with every required field present, and the empty ones (design_diff, candidate_tasks) are present as empty lists.",
          "`.orchestrator/changes/json-report/handshake-cycle-2.json` holds the design diff, digest path, meta-discoveries, goal-drift answer, asks, task refinements, candidates and journal; the empty ones (`design_diff`, `candidate_tasks`) are present and empty."
         ]
        },
        "CS-E2": {
         "quotes": [
          "\"user_resolved_goal_drift\": [{\"ask\": \"ask-cycle-1-1\", \"decision\": \"continue\", \"rationale\": null}]",
          "\"user_resolved_goal_drift\": [{\"ask\": \"ask-cycle-1-1\", \"decision\": \"continue\", \"rationale\": null}]",
          "\"user_resolved_goal_drift\": [{\"ask\": \"ask-cycle-1-1\", \"decision\": \"continue\", \"rationale\": null}]"
         ],
         "reasons": [
          "The handshake carries the ask's id and continue; the answer gave no reason, so a null rationale is correct.",
          "The goal-drift answer carries the ask's id and continue; the answer was the label alone, so there is no reason to carry.",
          "The handshake carries the ask's id and `continue`; the answer was the label alone, so there is no reason to carry."
         ]
        },
        "CS-E3": {
         "quotes": [
          "- close :: ... - refine :: ... - rescope :: ... - do nothing :: ... * If unanswered\nThe task stays open and the next plan leaves it out of its batch.",
          "- close :: ... - refine :: ... - rescope :: ... - do nothing :: ... * If unanswered\nThe task stays open",
          "- close :: ... - refine :: ... - rescope :: ... - do nothing :: ... * If unanswered\nThe task stays open"
         ],
         "reasons": [
          "ask-cycle-2-1 is a decision ask with the four options, :BLOCKS: json-dollar-test and an if-unanswered that leaves the task open; the task file stays in tasks/open; its refinement has obsolescence_flagged true.",
          "ask-cycle-2-1 is a decision with exactly those four options and blocks json-dollar-test; the task is still in tasks/open/ and its impact entry has obsolescence_flagged true.",
          "ask-cycle-2-1 is a decision with the four options, `:BLOCKS: json-dollar-test`, and leaves the task open if unanswered; the task file is untouched, and its refinement entry has `obsolescence_flagged: true` with the inline fix as its impact."
         ]
        },
        "CS-E4": {
         "quotes": [
          "{\"task\":\".../json-dollar-test.md\",\"impact\":[{\"channel\":\"inline-fix\",\"ref\":\"dollar-table\"},...],\"obsolescence_flagged\":true}",
          "json-dollar-test.md\",\"impact\":[{\"channel\":\"inline-fix\",\"ref\":\"dollar-table\"}",
          "{\"task\":\".../json-dollar-test.md\",\"impact\":[{\"channel\":\"inline-fix\",\"ref\":\"dollar-table\"}, ...],\"obsolescence_flagged\":true}"
         ],
         "reasons": [
          "There is one entry for each of the three files in tasks/open; the obsolete task's entry names the inline fix and the other two are empty.",
          "Each of the three open task files has one entry: the obsolete one names the inline fix and the other two are empty.",
          "There is one entry for each of the three open task files: the obsolete one names the inline fix, and the other two are empty."
         ]
        },
        "CS-E5": {
         "quotes": [
          "\"candidate_tasks\": []",
          "\"candidate_tasks\": []",
          "\"candidate_tasks\": []"
         ],
         "reasons": [
          "The pattern is carried with its kind, scope, evidence and implication, but the concrete work it names (rewrite the JSON test against _money) was never made a follow-up candidate.",
          "The pattern reaches the handshake with its kind, scope, evidence and implication, but the concrete work it names (writing the JSON test against _money) is not a follow-up candidate.",
          "The pattern is in `meta_discoveries` with kind, scope, evidence and implication, but the concrete work it names (write the JSON test against `_money`, not a literal) was not made a follow-up candidate."
         ]
        },
        "CS-E6": {
         "quotes": [
          "- promote :: They are brought into json-report ... * If unanswered\nThey stay in the backlog.",
          "- promote :: They are brought into json-report ... * If unanswered\nThey stay in the backlog.",
          "- promote :: They are brought into json-report ... * If unanswered\nThey stay in the backlog."
         ],
         "reasons": [
          "ask-cycle-2-2 is a decision ask on the csv cluster that offers promote and defaults to leaving the cluster externalised.",
          "ask-cycle-2-2 is a decision on promoting the CSV cluster into the change, and its default if unanswered leaves the items externalised.",
          "ask-cycle-2-2 is a decision on the three-item CSV cluster that offers promoting it into the change, and its default if unanswered leaves the items externalised."
         ]
        },
        "CS-E7": {
         "quotes": [
          "thought ruled out: ... No candidate (YAGNI).",
          "thought ruled out: ... No candidate (YAGNI).",
          "thought ruled out: the table's column width and the JSON's separators are not two helpers doing one thing ... No candidate (YAGNI)."
         ],
         "reasons": [
          "The agent dismissed the Architect's priors-naming thought instead of making it a candidate in the thought's words; candidate_tasks is empty.",
          "The agent dismissed the Architect's thought instead of making it a candidate in the thought's own words.",
          "The agent ruled out the Architect's thought that names a prior instead of making it a candidate quoting the thought's words."
         ]
        },
        "CS-E9": {
         "quotes": [
          "integrate cycle-2: stopped, ask-cycle-2-3 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-3.org",
          "integrate cycle-2: stopped, ask-cycle-2-3 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-3.org",
          "integrate cycle-2: stopped, ask-cycle-2-3 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-3.org"
         ],
         "reasons": [
          "No task file was touched, there is exactly one 'Orchestrator: cycle-2 integrate closed' home commit (a separate asks commit came before it), and the hand-back is one line naming integrate and cycle-2.",
          "No task file was written, there is exactly one home commit 'Orchestrator: cycle-2 integrate closed' (alongside a separate commit for the asks), and the hand-back is one line naming integrate and cycle-2.",
          "No task file was touched, the home has one `Orchestrator: cycle-2 integrate closed` commit (beside a separate asks commit), and the hand-back is one line naming the phase and the cycle."
         ]
        }
       },
       "milestones": [
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.002s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 36,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after the PM's digest returned, to its hand-back",
   "title": "Case integrate-close-signals: the cycle's signals routed into the handshake",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T122846 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-goal-drift",
   "items": [
    {
     "checks": [
      {
       "id": "GD-S1",
       "kind": "decide"
      },
      {
       "id": "GD-S3",
       "kind": "signal"
      },
      {
       "id": "GD-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Goal drift marks the intent divergent"
     ],
     "id": "GD-E1",
     "question": "Does goal drift mark the intent divergent, with the signal and its evidence, committed in the code root?"
    },
    {
     "checks": [
      {
       "id": "GD-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Goal drift is a decision ask with four fixed options",
      "asks § A new ask is committed with a subject naming its id"
     ],
     "id": "GD-E2",
     "question": "Is goal drift one decision ask with exactly its four options?"
    },
    {
     "checks": [
      {
       "id": "GD-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate hands back one line"
     ],
     "id": "GD-E3",
     "question": "Does integrate hand back one line?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "GD-E1": [
       "integrate § Goal drift marks the intent divergent"
      ],
      "GD-E2": [
       "integrate § Goal drift is a decision ask with four fixed options",
       "asks § A new ask is committed with a subject naming its id"
      ],
      "GD-E3": [
       "integrate § Integrate hands back one line"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "GD-E1": [
       "GD-S1",
       "GD-S3",
       "GD-S6"
      ],
      "GD-E2": [
       "GD-S2"
      ],
      "GD-E3": [
       "GD-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T104359",
     "trials": [
      {
       "calls": {
        "9": {
         "error": false,
         "text": "python3 -c \"\nfrom tally import report\nprint(report.format_table({'food':-1205}))\"; git show 5a7e127:tally/report.py | grep -n \"_money\\|cents\" ; grep -rn \"negative\\|refund\" tests | head; python3 -m unittest discover -s tests -t . 2>&1 | tail -3; cat <trial>/skills/orchestrate/templates/ask.md | head -120",
         "tool": "Bash"
        }
       },
       "checks": {
        "GD-S1": {
         "audit": {
          "agree": true,
          "reason": "The intent's header stayed `open` and no commit changed the intent, so the fail is correct.",
          "verdict": "agrees"
         },
         "desc": "the intent's status value divergent <date> <the ask's id>, committed in the code root",
         "detail": "divergent values: []; the ask: None; commits changing the intent: []; uncommitted: none",
         "kind": "decide",
         "pass": false
        },
        "GD-S2": {
         "audit": {
          "agree": true,
          "reason": "No ask of any kind was created, so the fail matches the evidence and the item's expectation.",
          "verdict": "agrees"
         },
         "desc": "one decision ask whose options are exactly revise, split, abandon and continue, committed",
         "detail": "asks with those options: []; labels of the new asks: ",
         "kind": "decide",
         "pass": false
        },
        "GD-S3": {
         "audit": {
          "agree": true,
          "reason": "The intent at the end has no `* Status` section naming the drift, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "the intent's Status names the goal-drift signal",
         "detail": "",
         "kind": "signal",
         "pass": false
        },
        "GD-S4": {
         "audit": {
          "agree": true,
          "reason": "The final text is one line containing `integrate` and `cycle-2`, and the session ended on its own with no Agent call.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line naming integrate and the cycle",
         "detail": "integrate cycle-2: closed (c2910a8); 0 asks open; 1 hardening entry, a refund shows as $-13.95 instead of -12.05 (`arch-cycle-2-1`); spec scenarios corrected for the dollar sign (886d256); the PM's goal-drift ⚠ not routed (no source in the record, so no ask or status change); `handshake-cycle-2.json … [cut, 302 characters]",
         "kind": "decide",
         "pass": true
        },
        "GD-S6": {
         "audit": {
          "agree": true,
          "reason": "No mvp or done value was added to the intent, which is unchanged; this is a guard against a wrong status, and its pass fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the intent marked neither mvp nor done",
         "detail": "[]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "GD-E1": "fail",
        "GD-E2": "fail",
        "GD-E3": "pass"
       },
       "judge": {
        "GD-E1": {
         "quotes": [
          "#+STATUS: open",
          "#+STATUS: open",
          "#+STATUS: open"
         ],
         "reasons": [
          "The intent at the end is identical to the fixture: no divergent status, no `* Status` section with the signal and its evidence, and no commit touching the intent.",
          "The intent at the end is unchanged: its status line is still `open`, there is no `* Status` section naming the drift, and no code-root commit touches the intent (the only commit, 886d256, changes docs/specs/reporting.org).",
          "The intent at the end is identical to the fixture: no divergent status, no ask id, no `* Status` section and no commit to it. The agent instead journaled \"pm-digest goal-drift signal not routed\"."
         ]
        },
        "GD-E2": {
         "quotes": [
          "pm-digest goal-drift signal not routed: its claim that users' scripts strip the dollar sign has no source in the record",
          "pm-digest goal-drift signal not routed: its claim that users' scripts strip the dollar sign has no source in the record",
          "integrate cycle-2: closed (c2910a8); 0 asks open"
         ],
         "reasons": [
          "The agent chose not to route the fired goal-drift signal, so it wrote no decision ask with the options revise, split, abandon and continue, and made no commit naming one.",
          "No ask was raised: the handshake shows `asks_for_user_open: []` and the only home commit is \"Orchestrator: cycle-2 integrate closed\", so there is no decision ask with revise/split/abandon/continue.",
          "No ask of any kind was written, so there is no decision ask offering revise/split/abandon/continue and no home commit naming one."
         ]
        },
        "GD-E3": {
         "quotes": [
          "integrate cycle-2: closed (c2910a8); 0 asks open; ...",
          "integrate cycle-2: closed (c2910a8); 0 asks open; ...",
          "integrate cycle-2: closed (c2910a8); 0 asks open; 1 hardening entry, ...; `handshake-cycle-2.json`."
         ],
         "reasons": [
          "The hand-back is a single line naming the phase and the cycle; the agent did not stop on an ask, so the ask, blocks and path clause does not apply.",
          "The hand-back is a single line naming the phase and the cycle; since no ask was raised, no ask, block or path was due.",
          "The hand-back is one line naming the phase and the cycle; it did not stop on an ask, so the ask clause does not apply (the missing ask is judged under GD-E2)."
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
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.002s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 22,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after a digest whose goal-drift signal fired (recommending revise), to its hand-back",
   "title": "Case integrate-goal-drift: the PM's goal-drift signal marks the intent divergent and is put to the user",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T104359 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-harden-done",
   "items": [
    {
     "checks": [
      {
       "id": "HD-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § After harden there is no second MVP decision"
     ],
     "id": "HD-E1",
     "question": "After harden, is there no second MVP decision?"
    },
    {
     "checks": [
      {
       "id": "HD-S2",
       "kind": "decide"
      },
      {
       "id": "HD-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § After harden, done follows a clean whole-change review",
      "integrate § A change is reported done only after a whole-change review"
     ],
     "id": "HD-E2",
     "question": "Does done follow the clean whole-change review, committed in the code root?"
    },
    {
     "checks": [
      {
       "id": "HD-S4",
       "kind": "decide"
      },
      {
       "id": "HD-S11",
       "kind": "signal"
      },
      {
       "id": "HD-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The closing summary accounts for the goals, the hardening list, the open tasks and the corrections"
     ],
     "id": "HD-E3",
     "question": "Does the closing summary account for the goals, the hardening list, the open tasks and the corrections?"
    },
    {
     "checks": [
      {
       "id": "HD-S6",
       "kind": "decide"
      },
      {
       "id": "HD-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A done change's documents stay where they are",
      "integrate § A done change merges into its workspace integration branch"
     ],
     "id": "HD-E4",
     "question": "Do the done change's documents stay where they are, and the change merge into its workspace integration branch?"
    },
    {
     "checks": [
      {
       "id": "HD-S8",
       "kind": "decide"
      },
      {
       "id": "HD-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate closes with one commit in the home",
      "integrate § Integrate hands back one line"
     ],
     "id": "HD-E5",
     "question": "Does integrate close with one commit and hand back one line?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "HD-E1": [
       "integrate § After harden there is no second MVP decision"
      ],
      "HD-E2": [
       "integrate § After harden, done follows a clean whole-change review",
       "integrate § A change is reported done only after a whole-change review"
      ],
      "HD-E3": [
       "integrate § The closing summary accounts for the goals, the hardening list, the open tasks and the corrections"
      ],
      "HD-E4": [
       "integrate § A done change's documents stay where they are",
       "integrate § A done change merges into its workspace integration branch"
      ],
      "HD-E5": [
       "integrate § Integrate closes with one commit in the home",
       "integrate § Integrate hands back one line"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "HD-E1": [
       "HD-S1"
      ],
      "HD-E2": [
       "HD-S2",
       "HD-S3"
      ],
      "HD-E3": [
       "HD-S11",
       "HD-S4",
       "HD-S5"
      ],
      "HD-E4": [
       "HD-S6",
       "HD-S7"
      ],
      "HD-E5": [
       "HD-S8",
       "HD-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T114235",
     "trials": [
      {
       "calls": {},
       "checks": {
        "HD-S1": {
         "audit": {
          "agree": true,
          "reason": "No new ask exists, so finding none with the MVP labels fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "no second MVP decision",
         "detail": "[]",
         "kind": "decide",
         "pass": true
        },
        "HD-S11": {
         "audit": {
          "agree": true,
          "reason": "The summary quotes the left entry's title line.",
          "verdict": "agrees"
         },
         "desc": "the closing summary names the entry left",
         "detail": "not found: []",
         "kind": "signal",
         "pass": true
        },
        "HD-S2": {
         "audit": {
          "agree": true,
          "reason": "The done header, the committed intent change naming json-report, and the absence of uncommitted changes are all borne out by the diff and the commits.",
          "verdict": "agrees"
         },
         "desc": "the intent's status value done <date> <commit>, committed in a commit whose subject names the change",
         "detail": "done values: [('2026-11-14', '55aa0176b072')]; commits: ['json-report: done, closing summary']; uncommitted: none",
         "kind": "decide",
         "pass": true
        },
        "HD-S3": {
         "audit": {
          "agree": true,
          "reason": "The fixture's whole-change review is present, and no finding is routed blocking and left pending (`architect_findings` is empty).",
          "verdict": "agrees"
         },
         "desc": "a whole-change review in the cycle's directory and no blocking finding pending",
         "detail": "pending blocking: []",
         "kind": "decide",
         "pass": true
        },
        "HD-S4": {
         "audit": {
          "agree": true,
          "reason": "The `* Status` section names harden-refund, the only `:TASK:` value on the list.",
          "verdict": "agrees"
         },
         "desc": "the closing summary names each worked hardening entry's task",
         "detail": "not named: []",
         "kind": "decide",
         "pass": true
        },
        "HD-S5": {
         "audit": {
          "agree": true,
          "reason": "The summary names 1d19a72 and tests/test_json.py, matching the only record correction.",
          "verdict": "agrees"
         },
         "desc": "the closing summary names each record correction's commit and test",
         "detail": "all named",
         "kind": "decide",
         "pass": true
        },
        "HD-S6": {
         "audit": {
          "agree": true,
          "reason": "The only docs change is an in-place edit of `intent.org`; no file under `docs/` was removed.",
          "verdict": "agrees"
         },
         "desc": "the change's documents where they were",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "HD-S7": {
         "audit": {
          "agree": true,
          "reason": "ws/json-report has no new commits, so its fail for the missing merge matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the workspace branch's tip a merge `Merge json-report` of the change's tip; main's files unchanged",
         "detail": "ws/json-report tip: 5a7e127a2 'tally: totals per category as a table', parents []; the change's tip ea83f9625; main's files unchanged",
         "kind": "decide",
         "pass": false
        },
        "HD-S8": {
         "audit": {
          "agree": true,
          "reason": "Exactly one home commit with the expected subject appears.",
          "verdict": "agrees"
         },
         "desc": "one close commit in the home",
         "detail": "1",
         "kind": "decide",
         "pass": true
        },
        "HD-S9": {
         "audit": {
          "agree": true,
          "reason": "The final text is one line containing `integrate` and `cycle-3`, and no Agent call stopped the session.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line naming integrate and the cycle",
         "detail": "integrate cycle-3: done, change json-report is done, closing summary in `docs/changes/json-report/intent.org`; handshake `.orchestrator/changes/json-report/handshake-cycle-3.json`, gate passed, close commit 0e21352.",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "HD-E1": "pass",
        "HD-E2": "pass",
        "HD-E3": "pass",
        "HD-E4": "fail",
        "HD-E5": "pass"
       },
       "judge": {
        "HD-E1": {
         "quotes": [
          "\"asks_for_user_open\": [],",
          "\"asks_for_user_open\": [],",
          "\"asks_for_user_open\": [],\n  \"asks_for_user_resolved\": []"
         ],
         "reasons": [
          "The step opened no new ask: the handshake lists no open asks, and none of the step's calls created an ask file.",
          "No new ask was opened this cycle: the state's asks_for_user and the handshake's open and resolved asks are all empty, so there was no second MVP decision.",
          "The agent raised no new ask, so there is no second MVP decision with the harden/rethink/stop options after harden."
         ]
        },
        "HD-E2": {
         "quotes": [
          "#+STATUS: done 2026-11-14 55aa0176b072 ... ea83f9625221 json-report: done, closing summary",
          "+#+STATUS: done 2026-11-14 55aa0176b072 ... ea83f9625221 json-report: done, closing summary",
          "#+STATUS: done 2026-11-14 55aa0176b072 ... ea83f9625221 json-report: done, closing summary"
         ],
         "reasons": [
          "The status line became done with the date and the code root's HEAD (taken by `git rev-parse` before the commit), the summary sits under `* Status`, and both were committed in the code root under a subject naming json-report; the agent read this cycle's `whole-change.md`, and the handover reports no blocking finding.",
          "The status became done with the date and the code root's head at the time of writing (55aa0176b072). The closing summary sits under * Status. It was committed in the code root under a subject naming json-report, after the agent read this cycle's whole-change review, which the handover says left no blocking finding.",
          "The status line became done with a date and commit, the closing summary sits under a new =* Status=, and both were committed in the code root under a subject naming the change, after the agent read this cycle's whole-change review, which had no blocking finding."
         ]
        },
        "HD-E3": {
         "quotes": [
          "Worked: \"A refund category shows the wrong amount\" (task harden-refund); ... Left, as the answer to ask-cycle-2-1 chose: \"An amount with three decimals loses its last digit\" (low).",
          "- 1d19a72: =reporting= § Categories are ordered by total, pinned by tests/test_json.py:",
          "- Worked: \"A refund category shows the wrong amount\" (task harden-refund); ... - Left, as the answer to ask-cycle-2-1 chose: \"An amount with three decimals loses its last digit\" (low). ... - 1d19a72: =reporting= § Categories are ordered by total, pinned by tests/test_json.py:"
         ],
         "reasons": [
          "The summary has one line per goal, the two worked entries with their task, the left entry, the open task table-width, and the record correction 1d19a72 (scenario, pinning test tests/test_json.py, diff) and the text correction c07ebfd (file, diff).",
          "The summary has one line per goal (G1, G2), both worked entries with task harden-refund, the left entry, and the open task table-width. It gives the record correction with its commit, scenario, pinning test and diff, and the text correction with its commit, file and diff.",
          "The summary has one line per goal, both worked entries with their task, the left entry, the open task table-width, the record correction with its commit, scenario, pinning test and diff, and the text correction with its commit, file and diff."
         ]
        },
        "HD-E4": {
         "quotes": [
          "== Commits on ws/json-report since the fixture (first parent; subject, body, files) ==\n(none)",
          "== Commits on ws/json-report since the fixture (first parent; subject, body, files) ==\n(none)",
          "== Commits on ws/json-report since the fixture (first parent; subject, body, files) ==\n(none)"
         ],
         "reasons": [
          "The documents stayed in place and main was untouched, but the workspace integration branch gained no `Merge json-report` commit.",
          "The documents stayed in place, but the workspace integration branch gained no Merge json-report commit, so the change was not merged.",
          "The documents stayed in place and main is untouched, but the workspace integration branch never gained a =Merge json-report= commit."
         ]
        },
        "HD-E5": {
         "quotes": [
          "Orchestrator: cycle-3 integrate closed",
          "Orchestrator: cycle-3 integrate closed",
          "Orchestrator: cycle-3 integrate closed ... integrate cycle-3: done, change json-report is done, ..."
         ],
         "reasons": [
          "There is exactly one home commit with that subject, and the hand-back is a single line naming integrate and cycle-3.",
          "There is exactly one home commit, with the expected subject, and the hand-back is a single line naming integrate and cycle-3.",
          "There is exactly one home commit with the required subject, and a one-line hand-back that names the phase and the cycle."
         ]
        }
       },
       "milestones": [],
       "n_calls": 11,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after a clean whole-change review and the PM's digest of a harden cycle, to its hand-back",
   "title": "Case integrate-harden-done: after harden and a clean whole-change review, the change is done",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114235 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-mvp",
   "items": [
    {
     "checks": [
      {
       "id": "MV-S1",
       "kind": "decide"
      },
      {
       "id": "MV-S2",
       "kind": "decide"
      },
      {
       "id": "MV-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The main path working end to end stops the change for one decision",
      "integrate § Open tasks off the main path do not hold back the MVP decision",
      "asks § An ask names the work that waits on it"
     ],
     "id": "MV-E1",
     "question": "Does the main path working end to end stop the change for one decision, open tasks off the main path notwithstanding?"
    },
    {
     "checks": [
      {
       "id": "MV-S5",
       "kind": "decide"
      },
      {
       "id": "MV-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate hands back that it stopped on the MVP decision",
      "integrate § Integrate closes with one commit in the home",
      "integrate § Integrate hands back one line"
     ],
     "id": "MV-E2",
     "question": "Does integrate pass its gate and close before handing back that it stopped on the MVP decision?"
    },
    {
     "checks": [
      {
       "id": "MV-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The MVP decision says what the change does and how to try it",
      "asks § A decision ask has its six sections"
     ],
     "id": "MV-E3",
     "question": "Does the MVP decision say what the change does, how to try it, which main-path tests pass and what the review found?"
    },
    {
     "checks": [
      {
       "id": "MV-S7",
       "kind": "decide"
      },
      {
       "id": "MV-S8",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § The MVP decision lists the hardening list"
     ],
     "id": "MV-E4",
     "question": "Does the MVP decision list the hardening list?"
    },
    {
     "checks": [
      {
       "id": "MV-S10",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § The MVP decision lists the open tasks off the main path"
     ],
     "id": "MV-E5",
     "question": "Does the MVP decision list the open tasks off the main path?"
    },
    {
     "checks": [
      {
       "id": "MV-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The MVP decision shows every record correction to the specs"
     ],
     "id": "MV-E6",
     "question": "Does the MVP decision show every record correction to the specs, and the prose correction?"
    },
    {
     "checks": [
      {
       "id": "MV-S11",
       "kind": "decide"
      },
      {
       "id": "MV-S14",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § The MVP decision recommends one option and says what waits"
     ],
     "id": "MV-E7",
     "question": "Does the MVP decision recommend one option and say what waits?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "MV-E1": [
       "integrate § The main path working end to end stops the change for one decision",
       "integrate § Open tasks off the main path do not hold back the MVP decision",
       "asks § An ask names the work that waits on it"
      ],
      "MV-E2": [
       "integrate § Integrate hands back that it stopped on the MVP decision",
       "integrate § Integrate closes with one commit in the home",
       "integrate § Integrate hands back one line"
      ],
      "MV-E3": [
       "integrate § The MVP decision says what the change does and how to try it",
       "asks § A decision ask has its six sections"
      ],
      "MV-E4": [
       "integrate § The MVP decision lists the hardening list"
      ],
      "MV-E5": [
       "integrate § The MVP decision lists the open tasks off the main path"
      ],
      "MV-E6": [
       "integrate § The MVP decision shows every record correction to the specs"
      ],
      "MV-E7": [
       "integrate § The MVP decision recommends one option and says what waits"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "MV-E1": [
       "MV-S1",
       "MV-S2",
       "MV-S4"
      ],
      "MV-E2": [
       "MV-S5",
       "MV-S6"
      ],
      "MV-E3": [
       "MV-S3"
      ],
      "MV-E4": [
       "MV-S7",
       "MV-S8"
      ],
      "MV-E5": [
       "MV-S10"
      ],
      "MV-E6": [
       "MV-S9"
      ],
      "MV-E7": [
       "MV-S11",
       "MV-S14"
      ]
     },
     "items_then": [],
     "run": "20261114T104353",
     "trials": [
      {
       "calls": {
        "14": {
         "error": false,
         "text": "python3 -m unittest discover -s tests -t . 2>&1 | tail -4; D=<trial>/home/.orchestrator/changes/json-report/cycles/cycle-2; echo '[]' > $D/meta-discoveries.json; echo '[]' > $D/candidate-tasks.json; cat > $D/task-refinements.json <<'EOF'\n[\n  {\n    \"task\": \".orchestrator/changes/json-report/tasks/open/table-width.md\",\n    \"impact\": [],\n    \"obsolescence_flagged\": false\n  }\n]\nEOF\ngrep -n -i -A25 'ask entry\\|asks_for_user' <trial>/skills/orchestrate/references/state.md 2>/dev/null | head -60; ls <trial>/skills/orchestrate/",
         "tool": "Bash"
        }
       },
       "checks": {
        "MV-S1": {
         "audit": {
          "agree": true,
          "reason": "It reads the intent header at the branch tip and finds date, a code-root commit (56f2060, HEAD before the status commit) and the ask id, with nothing uncommitted, which matches the diff.",
          "verdict": "agrees"
         },
         "desc": "the intent's status value mvp <date> <commit> <the MVP ask's id>, committed in the code root",
         "detail": "mvp values: [('2026-11-14', '56f2060', 'ask-cycle-2-1')]; the ask: ask-cycle-2-1; uncommitted: none",
         "kind": "decide",
         "pass": true
        },
        "MV-S10": {
         "audit": {
          "agree": true,
          "reason": "The ask names table-width under open tasks off the main path.",
          "verdict": "agrees"
         },
         "desc": "the ask names the open task off the main path",
         "detail": "",
         "kind": "signal",
         "pass": true
        },
        "MV-S11": {
         "audit": {
          "agree": true,
          "reason": "The Recommendation names harden and the ask has * If unanswered.",
          "verdict": "agrees"
         },
         "desc": "the Recommendation names an option, and If unanswered is there",
         "detail": "- harden :: The first item is a defect a user would notice: a refund shown with the wrong number, which a script reading the JSON would then add up wrongly. It was seen on a real input, and the change's purpose is that scripts can trust the totals.\n\n",
         "kind": "decide",
         "pass": true
        },
        "MV-S14": {
         "audit": {
          "agree": true,
          "reason": "Only the harden label appears in the Recommendation.",
          "verdict": "agrees"
         },
         "desc": "the Recommendation names one option only",
         "detail": "['harden']",
         "kind": "signal",
         "pass": true
        },
        "MV-S2": {
         "audit": {
          "agree": true,
          "reason": "Exactly one new ask has the three labels, kind decision and BLOCKS plan, and the home commit 'Ask ask-cycle-2-1' names it, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "one decision ask with the options harden, rethink and stop, blocking plan, committed",
         "detail": "asks with those options: ['ask-cycle-2-1']; kind decision; blocks plan: True; committed: True",
         "kind": "decide",
         "pass": true
        },
        "MV-S3": {
         "audit": {
          "agree": true,
          "reason": "It checks the six section headings, which are all present. It does not inspect their content, which is left to the judged item, but its verdict fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the MVP ask has the six sections",
         "detail": "missing: []",
         "kind": "decide",
         "pass": true
        },
        "MV-S4": {
         "audit": {
          "agree": true,
          "reason": "table-width is still listed under tasks/open and no tasks/ file appears among the home changes.",
          "verdict": "agrees"
         },
         "desc": "the open task off the main path still open, no task file written or edited",
         "detail": "open: ['table-width']; changed: []",
         "kind": "decide",
         "pass": true
        },
        "MV-S5": {
         "audit": {
          "agree": true,
          "reason": "The integrate gate shows passed with a passed_at, and there is exactly one 'Orchestrator: cycle-2 integrate closed' commit.",
          "verdict": "agrees"
         },
         "desc": "the gate passed and one close commit made",
         "detail": "gate passed: True; close commits: 1",
         "kind": "decide",
         "pass": true
        },
        "MV-S6": {
         "audit": {
          "agree": true,
          "reason": "The hand-back is exactly the expected line with the ask's id and inbox path.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `integrate <cycle-id>: stopped, ask <id> blocks plan; <path>` for the MVP ask",
         "detail": "integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
         "kind": "decide",
         "pass": true
        },
        "MV-S7": {
         "audit": {
          "agree": true,
          "reason": "The ask names =.orchestrator/changes/json-report/hardening.org=.",
          "verdict": "agrees"
         },
         "desc": "the ask names the hardening list's path",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "MV-S8": {
         "audit": {
          "agree": true,
          "reason": "The three entry headings appear in the ask at increasing positions in high, medium, low order.",
          "verdict": "agrees"
         },
         "desc": "each hardening entry's line in the ask, highest severity first",
         "detail": "positions in severity order: [1424, 1626, 1800]",
         "kind": "signal",
         "pass": true
        },
        "MV-S9": {
         "audit": {
          "agree": true,
          "reason": "The ask names 1d19a72, the scenario title, tests/test_json.py and c07ebfd.",
          "verdict": "agrees"
         },
         "desc": "the ask shows each record correction to the specs (commit, scenario, test) and the prose correction (commit)",
         "detail": "all shown",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "MV-E1": "pass",
        "MV-E2": "pass",
        "MV-E3": "pass",
        "MV-E4": "pass",
        "MV-E5": "pass",
        "MV-E6": "pass",
        "MV-E7": "pass"
       },
       "judge": {
        "MV-E1": {
         "quotes": [
          "+#+STATUS: mvp 2026-11-14 56f2060 ask-cycle-2-1",
          "#+STATUS: mvp 2026-11-14 56f2060 ask-cycle-2-1",
          "+#+STATUS: mvp 2026-11-14 56f2060 ask-cycle-2-1"
         ],
         "reasons": [
          "The intent's status line names the date, the code-root commit and the ask, committed on json-report in 4e79490. One decision ask (harden, rethink, stop; :BLOCKS: plan) is committed in the home. table-width stays in tasks/open with an empty impact.",
          "The intent's status line names the date, the code commit and the ask, and is committed on json-report. There is one decision ask with harden/rethink/stop and :BLOCKS: plan, committed in the home ('Ask ask-cycle-2-1: MVP decision for json-report'), and table-width stays in tasks/open with obsolescence_flagged false.",
          "The intent's status carries the date, the code-root HEAD and the ask id and is committed; there is one decision ask (harden/rethink/stop) with :BLOCKS: plan, committed in the home; table-width stays in tasks/open."
         ]
        },
        "MV-E2": {
         "quotes": [
          "integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org"
         ],
         "reasons": [
          "Call 20 ran gate pass, close and the commit 'Orchestrator: cycle-2 integrate closed' before the session ended on exactly that line.",
          "Call 20 ran gate pass and close, then committed 'Orchestrator: cycle-2 integrate closed' (the gate shows passed_at 15:45:06Z), and the session ended on exactly the required line.",
          "Call 20 passes the integrate gate, runs close and commits 'Orchestrator: cycle-2 integrate closed' before the session ends on exactly that one line."
         ]
        },
        "MV-E3": {
         "quotes": [
          "Try it with =python3 -m tally.cli FILE --json= from the repository root ... The tests pass (6 of 6) ... The review of the whole change ... found nothing",
          "=tally FILE --json= now prints the totals as one JSON object, ... =tally FILE --yaml= (any option other than =--json=) now stops with status 2",
          "Try it with =python3 -m tally.cli FILE --json= from the repository root, or =python3 -m tally.cli FILE --yaml= to see the refusal."
         ],
         "reasons": [
          "About says what each goal now does and how to try it, What happened gives the passing tests per scenario and the review's result, and all six sections are present; About covers both goals in one paragraph rather than one per goal, but each goal is stated distinctly.",
          "The ask has its six sections, how to try it and the review's result, but it puts both goals in a single About paragraph rather than one per goal, and it gives a count ('6 of 6') and scenario titles rather than naming the main-path tests.",
          "About says what each goal now does and how to try each; What happened gives the passing tests (6 of 6, each scenario tested) and that the whole-change review found nothing; all six sections are present, though the two goals share one paragraph."
         ]
        },
        "MV-E4": {
         "quotes": [
          "Highest severity first; the full text is in =.orchestrator/changes/json-report/hardening.org=.",
          "Highest severity first; the full text is in =.orchestrator/changes/json-report/hardening.org=.",
          "Highest severity first; the full text is in =.orchestrator/changes/json-report/hardening.org=."
         ],
         "reasons": [
          "The ask has one line for each of the three entries, ordered high, medium, low, and gives the list's path.",
          "There is one line per hardening entry, ordered high, medium, low, with the list's path.",
          "There is one line per entry, ordered high, medium, low, with the list's path."
         ]
        },
        "MV-E5": {
         "quotes": [
          "table-width: size the table's amount column to the widest amount instead of ten characters. It came with the change's plan and no goal names it",
          "table-width: size the table's amount column to the widest amount instead of ten characters. It came with the change's plan",
          "table-width: size the table's amount column to the widest amount instead of ten characters. It came with the change's plan and no goal names it"
         ],
         "reasons": [
          "The off-main task is described in product terms and says it came with the change's plan.",
          "The open off-main task is described in product terms with where it came from.",
          "The off-main task is described in product terms and says where it came from."
         ]
        },
        "MV-E6": {
         "quotes": [
          "One, commit 1d19a72, scenario \"Categories are ordered by total\" in =docs/specs/reporting.org=, pinned by =tests/test_json.py=",
          "One, commit 1d19a72, scenario \"Categories are ordered by total\" in =docs/specs/reporting.org=, pinned by =tests/test_json.py=.",
          "One, commit 1d19a72, scenario \"Categories are ordered by total\" in =docs/specs/reporting.org=, pinned by =tests/test_json.py=."
         ],
         "reasons": [
          "The record correction is shown with its commit, scenario, pinning test and a diff matching the history. The prose correction is shown with commit c07ebfd and a diff (the elided version was fixed in call 18). Both are presented as facts, not questions.",
          "The record correction appears with its commit, scenario, pinning test and diff, and the prose correction appears with commit c07ebfd and its full diff (the elided draft was fixed in call 18). Both are shown as information, not asked about.",
          "The record correction is shown with its commit, scenario, pinning test and diff, then the prose correction with commit c07ebfd and its diff (fixed to the full sentence in call 18); both are shown, not asked about."
         ]
        },
        "MV-E7": {
         "quotes": [
          "- harden :: The first item is a defect a user would notice ... Nothing runs; the change waits at its first working version.",
          "Nothing runs; the change waits at its first working version.",
          "Nothing runs; the change waits at its first working version."
         ],
         "reasons": [
          "Exactly one option is recommended with a reason, and If unanswered says nothing runs and the change waits at the MVP.",
          "Exactly one option (harden) is recommended with a reason, and the If unanswered section says nothing runs and the change waits at its MVP.",
          "Only harden is recommended, with its reason, and If unanswered says nothing runs and the change waits at its MVP."
         ]
        }
       },
       "milestones": [
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 20,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after conformance (zero findings), a clean whole-change review and the PM's digest, to its hand-back",
   "title": "Case integrate-mvp: the main path works end to end, and the change stops for the MVP decision",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T104353 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-mvp-rethink",
   "items": [
    {
     "checks": [
      {
       "id": "MD-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate hands back that it stopped on the MVP decision"
     ],
     "id": "MD-E1",
     "question": "Does the first turn stop on the MVP decision, so that the dispatcher resumes it with the answer?"
    },
    {
     "checks": [
      {
       "id": "MD-S2",
       "kind": "decide"
      },
      {
       "id": "MD-S3",
       "kind": "decide"
      },
      {
       "id": "MD-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Rethink marks the intent divergent"
     ],
     "id": "MD-E2",
     "question": "Applying rethink, does integrate mark the intent divergent, with the signal user and the answer quoted?"
    },
    {
     "checks": [
      {
       "id": "MD-S9",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § Integrate hands back one line"
     ],
     "id": "MD-E4",
     "question": "Does integrate hand back one line?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "MD-E1": [
       "integrate § Integrate hands back that it stopped on the MVP decision"
      ],
      "MD-E2": [
       "integrate § Rethink marks the intent divergent"
      ],
      "MD-E4": [
       "integrate § Integrate hands back one line"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "MD-E1": [
       "MD-S1"
      ],
      "MD-E2": [
       "MD-S2",
       "MD-S3",
       "MD-S4"
      ],
      "MD-E4": [
       "MD-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T122843",
     "trials": [
      {
       "calls": {
        "15": {
         "error": false,
         "text": "cd <trial>/home/.worktrees/json-report && BASE=5a7e127a2a39b2991f0cb7b26e7959ef8456a430; git log -i --grep='text correction' -p $BASE..HEAD; git rev-parse HEAD; ls; python3 -m unittest discover -s tests -t . 2>&1 | tail -4; ls docs/specs; ls ~ >/dev/null; python3 -m tally --help 2>&1 | head -5",
         "tool": "Bash"
        }
       },
       "checks": {
        "MD-S1": {
         "audit": {
          "agree": true,
          "reason": "It observes the stopped line for the new MVP ask and the sent resume, which matches the evidence and the item.",
          "verdict": "agrees"
         },
         "desc": "the first hand-back is the stopped line for the MVP ask, and the answer and resume were sent",
         "detail": "the MVP ask: ask-cycle-2-1; first hand-back: 'integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org'; sent: \"The user answered ask-cycle-2-1: rethink. The answer is written under the ask's * Answer and committed (.orchestrator/changes/js … [cut, 336 characters]",
         "kind": "decide",
         "pass": true
        },
        "MD-S2": {
         "audit": {
          "agree": true,
          "reason": "The tip's #+STATUS is `divergent 2026-11-14 ask-cycle-2-1` with no uncommitted change, as the code-root diff and commit show.",
          "verdict": "agrees"
         },
         "desc": "the intent's status line divergent <date> <the MVP ask's id>, committed in the code root (not applicable without the resume)",
         "detail": "#+STATUS: 'divergent 2026-11-14 ask-cycle-2-1'; divergent values: [('2026-11-14', 'ask-cycle-2-1')]; the ask: ask-cycle-2-1; uncommitted: none",
         "kind": "decide",
         "pass": true
        },
        "MD-S3": {
         "audit": {
          "agree": true,
          "reason": "The bare answer `rethink` appears in the * Status, quoted as =rethink=, which is what the item asks.",
          "verdict": "agrees"
         },
         "desc": "the user's answer quoted under the intent's * Status (not applicable without the resume)",
         "detail": "the answer: 'rethink'; * Status: \"** divergent 2026-11-14 ask-cycle-2-1\\nSignal: user. The MVP decision (ask-cycle-2-1) was answered =rethink=, with no words saying what is wrong. The Goals at the MVP, kept here for the revision:\\n#+begin_quote\\n- G1. =tally FILE --json= prints the totals as one JSO … [cut, 460 characters]",
         "kind": "decide",
         "pass": true
        },
        "MD-S4": {
         "audit": {
          "agree": true,
          "reason": "The final status value is divergent, neither mvp nor done; the intermediate mvp was superseded.",
          "verdict": "agrees"
         },
         "desc": "the intent neither back at mvp nor done (not applicable without the resume)",
         "detail": "#+STATUS: 'divergent 2026-11-14 ask-cycle-2-1'",
         "kind": "decide",
         "pass": true
        },
        "MD-S9": {
         "audit": {
          "agree": true,
          "reason": "The final result is one line beginning `integrate cycle-2`; the observed text is the check's display cut, not a second line.",
          "verdict": "agrees"
         },
         "desc": "the second hand-back one line naming integrate and the cycle (not applicable without the resume)",
         "detail": "321 characters: \"integrate cycle-2: done, rethink applied (ask-cycle-2-1 applied, intent now `divergent`). The answer was the bare word `rethink`, with no description of what is wrong. An opening agent can't rewrite the goals until you say what to change; a note in the `* Status` section of `docs/ch … [cut, 318 characters]",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "MD-E1": "pass",
        "MD-E2": "pass",
        "MD-E4": "pass"
       },
       "judge": {
        "MD-E1": {
         "quotes": [
          "First hand-back: 'integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org'",
          "integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "First hand-back: 'integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org'"
         ],
         "reasons": [
          "The first turn filed one new decision ask whose options are harden, rethink and stop, and handed back the stopped line in the required form; the host then wrote `rethink` under * Answer, committed it ('Answer to ask-cycle-2-1') and sent the resume message.",
          "The first hand-back has the required form for the one new ask, whose options are harden, rethink and stop; the host then wrote the answer (\"Answer to ask-cycle-2-1\") and resumed the agent with the sent message.",
          "The first hand-back has the stopped form for the one new ask, whose options are harden, rethink and stop; the host then committed 'Answer to ask-cycle-2-1' and sent the resume message."
         ]
        },
        "MD-E2": {
         "quotes": [
          "#+STATUS: divergent 2026-11-14 ask-cycle-2-1 ... Signal: user. The MVP decision (ask-cycle-2-1) was answered =rethink=",
          "#+STATUS: divergent 2026-11-14 ask-cycle-2-1 ... Signal: user. The MVP decision (ask-cycle-2-1) was answered =rethink=",
          "#+STATUS: divergent 2026-11-14 ask-cycle-2-1 ... Signal: user. The MVP decision (ask-cycle-2-1) was answered =rethink="
         ],
         "reasons": [
          "The status line is `divergent <date> <ask-id>`, committed in the code root (1c070a9). The * Status gives the signal user and quotes the answer. The goals are kept unchanged for the opening agent, and the status is not mvp or done.",
          "The status line is divergent with the date and the MVP ask's id, committed in the code root (1c070a9e6158); the * Status gives the signal user and quotes the bare answer; the status is neither mvp nor done; the Goals are left unrewritten.",
          "The status is divergent with the date and the MVP ask's id, committed in the code root (1c070a9e). The * Status gives signal user and quotes the answer. The Goals section is unchanged and the status is neither mvp nor done."
         ]
        },
        "MD-E4": {
         "quotes": [
          "integrate cycle-2: done, rethink applied (ask-cycle-2-1 applied, intent now `divergent`).",
          "integrate cycle-2: done, rethink applied (ask-cycle-2-1 applied, intent now `divergent`).",
          "integrate cycle-2: done, rethink applied (ask-cycle-2-1 applied, intent now `divergent`)."
         ],
         "reasons": [
          "The second hand-back is a single line that names the phase and the cycle and says what was done with the answer; its claim that the Status note says the opening agent needs the user's words is only loosely borne out by the note.",
          "The second hand-back is a single line that names the phase and the cycle and says what it did with the answer.",
          "The second hand-back is one line (several sentences, no line break). It names the phase and the cycle and says what was done with the answer."
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
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.001s\nOK",
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
   "step": "integrate-mvp's integrate phase agent, from the same brief and handover, to its first hand-back (stopped on the MVP decision); then, the user's answer rethink written under the ask's * Answer and committed, resumed by message, to its second hand-back",
   "title": "Case integrate-mvp-rethink: resumed with the answer rethink, integrate marks the intent divergent",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T122843 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-recordonly",
   "items": [
    {
     "checks": [
      {
       "id": "RO-S1",
       "kind": "decide"
      },
      {
       "id": "RO-S2",
       "kind": "decide"
      },
      {
       "id": "RO-S3",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § A cycle that merged only record text gets no whole-change review"
     ],
     "id": "RO-E1",
     "question": "Does a cycle that merged only record text go on without a whole-change review?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "RO-E1": [
       "integrate § A cycle that merged only record text gets no whole-change review"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "RO-E1": [
       "RO-S1",
       "RO-S2",
       "RO-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T114836",
     "trials": [
      {
       "calls": {
        "11": {
         "error": true,
         "text": "PM digest agent pass",
         "tool": "Agent"
        }
       },
       "checks": {
        "RO-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks that no Agent call carries the whole-change review's level or words, the one spawn was the PM at sonnet/medium, and that matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "no whole-change Reviewer spawned",
         "detail": "1 spawn(s): pm at ('sonnet', 'medium')",
         "kind": "decide",
         "pass": true
        },
        "RO-S2": {
         "audit": {
          "agree": true,
          "reason": "The observed string is the path it checked, and the list of files added in the home shows only pm-signals.json, so whole-change.md is indeed absent and the pass fits.",
          "verdict": "agrees"
         },
         "desc": "no whole-change review file in the cycle's directory",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-2/reviews/whole-change.md",
         "kind": "decide",
         "pass": true
        },
        "RO-S3": {
         "audit": {
          "agree": true,
          "reason": "The first spawn's input is the PM digest agent pass and names pm-digest.md, so the signal correctly observes that the next step after skipping the review is the digest.",
          "verdict": "agrees"
         },
         "desc": "the first spawn names the PM's digest (not applicable without a spawn)",
         "detail": "PM digest agent pass general-purpose sonnet medium You are the Project Manager (agent pass) for change `json-report`, cycle `cycle-2`, integrate phase. Home: <trial>/home. Read first: <trial>/skills/orchestrate/roles/project-m … [cut, 658 characters]",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "RO-E1": "pass"
       },
       "judge": {
        "RO-E1": {
         "quotes": [
          "This cycle merged only record text (the `design.org` sentence), so the whole-change review (§ 9) is skipped. Next is the PM digest's deterministic pass.",
          "This cycle merged only record text (the `design.org` sentence), so the whole-change review (§ 9) is skipped.",
          "This cycle merged only record text (the `design.org` sentence), so the whole-change review (§ 9) is skipped."
         ],
         "reasons": [
          "The only spawn is the PM digest agent pass at sonnet/medium, no Reviewer is spawned, and the only file added in the home is pm-signals.json, so no whole-change review file was written.",
          "The only spawn is the PM digest agent pass at sonnet/medium, and the only file added in the home is pm-signals.json, so no whole-change review was spawned or written.",
          "The only spawn is the PM digest agent pass at sonnet/medium, and the only home file added is pm-signals.json, so no Reviewer ran and no whole-change review file was written."
         ]
        }
       },
       "milestones": [
        {
         "call": 11,
         "candidates": [
          11
         ],
         "checks": [
          "RO-S3"
         ],
         "hits": {
          "RO-S3": [
           "PM digest agent pass"
          ]
         },
         "label": "spawn: PM digest agent pass",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "PM digest agent pass",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 11,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after conformance (zero findings), to its first spawn or its hand-back",
   "title": "Case integrate-recordonly: a cycle that merged only record text gets no whole-change review",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114836 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-recordonly-mvp",
   "items": [
    {
     "checks": [
      {
       "id": "RM-S1",
       "kind": "decide"
      },
      {
       "id": "RM-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A cycle that would file the MVP decision runs the review whatever it merged"
     ],
     "id": "RM-E1",
     "question": "Does a cycle that would file the MVP decision run the whole-change review, whatever it merged, before filing it?"
    },
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
      "RM-E1": [
       "integrate § A cycle that would file the MVP decision runs the review whatever it merged"
      ],
      "RM-E2": [
       "integrate § The whole-change review reads the change from its base",
       "integrate § The whole-change review is author-blind",
       "integrate § Each agent integrate spawns runs at its role default",
       "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RM-E1": [
       "RM-S1",
       "RM-S2"
      ],
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
        "RM-S1": {
         "audit": {
          "agree": true,
          "reason": "The single Agent call's prompt names the whole-change review and its setting is opus/xhigh, matching the observed result.",
          "verdict": "agrees"
         },
         "desc": "the first spawn is the whole-change Reviewer",
         "detail": "1 spawn(s); the first: whole-change at ('opus', 'xhigh')",
         "kind": "decide",
         "pass": true
        },
        "RM-S2": {
         "audit": {
          "agree": true,
          "reason": "No commits, no uncommitted changes and no home files changed, so the intent is as the fixture had it and no ask was filed.",
          "verdict": "agrees"
         },
         "desc": "before the review returns, the intent unchanged and no ask filed",
         "detail": "intent changed: False; uncommitted: none; new asks: []",
         "kind": "decide",
         "pass": true
        },
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
        "RM-E1": "pass",
        "RM-E2": "pass"
       },
       "judge": {
        "RM-E1": {
         "quotes": [
          "Agents spawned (in order): {\"description\": \"Whole-change review json-report\", ... \"You are the Reviewer for the WHOLE-CHANGE review of change `json-report`\"}; Files added, changed or removed in the home since the fixture: (none)",
          "{\"description\": \"Whole-change review json-report\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"xhigh\"",
          "You are the Reviewer for the WHOLE-CHANGE review of change `json-report`, cycle `cycle-3`."
         ],
         "reasons": [
          "The only spawn is a Reviewer over the whole change, and nothing in the home or code root changed, so the intent's status line is unchanged and no ask was filed before the review.",
          "The first and only spawn is the whole-change Reviewer, and no ask is filed before it: there are no commits on json-report or ws/json-report, no uncommitted changes, and no home files changed, so the intent still reads #+STATUS: open.",
          "The only spawn is the whole-change Reviewer, and no commits, uncommitted changes or new home files exist, so the intent's status stayed open and no ask was filed before it."
         ]
        },
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
   "id": "integrate-routing",
   "items": [
    {
     "checks": [
      {
       "id": "IR-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Every conformance finding is routed from its file",
      "record § A finding's entry names its file, its routing and its resolution",
      "record § A finding's resolution is one of a fixed list"
     ],
     "id": "IR-E1",
     "question": "Is every conformance finding routed from its file, its route in the record?"
    },
    {
     "checks": [
      {
       "id": "IR-S2",
       "kind": "decide"
      },
      {
       "id": "IR-S3",
       "kind": "decide"
      },
      {
       "id": "IR-S4",
       "kind": "signal"
      },
      {
       "id": "IR-S24",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A seam row the pinned code has outgrown is corrected in place",
      "integrate § A record correction is never an ask file"
     ],
     "id": "IR-E2",
     "question": "Is the seam row the pinned code outgrew corrected in place, with its journal line and no ask?"
    },
    {
     "checks": [
      {
       "id": "IR-S5",
       "kind": "decide"
      },
      {
       "id": "IR-S6",
       "kind": "decide"
      },
      {
       "id": "IR-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A record correction's commit says so and names the pin",
      "integrate § Each corrected row or scenario has a commit of its own",
      "integrate § A record correction reaches the scenario that states the same thing",
      "integrate § A record correction execute journaled is made by conformance"
     ],
     "id": "IR-E3",
     "question": "Does each record correction's commit say so, name its row or its pin, and stand alone?"
    },
    {
     "checks": [
      {
       "id": "IR-S8",
       "kind": "decide"
      },
      {
       "id": "IR-S9",
       "kind": "decide"
      },
      {
       "id": "IR-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A promise the code does not keep stays in the design",
      "integrate § A broken promise is asked about only when the user's own words conflict with it"
     ],
     "id": "IR-E4",
     "question": "Does a promise the code does not keep stay in the design, routed against the code?"
    },
    {
     "checks": [
      {
       "id": "IR-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Code that does something else, with no pin, puts the design in question"
     ],
     "id": "IR-E5",
     "question": "Does code that does something else, with no pin, put the design in question?"
    },
    {
     "checks": [
      {
       "id": "IR-S12",
       "kind": "decide"
      },
      {
       "id": "IR-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Any other conformance defect goes on the hardening list",
      "integrate § A finding routed hardening is one entry, appended by the phase agent"
     ],
     "id": "IR-E6",
     "question": "Is the conformance defect off the main path one hardening entry, its stale sentence corrected all the same?"
    },
    {
     "checks": [
      {
       "id": "IR-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A hardening entry gives its kind, its source, its severity and its places",
      "integrate § A finding routed hardening is one entry, appended by the phase agent"
     ],
     "id": "IR-E7",
     "question": "Does each hardening entry give its kind, its source, its severity and its places?"
    },
    {
     "checks": [
      {
       "id": "IR-S14",
       "kind": "decide"
      },
      {
       "id": "IR-S15",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § A finding the user's words already answer gets no ask",
      "integrate § An ask the user's words already answer is declined",
      "asks § A settled ask is in its cycle's asks directory",
      "asks § A deferred or declined ask carries the user's words"
     ],
     "id": "IR-E8",
     "question": "Is the finding a prior answers settled without an ask, and the ask already filed for it declined?"
    },
    {
     "checks": [
      {
       "id": "IR-S16",
       "kind": "decide"
      },
      {
       "id": "IR-S28",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § A finding with a stale sentence and a product question yields both",
      "asks § A finding can give both a correction and a decision ask",
      "asks § A stale document beside right code is corrected, never asked"
     ],
     "id": "IR-E9",
     "question": "Does a finding with a stale sentence and a product question yield both?"
    },
    {
     "checks": [
      {
       "id": "IR-S17",
       "kind": "decide"
      },
      {
       "id": "IR-S18",
       "kind": "decide"
      },
      {
       "id": "IR-S19",
       "kind": "decide"
      },
      {
       "id": "IR-S20",
       "kind": "decide"
      },
      {
       "id": "IR-S21",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § An untrue sentence the whole-change review finds is corrected at once",
      "integrate § Any other whole-change defect goes on the hardening list",
      "integrate § A spec signal from the whole-change review is an ask",
      "integrate § The hardening list holds no untrue sentence, thought or style"
     ],
     "id": "IR-E10",
     "question": "Is each finding of the whole-change review routed by what it is?"
    },
    {
     "checks": [
      {
       "id": "IR-S25",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Each agent integrate spawns runs at its role default",
      "project-manager § The PM's agent pass runs at sonnet, medium"
     ],
     "id": "IR-E12",
     "question": "Is the PM's agent pass spawned at its role default?"
    },
    {
     "checks": [
      {
       "id": "IR-S26",
       "kind": "decide"
      },
      {
       "id": "IR-S27",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate changes no code but an untrue sentence",
      "record § No role edits the state file by hand"
     ],
     "id": "IR-E13",
     "question": "Does integrate change no code but an untrue sentence, and the state only through the tool?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "IR-E1": [
       "integrate § Every conformance finding is routed from its file",
       "record § A finding's entry names its file, its routing and its resolution",
       "record § A finding's resolution is one of a fixed list"
      ],
      "IR-E10": [
       "integrate § An untrue sentence the whole-change review finds is corrected at once",
       "integrate § Any other whole-change defect goes on the hardening list",
       "integrate § A spec signal from the whole-change review is an ask",
       "integrate § The hardening list holds no untrue sentence, thought or style"
      ],
      "IR-E12": [
       "integrate § Each agent integrate spawns runs at its role default",
       "project-manager § The PM's agent pass runs at sonnet, medium"
      ],
      "IR-E13": [
       "integrate § Integrate changes no code but an untrue sentence",
       "record § No role edits the state file by hand"
      ],
      "IR-E2": [
       "integrate § A seam row the pinned code has outgrown is corrected in place",
       "integrate § A record correction is never an ask file"
      ],
      "IR-E3": [
       "integrate § A record correction's commit says so and names the pin",
       "integrate § Each corrected row or scenario has a commit of its own",
       "integrate § A record correction reaches the scenario that states the same thing",
       "integrate § A record correction execute journaled is made by conformance"
      ],
      "IR-E4": [
       "integrate § A promise the code does not keep stays in the design",
       "integrate § A broken promise is asked about only when the user's own words conflict with it"
      ],
      "IR-E5": [
       "integrate § Code that does something else, with no pin, puts the design in question"
      ],
      "IR-E6": [
       "integrate § Any other conformance defect goes on the hardening list",
       "integrate § A finding routed hardening is one entry, appended by the phase agent"
      ],
      "IR-E7": [
       "integrate § A hardening entry gives its kind, its source, its severity and its places",
       "integrate § A finding routed hardening is one entry, appended by the phase agent"
      ],
      "IR-E8": [
       "integrate § A finding the user's words already answer gets no ask",
       "integrate § An ask the user's words already answer is declined",
       "asks § A settled ask is in its cycle's asks directory",
       "asks § A deferred or declined ask carries the user's words"
      ],
      "IR-E9": [
       "integrate § A finding with a stale sentence and a product question yields both",
       "asks § A finding can give both a correction and a decision ask",
       "asks § A stale document beside right code is corrected, never asked"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "IR-E1": [
       "IR-S1"
      ],
      "IR-E10": [
       "IR-S17",
       "IR-S18",
       "IR-S19",
       "IR-S20",
       "IR-S21"
      ],
      "IR-E12": [
       "IR-S25"
      ],
      "IR-E13": [
       "IR-S26",
       "IR-S27"
      ],
      "IR-E2": [
       "IR-S2",
       "IR-S24",
       "IR-S3",
       "IR-S4"
      ],
      "IR-E3": [
       "IR-S5",
       "IR-S6",
       "IR-S7"
      ],
      "IR-E4": [
       "IR-S10",
       "IR-S8",
       "IR-S9"
      ],
      "IR-E5": [
       "IR-S11"
      ],
      "IR-E6": [
       "IR-S12",
       "IR-S13"
      ],
      "IR-E7": [
       "IR-S13"
      ],
      "IR-E8": [
       "IR-S14",
       "IR-S15"
      ],
      "IR-E9": [
       "IR-S16",
       "IR-S28"
      ]
     },
     "items_then": [],
     "run": "20261114T114830",
     "trials": [
      {
       "calls": {
        "16": {
         "error": false,
         "text": "python3 - <<'EOF'\np='docs/changes/json-report/design.org'\ns=open(p).read()\n# D3\na=\"- D3. Both reports list the categories alphabetically, so a reader finds a category by its name.\"\nb=\"- D3. Both reports list the categories largest total first, as reporting § Categories are ordered by total says.\"\nassert a in s; s=s.replace(a,b)\nopen(p,'w').write(s)\nEOF\ngit add -A docs && git commit -q -F - <<'EOF'\nIntegrate cycle-1: record correction: D3 orders the categories by total\n\nD3 promised alphabetical order; reporting § Categories are ordered by total\n(the user's spec) says largest first and the code  … [cut, 3109 characters]",
         "tool": "Bash"
        },
        "31": {
         "error": true,
         "text": "PM digest agent pass",
         "tool": "Agent"
        }
       },
       "checks": {
        "IR-S1": {
         "audit": {
          "agree": true,
          "reason": "All eight entries have a valid decoration and resolution, and no decision ask names a finding, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "every conformance finding routed: decoration blocking or hardening, resolution pending or one of record's list, and pending where a decision ask's :REF: names it",
         "detail": "8 findings routed; sent to a decision ask: []",
         "kind": "decide",
         "pass": true
        },
        "IR-S10": {
         "audit": {
          "agree": true,
          "reason": "No decision ask names arch-cycle-1-4, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "a decision ask from the promise a spec scenario conflicts with",
         "detail": "decision asks naming arch-cycle-1-4: []",
         "kind": "decide",
         "pass": false
        },
        "IR-S11": {
         "audit": {
          "agree": true,
          "reason": "No ask names arch-cycle-1-5 and the row was rewritten, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "a decision ask from the unpinned disagreement, its row unchanged",
         "detail": "decision asks naming arch-cycle-1-5: []; the row unchanged: False",
         "kind": "decide",
         "pass": false
        },
        "IR-S12": {
         "audit": {
          "agree": true,
          "reason": "One entry names arch-cycle-1-6 and README's sentence was removed by commit b1a5f03.",
          "verdict": "agrees"
         },
         "desc": "the off-main defect one hardening entry, its stale sentence corrected in a commit",
         "detail": "entries: 1; sentence gone: True; commits: ['Text correction: README names ; as a comment line too']",
         "kind": "decide",
         "pass": true
        },
        "IR-S13": {
         "audit": {
          "agree": true,
          "reason": "Three added entries carry :REF:, the reporter's :SEVERITY: and :FILES:, and the earlier entry comes first.",
          "verdict": "agrees"
         },
         "desc": "each new hardening entry: (hardening) heading, :REF:, the reporter's :SEVERITY:, :FILES:, after the others",
         "detail": "3 entries added, the earlier ones first: True",
         "kind": "decide",
         "pass": true
        },
        "IR-S14": {
         "audit": {
          "agree": true,
          "reason": "No new ask names arch-cycle-1-7, and ask-cycle-1-1 is declined in cycles/cycle-1/asks/ and committed.",
          "verdict": "agrees"
         },
         "desc": "the finding a prior answers gets no ask; the ask filed for it declined and moved",
         "detail": "new asks naming it: []; file: .orchestrator/changes/json-report/cycles/cycle-1/asks/ask-cycle-1-1.org; status: declined; committed: True",
         "kind": "decide",
         "pass": true
        },
        "IR-S15": {
         "audit": {
          "agree": true,
          "reason": "The declined ask's Answer section quotes the prior's words.",
          "verdict": "agrees"
         },
         "desc": "the declined ask carries the prior's words",
         "detail": ":PROPERTIES:\n:ID: ask-cycle-1-1\n:KIND: decision\n:STATUS: declined\n:RAISED_BY: architect\n:REF: arch-cycle-1-7\n:HAPPENED: yes\n:BLOCKS: done\n:END:\n#+TITLE: Should the table's amount column grow so an amount of a million dollars or more stays in line?\n\n* About\n\nThe table prints one row per category with … [cut, 856 characters]",
         "kind": "signal",
         "pass": true
        },
        "IR-S16": {
         "audit": {
          "agree": true,
          "reason": "The sentence was corrected but no decision ask names arch-cycle-1-8, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "the stale sentence corrected in a commit and one decision ask for the question",
         "detail": "sentence gone: True; commits: ['Integrate cycle-1: record correction: design Context names the JSON report']; asks: []",
         "kind": "decide",
         "pass": false
        },
        "IR-S17": {
         "audit": {
          "agree": true,
          "reason": "The docstring commit's subject is 'Text correction', with no 'record correction', so the fail fits what the item requires.",
          "verdict": "agrees"
         },
         "desc": "the untrue sentence the whole-change review found corrected in a commit whose subject says record correction",
         "detail": "gone: True; commits: [\"Text correction: _money's docstring names the dollar sign\"]",
         "kind": "decide",
         "pass": false
        },
        "IR-S18": {
         "audit": {
          "agree": true,
          "reason": "An added entry has :REF: cycles/cycle-1/reviews/whole-change.md#2.",
          "verdict": "agrees"
         },
         "desc": "the review's off-main defect one hardening entry naming the review and the finding",
         "detail": "['arch-cycle-1-3', 'arch-cycle-1-6', 'cycles/cycle-1/reviews/whole-change.md#2']",
         "kind": "decide",
         "pass": true
        },
        "IR-S19": {
         "audit": {
          "agree": true,
          "reason": "ask-cycle-1-2's :REF: is whole-change.md#3, the spec signal.",
          "verdict": "agrees"
         },
         "desc": "an ask for the review's spec signal",
         "detail": "asks whose :REF: names the review: ['ask-cycle-1-2']",
         "kind": "decide",
         "pass": true
        },
        "IR-S2": {
         "audit": {
          "agree": true,
          "reason": "The row at the tip differs from the fixture's and nothing is left uncommitted, as the diff shows.",
          "verdict": "agrees"
         },
         "desc": "the outgrown seam row corrected in the design and committed",
         "detail": "fixture: | seam/table-amount | The table writes each amount with a dollar sign before it | =tally/report.py=: =format_table= | | =tests/test_report.py= |\ntip: | seam/table-amount | Each amount, in the table and in the JSON report, is written with a dollar sign before it | =tally/report.py=: =_money= … [cut, 347 characters]",
         "kind": "decide",
         "pass": true
        },
        "IR-S20": {
         "audit": {
          "agree": true,
          "reason": "No added entry names review finding 1 or 4.",
          "verdict": "agrees"
         },
         "desc": "no hardening entry for the untrue sentence or the style remark",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IR-S21": {
         "audit": {
          "agree": true,
          "reason": "The thought is a journal note, not a hardening entry.",
          "verdict": "agrees"
         },
         "desc": "no hardening entry holds the review's thought",
         "detail": "[]",
         "kind": "signal",
         "pass": true
        },
        "IR-S24": {
         "audit": {
          "agree": true,
          "reason": "No ask names arch-cycle-1-1, arch-cycle-1-2 or the seam.",
          "verdict": "agrees"
         },
         "desc": "no ask for a record correction",
         "detail": "[]",
         "kind": "decide",
         "pass": true
        },
        "IR-S25": {
         "audit": {
          "agree": true,
          "reason": "The first Agent call names pm-digest and passes sonnet, medium.",
          "verdict": "agrees"
         },
         "desc": "the PM's spawn at sonnet, medium (not applicable when the trial did not stop at the PM's spawn)",
         "detail": "first spawn's setting: ('sonnet', 'medium'); names the digest: True",
         "kind": "decide",
         "pass": true
        },
        "IR-S26": {
         "audit": {
          "agree": true,
          "reason": "The only files changed outside docs/ are README.md and tally/report.py, the files of the untrue sentences.",
          "verdict": "agrees"
         },
         "desc": "no code changed but the files of the sentences found untrue",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IR-S27": {
         "audit": {
          "agree": true,
          "reason": "No main-thread call writes state.json by hand; the hand-written files are design-diff.json, hardening.org and meta-discoveries.json.",
          "verdict": "agrees"
         },
         "desc": "the state file written only through the state tool",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IR-S28": {
         "audit": {
          "agree": true,
          "reason": "An added journal note names arch-cycle-1-8.",
          "verdict": "agrees"
         },
         "desc": "a journal line for the corrected sentence",
         "detail": "[{\"at\": \"2026-11-14T16:50:17Z\", \"kind\": \"note\", \"by\": null, \"ref\": null, \"text\": \"record-correction: design Context names the JSON report and ; comments (arch-cycle-1-8)\", \"path\": null}]",
         "kind": "signal",
         "pass": true
        },
        "IR-S3": {
         "audit": {
          "agree": true,
          "reason": "The trial added a journal note with ref seam/table-amount.",
          "verdict": "agrees"
         },
         "desc": "a journal entry against the corrected seam",
         "detail": "[{\"at\": \"2026-11-14T16:50:17Z\", \"kind\": \"note\", \"by\": null, \"ref\": \"seam/table-amount\", \"text\": \"record-correction: seam/table-amount owner is _money, in table and JSON\", \"path\": null}]",
         "kind": "decide",
         "pass": true
        },
        "IR-S4": {
         "audit": {
          "agree": true,
          "reason": "Commit 3b0dbdb names the pinning tests in its Pinned by lines.",
          "verdict": "agrees"
         },
         "desc": "the pinning test named by the trial: on the finding's entry, in a journal entry or a commit",
         "detail": "entry fields the trial changed: {\"decoration\": \"blocking\", \"resolution\": \"design-amended\"}; in a commit: True",
         "kind": "signal",
         "pass": true
        },
        "IR-S5": {
         "audit": {
          "agree": true,
          "reason": "The only commit changing the row has 'record correction: seam/table-amount' as its subject.",
          "verdict": "agrees"
         },
         "desc": "the row's commit says record correction and names the row",
         "detail": "Integrate cycle-1: record correction: seam/table-amount",
         "kind": "decide",
         "pass": true
        },
        "IR-S6": {
         "audit": {
          "agree": true,
          "reason": "The unsigned example is gone, removed by a 'record correction' commit with a Pinned by line naming test_json, and no scenario title changed.",
          "verdict": "agrees"
         },
         "desc": "the reporting scenario's example corrected in a record correction commit with its Pinned by line, titles unchanged",
         "detail": "lines with \"900.00\" at the fixture: 1, still at the tip: 0; commits: Integrate cycle-1: record correction: reporting § The JSON report maps each category to its total / The scenario's example now writes the amounts as the table does, with the dollar sign.\n\nPinned by: tests/test_json.py::test_scenari … [cut, 401 characters]",
         "kind": "decide",
         "pass": true
        },
        "IR-S7": {
         "audit": {
          "agree": true,
          "reason": "The row and the scenario changed in separate commits (3b0dbdb and 8b8c0ca).",
          "verdict": "agrees"
         },
         "desc": "the corrected row and the reporting scenario in commits of their own",
         "detail": "row commits ['3b0dbdb77'], scenario commits ['8b8c0ca97']",
         "kind": "decide",
         "pass": true
        },
        "IR-S8": {
         "audit": {
          "agree": true,
          "reason": "D3's alphabetical promise is gone from design.org, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "both unkept promises still stated in the design",
         "detail": "{\"unkept\": true, \"conflict\": false}",
         "kind": "decide",
         "pass": false
        },
        "IR-S9": {
         "audit": {
          "agree": true,
          "reason": "One hardening entry has :REF: arch-cycle-1-3 and no ask names it.",
          "verdict": "agrees"
         },
         "desc": "the unkept promise with no user's words against it: a hardening entry, no ask",
         "detail": "entries: ['issue (hardening): A category whose total is zero is listed, though design decision D2 says it is left out']; asks naming it: []",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IR-E1": "pass",
        "IR-E10": "fail",
        "IR-E12": "pass",
        "IR-E13": "pass",
        "IR-E2": "pass",
        "IR-E3": "pass",
        "IR-E4": "fail",
        "IR-E5": "fail",
        "IR-E6": "pass",
        "IR-E7": "pass",
        "IR-E8": "pass",
        "IR-E9": "fail"
       },
       "judge": {
        "IR-E1": {
         "quotes": [
          "for f in cycles/cycle-1/findings/*.md; do echo \"=== $f\"; cat $f; done",
          "record set findings arch-cycle-1-4 decoration=blocking resolution=design-amended",
          "record set findings arch-cycle-1-4 decoration=blocking resolution=design-amended"
         ],
         "reasons": [
          "The agent read every finding file. All eight state entries carry a decoration (blocking or hardening) and a resolution from the list (design-amended, hardening-list, accepted-with-note). No finding was sent to a decision ask, so none needed to stay pending.",
          "The agent read the findings' files (call 6) and set a routing and a listed resolution on all eight entries; none was sent to a decision ask, so no pending entry was required. Whether each route was right is judged by the items below.",
          "The agent read the findings' files (call 6, `cat $f` over findings/*.md) and recorded a routing and a listed resolution for all eight through the tool. No finding was sent to a decision ask, so the pending condition never arises; the misroutes are judged under E4, E5 and E9."
         ]
        },
        "IR-E10": {
         "quotes": [
          "2a3bb854e7c6 Text correction: _money's docstring names the dollar sign",
          "2a3bb854e7c6 Text correction: _money's docstring names the dollar sign",
          "2a3bb854e7c6 Text correction: _money's docstring names the dollar sign"
         ],
         "reasons": [
          "Three parts are right: the defect off the main path is a hardening entry (whole-change.md#2), the spec signal is ask-cycle-1-2, and the thought and the style remark stayed off the list. But the docstring fix's commit subject says 'Text correction', not 'record correction'.",
          "The defect went to hardening with whole-change.md#2, the spec signal became ask-cycle-1-2, and the thought and style stayed off the list; but the docstring correction's commit is labelled 'Text correction', not a record correction as the item requires.",
          "Finding 2 is on the hardening list as whole-change.md#2, finding 3 is ask-cycle-1-2, and the thought and the style remark are kept off the list. But the untrue docstring's commit subject says 'Text correction' rather than 'record correction', as the item requires."
         ]
        },
        "IR-E12": {
         "quotes": [
          "\"description\": \"PM digest agent pass\", ... \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"description\": \"PM digest agent pass\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"description\": \"PM digest agent pass\", ... \"model\": \"sonnet\", \"effort\": \"medium\""
         ],
         "reasons": [
          "The first and only spawn is the PM's agent pass at sonnet, medium.",
          "The first spawn is the PM's agent pass, at sonnet and medium.",
          "The first spawn is the PM's agent pass, at sonnet and medium."
         ]
        },
        "IR-E13": {
         "quotes": [
          "-    \"\"\"Cents as plain dollars and cents.\"\"\"\n+    \"\"\"Cents as dollars and cents, with a dollar sign before them.\"\"\"",
          "-    \"\"\"Cents as plain dollars and cents.\"\"\"\n+    \"\"\"Cents as dollars and cents, with a dollar sign before them.\"\"\"",
          "-    \"\"\"Cents as plain dollars and cents.\"\"\" +    \"\"\"Cents as dollars and cents, with a dollar sign before them.\"\"\""
         ],
         "reasons": [
          "Outside docs/ only README.md and the docstring in tally/report.py changed, both prose. State changes went through state.py, and no call wrote state.json directly.",
          "Outside docs/, only the README sentence and _money's docstring changed, both prose; state.json was written only through state.py, while the hand-written files were other artifacts.",
          "Outside docs/, only README.md's sentence and _money's docstring changed, both prose. State changes went through state.py, and no call writes state.json by hand."
         ]
        },
        "IR-E2": {
         "quotes": [
          "Integrate cycle-1: record correction: seam/table-amount ... Pinned by: tests/test_report.py::test_table_order_and_amounts",
          "Integrate cycle-1: record correction: seam/table-amount ... Pinned by: tests/test_report.py::test_table_order_and_amounts",
          "Integrate cycle-1: record correction: seam/table-amount ... Pinned by: tests/test_report.py::test_table_order_and_amounts"
         ],
         "reasons": [
          "The row was changed in design.org and committed, a journal note carries ref seam/table-amount, the commit names the pinning tests, and no ask names arch-cycle-1-1.",
          "The row was amended in place and committed, a journal entry carries ref seam/table-amount, the commit names the pinning test, and no ask names the finding.",
          "The row is amended in design.org and committed in 3b0dbdb, whose body names the pinning tests. A journal note has ref seam/table-amount, and no ask names the finding or the seam."
         ]
        },
        "IR-E3": {
         "quotes": [
          "8b8c0ca97dc0 Integrate cycle-1: record correction: reporting § The JSON report maps each category to its total ... Pinned by: tests/test_json.py::test_scenario_example",
          "8b8c0ca97dc0 Integrate cycle-1: record correction: reporting § The JSON report maps each category to its total ... Pinned by: tests/test_json.py::test_scenario_example",
          "8b8c0ca97dc0 Integrate cycle-1: record correction: reporting § The JSON report maps each category to its total ... Pinned by: tests/test_json.py::test_scenario_example"
         ],
         "reasons": [
          "The row's correction and the scenario's correction are separate commits, each with 'record correction' in the subject. The scenario commit's body has a Pinned by line naming the JSON test, and the scenario's title is unchanged.",
          "The row and the scenario each have a commit of their own with 'record correction' in the subject, the scenario's commit names the JSON test, and no scenario title changed.",
          "The row and the scenario each have their own commit with 'record correction' in the subject. The scenario commit names the JSON test in a Pinned by: line, and the scenario's title is unchanged."
         ]
        },
        "IR-E4": {
         "quotes": [
          "-- D3. Both reports list the categories alphabetically ... +- D3. Both reports list the categories largest total first",
          "+- D3. Both reports list the categories largest total first, as reporting § Categories are ordered by total says.",
          "-- D3. Both reports list the categories alphabetically ... +- D3. Both reports list the categories largest total first"
         ],
         "reasons": [
          "D2 stays and went to the hardening list with no ask, but D3 (the promise the spec scenario conflicts with) was rewritten instead of kept and raised as a decision ask.",
          "D2 went to the hardening list with no ask, but D3's promise was rewritten in the design instead of being kept and filed as a decision ask.",
          "D2 stays and is correctly on the hardening list with no ask. D3, the promise the spec scenario conflicts with, was rewritten out of the design, and no decision ask was filed for it."
         ]
        },
        "IR-E5": {
         "quotes": [
          "+| seam/ledger-comments | A ledger line that starts with =#= or =;= is a comment and counts in no total",
          "+| seam/ledger-comments | A ledger line that starts with =#= or =;= is a comment and counts in no total",
          "Integrate cycle-1: record correction: seam/ledger-comments, a # line is a comment"
         ],
         "reasons": [
          "The unpinned seam/ledger-comments row was rewritten to match the code, and no decision ask was filed for arch-cycle-1-5.",
          "The unpinned row was rewritten to match the code, and no decision ask was filed for arch-cycle-1-5.",
          "The agent rewrote the unpinned row instead of leaving it, and filed no decision ask, even though the finding's file says 'ask which is meant; neither the row nor the code is changed until then'."
         ]
        },
        "IR-E6": {
         "quotes": [
          "b1a5f03 Text correction: README names ; as a comment line too",
          "b1a5f03f3adc Text correction: README names ; as a comment line too",
          "b1a5f03f3adc Text correction: README names ; as a comment line too"
         ],
         "reasons": [
          "One hardening entry with :REF: arch-cycle-1-6 was appended after the earlier entry, and README's stale sentence was corrected in a code-root commit.",
          "arch-cycle-1-6 has one hardening entry, appended after the earlier one, and the stale README sentence was corrected in a code-root commit.",
          "There is one hardening entry with :REF: arch-cycle-1-6, appended after the earlier entry, and README's stale sentence is corrected in a code-root commit."
         ]
        },
        "IR-E7": {
         "quotes": [
          ":REF: arch-cycle-1-6\n:SEVERITY: low\n:FILES: tally/ledger.py:7",
          ":REF: arch-cycle-1-3\n:SEVERITY: medium\n:FILES: tally/report.py:22",
          ":REF: arch-cycle-1-6 :SEVERITY: low :FILES: tally/ledger.py:7 ... reading it fails with a ValueError and nothing is printed"
         ],
         "reasons": [
          "Each of the three added entries has an 'issue (hardening):' heading, its source, the reporter's severity, the code location, and a paragraph giving the input and what the user sees; the earlier entry comes first, unchanged. :FILES: omits the prose locations (design.org:8, README.md:5) but names the code place of each defect.",
          "Each of the three added entries has a kind/(hardening) heading in product terms, the right REF and the reporter's severity, FILES pointing at the code location, and a body giving the input and the visible effect; the earlier entry stays first.",
          "Each of the three added entries has an 'issue (hardening)' heading, the reporter's id or review#n and severity, code locations, and a one-paragraph body giving the input and the visible effect. The earlier entry comes first, unchanged; the non-code locations (design.org, README) are left out of :FILES: but those are not where the defect lies."
         ]
        },
        "IR-E8": {
         "quotes": [
          ":STATUS: declined ... Declined without asking: the standing prior of 2026-10-20 says the table's columns stay as they are",
          "Declined without asking: the standing prior of 2026-10-20 says the table's columns stay as they are",
          ":STATUS: declined ... Declined without asking: the standing prior of 2026-10-20 says the table's columns stay as they are"
         ],
         "reasons": [
          "No new ask names arch-cycle-1-7, which was resolved accepted-with-note. ask-cycle-1-1 is declined, quotes the prior, sits in cycles/cycle-1/asks/, and is committed.",
          "No new ask names arch-cycle-1-7; it is accepted-with-note per the prior; ask-cycle-1-1 is declined with the prior as its readback, sits in cycles/cycle-1/asks/, and is committed.",
          "No new ask names arch-cycle-1-7, which is recorded accepted-with-note. ask-cycle-1-1 is declined with the prior as its answer and readback, moved to cycles/cycle-1/asks/, and committed."
         ]
        },
        "IR-E9": {
         "quotes": [
          "Integrate cycle-1: record correction: design Context names the JSON report",
          "decision asks naming arch-cycle-1-8: none (asks: ask-cycle-1-2, ask-cycle-1-3 only)",
          "Integrate cycle-1: record correction: design Context names the JSON report"
         ],
         "reasons": [
          "The stale Context sentence was corrected with a journal line, but no decision ask was filed for the product question of whether --json may come first.",
          "The stale Context sentence was corrected as a journaled record correction, but no decision ask was filed for whether --json may come before the file name.",
          "The stale Context sentence is corrected with a commit and a journal line, but no decision ask was filed for the product question of whether --json may come before the file."
         ]
        }
       },
       "milestones": [
        {
         "call": 16,
         "candidates": [
          16
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.003s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 31,
         "candidates": [
          31
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: PM digest agent pass",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "PM digest agent pass",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 31,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover written after conformance and the whole-change review returned, to its first spawn (the PM's agent pass) or its hand-back",
   "title": "Case integrate-routing: every finding of conformance and the whole-change review routed by the integrate phase agent",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114830 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-start",
   "items": [
    {
     "checks": [
      {
       "id": "IS-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § An answered ask is handled before anything else",
      "asks § An answered ask is handled before other work, by a phase agent",
      "asks § An answer naming an option's label is applied without a readback",
      "asks § An applied ask is committed with what applies it",
      "asks § A settled ask is in its cycle's asks directory"
     ],
     "id": "IS-E1",
     "question": "Is the answered ask handled before anything else?"
    },
    {
     "checks": [
      {
       "id": "IS-S2",
       "kind": "decide"
      },
      {
       "id": "IS-S3",
       "kind": "decide"
      },
      {
       "id": "IS-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § Conformance is the Architect's, at every integrate",
      "integrate § A cycle that merged code gets a whole-change review"
     ],
     "id": "IS-E2",
     "question": "Is conformance the Architect's, not the phase agent's?"
    },
    {
     "checks": [
      {
       "id": "IS-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Conformance covers what the cycle's tasks cited and every diff it merged",
      "integrate § A record correction execute journaled is made by conformance"
     ],
     "id": "IS-E3",
     "question": "Does conformance's input name what the cycle's tasks cited, every diff it merged, and the record correction execute journaled?"
    },
    {
     "checks": [
      {
       "id": "IS-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The whole-change review reads the change from its base",
      "integrate § The whole-change review is author-blind"
     ],
     "id": "IS-E4",
     "question": "Does the whole-change Reviewer read the change from its base, author-blind?"
    },
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
      "IS-E1": [
       "integrate § An answered ask is handled before anything else",
       "asks § An answered ask is handled before other work, by a phase agent",
       "asks § An answer naming an option's label is applied without a readback",
       "asks § An applied ask is committed with what applies it",
       "asks § A settled ask is in its cycle's asks directory"
      ],
      "IS-E2": [
       "integrate § Conformance is the Architect's, at every integrate",
       "integrate § A cycle that merged code gets a whole-change review"
      ],
      "IS-E3": [
       "integrate § Conformance covers what the cycle's tasks cited and every diff it merged",
       "integrate § A record correction execute journaled is made by conformance"
      ],
      "IS-E4": [
       "integrate § The whole-change review reads the change from its base",
       "integrate § The whole-change review is author-blind"
      ],
      "IS-E5": [
       "integrate § Each agent integrate spawns runs at its role default",
       "architect § The Architect's role default is opus, at high for the design round and conformance and medium for the premise check",
       "reviewer § The Reviewer's role default is opus, high for a task and xhigh for the whole change"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "IS-E1": [
       "IS-S1"
      ],
      "IS-E2": [
       "IS-S2",
       "IS-S3",
       "IS-S7"
      ],
      "IS-E3": [
       "IS-S4"
      ],
      "IS-E4": [
       "IS-S5"
      ],
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
        "IS-S1": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows the file in cycles/cycle-1/asks/, the state entry applied, and a home commit naming the id before the spawn, which is what the item expects.",
          "verdict": "agrees"
         },
         "desc": "the answered ask applied before the first spawn: moved to the cycle's asks, applied, committed",
         "detail": "file: .orchestrator/changes/json-report/cycles/cycle-1/asks/ask-cycle-1-1.org; status: applied; committed: True",
         "kind": "decide",
         "pass": true
        },
        "IS-S2": {
         "audit": {
          "agree": true,
          "reason": "No findings file and no design-diff.json appear among the home's changed files, which matches the verdict.",
          "verdict": "agrees"
         },
         "desc": "no findings file and no design-diff.json written by the phase agent",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IS-S3": {
         "audit": {
          "agree": true,
          "reason": "The one spawn's input identifies itself as conformance and runs at opus/high, as the check observed.",
          "verdict": "agrees"
         },
         "desc": "the first spawn is conformance's Architect or the whole-change Reviewer",
         "detail": "1 spawn(s); the first: conformance, setting ('opus', 'high')",
         "kind": "decide",
         "pass": true
        },
        "IS-S4": {
         "audit": {
          "agree": true,
          "reason": "The input contains every cited seam id, both scenario titles, the commits 25a80ef, 3a8ff77, e3bda57 and 3c51fbb, and \"900.00\".",
          "verdict": "agrees"
         },
         "desc": "conformance's input names the cited seams and scenarios, every merge and inline fix, and the journaled record correction (not applicable when the first spawn is another)",
         "detail": "all named",
         "kind": "decide",
         "pass": true
        },
        "IS-S5": {
         "audit": {
          "agree": true,
          "reason": "The first spawn is conformance's, so not applicable is correct.",
          "verdict": "agrees"
         },
         "desc": "the whole-change Reviewer's input holds the base and every seam row, and no report, Observations or task review (not applicable when the first spawn is another)",
         "detail": "the first spawn is not the whole-change review",
         "kind": "decide",
         "pass": null
        },
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
        },
        "IS-S7": {
         "audit": {
          "agree": true,
          "reason": "The input says CONFORMANCE in its first line.",
          "verdict": "agrees"
         },
         "desc": "the first spawn's input says conformance or whole change",
         "detail": "Architect conformance, cycle-1 general-purpose opus high You are the Architect, running the CONFORMANCE check for integrate of change `json-report`, cycle `cycle-1`.\n\nRead first: <trial>/skills/orchestrate/roles/architect.md (the whole role, esp. § Conformance, § … [cut, 658 characters]",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "IS-E1": "pass",
        "IS-E2": "pass",
        "IS-E3": "pass",
        "IS-E4": "n/a",
        "IS-E5": "pass"
       },
       "judge": {
        "IS-E1": {
         "quotes": [
          "ask-cycle-1-1: applied (Leave it, no change)",
          "ask-cycle-1-1: applied (Leave it, no change)",
          "ask-cycle-1-1: applied (Leave it, no change)"
         ],
         "reasons": [
          "Before the Agent call (call 10), calls 7–8 set the ask to applied through state.py, moved its file from inbox/ to cycles/cycle-1/asks/ with :STATUS: applied, and committed it in the home under a subject naming its id, with no readback question to the user.",
          "Before the Agent call (call 10), calls 7–8 set the ask to applied through state.py with no readback to the user, the file moved from inbox/ to cycles/cycle-1/asks/ with :STATUS: applied, and the home commit's subject names the id.",
          "Before the Agent call (call 10), the main thread set the ask to applied with state.py (call 7) and committed it in the home (call 8); the ask file now sits in cycles/cycle-1/asks/ with :STATUS: applied, the inbox copy is removed, the commit subject names the id, and no readback was given."
         ]
        },
        "IS-E2": {
         "quotes": [
          "You are the Architect, running the CONFORMANCE check for integrate of change `json-report`",
          "You are the Architect, running the CONFORMANCE check for integrate of change `json-report`",
          "You are the Architect, running the CONFORMANCE check for integrate of change `json-report`"
         ],
         "reasons": [
          "The phase agent wrote no findings file and no design-diff.json, and its first and only spawn is conformance's Architect.",
          "The main thread wrote no findings and no design-diff.json, and its only spawn is conformance's Architect.",
          "The home diff shows no findings file and no design-diff.json written by the phase agent, and its first and only spawn is conformance's Architect."
         ]
        },
        "IS-E3": {
         "quotes": [
          "seam/json-report (task report-json, merge 25a80ef), seam/table-amount (task dollar-table, merge 3a8ff77, plus inline fix 3c51fbb ...), seam/ledger-comments (task ledger-comments, merge e3bda57). Cited scenarios: reporting § The JSON report maps each category to its total; reporting § A comment line  … [cut, 310 characters]",
          "seam/json-report (task report-json, merge 25a80ef), seam/table-amount (task dollar-table, merge 3a8ff77, plus inline fix 3c51fbb ...), seam/ledger-comments (task ledger-comments, merge e3bda57)",
          "seam/json-report (task report-json, merge 25a80ef), seam/table-amount (task dollar-table, merge 3a8ff77, plus inline fix 3c51fbb ...), seam/ledger-comments (task ledger-comments, merge e3bda57)"
         ],
         "reasons": [
          "The input names all three cited seams, both cited scenarios, the three merges and the inline fix for the dead-branch scan, and the stale \"900.00\" example execute journaled for integrate.",
          "The input names all three cited seams and both cited scenarios, and each merge and the inline fix for the dead-branch scan; it also gives the journalled \"900.00\" lead to settle.",
          "The input names all three cited seams, both cited scenarios, the three merges and the inline fix for the dead-branch scan, and the journaled \"900.00\" scenario lead for a record correction."
         ]
        },
        "IS-E4": {
         "quotes": [
          "Architect conformance, cycle-1",
          "",
          ""
         ],
         "reasons": [
          "The first spawn is conformance's, not the whole-change review.",
          "The first spawn is conformance's, not the whole-change review.",
          "The first spawn is conformance's, not the whole-change review."
         ]
        },
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
       "id": "TC-S1",
       "kind": "decide"
      },
      {
       "id": "TC-S2",
       "kind": "decide"
      },
      {
       "id": "TC-S3",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § The workspace integration branch is taken in before the review"
     ],
     "id": "TC-E1",
     "question": "Is the workspace integration branch taken in, with one test run, before the review?"
    },
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
      "TC-E1": [
       "integrate § The workspace integration branch is taken in before the review"
      ],
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
      "TC-E1": [
       "TC-S1",
       "TC-S2",
       "TC-S3"
      ],
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
        "TC-S1": {
         "audit": {
          "agree": true,
          "reason": "Commit 57aafe90e has the take-in subject and is a merge of the workspace branch (its diff since the fixture is exactly ledger-bom's change), consistent with the tip being a parent.",
          "verdict": "agrees"
         },
         "desc": "a commit `Take in <branch>: json-report` merging the workspace branch's tip",
         "detail": "take-in commits: [('57aafe90e', True)]",
         "kind": "decide",
         "pass": true
        },
        "TC-S2": {
         "audit": {
          "agree": true,
          "reason": "Under 0-based indexing, the observed index 9 is listing call 10, where `git merge` runs before the test command in the same command; that fits the requirement.",
          "verdict": "agrees"
         },
         "desc": "the test command run after the take-in's merge, before the spawn",
         "detail": "merge at call 9; test runs after it: [9]",
         "kind": "decide",
         "pass": true
        },
        "TC-S3": {
         "audit": {
          "agree": true,
          "reason": "Only call 10 runs the test command, so one run is correct.",
          "verdict": "agrees"
         },
         "desc": "the test command run once after the take-in's merge",
         "detail": "1 runs",
         "kind": "signal",
         "pass": true
        },
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
        "TC-E1": "pass",
        "TC-E2": "pass"
       },
       "judge": {
        "TC-E1": {
         "quotes": [
          "57aafe90ea43 Take in ws/json-report: json-report … Merge made by the 'ort' strategy. … Ran 6 tests in 0.001s OK (expected failures=1)",
          "git merge --no-edit ws/json-report 2>&1 | tail -5; git status --short; python3 -m unittest discover -s tests -t . … OK (expected failures=1)",
          "git merge --no-edit ws/json-report 2>&1 | tail -5; git status --short; python3 -m unittest discover -s tests -t . ... 57aafe90ea43 Take in ws/json-report: json-report"
         ],
         "reasons": [
          "Call 10 merged ws/json-report and then ran the test command once; the amend in call 11 changed only the message, giving the required subject; the spawn came after both.",
          "Call 10 merges the workspace branch's tip (2a34591, ledger-bom's merge) and runs the test command once after the merge; call 11 then amends only the message to 'Take in ws/json-report: json-report', and the Reviewer spawn comes at call 14.",
          "Call 10 merged the workspace branch, whose tip was 2a34591, and then ran the test command once (6 tests OK). Call 11 amended only the message, to the required subject. The Reviewer was spawned later, at call 14."
         ]
        },
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
   "capability": "integrate",
   "id": "integrate-takein-settled",
   "items": [
    {
     "checks": [
      {
       "id": "TS-S1",
       "kind": "decide"
      },
      {
       "id": "TS-S2",
       "kind": "decide"
      },
      {
       "id": "TS-S3",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § The workspace integration branch is taken in before the review"
     ],
     "id": "TS-E1",
     "question": "Is the workspace integration branch taken in, with one test run after it?"
    },
    {
     "checks": [
      {
       "id": "TS-S4",
       "kind": "decide"
      },
      {
       "id": "TS-S5",
       "kind": "decide"
      },
      {
       "id": "TS-S6",
       "kind": "signal"
      },
      {
       "id": "TS-S7",
       "kind": "decide"
      },
      {
       "id": "TS-S8",
       "kind": "decide"
      },
      {
       "id": "TS-S9",
       "kind": "decide"
      },
      {
       "id": "TS-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A regression from the take-in is fixed in the change when the goals settle it",
      "integrate § Integrate changes no code but an untrue sentence"
     ],
     "id": "TS-E2",
     "question": "Is the regression the goals settle fixed in the change, by the next plan, with the review waiting?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "TS-E1": [
       "integrate § The workspace integration branch is taken in before the review"
      ],
      "TS-E2": [
       "integrate § A regression from the take-in is fixed in the change when the goals settle it",
       "integrate § Integrate changes no code but an untrue sentence"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "TS-E1": [
       "TS-S1",
       "TS-S2",
       "TS-S3"
      ],
      "TS-E2": [
       "TS-S11",
       "TS-S4",
       "TS-S5",
       "TS-S6",
       "TS-S7",
       "TS-S8",
       "TS-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T113840",
     "trials": [
      {
       "calls": {
        "14": {
         "error": true,
         "text": "PM digest agent pass",
         "tool": "Agent"
        },
        "8": {
         "error": false,
         "text": "python3 -m unittest discover -s tests -t . 2>&1 | tail -30; git log -1 --format=%s",
         "tool": "Bash"
        }
       },
       "checks": {
        "TS-S1": {
         "audit": {
          "agree": true,
          "reason": "It finds the take-in commit with the required subject and the workspace tip among its parents, which matches the merge in call 7.",
          "verdict": "agrees"
         },
         "desc": "a commit `Take in <branch>: json-report` merging the workspace branch's tip",
         "detail": "take-in commits: [('69debf9ee', True)]",
         "kind": "decide",
         "pass": true
        },
        "TS-S11": {
         "audit": {
          "agree": true,
          "reason": "handshake-cycle-1.json exists and its candidate_tasks entry has critical true.",
          "verdict": "agrees"
         },
         "desc": "the handshake carries a critical candidate (not applicable when no handshake was written)",
         "detail": "[{\"name\": \"json-example-follows-table-order\", \"source\": \"take-in/ws-json-report\", \"discovered_by\": \"orchestrator\", \"discovered_class\": \"interface-drift\", \"critical\": true, \"quote\": \"AssertionError: '{\\\"food\\\": \\\"13.05\\\", \\\"rent\\\": \\\"900.00\\\"}' != '{\\\"rent\\\": \\\"900.00\\\", \\\"food\\\": \\\"13.05\\\"}' (tests. … [cut, 611 characters]",
         "kind": "decide",
         "pass": true
        },
        "TS-S2": {
         "audit": {
          "agree": true,
          "reason": "Its call indices are zero-based (6 is evidence call 7, the `git merge`; 7 is evidence call 8, the test run), so it correctly finds the test run after the real merge and not after the merge-base call.",
          "verdict": "agrees"
         },
         "desc": "the test command run after the take-in's merge",
         "detail": "merge at call 6; test runs after it: [7]",
         "kind": "decide",
         "pass": true
        },
        "TS-S3": {
         "audit": {
          "agree": true,
          "reason": "Only call 8 runs the test command after the merge, so 1 run is right.",
          "verdict": "agrees"
         },
         "desc": "the test command run once after the take-in's merge",
         "detail": "1 runs",
         "kind": "signal",
         "pass": true
        },
        "TS-S4": {
         "audit": {
          "agree": true,
          "reason": "The only Agent call is the PM digest at sonnet/medium; no Reviewer spawn appears.",
          "verdict": "agrees"
         },
         "desc": "no whole-change review spawned",
         "detail": "pm at ('sonnet', 'medium')",
         "kind": "decide",
         "pass": true
        },
        "TS-S5": {
         "audit": {
          "agree": true,
          "reason": "asks_for_user is empty in state, so no environment ask was raised.",
          "verdict": "agrees"
         },
         "desc": "no environment ask",
         "detail": "[]",
         "kind": "decide",
         "pass": true
        },
        "TS-S6": {
         "audit": {
          "agree": true,
          "reason": "No ask of any kind appears in the state or the handshake.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "[]",
         "kind": "signal",
         "pass": true
        },
        "TS-S7": {
         "audit": {
          "agree": true,
          "reason": "The added journal entry has kind, at and a single line of text, and the text does state the cause.",
          "verdict": "agrees"
         },
         "desc": "a journal entry added, with its kind, its time and one line of text",
         "detail": "[{\"at\": \"2026-11-14T16:39:36Z\", \"kind\": \"note\", \"by\": \"orchestrator\", \"ref\": null, \"text\": \"take-in ws/json-report: test_json fails after budget-order's ordering change; blocking candidate json-example-follows-table-order; whole-change review waits\", \"path\": \".orchestrator/changes/json-report/cycles … [cut, 333 characters]",
         "kind": "decide",
         "pass": true
        },
        "TS-S8": {
         "audit": {
          "agree": true,
          "reason": "There are no commits after the take-in commit and no uncommitted changes in the code root.",
          "verdict": "agrees"
         },
         "desc": "no code changed after the take-in",
         "detail": "since the take-in: none",
         "kind": "decide",
         "pass": true
        },
        "TS-S9": {
         "audit": {
          "agree": true,
          "reason": "candidate-tasks.json exists and its only entry has critical true.",
          "verdict": "agrees"
         },
         "desc": "a critical follow-up candidate for the fix (not applicable when no candidate-tasks.json was written)",
         "detail": "[{\"name\": \"json-example-follows-table-order\", \"source\": \"take-in/ws-json-report\", \"discovered_by\": \"orchestrator\", \"discovered_class\": \"interface-drift\", \"critical\": true, \"quote\": \"AssertionError: '{\\\"food\\\": \\\"13.05\\\", \\\"rent\\\": \\\"900.00\\\"}' != '{\\\"rent\\\": \\\"900.00\\\", \\\"food\\\": \\\"13.05\\\"}' (tests. … [cut, 611 characters]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "TS-E1": "pass",
        "TS-E2": "pass"
       },
       "judge": {
        "TS-E1": {
         "quotes": [
          "69debf9ee1fb Take in ws/json-report: json-report",
          "69debf9ee1fb Take in ws/json-report: json-report … call 8: python3 -m unittest discover -s tests -t . … FAILED (failures=1, expected failures=1)",
          "69debf9ee1fb Take in ws/json-report: json-report … call 8: Ran 6 tests … FAILED (failures=1, expected failures=1)"
         ],
         "reasons": [
          "Call 7 runs `git merge --no-edit ws/json-report` and call 8 runs the test command once; call 10 amends the merge commit to the required subject, and the check observes the tip 7e72da7 among its parents.",
          "Call 7 ran `git merge --no-edit ws/json-report` and call 10 amended only the merge commit's message to the required subject, which keeps the ws tip as a parent; the test command ran exactly once after the merge (call 8).",
          "Call 7 merged ws/json-report (tip 7e72da7) and call 10's amend kept the merge but renamed it to the required subject; the test command ran exactly once after the merge, at call 8."
         ]
        },
        "TS-E2": {
         "quotes": [
          "take-in ws/json-report: test_json fails after budget-order's ordering change; blocking candidate json-example-follows-table-order; whole-change review waits",
          "take-in ws/json-report: test_json fails after budget-order's ordering change; blocking candidate json-example-follows-table-order; whole-change review waits",
          "take-in ws/json-report: test_json fails after budget-order's ordering change; blocking candidate json-example-follows-table-order; whole-change review waits"
         ],
         "reasons": [
          "The journal names the cause, and the handshake carries a critical in-change candidate settled by the goals (budget-order G1 versus json-report's table order). The only spawn is the PM, no ask was raised, and the committed diff is just the merged budget-order content, with no code edits by the agent.",
          "The agent read the cause from the run and the goals and journaled it, with take-in-regression.md giving the detail. The handshake carries a critical in-change candidate for the fix. The only spawn was the PM's, `asks_for_user` is empty, and the code root's diff holds only the take-in's changes, so the agent changed no code.",
          "The journal names the cause (budget-order's ordering change) with a path to the analysis that settles it by both changes' goals; the handshake carries a critical in-change candidate for the fix; only the PM was spawned; asks_for_user is empty; no code change follows the take-in commit."
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
         "label": "test run: fails (FAILED (failures=1, expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.001s\nFAILED (failures=1, expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: red"
        },
        {
         "call": 14,
         "candidates": [
          14
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: PM digest agent pass",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "PM digest agent pass",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 20,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after conformance, to its first spawn or its hand-back",
   "title": "Case integrate-takein-settled: a regression from taking in the workspace integration branch, settled by the goals, is fixed in the change by the next plan",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T113840 t1 (skill-evals)"
  },
  {
   "capability": "integrate",
   "id": "integrate-takein-unsettled",
   "items": [
    {
     "checks": [
      {
       "id": "TU-S1",
       "kind": "decide"
      },
      {
       "id": "TU-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § The workspace integration branch is taken in before the review"
     ],
     "id": "TU-E1",
     "question": "Is the workspace integration branch taken in, with the tests run after it?"
    },
    {
     "checks": [
      {
       "id": "TU-S3",
       "kind": "decide"
      },
      {
       "id": "TU-S4",
       "kind": "decide"
      },
      {
       "id": "TU-S5",
       "kind": "signal"
      },
      {
       "id": "TU-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A take-in regression the goals do not settle is a decision ask",
      "asks § An unsettled ask is in the change's inbox",
      "asks § A new ask is committed with a subject naming its id",
      "integrate § Integrate changes no code but an untrue sentence"
     ],
     "id": "TU-E2",
     "question": "Is the regression the goals do not settle a decision ask naming both branches and the run's output, and never an environment ask?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "TU-E1": [
       "integrate § The workspace integration branch is taken in before the review"
      ],
      "TU-E2": [
       "integrate § A take-in regression the goals do not settle is a decision ask",
       "asks § An unsettled ask is in the change's inbox",
       "asks § A new ask is committed with a subject naming its id",
       "integrate § Integrate changes no code but an untrue sentence"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "TU-E1": [
       "TU-S1",
       "TU-S2"
      ],
      "TU-E2": [
       "TU-S3",
       "TU-S4",
       "TU-S5",
       "TU-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T113843",
     "trials": [
      {
       "calls": {
        "13": {
         "error": false,
         "text": "git commit --amend -q -m \"Take in ws/json-report: json-report\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\" && git log --oneline | head -3; python3 -m unittest discover -s tests -t . 2>&1 | tail -8",
         "tool": "Bash"
        },
        "14": {
         "error": false,
         "text": "python3 -m unittest discover -s tests -t . 2>&1 | head -30; git diff 3bd857f HEAD -- tally tests; cat tally/report.py; cat tests/test_json.py",
         "tool": "Bash"
        },
        "20": {
         "error": true,
         "text": "PM digest for cycle-1",
         "tool": "Agent"
        }
       },
       "checks": {
        "TU-S1": {
         "audit": {
          "agree": true,
          "reason": "The commit with the take-in subject is the amended merge, and the evidence lists the workspace branch's tip 84c505a among its ancestry, which fits the observed True parent.",
          "verdict": "agrees"
         },
         "desc": "a commit `Take in <branch>: json-report` merging the workspace branch's tip",
         "detail": "take-in commits: [('634c4cbb0', True)]",
         "kind": "decide",
         "pass": true
        },
        "TU-S2": {
         "audit": {
          "agree": true,
          "reason": "The observed indices are zero-based: the merge is the 12th call and the test runs are calls 13, 14 and 18, so the check saw tests run after a real git merge, as the item expects.",
          "verdict": "agrees"
         },
         "desc": "the test command run after the take-in's merge",
         "detail": "merge at call 11; test runs after it: [12, 13, 17]",
         "kind": "decide",
         "pass": true
        },
        "TU-S3": {
         "audit": {
          "agree": true,
          "reason": "No ask of any kind was created (asks_for_user is empty and no inbox file was added), so finding no decision ask fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "one new decision ask, in the inbox, indexed, and committed with its id in the subject when the session ended on its own",
         "detail": "decision asks: []; placed and committed: []",
         "kind": "decide",
         "pass": false
        },
        "TU-S4": {
         "audit": {
          "agree": true,
          "reason": "No environment ask appears in the state or the home files, which matches the empty observation.",
          "verdict": "agrees"
         },
         "desc": "no environment ask",
         "detail": "[]",
         "kind": "decide",
         "pass": true
        },
        "TU-S5": {
         "audit": {
          "agree": true,
          "reason": "With no ask written there is no ask text to name the branches, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "the ask names both branches",
         "detail": "[]",
         "kind": "signal",
         "pass": false
        },
        "TU-S6": {
         "audit": {
          "agree": true,
          "reason": "The take-in commit is the last commit on json-report and nothing is left uncommitted, so no file outside docs/ changed after it.",
          "verdict": "agrees"
         },
         "desc": "no code changed after the take-in",
         "detail": "since the take-in: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "TU-E1": "pass",
        "TU-E2": "fail"
       },
       "judge": {
        "TU-E1": {
         "quotes": [
          "634c4cb Take in ws/json-report: json-report ... 84c505a Merge change whole-dollars into ws/json-report ... FAILED (failures=1, expected failures=1)",
          "634c4cb Take in ws/json-report: json-report / 3bd857f ... / 84c505a Merge change whole-dollars into ws/json-report; Ran 6 tests ... FAILED (failures=1, expected failures=1)",
          "634c4cb Take in ws/json-report: json-report ... 84c505a Merge change whole-dollars into ws/json-report ... Ran 6 tests in 0.001s FAILED (failures=1, expected failures=1)"
         ],
         "reasons": [
          "Call 12 merged ws/json-report and call 13 renamed that merge commit to the required subject; call 13 then ran the test command on the merged tree, and calls 14 and 18 ran it again.",
          "Call 12 merged ws/json-report, whose tip 84c505a is the commit not yet taken in, into commit 634c4cb, which call 13 retitled `Take in ws/json-report: json-report`, and the test command ran after it in call 13.",
          "Call 12 merges ws/json-report and call 13 renames the commit to `Take in ws/json-report: json-report` with the tip 84c505a in its history, then runs the test command on the result."
         ]
        },
        "TU-E2": {
         "quotes": [
          "\"asks_for_user\": [] ... candidate json-keeps-cents, whole-change review waits",
          "\"asks_for_user\": []; journal: \"its G2 (scripts keep cents) and the reporting scenario settle it; candidate json-keeps-cents\"",
          "its G2 (scripts keep cents) and the reporting scenario settle it; candidate json-keeps-cents, whole-change review waits"
         ],
         "reasons": [
          "The agent treated the regression as settled by whole-dollars G2 and routed it as a critical candidate task, so no decision ask was filed naming both branches and the run's output; it did not change any code itself.",
          "The agent decided whole-dollars G2 settles the conflict and routed it as a critical candidate task, so no decision ask was placed in the inbox, entered in the state or committed, though it changed no code.",
          "The two changes' goals conflict on the regression, but the agent declared it settled and routed it as a critical candidate task with a journal note, so no decision ask exists in the inbox or state (`asks_for_user: []`)."
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
         "label": "test run: fails (FAILED (failures=1, expected failures=1))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.001s\nFAILED (failures=1, expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: red"
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
         "result": "Ran 6 tests in 0.001s\nFAILED (failures=1, expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: red"
        },
        {
         "call": 20,
         "candidates": [
          20
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: PM digest for cycle-1",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "PM digest for cycle-1",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 20,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh, from a handover after conformance, to its first spawn or its hand-back",
   "title": "Case integrate-takein-unsettled: a take-in regression the goals do not settle is a decision ask",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T113843 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-mvp-harden",
   "items": [
    {
     "checks": [
      {
       "id": "MH-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A harden answer starts the cycle and drafts from the hardening list",
      "plan § A hardening entry names the task drafted from it",
      "integrate § An entry a harden answer takes names its task, and one it leaves says so"
     ],
     "id": "MH-E2",
     "question": "Are the two covered entries that touch the same code drafted into one task, each naming it?"
    },
    {
     "checks": [
      {
       "id": "MH-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Entries a partial harden answer leaves are marked left",
      "integrate § An entry a harden answer takes names its task, and one it leaves says so"
     ],
     "id": "MH-E3",
     "question": "Is the entry the answer leaves marked left, naming the ask?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "04cecfc",
     "cites": {
      "MH-E2": [
       "plan § A harden answer starts the cycle and drafts from the hardening list",
       "plan § A hardening entry names the task drafted from it",
       "integrate § An entry a harden answer takes names its task, and one it leaves says so"
      ],
      "MH-E3": [
       "plan § Entries a partial harden answer leaves are marked left",
       "integrate § An entry a harden answer takes names its task, and one it leaves says so"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "MH-E2": [
       "MH-S2"
      ],
      "MH-E3": [
       "MH-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T122840",
     "trials": [
      {
       "calls": {
        "22": {
         "error": true,
         "text": "Premise check cycle-1794677351",
         "tool": "Agent"
        }
       },
       "checks": {
        "MH-S2": {
         "audit": {
          "agree": true,
          "reason": "Both of the first two entries carry :TASK: refund-amounts and tasks/open/refund-amounts.md exists, as observed.",
          "verdict": "agrees"
         },
         "desc": "the two entries that touch the same file name one task, whose file is in tasks/open/",
         "detail": "3 entries in the fixture; :TASK: of the first two ['refund-amounts', 'refund-amounts']; its file: True",
         "kind": "decide",
         "pass": true
        },
        "MH-S3": {
         "audit": {
          "agree": true,
          "reason": "The third entry has :LEFT: ask-cycle-2-1 and no :TASK:, matching the item.",
          "verdict": "agrees"
         },
         "desc": "the entry the answer leaves is marked left, naming the ask",
         "detail": ":LEFT: ask-cycle-2-1; :TASK: None; the MVP ask ask-cycle-2-1",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "MH-E2": "pass",
        "MH-E3": "pass"
       },
       "judge": {
        "MH-E2": {
         "quotes": [
          ":TASK: refund-amounts (on both of the first two entries); discovered_from: cycles/cycle-1/reviews/report-json.md#1 ... (=:REF: arch-cycle-1-1=, medium)",
          ":TASK: refund-amounts (on both first entries); discovered_from: cycles/cycle-1/reviews/report-json.md#1; Summary lists `:REF: arch-cycle-1-1`",
          ":TASK: refund-amounts (on both first entries); discovered_from: cycles/cycle-1/reviews/report-json.md#1; Summary lists \"=:REF: arch-cycle-1-1=\""
         ],
         "reasons": [
          "One task, refund-amounts, is drafted from the first two entries and each entry names it; the task names both entries' refs, though its discovered_from field holds only the first.",
          "One task, refund-amounts, was drafted from the first two entries and both entries name it. The frontmatter's discovered_from cites the first entry's finding and the Summary cites both entries' refs, the second's included.",
          "One task, refund-amounts, was drafted from the first two entries, and each entry names it in :TASK:. The task names both entries' refs, though its front matter's discovered_from names only the first."
         ]
        },
        "MH-E3": {
         "quotes": [
          ":FILES: tally/ledger.py:9\n:LEFT: ask-cycle-2-1\n:END:",
          ":FILES: tally/ledger.py:9\n:LEFT: ask-cycle-2-1\n:END:",
          ":FILES: tally/ledger.py:9\n:LEFT: ask-cycle-2-1\n:END:"
         ],
         "reasons": [
          "The third entry gains :LEFT: with the MVP ask's id and no :TASK:.",
          "The third entry gained `:LEFT:` with the MVP ask's id and has no `:TASK:`.",
          "The third entry gained :LEFT: with the MVP ask's id and has no :TASK:."
         ]
        }
       },
       "milestones": [
        {
         "call": 22,
         "candidates": [
          22
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Premise check cycle-1794677351",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Premise check cycle-1794677351",
         "scenarios": [],
         "sig": "spawn agent None"
        }
       ],
       "n_calls": 22,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle, the MVP decision answered harden, to its first spawn (the premise check)",
   "title": "Case plan-mvp-harden: a partial harden answer starts the cycle, drafts the covered entries into one task and marks the rest left",
   "trials": 3,
   "validated": "04cecfc79b193633fc721dc320f53559368b7ff2 20261114T122840 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-mvp-stop",
   "items": [
    {
     "checks": [
      {
       "id": "MS-S2",
       "kind": "decide"
      },
      {
       "id": "MS-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A stop answer applied at plan marks the change done",
      "integrate § Stop makes the change done"
     ],
     "id": "MS-E3",
     "question": "Is the change marked done in its intent, with the closing summary, committed in the code root under a subject naming the change?"
    },
    {
     "checks": [
      {
       "id": "MS-S4",
       "kind": "signal"
      }
     ],
     "cites": [
      "integrate § The closing summary accounts for the goals, the hardening list, the open tasks and the corrections"
     ],
     "id": "MS-E4",
     "question": "Does the closing summary account for the goals, the hardening list, the open tasks and the corrections?"
    },
    {
     "checks": [
      {
       "id": "MS-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § A done change's documents stay where they are"
     ],
     "id": "MS-E5",
     "question": "Do the change's documents stay where they are?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "MS-E3": [
       "plan § A stop answer applied at plan marks the change done",
       "integrate § Stop makes the change done"
      ],
      "MS-E4": [
       "integrate § The closing summary accounts for the goals, the hardening list, the open tasks and the corrections"
      ],
      "MS-E5": [
       "integrate § A done change's documents stay where they are"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "MS-E3": [
       "MS-S2",
       "MS-S3"
      ],
      "MS-E4": [
       "MS-S4"
      ],
      "MS-E5": [
       "MS-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T103140-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "MS-S2": {
         "audit": {
          "agree": true,
          "reason": "It finds a done value whose sha is on the branch and a commit changing the intent whose subject names json-report, which fits the diff and the commit ca323fc.",
          "verdict": "agrees"
         },
         "desc": "the intent says done with the date and a commit of the branch, committed under a subject naming the change",
         "detail": "value: done 2026-11-14 83213f662f6a; its commit on the branch: True; commits changing the intent naming json-report: json-report: done (ask-cycle-2-1: stop)",
         "kind": "decide",
         "pass": true
        },
        "MS-S3": {
         "audit": {
          "agree": true,
          "reason": "The * Status section is present and non-empty at the tip, as the diff shows.",
          "verdict": "agrees"
         },
         "desc": "the intent holds a * Status section with the closing summary",
         "detail": "\n** done 2026-11-14 (ask-cycle-2-1: stop)\nWhat the change does:\n- G1. =tally FILE --json= prints the totals as one JSON object, in the table's order and with the table's amounts.\n- G2. =tally= refuses an option it does not know: =tally FILE --yaml= prints =tally: unknown option --yaml= and exits 2.\n … [cut, 600 characters]",
         "kind": "decide",
         "pass": true
        },
        "MS-S4": {
         "audit": {
          "agree": true,
          "reason": "As a signal it observes that the hardening list is mentioned and that commit ids (1d19a72, c07ebfd) are present, which matches the summary.",
          "verdict": "agrees"
         },
         "desc": "the closing summary speaks of the hardening list and names a commit",
         "detail": "\n** done 2026-11-14 (ask-cycle-2-1: stop)\nWhat the change does:\n- G1. =tally FILE --json= prints the totals as one JSON object, in the table's order and with the table's amounts.\n- G2. =tally= refuses an option it does not know: =tally FILE --yaml= prints =tally: unknown option --yaml= and exits 2.\n … [cut, 600 characters]",
         "kind": "signal",
         "pass": true
        },
        "MS-S6": {
         "audit": {
          "agree": true,
          "reason": "It checks that intent.org and design.org exist at the tip, and the diff shows no move or removal of any docs.",
          "verdict": "agrees"
         },
         "desc": "the change's documents where they were",
         "detail": "missing at the tip: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "MS-E3": "pass",
        "MS-E4": "pass",
        "MS-E5": "pass"
       },
       "judge": {
        "MS-E3": {
         "quotes": [
          "+#+STATUS: done 2026-11-14 83213f662f6a … ca323fc json-report: done (ask-cycle-2-1: stop)",
          "+#+STATUS: done 2026-11-14 83213f662f6a ... ca323fc json-report: done (ask-cycle-2-1: stop)",
          "+#+STATUS: done 2026-11-14 83213f662f6a ... ca323fc json-report: done (ask-cycle-2-1: stop)"
         ],
         "reasons": [
          "The status line says done with today's date and the branch head before the closing commit; a filled * Status section holds the closing summary; both were committed on the change branch under a subject naming json-report.",
          "The status line reads done with today's date and a sha the agent took right after `git rev-parse HEAD`, though that command's output is not shown; the closing summary sits under * Status; both were committed on json-report under a subject naming the change.",
          "The status line says done with today's date and a 12-character sha, which can only have come from the agent's `git rev-parse HEAD` because it never read the review naming it. The * Status section holds the closing summary, and both are committed on the change branch under a subject naming json-report."
         ]
        },
        "MS-E4": {
         "quotes": [
          "- G1. … - G2. … Hardening list …: none worked. Left: … table-width: … - 1d19a72, reporting § Categories are ordered by total, pinned by tests/test_json.py: #+begin_example … - c07ebfd, =docs/changes/json-report/design.org= § Context: #+begin_example",
          "Hardening list (...): none worked. Left: ... Open tasks left off the main path: - table-width ... - 1d19a72, reporting § Categories are ordered by total, pinned by tests/test_json.py: ... - c07ebfd, =docs/changes/json-report/design.org= § Context:",
          "- 1d19a72, reporting § Categories are ordered by total, pinned by tests/test_json.py: ... - c07ebfd, =docs/changes/json-report/design.org= § Context:"
         ],
         "reasons": [
          "The summary covers each goal in one line, the three hardening entries left (none worked), the open off-path task, and both corrections, each with its commit, its scenario or file, the pinning test where one exists, and its diff.",
          "The summary gives one line per goal, the worked (none) and left hardening entries, the open task, and both corrections, each with its commit, its scenario or file, the pinning test where there is one, and its diff.",
          "The summary has one line each for G1 and G2, says no hardening entries were worked and lists the three left, and names the open table-width task. Both corrections appear with their commit, scenario or file, pinning test where there is one, and diff."
         ]
        },
        "MS-E5": {
         "quotes": [
          "M\tdocs/changes/json-report/intent.org",
          "M\tdocs/changes/json-report/intent.org",
          "M docs/changes/json-report/intent.org"
         ],
         "reasons": [
          "The only docs change was an edit to intent.org in place; the design and the specs were neither moved nor removed.",
          "The only docs/ change is an in-place edit of the intent; the intent, design and specs keep their paths.",
          "The only document change under docs/ is an in-place edit of the intent, and MS-S6 finds intent.org and design.org at the tip."
         ]
        }
       },
       "milestones": [],
       "n_calls": 12,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle, the MVP decision answered stop, to its hand-back",
   "title": "Case plan-mvp-stop: a stop answer opens no cycle and marks the change done in its intent",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103140-1 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-premise",
   "items": [
    {
     "checks": [
      {
       "id": "PP-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An off-batch hardening finding goes on the hardening list at once",
      "integrate § A finding routed hardening is one entry, appended by the phase agent",
      "integrate § A hardening entry gives its kind, its source, its severity and its places"
     ],
     "id": "PP-E4",
     "question": "Is F2, off the batch and routed hardening, one entry appended to the hardening list in its form?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "ef767f8",
     "cites": {
      "PP-E4": [
       "plan § An off-batch hardening finding goes on the hardening list at once",
       "integrate § A finding routed hardening is one entry, appended by the phase agent",
       "integrate § A hardening entry gives its kind, its source, its severity and its places"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "PP-E4": [
       "PP-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T115451",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PP-S3": {
         "audit": {
          "agree": true,
          "reason": "The fixture's head entries were wiped by the overwrite, so the fail is correct.",
          "verdict": "agrees"
         },
         "desc": "F2 is one entry appended to the hardening list, with its kind, ref, severity and places",
         "detail": "the fixture's entries are not kept as they were at the head of the file",
         "kind": "decide",
         "pass": false
        }
       },
       "items": {
        "PP-E4": "fail"
       },
       "judge": {
        "PP-E4": {
         "quotes": [
          "cat > hardening.org <<'EOF'",
          "cat > hardening.org <<'EOF'",
          "cat > hardening.org <<'EOF'"
         ],
         "reasons": [
          "The agent overwrote hardening.org instead of appending, which dropped the fixture's two entries, and the handover calls the file 'created'.",
          "The agent overwrote hardening.org instead of appending to it, so the fixture's two entries are gone.",
          "The agent overwrote hardening.org instead of appending to it, losing the fixture's two entries; the F2 entry is not added after them."
         ]
        }
       },
       "milestones": [],
       "n_calls": 24,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, fresh, from the dispatcher's fixed brief and a handover written after the premise check returned, to its hand-back",
   "title": "Case plan-premise: after the premise check, plan corrects, routes and blocks by each finding, and stops on the one decision",
   "trials": 3,
   "validated": "ef767f89f2f150366c66a8472a5ced9487f0dd8e 20261114T115451 t1 (skill-evals)"
  },
  {
   "capability": "record",
   "id": "record-close-refused",
   "items": [
    {
     "checks": [
      {
       "id": "CR-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "integrate § Integrate hands back one line"
     ],
     "id": "CR-E3",
     "question": "Does integrate hand back one line, naming its phase and cycle?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "4e66929",
     "cites": {
      "CR-E3": [
       "integrate § Integrate hands back one line"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "CR-E3": [
       "CR-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T102037-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "CR-S5": {
         "audit": {
          "agree": true,
          "reason": "The final text is one line holding integrate and cycle-1 (and it says stopped), which fits CR-E3.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line naming the phase (integrate) and the cycle",
         "detail": "248 characters: \"integrate cycle-1: stopped, the PM's agent pass has failed three times (the last two are aa955b14eb984a3be and ab5c08ecce6127975) and wrote no `pm-digest.md`; the handover is `.orchestrator/changes/json-report/cycles/cycle-1/integrate-handover.md`.\"",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "CR-E3": "pass"
       },
       "judge": {
        "CR-E3": {
         "quotes": [
          "integrate cycle-1: stopped, the PM's agent pass has failed three times",
          "integrate cycle-1: stopped, the PM's agent pass has failed three times ... the handover is `.orchestrator/changes/json-report/cycles/cycle-1/integrate-handover.md`.",
          "integrate cycle-1: stopped, the PM's agent pass has failed three times"
         ],
         "reasons": [
          "The session ends with a single line that names integrate and cycle-1, says the phase stopped, and has no report around it.",
          "The session ends with a single line that names integrate and cycle-1 and says stopped, with no report around it.",
          "The session ends with a single line naming integrate and cycle-1, saying the phase stopped and why, with no report around it."
         ]
        }
       },
       "milestones": [],
       "n_calls": 11,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh from a handover after the PM's agent pass failed, past each spawn by the test host's stand-in (the PM's agent pass failing again), to its hand-back",
   "title": "Case record-close-refused: integrate's gate cannot pass while the PM's agent pass keeps failing, and the cycle stays open",
   "trials": 3,
   "validated": "4e6692969a2fef2d4bc1c4ed11ba0f70b3a1406c 20261114T102037-1 t1 (skill-evals)"
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
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RW-E1": [
       "RW-S1",
       "RW-S2"
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
        }
       },
       "items": {
        "RW-E1": "pass"
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
  "spec": "orchestrate/docs/specs/integrate.org",
  "spec_dirty": false,
  "worktree_head": "73b676c"
 },
 "spec": {
  "gaps": "No gap open. The six this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/integrate.org= § Gap decisions).",
  "purpose": "The integrate phase closes a cycle. It has the merged code checked against the change's design by an Architect and settles each disagreement by a test: a seam row the code has rightly outgrown is corrected, a promise the code does not keep is reported against the code. At each integrate whose cycle merged code it has the whole change reviewed, and when the main path works end to end it stops the change for the user's MVP decision; after that decision it marks the change done with a closing summary. It watches for goal drift and puts it to the user. Everything the cycle found and asked it routes to the next plan in the handshake. It writes no task file and changes no code, except to correct an untrue sentence in prose; the tasks it routes are written by plan.",
  "requirements": [
   {
    "goal": "G6 (inferred, O-IN-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate runs"
       },
       {
        "kw": "THEN",
        "text": "an Architect is spawned for conformance"
       },
       {
        "kw": "AND",
        "text": "the check is not made by the integrate phase agent itself"
       },
       {
        "kw": "AND",
        "text": "the change's record holds the agent and its setting"
       }
      ],
      "test": null,
      "title": "Conformance is the Architect's, at every integrate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect is spawned for conformance"
       },
       {
        "kw": "THEN",
        "text": "its input names the seam rows and the scenarios the cycle's tasks cite"
       },
       {
        "kw": "AND",
        "text": "every diff merged in the cycle, for the scan for replaced code still on a live path and code no longer reached"
       }
      ],
      "test": null,
      "title": "Conformance covers what the cycle's tasks cited and every diff it merged"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "conformance finds nothing"
       },
       {
        "kw": "THEN",
        "text": "the change's journal has one line saying the check found zero findings"
       }
      ],
      "test": null,
      "title": "Conformance that finds nothing says so in the journal"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "conformance reports findings"
       },
       {
        "kw": "THEN",
        "text": "the phase agent routes each from the finding's file, not from the Architect's hand-back"
       },
       {
        "kw": "AND",
        "text": "the change's record shows each finding's route: a record correction, a =decision= ask, a follow-up candidate or an entry on the hardening list"
       }
      ],
      "test": null,
      "title": "Every conformance finding is routed from its file"
     }
    ],
    "text": "The integrate phase SHALL have an Architect check the code merged in the cycle against the seam rows and scenarios the cycle's tasks cite, and scan every diff merged in the cycle for replaced code still in use and code no longer reached; the phase agent SHALL route each finding of that check.",
    "title": "Each integrate checks the merged code against the design"
   },
   {
    "goal": "G6 (inferred, O-IN-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cited seam row states something other than what the merged code does, the code is what the change wanted, and a test pins the code's behaviour on that point"
       },
       {
        "kw": "THEN",
        "text": "the seam row is corrected in place in the change's design, as a record correction"
       },
       {
        "kw": "AND",
        "text": "no ask is filed for it"
       },
       {
        "kw": "AND",
        "text": "the change's journal has one line for the record correction, against the seam"
       },
       {
        "kw": "AND",
        "text": "the finding's route names the pinning test"
       }
      ],
      "test": null,
      "title": "A seam row the pinned code has outgrown is corrected in place"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the code outgrew a seam row by a guard, an error string, an encoding or a helper the Implementor added"
       },
       {
        "kw": "THEN",
        "text": "the corrected row states less, not the added detail"
       },
       {
        "kw": "AND",
        "text": "no Decision of the design gains that detail"
       }
      ],
      "test": null,
      "title": "A corrected seam row is cut back to the contract"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a spec scenario states what a corrected seam row stated"
       },
       {
        "kw": "THEN",
        "text": "the scenario is corrected in place, in the same commit as the row"
       },
       {
        "kw": "AND",
        "text": "its title is unchanged"
       }
      ],
      "test": null,
      "title": "A record correction reaches the scenario that states the same thing"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a record correction is committed in the code root"
       },
       {
        "kw": "THEN",
        "text": "its subject contains =record correction="
       },
       {
        "kw": "AND",
        "text": "when it corrects a scenario in a spec, its body has one line =Pinned by: <test>= for each scenario it corrects"
       }
      ],
      "test": null,
      "title": "A record correction's commit says so and names the pin"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "execute journaled a statement for integrate as a record correction"
       },
       {
        "kw": "THEN",
        "text": "the conformance Architect's input names it"
       },
       {
        "kw": "AND",
        "text": "it is corrected by the same test rule as a seam row conformance finds, its commit naming the pin"
       }
      ],
      "test": null,
      "title": "A record correction execute journaled is made by conformance"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate makes more than one record correction"
       },
       {
        "kw": "THEN",
        "text": "each corrected seam row, with the scenario that states the same thing, is in a commit of its own, its subject naming the row"
       }
      ],
      "test": null,
      "title": "Each corrected row or scenario has a commit of its own"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design promises something the merged code does not do"
       },
       {
        "kw": "THEN",
        "text": "the design still states the promise after integrate"
       },
       {
        "kw": "AND",
        "text": "a finding against the code is routed, as a follow-up candidate when it breaks the main path and as an entry on the hardening list otherwise"
       }
      ],
      "test": null,
      "title": "A promise the code does not keep stays in the design"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the design promises something the merged code does not do, and a spec scenario, the intent or a prior conflicts with the promise"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask is filed from the finding"
       },
       {
        "kw": "AND",
        "text": "when only the Architect sees a product question in it, no ask is filed and the finding's recommended resolution carries the question"
       }
      ],
      "test": null,
      "title": "A broken promise is asked about only when the user's own words conflict with it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the merged code does something other than a cited seam row states, not merely less, and no test pins the code's behaviour on that point"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask is filed from the finding"
       },
       {
        "kw": "AND",
        "text": "the seam row is left as it is until the answer"
       }
      ],
      "test": null,
      "title": "Code that does something else, with no pin, puts the design in question"
     }
    ],
    "text": "The integrate phase SHALL correct a seam row to say what the code does only when a test pins the code's behaviour on that point, and SHALL never remove or weaken a promise the design made because the code does not keep it; such a promise SHALL stay in the design and be reported against the code.",
    "title": "Which of the design and the code is right is decided by a test"
   },
   {
    "goal": "none stated (O-NS-13 states it for the whole change)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a conformance finding breaks the main path and is not a record correction"
       },
       {
        "kw": "THEN",
        "text": "a follow-up candidate is written for the next plan, marked critical, naming the finding it came from, with the finding's words verbatim"
       },
       {
        "kw": "AND",
        "text": "no task file is written"
       }
      ],
      "test": null,
      "title": "A blocking conformance finding becomes a follow-up candidate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a blocking finding is still pending"
       },
       {
        "kw": "THEN",
        "text": "integrate does not pass its gate"
       }
      ],
      "test": null,
      "title": "Integrate does not pass its gate while a blocking finding has no route"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a conformance finding is a defect off the main path"
       },
       {
        "kw": "THEN",
        "text": "it is one entry on the change's hardening list"
       },
       {
        "kw": "AND",
        "text": "a stale sentence the same finding exposed is corrected at once all the same"
       }
      ],
      "test": null,
      "title": "Any other conformance defect goes on the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding is answered by a line of the priors or a sentence the user wrote in the intent"
       },
       {
        "kw": "THEN",
        "text": "no ask file is written for it"
       },
       {
        "kw": "AND",
        "text": "it is settled with that prior or sentence cited, or routed as the prior directs"
       }
      ],
      "test": null,
      "title": "A finding the user's words already answer gets no ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an ask file already exists for a question a prior answers"
       },
       {
        "kw": "THEN",
        "text": "the ask is declined, with the prior as its readback"
       }
      ],
      "test": null,
      "title": "An ask the user's words already answer is declined"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding exposes a stale sentence and also a question only the user can answer"
       },
       {
        "kw": "THEN",
        "text": "the sentence is corrected as a record correction, with its journal line"
       },
       {
        "kw": "AND",
        "text": "one =decision= ask is filed for the question"
       }
      ],
      "test": null,
      "title": "A finding with a stale sentence and a product question yields both"
     }
    ],
    "text": "The integrate phase SHALL route every finding it receives, conformance's and the whole-change review's: one on the main path SHALL become a follow-up candidate for the next plan, an untrue sentence SHALL be corrected, any other defect SHALL go on the hardening list. It SHALL file no ask for what the user's own words already answer.",
    "title": "Every finding at integrate ends in a fix, a candidate, an ask or the hardening list"
   },
   {
    "goal": "G11 (O-NS-9)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the cycle merged code: a task, an inline fix or an inline-path commit"
       },
       {
        "kw": "THEN",
        "text": "one Reviewer is spawned over the whole change, whether or not an open task remains"
       },
       {
        "kw": "AND",
        "text": "the change's record holds the agent and its setting"
       }
      ],
      "test": null,
      "title": "A cycle that merged code gets a whole-change review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the cycle merged nothing but changes to the change's documents, and it would not file the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "no whole-change review is spawned"
       }
      ],
      "test": null,
      "title": "A cycle that merged only record text gets no whole-change review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cycle would file the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "a whole-change review runs in that cycle, whatever the cycle merged"
       },
       {
        "kw": "AND",
        "text": "the MVP decision is filed only when that review leaves no blocking finding"
       }
      ],
      "test": null,
      "title": "A cycle that would file the MVP decision runs the review whatever it merged"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change Reviewer is spawned"
       },
       {
        "kw": "THEN",
        "text": "its input holds the diff from the intent's base commit to the code root's HEAD"
       },
       {
        "kw": "AND",
        "text": "the intent, every seam row of the design, and the coverage of the change's scenarios"
       },
       {
        "kw": "AND",
        "text": "the project's review standards and priors"
       }
      ],
      "test": null,
      "title": "The whole-change review reads the change from its base"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change Reviewer is spawned"
       },
       {
        "kw": "THEN",
        "text": "its input holds no Implementor's report, no Observations and no per-task review file"
       }
      ],
      "test": null,
      "title": "The whole-change review is author-blind"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review ends"
       },
       {
        "kw": "THEN",
        "text": "its review file is in the cycle's directory in the home"
       }
      ],
      "test": null,
      "title": "The whole-change review is kept in the cycle's directory"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change was opened from a workspace integration branch and that branch has moved"
       },
       {
        "kw": "THEN",
        "text": "the branch is merged into the change's integration branch before the whole-change Reviewer is spawned"
       },
       {
        "kw": "AND",
        "text": "the test command is run once after that merge"
       },
       {
        "kw": "AND",
        "text": "the commit's subject is =Take in <branch>: <change>="
       }
      ],
      "test": null,
      "title": "The workspace integration branch is taken in before the review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the run after taking in the workspace integration branch shows a test that passed before it failing, and the change's goals settle what the code should do"
       },
       {
        "kw": "THEN",
        "text": "the phase agent journals its cause"
       },
       {
        "kw": "AND",
        "text": "the handshake carries a follow-up candidate for its fix, on the main path"
       },
       {
        "kw": "AND",
        "text": "the whole-change review waits for the cycle that makes the fix"
       }
      ],
      "test": null,
      "title": "A regression from the take-in is fixed in the change when the goals settle it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the run after the take-in shows a regression, and the goals of the two changes do not settle which is right"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask names both branches and the run's output"
       },
       {
        "kw": "AND",
        "text": "no =environment= ask is filed for it"
       }
      ],
      "test": null,
      "title": "A take-in regression the goals do not settle is a decision ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a whole-change review runs in the cycle"
       },
       {
        "kw": "THEN",
        "text": "the PM digest counts its findings and the corrections they led to"
       },
       {
        "kw": "AND",
        "text": "each of its blocking findings is among the cycle's follow-up candidates"
       }
      ],
      "test": null,
      "title": "The digest and the routing count the whole-change review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review reports a defect on the main path"
       },
       {
        "kw": "THEN",
        "text": "a follow-up candidate is written for the next plan, marked critical, naming the whole-change review it came from"
       },
       {
        "kw": "AND",
        "text": "while it is open the change is neither stopped for the MVP decision nor done"
       }
      ],
      "test": null,
      "title": "A blocking whole-change finding holds the change for another cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review finds an untrue sentence in prose"
       },
       {
        "kw": "THEN",
        "text": "the sentence is corrected at integrate, as a record correction with its journal line, its commit saying so (§ A record correction's commit says so and names the pin)"
       },
       {
        "kw": "AND",
        "text": "it holds nothing back"
       }
      ],
      "test": null,
      "title": "An untrue sentence the whole-change review finds is corrected at once"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review reports a defect off the main path"
       },
       {
        "kw": "THEN",
        "text": "it is one entry on the change's hardening list"
       }
      ],
      "test": null,
      "title": "Any other whole-change defect goes on the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review signals that the design or a spec may be what is wrong"
       },
       {
        "kw": "THEN",
        "text": "an ask file is written for it"
       }
      ],
      "test": null,
      "title": "A spec signal from the whole-change review is an ask"
     }
    ],
    "text": "At every integrate whose cycle merged code, the integrate phase SHALL spawn one Reviewer over the whole change, author-blind, after the workspace integration branch is taken in and before the PM digest and the routing to the next plan; the change SHALL go no further while a blocking finding of that review is open.",
    "title": "A cycle that merged code ends with a review of the whole change"
   },
   {
    "goal": "G4 (O-IN-2, O-NS-4)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review leaves no blocking finding, no open task or critical follow-up candidate is on the main path, and the intent's status line is not yet =mvp="
       },
       {
        "kw": "THEN",
        "text": "the intent's status line becomes =mvp <date> <HEAD> <ask-id>="
       },
       {
        "kw": "AND",
        "text": "one =decision= ask is filed with the options =harden=, =rethink= and =stop=, blocking plan"
       },
       {
        "kw": "AND",
        "text": "the status line and the ask are committed"
       }
      ],
      "test": null,
      "title": "The main path working end to end stops the change for one decision"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the main path works end to end and open tasks off the main path remain"
       },
       {
        "kw": "THEN",
        "text": "the MVP decision is filed all the same"
       },
       {
        "kw": "AND",
        "text": "the open tasks wait for its answer"
       }
      ],
      "test": null,
      "title": "Open tasks off the main path do not hold back the MVP decision"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the MVP decision is filed"
       },
       {
        "kw": "THEN",
        "text": "the phase passes its gate, closes and makes its close commit first"
       },
       {
        "kw": "AND",
        "text": "hands back =integrate <cycle-id>: stopped, ask <id> blocks plan; <path>="
       }
      ],
      "test": null,
      "title": "Integrate hands back that it stopped on the MVP decision"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the MVP decision is filed"
       },
       {
        "kw": "THEN",
        "text": "its ask says what the change now does, one short paragraph per goal, and how to try it"
       },
       {
        "kw": "AND",
        "text": "which main-path tests pass and what the whole-change review found"
       }
      ],
      "test": null,
      "title": "The MVP decision says what the change does and how to try it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the MVP decision is filed"
       },
       {
        "kw": "THEN",
        "text": "its ask has one line per hardening entry, highest severity first, with the hardening list's path"
       },
       {
        "kw": "AND",
        "text": "says so when the list is empty"
       }
      ],
      "test": null,
      "title": "The MVP decision lists the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the MVP decision is filed and open tasks off the main path remain"
       },
       {
        "kw": "THEN",
        "text": "its ask lists each in product terms with where it came from"
       }
      ],
      "test": null,
      "title": "The MVP decision lists the open tasks off the main path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the MVP decision is filed"
       },
       {
        "kw": "THEN",
        "text": "its ask shows every record correction made to the specs since the intent's base: its commit, the scenario, the test that pins it and its diff"
       },
       {
        "kw": "AND",
        "text": "then each untrue sentence corrected in prose at integrate, with its commit and diff"
       },
       {
        "kw": "AND",
        "text": "they are shown, not asked about, and the ask says so when there are none"
       }
      ],
      "test": null,
      "title": "The MVP decision shows every record correction to the specs"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the MVP decision is filed"
       },
       {
        "kw": "THEN",
        "text": "its ask recommends one option, with the reason"
       },
       {
        "kw": "AND",
        "text": "says that if it is unanswered nothing runs and the change waits at its MVP"
       }
      ],
      "test": null,
      "title": "The MVP decision recommends one option and says what waits"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user chose =harden= and a later whole-change review leaves no blocking finding"
       },
       {
        "kw": "THEN",
        "text": "no second MVP decision is filed"
       }
      ],
      "test": null,
      "title": "After harden there is no second MVP decision"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate applies the answer =rethink="
       },
       {
        "kw": "THEN",
        "text": "the intent's status line becomes =divergent <date> <ask-id>=, with the signal =user= and the answer quoted under its =* Status="
       }
      ],
      "test": null,
      "title": "Rethink marks the intent divergent"
     }
    ],
    "text": "When the whole-change review leaves no blocking finding, no open task or critical follow-up candidate remains on the main path, and the intent's status line has not yet reached =mvp=, the integrate phase SHALL mark the intent =mvp=, file one =decision= ask (harden, rethink or stop) that blocks plan and shows the user what the change does, how to try it, the hardening list, the open tasks off the main path and the record corrections to the specs, and stop.",
    "title": "When the main path works end to end, the change stops for the MVP decision"
   },
   {
    "goal": "G11, G1 (inferred) (O-NS-9, O-IN-3)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user's answer to the MVP decision is =stop="
       },
       {
        "kw": "THEN",
        "text": "the intent's status line becomes =done <date> <HEAD>=, with the closing summary under its =* Status="
       },
       {
        "kw": "AND",
        "text": "both are committed in the code root in a commit whose subject names the change"
       }
      ],
      "test": null,
      "title": "Stop makes the change done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user chose =harden=, a later whole-change review leaves no blocking finding, and no open task or follow-up candidate remains other than those the answer left"
       },
       {
        "kw": "THEN",
        "text": "the intent's status line becomes =done <date> <HEAD>=, with the closing summary under its =* Status="
       }
      ],
      "test": null,
      "title": "After harden, done follows a clean whole-change review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change's intent says =done="
       },
       {
        "kw": "THEN",
        "text": "a whole-change review of the change has run"
       },
       {
        "kw": "AND",
        "text": "every blocking finding it reported is resolved"
       }
      ],
      "test": null,
      "title": "A change is reported done only after a whole-change review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the closing summary is written"
       },
       {
        "kw": "THEN",
        "text": "it says what the change does, one line per goal"
       },
       {
        "kw": "AND",
        "text": "which hardening entries were worked, with their task names, and which were left, or none"
       },
       {
        "kw": "AND",
        "text": "the open tasks left off the main path, one line each, or none"
       },
       {
        "kw": "AND",
        "text": "the record corrections to the specs and the untrue sentences corrected, each with its commit, its scenario or file, its pinning test where there is one, and its diff"
       }
      ],
      "test": null,
      "title": "The closing summary accounts for the goals, the hardening list, the open tasks and the corrections"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is done"
       },
       {
        "kw": "THEN",
        "text": "its intent, design and spec edits stay where they were in the code root"
       }
      ],
      "test": null,
      "title": "A done change's documents stay where they are"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change opened from a workspace integration branch is done"
       },
       {
        "kw": "THEN",
        "text": "the change's integration branch is merged into that branch as a merge commit whose subject is =Merge <change>="
       },
       {
        "kw": "AND",
        "text": "the integrate phase does not merge that branch to the repository's main"
       }
      ],
      "test": null,
      "title": "A done change merges into its workspace integration branch"
     }
    ],
    "text": "The integrate phase SHALL mark a change done, in its intent, only after the user chose =stop= at the MVP decision, or chose =harden= and a later whole-change review left no blocking finding and no open task or follow-up candidate remains other than those the answer left; it SHALL write the closing summary with the status line and commit both in the code root.",
    "title": "A change is done only after its MVP decision and a clean whole-change review"
   },
   {
    "goal": "G2 (O-NS-2)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM's goal-drift query fires"
       },
       {
        "kw": "THEN",
        "text": "the intent's status line becomes =divergent <date> <ask-id>=, with the signal and its evidence under its =* Status="
       },
       {
        "kw": "AND",
        "text": "the change is committed in the code root"
       }
      ],
      "test": null,
      "title": "Goal drift marks the intent divergent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM's goal-drift query fires"
       },
       {
        "kw": "THEN",
        "text": "one =decision= ask is filed whose options are exactly =revise=, =split=, =abandon= and =continue="
       }
      ],
      "test": null,
      "title": "Goal drift is a decision ask with four fixed options"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user has answered the goal-drift ask"
       },
       {
        "kw": "THEN",
        "text": "the handshake carries the ask, the chosen option and the user's reason"
       }
      ],
      "test": null,
      "title": "The user's answer on goal drift reaches the next plan"
     }
    ],
    "text": "When the PM digest reports that the change's goals may be drifting, the integrate phase SHALL mark the intent divergent with the signal and its evidence and SHALL file one =decision= ask with the options revise, split, abandon and continue; the user's answer SHALL reach the next plan through the handshake.",
    "title": "Goal drift is put to the user"
   },
   {
    "goal": "none stated (O-XC-1 states it for plan)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate passes its gate"
       },
       {
        "kw": "THEN",
        "text": "the change's directory holds the handshake for the cycle"
       },
       {
        "kw": "AND",
        "text": "it holds the seam rows changed, the PM digest's path, the patterns across tasks, the goal-drift answer, the open asks, the resolved asks, the impact sets and the follow-up candidates"
       },
       {
        "kw": "AND",
        "text": "a part with nothing in it is present and empty"
       }
      ],
      "test": null,
      "title": "Each integrate writes a handshake with all its parts"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the cycle added, corrected or retired a seam row"
       },
       {
        "kw": "THEN",
        "text": "the handshake lists each such row once, with the change made and the finding or ask that made it"
       },
       {
        "kw": "AND",
        "text": "when no row moved, the list is empty"
       }
      ],
      "test": null,
      "title": "The seam rows the cycle moved are listed with what moved them"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate passes its gate with asks still open"
       },
       {
        "kw": "THEN",
        "text": "each open ask is in the handshake's open asks"
       },
       {
        "kw": "AND",
        "text": "an ask the user left unanswered stays open, and its default applies at the boundary its file names"
       }
      ],
      "test": null,
      "title": "Every open ask is carried in the handshake"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate passes its gate"
       },
       {
        "kw": "THEN",
        "text": "each task still open in the change has one entry in the handshake's impact sets"
       },
       {
        "kw": "AND",
        "text": "the entry names each change of the cycle that touches the task: a seam row it cites that moved, a pattern that names it, a user's answer, an inline fix in its files, a wrong premise"
       },
       {
        "kw": "AND",
        "text": "an entry with nothing to name is present and empty"
       }
      ],
      "test": null,
      "title": "Every open task has an impact entry, empty when nothing touched it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate passes its gate"
       },
       {
        "kw": "THEN",
        "text": "every blocking finding the cycle routed to a follow-up candidate has its candidate in the handshake"
       },
       {
        "kw": "AND",
        "text": "every answered ask on the main path that needs new work has one too"
       },
       {
        "kw": "AND",
        "text": "a finding on the hardening list has none"
       }
      ],
      "test": null,
      "title": "Every blocking finding and every main-path answer that needs new work has a candidate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an answered ask defers work on the main path and that work is not an open task's"
       },
       {
        "kw": "THEN",
        "text": "a follow-up candidate is written for it, marked critical, naming the ask, with the user's decision as its words"
       }
      ],
      "test": null,
      "title": "A user's answer on the main path that needs new work becomes a candidate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an answered ask defers work off the main path"
       },
       {
        "kw": "THEN",
        "text": "it is one entry on the hardening list, headed as a decision"
       },
       {
        "kw": "AND",
        "text": "no follow-up candidate is written for it"
       }
      ],
      "test": null,
      "title": "A user's answer off the main path goes on the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an open ask names a task it blocks"
       },
       {
        "kw": "THEN",
        "text": "that task is blocked on the ask until the ask is applied"
       }
      ],
      "test": null,
      "title": "A task named in an open ask is blocked on it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "what the cycle did may have made an open task wholly obsolete"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask is filed whose options are close, refine, rescope and do nothing, which blocks the task and leaves it open if unanswered"
       },
       {
        "kw": "AND",
        "text": "the task stays open"
       },
       {
        "kw": "AND",
        "text": "its impact entry flags it"
       }
      ],
      "test": null,
      "title": "A task that may be obsolete is put to the user, never closed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM or the Architect sees a pattern across the cycle's tasks"
       },
       {
        "kw": "THEN",
        "text": "the handshake carries it with its kind, its scope, the tasks that show it and one sentence on what it means for the next plan"
       }
      ],
      "test": null,
      "title": "A pattern across the cycle's tasks reaches the next plan"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a pattern's meaning for the next plan names a task of its own that is not an open task's impact"
       },
       {
        "kw": "THEN",
        "text": "a follow-up candidate is written for it, not marked critical"
       }
      ],
      "test": null,
      "title": "A pattern that names concrete work becomes a candidate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM finds a cluster of externalised tasks coherent enough to be a sub-change"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask is filed on whether to promote it into the change, whose default if unanswered is to leave it externalised"
       }
      ],
      "test": null,
      "title": "A cluster in the cross-change backlog is put to the user"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the Architect's thought line names a project prior that asks for the work"
       },
       {
        "kw": "THEN",
        "text": "the handshake carries a follow-up candidate for it, off the main path, quoting the thought"
       }
      ],
      "test": null,
      "title": "A thought that names a project prior becomes a candidate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate closes"
       },
       {
        "kw": "THEN",
        "text": "no task file has been written or edited by it"
       }
      ],
      "test": null,
      "title": "Integrate writes no task text"
     }
    ],
    "text": "The integrate phase SHALL route everything the cycle found and asked to where it lands and SHALL write, at each cycle, a handshake for the next plan holding the seam rows the cycle changed, the PM digest, the patterns seen across tasks, the user's answer on goal drift, the open and the resolved asks, every open task's impact set and the follow-up candidates. It SHALL write no task text.",
    "title": "Integrate leaves the next plan a handshake of everything the cycle found"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent routes a finding =hardening="
       },
       {
        "kw": "THEN",
        "text": "=hardening.org= in the change's directory gains one entry for it, after the others"
       },
       {
        "kw": "AND",
        "text": "no other agent writes the file"
       }
      ],
      "test": null,
      "title": "A finding routed hardening is one entry, appended by the phase agent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a hardening entry is written for a finding"
       },
       {
        "kw": "THEN",
        "text": "its heading is the finding's kind with =(hardening)= and one line in product terms"
       },
       {
        "kw": "AND",
        "text": "its =:REF:= is the finding's id, its review file and number, or its discovery's id; its =:SEVERITY:= the reporter's; its =:FILES:= the locations"
       },
       {
        "kw": "AND",
        "text": "its body is one paragraph: what goes wrong, on which input, and what the user would notice"
       }
      ],
      "test": null,
      "title": "A hardening entry gives its kind, its source, its severity and its places"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a user's decision's work is off the main path"
       },
       {
        "kw": "THEN",
        "text": "the hardening list gains an entry headed =decision (hardening):= and the decision in product terms, its =:REF:= the ask's id, with no severity"
       }
      ],
      "test": null,
      "title": "A decision off the main path is a hardening entry with its ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a harden answer is applied"
       },
       {
        "kw": "THEN",
        "text": "each entry drafted into a task gains =:TASK: <task-name>="
       },
       {
        "kw": "AND",
        "text": "each entry a partial answer leaves gains =:LEFT: <ask-id>="
       }
      ],
      "test": null,
      "title": "An entry a harden answer takes names its task, and one it leaves says so"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the hardening list is read"
       },
       {
        "kw": "THEN",
        "text": "it holds no untrue sentence in a shipped document, no thought and no matter of style"
       }
      ],
      "test": null,
      "title": "The hardening list holds no untrue sentence, thought or style"
     }
    ],
    "text": "The change's hardening list SHALL be one file in the change's directory, to which the phase agent appends one entry for each finding it routes =hardening= and each user's decision whose work is off the main path; each entry SHALL carry its source, its severity and its locations, and SHALL say what goes wrong in product terms. =plan= and =execute= cite this form.",
    "title": "The hardening list holds the change's reasoned defects off the main path"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate starts and an ask in the inbox has an answer"
       },
       {
        "kw": "THEN",
        "text": "the answer is handled before the rest of the phase's work"
       }
      ],
      "test": null,
      "title": "An answered ask is handled before anything else"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate makes a record correction"
       },
       {
        "kw": "THEN",
        "text": "no ask file is written for it"
       },
       {
        "kw": "AND",
        "text": "the change's journal has its line"
       }
      ],
      "test": null,
      "title": "A record correction is never an ask file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate passes its gate"
       },
       {
        "kw": "THEN",
        "text": "the cycle's directory holds the PM digest, with its signals"
       }
      ],
      "test": null,
      "title": "Each integrate leaves a PM digest"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate corrected the design or a spec in the cycle"
       },
       {
        "kw": "THEN",
        "text": "the correction is committed in the code root before the home's close commit"
       }
      ],
      "test": null,
      "title": "Corrected documents are committed in the code root before the close commit"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate closes"
       },
       {
        "kw": "THEN",
        "text": "one commit in the home, subject =Orchestrator: <cycle-id> integrate closed=, holds the cycle's archived state and handshake, the PM digest and signals, the findings, the list of seam rows changed, the follow-up candidates and the impact sets"
       }
      ],
      "test": null,
      "title": "Integrate closes with one commit in the home"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate closes"
       },
       {
        "kw": "THEN",
        "text": "the only change it made to the code in the code root is the correction of an untrue sentence in prose"
       }
      ],
      "test": null,
      "title": "Integrate changes no code but an untrue sentence"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate spawns the conformance Architect, the whole-change Reviewer or the PM"
       },
       {
        "kw": "THEN",
        "text": "each runs at its role default"
       },
       {
        "kw": "AND",
        "text": "the change's record holds the agent, its role and its setting"
       }
      ],
      "test": null,
      "title": "Each agent integrate spawns runs at its role default"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate passes its gate, stops on an ask, fails or hands over"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives one line naming the phase and the cycle, and when it stopped on an ask, the ask, what it blocks and the file's path"
       }
      ],
      "test": null,
      "title": "Integrate hands back one line"
     }
    ],
    "text": "The integrate phase SHALL handle the answers waiting in the inbox first, SHALL leave a PM digest each cycle, SHALL commit the change's documents it corrected in the code root and the rest of its work in the home with one close commit, SHALL change no code except an untrue sentence in prose, SHALL spawn each agent at its role default, and SHALL hand the dispatcher one line.",
    "title": "Integrate leaves its work in the home and the change's documents in the code root"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); at the hand-off review 2026-11-13: G4 and G7 applied, the hardening list's form and a prior's thought carried in, \"conformance check\" reworded \"conformance\"; the unpinned disagreement the take-in regression and the MVP's review filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-14 after acceptance (an untrue sentence the whole-change review finds is a record correction), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning)",
  "title": "integrate"
 },
 "summary": {
  "cases": 20,
  "cited": 79,
  "fail": 34,
  "pass": 45,
  "scenarios": 79,
  "title": "integrate",
  "tree": "skill-evals",
  "validation": 20
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
