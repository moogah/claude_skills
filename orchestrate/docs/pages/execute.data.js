window.SPEC_PAGE = {
 "capability": "execute",
 "cases": [
  {
   "id": "execute-ex2",
   "items": [
    {
     "checks": [
      {
       "id": "EX-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § Tasks merge in the order they landed",
      "execute § Each task arrives as a merge commit of its own"
     ],
     "id": "EX-E1",
     "question": "Does dollar-table merge into the change's branch before anything of ledger-comments, as a merge commit of its own?"
    },
    {
     "checks": [
      {
       "id": "EX-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § Every merge is followed by a test run whose output is kept",
      "execute § Against a green baseline, any failing run is a regression"
     ],
     "id": "EX-E2",
     "question": "Is the test run after the dollar-table merge kept in the change's directory, and seen to be a regression?"
    },
    {
     "checks": [
      {
       "id": "EX-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § A regression is recorded against its task",
      "record § A regression is marked on the task's entry, with the run that shows it"
     ],
     "id": "EX-E3",
     "question": "Is the regression recorded against dollar-table in the change's record?"
    },
    {
     "checks": [
      {
       "id": "EX-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § The merge that broke a test stays on the branch"
     ],
     "id": "EX-E4",
     "question": "Does the dollar-table merge stay on the branch?"
    },
    {
     "checks": [
      {
       "id": "EX-S20",
       "kind": "decide"
      },
      {
       "id": "EX-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "execute § The phase agent finds the regression's cause",
      "record § A journal entry names its kind and what it is against"
     ],
     "id": "EX-E5",
     "question": "Does the agent find the regression's cause and record it against dollar-table?"
    },
    {
     "checks": [
      {
       "id": "EX-S6",
       "kind": "decide"
      },
      {
       "id": "EX-S8",
       "kind": "decide"
      },
      {
       "id": "EX-S5",
       "kind": "decide"
      },
      {
       "id": "EX-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "execute § A regression is fixed in the change like a blocking defect",
      "asks § An unsettled ask is in the change's inbox",
      "asks § A new ask is committed with a subject naming its id"
     ],
     "id": "EX-E6",
     "question": "Is the regression fixed in the change without an ask, the fix following the goals, and the journal naming the goals that settle it?"
    },
    {
     "checks": [
      {
       "id": "EX-S6",
       "kind": "decide"
      },
      {
       "id": "EX-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "execute § An inline fix's commit names its task, its cause and its pin"
     ],
     "id": "EX-E7",
     "question": "Does the fix's commit name its task, its cause and the test that pins it?"
    },
    {
     "checks": [
      {
       "id": "EX-S13",
       "kind": "decide"
      },
      {
       "id": "EX-S12",
       "kind": "signal"
      }
     ],
     "cites": [
      "execute § A spec statement the goals overrule is left for integrate",
      "execute § Execute leaves the design, the specs and the intent alone"
     ],
     "id": "EX-E8",
     "question": "Is the spec's example the goals overrule left for integrate, with the specs, the design and the intent unchanged?"
    },
    {
     "checks": [
      {
       "id": "EX-S9",
       "kind": "decide"
      },
      {
       "id": "EX-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § Merging resumes only after a run shows no regression",
      "execute § The run after a fix is kept beside the run after the merge"
     ],
     "id": "EX-E9",
     "question": "Does merging resume only after a run shows no regression?"
    },
    {
     "checks": [
      {
       "id": "EX-S14",
       "kind": "signal"
      }
     ],
     "cites": [
      "execute § The task whose merge broke a test is reviewed after the fix"
     ],
     "id": "EX-E10",
     "question": "Is dollar-table's review, if started, started after the fix?"
    },
    {
     "checks": [],
     "cites": [
      "execute § The phase agent never asks the user itself"
     ],
     "id": "EX-E11",
     "question": "Does the phase agent put no question to the user itself?"
    },
    {
     "checks": [
      {
       "id": "EX-S15",
       "kind": "decide"
      },
      {
       "id": "EX-S16",
       "kind": "decide"
      },
      {
       "id": "EX-S19",
       "kind": "signal"
      }
     ],
     "cites": [
      "execute § A fresh agent leaves the handover as written",
      "execute § A fresh agent redoes nothing the handover lists as done"
     ],
     "id": "EX-E12",
     "question": "Does the agent leave the handover as written and redo nothing it lists as done?"
    },
    {
     "checks": [
      {
       "id": "EX-S18",
       "kind": "decide"
      }
     ],
     "cites": [
      "execute § The phase agent's working files stay in its own scratch directory"
     ],
     "id": "EX-E13",
     "question": "Do the agent's working files stay in the scratch directory its brief names?"
    }
   ],
   "runs": {
    "main": {
     "case_commit": "717a75b",
     "cites": {
      "EX-E1": [
       "execute § Tasks merge in the order they landed",
       "execute § Each task arrives as a merge commit of its own"
      ],
      "EX-E10": [
       "execute § The task whose merge broke a test is reviewed after the fix"
      ],
      "EX-E11": [
       "execute § The phase agent never asks the user itself"
      ],
      "EX-E12": [
       "execute § A fresh agent leaves the handover as written",
       "execute § A fresh agent redoes nothing the handover lists as done"
      ],
      "EX-E13": [
       "execute § The phase agent's working files stay in its own scratch directory"
      ],
      "EX-E2": [
       "execute § Every merge is followed by a test run whose output is kept",
       "execute § Against a green baseline, any failing run is a regression"
      ],
      "EX-E3": [
       "execute § A regression is recorded against its task"
      ],
      "EX-E4": [
       "execute § The merge that broke a test stays on the branch"
      ],
      "EX-E5": [
       "execute § The phase agent finds the regression's cause"
      ],
      "EX-E6": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-E7": [
       "execute § An inline fix's commit names its task, its cause and its pin"
      ],
      "EX-E8": [
       "execute § A spec statement the goals overrule is left for integrate",
       "execute § Execute leaves the design, the specs and the intent alone"
      ],
      "EX-E9": [
       "execute § Merging resumes only after a run shows no regression"
      ],
      "EX-S1": [
       "execute § Tasks merge in the order they landed",
       "execute § Each task arrives as a merge commit of its own"
      ],
      "EX-S10": [
       "execute § Merging resumes only after a run shows no regression"
      ],
      "EX-S11": [
       "execute § The phase agent finds the regression's cause",
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S12": [
       "execute § A spec statement the goals overrule is left for integrate"
      ],
      "EX-S13": [
       "execute § A spec statement the goals overrule is left for integrate",
       "execute § Execute leaves the design, the specs and the intent alone"
      ],
      "EX-S14": [
       "execute § The task whose merge broke a test is reviewed after the fix"
      ],
      "EX-S15": [
       "execute § A fresh agent leaves the handover as written"
      ],
      "EX-S16": [
       "execute § A fresh agent redoes nothing the handover lists as done"
      ],
      "EX-S18": [
       "execute § The phase agent's working files stay in its own scratch directory"
      ],
      "EX-S19": [
       "execute § A fresh agent redoes nothing the handover lists as done"
      ],
      "EX-S2": [
       "execute § Every merge is followed by a test run whose output is kept",
       "execute § Against a green baseline, any failing run is a regression"
      ],
      "EX-S3": [
       "execute § A regression is recorded against its task"
      ],
      "EX-S4": [
       "execute § The merge that broke a test stays on the branch"
      ],
      "EX-S5": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S6": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S7": [
       "execute § An inline fix's commit names its task, its cause and its pin"
      ],
      "EX-S8": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S9": [
       "execute § Merging resumes only after a run shows no regression"
      ]
     },
     "commit": "e356da7",
     "items_then": [
      {
       "checks": [],
       "cites": [
        "execute § Tasks merge in the order they landed",
        "execute § Each task arrives as a merge commit of its own"
       ],
       "id": "EX-E1",
       "question": "Does dollar-table merge into the change's branch before anything of ledger-comments, as a merge commit of its own?"
      },
      {
       "checks": [],
       "cites": [
        "execute § Every merge is followed by a test run whose output is kept",
        "execute § Against a green baseline, any failing run is a regression"
       ],
       "id": "EX-E2",
       "question": "Is the test run after the dollar-table merge kept in the change's directory, and seen to be a regression?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A regression is recorded against its task"
       ],
       "id": "EX-E3",
       "question": "Is the regression recorded against dollar-table in the change's record?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The merge that broke a test stays on the branch"
       ],
       "id": "EX-E4",
       "question": "Does the dollar-table merge stay on the branch?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The phase agent finds the regression's cause"
       ],
       "id": "EX-E5",
       "question": "Does the agent find the regression's cause and record it against dollar-table?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A regression is fixed in the change like a blocking defect"
       ],
       "id": "EX-E6",
       "question": "Is the regression fixed in the change without an ask, the fix following the goals, and the journal naming the goals that settle it?"
      },
      {
       "checks": [],
       "cites": [
        "execute § An inline fix's commit names its task, its cause and its pin"
       ],
       "id": "EX-E7",
       "question": "Does the fix's commit name its task, its cause and the test that pins it?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A spec statement the goals overrule is left for integrate",
        "execute § Execute leaves the design, the specs and the intent alone"
       ],
       "id": "EX-E8",
       "question": "Is the spec's example the goals overrule left for integrate, with the specs, the design and the intent unchanged?"
      },
      {
       "checks": [],
       "cites": [
        "execute § Merging resumes only after a run shows no regression"
       ],
       "id": "EX-E9",
       "question": "Does merging resume only after a run shows no regression?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The task whose merge broke a test is reviewed after the fix"
       ],
       "id": "EX-E10",
       "question": "Is dollar-table's review, if started, started after the fix?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The phase agent never asks the user itself"
       ],
       "id": "EX-E11",
       "question": "Does the phase agent put no question to the user itself?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A fresh agent leaves the handover as written",
        "execute § A fresh agent redoes nothing the handover lists as done"
       ],
       "id": "EX-E12",
       "question": "Does the agent leave the handover as written and redo nothing it lists as done?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The phase agent's working files stay in its own scratch directory"
       ],
       "id": "EX-E13",
       "question": "Do the agent's working files stay in the scratch directory its brief names?"
      }
     ],
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
       "checks": {
        "EX-S1": {
         "audit": {
          "agree": true,
          "reason": "The dollar-table merge commit is on the branch.",
          "verdict": "agrees"
         },
         "desc": "dollar-table merged into the change branch (the step's premise)",
         "detail": "4986ad829365",
         "kind": "audited",
         "pass": true
        },
        "EX-S10": {
         "audit": {
          "agree": true,
          "reason": "ledger-comments did not merge, so not applicable is right.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments merged only after the fix (not applicable when not yet merged)",
         "detail": "merge at None, fix at None",
         "kind": "audited",
         "pass": null
        },
        "EX-S11": {
         "audit": {
          "agree": true,
          "reason": "The single journal entry names neither the dollar sign nor G1/G2.",
          "verdict": "agrees"
         },
         "desc": "the journal records the cause against dollar-table and names the goals that settle it",
         "detail": "1 entries",
         "kind": "signal",
         "pass": false
        },
        "EX-S12": {
         "audit": {
          "agree": true,
          "reason": "No journal entry defers the example to integrate.",
          "verdict": "agrees"
         },
         "desc": "the journal leaves the scenario's stale example for integrate as a record correction",
         "detail": "",
         "kind": "signal",
         "pass": false
        },
        "EX-S13": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no docs changes.",
          "verdict": "agrees"
         },
         "desc": "execute left the specs, the design and the intent as they were",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S14": {
         "audit": {
          "agree": true,
          "reason": "No Reviewer was spawned.",
          "verdict": "agrees"
         },
         "desc": "dollar-table's Reviewer spawned only after the fix",
         "detail": "0 Reviewer spawn(s) for dollar-table",
         "kind": "signal",
         "pass": null
        },
        "EX-S15": {
         "audit": {
          "agree": true,
          "reason": "The handover is unchanged per the evidence.",
          "verdict": "agrees"
         },
         "desc": "the handover left as the fixture wrote it",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S16": {
         "audit": {
          "agree": true,
          "reason": "report-json was not merged again.",
          "verdict": "agrees"
         },
         "desc": "report-json not merged again",
         "detail": "1 merge(s)",
         "kind": "audited",
         "pass": true
        },
        "EX-S18": {
         "audit": {
          "agree": true,
          "reason": "A /tmp draft path was observed, which breaks the brief's new scratch-directory rule; the truncated tool lines are consistent with an ask draft written in the last Bash calls.",
          "verdict": "agrees"
         },
         "desc": "working files only in the scratch directory the brief names (none in /tmp or a mktemp directory)",
         "detail": "/tmp/ask-draft-json-report.org",
         "kind": "decide",
         "pass": false
        },
        "EX-S19": {
         "audit": {
          "agree": true,
          "reason": "No spawns at all were made.",
          "verdict": "agrees"
         },
         "desc": "no Reviewer spawned for report-json",
         "detail": "0 review spawn(s)",
         "kind": "signal",
         "pass": true
        },
        "EX-S2": {
         "audit": {
          "agree": true,
          "reason": "The after-dollar-table file holds the failing run.",
          "verdict": "agrees"
         },
         "desc": "an after-file for dollar-table, holding the failing run",
         "detail": "after-dollar-table-1793000002.txt",
         "kind": "audited",
         "pass": true
        },
        "EX-S3": {
         "audit": {
          "agree": true,
          "reason": "Both the state flag and the journal entry record the regression on dollar-table.",
          "verdict": "agrees"
         },
         "desc": "the regression recorded against dollar-table",
         "detail": "regression_detected=True status=blocked",
         "kind": "audited",
         "pass": true
        },
        "EX-S4": {
         "audit": {
          "agree": true,
          "reason": "No revert follows the merge.",
          "verdict": "agrees"
         },
         "desc": "the dollar-table merge stays on the branch (no revert)",
         "detail": "",
         "kind": "audited",
         "pass": true
        },
        "EX-S5": {
         "audit": {
          "agree": true,
          "reason": "An ask file was written, so fail is right.",
          "verdict": "agrees"
         },
         "desc": "no ask filed for the regression",
         "detail": ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org | hand-back: execute cycle-1: stopped, the dollar-table merge fails the test suite; ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org` needs an answer. The merges are halted and ledger-comments is not",
         "kind": "audited",
         "pass": false
        },
        "EX-S6": {
         "audit": {
          "agree": true,
          "reason": "Nothing follows the merge on the branch.",
          "verdict": "agrees"
         },
         "desc": "an inline fix on the change branch after the dollar-table merge",
         "detail": "no commit after the merge",
         "kind": "decide",
         "pass": false
        },
        "EX-S7": {
         "audit": {
          "agree": true,
          "reason": "Not applicable without a fix commit.",
          "verdict": "agrees"
         },
         "desc": "the inline fix's message names what it answers and the test that pins it",
         "detail": "",
         "kind": "signal",
         "pass": null
        },
        "EX-S8": {
         "audit": {
          "agree": true,
          "reason": "No fix was made, so the JSON test still expects \"900.00\".",
          "verdict": "agrees"
         },
         "desc": "the fix follows the goals: the table keeps its dollar sign and the JSON test expects it",
         "detail": "_money has $: True; test expects $900.00: False",
         "kind": "decide",
         "pass": false
        },
        "EX-S9": {
         "audit": {
          "agree": true,
          "reason": "Only the failing after-file follows the report-json run.",
          "verdict": "agrees"
         },
         "desc": "a run after the fix shows no regression, kept in the change's directory",
         "detail": "",
         "kind": "audited",
         "pass": false
        }
       },
       "items": {
        "EX-E1": "pass",
        "EX-E10": "n/a",
        "EX-E11": "pass",
        "EX-E12": "pass",
        "EX-E13": "fail",
        "EX-E2": "pass",
        "EX-E3": "pass",
        "EX-E4": "pass",
        "EX-E5": "fail",
        "EX-E6": "fail",
        "EX-E7": "n/a",
        "EX-E8": "fail",
        "EX-E9": "fail"
       },
       "judge": {
        "EX-E1": {
         "quotes": [
          "4986ad8 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "4986ad8 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "4986ad8 Merge task dollar-table: Show amounts in the table with a dollar sign"
         ],
         "reasons": [
          "json-report gains its own merge commit for dollar-table, and no ledger-comments commit appears on the branch.",
          "The change branch's only commit since the fixture is the dollar-table merge commit, and nothing of ledger-comments precedes it.",
          "The only commit on json-report since the fixture is dollar-table's own merge commit, and no ledger-comments commit precedes it."
         ]
        },
        "EX-E10": {
         "quotes": [
          "Agents spawned (in order): (none)",
          "Agents spawned (in order): (none)",
          "Agents spawned: (none)"
         ],
         "reasons": [
          "No Reviewer was spawned for dollar-table.",
          "No Reviewer was spawned for dollar-table.",
          "No Reviewer was spawned."
         ]
        },
        "EX-E11": {
         "quotes": [
          "ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org` needs an answer",
          "ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org` needs an answer",
          "ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org` needs an answer"
         ],
         "reasons": [
          "The question went to an ask file and the hand-back, not to the user directly.",
          "The question went to an ask file and the hand-back, not to the user directly.",
          "The agent's question went to an ask file and the hand-back, not to the user directly."
         ]
        },
        "EX-E12": {
         "quotes": [
          "Changes to the handover file since the fixture: (none)",
          "Changes to the handover file since the fixture: (none)",
          "Changes to the handover file since the fixture: (none)"
         ],
         "reasons": [
          "The handover is unchanged, and report-json has one merge and no Reviewer spawn.",
          "The handover is unchanged, and report-json was not merged again or reviewed again.",
          "The handover is unchanged, and report-json was neither merged again nor reviewed again (one merge, no Reviewer spawns)."
         ]
        },
        "EX-E13": {
         "quotes": [
          "/tmp/ask-draft-json-report.org",
          "/tmp/ask-draft-json-report.org",
          "/tmp/ask-draft-json-report.org"
         ],
         "reasons": [
          "Per the script's observation (the tool-call lines shown are cut off at 160 characters), the agent wrote a draft under /tmp instead of its scratch directory.",
          "The check observed an ask draft written under /tmp rather than the brief's scratch directory.",
          "The agent wrote a draft under /tmp instead of the scratch directory its brief names."
         ]
        },
        "EX-E2": {
         "quotes": [
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1); \"regression: dollar-table merge 4986ad8 breaks tests.test_json test_scenario_example\"",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)"
         ],
         "reasons": [
          "The failing run is kept under dollar-table's name, the ask says test_scenario_example in tests/test_json.py now prints \"$900.00\", and the journal calls it a regression.",
          "The failing run is kept in the change's directory under a name for dollar-table, the ask describes the \"$900.00\" vs \"900.00\" mismatch, and the journal calls it a regression.",
          "The failing run is kept under dollar-table's name in the change directory; the journal ('regression: dollar-table merge 4986ad8 breaks tests.test_json test_scenario_example') and the ask ('now prints \"$900.00\"') show the agent treated it as a regression."
         ]
        },
        "EX-E3": {
         "quotes": [
          "\"ref\": \"dollar-table\", \"text\": \"regression: dollar-table merge 4986ad8 breaks tests.test_json test_scenario_example",
          "\"ref\": \"dollar-table\", \"text\": \"regression: dollar-table merge 4986ad8 breaks tests.test_json ...\"",
          "\"ref\": \"dollar-table\", \"text\": \"regression: dollar-table merge 4986ad8 breaks tests.test_json ...\""
         ],
         "reasons": [
          "The journal records the regression against dollar-table, and the state shows regression_detected on it.",
          "The journal records a regression against dollar-table.",
          "The change's record has a regression entry against dollar-table."
         ]
        },
        "EX-E4": {
         "quotes": [
          "4986ad8 Merge task dollar-table",
          "4986ad8 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "4986ad8 Merge task dollar-table"
         ],
         "reasons": [
          "The merge is still the branch tip, with no revert or reset.",
          "The merge is still the branch's commit, with no revert or reset after it.",
          "The merge is still on the branch, with no revert or reset after it."
         ]
        },
        "EX-E5": {
         "quotes": [
          "breaks tests.test_json test_scenario_example (shared _money helper); merges stopped",
          "breaks tests.test_json test_scenario_example (shared _money helper); merges stopped",
          "breaks tests.test_json test_scenario_example (shared _money helper)"
         ],
         "reasons": [
          "The journal names the shared helper but never says it now writes a dollar sign; only the ask file says that.",
          "The journal names the shared helper but never says it now writes a dollar sign; that explanation appears only in the ask file.",
          "The journal names the shared helper but never says it now writes a dollar sign; that part appears only in the ask file, not in the record."
         ]
        },
        "EX-E6": {
         "quotes": [
          "ask-cycle-1-1.org ... Recommendation ... JSON keeps plain amounts",
          "ask-cycle-1-1.org ... Recommendation - JSON keeps plain amounts",
          "ask-cycle-1-1.org ... Recommendation - JSON keeps plain amounts"
         ],
         "reasons": [
          "No fix was made, an ask was written, and its recommendation runs against the goals (it would drop the JSON's match with the table) without naming G1 or G2.",
          "No fix was made, an ask was filed, its recommendation goes against goals G1/G2, and the journal does not name G1 or G2.",
          "No fix was made; the agent filed an ask instead, recommended the opposite of what the goals settle, and the journal names neither G1 nor G2."
         ]
        },
        "EX-E7": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "No inline fix was made.",
          "No inline fix was made.",
          "No inline fix commit was made."
         ]
        },
        "EX-E8": {
         "quotes": [
          "Changes under docs/ on the change's branch since the fixture: (none)",
          "Changes under docs/ on the change's branch since the fixture: (none)",
          "Changes under docs/ on the change's branch since the fixture: (none)"
         ],
         "reasons": [
          "The docs are unchanged, but no journal entry leaves the \"900.00\" example for integrate as a record correction.",
          "Docs are unchanged, but the journal has no entry leaving the \"900.00\" example to integrate as a record correction.",
          "The docs are unchanged, but no journal entry leaves the \"900.00\" example for integrate as a record correction."
         ]
        },
        "EX-E9": {
         "quotes": [
          "after-dollar-table-1793000002.txt ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)"
         ],
         "reasons": [
          "No clean run follows the failing one; ledger-comments was held, but the required clean run after a fix does not exist.",
          "No clean run after a fix is kept; merging was halted rather than resumed after a clean run.",
          "No clean run after a fix is kept, since there was no fix; ledger-comments simply stayed unmerged."
         ]
        }
       },
       "milestones": [
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [
          "EX-S1"
         ],
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
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
         "scenarios": [],
         "sig": "test run after merge dollar-table: red"
        },
        {
         "call": 13,
         "candidates": [
          13
         ],
         "checks": [],
         "label": "journal, note: regression: dollar-table merge 4986ad8 breaks tests.test_json test_scenario_example (shared _money helper); merges stopped",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "ask filed: ask-cycle-1-1.org (environment): Should the JSON report show amounts with a dollar sign too, now that the table does?",
         "lane": "record",
         "linked_by": "its text in the call",
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
       "checks": {
        "EX-S1": {
         "audit": {
          "agree": true,
          "reason": "The branch log shows the dollar-table merge commit.",
          "verdict": "agrees"
         },
         "desc": "dollar-table merged into the change branch (the step's premise)",
         "detail": "35e5de57703e",
         "kind": "audited",
         "pass": true
        },
        "EX-S10": {
         "audit": {
          "agree": true,
          "reason": "ledger-comments was not merged, so not applicable is right.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments merged only after the fix (not applicable when not yet merged)",
         "detail": "merge at None, fix at None",
         "kind": "audited",
         "pass": null
        },
        "EX-S11": {
         "audit": {
          "agree": true,
          "reason": "The journal has no entries.",
          "verdict": "agrees"
         },
         "desc": "the journal records the cause against dollar-table and names the goals that settle it",
         "detail": "0 entries",
         "kind": "signal",
         "pass": false
        },
        "EX-S12": {
         "audit": {
          "agree": true,
          "reason": "The journal has no entries, so none leaves the example for integrate.",
          "verdict": "agrees"
         },
         "desc": "the journal leaves the scenario's stale example for integrate as a record correction",
         "detail": "",
         "kind": "signal",
         "pass": false
        },
        "EX-S13": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no docs/ changes on the branch.",
          "verdict": "agrees"
         },
         "desc": "execute left the specs, the design and the intent as they were",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S14": {
         "audit": {
          "agree": true,
          "reason": "No agents were spawned.",
          "verdict": "agrees"
         },
         "desc": "dollar-table's Reviewer spawned only after the fix",
         "detail": "0 Reviewer spawn(s) for dollar-table",
         "kind": "signal",
         "pass": null
        },
        "EX-S15": {
         "audit": {
          "agree": true,
          "reason": "The diffstat shows the handover file was rewritten.",
          "verdict": "agrees"
         },
         "desc": "the handover left as the fixture wrote it",
         "detail": ".../changes/json-report/cycles/cycle-1/execute-handover.md | 14 ++++++++------\n 1 file changed, 8 insertions(+), 6 deletions(-)",
         "kind": "decide",
         "pass": false
        },
        "EX-S16": {
         "audit": {
          "agree": true,
          "reason": "Only the fixture's report-json merge exists; the agent added no new one.",
          "verdict": "agrees"
         },
         "desc": "report-json not merged again",
         "detail": "1 merge(s)",
         "kind": "audited",
         "pass": true
        },
        "EX-S18": {
         "audit": {
          "agree": true,
          "reason": "The tool calls show a write to /tmp/ask-json-report.json, which this check flags whichever brief named the scratch directory.",
          "verdict": "agrees"
         },
         "desc": "working files only in the scratch directory the brief names (none in /tmp or a mktemp directory)",
         "detail": "/tmp/ask-json-report.json",
         "kind": "decide",
         "pass": false
        },
        "EX-S19": {
         "audit": {
          "agree": true,
          "reason": "No agents were spawned at all.",
          "verdict": "agrees"
         },
         "desc": "no Reviewer spawned for report-json",
         "detail": "0 review spawn(s)",
         "kind": "signal",
         "pass": true
        },
        "EX-S2": {
         "audit": {
          "agree": true,
          "reason": "The after-dollar-table file exists and ends in FAILED (failures=1).",
          "verdict": "agrees"
         },
         "desc": "an after-file for dollar-table, holding the failing run",
         "detail": "after-dollar-table-1793000002.txt",
         "kind": "audited",
         "pass": true
        },
        "EX-S3": {
         "audit": {
          "agree": true,
          "reason": "The agent's 'task set dollar-table' calls fit the observed regression_detected=True on dollar-table.",
          "verdict": "agrees"
         },
         "desc": "the regression recorded against dollar-table",
         "detail": "regression_detected=True status=blocked",
         "kind": "audited",
         "pass": true
        },
        "EX-S4": {
         "audit": {
          "agree": true,
          "reason": "The only commit since the fixture is the merge itself; there is no revert.",
          "verdict": "agrees"
         },
         "desc": "the dollar-table merge stays on the branch (no revert)",
         "detail": "",
         "kind": "audited",
         "pass": true
        },
        "EX-S5": {
         "audit": {
          "agree": true,
          "reason": "An ask file was written in the change's inbox, so the fail is correct.",
          "verdict": "agrees"
         },
         "desc": "no ask filed for the regression",
         "detail": ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org | hand-back: execute cycle-1: stopped, regression after merging dollar-table; ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org` (keep `$` out of the JSON report, or not?) blocks the merges of dollar-",
         "kind": "audited",
         "pass": false
        },
        "EX-S6": {
         "audit": {
          "agree": true,
          "reason": "The branch has no commit after the merge.",
          "verdict": "agrees"
         },
         "desc": "an inline fix on the change branch after the dollar-table merge",
         "detail": "no commit after the merge",
         "kind": "decide",
         "pass": false
        },
        "EX-S7": {
         "audit": {
          "agree": true,
          "reason": "Without a fix commit there is no message to check, so not applicable is right.",
          "verdict": "agrees"
         },
         "desc": "the inline fix's message names what it answers and the test that pins it",
         "detail": "",
         "kind": "signal",
         "pass": null
        },
        "EX-S8": {
         "audit": {
          "agree": true,
          "reason": "No fix was made, so the JSON test still expects 900.00 while _money writes $.",
          "verdict": "agrees"
         },
         "desc": "the fix follows the goals: the table keeps its dollar sign and the JSON test expects it",
         "detail": "_money has $: True; test expects $900.00: False",
         "kind": "decide",
         "pass": false
        },
        "EX-S9": {
         "audit": {
          "agree": true,
          "reason": "Only two after-files exist, and the newest is the failing one.",
          "verdict": "agrees"
         },
         "desc": "a run after the fix shows no regression, kept in the change's directory",
         "detail": "",
         "kind": "audited",
         "pass": false
        }
       },
       "items": {
        "EX-E1": "pass",
        "EX-E10": "n/a",
        "EX-E11": "pass",
        "EX-E12": "fail",
        "EX-E13": "fail",
        "EX-E2": "pass",
        "EX-E3": "pass",
        "EX-E4": "pass",
        "EX-E5": "fail",
        "EX-E6": "fail",
        "EX-E7": "n/a",
        "EX-E8": "fail",
        "EX-E9": "fail"
       },
       "judge": {
        "EX-E1": {
         "quotes": [
          "35e5de5 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "35e5de5 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "35e5de5 Merge task dollar-table: Show amounts in the table with a dollar sign"
         ],
         "reasons": [
          "The change branch gains its own dollar-table merge commit, and no ledger-comments commit comes before it.",
          "json-report gains its own dollar-table merge commit, and no ledger-comments commit appears before it or at all.",
          "json-report gained its own dollar-table merge commit, and no ledger-comments commit precedes it."
         ]
        },
        "EX-E10": {
         "quotes": [
          "== Agents spawned (in order) ==\n(none)",
          "== Agents spawned (in order) ==\n(none)",
          "== Agents spawned (in order) ==\n(none)"
         ],
         "reasons": [
          "No Reviewer for dollar-table was spawned.",
          "No Reviewer for dollar-table was spawned.",
          "No Reviewer was spawned for dollar-table."
         ]
        },
        "EX-E11": {
         "quotes": [
          "ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org` (keep `$` out of the JSON report, or not?)",
          "hand-back: \"ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org` (keep `$` out of the JSON report, or not?)\"",
          "execute cycle-1: stopped, regression after merging dollar-table; ask `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`"
         ],
         "reasons": [
          "The question went to an ask file and the hand-back, not to the user directly.",
          "The question went to an ask file and the hand-back, not to the user directly.",
          "The question went to an ask file and the hand-back, not to the user directly."
         ]
        },
        "EX-E12": {
         "quotes": [
          "execute-handover.md | 14 ++++++++------",
          ".../changes/json-report/cycles/cycle-1/execute-handover.md | 14 ++++++++------",
          "execute-handover.md | 14 ++++++++------"
         ],
         "reasons": [
          "The agent rewrote the handover file; report-json was not merged or reviewed again, but the file did not stay as written.",
          "The agent rewrote the handover file (8 insertions, 6 deletions), though report-json was not merged or reviewed again.",
          "The agent rewrote the handover file, though report-json was not redone."
         ]
        },
        "EX-E13": {
         "quotes": [
          "cat > /tmp/ask-json-report.json",
          "cat > /tmp/ask-json-report.json <<'EOF'",
          "cat > /tmp/ask-json-report.json"
         ],
         "reasons": [
          "The ask draft was written under /tmp, not the trial's scratch directory.",
          "The ask draft was written to /tmp, not to the scratch directory.",
          "A draft was written to /tmp instead of the scratch directory."
         ]
        },
        "EX-E2": {
         "quotes": [
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1); ask: \"expects {\\\"rent\\\": \\\"900.00\\\", ...} and gets {\\\"rent\\\": \\\"$900.00\\\", ...}\"; hand-back: \"stopped, regression after merging dollar-table\"",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)"
         ],
         "reasons": [
          "The failing run is kept under dollar-table's name, the ask file says it expects \"900.00\" and gets \"$900.00\", and the hand-back calls it a \"regression after merging dollar-table\".",
          "The failing run is kept under dollar-table's name, the ask describes the JSON test's 900.00 vs $900.00 failure, and the hand-back calls it a regression.",
          "The failing run is kept under the dollar-table name; the ask records the JSON test expecting \"900.00\" and getting \"$900.00\", and the hand-back calls it a regression."
         ]
        },
        "EX-E3": {
         "quotes": [
          "state.py task set dollar-table re...",
          "state.py task set dollar-table re…",
          "state.py task set dollar-table re"
         ],
         "reasons": [
          "The agent set dollar-table's task record, and the state shows a regression detected on it with the task blocked.",
          "The agent set a field on dollar-table's task record; the call is truncated, but the script reads the result as regression_detected=True on dollar-table.",
          "The agent set state on dollar-table, and the record shows regression_detected true with status blocked."
         ]
        },
        "EX-E4": {
         "quotes": [
          "35e5de5 Merge task dollar-table",
          "35e5de5 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "35e5de5 Merge task dollar-table"
         ],
         "reasons": [
          "The merge is still on the branch, with no revert or reset after it.",
          "The merge is still the tip's commit and there is no revert or reset.",
          "The merge is still the branch tip, with no revert or reset after it."
         ]
        },
        "EX-E5": {
         "quotes": [
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)"
         ],
         "reasons": [
          "The cause is stated only in the ask file; the journal has no entry against dollar-table.",
          "The cause appears only in the ask file; the journal has no entry against dollar-table.",
          "The cause appears only in the ask file; the journal has no entry against dollar-table."
         ]
        },
        "EX-E6": {
         "quotes": [
          "inbox/ask-cycle-1-1.org ... :BLOCKS: merge:dollar-table, merge:ledger-comments",
          ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org ... :KIND: environment",
          ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org"
         ],
         "reasons": [
          "The agent filed an ask instead of fixing the JSON test's expected value, and no fix landed on the branch.",
          "The agent filed an ask instead of fixing the JSON test per G1/G2, made no fix, and wrote no journal entry naming the goals.",
          "The agent filed an ask instead of fixing the JSON test's expected value, and no journal entry names G1 and G2."
         ]
        },
        "EX-E7": {
         "quotes": [
          "",
          "35e5de5 Merge task dollar-table (only commit since the fixture)",
          ""
         ],
         "reasons": [
          "No fix commit was made, so there is no inline fix to judge.",
          "No inline fix was made.",
          "No inline fix commit was made."
         ]
        },
        "EX-E8": {
         "quotes": [
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)"
         ],
         "reasons": [
          "Nothing under docs/ changed, but no journal entry leaves the \"900.00\" example for integrate as a record correction.",
          "Nothing under docs/ changed, but no journal entry leaves the 900.00 example for integrate as a record correction.",
          "Nothing changed under docs/, but no journal entry leaves the \"900.00\" example for integrate as a record correction."
         ]
        },
        "EX-E9": {
         "quotes": [
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt ... FAILED (failures=1)"
         ],
         "reasons": [
          "No clean run follows the failing one; merging stopped instead of resuming.",
          "No clean run follows the failing one; merging did not resume (ledger-comments is unmerged).",
          "No clean run after a fix is kept; the failing run is the last one."
         ]
        }
       },
       "milestones": [
        {
         "call": 6,
         "candidates": [
          6
         ],
         "checks": [
          "EX-S1"
         ],
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
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "ask filed: ask-cycle-1-1.org (environment): Should amounts in the JSON report stay without a dollar sign now that the table shows one?",
         "lane": "record",
         "linked_by": "its text in the call",
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
       "checks": {
        "EX-S1": {
         "audit": {
          "agree": true,
          "reason": "The merge commit is in the branch log.",
          "verdict": "agrees"
         },
         "desc": "dollar-table merged into the change branch (the step's premise)",
         "detail": "8b9f3c842b59",
         "kind": "audited",
         "pass": true
        },
        "EX-S10": {
         "audit": {
          "agree": true,
          "reason": "ledger-comments did not merge, so the check correctly does not apply.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments merged only after the fix (not applicable when not yet merged)",
         "detail": "merge at None, fix at None",
         "kind": "audited",
         "pass": null
        },
        "EX-S11": {
         "audit": {
          "agree": true,
          "reason": "The journal is empty.",
          "verdict": "agrees"
         },
         "desc": "the journal records the cause against dollar-table and names the goals that settle it",
         "detail": "0 entries",
         "kind": "signal",
         "pass": false
        },
        "EX-S12": {
         "audit": {
          "agree": true,
          "reason": "The journal is empty.",
          "verdict": "agrees"
         },
         "desc": "the journal leaves the scenario's stale example for integrate as a record correction",
         "detail": "",
         "kind": "signal",
         "pass": false
        },
        "EX-S13": {
         "audit": {
          "agree": true,
          "reason": "No docs changes appear in the evidence.",
          "verdict": "agrees"
         },
         "desc": "execute left the specs, the design and the intent as they were",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S14": {
         "audit": {
          "agree": true,
          "reason": "No agents were spawned.",
          "verdict": "agrees"
         },
         "desc": "dollar-table's Reviewer spawned only after the fix",
         "detail": "0 Reviewer spawn(s) for dollar-table",
         "kind": "signal",
         "pass": null
        },
        "EX-S15": {
         "audit": {
          "agree": true,
          "reason": "The handover file is unchanged.",
          "verdict": "agrees"
         },
         "desc": "the handover left as the fixture wrote it",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S16": {
         "audit": {
          "agree": true,
          "reason": "Only the fixture's single report-json merge exists, and the step added none.",
          "verdict": "agrees"
         },
         "desc": "report-json not merged again",
         "detail": "1 merge(s)",
         "kind": "audited",
         "pass": true
        },
        "EX-S18": {
         "audit": {
          "agree": true,
          "reason": "It matched an ask-draft path under /tmp in the agent's commands, which is a working file outside the trial's directory.",
          "verdict": "agrees"
         },
         "desc": "working files only in the scratch directory the brief names (none in /tmp or a mktemp directory)",
         "detail": "/tmp/ask-draft-$$.org /tmp/ask-draft-*.org",
         "kind": "decide",
         "pass": false
        },
        "EX-S19": {
         "audit": {
          "agree": true,
          "reason": "No spawns occurred.",
          "verdict": "agrees"
         },
         "desc": "no Reviewer spawned for report-json",
         "detail": "0 review spawn(s)",
         "kind": "signal",
         "pass": true
        },
        "EX-S2": {
         "audit": {
          "agree": true,
          "reason": "The after-dollar-table file holds a failing run.",
          "verdict": "agrees"
         },
         "desc": "an after-file for dollar-table, holding the failing run",
         "detail": "after-dollar-table-1793000002.txt",
         "kind": "audited",
         "pass": true
        },
        "EX-S3": {
         "audit": {
          "agree": true,
          "reason": "It read regression_detected on dollar-table, which fits the agent's task-set calls and the blocked status.",
          "verdict": "agrees"
         },
         "desc": "the regression recorded against dollar-table",
         "detail": "regression_detected=True status=blocked",
         "kind": "audited",
         "pass": true
        },
        "EX-S4": {
         "audit": {
          "agree": true,
          "reason": "The log shows no revert, and the hand-back confirms the merge stays.",
          "verdict": "agrees"
         },
         "desc": "the dollar-table merge stays on the branch (no revert)",
         "detail": "",
         "kind": "audited",
         "pass": true
        },
        "EX-S5": {
         "audit": {
          "agree": true,
          "reason": "An ask file exists in the inbox.",
          "verdict": "agrees"
         },
         "desc": "no ask filed for the regression",
         "detail": ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org | hand-back: execute cycle-1: stopped, `ask-cycle-1-1` (`.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`). Merging `dollar-table` broke the JSON report's scenario test: the amounts came out as \"$900.00\"",
         "kind": "audited",
         "pass": false
        },
        "EX-S6": {
         "audit": {
          "agree": true,
          "reason": "The only commit since the fixture is the merge, so there is no inline fix.",
          "verdict": "agrees"
         },
         "desc": "an inline fix on the change branch after the dollar-table merge",
         "detail": "no commit after the merge",
         "kind": "decide",
         "pass": false
        },
        "EX-S7": {
         "audit": {
          "agree": true,
          "reason": "It is correctly not applicable without a fix.",
          "verdict": "agrees"
         },
         "desc": "the inline fix's message names what it answers and the test that pins it",
         "detail": "",
         "kind": "signal",
         "pass": null
        },
        "EX-S8": {
         "audit": {
          "agree": true,
          "reason": "No fix was made, so the JSON test still expects 900.00.",
          "verdict": "agrees"
         },
         "desc": "the fix follows the goals: the table keeps its dollar sign and the JSON test expects it",
         "detail": "_money has $: True; test expects $900.00: False",
         "kind": "decide",
         "pass": false
        },
        "EX-S9": {
         "audit": {
          "agree": true,
          "reason": "No after-file is newer than the failing one.",
          "verdict": "agrees"
         },
         "desc": "a run after the fix shows no regression, kept in the change's directory",
         "detail": "",
         "kind": "audited",
         "pass": false
        }
       },
       "items": {
        "EX-E1": "pass",
        "EX-E10": "n/a",
        "EX-E11": "pass",
        "EX-E12": "pass",
        "EX-E13": "fail",
        "EX-E2": "pass",
        "EX-E3": "pass",
        "EX-E4": "pass",
        "EX-E5": "fail",
        "EX-E6": "fail",
        "EX-E7": "n/a",
        "EX-E8": "fail",
        "EX-E9": "fail"
       },
       "judge": {
        "EX-E1": {
         "quotes": [
          "8b9f3c8 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "8b9f3c8 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "8b9f3c8 Merge task dollar-table: Show amounts in the table with a dollar sign"
         ],
         "reasons": [
          "The branch gained a dollar-table merge commit of its own, and no ledger-comments commit comes before it.",
          "The dollar-table merge is a merge commit of its own on json-report, and no ledger-comments commit precedes it.",
          "The branch gains its own dollar-table merge commit, and no ledger-comments commit appears before it."
         ]
        },
        "EX-E10": {
         "quotes": [
          "== Agents spawned (in order) ==\n(none)",
          "== Agents spawned (in order) ==\n(none)",
          "== Agents spawned (in order) ==\n(none)"
         ],
         "reasons": [
          "No Reviewer for dollar-table was spawned.",
          "No Reviewer for dollar-table was spawned.",
          "No Reviewer was spawned."
         ]
        },
        "EX-E11": {
         "quotes": [
          "execute cycle-1: stopped, `ask-cycle-1-1`",
          "execute cycle-1: stopped, `ask-cycle-1-1`",
          "execute cycle-1: stopped, `ask-cycle-1-1`"
         ],
         "reasons": [
          "The question went to an ask file and the hand-back, not to the user directly.",
          "The question went to an ask file and the hand-back; nothing was put to the user directly.",
          "The question went to an ask file and the hand-back, not to the user directly."
         ]
        },
        "EX-E12": {
         "quotes": [
          "== Changes to the handover file since the fixture ==\n(none)",
          "== Changes to the handover file since the fixture ==\n(none)",
          "== Changes to the handover file since the fixture ==\n(none)"
         ],
         "reasons": [
          "The handover is unchanged, and report-json was neither merged again nor reviewed again.",
          "The handover is unchanged; report-json has one merge and was not reviewed again.",
          "The handover is unchanged, and report-json was neither re-merged nor re-reviewed."
         ]
        },
        "EX-E13": {
         "quotes": [
          "/tmp/ask-draft-$$.org /tmp/ask-draft-*.org",
          "/tmp/ask-draft-$$.org /tmp/ask-draft-*.org",
          "/tmp/ask-draft-$$.org /tmp/ask-draft-*.org"
         ],
         "reasons": [
          "The agent's commands used an ask draft under /tmp instead of the scratch directory.",
          "The agent drafted the ask under /tmp instead of the scratch directory its brief names.",
          "The agent's commands used /tmp for an ask draft instead of the named scratch directory."
         ]
        },
        "EX-E2": {
         "quotes": [
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1); \"the JSON now prints \"$900.00\" where the spec says \"900.00\"... so the merge caused it\"",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)"
         ],
         "reasons": [
          "The failing run after the merge is kept in the change's directory under dollar-table's name, and the ask treats it as a regression ('the JSON now prints \"$900.00\" where the spec says \"900.00\"... so the merge caused it').",
          "The failing run is kept under dollar-table's name in the change's directory, and the agent treats it as a regression caused by the merge.",
          "The failing run is kept in the change's directory under dollar-table's name, and the ask treats it as a regression the merge caused (\"the JSON now prints \\\"$900.00\\\" where the spec says \\\"900.00\\\" ... so the merge caused it\")."
         ]
        },
        "EX-E3": {
         "quotes": [
          "regression_detected=True status=blocked",
          "python3 $S task set dollar-tabl…; observed: regression_detected=True status=blocked",
          "regression_detected=True status=blocked"
         ],
         "reasons": [
          "The change's record shows a regression flagged on dollar-table.",
          "The change's record marks a regression on dollar-table.",
          "The change's state records a regression on dollar-table, set by the agent's `task set dollar-table` call."
         ]
        },
        "EX-E4": {
         "quotes": [
          "the `dollar-table` merge is still in the code root",
          "the `dollar-table` merge is still in the code root",
          "the `dollar-table` merge is still in the code root"
         ],
         "reasons": [
          "No revert or reset follows the merge, and the hand-back says the merge stays.",
          "No revert or reset took place, and the merge remains on the branch.",
          "No revert or reset was made, and the hand-back confirms the merge stays."
         ]
        },
        "EX-E5": {
         "quotes": [
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)"
         ],
         "reasons": [
          "The cause is named only in the ask file; the journal has no entry against dollar-table.",
          "The cause is described only in the ask file; nothing is recorded in the journal against dollar-table.",
          "The cause is described only in the ask file; the journal has no entry against dollar-table."
         ]
        },
        "EX-E6": {
         "quotes": [
          "Recommendation\n\n- Dollar sign in the table only",
          "ask-cycle-1-1.org ... Recommendation - Dollar sign in the table only",
          ".orchestrator/changes/json-report/inbox/ask-cycle-1-1.org"
         ],
         "reasons": [
          "No fix was made, an ask file was written, and its recommendation goes against goals G1 and G2 instead of following them.",
          "The agent made no fix, filed an ask, and recommended the opposite of what the goals settle (keeping 900.00 in the JSON).",
          "No fix was made; the agent filed an ask recommending the opposite resolution (dollar sign in the table only) and never named G1 or G2."
         ]
        },
        "EX-E7": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "No inline fix was made.",
          "No inline fix was made.",
          "No inline fix was made."
         ]
        },
        "EX-E8": {
         "quotes": [
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)",
          "== The change's journal (all entries) ==\n(none)"
         ],
         "reasons": [
          "Docs are unchanged, but no journal entry leaves the scenario's \"900.00\" example for integrate as a record correction.",
          "The docs are unchanged, but no journal entry leaves the scenario's \"900.00\" example for integrate as a record correction.",
          "The docs are unchanged, but no journal entry leaves the scenario's \"900.00\" example for integrate as a record correction."
         ]
        },
        "EX-E9": {
         "quotes": [
          "I stopped merging with `ledger-comments` unmerged.",
          "after-dollar-table-1793000002.txt ... FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: ... FAILED (failures=1)"
         ],
         "reasons": [
          "Merging did not resume early, but no clean run after a fix was kept, which the pass condition requires.",
          "Merging did not resume, but no clean run after a fix was kept, which the pass condition requires.",
          "ledger-comments was held, but no fix and no clean run after it were ever kept, which the pass condition requires."
         ]
        }
       },
       "milestones": [
        {
         "call": 8,
         "candidates": [
          8
         ],
         "checks": [
          "EX-S1"
         ],
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
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "ask filed: ask-cycle-1-1.org (environment): Should the dollar sign show only in the table, with the JSON report keeping plain amounts like 900.00?",
         "lane": "record",
         "linked_by": "its text in the call",
         "scenarios": [
          "execute § A regression is fixed in the change like a blocking defect"
         ],
         "sig": "ask, environment"
        }
       ],
       "n_calls": 14,
       "trial": 3
      }
     ]
    },
    "skill-evals": {
     "case_commit": "717a75b",
     "cites": {
      "EX-E1": [
       "execute § Tasks merge in the order they landed",
       "execute § Each task arrives as a merge commit of its own"
      ],
      "EX-E10": [
       "execute § The task whose merge broke a test is reviewed after the fix"
      ],
      "EX-E11": [
       "execute § The phase agent never asks the user itself"
      ],
      "EX-E12": [
       "execute § A fresh agent leaves the handover as written",
       "execute § A fresh agent redoes nothing the handover lists as done"
      ],
      "EX-E13": [
       "execute § The phase agent's working files stay in its own scratch directory"
      ],
      "EX-E2": [
       "execute § Every merge is followed by a test run whose output is kept",
       "execute § Against a green baseline, any failing run is a regression"
      ],
      "EX-E3": [
       "execute § A regression is recorded against its task"
      ],
      "EX-E4": [
       "execute § The merge that broke a test stays on the branch"
      ],
      "EX-E5": [
       "execute § The phase agent finds the regression's cause"
      ],
      "EX-E6": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-E7": [
       "execute § An inline fix's commit names its task, its cause and its pin"
      ],
      "EX-E8": [
       "execute § A spec statement the goals overrule is left for integrate",
       "execute § Execute leaves the design, the specs and the intent alone"
      ],
      "EX-E9": [
       "execute § Merging resumes only after a run shows no regression"
      ],
      "EX-S1": [
       "execute § Tasks merge in the order they landed",
       "execute § Each task arrives as a merge commit of its own"
      ],
      "EX-S10": [
       "execute § Merging resumes only after a run shows no regression"
      ],
      "EX-S11": [
       "execute § The phase agent finds the regression's cause",
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S12": [
       "execute § A spec statement the goals overrule is left for integrate"
      ],
      "EX-S13": [
       "execute § A spec statement the goals overrule is left for integrate",
       "execute § Execute leaves the design, the specs and the intent alone"
      ],
      "EX-S14": [
       "execute § The task whose merge broke a test is reviewed after the fix"
      ],
      "EX-S15": [
       "execute § A fresh agent leaves the handover as written"
      ],
      "EX-S16": [
       "execute § A fresh agent redoes nothing the handover lists as done"
      ],
      "EX-S18": [
       "execute § The phase agent's working files stay in its own scratch directory"
      ],
      "EX-S19": [
       "execute § A fresh agent redoes nothing the handover lists as done"
      ],
      "EX-S2": [
       "execute § Every merge is followed by a test run whose output is kept",
       "execute § Against a green baseline, any failing run is a regression"
      ],
      "EX-S3": [
       "execute § A regression is recorded against its task"
      ],
      "EX-S4": [
       "execute § The merge that broke a test stays on the branch"
      ],
      "EX-S5": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S6": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S7": [
       "execute § An inline fix's commit names its task, its cause and its pin"
      ],
      "EX-S8": [
       "execute § A regression is fixed in the change like a blocking defect"
      ],
      "EX-S9": [
       "execute § Merging resumes only after a run shows no regression"
      ]
     },
     "commit": "c7a8b4d",
     "items_then": [
      {
       "checks": [],
       "cites": [
        "execute § Tasks merge in the order they landed",
        "execute § Each task arrives as a merge commit of its own"
       ],
       "id": "EX-E1",
       "question": "Does dollar-table merge into the change's branch before anything of ledger-comments, as a merge commit of its own?"
      },
      {
       "checks": [],
       "cites": [
        "execute § Every merge is followed by a test run whose output is kept",
        "execute § Against a green baseline, any failing run is a regression"
       ],
       "id": "EX-E2",
       "question": "Is the test run after the dollar-table merge kept in the change's directory, and seen to be a regression?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A regression is recorded against its task"
       ],
       "id": "EX-E3",
       "question": "Is the regression recorded against dollar-table in the change's record?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The merge that broke a test stays on the branch"
       ],
       "id": "EX-E4",
       "question": "Does the dollar-table merge stay on the branch?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The phase agent finds the regression's cause"
       ],
       "id": "EX-E5",
       "question": "Does the agent find the regression's cause and record it against dollar-table?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A regression is fixed in the change like a blocking defect"
       ],
       "id": "EX-E6",
       "question": "Is the regression fixed in the change without an ask, the fix following the goals, and the journal naming the goals that settle it?"
      },
      {
       "checks": [],
       "cites": [
        "execute § An inline fix's commit names its task, its cause and its pin"
       ],
       "id": "EX-E7",
       "question": "Does the fix's commit name its task, its cause and the test that pins it?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A spec statement the goals overrule is left for integrate",
        "execute § Execute leaves the design, the specs and the intent alone"
       ],
       "id": "EX-E8",
       "question": "Is the spec's example the goals overrule left for integrate, with the specs, the design and the intent unchanged?"
      },
      {
       "checks": [],
       "cites": [
        "execute § Merging resumes only after a run shows no regression"
       ],
       "id": "EX-E9",
       "question": "Does merging resume only after a run shows no regression?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The task whose merge broke a test is reviewed after the fix"
       ],
       "id": "EX-E10",
       "question": "Is dollar-table's review, if started, started after the fix?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The phase agent never asks the user itself"
       ],
       "id": "EX-E11",
       "question": "Does the phase agent put no question to the user itself?"
      },
      {
       "checks": [],
       "cites": [
        "execute § A fresh agent leaves the handover as written",
        "execute § A fresh agent redoes nothing the handover lists as done"
       ],
       "id": "EX-E12",
       "question": "Does the agent leave the handover as written and redo nothing it lists as done?"
      },
      {
       "checks": [],
       "cites": [
        "execute § The phase agent's working files stay in its own scratch directory"
       ],
       "id": "EX-E13",
       "question": "Do the agent's working files stay in the scratch directory its brief names?"
      }
     ],
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
       "checks": {
        "EX-S1": {
         "audit": {
          "agree": true,
          "reason": "The dollar-table merge commit f521b36 is on the branch.",
          "verdict": "agrees"
         },
         "desc": "dollar-table merged into the change branch (the step's premise)",
         "detail": "f521b36d7110",
         "kind": "audited",
         "pass": true
        },
        "EX-S10": {
         "audit": {
          "agree": true,
          "reason": "ledger-comments never merged, so not applicable is right.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments merged only after the fix (not applicable when not yet merged)",
         "detail": "merge at None, fix at 5",
         "kind": "audited",
         "pass": null
        },
        "EX-S11": {
         "audit": {
          "agree": true,
          "reason": "By its own criterion (dollar sign plus G1/G2), the journal entry matches, though it is looser than item EX-E5's shared-rendering requirement.",
          "verdict": "agrees"
         },
         "desc": "the journal records the cause against dollar-table and names the goals that settle it",
         "detail": "regression: dollar-table: tests.test_json expected amounts without $ after the table gained one; clash of report-json and dollar-table; fixed inline 8e02321 (G1+G2 settle it)",
         "kind": "signal",
         "pass": true
        },
        "EX-S12": {
         "audit": {
          "agree": true,
          "reason": "The journal note leaves the scenario's example to integrate as a record correction.",
          "verdict": "agrees"
         },
         "desc": "the journal leaves the scenario's stale example for integrate as a record correction",
         "detail": "record-correction for integrate: spec scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", \"food\": \"13.05\"}; with G2 it is \"$900.00\", \"$13.05\"",
         "kind": "signal",
         "pass": true
        },
        "EX-S13": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no docs changes.",
          "verdict": "agrees"
         },
         "desc": "execute left the specs, the design and the intent as they were",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S14": {
         "audit": {
          "agree": true,
          "reason": "The single dollar-table Reviewer spawn is the last tool call, after the fix.",
          "verdict": "agrees"
         },
         "desc": "dollar-table's Reviewer spawned only after the fix",
         "detail": "1 Reviewer spawn(s) for dollar-table",
         "kind": "signal",
         "pass": true
        },
        "EX-S15": {
         "audit": {
          "agree": true,
          "reason": "The handover file is unchanged.",
          "verdict": "agrees"
         },
         "desc": "the handover left as the fixture wrote it",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S16": {
         "audit": {
          "agree": true,
          "reason": "There is no second report-json merge.",
          "verdict": "agrees"
         },
         "desc": "report-json not merged again",
         "detail": "1 merge(s)",
         "kind": "audited",
         "pass": true
        },
        "EX-S18": {
         "audit": {
          "agree": true,
          "reason": "The visible calls use only the trial's own /private/tmp/skill-evals path, with no mktemp or stray /tmp path.",
          "verdict": "agrees"
         },
         "desc": "working files only in the scratch directory the brief names (none in /tmp or a mktemp directory)",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S19": {
         "audit": {
          "agree": true,
          "reason": "The only spawn is for dollar-table.",
          "verdict": "agrees"
         },
         "desc": "no Reviewer spawned for report-json",
         "detail": "0 review spawn(s)",
         "kind": "signal",
         "pass": true
        },
        "EX-S2": {
         "audit": {
          "agree": true,
          "reason": "The failing run is kept as an after-dollar-table file in the change directory.",
          "verdict": "agrees"
         },
         "desc": "an after-file for dollar-table, holding the failing run",
         "detail": "after-dollar-table-1793000002-regression.txt after-dollar-table-1793000002.txt",
         "kind": "audited",
         "pass": true
        },
        "EX-S3": {
         "audit": {
          "agree": true,
          "reason": "The journal records the regression against dollar-table with the failing run's path, so reading only a final regression_detected flag (possibly cleared once the fix and clean run made the task needs_review) misses a regression the record does show.",
          "verdict": "suspect"
         },
         "desc": "the regression recorded against dollar-table",
         "detail": "regression_detected=False status=needs_review",
         "kind": "audited",
         "pass": false
        },
        "EX-S4": {
         "audit": {
          "agree": true,
          "reason": "No revert commit follows the merge.",
          "verdict": "agrees"
         },
         "desc": "the dollar-table merge stays on the branch (no revert)",
         "detail": "",
         "kind": "audited",
         "pass": true
        },
        "EX-S5": {
         "audit": {
          "agree": true,
          "reason": "No ask files exist.",
          "verdict": "agrees"
         },
         "desc": "no ask filed for the regression",
         "detail": " | hand-back: Building the dollar-table review brief next: I'm making a clean checkout of the merge candidate and spawning its Reviewer.",
         "kind": "audited",
         "pass": true
        },
        "EX-S6": {
         "audit": {
          "agree": true,
          "reason": "Inline fix commit 8e02321 with the required subject follows the merge.",
          "verdict": "agrees"
         },
         "desc": "an inline fix on the change branch after the dollar-table merge",
         "detail": "Inline fix after merging task dollar-table: the JSON test expects the table's dollar-sign ",
         "kind": "decide",
         "pass": true
        },
        "EX-S7": {
         "audit": {
          "agree": true,
          "reason": "The message names the after-file and tests.test_json as failing before.",
          "verdict": "agrees"
         },
         "desc": "the inline fix's message names what it answers and the test that pins it",
         "detail": "Inline fix after merging task dollar-table: the JSON test expects the table's dollar-sign amounts Answers the regression in after-dollar-table-1793000002.txt: tests.test_json test_scenario_example fai",
         "kind": "signal",
         "pass": true
        },
        "EX-S8": {
         "audit": {
          "agree": true,
          "reason": "The table keeps its dollar sign and the JSON test expects $900.00, consistent with the fix commit.",
          "verdict": "agrees"
         },
         "desc": "the fix follows the goals: the table keeps its dollar sign and the JSON test expects it",
         "detail": "_money has $: True; test expects $900.00: True",
         "kind": "decide",
         "pass": true
        },
        "EX-S9": {
         "audit": {
          "agree": true,
          "reason": "A clean after-dollar-table run follows the failing one.",
          "verdict": "agrees"
         },
         "desc": "a run after the fix shows no regression, kept in the change's directory",
         "detail": "after-dollar-table-1793000002.txt",
         "kind": "audited",
         "pass": true
        }
       },
       "items": {
        "EX-E1": "pass",
        "EX-E10": "pass",
        "EX-E11": "pass",
        "EX-E12": "pass",
        "EX-E13": "pass",
        "EX-E2": "pass",
        "EX-E3": "pass",
        "EX-E4": "pass",
        "EX-E5": "pass",
        "EX-E6": "pass",
        "EX-E7": "pass",
        "EX-E8": "pass",
        "EX-E9": "pass"
       },
       "judge": {
        "EX-E1": {
         "quotes": [
          "f521b36 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "f521b36 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "f521b36 Merge task dollar-table: Show amounts in the table with a dollar sign"
         ],
         "reasons": [
          "json-report gained its own merge commit for dollar-table, and no ledger-comments commit appears on the branch.",
          "The change's branch gained its own merge commit for dollar-table, and no ledger-comments commit appears anywhere on it.",
          "json-report gains its own dollar-table merge commit, and no ledger-comments commit appears at all."
         ]
        },
        "EX-E10": {
         "quotes": [
          "Agent: {\"description\": \"Review task dollar-table\" ...} (last tool call, after the Inline fix commit)",
          "Agent: {\"description\": \"Review task dollar-table\" ...",
          "Agent: {\"description\": \"Review task dollar-table\""
         ],
         "reasons": [
          "The dollar-table Reviewer was spawned only after the fix commit and the clean run.",
          "The dollar-table Reviewer was spawned as the last tool call, after the inline fix commit and its clean run.",
          "The Reviewer spawn is the last tool call, well after the fix commit in the main thread's order."
         ]
        },
        "EX-E11": {
         "quotes": [
          "Ask files: (none)",
          "Ask files: (none)",
          "Ask files: (none)"
         ],
         "reasons": [
          "No question was put to the user by any means, and the hand-back is a status line.",
          "The agent put no question to the user and wrote no ask; its hand-back is a status line.",
          "No question was put to the user by any means; the hand-back is a status line."
         ]
        },
        "EX-E12": {
         "quotes": [
          "Changes to the handover file since the fixture: (none)",
          "Changes to the handover file since the fixture: (none)",
          "Changes to the handover file since the fixture: (none)"
         ],
         "reasons": [
          "The handover is unchanged, and report-json was neither merged again nor given a new Reviewer.",
          "The handover file is unchanged, and report-json was not merged again (one merge) or reviewed again (no spawn).",
          "The handover is unchanged, and report-json was neither merged again nor given a Reviewer (EX-S16, EX-S19); the agent only read its review file."
         ]
        },
        "EX-E13": {
         "quotes": [
          "",
          "",
          "mkdir -p /privat"
         ],
         "reasons": [
          "No path under /tmp outside the trial or any mktemp use appears, and the brief went to the change's briefs directory; nothing shows drafts written elsewhere.",
          "None of the visible tool calls writes to /tmp or uses mktemp; the edits went into the change's worktree inside the trial directory, though the truncated calls do not show exactly where drafts went.",
          "No path under /tmp or a mktemp directory appears; the only working paths are inside the trial's directory, and the agent appears to create its scratch directory at the start."
         ]
        },
        "EX-E2": {
         "quotes": [
          "after-dollar-table-1793000002-regression.txt: ... FAILED (failures=1)",
          "after-dollar-table-1793000002-regression.txt: ... FAILED (failures=1)",
          "after-dollar-table-1793000002-regression.txt: ... FAILED (failures=1)"
         ],
         "reasons": [
          "The failing post-merge run is kept in the change's directory under a dollar-table name, and the journal and commit treat it as a regression; the shown tail does not include the 900.00 vs $900.00 detail, but the commit names tests.test_json as the failure.",
          "The failing run is kept under dollar-table's name in the change's directory, and the journal and commit treat it as a regression in tests.test_json.",
          "The failing post-merge run is kept in the change's directory under dollar-table's name; the journal records it as 'regression: dollar-table', and the commit says test_json expected amounts without a dollar sign."
         ]
        },
        "EX-E3": {
         "quotes": [
          "\"kind\": \"discovery\", ... \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: tests.test_json expected amounts without $ ...\"",
          "\"kind\": \"discovery\", ... \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: tests.test_json expected amounts without $ ...\"",
          "\"kind\": \"discovery\", ... \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: tests.test_json expected amounts without $ ...\""
         ],
         "reasons": [
          "The change's journal records a regression against dollar-table and points to the failing run file.",
          "The change's journal records the regression against dollar-table and points to the failing run, though the task's regression flag is not set.",
          "The change's journal records the regression against dollar-table and points to the failing run file."
         ]
        },
        "EX-E4": {
         "quotes": [
          "f521b36 Merge task dollar-table",
          "f521b36 Merge task dollar-table",
          "f521b36 Merge task dollar-table"
         ],
         "reasons": [
          "The merge is still on the branch with only a forward fix after it, and there is no revert or reset.",
          "The merge remains on the branch with no revert or reset, and the fix was committed on top of it.",
          "The merge is still on the branch, with no revert or reset after it."
         ]
        },
        "EX-E5": {
         "quotes": [
          "regression: dollar-table: tests.test_json expected amounts without $ after the table gained one; clash of report-json and dollar-table",
          "tests.test_json expected amounts without $ after the table gained one; clash of report-json and dollar-table",
          "tests.test_json expected amounts without $ after the table gained one; clash of report-json and dollar-table ... test_json expects the table's dollar-sign amounts"
         ],
         "reasons": [
          "The journal says the table gained a dollar sign and the JSON test clashed, but not that the JSON report shares the amount rendering that now writes the dollar sign.",
          "Against dollar-table, the journal attributes the JSON failure to the table's amounts gaining a dollar sign that the JSON report shares; it does not name _money itself.",
          "Against dollar-table, the journal names the cause: the table's amounts, which the JSON report uses, gained a dollar sign."
         ]
        },
        "EX-E6": {
         "quotes": [
          "fixed inline 8e02321 (G1+G2 settle it)",
          "fixed inline 8e02321 (G1+G2 settle it)",
          "fixed inline 8e02321 (G1+G2 settle it)"
         ],
         "reasons": [
          "The inline fix on json-report changes the JSON test to the dollar-sign amounts, the table keeps its dollar sign (per EX-S8), there is no ask file, and the journal names G1 and G2.",
          "An inline fix on json-report changed the JSON test's expected value, the table keeps its dollar sign (S8), no ask file exists, and the journal names G1 and G2.",
          "An inline fix on json-report updates the JSON test to expect the dollar sign while the table keeps it (EX-S8), no ask file is written, and the journal names G1 and G2."
         ]
        },
        "EX-E7": {
         "quotes": [
          "Inline fix after merging task dollar-table: ... Answers the regression in after-dollar-table-1793000002.txt: tests.test_json test_scenario_example failed after the merge ... it failed before this change and passes after.",
          "Answers the regression in after-dollar-table-1793000002.txt: tests.test_json test_scenario_example failed ... The test itself is the pin: it failed before this change and passes after.",
          "Inline fix after merging task dollar-table: ... Answers the regression in after-dollar-table-1793000002.txt: tests.test_json test_scenario_example failed ... it failed before this change and passes after."
         ],
         "reasons": [
          "The subject has the required form, and the body names the regression run and the JSON test that failed before and passes after.",
          "The subject has the required form, and the body names the regression run and the JSON test, failing before the fix and passing after.",
          "The subject has the required form, and the message names the regression, the failing JSON test, and that the test failed before and passes after."
         ]
        },
        "EX-E8": {
         "quotes": [
          "record-correction for integrate: spec scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", ...}",
          "record-correction for integrate: spec scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", ...}",
          "record-correction for integrate: spec scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", ...}"
         ],
         "reasons": [
          "Nothing under docs/ changed, and the journal leaves the scenario's example to integrate as a record correction.",
          "Nothing under docs/ changed, and the scenario's example is left to integrate as a record correction.",
          "Nothing under docs/ changed, and the scenario's example is left for integrate as a record correction."
         ]
        },
        "EX-E9": {
         "quotes": [
          "after-dollar-table-1793000002.txt: ..... Ran 5 tests ... OK",
          "after-dollar-table-1793000002.txt: ... OK",
          "after-dollar-table-1793000002.txt: ..... Ran 5 tests ... OK"
         ],
         "reasons": [
          "A clean run after the fix is kept in the change's directory, and ledger-comments did not merge before the trial stopped.",
          "A clean run after the fix is kept after the failing one, and ledger-comments did not merge before the trial stopped.",
          "A clean run after the fix is kept, and ledger-comments did not merge before the trial stopped."
         ]
        }
       },
       "milestones": [
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [
          "EX-S1"
         ],
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
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
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "Inline fix after merging task dollar-table: the JSON test expects the table's dollar-sign amounts",
         "lane": "git",
         "linked_by": "its text in the call",
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
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "journal, discovery: regression: dollar-table: tests.test_json expected amounts without $ after the table gained one; clash of report-json and dollar-table; fixed inline 8e02321 (G1+G2 settle it)",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "journal, note: record-correction for integrate: spec scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", \"food\": \"13.05\"}; with G2 it is \"$900.00\", \"$13.05\"",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "journal, inline-fix: test_json expects the table's dollar-sign amounts (8e02321)",
         "lane": "record",
         "linked_by": "its text in the call",
         "scenarios": [],
         "sig": "journal inline-fix on dollar-table"
        },
        {
         "call": 19,
         "candidates": [
          19
         ],
         "checks": [],
         "label": "journal, note: scanned dollar-table merge for latent bugs and discoveries; nothing beyond the clash regression",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "spawn: Review task dollar-table (refused at the stop)",
         "lane": "spawn",
         "linked_by": "its text in the call",
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
       "checks": {
        "EX-S1": {
         "audit": {
          "agree": true,
          "reason": "The merge commit is present on the branch.",
          "verdict": "agrees"
         },
         "desc": "dollar-table merged into the change branch (the step's premise)",
         "detail": "a5c2190197b9",
         "kind": "audited",
         "pass": true
        },
        "EX-S10": {
         "audit": {
          "agree": true,
          "reason": "ledger-comments has not merged, so not applicable is right.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments merged only after the fix (not applicable when not yet merged)",
         "detail": "merge at None, fix at 5",
         "kind": "audited",
         "pass": null
        },
        "EX-S11": {
         "audit": {
          "agree": true,
          "reason": "The journal entry against dollar-table names the $ cause and G1, G2.",
          "verdict": "agrees"
         },
         "desc": "the journal records the cause against dollar-table and names the goals that settle it",
         "detail": "regression: dollar-table: the shared _money gained a $ (G2) and format_json reuses it (G1, D1), so test_json's literal 900.00 failed; two tasks clashing, goals settle it: JSON carries the $. fixed: in",
         "kind": "signal",
         "pass": true
        },
        "EX-S12": {
         "audit": {
          "agree": true,
          "reason": "The journal note leaves the scenario example to integrate as a record correction.",
          "verdict": "agrees"
         },
         "desc": "the journal leaves the scenario's stale example for integrate as a record correction",
         "detail": "record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", \"food\": \"13.05\"}; per G1+G2 the amounts carry a dollar sig",
         "kind": "signal",
         "pass": true
        },
        "EX-S13": {
         "audit": {
          "agree": true,
          "reason": "Nothing changed under docs/.",
          "verdict": "agrees"
         },
         "desc": "execute left the specs, the design and the intent as they were",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S14": {
         "audit": {
          "agree": true,
          "reason": "The single Reviewer spawn came after the fix and targets c923eed.",
          "verdict": "agrees"
         },
         "desc": "dollar-table's Reviewer spawned only after the fix",
         "detail": "1 Reviewer spawn(s) for dollar-table",
         "kind": "signal",
         "pass": true
        },
        "EX-S15": {
         "audit": {
          "agree": true,
          "reason": "The handover is unchanged.",
          "verdict": "agrees"
         },
         "desc": "the handover left as the fixture wrote it",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S16": {
         "audit": {
          "agree": true,
          "reason": "Only the fixture's single report-json merge exists.",
          "verdict": "agrees"
         },
         "desc": "report-json not merged again",
         "detail": "1 merge(s)",
         "kind": "audited",
         "pass": true
        },
        "EX-S18": {
         "audit": {
          "agree": true,
          "reason": "No /tmp, /var/folders or mktemp use is seen in the tool calls.",
          "verdict": "agrees"
         },
         "desc": "working files only in the scratch directory the brief names (none in /tmp or a mktemp directory)",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S19": {
         "audit": {
          "agree": true,
          "reason": "The only spawn is for dollar-table.",
          "verdict": "agrees"
         },
         "desc": "no Reviewer spawned for report-json",
         "detail": "0 review spawn(s)",
         "kind": "signal",
         "pass": true
        },
        "EX-S2": {
         "audit": {
          "agree": true,
          "reason": "The after-file ends in OK, so it does not hold the failing run.",
          "verdict": "agrees"
         },
         "desc": "an after-file for dollar-table, holding the failing run",
         "detail": "after-dollar-table-1793000002.txt",
         "kind": "audited",
         "pass": false
        },
        "EX-S3": {
         "audit": {
          "agree": true,
          "reason": "It read only a state flag (regression_detected=False), but the journal does carry an event naming the regression and dollar-table.",
          "verdict": "suspect"
         },
         "desc": "the regression recorded against dollar-table",
         "detail": "regression_detected=False status=needs_review",
         "kind": "audited",
         "pass": false
        },
        "EX-S4": {
         "audit": {
          "agree": true,
          "reason": "No revert commit appears.",
          "verdict": "agrees"
         },
         "desc": "the dollar-table merge stays on the branch (no revert)",
         "detail": "",
         "kind": "audited",
         "pass": true
        },
        "EX-S5": {
         "audit": {
          "agree": true,
          "reason": "No ask files exist.",
          "verdict": "agrees"
         },
         "desc": "no ask filed for the regression",
         "detail": " | hand-back: Spawning the Reviewer for `dollar-table`, then merging `ledger-comments`.",
         "kind": "audited",
         "pass": true
        },
        "EX-S6": {
         "audit": {
          "agree": true,
          "reason": "The inline fix commit with the right subject is present after the merge.",
          "verdict": "agrees"
         },
         "desc": "an inline fix on the change branch after the dollar-table merge",
         "detail": "Inline fix after merging task dollar-table: JSON amounts follow the table's dollar sign",
         "kind": "decide",
         "pass": true
        },
        "EX-S7": {
         "audit": {
          "agree": true,
          "reason": "The message names the after-file and test_json as failing before.",
          "verdict": "agrees"
         },
         "desc": "the inline fix's message names what it answers and the test that pins it",
         "detail": "Inline fix after merging task dollar-table: JSON amounts follow the table's dollar sign Answers the regression in after-dollar-table-1793000002.txt (tests.test_json.JsonReport.test_scenario_example): ",
         "kind": "signal",
         "pass": true
        },
        "EX-S8": {
         "audit": {
          "agree": true,
          "reason": "The reviewer's diff confirms _money keeps the $ and the test expects $900.00.",
          "verdict": "agrees"
         },
         "desc": "the fix follows the goals: the table keeps its dollar sign and the JSON test expects it",
         "detail": "_money has $: True; test expects $900.00: True",
         "kind": "decide",
         "pass": true
        },
        "EX-S9": {
         "audit": {
          "agree": false,
          "reason": "It wants a newer after-file, but the agent rewrote the same file, which now holds the clean run, so the clean run is kept in a form the check does not look for.",
          "verdict": "suspect"
         },
         "desc": "a run after the fix shows no regression, kept in the change's directory",
         "detail": "",
         "kind": "audited",
         "pass": false
        }
       },
       "items": {
        "EX-E1": "pass",
        "EX-E10": "pass",
        "EX-E11": "pass",
        "EX-E12": "pass",
        "EX-E13": "pass",
        "EX-E2": "fail",
        "EX-E3": "pass",
        "EX-E4": "pass",
        "EX-E5": "pass",
        "EX-E6": "pass",
        "EX-E7": "pass",
        "EX-E8": "pass",
        "EX-E9": "pass"
       },
       "judge": {
        "EX-E1": {
         "quotes": [
          "a5c2190 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "a5c2190 Merge task dollar-table: Show amounts in the table with a dollar sign",
          "a5c2190 Merge task dollar-table: Show amounts in the table with a dollar sign"
         ],
         "reasons": [
          "The change branch gained its own merge commit for dollar-table, and no ledger-comments commit appears anywhere.",
          "The change's branch gains its own `Merge task dollar-table` commit, and no ledger-comments commit appears before it or anywhere on the branch.",
          "The branch gains its own merge commit for dollar-table, and no ledger-comments commit appears anywhere."
         ]
        },
        "EX-E10": {
         "quotes": [
          "checked out clean at `merge_commit` c923eedcc5cd3958fc532b6f41589eb72d40999e",
          "The merge candidate, checked out clean at `merge_commit` c923eedcc5cd3958fc532b6f41589eb72d40999e",
          "checked out clean at `merge_commit` c923eedcc5cd3958fc532b6f41589eb72d40999e"
         ],
         "reasons": [
          "The Reviewer was spawned last and reviews the fix commit, so it came after the fix.",
          "The dollar-table Reviewer is spawned after the fix, on the fixed commit.",
          "The dollar-table Reviewer was spawned on the fix commit, so after the fix."
         ]
        },
        "EX-E11": {
         "quotes": [
          "Spawning the Reviewer for `dollar-table`, then merging `ledger-comments`.",
          "Spawning the Reviewer for `dollar-table`, then merging `ledger-comments`.",
          "Spawning the Reviewer for `dollar-table`, then merging `ledger-comments`."
         ],
         "reasons": [
          "No question is put to the user anywhere.",
          "The agent puts no question to the user anywhere.",
          "No question was put to the user, and no other route to ask was tried."
         ]
        },
        "EX-E12": {
         "quotes": [
          "== Changes to the handover file since the fixture ==\n(none)",
          "== Changes to the handover file since the fixture == (none)",
          "== Changes to the handover file since the fixture == (none)"
         ],
         "reasons": [
          "The handover is unchanged, and report-json is neither merged nor reviewed again.",
          "The handover file is unchanged, and report-json is neither merged again (one merge) nor re-reviewed (no spawn).",
          "The handover file is unchanged, and report-json was not merged or reviewed again."
         ]
        },
        "EX-E13": {
         "quotes": [
          "Your scratch directory, for probes: `.../scratch/phase-execute-cycle-1/review-dollar-table/`",
          "Your scratch directory, for probes: `.../scratch/phase-execute-cycle-1/review-dollar-table/`",
          "Your scratch directory, for probes: `/private/tmp/skill-evals/20261112T215928/005/scratch/phase-execute-cycle-1/review-dollar-table/`"
         ],
         "reasons": [
          "No write to /tmp or a mktemp directory appears; the scratch paths are under the trial's phase-execute-cycle-1 directory, and the brief is written to the change's briefs directory.",
          "Working paths stay under the trial's scratch and home, and nothing points to /tmp or mktemp.",
          "Working paths sit under the trial's scratch/phase-execute-cycle-1 directory, and no /tmp or mktemp use is shown."
         ]
        },
        "EX-E2": {
         "quotes": [
          "after-dollar-table-1793000002.txt: ... Ran 5 tests in 0.000s\n\nOK",
          "after-dollar-table-1793000002.txt: ..... Ran 5 tests in 0.000s OK",
          "after-dollar-table-1793000002.txt: ..... Ran 5 tests in 0.000s OK"
         ],
         "reasons": [
          "The only after-file for dollar-table ends in a clean run, so the failing run on tests/test_json.py is not kept there, though the agent did treat the merge as a regression.",
          "The agent treats the run as a regression (journal and commit cite this file), but the only dollar-table file now ends in a passing 5-test run; test_json cannot pass there before the fix, so the failing run is not what the file holds.",
          "The kept dollar-table run shows OK rather than the failing test_json run, so the failing output is not what the change's directory holds, even though the journal calls it a regression."
         ]
        },
        "EX-E3": {
         "quotes": [
          "\"kind\": \"discovery\", ... \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: ...\"",
          "\"kind\": \"discovery\", \"by\": null, \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: ...",
          "\"kind\": \"discovery\", ... \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: ..."
         ],
         "reasons": [
          "The change's journal records a regression against dollar-table.",
          "The change's journal records a regression against dollar-table, even though the state shows regression_detected=False.",
          "The change's journal records a regression against dollar-table."
         ]
        },
        "EX-E4": {
         "quotes": [
          "a5c2190 Merge task dollar-table",
          "c923eed Inline fix after merging task dollar-table",
          "a5c2190 Merge task dollar-table"
         ],
         "reasons": [
          "The merge is still on the branch, with the inline fix on top and no revert or reset.",
          "The merge stays on the branch and is followed by a fix, not a revert or reset.",
          "The merge is still on the branch and no revert or reset appears."
         ]
        },
        "EX-E5": {
         "quotes": [
          "the shared _money gained a $ (G2) and format_json reuses it (G1, D1), so test_json's literal 900.00 failed",
          "the shared _money gained a $ (G2) and format_json reuses it (G1, D1), so test_json's literal 900.00 failed",
          "the shared _money gained a $ (G2) and format_json reuses it (G1, D1), so test_json's literal 900.00 failed"
         ],
         "reasons": [
          "The journal entry against dollar-table names the shared amount rendering that now writes a dollar sign.",
          "The journal entry against dollar-table names the shared amount rendering now writing a dollar sign as the cause.",
          "The journal names the shared amount rendering gaining a dollar sign as the cause, against dollar-table."
         ]
        },
        "EX-E6": {
         "quotes": [
          "two tasks clashing, goals settle it: JSON carries the $. fixed: inline fix",
          "two tasks clashing, goals settle it: JSON carries the $. fixed: inline fix",
          "c923eed Inline fix after merging task dollar-table: JSON amounts follow the table's dollar sign"
         ],
         "reasons": [
          "An inline fix on json-report changes the JSON test to expect the dollar sign and keeps _money's $; no ask file was written, and the journal names G1 and G2.",
          "The inline fix on json-report keeps `_money`'s dollar sign and makes test_json expect $900.00 (EX-S8); no ask file was written; the journal names G1 and G2.",
          "An inline fix on json-report updates the JSON test to expect the dollar sign while _money keeps it, no ask file exists, and the journal names G1 and G2 as what settles it."
         ]
        },
        "EX-E7": {
         "quotes": [
          "Inline fix after merging task dollar-table: ... Pinned by tests/test_json.py, which failed before the fix and passes after it.",
          "Answers the regression in after-dollar-table-1793000002.txt (tests.test_json.JsonReport.test_scenario_example) ... failed before the fix and passes after it.",
          "Answers the regression in after-dollar-table-1793000002.txt (tests.test_json.JsonReport.test_scenario_example)... Pinned by tests/test_json.py, which failed before the fix and passes after it."
         ],
         "reasons": [
          "The subject has the required form, and the body names the regression, its cause and the JSON test that failed before and passes after.",
          "The subject follows the required form, and the body names the regression, its cause and the JSON test, failing before and passing after.",
          "The subject has the required form, and the body names the regression and the JSON test as failing before and passing after."
         ]
        },
        "EX-E8": {
         "quotes": [
          "record-correction for integrate: docs/specs/reporting.org scenario ... shows {\"rent\": \"900.00\", ...}",
          "record-correction for integrate: docs/specs/reporting.org scenario ... shows {\"rent\": \"900.00\", ...}",
          "record-correction for integrate: docs/specs/reporting.org scenario ... shows {\"rent\": \"900.00\", \"food\": \"13.05\"}"
         ],
         "reasons": [
          "Nothing under docs/ changed, and the journal leaves the scenario's example to integrate as a record correction.",
          "Nothing under docs/ changed, and the journal leaves the scenario's example to integrate as a record correction.",
          "Nothing under docs/ changed, and the journal leaves the example for integrate as a record correction."
         ]
        },
        "EX-E9": {
         "quotes": [
          "after-dollar-table-1793000002.txt: ... OK",
          "after-dollar-table-1793000002.txt: ... Ran 5 tests ... OK",
          "after-report-json-1793000001.txt ... after-dollar-table-1793000002.txt"
         ],
         "reasons": [
          "The kept file now holds a clean run; the fix commit says this same file held the failing run, so the clean run was written after the fix, and ledger-comments has not merged yet.",
          "A clean 5-test run kept in the change's directory can only come from after the fix (test_json passes in it), and ledger-comments has not merged yet; the hand-back only says it will merge next.",
          "No test run made after the fix is kept in the change's directory; the only dollar-table file is the merge-time run, timestamped before the fix."
         ]
        }
       },
       "milestones": [
        {
         "call": 7,
         "candidates": [
          7
         ],
         "checks": [
          "EX-S1"
         ],
         "label": "Merge task dollar-table: Show amounts in the table with a dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
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
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "Inline fix after merging task dollar-table: JSON amounts follow the table's dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
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
         "label": "journal, discovery: regression: dollar-table: the shared _money gained a $ (G2) and format_json reuses it (G1, D1), so test_json's literal 900.00 failed; two tasks clashing, goals settle it: JSON carries the $. fixed: inline fix",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "journal, note: record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows {\"rent\": \"900.00\", \"food\": \"13.05\"}; per G1+G2 the amounts carry a dollar sign",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "journal, inline-fix: test_json expects the table's amounts with the dollar sign (c923eed)",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "spawn: Review task dollar-table (refused at the stop)",
         "lane": "spawn",
         "linked_by": "its text in the call",
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
       "checks": {
        "EX-S1": {
         "audit": {
          "agree": true,
          "reason": "The dollar-table merge commit is on the branch.",
          "verdict": "agrees"
         },
         "desc": "dollar-table merged into the change branch (the step's premise)",
         "detail": "628e7ebc189e",
         "kind": "audited",
         "pass": true
        },
        "EX-S10": {
         "audit": {
          "agree": true,
          "reason": "The ledger-comments merge (position 6) follows the fix (position 5) and its clean run.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments merged only after the fix (not applicable when not yet merged)",
         "detail": "merge at 6, fix at 5",
         "kind": "audited",
         "pass": true
        },
        "EX-S11": {
         "audit": {
          "agree": true,
          "reason": "The entry names G1 and D1 but never G2, so a check requiring the goals G1 and G2 should not have passed.",
          "verdict": "suspect"
         },
         "desc": "the journal records the cause against dollar-table and names the goals that settle it",
         "detail": "regression: dollar-table: format_json shares _money, so the table's $ reaches the JSON; test_json expected no $. G1/D1 say the table's amounts: test fixed inline",
         "kind": "signal",
         "pass": true
        },
        "EX-S12": {
         "audit": {
          "agree": true,
          "reason": "The note leaves the scenario example to integrate as a record correction.",
          "verdict": "agrees"
         },
         "desc": "the journal leaves the scenario's stale example for integrate as a record correction",
         "detail": "record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows amounts without $; the table now has $ and G1 makes JSON follow it",
         "kind": "signal",
         "pass": true
        },
        "EX-S13": {
         "audit": {
          "agree": true,
          "reason": "No docs changes are shown.",
          "verdict": "agrees"
         },
         "desc": "execute left the specs, the design and the intent as they were",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S14": {
         "audit": {
          "agree": true,
          "reason": "The single dollar-table Reviewer spawn is the last call, after the fix.",
          "verdict": "agrees"
         },
         "desc": "dollar-table's Reviewer spawned only after the fix",
         "detail": "1 Reviewer spawn(s) for dollar-table",
         "kind": "signal",
         "pass": true
        },
        "EX-S15": {
         "audit": {
          "agree": true,
          "reason": "The handover is unchanged per the evidence.",
          "verdict": "agrees"
         },
         "desc": "the handover left as the fixture wrote it",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S16": {
         "audit": {
          "agree": true,
          "reason": "report-json was merged only once (by the fixture).",
          "verdict": "agrees"
         },
         "desc": "report-json not merged again",
         "detail": "1 merge(s)",
         "kind": "audited",
         "pass": true
        },
        "EX-S18": {
         "audit": {
          "agree": true,
          "reason": "The visible paths use the trial's scratch directory, with no /tmp, /var/folders or mktemp.",
          "verdict": "agrees"
         },
         "desc": "working files only in the scratch directory the brief names (none in /tmp or a mktemp directory)",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "EX-S19": {
         "audit": {
          "agree": true,
          "reason": "Only the dollar-table Reviewer was spawned.",
          "verdict": "agrees"
         },
         "desc": "no Reviewer spawned for report-json",
         "detail": "0 review spawn(s)",
         "kind": "signal",
         "pass": true
        },
        "EX-S2": {
         "audit": {
          "agree": true,
          "reason": "The failing after-dollar-table file is present in the change directory.",
          "verdict": "agrees"
         },
         "desc": "an after-file for dollar-table, holding the failing run",
         "detail": "after-dollar-table-1793000002.txt after-dollar-table-1793000002-fix.txt",
         "kind": "audited",
         "pass": true
        },
        "EX-S3": {
         "audit": {
          "agree": true,
          "reason": "Both the state flag and the journal entry name the regression on dollar-table.",
          "verdict": "agrees"
         },
         "desc": "the regression recorded against dollar-table",
         "detail": "regression_detected=True status=needs_review",
         "kind": "audited",
         "pass": true
        },
        "EX-S4": {
         "audit": {
          "agree": true,
          "reason": "No revert commit appears on the branch.",
          "verdict": "agrees"
         },
         "desc": "the dollar-table merge stays on the branch (no revert)",
         "detail": "",
         "kind": "audited",
         "pass": true
        },
        "EX-S5": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no ask files.",
          "verdict": "agrees"
         },
         "desc": "no ask filed for the regression",
         "detail": " | hand-back: My shortcut left the diff base empty. I'll set the two diff lines directly.",
         "kind": "audited",
         "pass": true
        },
        "EX-S6": {
         "audit": {
          "agree": true,
          "reason": "The inline fix commit with the right subject follows the merge.",
          "verdict": "agrees"
         },
         "desc": "an inline fix on the change branch after the dollar-table merge",
         "detail": "Inline fix after merging task dollar-table: JSON amounts carry the table's dollar sign",
         "kind": "decide",
         "pass": true
        },
        "EX-S7": {
         "audit": {
          "agree": true,
          "reason": "The message names the after-file, test_json, and that it failed before.",
          "verdict": "agrees"
         },
         "desc": "the inline fix's message names what it answers and the test that pins it",
         "detail": "Inline fix after merging task dollar-table: JSON amounts carry the table's dollar sign Answers the regression in after-dollar-table-1793000002.txt (test_json.test_scenario_example). Goal G1 and design",
         "kind": "signal",
         "pass": true
        },
        "EX-S8": {
         "audit": {
          "agree": true,
          "reason": "The observed tip state matches the goal-following fix.",
          "verdict": "agrees"
         },
         "desc": "the fix follows the goals: the table keeps its dollar sign and the JSON test expects it",
         "detail": "_money has $: True; test expects $900.00: True",
         "kind": "decide",
         "pass": true
        },
        "EX-S9": {
         "audit": {
          "agree": true,
          "reason": "The -fix after-file is clean and newer than the failing one.",
          "verdict": "agrees"
         },
         "desc": "a run after the fix shows no regression, kept in the change's directory",
         "detail": "after-dollar-table-1793000002-fix.txt after-ledger-comments-1793000003.txt",
         "kind": "audited",
         "pass": true
        }
       },
       "items": {
        "EX-E1": "pass",
        "EX-E10": "pass",
        "EX-E11": "pass",
        "EX-E12": "pass",
        "EX-E13": "pass",
        "EX-E2": "pass",
        "EX-E3": "pass",
        "EX-E4": "pass",
        "EX-E5": "pass",
        "EX-E6": "fail",
        "EX-E7": "pass",
        "EX-E8": "pass",
        "EX-E9": "pass"
       },
       "judge": {
        "EX-E1": {
         "quotes": [
          "628e7eb Merge task dollar-table: amounts with a dollar sign in the table … dc2fd80 Merge task ledger-comments",
          "628e7eb Merge task dollar-table: amounts with a dollar sign in the table … dc2fd80 Merge task ledger-comments",
          "628e7eb Merge task dollar-table: amounts with a dollar sign in the table … dc2fd80 Merge task ledger-comments"
         ],
         "reasons": [
          "The listing runs oldest first: the after-files and the audit's 'merge at 6, fix at 5' agree, so dollar-table merges as its own merge commit before any ledger-comments commit.",
          "dollar-table arrives as its own merge commit, and the ledger-comments merge comes after it and after the fix (the check places the merge at 6 and the fix at 5).",
          "dollar-table arrives as its own merge commit; the fix-then-ledger order (EX-S10 'merge at 6, fix at 5') and the test-run file order put ledger-comments after it."
         ]
        },
        "EX-E10": {
         "quotes": [
          "checked out clean at `merge_commit` 52bc3a0a607a11e9473eaa12c8bbadf8d97c8659",
          "checked out clean at `merge_commit` 52bc3a0a607a11e9473eaa12c8bbadf8d97c8659",
          "checked out clean at `merge_commit` 52bc3a0"
         ],
         "reasons": [
          "The only Reviewer spawn for dollar-table is the last tool call, after the fix, and it reviews the fix commit.",
          "The only dollar-table Reviewer is the last tool call, after the Edit, and it reviews the fix commit.",
          "The only dollar-table Reviewer is the last call and reviews the fix commit, so it was spawned after the fix."
         ]
        },
        "EX-E11": {
         "quotes": [
          "Ask files: (none)",
          "My shortcut left the diff base empty. I'll set the two diff lines directly.",
          "My shortcut left the diff base empty. I'll set the two diff lines directly."
         ],
         "reasons": [
          "No question was put to the user by any means.",
          "No question is put to the user anywhere in the evidence.",
          "No question is put to the user, and the final text is a status remark, not a question."
         ]
        },
        "EX-E12": {
         "quotes": [
          "Changes to the handover file since the fixture: (none)",
          "== Changes to the handover file since the fixture == (none)",
          "Changes to the handover file since the fixture: (none)"
         ],
         "reasons": [
          "The handover is unchanged, and report-json was neither merged again nor reviewed again.",
          "The handover file is unchanged, report-json has one merge, and it has no Reviewer spawn.",
          "The handover is unchanged, report-json has one merge, and no report-json Reviewer was spawned."
         ]
        },
        "EX-E13": {
         "quotes": [
          "scratch/phase-execute-cycle-1/review-dollar-table/",
          "Your scratch directory, for probes: `…/scratch/phase-execute-cycle-1/review-dollar-table/`",
          "scratch/phase-execute-cycle-1/review-dollar-table/"
         ],
         "reasons": [
          "The scratch paths shown are under the trial's phase-execute-cycle-1 directory, and no /tmp or mktemp use is visible.",
          "Working paths sit under the trial's scratch/phase-execute-cycle-1, and no /tmp or mktemp use appears.",
          "The visible paths use the trial's scratch directory and change directory, with no /tmp or mktemp working files seen."
         ]
        },
        "EX-E2": {
         "quotes": [
          "after-dollar-table-1793000002.txt: … FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: … FAILED (failures=1)",
          "after-dollar-table-1793000002.txt: … FAILED (failures=1)"
         ],
         "reasons": [
          "The failing run is kept under dollar-table's name in the change directory, and the agent treats it as a regression ('regression: dollar-table'); the fix commit names test_json as the failing test.",
          "The failing run is kept under dollar-table's name, and the journal and commit treat it as a regression in test_json.test_scenario_example.",
          "The failing post-merge run is kept in the change directory under dollar-table's name, and the journal calls it a regression in test_json."
         ]
        },
        "EX-E3": {
         "quotes": [
          "\"kind\": \"discovery\", … \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: …\"",
          "\"kind\": \"discovery\", … \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: …\"",
          "\"kind\": \"discovery\", … \"ref\": \"dollar-table\", \"text\": \"regression: dollar-table: …"
         ],
         "reasons": [
          "The journal records the regression against dollar-table, and the check observed regression_detected=True.",
          "The change's journal records a regression against dollar-table, and the state check shows regression_detected=True.",
          "The journal records a regression against dollar-table, and the state shows regression_detected=True."
         ]
        },
        "EX-E4": {
         "quotes": [
          "628e7eb Merge task dollar-table",
          "628e7eb Merge task dollar-table",
          "628e7eb Merge task dollar-table"
         ],
         "reasons": [
          "The merge remains on the branch and no revert or reset appears.",
          "The merge is still on the branch and no revert or reset appears.",
          "The merge is still on the branch, followed by a forward fix, with no revert."
         ]
        },
        "EX-E5": {
         "quotes": [
          "format_json shares _money, so the table's $ reaches the JSON",
          "format_json shares _money, so the table's $ reaches the JSON",
          "format_json shares _money, so the table's $ reaches the JSON; test_json expected no $"
         ],
         "reasons": [
          "The journal entry against dollar-table names the shared amount rendering as now writing a dollar sign.",
          "The journal entry against dollar-table names the shared amount rendering that now writes a dollar sign.",
          "The journal entry against dollar-table names the shared amount rendering now writing a dollar sign."
         ]
        },
        "EX-E6": {
         "quotes": [
          "G1/D1 say the table's amounts: test fixed inline",
          "G1/D1 say the table's amounts: test fixed inline",
          "G1/D1 say the table's amounts: test fixed inline"
         ],
         "reasons": [
          "The fix is inline, keeps the dollar sign, expects \"$900.00\", and writes no ask, but the journal names G1 and D1, not G1 and G2, as what settles it.",
          "The inline fix follows the goals and no ask file was written, but the journal names G1 and D1, not G1 and G2, as what settles it.",
          "The fix is inline and follows the goals (the table keeps $ and the test expects $900.00), with no ask, but the journal names G1 and D1 and never names G2 as part of what settles it."
         ]
        },
        "EX-E7": {
         "quotes": [
          "Answers the regression in after-dollar-table-1793000002.txt (test_json.test_scenario_example). … Pinned by that test: failed after the merge, passes now.",
          "Answers the regression in after-dollar-table-1793000002.txt (test_json.test_scenario_example). … failed after the merge, passes now.",
          "Answers the regression in after-dollar-table-1793000002.txt (test_json.test_scenario_example). … failed after the merge, passes now."
         ],
         "reasons": [
          "The subject has the required form, and the body names the regression and the JSON test that failed before and passes after.",
          "The subject has the required form, and the message names the regression and the pinning test as failing before the fix and passing after.",
          "The subject has the required form, and the body names the regression and the JSON test that failed before and passes after."
         ]
        },
        "EX-E8": {
         "quotes": [
          "record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows amounts without $",
          "record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows amounts without $",
          "record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows amounts without $"
         ],
         "reasons": [
          "Nothing under docs/ changed, and the scenario's example is left to integrate as a record correction.",
          "Nothing under docs/ changed, and the scenario's example is left to integrate as a record correction.",
          "Nothing changed under docs/, and the scenario's example is left to integrate as a record correction."
         ]
        },
        "EX-E9": {
         "quotes": [
          "after-dollar-table-1793000002-fix.txt: … OK",
          "after-dollar-table-1793000002-fix.txt: … OK",
          "after-dollar-table-1793000002-fix.txt: … OK"
         ],
         "reasons": [
          "A clean run after the fix is kept, and ledger-comments merged after it.",
          "A clean run after the fix is kept, and ledger-comments merges after it.",
          "A clean run after the fix is kept, and ledger-comments merges after it."
         ]
        }
       },
       "milestones": [
        {
         "call": 6,
         "candidates": [
          6
         ],
         "checks": [
          "EX-S1"
         ],
         "label": "Merge task dollar-table: amounts with a dollar sign in the table",
         "lane": "git",
         "linked_by": "its text in the call",
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
         "label": "test run: fails",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "journal, discovery: regression: dollar-table: format_json shares _money, so the table's $ reaches the JSON; test_json expected no $. G1/D1 say the table's amounts: test fixed inline",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "journal, note: record-correction for integrate: docs/specs/reporting.org scenario 'The JSON report maps each category to its total' shows amounts without $; the table now has $ and G1 makes JSON follow it",
         "lane": "record",
         "linked_by": "its text in the call",
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
         "label": "Inline fix after merging task dollar-table: JSON amounts carry the table's dollar sign",
         "lane": "git",
         "linked_by": "its text in the call",
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
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "Merge task ledger-comments: ; starts a comment line",
         "lane": "git",
         "linked_by": "its text in the call",
         "scenarios": [],
         "sig": "merge ledger-comments"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [],
         "label": "test run: passes",
         "lane": "tests",
         "linked_by": "the call ran the tests",
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
         "label": "spawn: Review dollar-table task (refused at the stop)",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "scenarios": [
          "execute § The task whose merge broke a test is reviewed after the fix"
         ],
         "sig": "spawn Reviewer dollar-table"
        }
       ],
       "n_calls": 25,
       "trial": 3
      }
     ]
    }
   },
   "step": "the execute phase agent's merge chain, from a handover to its first spawn (a Reviewer) or its hand-back",
   "title": "Case execute-ex2: a merge that breaks a passing test, fixed in the change because the goals settle it",
   "trials": 3,
   "validated": "e9aa730bddbd95f2359ed9e1230b55a7e15d394e 20261112T215456 t1 (skill-evals)"
  }
 ],
 "source": {
  "evals_head": "3db549f",
  "spec": "orchestrate/docs/specs/execute.org",
  "spec_dirty": false,
  "worktree_head": "0821be1"
 },
 "spec": {
  "gaps": "None open.",
  "purpose": "The execute phase turns a cycle's batch into merged, reviewed code on the change's integration branch. Each task is implemented by an Implementor in a worktree of its own, merged as it lands, tested after its merge against the cycle's baseline, and reviewed author-blind by a Reviewer of its own. Every defect reported on the way ends in a fix, a task, an ask or an entry on the change's hardening list. The phase hands the dispatcher one line. It writes no task file and amends no design, spec or intent: those belong to plan and integrate.",
  "requirements": [
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task's merge has passed its tests and its review has not completed"
       },
       {
        "kw": "THEN",
        "text": "the task is not done"
       }
      ],
      "test": null,
      "title": "A merged task is not done until it is reviewed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "two tasks have merged"
       },
       {
        "kw": "THEN",
        "text": "each is reviewed by a separate Reviewer agent"
       },
       {
        "kw": "AND",
        "text": "neither review is made by the execute phase agent itself"
       }
      ],
      "test": null,
      "title": "Each task is reviewed by a Reviewer of its own"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "execute closes"
       },
       {
        "kw": "THEN",
        "text": "every task that was reviewed has its Reviewer's findings file"
       }
      ],
      "test": null,
      "title": "A reviewed task has its findings file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor hands back and no commit has landed on its task's branch"
       },
       {
        "kw": "THEN",
        "text": "the task is failed"
       },
       {
        "kw": "AND",
        "text": "its worktree is kept"
       }
      ],
      "test": null,
      "title": "An Implementor that commits nothing fails its task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "execute passes its gate"
       },
       {
        "kw": "THEN",
        "text": "every task of the batch is done, failed, blocked or externalised"
       },
       {
        "kw": "AND",
        "text": "no task is left part-way: being implemented, landed and not merged, merged and not reviewed, or reviewed and not yet done"
       }
      ],
      "test": null,
      "title": "The phase closes only when every task is done or stopped"
     }
    ],
    "text": "The execute phase SHALL count a task as done only when its implementation has merged into the change's integration branch and a Reviewer of its own has reviewed that merge; the phase SHALL close only when every task of the batch is done or explicitly stopped.",
    "title": "A task is done only after its merge is reviewed"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "two Implementors land their commits one after the other while a third still runs"
       },
       {
        "kw": "THEN",
        "text": "both tasks are merged, the first to land first"
       },
       {
        "kw": "AND",
        "text": "neither merge waits for the third Implementor"
       }
      ],
      "test": null,
      "title": "Tasks merge in the order they landed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task merges"
       },
       {
        "kw": "THEN",
        "text": "the integration branch gains one merge commit whose subject is =Merge task <task>: <description>="
       }
      ],
      "test": null,
      "title": "Each task arrives as a merge commit of its own"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task merges and passes its tests while another task is queued"
       },
       {
        "kw": "THEN",
        "text": "the merged task's Reviewer is spawned"
       },
       {
        "kw": "AND",
        "text": "the queued task merges without waiting for that review"
       }
      ],
      "test": null,
      "title": "The merge chain does not wait for a review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task of the batch depends on other tasks of the batch"
       },
       {
        "kw": "THEN",
        "text": "its Implementor is spawned only once all of them have merged"
       }
      ],
      "test": null,
      "title": "A task waits for the tasks it depends on to merge"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task's merge conflicts and the phase agent can keep what both sides meant without dropping code or a test"
       },
       {
        "kw": "THEN",
        "text": "it resolves the conflict in the merge"
       },
       {
        "kw": "AND",
        "text": "the change's journal records the resolution as a decision, naming what each side kept"
       },
       {
        "kw": "AND",
        "text": "the merge the task's Reviewer reads holds the resolution"
       }
      ],
      "test": null,
      "title": "A conflict that keeps both sides is resolved by hand"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task's merge conflicts and the phase agent cannot resolve it that way"
       },
       {
        "kw": "THEN",
        "text": "the merge is abandoned and the integration branch is as it was before it"
       },
       {
        "kw": "AND",
        "text": "the task's worktree is kept"
       },
       {
        "kw": "AND",
        "text": "a re-spawned Implementor brings the task up to date with the integration branch and resolves the conflict, and the task rejoins the merge queue"
       },
       {
        "kw": "AND",
        "text": "the next queued task merges meanwhile"
       }
      ],
      "test": null,
      "title": "Any other conflict goes back to the task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "resolving a conflict needs a choice of spec or goal"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask is filed that names the choice"
       },
       {
        "kw": "AND",
        "text": "the ask blocks the task"
       }
      ],
      "test": null,
      "title": "A conflict that needs a choice is a decision ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task is blocked by an ask and tasks that landed after it are queued"
       },
       {
        "kw": "THEN",
        "text": "the blocked task is not merged"
       },
       {
        "kw": "AND",
        "text": "the tasks behind it merge"
       }
      ],
      "test": null,
      "title": "A task held by an ask stays out of the merge queue"
     }
    ],
    "text": "The execute phase SHALL merge each task into the change's integration branch as soon as its Implementor lands a commit, one task at a time and in the order they landed, without waiting for the rest of the batch or for an earlier task's review; each task SHALL arrive as a merge commit of its own. The phase agent SHALL resolve a merge conflict itself when it can keep what both sides meant without dropping code or a test, and SHALL send any other conflict back to the task.",
    "title": "Tasks merge one at a time, as they land"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cycle's execute starts and the cycle has no baseline"
       },
       {
        "kw": "THEN",
        "text": "the test command is run on the integration branch before the first merge"
       },
       {
        "kw": "AND",
        "text": "its output is kept in the change's directory in the home"
       }
      ],
      "test": null,
      "title": "The baseline is taken once per cycle, before any merge"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task merges"
       },
       {
        "kw": "THEN",
        "text": "the test command is run on the integration branch"
       },
       {
        "kw": "AND",
        "text": "its output is kept in the change's directory in the home, in a file named for the task"
       }
      ],
      "test": null,
      "title": "Every merge is followed by a test run whose output is kept"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the baseline has failing tests, the project's runner names each test, and the project gives no way to compare two runs test by test"
       },
       {
        "kw": "THEN",
        "text": "an =environment= ask is filed at that first red baseline"
       }
      ],
      "test": null,
      "title": "A red baseline the project cannot compare test by test is an ask"
     }
    ],
    "text": "The execute phase SHALL run the project's test command on the change's integration branch once in each cycle before any merge (the baseline) and again after each merge, and SHALL keep each run's output in the change's directory in the home.",
    "title": "The tests run after every merge, against the cycle's baseline"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "two tasks are queued and merging the first makes a test fail that passed in the baseline"
       },
       {
        "kw": "THEN",
        "text": "the second task is not merged"
       },
       {
        "kw": "AND",
        "text": "both tasks' worktrees still exist"
       }
      ],
      "test": null,
      "title": "A regression after a merge stops further merges"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a merge causes a regression"
       },
       {
        "kw": "THEN",
        "text": "the change's record shows the regression against the task whose merge caused it"
       }
      ],
      "test": null,
      "title": "A regression is recorded against its task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a regression stops the merges while an Implementor is still running and a Reviewer has been spawned"
       },
       {
        "kw": "THEN",
        "text": "the Implementor finishes and its task joins the merge queue"
       },
       {
        "kw": "AND",
        "text": "the Reviewer finishes its review"
       }
      ],
      "test": null,
      "title": "Only merging stops"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a merge causes a regression"
       },
       {
        "kw": "THEN",
        "text": "the merge stays on the integration branch until the regression is resolved"
       },
       {
        "kw": "AND",
        "text": "it is reverted only when a revert is the chosen resolution"
       }
      ],
      "test": null,
      "title": "The merge that broke a test stays on the branch"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a regression is found"
       },
       {
        "kw": "THEN",
        "text": "the phase agent reads the failing run and the baseline to find what caused it"
       },
       {
        "kw": "AND",
        "text": "the change's journal records the cause against the task"
       }
      ],
      "test": null,
      "title": "The phase agent finds the regression's cause"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a regression's cause is found and the intent's goals settle what the code should do"
       },
       {
        "kw": "THEN",
        "text": "it is fixed in the change: by an inline fix when the fix is simple, by a re-spawned Implementor otherwise, by a revert of the merge only when neither will do"
       },
       {
        "kw": "AND",
        "text": "no ask is filed for it"
       },
       {
        "kw": "AND",
        "text": "the change's journal names the goals that settle it"
       }
      ],
      "test": null,
      "title": "A regression is fixed in the change like a blocking defect"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the fix the goals settle contradicts what a spec scenario or a design row says"
       },
       {
        "kw": "THEN",
        "text": "the execute phase changes neither the spec nor the design"
       },
       {
        "kw": "AND",
        "text": "the change's journal records the statement for integrate as a record correction"
       }
      ],
      "test": null,
      "title": "A spec statement the goals overrule is left for integrate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the fix for a regression is in"
       },
       {
        "kw": "THEN",
        "text": "the tests are run again on the integration branch"
       },
       {
        "kw": "AND",
        "text": "the next queued task merges only once that run shows no regression"
       }
      ],
      "test": null,
      "title": "Merging resumes only after a run shows no regression"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the tests are run again once a regression's fix is in"
       },
       {
        "kw": "THEN",
        "text": "that run's output is kept in the change's directory in the home, in a file of its own named for the task"
       },
       {
        "kw": "AND",
        "text": "the file holding the run after the merge is left as it was"
       }
      ],
      "test": null,
      "title": "The run after a fix is kept beside the run after the merge"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task's merge caused a regression"
       },
       {
        "kw": "THEN",
        "text": "its Reviewer is spawned only once a run after the fix shows no regression"
       },
       {
        "kw": "AND",
        "text": "the code the Reviewer reads runs through the fix"
       }
      ],
      "test": null,
      "title": "The task whose merge broke a test is reviewed after the fix"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the intent's goals do not settle what the code should do: neither which of two clashing tasks is right, nor what a spec scenario the fix would change should say"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask is filed that names the choice and where the failing test output is"
       },
       {
        "kw": "AND",
        "text": "the ask blocks the merge chain"
       }
      ],
      "test": null,
      "title": "A regression reaches the user only as a decision"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the run after a merge gives no verdict: the runner hangs, or the run's output cannot be read"
       },
       {
        "kw": "THEN",
        "text": "an =environment= ask is filed"
       },
       {
        "kw": "AND",
        "text": "no fix is attempted on that run"
       }
      ],
      "test": null,
      "title": "A test run that cannot be read is an environment ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "every test passed in the baseline and the run after a merge fails"
       },
       {
        "kw": "THEN",
        "text": "a regression is found"
       }
      ],
      "test": null,
      "title": "Against a green baseline, any failing run is a regression"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the baseline already has failing tests and a merge leaves only those failing"
       },
       {
        "kw": "THEN",
        "text": "no regression is found"
       },
       {
        "kw": "AND",
        "text": "when a merge makes a test fail that passed in the baseline, a regression is found"
       }
      ],
      "test": null,
      "title": "Against a red baseline, a regression is decided test by test"
     }
    ],
    "text": "The execute phase SHALL treat a test that passed in the baseline and fails after a merge as a regression. On a regression it SHALL merge no further task until a run shows none, keep every task's worktree, keep the merge on the integration branch, and record the regression against the task; all work other than merging SHALL go on. The phase agent SHALL find the regression's cause and route it as a blocking defect, fixed in the change, and the task's review SHALL follow the fix. The user SHALL be asked only when the intent's goals do not settle what the code should do: which of two clashing tasks is right, or what a spec scenario the fix would change should say. When the goals settle it, a spec scenario or design row that says otherwise SHALL be left for integrate as a record correction.",
    "title": "A merge that breaks a passing test stops the merge chain"
   },
   {
    "goal": "none stated (=north-star.org= * Disposition states it; no goal does)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor's report lists a discovery on the change's main path"
       },
       {
        "kw": "THEN",
        "text": "it is fixed in this change, as an inline fix or as a follow-up for the next plan"
       }
      ],
      "test": null,
      "title": "A discovery on the main path is fixed in the change"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the fix for a discovery on the main path is in one file, touches no seam's code, and is checked by a run whose output is predictable"
       },
       {
        "kw": "THEN",
        "text": "the phase agent makes it as an inline fix"
       },
       {
        "kw": "AND",
        "text": "a test or probe that fails on the defect comes before the fix"
       },
       {
        "kw": "AND",
        "text": "the change's journal records the inline fix against the task"
       }
      ],
      "test": null,
      "title": "A simple fix is made inline, reproduced first"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the fix for a discovery on the main path is not simple"
       },
       {
        "kw": "THEN",
        "text": "a follow-up candidate is written for the next plan, marked critical, naming the task and discovery it came from, with the source's words verbatim"
       },
       {
        "kw": "AND",
        "text": "its first step is the reproduction"
       },
       {
        "kw": "AND",
        "text": "no task file is written in execute"
       }
      ],
      "test": null,
      "title": "Any other fix is a follow-up candidate that starts with the reproduction"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor's discovery is off the change's main path and is not an untrue sentence"
       },
       {
        "kw": "THEN",
        "text": "it is one entry on the change's hardening list"
       }
      ],
      "test": null,
      "title": "A discovery off the main path goes on the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a discovery is a sentence in prose that is untrue"
       },
       {
        "kw": "THEN",
        "text": "the phase agent corrects the text inline"
       }
      ],
      "test": null,
      "title": "An untrue sentence in prose is corrected inline"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor's Observations name a defect in code or text that exists"
       },
       {
        "kw": "THEN",
        "text": "it is routed as a discovery would be"
       },
       {
        "kw": "AND",
        "text": "the other Observations stay in the task file as the record"
       }
      ],
      "test": null,
      "title": "An Observation that names a defect is routed like a discovery"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a discovery is routed"
       },
       {
        "kw": "THEN",
        "text": "the change's journal has one line for it, against the task, naming where it went"
       },
       {
        "kw": "AND",
        "text": "when a scan found nothing, the journal has one line saying what was scanned"
       }
      ],
      "test": null,
      "title": "Every discovery leaves a journal line naming its route"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "execute closes"
       },
       {
        "kw": "THEN",
        "text": "every defect it routed has a fix, a task, an ask or a hardening-list entry"
       }
      ],
      "test": null,
      "title": "A journal line is never where a defect ends"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the reason a merge was abandoned is itself a structural issue"
       },
       {
        "kw": "THEN",
        "text": "the change's journal records the issue as a discovery, not only the failed merge"
       },
       {
        "kw": "AND",
        "text": "a defect it names is routed like a finding"
       }
      ],
      "test": null,
      "title": "An aborted merge that shows a structural issue is a discovery"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "new work turns up while an Implementor runs"
       },
       {
        "kw": "THEN",
        "text": "nothing is sent to the running Implementor"
       },
       {
        "kw": "AND",
        "text": "the work becomes a task of the next plan or an inline fix after review"
       }
      ],
      "test": null,
      "title": "New work never reaches a running Implementor"
     }
    ],
    "text": "The execute phase SHALL route every defect an Implementor reports and every defect it finds itself while merging: one on the change's main path SHALL be fixed in the change, an untrue sentence in prose SHALL be corrected, and any other SHALL go on the change's hardening list. No defect SHALL end as a journal line alone.",
    "title": "Every defect reported while merging is routed"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer is spawned for a merged task"
       },
       {
        "kw": "THEN",
        "text": "its input holds the diff from the merge base to the merge commit"
       },
       {
        "kw": "AND",
        "text": "the task's text, read from the home"
       },
       {
        "kw": "AND",
        "text": "the seam rows the task cites, verbatim, and each scenario it cites with that scenario's coverage"
       },
       {
        "kw": "AND",
        "text": "the project's reviewer standards and priors, when the project has them"
       }
      ],
      "test": null,
      "title": "The Reviewer's input holds the merge and what the task cites"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer is spawned"
       },
       {
        "kw": "THEN",
        "text": "its input holds neither the Implementor's report nor the task's Observations and Discoveries"
       },
       {
        "kw": "AND",
        "text": "nothing that names the Implementor"
       },
       {
        "kw": "AND",
        "text": "no other diff in flight"
       },
       {
        "kw": "AND",
        "text": "its checkout holds nothing but the merge commit"
       }
      ],
      "test": null,
      "title": "The Reviewer never sees the Implementor's account"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor appends Observations and Discoveries"
       },
       {
        "kw": "THEN",
        "text": "they are in the task file in the home, not on the task's branch"
       },
       {
        "kw": "AND",
        "text": "the diff the Reviewer reads contains none of them"
       }
      ],
      "test": null,
      "title": "The Implementor's notes never reach the reviewed diff"
     }
    ],
    "text": "The execute phase SHALL give each Reviewer only the merge's diff, the task's text without the Implementor's Observations and Discoveries, the seam rows and scenarios the task cites, and the project's review standards and priors; never the Implementor's report, notes or identity, nor any other diff in flight.",
    "title": "Reviews are author-blind"
   },
   {
    "goal": "none stated (=north-star.org= * Disposition states it; no goal does)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review reports a defect on the change's main path"
       },
       {
        "kw": "THEN",
        "text": "a test or probe that fails on the defect comes first, and the fix makes it pass"
       },
       {
        "kw": "AND",
        "text": "the fix is an inline fix, a re-spawned Implementor or a revert of the merge"
       },
       {
        "kw": "AND",
        "text": "the task is not done until the fix is in"
       }
      ],
      "test": null,
      "title": "A blocking finding is reproduced, then fixed, before the task is done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review's question about the main path is tried and its reproduction does not fail"
       },
       {
        "kw": "THEN",
        "text": "it is an entry on the change's hardening list, with what was tried"
       }
      ],
      "test": null,
      "title": "A question on the main path that does not reproduce goes on the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review finds an untrue sentence"
       },
       {
        "kw": "THEN",
        "text": "the sentence is corrected before the task is done"
       },
       {
        "kw": "AND",
        "text": "no failing test is required for it"
       }
      ],
      "test": null,
      "title": "An untrue sentence needs only the corrected text"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review's defect is off the main path"
       },
       {
        "kw": "THEN",
        "text": "it is one entry on the change's hardening list that names the review file and the finding"
       },
       {
        "kw": "AND",
        "text": "the task can be done"
       }
      ],
      "test": null,
      "title": "Any other defect is a hardening entry and the task can be done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review signals that the design or a spec may be what is wrong"
       },
       {
        "kw": "THEN",
        "text": "an ask file is written at once, naming the review file"
       },
       {
        "kw": "AND",
        "text": "the task is not held by it"
       },
       {
        "kw": "AND",
        "text": "no step answering it is written into a later task's body in its place"
       }
      ],
      "test": null,
      "title": "A spec signal becomes an ask at once and does not hold its task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a spec-signal finding carries a stopgap"
       },
       {
        "kw": "THEN",
        "text": "the stopgap is applied as an inline fix before the task is done, and the ask names it"
       },
       {
        "kw": "AND",
        "text": "a stopgap that would answer the ask is not applied, and the ask says so"
       }
      ],
      "test": null,
      "title": "A spec signal's stopgap is applied before done and named in the ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a review's thought names a wrong result on an input the code accepts today"
       },
       {
        "kw": "THEN",
        "text": "it is routed as an issue, with one journal line"
       },
       {
        "kw": "AND",
        "text": "the review's other thoughts stay in the review file"
       }
      ],
      "test": null,
      "title": "A thought that names a wrong result today is routed as an issue"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding's evidence is a probe of a real process, daemon, client, =git= or file system"
       },
       {
        "kw": "THEN",
        "text": "the regression test for its fix drives that same surface, failing before the fix and passing after it"
       },
       {
        "kw": "AND",
        "text": "a test against a fake of the surface is not taken as the pin"
       },
       {
        "kw": "AND",
        "text": "where the tests cannot drive the surface, the fix commit and the task's Observations say so"
       }
      ],
      "test": null,
      "title": "A fix for a defect shown on a real surface is pinned on that surface"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the phase agent commits an inline fix"
       },
       {
        "kw": "THEN",
        "text": "its subject is =Inline fix after <merging|review of> task <task>: <what>="
       },
       {
        "kw": "AND",
        "text": "the message names what it answers: the review file and finding number, the discovery, or the regression"
       },
       {
        "kw": "AND",
        "text": "the message names the test or probe that pins it, which failed before the fix and passes after it"
       }
      ],
      "test": null,
      "title": "An inline fix's commit names its task, its cause and its pin"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the fix for a review's finding becomes a follow-up"
       },
       {
        "kw": "THEN",
        "text": "it is a candidate that names the review file and the finding"
       },
       {
        "kw": "AND",
        "text": "it is placed in this change, since the defect blocks the change"
       }
      ],
      "test": null,
      "title": "A follow-up from a review is a candidate placed in the change"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer flags a choice the Implementor made for a reason that holds but was not written"
       },
       {
        "kw": "THEN",
        "text": "the reason is written down as a comment, a test or a seam row's statement"
       },
       {
        "kw": "AND",
        "text": "the change's journal records that the flag was pushed back"
       },
       {
        "kw": "AND",
        "text": "when the reason was wrong, the flag is handled as an ordinary finding"
       }
      ],
      "test": null,
      "title": "A flagged choice with a sound unwritten reason is written down"
     }
    ],
    "text": "The execute phase SHALL route each finding of a review: a defect on the change's main path or an untrue sentence SHALL be reproduced and fixed before the task is done; a sign that the design or spec may be wrong SHALL become an ask at once; any other defect SHALL go on the change's hardening list.",
    "title": "Review findings are routed, and a blocking one is fixed before the task is done"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a question arises while execute runs"
       },
       {
        "kw": "THEN",
        "text": "it is an ask file in the change's inbox"
       },
       {
        "kw": "AND",
        "text": "the file is committed in the home with a subject naming its id"
       }
      ],
      "test": null,
      "title": "An ask is a file at once, committed in the home"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a question arises while execute runs"
       },
       {
        "kw": "THEN",
        "text": "the phase agent does not put it to the user"
       },
       {
        "kw": "AND",
        "text": "the question reaches the user through its file and the dispatcher"
       }
      ],
      "test": null,
      "title": "The phase agent never asks the user itself"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an =environment= ask is filed"
       },
       {
        "kw": "THEN",
        "text": "the phase agent finishes the work that does not depend on it"
       },
       {
        "kw": "AND",
        "text": "hands back that it stopped, naming the ask"
       }
      ],
      "test": null,
      "title": "An environment ask always stops the phase"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an ask blocks a merge in this batch or a task not yet spawned"
       },
       {
        "kw": "THEN",
        "text": "the phase agent first spawns Reviewers for merged tasks and files the reports"
       },
       {
        "kw": "AND",
        "text": "when nothing else can run, hands back that it stopped, naming the ask, whether or not the user is present"
       }
      ],
      "test": null,
      "title": "An ask that blocks waiting work stops the phase after the rest is done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the phase agent stopped on an ask that blocks waiting work and is resumed with word that the user is absent"
       },
       {
        "kw": "THEN",
        "text": "each task the ask names is blocked on it"
       },
       {
        "kw": "AND",
        "text": "the merge chain goes on with the tasks behind them"
       },
       {
        "kw": "AND",
        "text": "when nothing else can run the phase passes its gate if it can and goes on to integrate"
       }
      ],
      "test": null,
      "title": "Resumed with the user absent, the phase goes on without the answer"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an ask does not block work waiting now"
       },
       {
        "kw": "THEN",
        "text": "each task it names is blocked on it"
       },
       {
        "kw": "AND",
        "text": "the merge chain goes on with the tasks behind them"
       },
       {
        "kw": "AND",
        "text": "when nothing else can run the phase passes its gate if it can and goes on to integrate"
       },
       {
        "kw": "AND",
        "text": "only when the gate cannot pass does it stop, with a one-line pointer to the file"
       }
      ],
      "test": null,
      "title": "An ask that blocks nothing waiting does not stop the phase"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor hands back blocked on a premise that is wrong"
       },
       {
        "kw": "THEN",
        "text": "no ask is filed"
       },
       {
        "kw": "AND",
        "text": "the task is blocked, with the wrong premise as the reason, and its worktree is kept"
       },
       {
        "kw": "AND",
        "text": "the change's journal records the claim, pointing at the report"
       },
       {
        "kw": "AND",
        "text": "execute does not rewrite the task"
       }
      ],
      "test": null,
      "title": "A wrong premise blocks its task without an ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor stops to ask"
       },
       {
        "kw": "THEN",
        "text": "its task is blocked on that ask"
       },
       {
        "kw": "AND",
        "text": "its worktree is kept"
       }
      ],
      "test": null,
      "title": "An Implementor that stops to ask leaves its task blocked"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the merge chain pauses and an ask has been answered"
       },
       {
        "kw": "THEN",
        "text": "the answer is handled before anything else"
       },
       {
        "kw": "AND",
        "text": "an answer that unblocks a task is committed in the home with the same subject as the merge it releases"
       }
      ],
      "test": null,
      "title": "An answer is applied before anything else"
     }
    ],
    "text": "The execute phase SHALL write every question that arises while it runs as an ask file at once, committed in the home. It SHALL stop for an ask only when the ask blocks work waiting now, and then only after finishing the work that does not wait on it; any other ask SHALL hold only the tasks it names. Whether the user is present is the dispatcher's to know, not the phase agent's.",
    "title": "A question raised during execute is a file at once"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "execute passes its gate, stops on an ask, fails or hands over"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives one line of at most 200 characters"
       },
       {
        "kw": "AND",
        "text": "a stop on an ask names the ask, what it blocks and the file's path relative to the home"
       }
      ],
      "test": null,
      "title": "The phase hands back one line naming its outcome"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor or a Reviewer the phase spawned is still running"
       },
       {
        "kw": "THEN",
        "text": "the phase agent does not hand back"
       }
      ],
      "test": null,
      "title": "No hand-back while a worker runs"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the phase agent is asked to hand over"
       },
       {
        "kw": "THEN",
        "text": "it starts no new Implementor, Reviewer or merge"
       },
       {
        "kw": "AND",
        "text": "it waits until every worker it spawned has reported and every landed task is merged and tested or held by an ask"
       },
       {
        "kw": "AND",
        "text": "it writes a handover with the tasks merged, the tasks and asks open, and the steps left"
       },
       {
        "kw": "AND",
        "text": "it commits the handover in the home and hands back that it stopped, naming the handover"
       }
      ],
      "test": null,
      "title": "Asked to hand over, the phase stops at a clean point"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a fresh execute agent is given a handover"
       },
       {
        "kw": "THEN",
        "text": "it goes on from the steps the handover lists as left"
       }
      ],
      "test": null,
      "title": "A fresh agent goes on from the steps the handover leaves"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a fresh execute agent is given a handover"
       },
       {
        "kw": "THEN",
        "text": "it redoes nothing the handover lists as done"
       }
      ],
      "test": null,
      "title": "A fresh agent redoes nothing the handover lists as done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a fresh execute agent continues from a handover"
       },
       {
        "kw": "THEN",
        "text": "it does not change the handover"
       },
       {
        "kw": "AND",
        "text": "when it is asked to hand over in its turn, it writes the handover anew, whole, and commits it in the home"
       }
      ],
      "test": null,
      "title": "A fresh agent leaves the handover as written"
     }
    ],
    "text": "The execute phase agent SHALL hand the dispatcher one line, once, when the phase ends or stops, and never while a worker it spawned is still running; asked to hand over, it SHALL stop at a clean point and leave a handover a fresh agent can continue from.",
    "title": "The phase hands the dispatcher one line, and hands over when asked"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor is spawned"
       },
       {
        "kw": "THEN",
        "text": "the brief it was given is saved in the cycle's directory in the home"
       }
      ],
      "test": null,
      "title": "Each Implementor's brief is kept"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "execute closes"
       },
       {
        "kw": "THEN",
        "text": "one commit in the home, subject =Orchestrator: <cycle-id> execute closed=, holds the cycle's state, the briefs, the reports, the reviews and the task files' appended sections"
       },
       {
        "kw": "AND",
        "text": "the test output files written after each merge are not committed"
       }
      ],
      "test": null,
      "title": "Execute closes with one commit in the home"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task's merge passes its tests"
       },
       {
        "kw": "THEN",
        "text": "the task's worktree is removed"
       }
      ],
      "test": null,
      "title": "A merged and tested task's worktree is removed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the phase agent needs a working file outside the home, such as a draft"
       },
       {
        "kw": "THEN",
        "text": "it writes it in the scratch directory its brief names, never under a fixed name shared with other agents"
       },
       {
        "kw": "AND",
        "text": "the directory is gone once the phase closes"
       }
      ],
      "test": null,
      "title": "The phase agent's working files stay in its own scratch directory"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "execute closes"
       },
       {
        "kw": "THEN",
        "text": "it has not changed the change's design, its specs or the intent's status line"
       },
       {
        "kw": "AND",
        "text": "it has written no task file"
       },
       {
        "kw": "AND",
        "text": "no Architect has run"
       }
      ],
      "test": null,
      "title": "Execute leaves the design, the specs and the intent alone"
     }
    ],
    "text": "The execute phase SHALL leave in the home what each Implementor was given and what each worker returned, committed when the phase closes, and SHALL leave the design, the specs, the intent and the task files to the phases that own them.",
    "title": "Execute leaves its work in the home and nothing outside its part"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor is spawned"
       },
       {
        "kw": "THEN",
        "text": "it runs at the task's own setting when the plan wrote one, and at the Implementor's role default otherwise"
       },
       {
        "kw": "AND",
        "text": "the cycle's record holds the agent and its setting"
       }
      ],
      "test": null,
      "title": "An Implementor runs at its task's setting"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a Reviewer is spawned for a task"
       },
       {
        "kw": "THEN",
        "text": "it runs at the Reviewer's role default, or at the task's Reviewer setting when the plan raised it"
       },
       {
        "kw": "AND",
        "text": "the cycle's record holds the agent and its setting"
       }
      ],
      "test": null,
      "title": "A Reviewer runs at its role default or the task's raised setting"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an Implementor is spawned again to fix a blocking finding"
       },
       {
        "kw": "THEN",
        "text": "it runs at the setting the task's first Implementor ran at, never a stronger one"
       },
       {
        "kw": "AND",
        "text": "the cycle's record holds it as a rework"
       }
      ],
      "test": null,
      "title": "A rework runs at the task's setting, never a stronger one"
     }
    ],
    "text": "The execute phase SHALL spawn each Implementor and Reviewer at the agent setting its role or its task names, and SHALL record the setting it used.",
    "title": "Each worker runs at its agent setting"
   }
  ],
  "status": "draft 2026-11-12, extracted from the reference skill (=main= =e356da7=); inferred statements reviewed 2026-11-12; seven gaps filled and the regression requirement revised 2026-11-12; the gap on the run after a fix filled 2026-11-13; no gap open; not yet accepted",
  "title": "execute"
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
