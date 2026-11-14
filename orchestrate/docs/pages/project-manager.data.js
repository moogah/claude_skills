window.SPEC_PAGE = {
 "capability": "project-manager",
 "cases": [
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
       "id": "CS-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A cascade's conformance is recorded as applied without asking",
      "asks § A confirmation is written applied and is never a question"
     ],
     "id": "CS-E8",
     "question": "Is the PM's cascade conformance recorded as applied without asking?"
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
    },
    {
     "checks": [
      {
       "id": "CS-S15",
       "kind": "decide"
      }
     ],
     "cites": [
      "record § No role edits the state file by hand"
     ],
     "id": "CS-E10",
     "question": "Is the state written only through the state tool?"
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
      "CS-E10": [
       "record § No role edits the state file by hand"
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
      "CS-E8": [
       "project-manager § A cascade's conformance is recorded as applied without asking",
       "asks § A confirmation is written applied and is never a question"
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
      "CS-E10": [
       "CS-S15"
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
      "CS-E8": [
       "CS-S10"
      ],
      "CS-E9": [
       "CS-S11",
       "CS-S13",
       "CS-S14"
      ]
     },
     "items_then": [
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
         "id": "CS-S10",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A cascade's conformance is recorded as applied without asking",
        "asks § A confirmation is written applied and is never a question"
       ],
       "id": "CS-E8",
       "question": "Is the PM's cascade conformance recorded as applied without asking?"
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
      },
      {
       "checks": [
        {
         "id": "CS-S15",
         "kind": "decide"
        }
       ],
       "cites": [
        "record § No role edits the state file by hand"
       ],
       "id": "CS-E10",
       "question": "Is the state written only through the state tool?"
      }
     ],
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
        "CS-S10": {
         "audit": {
          "agree": true,
          "reason": "No confirmation ask exists in cycles/cycle-2/asks/; all three new asks are inbox decisions.",
          "verdict": "agrees"
         },
         "desc": "a confirmation ask, applied, in the cycle's asks directory",
         "detail": "[]; new asks: ask-cycle-2-2 decision .orchestrator/changes/json-report/inbox/ask-cycle-2-2.org; ask-cycle-2-3 decision .orchestrator/changes/json-report/inbox/ask-cycle-2-3.org; ask-cycle-2-1 decision .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
         "kind": "decide",
         "pass": false
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
        "CS-S15": {
         "audit": {
          "agree": true,
          "reason": "No tool call writes state.json directly; the python edits target pm-digest.md and the inbox/scratch files only.",
          "verdict": "agrees"
         },
         "desc": "the state file written only through the state tool",
         "detail": "",
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
        "CS-E10": "pass",
        "CS-E2": "pass",
        "CS-E3": "pass",
        "CS-E4": "pass",
        "CS-E5": "fail",
        "CS-E6": "pass",
        "CS-E7": "fail",
        "CS-E8": "fail",
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
        "CS-E10": {
         "quotes": [
          "state.py record add findings ... state.py record set asks ... state.py close",
          "state.py record add findings ... state.py handshake ... state.py close",
          "state.py record add findings ... state.py note ... state.py handshake ... state.py close"
         ],
         "reasons": [
          "Every state change went through state.py; no main-thread call wrote state.json directly.",
          "Every state change went through state.py; the heredoc writes target meta-discoveries.json, task-refinements.json, candidate-tasks.json, hardening.org and the digest, never state.json.",
          "Every state change went through `state.py`; the hand-written files were the digest, hardening list and side JSON files, never `state.json`."
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
        "CS-E8": {
         "quotes": [
          "(A cascade signal an earlier draft of this digest carried is not supported by the counts and is removed.)",
          "(A cascade signal an earlier draft of this digest carried is not supported by the counts and is removed.)",
          "(A cascade signal an earlier draft of this digest carried is not supported by the counts and is removed.)"
         ],
         "reasons": [
          "No confirmation ask was written to cycles/cycle-2/asks/; the agent rewrote the PM's digest to remove the cascade signal and logged the agent records as absent instead.",
          "No confirmation ask recording the PM's cascade conformance run exists in cycles/cycle-2/asks/; the agent instead removed the cascade signal from the PM's digest.",
          "No confirmation ask was written in the cycle's `asks/` for the PM's cascade conformance run; the agent instead removed the cascade signal from the PM's digest."
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
       "id": "IR-S22",
       "kind": "decide"
      },
      {
       "id": "IR-S23",
       "kind": "signal"
      }
     ],
     "cites": [
      "asks § An unsettled ask is in the change's inbox",
      "asks § A decision ask has its six sections",
      "asks § The state holds an ask's id, kind, status, blocks and path"
     ],
     "id": "IR-E11",
     "question": "Is each ask written in its place, committed, with a decision's six sections?"
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
      "IR-E11": [
       "asks § An unsettled ask is in the change's inbox",
       "asks § A decision ask has its six sections",
       "asks § The state holds an ask's id, kind, status, blocks and path"
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
      "IR-E11": [
       "IR-S22",
       "IR-S23"
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
     "items_then": [
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
         "id": "IR-S22",
         "kind": "decide"
        },
        {
         "id": "IR-S23",
         "kind": "signal"
        }
       ],
       "cites": [
        "asks § An unsettled ask is in the change's inbox",
        "asks § A decision ask has its six sections",
        "asks § The state holds an ask's id, kind, status, blocks and path"
       ],
       "id": "IR-E11",
       "question": "Is each ask written in its place, committed, with a decision's six sections?"
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
        "IR-S22": {
         "audit": {
          "agree": true,
          "reason": "Both new asks are in inbox/, indexed in asks_for_user, and have the six decision sections.",
          "verdict": "agrees"
         },
         "desc": "each new ask in the inbox, indexed, a decision with its six sections",
         "detail": "2 new asks",
         "kind": "decide",
         "pass": true
        },
        "IR-S23": {
         "audit": {
          "agree": true,
          "reason": "Only two new asks exist, not four, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "four new asks",
         "detail": "2: ['ask-cycle-1-2', 'ask-cycle-1-3']",
         "kind": "signal",
         "pass": false
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
        "IR-E11": "fail",
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
        "IR-E11": {
         "quotes": [
          "ask-cycle-1-2 ... ask-cycle-1-3 ... :REF: mvp-json-report",
          "asks_for_user: ask-cycle-1-2 (open), ask-cycle-1-3 (open)",
          "asks_for_user ... ask-cycle-1-2 ... ask-cycle-1-3"
         ],
         "reasons": [
          "Only one of the four expected asks was filed (the review's spec signal); the asks for D3, seam/ledger-comments and the --json question are missing. The other new ask is the MVP ask.",
          "Only one of the four expected asks (the review's spec signal) was filed; the asks for D3, seam/ledger-comments and the --json question are missing.",
          "Of the four expected asks, only the review's spec signal (ask-cycle-1-2) was filed; ask-cycle-1-3 is the MVP ask. The asks for D3, seam/ledger-comments and the --json question are missing, although the asks that were filed are in the inbox, indexed, with six sections."
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
   "capability": "project-manager",
   "id": "pm-cascade-spawn",
   "items": [
    {
     "checks": [
      {
       "id": "PC-S1",
       "kind": "decide"
      },
      {
       "id": "PC-S2",
       "kind": "decide"
      },
      {
       "id": "PC-S3",
       "kind": "decide"
      },
      {
       "id": "PC-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM hands back one line of its signals and its digest's path"
     ],
     "id": "PC-E1",
     "question": "Does the PM hand back one line, naming the conformance run?"
    },
    {
     "checks": [
      {
       "id": "PC-S13",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § A cascade fires when one task has too many tasks discovered from it"
     ],
     "id": "PC-E2",
     "question": "Does the cascade fire, naming its task and how many were discovered from it?"
    },
    {
     "checks": [
      {
       "id": "PC-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A cascade starts the Architect's conformance, not a question"
     ],
     "id": "PC-E3",
     "question": "Does the cascade start the Architect's conformance over the cluster, not a question?"
    },
    {
     "checks": [
      {
       "id": "PC-S8",
       "kind": "decide"
      },
      {
       "id": "PC-S12",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § The cascade's conformance brief names the cluster and its mission"
     ],
     "id": "PC-E4",
     "question": "Does the conformance brief name the cluster and its mission?"
    },
    {
     "checks": [
      {
       "id": "PC-S7",
       "kind": "decide"
      },
      {
       "id": "PC-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The cascade's conformance runs at its role default and is recorded"
     ],
     "id": "PC-E5",
     "question": "Does the conformance run at its role default, and is it recorded?"
    },
    {
     "checks": [
      {
       "id": "PC-S11",
       "kind": "signal"
      },
      {
       "id": "PC-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A pause is recommended when the design looks wrong"
     ],
     "id": "PC-E6",
     "question": "Is a pause recommended, and not taken?"
    },
    {
     "checks": [
      {
       "id": "PC-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM writes no ask file"
     ],
     "id": "PC-E7",
     "question": "Does the PM write no ask file?"
    },
    {
     "checks": [
      {
       "id": "PC-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM changes no code and no task's text"
     ],
     "id": "PC-E8",
     "question": "Does the PM change no code and no task's text?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "a1354e6",
     "cites": {
      "PC-E1": [
       "project-manager § The PM hands back one line of its signals and its digest's path"
      ],
      "PC-E2": [
       "project-manager § A cascade fires when one task has too many tasks discovered from it"
      ],
      "PC-E3": [
       "project-manager § A cascade starts the Architect's conformance, not a question"
      ],
      "PC-E4": [
       "project-manager § The cascade's conformance brief names the cluster and its mission"
      ],
      "PC-E5": [
       "project-manager § The cascade's conformance runs at its role default and is recorded"
      ],
      "PC-E6": [
       "project-manager § A pause is recommended when the design looks wrong"
      ],
      "PC-E7": [
       "project-manager § The PM writes no ask file"
      ],
      "PC-E8": [
       "project-manager § The PM changes no code and no task's text"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "PC-E1": [
       "PC-S1",
       "PC-S2",
       "PC-S3",
       "PC-S6"
      ],
      "PC-E2": [
       "PC-S13"
      ],
      "PC-E3": [
       "PC-S8"
      ],
      "PC-E4": [
       "PC-S12",
       "PC-S8"
      ],
      "PC-E5": [
       "PC-S7",
       "PC-S9"
      ],
      "PC-E6": [
       "PC-S10",
       "PC-S11"
      ],
      "PC-E7": [
       "PC-S4"
      ],
      "PC-E8": [
       "PC-S5"
      ]
     },
     "items_then": [
      {
       "checks": [
        {
         "id": "PC-S1",
         "kind": "decide"
        },
        {
         "id": "PC-S2",
         "kind": "decide"
        },
        {
         "id": "PC-S3",
         "kind": "decide"
        },
        {
         "id": "PC-S6",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM hands back one line of its signals and its digest's path"
       ],
       "id": "PC-E1",
       "question": "Does the PM hand back one line, naming the conformance run?"
      },
      {
       "checks": [
        {
         "id": "PC-S13",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § A cascade fires when one task has too many tasks discovered from it"
       ],
       "id": "PC-E2",
       "question": "Does the cascade fire, naming its task and how many were discovered from it?"
      },
      {
       "checks": [
        {
         "id": "PC-S8",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A cascade starts the Architect's conformance, not a question"
       ],
       "id": "PC-E3",
       "question": "Does the cascade start the Architect's conformance over the cluster, not a question?"
      },
      {
       "checks": [
        {
         "id": "PC-S8",
         "kind": "decide"
        },
        {
         "id": "PC-S12",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § The cascade's conformance brief names the cluster and its mission"
       ],
       "id": "PC-E4",
       "question": "Does the conformance brief name the cluster and its mission?"
      },
      {
       "checks": [
        {
         "id": "PC-S7",
         "kind": "decide"
        },
        {
         "id": "PC-S9",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The cascade's conformance runs at its role default and is recorded"
       ],
       "id": "PC-E5",
       "question": "Does the conformance run at its role default, and is it recorded?"
      },
      {
       "checks": [
        {
         "id": "PC-S11",
         "kind": "signal"
        },
        {
         "id": "PC-S10",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A pause is recommended when the design looks wrong"
       ],
       "id": "PC-E6",
       "question": "Is a pause recommended, and not taken?"
      },
      {
       "checks": [
        {
         "id": "PC-S4",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM writes no ask file"
       ],
       "id": "PC-E7",
       "question": "Does the PM write no ask file?"
      },
      {
       "checks": [
        {
         "id": "PC-S5",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM changes no code and no task's text"
       ],
       "id": "PC-E8",
       "question": "Does the PM change no code and no task's text?"
      }
     ],
     "run": "20261114T102922",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PC-S1": {
         "audit": {
          "agree": true,
          "reason": "The digest exists only at the cycle's path, which is a precondition of the hand-back line's path.",
          "verdict": "agrees"
         },
         "desc": "the digest is pm-digest.md in the cycle's directory, and no other digest is written",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md: present; other digests written: none",
         "kind": "decide",
         "pass": true
        },
        "PC-S10": {
         "audit": {
          "agree": true,
          "reason": "The phase is still integrate and every task's queue fields match the fixture, so no pause was taken.",
          "verdict": "agrees"
         },
         "desc": "the state's tasks and phase as the fixture left them (no pause taken)",
         "detail": "tasks changed: {}; phase integrate (fixture integrate)",
         "kind": "decide",
         "pass": true
        },
        "PC-S11": {
         "audit": {
          "agree": true,
          "reason": "No line of the digest mentions a pause (the goal-drift recommendation is \"continue\"), so the signal's fail fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the digest mentions a pause other than to decline one",
         "detail": "",
         "kind": "signal",
         "pass": false
        },
        "PC-S12": {
         "audit": {
          "agree": true,
          "reason": "The brief mentions seam rows and asks for findings, as the signal looks for.",
          "verdict": "agrees"
         },
         "desc": "the spawn's input asks about the seam rows and for findings",
         "detail": "Cascade conformance run general-purpose opus high  Architect conformance run — cascade investigation (you are acting as the Architect, trigger: conformance; read <trial>/skills/orchestrate/roles/architect.md and templates/architect-finding.md under /private/tmp/s … [cut, 600 characters]",
         "kind": "signal",
         "pass": true
        },
        "PC-S13": {
         "audit": {
          "agree": true,
          "reason": "The ⚠ CASCADE line in Signals names report-json.",
          "verdict": "agrees"
         },
         "desc": "a ⚠ cascade line in Signals naming the source task",
         "detail": "⚠ **CASCADE**: `report-json` has 4 follow-ups (json-escape, json-zero, json-sign, json-nan), all found after it was done. They look like one gap: the seam row `seam/json-report` says each amount is \"as the table renders it\" but gives no rule for an amount's form (zero, negative, non-number, escaping … [cut, 341 characters]",
         "kind": "signal",
         "pass": true
        },
        "PC-S2": {
         "audit": {
          "agree": true,
          "reason": "The final text is one 147-character line in the required form, with the singulars correct and the path relative to the home.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters in the PM's form, the digest's path relative to the home",
         "detail": "147 characters: pm cycle-4: 6 signals (4 ⚠), 1 candidate question, conformance run a3c9e1f07b52d4a86; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md | the digest relative to the home: .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md",
         "kind": "decide",
         "pass": true
        },
        "PC-S3": {
         "audit": {
          "agree": true,
          "reason": "The Signals section has 6 marked lines (4 ⚠, 1 ✓, 1 ◦) and the asks table has 1 row not yet filed, matching the line's 6/4/1.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the digest's: its marked signal lines (⚠, ✓ and ◦), its ⚠ lines, its rows not yet filed",
         "detail": "line: 6 signals, 4 ⚠, 1 candidate questions | digest: the Signals section: 6 marked lines, 4 ⚠; the whole digest: 7 marked lines, 4 ⚠; 1 rows not yet filed",
         "kind": "decide",
         "pass": true
        },
        "PC-S4": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no ask file and no commits in the home.",
          "verdict": "agrees"
         },
         "desc": "no ask file added or changed, and no home commit naming an ask",
         "detail": "ask files added or changed: none | commits naming an ask: none (0 ask files at the fixture)",
         "kind": "decide",
         "pass": true
        },
        "PC-S5": {
         "audit": {
          "agree": true,
          "reason": "The code is unchanged and the changed files list contains no task file.",
          "verdict": "agrees"
         },
         "desc": "the code repository and its worktrees as the fixture left them, and no task file changed",
         "detail": "code: unchanged | task files changed: none",
         "kind": "decide",
         "pass": true
        },
        "PC-S6": {
         "audit": {
          "agree": true,
          "reason": "The line's id a3c9e1f07b52d4a86 is the agentId in the host's message.",
          "verdict": "agrees"
         },
         "desc": "the line names the conformance run by the id the host gave",
         "detail": "pm cycle-4: 6 signals (4 ⚠), 1 candidate question, conformance run a3c9e1f07b52d4a86; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md | the id given: a3c9e1f07b52d4a86",
         "kind": "decide",
         "pass": true
        },
        "PC-S7": {
         "audit": {
          "agree": true,
          "reason": "The single Agent call's input has model opus and effort high.",
          "verdict": "agrees"
         },
         "desc": "the conformance run spawned at opus, high",
         "detail": "1 spawn(s); model opus, effort high",
         "kind": "decide",
         "pass": true
        },
        "PC-S8": {
         "audit": {
          "agree": false,
          "reason": "It correctly finds report-json and the four discovered tasks named, but as the only decide check on PC-E3 and PC-E4 its pass leaves both items passing. It does not observe the candidate question raised on the cluster (PC-E3) or the brief's omission of the discovered classes (PC-E4), which the evidence shows.",
          "verdict": "suspect"
         },
         "desc": "the spawn's input names the source task and each task discovered from it",
         "detail": "source report-json, discovered ['json-escape', 'json-zero', 'json-sign', 'json-nan']; not named: []",
         "kind": "decide",
         "pass": true
        },
        "PC-S9": {
         "audit": {
          "agree": true,
          "reason": "state.json's agents list holds the architect-conformance entry with the host's id.",
          "verdict": "agrees"
         },
         "desc": "the change's record gains the run as an architect-conformance agent, with the id the host gave",
         "detail": "[{\"task\": null, \"cycle\": \"cycle-4\", \"source\": \"role-default\", \"reason\": \"role default\", \"agent_id\": \"a3c9e1f07b52d4a86\", \"role\": \"architect-conformance\", \"model\": \"opus\", \"effort\": \"high\", \"recorded_at\": \"2026-11-14T15:29:41Z\"}]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "PC-E1": "pass",
        "PC-E2": "pass",
        "PC-E3": "fail",
        "PC-E4": "fail",
        "PC-E5": "pass",
        "PC-E6": "fail",
        "PC-E7": "pass",
        "PC-E8": "pass"
       },
       "judge": {
        "PC-E1": {
         "quotes": [
          "pm cycle-4: 6 signals (4 ⚠), 1 candidate question, conformance run a3c9e1f07b52d4a86; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md",
          "pm cycle-4: 6 signals (4 ⚠), 1 candidate question, conformance run a3c9e1f07b52d4a86; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md",
          "pm cycle-4: 6 signals (4 ⚠), 1 candidate question, conformance run a3c9e1f07b52d4a86; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md"
         ],
         "reasons": [
          "The hand-back is one line of 147 characters in the required form. It names the run id the host gave and the digest's path relative to the home, and its counts match the Signals section: 6 marked lines, 4 ⚠, and 1 row not yet filed.",
          "The final text is one 147-character line in the required form, with the id the host gave and the digest's path relative to the home; the counts match the digest's 6 marked Signals lines, 4 of them ⚠, and its one not-yet-filed row.",
          "The hand-back is one 147-character line in the required form. It carries the host's agent id and the digest's path relative to the home, and its counts match the Signals section (6 marked lines, 4 ⚠) and the one 'not yet filed' ask row."
         ]
        },
        "PC-E2": {
         "quotes": [
          "⚠ **CASCADE**: `report-json` has 4 follow-ups (json-escape, json-zero, json-sign, json-nan), all found after it was done.",
          "⚠ **CASCADE**: `report-json` has 4 follow-ups (json-escape, json-zero, json-sign, json-nan)",
          "⚠ **CASCADE**: `report-json` has 4 follow-ups (json-escape, json-zero, json-sign, json-nan), all found after it was done."
         ],
         "reasons": [
          "A ⚠ cascade signal names the source task and all four tasks discovered from it.",
          "A ⚠ cascade signal names the source task report-json and the four tasks discovered from it.",
          "A ⚠ cascade signal names the source task and the four tasks discovered from it."
         ]
        },
        "PC-E3": {
         "quotes": [
          "| 1 | What should the JSON report print for a zero, a negative and a non-number amount: the table's text, or a JSON number? Should the design state that form as a rule? | json-escape, json-zero, json-sign, json-nan (their merge) | ... | not yet filed |",
          "| 1 | What should the JSON report print for a zero, a negative and a non-number amount: the table's text, or a JSON number? Should the design state that form as a rule? | json-escape, json-zero, json-sign, json-nan (their merge) | ... | not yet filed |",
          "| 1 | What should the JSON report print for a zero, a negative and a non-number amount: the table's text, or a JSON number? Should the design state that form as a rule? | json-escape, json-zero, json-sign, json-nan (their merge) | State one rule in `seam/json-report` ... | not yet filed |"
         ],
         "reasons": [
          "The PM did spawn a conformance run scoped to the cluster, but it also raised a candidate question on the cascade's own matter, blocking exactly the four discovered tasks, which the item excludes.",
          "The PM did spawn the conformance run scoped to the cluster, but it also raised a candidate question on the cascade's own subject, blocking the four discovered tasks, which the item excludes.",
          "The PM did spawn the conformance over the cluster, but it also raised a candidate question about the cascade's own gap, blocking the four discovered tasks, which the item excludes."
         ]
        },
        "PC-E4": {
         "quotes": [
          "Discovered classes: see each task file's discovered_class",
          "Discovered classes: see each task file's discovered_class",
          "Discovered classes: see each task file's discovered_class"
         ],
         "reasons": [
          "The brief names the cluster, the source task, the four discovered tasks, the implicated modules and the full mission, but it does not state their discovered classes. It defers to task files for json-escape and the others, which do not exist under tasks/open/; only dollar-table, ledger-comments and report-json are there.",
          "The brief names the cluster, the source task, the four discovered tasks, the implicated modules and the full mission, but it does not name the discovered classes (shape-fragmentation in the state); it points to task files that are not under tasks/open.",
          "The brief names the cluster, the source task, the four tasks, the modules and the full mission, but it gives no discovered classes. It only points to task files, and the state shows those files are absent from tasks/open (the classes are 'shape-fragmentation' only in the state)."
         ]
        },
        "PC-E5": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"high\" ... \"role\": \"architect-conformance\"",
          "\"model\": \"opus\", \"effort\": \"high\" ... \"role\": \"architect-conformance\", \"agent_id\": \"a3c9e1f07b52d4a86\"",
          "\"model\": \"opus\", \"effort\": \"high\" ... \"role\": \"architect-conformance\", \"model\": \"opus\", \"effort\": \"high\""
         ],
         "reasons": [
          "The Agent call passes opus/high, and the state's agents list records the run under the host's id as architect-conformance.",
          "The Agent call passed opus/high, and the state's agents list records the run as an architect-conformance agent with the host's id.",
          "The Agent call passed opus/high, and the state's agents list holds the run under the host's id as an architect-conformance agent."
         ]
        },
        "PC-E6": {
         "quotes": [
          "**Recommendation**: continue — the work is on the intent's JSON goal, and the cost is rework, not drift.",
          "**Recommendation**: continue — the work is on the intent's JSON goal, and the cost is rework, not drift.",
          "**Recommendation**: continue — the work is on the intent's JSON goal, and the cost is rework, not drift."
         ],
         "reasons": [
          "The digest recommends continuing rather than pausing the change, so the recommendation the item requires is absent, although no pause was taken.",
          "The PM took no pause, but the digest recommends continuing rather than pausing, so the item's required recommendation is missing.",
          "The digest recommends continuing, not pausing, so the pass condition (a pause recommended) is not met, even though no pause was taken."
         ]
        },
        "PC-E7": {
         "quotes": [
          "\"asks_for_user\": [] ... File | not yet filed",
          "Other files added ... findings/arch-cycle-4-1.md, findings/arch-cycle-4-2.md, state.json, state.lock",
          "ask files added or changed: none"
         ],
         "reasons": [
          "No ask file was added or changed; the one ask is only a candidate row in the digest.",
          "No ask file was added or changed in the inbox or the cycle's asks; the asks row is marked not yet filed.",
          "No ask file was written in the inbox or any cycle's asks directory, and the digest's ask is marked 'not yet filed'."
         ]
        },
        "PC-E8": {
         "quotes": [
          "The code repository's refs and its worktrees' status == unchanged since the fixture",
          "The code repository's refs and its worktrees' status == unchanged since the fixture",
          "The code repository's refs and its worktrees' status == unchanged since the fixture"
         ],
         "reasons": [
          "The code is unchanged, and no task file was touched; the only state change is the agent record plus the stand-in's findings.",
          "The code is unchanged, and no task file appears among the changed files.",
          "The code is unchanged, and the only home files changed are the digest, the stand-in's findings and the state, with no task file touched."
         ]
        }
       },
       "milestones": [],
       "n_calls": 8,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the PM's agent pass, from its brief, under the runner's host, to its hand-back",
   "title": "Case pm-cascade-spawn: the cascade fires; the PM starts the Architect's conformance and recommends a pause",
   "trials": 3,
   "validated": "a1354e6244fd7de17c7618f12ae9aa5472727503 20261114T102922 t1 (skill-evals)"
  },
  {
   "capability": "project-manager",
   "id": "pm-digest-form",
   "items": [
    {
     "checks": [
      {
       "id": "PD-S11",
       "kind": "decide"
      },
      {
       "id": "PD-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § Every number in the digest is one the record's counts give",
      "project-manager § The digest counts the whole-change review's findings"
     ],
     "id": "PD-E1",
     "question": "Is every number in the digest's tables, its main-path readout and its asks table one the computed counts give, the PM's own words only framing them?"
    },
    {
     "checks": [
      {
       "id": "PD-S20",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § A zero rejection rate beside inline fixes is not read as a clean batch"
     ],
     "id": "PD-E2",
     "question": "Does the digest not call the batch clean, its rejection rate zero beside inline fixes?"
    },
    {
     "checks": [
      {
       "id": "PD-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The digest is one file in the cycle's directory"
     ],
     "id": "PD-E3",
     "question": "Is the digest one file, pm-digest.md, in the cycle's directory?"
    },
    {
     "checks": [
      {
       "id": "PD-S2",
       "kind": "decide"
      },
      {
       "id": "PD-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM hands back one line of its signals and its digest's path"
     ],
     "id": "PD-E4",
     "question": "Does the PM hand back one line of its signals, its candidate questions and the digest's path?"
    },
    {
     "checks": [
      {
       "id": "PD-S6",
       "kind": "decide"
      },
      {
       "id": "PD-S21",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § The digest opens with its change, cycle, time and history window"
     ],
     "id": "PD-E5",
     "question": "Does the digest open with its change, its cycle, when it was produced and its history window?"
    },
    {
     "checks": [
      {
       "id": "PD-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The digest's sections come in the form's order",
      "project-manager § The digest carries trends to watch, meta-discoveries and a goal-drift check"
     ],
     "id": "PD-E6",
     "question": "Are the digest's sections the form's, in its order, with trends to watch, meta-discoveries and a goal-drift check?"
    },
    {
     "checks": [
      {
       "id": "PD-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The digest's Signals section is never empty",
      "project-manager § Each signal line is marked actionable, healthy or informational",
      "project-manager § A healthy tracked dimension is shown, not left out"
     ],
     "id": "PD-E7",
     "question": "Is the Signals section never empty, each line marked, a healthy dimension shown with ✓?"
    },
    {
     "checks": [
      {
       "id": "PD-S9",
       "kind": "decide"
      },
      {
       "id": "PD-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The throughput table covers the history window",
      "project-manager § The throughput table holds the form's counts only"
     ],
     "id": "PD-E8",
     "question": "Does the throughput table cover the history window with the form's counts only?"
    },
    {
     "checks": [
      {
       "id": "PD-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The main-path readout counts active, blocked and complete tasks on the main path"
     ],
     "id": "PD-E9",
     "question": "Does the main-path readout count the active, blocked and complete tasks on the main path?"
    },
    {
     "checks": [],
     "cites": [
      "project-manager § The digest shows spend by role beside quality by the Implementor's setting",
      "project-manager § Spend is read against quality in one line"
     ],
     "id": "PD-E10",
     "question": "Does the agents section show spend by role beside quality by the Implementor's setting, read in one line?"
    },
    {
     "checks": [
      {
       "id": "PD-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § Every open ask is listed with its file"
     ],
     "id": "PD-E11",
     "question": "Is every open ask listed with its file?"
    },
    {
     "checks": [
      {
       "id": "PD-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A task blocked on an ask re-surfaces that ask"
     ],
     "id": "PD-E12",
     "question": "Does the task blocked on an ask re-surface that ask, and raise no new question?"
    },
    {
     "checks": [
      {
       "id": "PD-S15",
       "kind": "decide"
      },
      {
       "id": "PD-S16",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § A task blocked on anything else is a candidate question",
      "project-manager § A candidate question is plain, says what it blocks and recommends",
      "project-manager § A candidate question is a row of the asks table, marked not yet filed"
     ],
     "id": "PD-E13",
     "question": "Is the task blocked on something else a candidate question: plain, saying what it blocks, recommending, a row not yet filed?"
    },
    {
     "checks": [
      {
       "id": "PD-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM writes no ask file"
     ],
     "id": "PD-E14",
     "question": "Does the PM write no ask file?"
    },
    {
     "checks": [
      {
       "id": "PD-S17",
       "kind": "decide"
      },
      {
       "id": "PD-S25",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The digest counts the asks applied without asking and the process notes",
      "project-manager § The digest counts the findings by kind and the hardening list",
      "project-manager § The digest counts the record corrections"
     ],
     "id": "PD-E15",
     "question": "Does the digest count what was settled without the user and what was recorded rather than asked: asks applied without asking, process notes, findings by kind, the hardening list, record corrections?"
    },
    {
     "checks": [
      {
       "id": "PD-S18",
       "kind": "decide"
      },
      {
       "id": "PD-S19",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § The goal-drift check shows the main path's completion",
      "project-manager § The goal-drift recommendation is one of the ask's options, or none"
     ],
     "id": "PD-E16",
     "question": "Does the goal-drift check show the main path's completion and recommend none?"
    },
    {
     "checks": [
      {
       "id": "PD-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM changes no code and no task's text"
     ],
     "id": "PD-E17",
     "question": "Does the PM change no code and no task's text?"
    },
    {
     "checks": [
      {
       "id": "PD-S22",
       "kind": "decide"
      },
      {
       "id": "PD-S23",
       "kind": "decide"
      },
      {
       "id": "PD-S24",
       "kind": "decide"
      }
     ],
     "cites": [
      "record § A task an earlier cycle held is carried, not created",
      "record § The four ratios divide the cycle's counts",
      "record § Send-backs and inline fixes are counted from the record"
     ],
     "id": "PD-E18",
     "question": "Does this cycle's column count carried, created, send-backs and inline fixes as the record defines them, and its drainage divide them?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "PD-E1": [
       "project-manager § Every number in the digest is one the record's counts give",
       "project-manager § The digest counts the whole-change review's findings"
      ],
      "PD-E10": [
       "project-manager § The digest shows spend by role beside quality by the Implementor's setting",
       "project-manager § Spend is read against quality in one line"
      ],
      "PD-E11": [
       "project-manager § Every open ask is listed with its file"
      ],
      "PD-E12": [
       "project-manager § A task blocked on an ask re-surfaces that ask"
      ],
      "PD-E13": [
       "project-manager § A task blocked on anything else is a candidate question",
       "project-manager § A candidate question is plain, says what it blocks and recommends",
       "project-manager § A candidate question is a row of the asks table, marked not yet filed"
      ],
      "PD-E14": [
       "project-manager § The PM writes no ask file"
      ],
      "PD-E15": [
       "project-manager § The digest counts the asks applied without asking and the process notes",
       "project-manager § The digest counts the findings by kind and the hardening list",
       "project-manager § The digest counts the record corrections"
      ],
      "PD-E16": [
       "project-manager § The goal-drift check shows the main path's completion",
       "project-manager § The goal-drift recommendation is one of the ask's options, or none"
      ],
      "PD-E17": [
       "project-manager § The PM changes no code and no task's text"
      ],
      "PD-E18": [
       "record § A task an earlier cycle held is carried, not created",
       "record § The four ratios divide the cycle's counts",
       "record § Send-backs and inline fixes are counted from the record"
      ],
      "PD-E2": [
       "project-manager § A zero rejection rate beside inline fixes is not read as a clean batch"
      ],
      "PD-E3": [
       "project-manager § The digest is one file in the cycle's directory"
      ],
      "PD-E4": [
       "project-manager § The PM hands back one line of its signals and its digest's path"
      ],
      "PD-E5": [
       "project-manager § The digest opens with its change, cycle, time and history window"
      ],
      "PD-E6": [
       "project-manager § The digest's sections come in the form's order",
       "project-manager § The digest carries trends to watch, meta-discoveries and a goal-drift check"
      ],
      "PD-E7": [
       "project-manager § The digest's Signals section is never empty",
       "project-manager § Each signal line is marked actionable, healthy or informational",
       "project-manager § A healthy tracked dimension is shown, not left out"
      ],
      "PD-E8": [
       "project-manager § The throughput table covers the history window",
       "project-manager § The throughput table holds the form's counts only"
      ],
      "PD-E9": [
       "project-manager § The main-path readout counts active, blocked and complete tasks on the main path"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "PD-E1": [
       "PD-S11",
       "PD-S12"
      ],
      "PD-E10": [],
      "PD-E11": [
       "PD-S13"
      ],
      "PD-E12": [
       "PD-S14"
      ],
      "PD-E13": [
       "PD-S15",
       "PD-S16"
      ],
      "PD-E14": [
       "PD-S4"
      ],
      "PD-E15": [
       "PD-S17",
       "PD-S25"
      ],
      "PD-E16": [
       "PD-S18",
       "PD-S19"
      ],
      "PD-E17": [
       "PD-S5"
      ],
      "PD-E18": [
       "PD-S22",
       "PD-S23",
       "PD-S24"
      ],
      "PD-E2": [
       "PD-S20"
      ],
      "PD-E3": [
       "PD-S1"
      ],
      "PD-E4": [
       "PD-S2",
       "PD-S3"
      ],
      "PD-E5": [
       "PD-S21",
       "PD-S6"
      ],
      "PD-E6": [
       "PD-S7"
      ],
      "PD-E7": [
       "PD-S8"
      ],
      "PD-E8": [
       "PD-S10",
       "PD-S9"
      ],
      "PD-E9": [
       "PD-S12"
      ]
     },
     "items_then": [
      {
       "checks": [
        {
         "id": "PD-S11",
         "kind": "decide"
        },
        {
         "id": "PD-S12",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § Every number in the digest is one the record's counts give",
        "project-manager § The digest counts the whole-change review's findings"
       ],
       "id": "PD-E1",
       "question": "Is every number in the digest's tables, its main-path readout and its asks table one the computed counts give, the PM's own words only framing them?"
      },
      {
       "checks": [
        {
         "id": "PD-S20",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § A zero rejection rate beside inline fixes is not read as a clean batch"
       ],
       "id": "PD-E2",
       "question": "Does the digest not call the batch clean, its rejection rate zero beside inline fixes?"
      },
      {
       "checks": [
        {
         "id": "PD-S1",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The digest is one file in the cycle's directory"
       ],
       "id": "PD-E3",
       "question": "Is the digest one file, pm-digest.md, in the cycle's directory?"
      },
      {
       "checks": [
        {
         "id": "PD-S2",
         "kind": "decide"
        },
        {
         "id": "PD-S3",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM hands back one line of its signals and its digest's path"
       ],
       "id": "PD-E4",
       "question": "Does the PM hand back one line of its signals, its candidate questions and the digest's path?"
      },
      {
       "checks": [
        {
         "id": "PD-S6",
         "kind": "decide"
        },
        {
         "id": "PD-S21",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § The digest opens with its change, cycle, time and history window"
       ],
       "id": "PD-E5",
       "question": "Does the digest open with its change, its cycle, when it was produced and its history window?"
      },
      {
       "checks": [
        {
         "id": "PD-S7",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The digest's sections come in the form's order",
        "project-manager § The digest carries trends to watch, meta-discoveries and a goal-drift check"
       ],
       "id": "PD-E6",
       "question": "Are the digest's sections the form's, in its order, with trends to watch, meta-discoveries and a goal-drift check?"
      },
      {
       "checks": [
        {
         "id": "PD-S8",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The digest's Signals section is never empty",
        "project-manager § Each signal line is marked actionable, healthy or informational",
        "project-manager § A healthy tracked dimension is shown, not left out"
       ],
       "id": "PD-E7",
       "question": "Is the Signals section never empty, each line marked, a healthy dimension shown with ✓?"
      },
      {
       "checks": [
        {
         "id": "PD-S9",
         "kind": "decide"
        },
        {
         "id": "PD-S10",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The throughput table covers the history window",
        "project-manager § The throughput table holds the form's counts only"
       ],
       "id": "PD-E8",
       "question": "Does the throughput table cover the history window with the form's counts only?"
      },
      {
       "checks": [
        {
         "id": "PD-S12",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The main-path readout counts active, blocked and complete tasks on the main path"
       ],
       "id": "PD-E9",
       "question": "Does the main-path readout count the active, blocked and complete tasks on the main path?"
      },
      {
       "checks": [],
       "cites": [
        "project-manager § The digest shows spend by role beside quality by the Implementor's setting",
        "project-manager § Spend is read against quality in one line"
       ],
       "id": "PD-E10",
       "question": "Does the agents section show spend by role beside quality by the Implementor's setting, read in one line?"
      },
      {
       "checks": [
        {
         "id": "PD-S13",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § Every open ask is listed with its file"
       ],
       "id": "PD-E11",
       "question": "Is every open ask listed with its file?"
      },
      {
       "checks": [
        {
         "id": "PD-S14",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A task blocked on an ask re-surfaces that ask"
       ],
       "id": "PD-E12",
       "question": "Does the task blocked on an ask re-surface that ask, and raise no new question?"
      },
      {
       "checks": [
        {
         "id": "PD-S15",
         "kind": "decide"
        },
        {
         "id": "PD-S16",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § A task blocked on anything else is a candidate question",
        "project-manager § A candidate question is plain, says what it blocks and recommends",
        "project-manager § A candidate question is a row of the asks table, marked not yet filed"
       ],
       "id": "PD-E13",
       "question": "Is the task blocked on something else a candidate question: plain, saying what it blocks, recommending, a row not yet filed?"
      },
      {
       "checks": [
        {
         "id": "PD-S4",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM writes no ask file"
       ],
       "id": "PD-E14",
       "question": "Does the PM write no ask file?"
      },
      {
       "checks": [
        {
         "id": "PD-S17",
         "kind": "decide"
        },
        {
         "id": "PD-S25",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The digest counts the asks applied without asking and the process notes",
        "project-manager § The digest counts the findings by kind and the hardening list",
        "project-manager § The digest counts the record corrections"
       ],
       "id": "PD-E15",
       "question": "Does the digest count what was settled without the user and what was recorded rather than asked: asks applied without asking, process notes, findings by kind, the hardening list, record corrections?"
      },
      {
       "checks": [
        {
         "id": "PD-S18",
         "kind": "decide"
        },
        {
         "id": "PD-S19",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § The goal-drift check shows the main path's completion",
        "project-manager § The goal-drift recommendation is one of the ask's options, or none"
       ],
       "id": "PD-E16",
       "question": "Does the goal-drift check show the main path's completion and recommend none?"
      },
      {
       "checks": [
        {
         "id": "PD-S5",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM changes no code and no task's text"
       ],
       "id": "PD-E17",
       "question": "Does the PM change no code and no task's text?"
      },
      {
       "checks": [
        {
         "id": "PD-S22",
         "kind": "decide"
        },
        {
         "id": "PD-S23",
         "kind": "decide"
        },
        {
         "id": "PD-S24",
         "kind": "decide"
        }
       ],
       "cites": [
        "record § A task an earlier cycle held is carried, not created",
        "record § The four ratios divide the cycle's counts",
        "record § Send-backs and inline fixes are counted from the record"
       ],
       "id": "PD-E18",
       "question": "Does this cycle's column count carried, created, send-backs and inline fixes as the record defines them, and its drainage divide them?"
      }
     ],
     "run": "20261114T102049",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PD-S1": {
         "audit": {
          "agree": true,
          "reason": "It finds the single digest at the cycle path, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the digest is pm-digest.md in the cycle's directory, and no other digest is written",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-5/pm-digest.md: present; other digests written: none",
         "kind": "decide",
         "pass": true
        },
        "PD-S10": {
         "audit": {
          "agree": true,
          "reason": "All five cycle ids appear in the table header.",
          "verdict": "agrees"
         },
         "desc": "the throughput table has this cycle and each cycle of the history window",
         "detail": "cycles expected: ['cycle-5', 'cycle-1', 'cycle-2', 'cycle-3', 'cycle-4']; missing: []",
         "kind": "decide",
         "pass": true
        },
        "PD-S11": {
         "audit": {
          "agree": false,
          "reason": "The table and readout numbers all trace to pm-signals.json, though the check does not cover the asks table.",
          "verdict": "agrees"
         },
         "desc": "every number in the throughput table, the main-path readout and the agents' tables is one the computed counts give",
         "detail": "numbers no count gives: {'Throughput': [], 'Critical path': [], 'Agents (its tables)': []}",
         "kind": "decide",
         "pass": true
        },
        "PD-S12": {
         "audit": {
          "agree": true,
          "reason": "The 0/0 and 2/2 counts match, with no on-path blocked tasks.",
          "verdict": "agrees"
         },
         "desc": "the main-path readout shows its active, all active, its complete and its total, and names its blocked tasks",
         "detail": "counts ['0', '0', '2', '2'] against numbers ['0', '0', '0', '2', '2']; blocked []",
         "kind": "decide",
         "pass": true
        },
        "PD-S13": {
         "audit": {
          "agree": true,
          "reason": "The open decision ask's home-relative path is in the asks section.",
          "verdict": "agrees"
         },
         "desc": "each open ask (other than a process ask) has its file's path, relative to the home, in the asks table",
         "detail": "open asks: ['ask-cycle-5-1']; not listed with their path: []",
         "kind": "decide",
         "pass": true
        },
        "PD-S14": {
         "audit": {
          "agree": true,
          "reason": "dollar-table is named with ask-cycle-5-1, and no not-yet-filed row names it.",
          "verdict": "agrees"
         },
         "desc": "a task blocked on an ask is named with that ask, and no row not yet filed is raised for it",
         "detail": "['dollar-table'] named with their asks",
         "kind": "decide",
         "pass": true
        },
        "PD-S15": {
         "audit": {
          "agree": true,
          "reason": "A not-yet-filed row exists for the otherwise-blocked task.",
          "verdict": "agrees"
         },
         "desc": "a row of the asks table marked not yet filed for the task blocked on something else",
         "detail": "blocked otherwise: ['ledger-comments']; rows not yet filed: ['| 2 | The tests for the \";\" comment lines need a CI runner image that is not there yet. Can it be provided, and by when? | ledger-comments | Keep the task blocked until the image is there; do not work round it | not yet filed |']",
         "kind": "decide",
         "pass": true
        },
        "PD-S16": {
         "audit": {
          "agree": true,
          "reason": "The row names ledger-comments.",
          "verdict": "agrees"
         },
         "desc": "a row not yet filed names the task blocked on something else",
         "detail": "| 2 | The tests for the \";\" comment lines need a CI runner image that is not there yet. Can it be provided, and by when? | ledger-comments | Keep the task blocked until the image is there; do not work round it | not yet filed |",
         "kind": "signal",
         "pass": true
        },
        "PD-S17": {
         "audit": {
          "agree": false,
          "reason": "The digest does name hardening.org, though the check sees only that part of PD-E15 and misses the uncounted record correction.",
          "verdict": "agrees"
         },
         "desc": "the digest names the hardening list's file",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "PD-S18": {
         "audit": {
          "agree": true,
          "reason": "The section recommends \"continue\" and does not hold \"none\".",
          "verdict": "agrees"
         },
         "desc": "the goal-drift check's recommendation is none",
         "detail": "\n- Critical-path completion ratio: 2/2 = 1.00. There is no drift: all 4 tasks serve a stated goal.\n- **Recommendation**: continue — the work stays on the three goals; only G2 and G3 remain, and both wait on answers.\n",
         "kind": "decide",
         "pass": false
        },
        "PD-S19": {
         "audit": {
          "agree": true,
          "reason": "The section names \"continue\", as observed.",
          "verdict": "agrees"
         },
         "desc": "the goal-drift check names none of revise, split, abandon and continue",
         "detail": "named: ['continue']",
         "kind": "signal",
         "pass": false
        },
        "PD-S2": {
         "audit": {
          "agree": true,
          "reason": "The final text carries a preamble line, so it is not the one-line form the item expects.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters in the PM's form, the digest's path relative to the home",
         "detail": "164 characters: I've written the digest and I'm handing back now.\n\npm cycle-5: 9 signals (2 ⚠), 1 candidate questions; .orchestrator/changes/json-report/cycles/cycle-5/pm-digest.md | the digest relative to the home: .orchestrator/changes/json-report/cycles/cycle-5/pm-digest.md",
         "kind": "decide",
         "pass": false
        },
        "PD-S20": {
         "audit": {
          "agree": true,
          "reason": "The flagged line says \"This is not a clean batch\", which is what the item wants, so the check's word match penalises a negation.",
          "verdict": "suspect"
         },
         "desc": "no line calls the batch clean beside the rejections",
         "detail": "◦ **REJECTIONS**: rejection rate 0.50 (1 of 2) beside 2 inline fixes. This is not a clean batch; report-json went back once. One cycle is not a pattern.",
         "kind": "signal",
         "pass": false
        },
        "PD-S21": {
         "audit": {
          "agree": true,
          "reason": "The header mentions the window and the produced time.",
          "verdict": "agrees"
         },
         "desc": "the header names the history window and when the digest was produced",
         "detail": "# PM digest — change: json-report — cycle-5\n\n**Produced**: 2026-11-14T15:21:23Z\n**Phase**: integrate\n**History window**: K=5\n",
         "kind": "signal",
         "pass": true
        },
        "PD-S22": {
         "audit": {
          "agree": true,
          "reason": "Carried 1 and created 3 match the archived states.",
          "verdict": "agrees"
         },
         "desc": "this cycle's column shows carried and created as the record defines them (a task an earlier cycle held carried, not created)",
         "detail": "column {'carried': 1.0, 'created': 3.0, 'rejected': 1.0, 'inline': 2.0}; from the record (archived states): carried 1, created 3",
         "kind": "decide",
         "pass": true
        },
        "PD-S23": {
         "audit": {
          "agree": true,
          "reason": "It reads the rejections and inline-fix entries from the fixture's state, and the column matches.",
          "verdict": "agrees"
         },
         "desc": "this cycle's column shows the send-backs as rejected and the inline-fix journal entries as inline fixes",
         "detail": "column rejected 1.0, inline 2.0; record: 1 send-backs, 2 inline-fix entries",
         "kind": "decide",
         "pass": true
        },
        "PD-S24": {
         "audit": {
          "agree": true,
          "reason": "2/3 rounds to 0.67, as shown.",
          "verdict": "agrees"
         },
         "desc": "this cycle's drainage is its completed over its created",
         "detail": "completed 2.0, created 3.0, drainage 0.67",
         "kind": "decide",
         "pass": true
        },
        "PD-S25": {
         "audit": {
          "agree": true,
          "reason": "ask-cycle-5-2 appears only in the counts and meta-discoveries, not in a table row.",
          "verdict": "agrees"
         },
         "desc": "no row of the asks table names a process ask (by its id or its file)",
         "detail": "process asks: ['ask-cycle-5-2']; rows naming one: []",
         "kind": "decide",
         "pass": true
        },
        "PD-S3": {
         "audit": {
          "agree": true,
          "reason": "Its rule makes it not applicable when the line's form fails, and the form does fail; the embedded counts would have matched (9, 2, 1).",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the digest's: its marked signal lines (⚠, ✓ and ◦), its ⚠ lines, its rows not yet filed",
         "detail": "no line in the form, or no digest",
         "kind": "decide",
         "pass": null
        },
        "PD-S4": {
         "audit": {
          "agree": true,
          "reason": "No ask files were added or changed, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "no ask file added or changed, and no home commit naming an ask",
         "detail": "ask files added or changed: none | commits naming an ask: none (3 ask files at the fixture)",
         "kind": "decide",
         "pass": true
        },
        "PD-S5": {
         "audit": {
          "agree": true,
          "reason": "The code and task files are unchanged, as reported.",
          "verdict": "agrees"
         },
         "desc": "the code repository and its worktrees as the fixture left them, and no task file changed",
         "detail": "code: unchanged | task files changed: none",
         "kind": "decide",
         "pass": true
        },
        "PD-S6": {
         "audit": {
          "agree": true,
          "reason": "The first heading holds json-report and cycle-5.",
          "verdict": "agrees"
         },
         "desc": "the digest's title names the change and the cycle",
         "detail": "PM digest — change: json-report — cycle-5",
         "kind": "decide",
         "pass": true
        },
        "PD-S7": {
         "audit": {
          "agree": true,
          "reason": "All eight headings are present in order.",
          "verdict": "agrees"
         },
         "desc": "the form's eight sections, in its order",
         "detail": "Throughput: 1; Critical path: 2; Agents: 3; Signals: 4; Asks for the user: 5; Trends to watch: 6; Meta-discoveries: 7; Goal-drift check: 8",
         "kind": "decide",
         "pass": true
        },
        "PD-S8": {
         "audit": {
          "agree": true,
          "reason": "Marked lines are present in Signals, as observed.",
          "verdict": "agrees"
         },
         "desc": "the Signals section holds at least one line marked ⚠, ✓ or ◦",
         "detail": "9 marked lines: ✓ **THROUGHPUT**: drainage 0.67 this cycle, but the last three cycles read 0.50, 1.00, 0.67, so the run of ≥3 below 1.0 has not formed. | ◦ **REJECTIONS**: rejection rate 0.50 (1 of 2) beside 2 inline fixes. This is not a clean batch; report-json went back once. One cycle is not a pa … [cut, 1016 characters]",
         "kind": "decide",
         "pass": true
        },
        "PD-S9": {
         "audit": {
          "agree": true,
          "reason": "Exactly the eight expected rows are found.",
          "verdict": "agrees"
         },
         "desc": "the throughput table's rows are the form's seven counts and the drainage, and no other",
         "detail": "rows: ['created', 'carried', 'completed', 'reviewed', 'rejected', 'inline fixes', 'externalised', 'drainage'] | per count: {'created': ['created'], 'carried': ['carried'], 'completed': ['completed'], 'reviewed': ['reviewed'], 'rejected': ['rejected'], 'inline fixes': ['inline fixes'], 'externalised' … [cut, 362 characters]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "PD-E1": "fail",
        "PD-E10": "pass",
        "PD-E11": "pass",
        "PD-E12": "pass",
        "PD-E13": "pass",
        "PD-E14": "pass",
        "PD-E15": "fail",
        "PD-E16": "fail",
        "PD-E17": "pass",
        "PD-E18": "pass",
        "PD-E2": "pass",
        "PD-E3": "pass",
        "PD-E4": "fail",
        "PD-E5": "pass",
        "PD-E6": "pass",
        "PD-E7": "pass",
        "PD-E8": "pass",
        "PD-E9": "pass"
       },
       "judge": {
        "PD-E1": {
         "quotes": [
          "unrecorded         4        1            1               0",
          "A script reading the JSON would then get \"$12.05\" text, not a number.",
          "Do it: the goals say the JSON carries the table's amounts. A script reading the JSON would then get \"$12.05\" text, not a number."
         ],
         "reasons": [
          "The throughput, critical-path and agents-table numbers all match pm-signals.json; the asks table's only other figure is the illustrative amount \"$12.05\" in a recommendation, which is example text rather than a count (its source, possibly the ask file, is not shown).",
          "The throughput, critical-path and agents tables all match pm-signals.json, but the asks table's recommendation column carries the amount 12.05, which no computed count gives and which is not among the places the PM's own words belong.",
          "The numbers in the throughput, critical-path and agents tables match pm-signals.json, but the asks table's recommendation column carries an invented amount, 12.05, that no computed count gives."
         ]
        },
        "PD-E10": {
         "quotes": [
          "no spend or settings are recorded, so spend cannot be read against E3. The quality counts stand alone",
          "Spend by role: no agents are recorded for this cycle ... unrecorded         4        1            1               0",
          "◦ **AGENT SETTINGS**: no spend or settings are recorded, so spend cannot be read against E3. The quality counts stand alone"
         ],
         "reasons": [
          "Spend by role is stated as empty because no agents are recorded; quality by setting is tabled; and one agent-settings line reads the two together, though its \"E3\" reference is unexplained.",
          "The spend-by-role part says that no agents are recorded, the quality table gives tasks, rejections, rework and defects after done by setting, and one agent-settings line reads the two together.",
          "Spend by role is stated as absent (none is recorded), the quality table by Implementor setting is given, and one line reads the two together."
         ]
        },
        "PD-E11": {
         "quotes": [
          "| 1 | Should the table's dollar sign also appear in the JSON report? | dollar-table | Do it: ... | `.orchestrator/changes/json-report/inbox/ask-cycle-5-1.org` |",
          "| 1 | Should the table's dollar sign also appear in the JSON report? | dollar-table | Do it: ... | `.orchestrator/changes/json-report/inbox/ask-cycle-5-1.org` |",
          "| 1 | Should the table's dollar sign also appear in the JSON report? | dollar-table | Do it: ... | `.orchestrator/changes/json-report/inbox/ask-cycle-5-1.org` |"
         ],
         "reasons": [
          "The open decision ask has a row with its question, what it blocks, a recommendation and its home-relative path.",
          "The open decision ask has a row with its question, what it blocks, a recommendation and its path relative to the home.",
          "The open decision ask has a row with its question, what it blocks, a recommendation and its home-relative path."
         ]
        },
        "PD-E12": {
         "quotes": [
          "dollar-table waits on ask-cycle-5-1",
          "dollar-table waits on ask-cycle-5-1",
          "dollar-table waits on ask-cycle-5-1"
         ],
         "reasons": [
          "The digest re-surfaces the ask id for dollar-table, and no not-yet-filed row names that task.",
          "The digest names the ask that blocks dollar-table, and no not-yet-filed row is raised for that task.",
          "The task blocked on the ask re-surfaces that ask's id, and no not-yet-filed row is raised for it."
         ]
        },
        "PD-E13": {
         "quotes": [
          "| 2 | The tests for the \";\" comment lines need a CI runner image that is not there yet. Can it be provided, and by when? | ledger-comments | Keep the task blocked until the image is there; do not work round it | not yet filed |",
          "| 2 | The tests for the \";\" comment lines need a CI runner image that is not there yet. Can it be provided, and by when? | ledger-comments | Keep the task blocked until the image is there; do not work round it | not yet filed |",
          "| 2 | The tests for the \";\" comment lines need a CI runner image that is not there yet. Can it be provided, and by when? | ledger-comments | Keep the task blocked until the image is there; do not work round it | not yet filed |"
         ],
         "reasons": [
          "The candidate question is plain, says it blocks ledger-comments, makes a recommendation, and is marked not yet filed.",
          "ledger-comments gets a plain candidate question that says what it blocks, recommends, and is marked not yet filed.",
          "The question is plain, names what it blocks, recommends a course, and is marked not yet filed."
         ]
        },
        "PD-E14": {
         "quotes": [
          "ask files added or changed: none",
          "ask files added or changed: none | commits naming an ask: none",
          "ask files added or changed: none"
         ],
         "reasons": [
          "No ask file was written or changed; the candidate question appears only in the digest.",
          "No ask file was written or changed; the candidate question appears only in the digest.",
          "No ask file was written or changed; the candidate question exists in the digest only."
         ]
        },
        "PD-E15": {
         "quotes": [
          "Applied without asking: 1 (`cycles/cycle-5/asks/`). Process notes: 1 (inbox).\nFindings: 1 issue, 1 question; on the hardening list: 2",
          "Applied without asking: 1 (`cycles/cycle-5/asks/`). Process notes: 1 (inbox).\nFindings: 1 issue, 1 question; on the hardening list: 2",
          "Applied without asking: 1 (`cycles/cycle-5/asks/`). Process notes: 1 (inbox).\nFindings: 1 issue, 1 question; on the hardening list: 2"
         ],
         "reasons": [
          "The applied ask, the process note, the findings by kind and the hardening list are all counted, but the record correction in the journal (\"record-correction: seam/json-report's owning symbols name format_json\") is not counted anywhere in the digest.",
          "The applied ask, the process note, the findings and the hardening list are counted, but the record correction in the journal (\"record-correction: seam/json-report's owning symbols name format_json\") is not counted anywhere in the digest.",
          "The applied ask, the process note, the findings by kind and the hardening list are all counted, but the record's one record correction is not counted anywhere in the digest."
         ]
        },
        "PD-E16": {
         "quotes": [
          "**Recommendation**: continue — the work stays on the three goals",
          "**Recommendation**: continue — the work stays on the three goals",
          "**Recommendation**: continue — the work stays on the three goals"
         ],
         "reasons": [
          "No goal drift fired, yet the recommendation is \"continue\" rather than none, and the completion ratio is not shown moving over the window.",
          "With no goal drift fired the recommendation should be none, but it is continue; the section also does not show how completion moved over the window.",
          "No drift fired, so the recommendation should be none, but the digest recommends continue; it also does not show how the ratio moved over the window."
         ]
        },
        "PD-E17": {
         "quotes": [
          "code: unchanged | task files changed: none",
          "code: unchanged | task files changed: none",
          "code: unchanged | task files changed: none"
         ],
         "reasons": [
          "The code, the repository and the task files are as the fixture left them.",
          "The code repository and the task files are as the fixture left them.",
          "The code and repository are as the fixture left them, and no task file changed."
         ]
        },
        "PD-E18": {
         "quotes": [
          "column rejected 1.0, inline 2.0; record: 1 send-backs, 2 inline-fix entries",
          "column rejected 1.0, inline 2.0; record: 1 send-backs, 2 inline-fix entries",
          "column {'carried': 1.0, 'created': 3.0, 'rejected': 1.0, 'inline': 2.0}; ... record: 1 send-backs, 2 inline-fix entries; drainage 0.67"
         ],
         "reasons": [
          "Carried 1, created 3, inline fixes 2 and drainage 0.67 (2/3) match the record; rejected is 1, which is what the fixture's state actually holds, despite the item's \"none here\".",
          "The cycle-5 column matches the record: carried 1 and created 3 per the archived states, 2 inline fixes, and drainage 2/3=0.67; rejected is 1, not the item's \"none here\", but the record (status counts, PD-S23's state reading) holds one send-back, so the digest counts as the record defines.",
          "The cycle's column matches the record's definitions (the fixture actually records 1 send-back, not none), drainage is 2/3, and the rejection rate of 0.50 is rejected over reviewed."
         ]
        },
        "PD-E2": {
         "quotes": [
          "rejection rate 0.50 (1 of 2) beside 2 inline fixes. This is not a clean batch",
          "rejection rate 0.50 (1 of 2) beside 2 inline fixes. This is not a clean batch",
          "rejection rate 0.50 (1 of 2) beside 2 inline fixes. This is not a clean batch"
         ],
         "reasons": [
          "The rejection count is read together with the inline fixes, and the digest explicitly says the batch is not clean.",
          "The digest reads the rejection rate and the inline fixes together and explicitly does not call the batch clean.",
          "The digest reads the rejection beside the inline fixes and explicitly denies that the batch is clean; the record shows 1 send-back, not none."
         ]
        },
        "PD-E3": {
         "quotes": [
          ".orchestrator/changes/json-report/cycles/cycle-5/pm-digest.md: present; other digests written: none",
          ".orchestrator/changes/json-report/cycles/cycle-5/pm-digest.md: present; other digests written: none",
          "cat > pm-digest.md <<EOF (in cycles/cycle-5); Other files added... (none)"
         ],
         "reasons": [
          "The digest is the single pm-digest.md in the cycle's directory, and no other file was added or changed in the home.",
          "The digest is the single pm-digest.md in the cycle's directory, and no other file was added in the home.",
          "One digest was written to the cycle's directory and no other file was added."
         ]
        },
        "PD-E4": {
         "quotes": [
          "I've written the digest and I'm handing back now.\n\npm cycle-5: 9 signals (2 ⚠), 1 candidate questions; ...",
          "I've written the digest and I'm handing back now.\n\npm cycle-5: 9 signals (2 ⚠), 1 candidate questions; ...",
          "I've written the digest and I'm handing back now.\n\npm cycle-5: 9 signals (2 ⚠), 1 candidate questions; .orchestrator/..."
         ],
         "reasons": [
          "The pm line itself is correct (9 marked signal lines, 2 ⚠, 1 not-yet-filed row, a home-relative path), but the hand-back is not one line because a preamble sentence comes before it.",
          "The final text is not one line: a preamble sentence comes before the form line, even though the form line's counts and path are right.",
          "The counts and path in the formatted line are right, but the hand-back is not one line: a preamble sentence comes before it."
         ]
        },
        "PD-E5": {
         "quotes": [
          "# PM digest — change: json-report — cycle-5 ... **Produced**: 2026-11-14T15:21:23Z ... **History window**: K=5",
          "# PM digest — change: json-report — cycle-5 ... **Produced**: 2026-11-14T15:21:23Z ... **History window**: K=5",
          "# PM digest — change: json-report — cycle-5 ... **Produced**: 2026-11-14T15:21:23Z ... **History window**: K=5"
         ],
         "reasons": [
          "The title names the change and the cycle, and the header gives the production time and the history window.",
          "The title names the change and the cycle, and the header gives the production time and the history window.",
          "The title names the change and the cycle, and the header gives the produced time and the window."
         ]
        },
        "PD-E6": {
         "quotes": [
          "## Throughput ... ## Critical path ... ## Agents ... ## Signals ... ## Asks for the user ... ## Trends to watch ... ## Meta-discoveries ... ## Goal-drift check",
          "Throughput: 1; Critical path: 2; Agents: 3; Signals: 4; Asks for the user: 5; Trends to watch: 6; Meta-discoveries: 7; Goal-drift check: 8",
          "## Throughput ... ## Critical path ... ## Agents ... ## Signals ... ## Asks for the user ... ## Trends to watch ... ## Meta-discoveries ... ## Goal-drift check"
         ],
         "reasons": [
          "All eight sections appear in the form's order.",
          "All eight sections are present in the form's order.",
          "All eight sections are present in the form's order."
         ]
        },
        "PD-E7": {
         "quotes": [
          "✓ **CASCADE** ... ✓ **REVIEW-BALANCE** ... ✓ **EXTERNALISATION**",
          "✓ **REVIEW-BALANCE**: ratio 1.0, healthy.",
          "✓ **CASCADE** ... ✓ **REVIEW-BALANCE** ... ✓ **EXTERNALISATION**"
         ],
         "reasons": [
          "The Signals section has nine lines, each marked ⚠, ✓ or ◦, and the healthy dimensions are shown with ✓.",
          "The Signals section has nine lines, each marked ⚠, ✓ or ◦, and the healthy dimensions (cascade, review balance, externalisation) are shown with ✓.",
          "The Signals section is non-empty, every line is marked, and the healthy dimensions are shown with ✓."
         ]
        },
        "PD-E8": {
         "quotes": [
          "cycle-1  cycle-2  cycle-3  cycle-4  cycle-5",
          "cycle-1  cycle-2  cycle-3  cycle-4  cycle-5",
          "cycle-1  cycle-2  cycle-3  cycle-4  cycle-5 / created ... drainage"
         ],
         "reasons": [
          "There are five cycle columns and exactly the eight form rows, with no row for started, blocked, failed or done.",
          "The table has five cycle columns and exactly the eight form rows, with no started, blocked, failed or done row.",
          "There are five cycle columns and the eight form rows, with no started, blocked, failed or done rows."
         ]
        },
        "PD-E9": {
         "quotes": [
          "on-path active   : 0 / 0 active tasks\n- on-path blocked  : 0\n- on-path complete : 2 / 2 total on-path",
          "on-path active   : 0 / 0 active tasks\n- on-path blocked  : 0\n- on-path complete : 2 / 2 total on-path",
          "on-path active   : 0 / 0 active tasks\n- on-path blocked  : 0\n- on-path complete : 2 / 2 total on-path"
         ],
         "reasons": [
          "The readout gives on-path active against all active, on-path blocked (none, so nothing blocks), and on-path complete against the total, matching the counts.",
          "The readout gives on-path active against all active, on-path blocked (none, so no blockers to name), and complete against a total that matches on_path_total with no unbatched tasks.",
          "The readout gives active against all active, blocked (none), and complete against the total, all matching critical_path, which has no unbatched tasks."
         ]
        }
       },
       "milestones": [],
       "n_calls": 9,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the PM's agent pass, from its brief, to its hand-back",
   "title": "Case pm-digest-form: the PM's agent pass writes a digest whose every part the form fixes",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T102049 t1 (skill-evals)"
  },
  {
   "capability": "project-manager",
   "id": "pm-failed-pass",
   "items": [
    {
     "checks": [
      {
       "id": "PF-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The computed counts outlast a failed prose pass"
     ],
     "id": "PF-E1",
     "question": "Do the computed counts outlast the failed prose pass?"
    },
    {
     "checks": [
      {
       "id": "PF-S2",
       "kind": "decide"
      },
      {
       "id": "PF-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A failed prose pass leaves no digest, and is run again"
     ],
     "id": "PF-E2",
     "question": "Is no digest written from the computed output alone?"
    },
    {
     "checks": [
      {
       "id": "PF-S3",
       "kind": "decide"
      },
      {
       "id": "PF-S4",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § A failed prose pass leaves no digest, and is run again",
      "project-manager § The PM's agent pass runs at sonnet, medium"
     ],
     "id": "PF-E3",
     "question": "Is the agent pass run again, at sonnet, medium?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "eb7eff4",
     "cites": {
      "PF-E1": [
       "project-manager § The computed counts outlast a failed prose pass"
      ],
      "PF-E2": [
       "project-manager § A failed prose pass leaves no digest, and is run again"
      ],
      "PF-E3": [
       "project-manager § A failed prose pass leaves no digest, and is run again",
       "project-manager § The PM's agent pass runs at sonnet, medium"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "PF-E1": [
       "PF-S1"
      ],
      "PF-E2": [
       "PF-S2",
       "PF-S5"
      ],
      "PF-E3": [
       "PF-S3",
       "PF-S4"
      ]
     },
     "items_then": [
      {
       "checks": [
        {
         "id": "PF-S1",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The computed counts outlast a failed prose pass"
       ],
       "id": "PF-E1",
       "question": "Do the computed counts outlast the failed prose pass?"
      },
      {
       "checks": [
        {
         "id": "PF-S2",
         "kind": "decide"
        },
        {
         "id": "PF-S5",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A failed prose pass leaves no digest, and is run again"
       ],
       "id": "PF-E2",
       "question": "Is no digest written from the computed output alone?"
      },
      {
       "checks": [
        {
         "id": "PF-S3",
         "kind": "decide"
        },
        {
         "id": "PF-S4",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § A failed prose pass leaves no digest, and is run again",
        "project-manager § The PM's agent pass runs at sonnet, medium"
       ],
       "id": "PF-E3",
       "question": "Is the agent pass run again, at sonnet, medium?"
      }
     ],
     "run": "20261114T102701",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PF-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks that pm-signals.json is present in the cycle's directory at the stop, which is what PF-E1 expects, and the directory listing confirms it.",
          "verdict": "agrees"
         },
         "desc": "the computed counts, pm-signals.json, still in the cycle's directory",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-1/pm-signals.json: present; changed since the fixture: False",
         "kind": "decide",
         "pass": true
        },
        "PF-S2": {
         "audit": {
          "agree": true,
          "reason": "pm-digest.md is absent from the end listing, and because the trial stops at the first spawn, any digest there could only be the phase agent's own, so absence observes PF-E2's first half.",
          "verdict": "agrees"
         },
         "desc": "no pm-digest.md in the cycle's directory at the stop",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-1/pm-digest.md: absent",
         "kind": "decide",
         "pass": true
        },
        "PF-S3": {
         "audit": {
          "agree": true,
          "reason": "The only spawn names the PM's pass and passes sonnet/medium, matching PF-E3.",
          "verdict": "agrees"
         },
         "desc": "the PM's agent pass is spawned at sonnet, medium (not applicable without its spawn)",
         "detail": "1 spawn(s), 1 naming the PM's pass; model sonnet, effort medium",
         "kind": "decide",
         "pass": true
        },
        "PF-S4": {
         "audit": {
          "agree": true,
          "reason": "The first Agent call's input mentions the Project Manager and pm-digest, correctly signalling that the first spawn is the PM's pass.",
          "verdict": "agrees"
         },
         "desc": "the first spawn is the PM's agent pass",
         "detail": "{\"description\": \"PM digest agent pass\", \"model\": \"sonnet\", \"effort\": \"medium\", \"prompt\": \"You are the Project Manager agent pass for integrate of change `json-report`, cycle `cycle-1`. Home: <trial>/home. Code root: <trial>/ho … [cut, 600 characters]",
         "kind": "signal",
         "pass": true
        },
        "PF-S5": {
         "audit": {
          "agree": true,
          "reason": "phase_gates.integrate.passed is false and there are no commits since the fixture, which observes PF-E2's second half (the gate not passed without a digest).",
          "verdict": "agrees"
         },
         "desc": "integrate's gate not passed, and no integrate close commit",
         "detail": "phase_gates.integrate: {\"passed\": false, \"checks\": {\"blocking_findings_resolved\": false, \"pm_digest_produced\": false, \"user_asks_routed\": false, \"open_tasks_refined_against_handshake\": false, \"handshake_artifact_written\": false}}; close commits: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "PF-E1": "pass",
        "PF-E2": "pass",
        "PF-E3": "pass"
       },
       "judge": {
        "PF-E1": {
         "quotes": [
          "pm-signals.json (in 'The cycle's directory at the end')",
          "pm-signals.json (in \"The cycle's directory at the end\")",
          "pm-signals.json (in the cycle's directory at the end)"
         ],
         "reasons": [
          "The cycle's directory at the end still lists pm-signals.json, and no files in the home changed since the fixture.",
          "pm-signals.json is still listed in the cycle's directory at the stop, and no files in the home were added, changed or removed.",
          "The cycle's directory listing at the end still contains pm-signals.json, and no files in the home were changed or removed."
         ]
        },
        "PF-E2": {
         "quotes": [
          "Files added, changed or removed in the home since the fixture (outside the code), each in full == (none)",
          "Files added, changed or removed in the home since the fixture ... (none); \"pm_digest_produced\": false",
          "\"pm_digest_produced\": false ... Files added, changed or removed in the home since the fixture: (none)"
         ],
         "reasons": [
          "No pm-digest.md was written by the phase agent, and integrate's gate remains passed: false with no close commit.",
          "The phase agent wrote no pm-digest.md, and integrate's gate stayed unpassed with no close commit.",
          "The phase agent wrote no pm-digest.md; it spawned the PM's agent pass to write it, and the integrate gate stays unpassed with no close commit."
         ]
        },
        "PF-E3": {
         "quotes": [
          "\"description\": \"PM digest agent pass\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"description\": \"PM digest agent pass\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"description\": \"PM digest agent pass\", \"model\": \"sonnet\", \"effort\": \"medium\""
         ],
         "reasons": [
          "The first Agent call re-spawns the PM's agent pass with model sonnet and effort medium.",
          "The first and only Agent call re-spawns the PM's agent pass to write pm-digest.md, and it passes model sonnet and effort medium.",
          "The first and only Agent call re-runs the PM's agent pass and passes model sonnet and effort medium."
         ]
        }
       },
       "milestones": [],
       "n_calls": 6,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the integrate phase agent, fresh from a handover after the PM's agent pass failed, to its first spawn or its hand-back",
   "title": "Case pm-failed-pass: after the PM's agent pass failed, integrate keeps the counts, writes no digest and runs the pass again",
   "trials": 3,
   "validated": "eb7eff4a71e086c0da25efde752508494e72dcda 20261114T102701 t1 (skill-evals)"
  },
  {
   "capability": "project-manager",
   "id": "pm-goal-drift",
   "items": [
    {
     "checks": [
      {
       "id": "PG-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § Every number in the digest is one the record's counts give"
     ],
     "id": "PG-E1",
     "question": "Is every number in the digest's tables and main-path readout one the computed counts give?"
    },
    {
     "checks": [
      {
       "id": "PG-S1",
       "kind": "decide"
      },
      {
       "id": "PG-S2",
       "kind": "decide"
      },
      {
       "id": "PG-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM hands back one line of its signals and its digest's path"
     ],
     "id": "PG-E2",
     "question": "Does the PM hand back one line of its signals, its candidate questions and the digest's path?"
    },
    {
     "checks": [
      {
       "id": "PG-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A digest with nothing to ask has an empty asks table"
     ],
     "id": "PG-E3",
     "question": "With nothing to ask, is the asks table empty and the digest complete?"
    },
    {
     "checks": [],
     "cites": [
      "project-manager § The goal-drift check shows the main path's completion"
     ],
     "id": "PG-E4",
     "question": "Does the goal-drift check show the main path's completion and how it moved?"
    },
    {
     "checks": [
      {
       "id": "PG-S10",
       "kind": "decide"
      },
      {
       "id": "PG-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § Goal drift fires when the main path stalls while other work completes"
     ],
     "id": "PG-E5",
     "question": "Does goal drift fire, with a recommendation and a one-line reason?"
    },
    {
     "checks": [
      {
       "id": "PG-S8",
       "kind": "decide"
      },
      {
       "id": "PG-S9",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § The goal-drift recommendation is one of the ask's options, or none"
     ],
     "id": "PG-E6",
     "question": "Is the recommendation one of the goal-drift ask's options?"
    },
    {
     "checks": [
      {
       "id": "PG-S5",
       "kind": "decide"
      },
      {
       "id": "PG-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The goal-drift recommendation changes nothing"
     ],
     "id": "PG-E7",
     "question": "Does the recommendation change nothing, the course the user's?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "5e12baf",
     "cites": {
      "PG-E1": [
       "project-manager § Every number in the digest is one the record's counts give"
      ],
      "PG-E2": [
       "project-manager § The PM hands back one line of its signals and its digest's path"
      ],
      "PG-E3": [
       "project-manager § A digest with nothing to ask has an empty asks table"
      ],
      "PG-E4": [
       "project-manager § The goal-drift check shows the main path's completion"
      ],
      "PG-E5": [
       "project-manager § Goal drift fires when the main path stalls while other work completes"
      ],
      "PG-E6": [
       "project-manager § The goal-drift recommendation is one of the ask's options, or none"
      ],
      "PG-E7": [
       "project-manager § The goal-drift recommendation changes nothing"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "PG-E1": [
       "PG-S6"
      ],
      "PG-E2": [
       "PG-S1",
       "PG-S2",
       "PG-S3"
      ],
      "PG-E3": [
       "PG-S7"
      ],
      "PG-E4": [],
      "PG-E5": [
       "PG-S10",
       "PG-S11"
      ],
      "PG-E6": [
       "PG-S8",
       "PG-S9"
      ],
      "PG-E7": [
       "PG-S4",
       "PG-S5"
      ]
     },
     "items_then": [
      {
       "checks": [
        {
         "id": "PG-S6",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § Every number in the digest is one the record's counts give"
       ],
       "id": "PG-E1",
       "question": "Is every number in the digest's tables and main-path readout one the computed counts give?"
      },
      {
       "checks": [
        {
         "id": "PG-S1",
         "kind": "decide"
        },
        {
         "id": "PG-S2",
         "kind": "decide"
        },
        {
         "id": "PG-S3",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM hands back one line of its signals and its digest's path"
       ],
       "id": "PG-E2",
       "question": "Does the PM hand back one line of its signals, its candidate questions and the digest's path?"
      },
      {
       "checks": [
        {
         "id": "PG-S7",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A digest with nothing to ask has an empty asks table"
       ],
       "id": "PG-E3",
       "question": "With nothing to ask, is the asks table empty and the digest complete?"
      },
      {
       "checks": [],
       "cites": [
        "project-manager § The goal-drift check shows the main path's completion"
       ],
       "id": "PG-E4",
       "question": "Does the goal-drift check show the main path's completion and how it moved?"
      },
      {
       "checks": [
        {
         "id": "PG-S10",
         "kind": "decide"
        },
        {
         "id": "PG-S11",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § Goal drift fires when the main path stalls while other work completes"
       ],
       "id": "PG-E5",
       "question": "Does goal drift fire, with a recommendation and a one-line reason?"
      },
      {
       "checks": [
        {
         "id": "PG-S8",
         "kind": "decide"
        },
        {
         "id": "PG-S9",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § The goal-drift recommendation is one of the ask's options, or none"
       ],
       "id": "PG-E6",
       "question": "Is the recommendation one of the goal-drift ask's options?"
      },
      {
       "checks": [
        {
         "id": "PG-S5",
         "kind": "decide"
        },
        {
         "id": "PG-S4",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The goal-drift recommendation changes nothing"
       ],
       "id": "PG-E7",
       "question": "Does the recommendation change nothing, the course the user's?"
      }
     ],
     "run": "20261114T102924",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PG-S1": {
         "audit": {
          "agree": true,
          "reason": "The digest is present at the cycle's path and no other digest was written, which the evidence confirms.",
          "verdict": "agrees"
         },
         "desc": "the digest is pm-digest.md in the cycle's directory, and no other digest is written",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-6/pm-digest.md: present; other digests written: none",
         "kind": "decide",
         "pass": true
        },
        "PG-S10": {
         "audit": {
          "agree": true,
          "reason": "The Signals section has a ⚠ line.",
          "verdict": "agrees"
         },
         "desc": "a line of the Signals section marked ⚠",
         "detail": "⚠ **GOAL-DRIFT / INVERSION**: no on-path task is done after 6 cycles (0 / 2), while 3 off-path tasks finished this cycle (table-width, ledger-bom, readme-usage). `report-json`, the on-path task for G1 (the `--json` output the change exists for), sits in no batch. The only active task is `refuse-opti … [cut, 327 characters]",
         "kind": "decide",
         "pass": true
        },
        "PG-S11": {
         "audit": {
          "agree": true,
          "reason": "The ⚠ line is labelled GOAL-DRIFT.",
          "verdict": "agrees"
         },
         "desc": "a ⚠ line of the Signals section names goal drift",
         "detail": "⚠ **GOAL-DRIFT / INVERSION**: no on-path task is done after 6 cycles (0 / 2), while 3 off-path tasks finished this cycle (table-width, ledger-bom, readme-usage). `report-json`, the on-path task for G1 (the `--json` output the change exists for), sits in no batch. The only active task is `refuse-opti … [cut, 327 characters]",
         "kind": "signal",
         "pass": true
        },
        "PG-S2": {
         "audit": {
          "agree": true,
          "reason": "The final text is one line of 114 characters in the form, with the digest's path relative to the home.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters in the PM's form, the digest's path relative to the home",
         "detail": "114 characters: pm cycle-6: 10 signals (1 ⚠), 1 candidate questions; .orchestrator/changes/json-report/cycles/cycle-6/pm-digest.md | the digest relative to the home: .orchestrator/changes/json-report/cycles/cycle-6/pm-digest.md",
         "kind": "decide",
         "pass": true
        },
        "PG-S3": {
         "audit": {
          "agree": true,
          "reason": "The whole digest has 10 marked lines (9 in Signals plus the Agents ◦), 1 ⚠ and 1 row not yet filed, matching the line's counts.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the digest's: its marked signal lines (⚠, ✓ and ◦), its ⚠ lines, its rows not yet filed",
         "detail": "line: 10 signals, 1 ⚠, 1 candidate questions | digest: the Signals section: 9 marked lines, 1 ⚠; the whole digest: 10 marked lines, 1 ⚠; 1 rows not yet filed",
         "kind": "decide",
         "pass": true
        },
        "PG-S4": {
         "audit": {
          "agree": true,
          "reason": "No ask files and no commits were added, which the evidence confirms.",
          "verdict": "agrees"
         },
         "desc": "no ask file added or changed, and no home commit naming an ask",
         "detail": "ask files added or changed: none | commits naming an ask: none (0 ask files at the fixture)",
         "kind": "decide",
         "pass": true
        },
        "PG-S5": {
         "audit": {
          "agree": true,
          "reason": "The code repository is reported unchanged and no task file changed, consistent with the tool calls, which only wrote the digest.",
          "verdict": "agrees"
         },
         "desc": "the code repository and its worktrees as the fixture left them, and no task file changed",
         "detail": "code: unchanged | task files changed: none",
         "kind": "decide",
         "pass": true
        },
        "PG-S6": {
         "audit": {
          "agree": true,
          "reason": "Every number in the three sections traces to pm-signals.json, as a manual check confirms.",
          "verdict": "agrees"
         },
         "desc": "every number in the throughput table, the main-path readout and the agents' tables is one the computed counts give",
         "detail": "numbers no count gives: {'Throughput': [], 'Critical path': [], 'Agents (its tables)': []}",
         "kind": "decide",
         "pass": true
        },
        "PG-S7": {
         "audit": {
          "agree": true,
          "reason": "A row is marked not yet filed, so not applicable is correct.",
          "verdict": "agrees"
         },
         "desc": "the asks table is empty (not applicable when the digest raises a candidate question)",
         "detail": "| 1 | The change has finished only side work (spreadsheet-style table, comment lines, README) and not the `--json` output. Should the next batch be `report-json` alone, ahead of anything else? | next-plan | Yes, `report-json` first | not yet filed |",
         "kind": "decide",
         "pass": null
        },
        "PG-S8": {
         "audit": {
          "agree": true,
          "reason": "The recommendation line names 'revise'.",
          "verdict": "agrees"
         },
         "desc": "the goal-drift check recommends one of revise, split, abandon and continue",
         "detail": "named: ['revise'] | \n- Critical-path completion ratio: 0/2 = 0.00, stagnant across the window while off-path completions continue at 2–3 per cycle and drainage stays at 1.0.\n- **Recommendation**: revise — pull `report-json` (G1) into the next batch and have the orchestrator confirm `refuse-options`  … [cut, 315 characters]",
         "kind": "decide",
         "pass": true
        },
        "PG-S9": {
         "audit": {
          "agree": true,
          "reason": "Exactly one option, 'revise', is named.",
          "verdict": "agrees"
         },
         "desc": "the goal-drift check names exactly one of them",
         "detail": "named: ['revise']",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "PG-E1": "pass",
        "PG-E2": "pass",
        "PG-E3": "n/a",
        "PG-E4": "pass",
        "PG-E5": "pass",
        "PG-E6": "pass",
        "PG-E7": "pass"
       },
       "judge": {
        "PG-E1": {
         "quotes": [
          "on-path complete : 0 / 2 total on-path (on-path task with no batch: report-json)",
          "on-path complete : 0 / 2 total on-path",
          "drainage         0.67     1.00     1.00     1.00     1.00     1.00 … on-path complete : 0 / 2 total on-path … unrecorded         4        0            0               0"
         ],
         "reasons": [
          "Every number checks out against pm-signals.json: the throughput rows match the five history cycles and cycle-6's counts, the main-path readout (1/1, 0, 0/2) matches critical_path, and the agents table (unrecorded 4 0 0 0) matches agents.quality.",
          "Each number in the throughput table matches its cycle in pm-signals.json, including cycle-5's reviewed 2, completed 3 and drainage 0.67/1.00. The main-path readout matches the counts (1/1 active, 0 blocked, 0/2 done), and so does the agents table (unrecorded 4 tasks, 0 rejections, 0 rework, 0 defects).",
          "Every number in the throughput table, the critical-path readout and the agents' table matches pm-signals.json: the history and current counts, the critical_path block, and the quality.by_implementor_setting entry (rejections 0, defects_after_done empty)."
         ]
        },
        "PG-E2": {
         "quotes": [
          "pm cycle-6: 10 signals (1 ⚠), 1 candidate questions; .orchestrator/changes/json-report/cycles/cycle-6/pm-digest.md",
          "pm cycle-6: 10 signals (1 ⚠), 1 candidate questions; .orchestrator/changes/json-report/cycles/cycle-6/pm-digest.md",
          "pm cycle-6: 10 signals (1 ⚠), 1 candidate questions; .orchestrator/changes/json-report/cycles/cycle-6/pm-digest.md"
         ],
         "reasons": [
          "The final text is one line of 114 characters in the PM's form, and its counts match the digest: 10 marked lines over the whole digest, 1 ⚠ and 1 row not yet filed, with the path relative to the home. The Bash echo's '7 signals' is not the hand-back.",
          "The final text is one line of 114 characters in the PM's form, and the path is relative to the home. Its counts match the digest: 10 marked lines over the whole digest (9 in Signals plus 1 in Agents), 1 ⚠ and 1 row not yet filed. The Bash echo's stale '7 signals' is not the hand-back.",
          "The hand-back is one line of 114 characters in the form's literal template; its 10 signals and 1 ⚠ match the whole digest's marked lines, its 1 question matches the one not-yet-filed row, and the path is relative to the home; only the plural 'questions' for a count of one departs from the singular grammar."
         ]
        },
        "PG-E3": {
         "quotes": [
          "| 1 | The change has finished only side work ... | next-plan | Yes, `report-json` first | not yet filed |",
          "| next-plan | Yes, `report-json` first | not yet filed |",
          "| 1 | The change has finished only side work … | next-plan | Yes, `report-json` first | not yet filed |"
         ],
         "reasons": [
          "The digest raises a candidate question marked not yet filed, so the item's WHEN is not met.",
          "The digest raises a candidate question marked not yet filed, so the scenario's WHEN is not met.",
          "The digest raises a candidate question marked not yet filed, so the scenario's condition of nothing to ask is not met."
         ]
        },
        "PG-E4": {
         "quotes": [
          "Critical-path completion ratio: 0/2 = 0.00, stagnant across the window",
          "Critical-path completion ratio: 0/2 = 0.00, stagnant across the window",
          "Critical-path completion ratio: 0/2 = 0.00, stagnant across the window while off-path completions continue at 2–3 per cycle"
         ],
         "reasons": [
          "The check gives the main path's completion ratio and says it stayed flat across the window.",
          "The goal-drift check gives the ratio 0/2 and says it is flat across the window.",
          "The goal-drift check gives the main path's completion ratio and states that it was flat ('stagnant') across the window."
         ]
        },
        "PG-E5": {
         "quotes": [
          "⚠ **GOAL-DRIFT / INVERSION**: no on-path task is done after 6 cycles (0 / 2) ... **Recommendation**: revise — pull `report-json` (G1) into the next batch",
          "⚠ **GOAL-DRIFT / INVERSION**: no on-path task is done after 6 cycles (0 / 2) … **Recommendation**: revise — pull `report-json` (G1) into the next batch",
          "⚠ **GOAL-DRIFT / INVERSION**: no on-path task is done after 6 cycles (0 / 2) … **Recommendation**: revise — pull `report-json` (G1) into the next batch"
         ],
         "reasons": [
          "A ⚠ goal-drift signal fires, and the goal-drift check recommends a course with a one-line reason: the main path is stagnant while off-path completions continue.",
          "A ⚠ goal-drift signal fires in Signals, and the check recommends revise with a one-line reason resting on the stagnant ratio while off-path work completes.",
          "A ⚠ goal-drift signal fires, and the goal-drift check recommends a course with a one-line reason: the main path is stagnant while off-path work completes."
         ]
        },
        "PG-E6": {
         "quotes": [
          "**Recommendation**: revise",
          "**Recommendation**: revise",
          "**Recommendation**: revise"
         ],
         "reasons": [
          "The recommendation is 'revise', one of the four options.",
          "The recommendation names revise, which is one of the ask's options.",
          "The recommendation is 'revise', which is one of the ask's four options."
         ]
        },
        "PG-E7": {
         "quotes": [
          "Recommendation | ... | not yet filed",
          "| 1 | … Should the next batch be `report-json` alone, ahead of anything else? | next-plan | Yes, `report-json` first | not yet filed |",
          "Other files added, changed or removed in the home since the fixture (outside the code), each in full == (none) … code repository's refs and its worktrees' status == unchanged since the fixture"
         ],
         "reasons": [
          "The code root, which holds the intent, is unchanged, no ask file was written and no task file changed; the course is put to the user as a question.",
          "The code root, which holds the intent, is unchanged and no ask file or other home file was written. The course is put to the user as a question rather than applied.",
          "No ask file was written (the row reads 'not yet filed'), the intent and code root are unchanged, no task file changed, and the course is put to the user as a question."
         ]
        }
       },
       "milestones": [],
       "n_calls": 6,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the PM's agent pass, from its brief, to its hand-back",
   "title": "Case pm-goal-drift: the PM's agent pass when goal drift fires and nothing is to be asked",
   "trials": 3,
   "validated": "5e12baf561a76897f015a37fb620660e4371d8a4 20261114T102924 t1 (skill-evals)"
  },
  {
   "capability": "project-manager",
   "id": "pm-thresholds",
   "items": [
    {
     "checks": [
      {
       "id": "PS-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § Every number in the digest is one the record's counts give"
     ],
     "id": "PS-E1",
     "question": "Is every number in the digest's tables and main-path readout one the computed counts give?"
    },
    {
     "checks": [
      {
       "id": "PS-S1",
       "kind": "decide"
      },
      {
       "id": "PS-S2",
       "kind": "decide"
      },
      {
       "id": "PS-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM hands back one line of its signals and its digest's path"
     ],
     "id": "PS-E2",
     "question": "Does the PM hand back one line of its signals, its candidate questions and the digest's path?"
    },
    {
     "checks": [],
     "cites": [
      "project-manager § Each signal line is marked actionable, healthy or informational"
     ],
     "id": "PS-E3",
     "question": "Is each signal line marked actionable, healthy or informational?"
    },
    {
     "checks": [
      {
       "id": "PS-S18",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § Spend that fell while quality fell is flagged"
     ],
     "id": "PS-E4",
     "question": "Is spend that fell while quality fell flagged?"
    },
    {
     "checks": [
      {
       "id": "PS-S18",
       "kind": "signal"
      },
      {
       "id": "PS-S19",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § An agent that ran at another setting than it was spawned at is flagged"
     ],
     "id": "PS-E5",
     "question": "Is the agent that ran at another setting than it was spawned at flagged?"
    },
    {
     "checks": [],
     "cites": [
      "project-manager § The PM proposes no change to an agent setting"
     ],
     "id": "PS-E6",
     "question": "Does the PM propose no change to an agent setting?"
    },
    {
     "checks": [],
     "cites": [
      "project-manager § A threshold the project does not set takes its default"
     ],
     "id": "PS-E7",
     "question": "Does a threshold the project does not set take its default?"
    },
    {
     "checks": [
      {
       "id": "PS-S10",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § Throughput inversion fires on falling drainage"
     ],
     "id": "PS-E8",
     "question": "Does throughput inversion fire on the falling drainage?"
    },
    {
     "checks": [
      {
       "id": "PS-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § Review starvation fires when merged tasks wait on review"
     ],
     "id": "PS-E9",
     "question": "Does review starvation fire, and recommend slowing the Implementors or more Reviewers?"
    },
    {
     "checks": [
      {
       "id": "PS-S17",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § Externalisation pressure fires on a growing cross-change backlog"
     ],
     "id": "PS-E10",
     "question": "Does externalisation pressure fire on the growing backlog?"
    },
    {
     "checks": [
      {
       "id": "PS-S21",
       "kind": "signal"
      },
      {
       "id": "PS-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A coherent externalised cluster is proposed for promotion"
     ],
     "id": "PS-E11",
     "question": "Is the coherent externalised cluster proposed for promotion, and not promoted?"
    },
    {
     "checks": [
      {
       "id": "PS-S12",
       "kind": "signal"
      },
      {
       "id": "PS-S20",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § Priority inversion fires when little active work is on the main path"
     ],
     "id": "PS-E12",
     "question": "Does priority inversion fire, recommending a return to the main path?"
    },
    {
     "checks": [
      {
       "id": "PS-S13",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § A task not done for too long is a stale signal"
     ],
     "id": "PS-E13",
     "question": "Is the task not done for too long a stale signal?"
    },
    {
     "checks": [
      {
       "id": "PS-S14",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § A task blocked for too long is a blocked-path signal"
     ],
     "id": "PS-E14",
     "question": "Is the task blocked for too long a blocked-path signal?"
    },
    {
     "checks": [
      {
       "id": "PS-S15",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § A class completing at half the average rate is a cycle anti-pattern"
     ],
     "id": "PS-E15",
     "question": "Is the slow class a cycle anti-pattern, named?"
    },
    {
     "checks": [
      {
       "id": "PS-S16",
       "kind": "signal"
      }
     ],
     "cites": [
      "project-manager § Recurring rejection reasons are a process signal and a candidate process ask"
     ],
     "id": "PS-E16",
     "question": "Are the recurring rejection reasons a process signal and a candidate process ask?"
    },
    {
     "checks": [
      {
       "id": "PS-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM writes no ask file"
     ],
     "id": "PS-E17",
     "question": "Does the PM write no ask file?"
    },
    {
     "checks": [
      {
       "id": "PS-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM changes no code and no task's text"
     ],
     "id": "PS-E18",
     "question": "Does the PM change no code and no task's text?"
    },
    {
     "checks": [
      {
       "id": "PS-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM writes nothing to the queue"
     ],
     "id": "PS-E19",
     "question": "Does the PM write nothing to the queue?"
    },
    {
     "checks": [
      {
       "id": "PS-S8",
       "kind": "decide"
      },
      {
       "id": "PS-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § The PM spawns no Implementor or Reviewer"
     ],
     "id": "PS-E20",
     "question": "Does the PM spawn no Implementor or Reviewer?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "96c805d",
     "cites": {
      "PS-E1": [
       "project-manager § Every number in the digest is one the record's counts give"
      ],
      "PS-E10": [
       "project-manager § Externalisation pressure fires on a growing cross-change backlog"
      ],
      "PS-E11": [
       "project-manager § A coherent externalised cluster is proposed for promotion"
      ],
      "PS-E12": [
       "project-manager § Priority inversion fires when little active work is on the main path"
      ],
      "PS-E13": [
       "project-manager § A task not done for too long is a stale signal"
      ],
      "PS-E14": [
       "project-manager § A task blocked for too long is a blocked-path signal"
      ],
      "PS-E15": [
       "project-manager § A class completing at half the average rate is a cycle anti-pattern"
      ],
      "PS-E16": [
       "project-manager § Recurring rejection reasons are a process signal and a candidate process ask"
      ],
      "PS-E17": [
       "project-manager § The PM writes no ask file"
      ],
      "PS-E18": [
       "project-manager § The PM changes no code and no task's text"
      ],
      "PS-E19": [
       "project-manager § The PM writes nothing to the queue"
      ],
      "PS-E2": [
       "project-manager § The PM hands back one line of its signals and its digest's path"
      ],
      "PS-E20": [
       "project-manager § The PM spawns no Implementor or Reviewer"
      ],
      "PS-E3": [
       "project-manager § Each signal line is marked actionable, healthy or informational"
      ],
      "PS-E4": [
       "project-manager § Spend that fell while quality fell is flagged"
      ],
      "PS-E5": [
       "project-manager § An agent that ran at another setting than it was spawned at is flagged"
      ],
      "PS-E6": [
       "project-manager § The PM proposes no change to an agent setting"
      ],
      "PS-E7": [
       "project-manager § A threshold the project does not set takes its default"
      ],
      "PS-E8": [
       "project-manager § Throughput inversion fires on falling drainage"
      ],
      "PS-E9": [
       "project-manager § Review starvation fires when merged tasks wait on review"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "PS-E1": [
       "PS-S6"
      ],
      "PS-E10": [
       "PS-S17"
      ],
      "PS-E11": [
       "PS-S21",
       "PS-S5"
      ],
      "PS-E12": [
       "PS-S12",
       "PS-S20"
      ],
      "PS-E13": [
       "PS-S13"
      ],
      "PS-E14": [
       "PS-S14"
      ],
      "PS-E15": [
       "PS-S15"
      ],
      "PS-E16": [
       "PS-S16"
      ],
      "PS-E17": [
       "PS-S4"
      ],
      "PS-E18": [
       "PS-S5"
      ],
      "PS-E19": [
       "PS-S7"
      ],
      "PS-E2": [
       "PS-S1",
       "PS-S2",
       "PS-S3"
      ],
      "PS-E20": [
       "PS-S8",
       "PS-S9"
      ],
      "PS-E3": [],
      "PS-E4": [
       "PS-S18"
      ],
      "PS-E5": [
       "PS-S18",
       "PS-S19"
      ],
      "PS-E6": [],
      "PS-E7": [],
      "PS-E8": [
       "PS-S10"
      ],
      "PS-E9": [
       "PS-S11"
      ]
     },
     "items_then": [
      {
       "checks": [
        {
         "id": "PS-S6",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § Every number in the digest is one the record's counts give"
       ],
       "id": "PS-E1",
       "question": "Is every number in the digest's tables and main-path readout one the computed counts give?"
      },
      {
       "checks": [
        {
         "id": "PS-S1",
         "kind": "decide"
        },
        {
         "id": "PS-S2",
         "kind": "decide"
        },
        {
         "id": "PS-S3",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM hands back one line of its signals and its digest's path"
       ],
       "id": "PS-E2",
       "question": "Does the PM hand back one line of its signals, its candidate questions and the digest's path?"
      },
      {
       "checks": [],
       "cites": [
        "project-manager § Each signal line is marked actionable, healthy or informational"
       ],
       "id": "PS-E3",
       "question": "Is each signal line marked actionable, healthy or informational?"
      },
      {
       "checks": [
        {
         "id": "PS-S18",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § Spend that fell while quality fell is flagged"
       ],
       "id": "PS-E4",
       "question": "Is spend that fell while quality fell flagged?"
      },
      {
       "checks": [
        {
         "id": "PS-S18",
         "kind": "signal"
        },
        {
         "id": "PS-S19",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § An agent that ran at another setting than it was spawned at is flagged"
       ],
       "id": "PS-E5",
       "question": "Is the agent that ran at another setting than it was spawned at flagged?"
      },
      {
       "checks": [],
       "cites": [
        "project-manager § The PM proposes no change to an agent setting"
       ],
       "id": "PS-E6",
       "question": "Does the PM propose no change to an agent setting?"
      },
      {
       "checks": [],
       "cites": [
        "project-manager § A threshold the project does not set takes its default"
       ],
       "id": "PS-E7",
       "question": "Does a threshold the project does not set take its default?"
      },
      {
       "checks": [
        {
         "id": "PS-S10",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § Throughput inversion fires on falling drainage"
       ],
       "id": "PS-E8",
       "question": "Does throughput inversion fire on the falling drainage?"
      },
      {
       "checks": [
        {
         "id": "PS-S11",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § Review starvation fires when merged tasks wait on review"
       ],
       "id": "PS-E9",
       "question": "Does review starvation fire, and recommend slowing the Implementors or more Reviewers?"
      },
      {
       "checks": [
        {
         "id": "PS-S17",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § Externalisation pressure fires on a growing cross-change backlog"
       ],
       "id": "PS-E10",
       "question": "Does externalisation pressure fire on the growing backlog?"
      },
      {
       "checks": [
        {
         "id": "PS-S21",
         "kind": "signal"
        },
        {
         "id": "PS-S5",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A coherent externalised cluster is proposed for promotion"
       ],
       "id": "PS-E11",
       "question": "Is the coherent externalised cluster proposed for promotion, and not promoted?"
      },
      {
       "checks": [
        {
         "id": "PS-S12",
         "kind": "signal"
        },
        {
         "id": "PS-S20",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § Priority inversion fires when little active work is on the main path"
       ],
       "id": "PS-E12",
       "question": "Does priority inversion fire, recommending a return to the main path?"
      },
      {
       "checks": [
        {
         "id": "PS-S13",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § A task not done for too long is a stale signal"
       ],
       "id": "PS-E13",
       "question": "Is the task not done for too long a stale signal?"
      },
      {
       "checks": [
        {
         "id": "PS-S14",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § A task blocked for too long is a blocked-path signal"
       ],
       "id": "PS-E14",
       "question": "Is the task blocked for too long a blocked-path signal?"
      },
      {
       "checks": [
        {
         "id": "PS-S15",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § A class completing at half the average rate is a cycle anti-pattern"
       ],
       "id": "PS-E15",
       "question": "Is the slow class a cycle anti-pattern, named?"
      },
      {
       "checks": [
        {
         "id": "PS-S16",
         "kind": "signal"
        }
       ],
       "cites": [
        "project-manager § Recurring rejection reasons are a process signal and a candidate process ask"
       ],
       "id": "PS-E16",
       "question": "Are the recurring rejection reasons a process signal and a candidate process ask?"
      },
      {
       "checks": [
        {
         "id": "PS-S4",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM writes no ask file"
       ],
       "id": "PS-E17",
       "question": "Does the PM write no ask file?"
      },
      {
       "checks": [
        {
         "id": "PS-S5",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM changes no code and no task's text"
       ],
       "id": "PS-E18",
       "question": "Does the PM change no code and no task's text?"
      },
      {
       "checks": [
        {
         "id": "PS-S7",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM writes nothing to the queue"
       ],
       "id": "PS-E19",
       "question": "Does the PM write nothing to the queue?"
      },
      {
       "checks": [
        {
         "id": "PS-S8",
         "kind": "decide"
        },
        {
         "id": "PS-S9",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § The PM spawns no Implementor or Reviewer"
       ],
       "id": "PS-E20",
       "question": "Does the PM spawn no Implementor or Reviewer?"
      }
     ],
     "run": "20261114T112830",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PS-S1": {
         "audit": {
          "agree": true,
          "reason": "The digest is in the cycle's directory and no other digest was written, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the digest is pm-digest.md in the cycle's directory, and no other digest is written",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md: present; other digests written: none",
         "kind": "decide",
         "pass": true
        },
        "PS-S10": {
         "audit": {
          "agree": true,
          "reason": "The ⚠ THROUGHPUT line fires on drainage below 1.0.",
          "verdict": "agrees"
         },
         "desc": "a ⚠ throughput line in Signals",
         "detail": "⚠ **THROUGHPUT**: drainage 0.60, 0.60, 0.75, 0.60: below 1.0 for 4 cycles. The queue is growing. Cycle 4 also created 10 tasks (2 carried) against 6 completed.",
         "kind": "signal",
         "pass": true
        },
        "PS-S11": {
         "audit": {
          "agree": true,
          "reason": "The ⚠ review-starvation line is present and also recommends more Reviewers.",
          "verdict": "agrees"
         },
         "desc": "a ⚠ review-starvation line in Signals",
         "detail": "⚠ **REVIEW-BALANCE**: 4 tasks need review against 2 in progress (ratio 2.0, threshold 1.5). Review starvation: json-escape, json-zero, json-sign and json-nan are waiting. Only 2 were reviewed this cycle. Recommend scaling Reviewer parallelism before starting more implementation.",
         "kind": "signal",
         "pass": true
        },
        "PS-S12": {
         "audit": {
          "agree": true,
          "reason": "The ⚠ INVERSION line fires and recommends a return to the critical path.",
          "verdict": "agrees"
         },
         "desc": "a ⚠ priority-inversion line in Signals",
         "detail": "⚠ **INVERSION**: 1 of 9 active tasks is on the critical path. `refuse-options` (on-path) is still ready and unstarted, while the open work is mostly bug and refactor tasks (refactor: 0 of 4 done).",
         "kind": "signal",
         "pass": true
        },
        "PS-S13": {
         "audit": {
          "agree": true,
          "reason": "It matched 'stale threshold' inside the blocked-path line for ledger-bom; no stale signal exists for the four-cycle task.",
          "verdict": "suspect"
         },
         "desc": "a stale line in Signals",
         "detail": "⚠ **BLOCKED**: `ledger-bom` is blocked on \"the ledger sample from the bank\", with no ask behind it. Blocked in each of cycles 2, 3 and 4 (stale threshold 3). It is the source of 2 externalised follow-ups (csv-header, csv-quote). Candidate question below.",
         "kind": "signal",
         "pass": true
        },
        "PS-S14": {
         "audit": {
          "agree": true,
          "reason": "The ⚠ BLOCKED line is a blocked-path signal for ledger-bom.",
          "verdict": "agrees"
         },
         "desc": "a blocked-path line in Signals",
         "detail": "⚠ **BLOCKED**: `ledger-bom` is blocked on \"the ledger sample from the bank\", with no ask behind it. Blocked in each of cycles 2, 3 and 4 (stale threshold 3). It is the source of 2 externalised follow-ups (csv-header, csv-quote). Candidate question below.",
         "kind": "signal",
         "pass": true
        },
        "PS-S15": {
         "audit": {
          "agree": true,
          "reason": "It matched the word 'anti-pattern' on the send-back line, which does not name the slow refactor class the item requires.",
          "verdict": "suspect"
         },
         "desc": "a cycle anti-pattern line in Signals",
         "detail": "⚠ **ANTI-PATTERN**: three send-backs this cycle (report-json, json-escape, json-zero) share one reason: \"the test re-states the implementation instead of the scenario\". Cycle 3 had 2 rejections. A process bug in how the tests for the JSON tasks are briefed, not three separate defects. Candidate `pro … [cut, 330 characters]",
         "kind": "signal",
         "pass": true
        },
        "PS-S16": {
         "audit": {
          "agree": true,
          "reason": "The process signal and its candidate process ask are present.",
          "verdict": "agrees"
         },
         "desc": "a line of the Signals section mentions process",
         "detail": "⚠ **ANTI-PATTERN**: three send-backs this cycle (report-json, json-escape, json-zero) share one reason: \"the test re-states the implementation instead of the scenario\". Cycle 3 had 2 rejections. A process bug in how the tests for the JSON tasks are briefed, not three separate defects. Candidate `pro … [cut, 330 characters]",
         "kind": "signal",
         "pass": true
        },
        "PS-S17": {
         "audit": {
          "agree": true,
          "reason": "It passed on lines that mention externalisation, but the EXTERNALISATION line declines the signal ('no pressure yet'), so the signal did not fire.",
          "verdict": "suspect"
         },
         "desc": "an externalisation line in Signals",
         "detail": "⚠ **BLOCKED**: `ledger-bom` is blocked on \"the ledger sample from the bank\", with no ask behind it. Blocked in each of cycles 2, 3 and 4 (stale threshold 3). It is the source of 2 externalised follow-ups (csv-header, csv-quote). Candidate question below. | ◦ **EXTERNALISATION**: `.tasks/` holds 3 ta … [cut, 600 characters]",
         "kind": "signal",
         "pass": true
        },
        "PS-S18": {
         "audit": {
          "agree": false,
          "reason": "It sees only the ⚠ on the agent-settings line; for PS-E4 that line does not name opus/medium as the setting whose spend fell against rising rejections.",
          "verdict": "suspect"
         },
         "desc": "an agent-settings line marked ⚠",
         "detail": "⚠ **AGENT SETTINGS**: the one recorded implementor (report-json) was called at opus/medium but its transcript shows sonnet/medium (3 requests), so the record does not say what ran. Rejections rose 0, 1, 2, 3 over the four cycles while per-agent spend fell from 10000 to 4000 and then rose to 7875. On … [cut, 381 characters]",
         "kind": "signal",
         "pass": true
        },
        "PS-S19": {
         "audit": {
          "agree": true,
          "reason": "It requires the agent id, but the digest identifies the single mismatched agent unambiguously as the implementor of report-json, which meets the item's 'names that agent'.",
          "verdict": "suspect"
         },
         "desc": "the digest names the agent that ran at another setting than it was spawned at",
         "detail": "mismatched agents in the counts: ['a1793400001imp']",
         "kind": "signal",
         "pass": false
        },
        "PS-S2": {
         "audit": {
          "agree": true,
          "reason": "The line has the required form and length and gives the digest's path relative to the home.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters in the PM's form, the digest's path relative to the home",
         "detail": "113 characters: pm cycle-4: 8 signals (6 ⚠), 2 candidate questions; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md | the digest relative to the home: .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md",
         "kind": "decide",
         "pass": true
        },
        "PS-S20": {
         "audit": {
          "agree": true,
          "reason": "No line proposes a pause.",
          "verdict": "agrees"
         },
         "desc": "the digest recommends no pause (no cascade fires): no line mentions a pause but to decline one",
         "detail": "",
         "kind": "signal",
         "pass": true
        },
        "PS-S21": {
         "audit": {
          "agree": true,
          "reason": "The only not-yet-filed row is about the ledger sample, and the digest says 'no promotion is proposed'.",
          "verdict": "agrees"
         },
         "desc": "a row not yet filed asks whether to promote the externalised cluster",
         "detail": "| 1 | Can you send the ledger sample from the bank (the file that has the byte-order mark)? Without it `ledger-bom` cannot move. | ledger-bom | Send it; if it can't be had, drop ledger-bom and its two csv follow-ups | not yet filed |",
         "kind": "signal",
         "pass": false
        },
        "PS-S3": {
         "audit": {
          "agree": false,
          "reason": "It correctly finds that the hand-back's 2 candidate questions do not match the one not-yet-filed row in the asks table.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the digest's: its marked signal lines (⚠, ✓ and ◦), its ⚠ lines, its rows not yet filed",
         "detail": "line: 8 signals, 6 ⚠, 2 candidate questions | digest: the Signals section: 7 marked lines, 5 ⚠; the whole digest: 8 marked lines, 6 ⚠; 1 rows not yet filed",
         "kind": "decide",
         "pass": false
        },
        "PS-S4": {
         "audit": {
          "agree": true,
          "reason": "No ask file was added or changed, and no commit names an ask.",
          "verdict": "agrees"
         },
         "desc": "no ask file added or changed, and no home commit naming an ask",
         "detail": "ask files added or changed: none | commits naming an ask: none (0 ask files at the fixture)",
         "kind": "decide",
         "pass": true
        },
        "PS-S5": {
         "audit": {
          "agree": true,
          "reason": "Code and task files are unchanged, which covers the not-promoted half of PS-E11 and all of PS-E18.",
          "verdict": "agrees"
         },
         "desc": "the code repository and its worktrees as the fixture left them, and no task file changed",
         "detail": "code: unchanged | task files changed: none",
         "kind": "decide",
         "pass": true
        },
        "PS-S6": {
         "audit": {
          "agree": true,
          "reason": "Every table and readout number traces to pm-signals.json.",
          "verdict": "agrees"
         },
         "desc": "every number in the throughput table, the main-path readout and the agents' tables is one the computed counts give",
         "detail": "numbers no count gives: {'Throughput': [], 'Critical path': [], 'Agents (its tables)': []}",
         "kind": "decide",
         "pass": true
        },
        "PS-S7": {
         "audit": {
          "agree": true,
          "reason": "No task's queue fields changed.",
          "verdict": "agrees"
         },
         "desc": "no task's status, labels, priority, class, blocker note, blocked-by or main-path mark changed in the state",
         "detail": "{}",
         "kind": "decide",
         "pass": true
        },
        "PS-S8": {
         "audit": {
          "agree": true,
          "reason": "No implementor or reviewer agent entry was added.",
          "verdict": "agrees"
         },
         "desc": "no Implementor or Reviewer added to the change's record",
         "detail": "agents added: []",
         "kind": "decide",
         "pass": true
        },
        "PS-S9": {
         "audit": {
          "agree": true,
          "reason": "The tool calls show no Agent call.",
          "verdict": "agrees"
         },
         "desc": "no Agent call (no cascade fires, so the PM has nothing to spawn)",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "PS-E1": "pass",
        "PS-E10": "fail",
        "PS-E11": "fail",
        "PS-E12": "pass",
        "PS-E13": "fail",
        "PS-E14": "pass",
        "PS-E15": "fail",
        "PS-E16": "pass",
        "PS-E17": "pass",
        "PS-E18": "pass",
        "PS-E19": "pass",
        "PS-E2": "fail",
        "PS-E20": "pass",
        "PS-E3": "pass",
        "PS-E4": "fail",
        "PS-E5": "pass",
        "PS-E6": "pass",
        "PS-E7": "fail",
        "PS-E8": "pass",
        "PS-E9": "pass"
       },
       "judge": {
        "PS-E1": {
         "quotes": [
          "implementor       1    opus/medium     7875  100%      7875       3938    0.01",
          "drainage         0.60     0.60     0.75     0.60",
          "drainage         0.60     0.60     0.75     0.60 | on-path active   : 1 / 9 | implementor       1    opus/medium     7875  100%      7875       3938    0.01"
         ],
         "reasons": [
          "Every number in the throughput table, the critical-path readout and both agents' tables matches pm-signals.json, either directly or as a list length (rework_tasks: 3).",
          "Every number in the throughput table, the main-path readout and the agents' tables matches pm-signals.json, and the '4 cycles' annotation is borne out by the four recorded drainage values.",
          "Every number in the throughput table, the critical-path readout and the agents' tables matches pm-signals.json's counts, history, critical_path and agents fields, and the annotations only frame them."
         ]
        },
        "PS-E10": {
         "quotes": [
          "Externalised per cycle was 1, 1, 1, 0, so not monotonic and no pressure yet.",
          "Externalised per cycle was 1, 1, 1, 0, so not monotonic and no pressure yet.",
          "◦ **EXTERNALISATION**: ... Externalised per cycle was 1, 1, 1, 0, so not monotonic and no pressure yet."
         ],
         "reasons": [
          "The PM explicitly declines externalisation pressure despite one task externalised in each of cycles 1-3.",
          "The PM declines the externalisation-pressure signal instead of firing it on the backlog that grew through cycles 1-3.",
          "The line explicitly declines externalisation pressure instead of firing it on one externalisation in each of cycles 1-3."
         ]
        },
        "PS-E11": {
         "quotes": [
          "so no promotion is proposed",
          "so no promotion is proposed.",
          "The cluster is about CSV, which the intent's N1 (no other output format) may rule out of this change, so no promotion is proposed."
         ],
         "reasons": [
          "Backlog items are left in place, but no candidate question proposes promoting the cluster.",
          "The cluster is left in the backlog, but the digest has no candidate question proposing its promotion.",
          "The backlog is left in place, but no candidate question about promoting the three-item cluster is put to the user."
         ]
        },
        "PS-E12": {
         "quotes": [
          "→ recommend returning to the critical path; `refuse-options` first.",
          "→ recommend returning to the critical path; `refuse-options` first.",
          "⚠ **INVERSION**: 1 of 9 active tasks is on the critical path. ... → recommend returning to the critical path; `refuse-options` first."
         ],
         "reasons": [
          "A ⚠ priority-inversion signal recommends returning to the main path, and no pause is recommended.",
          "A ⚠ priority-inversion signal recommends a return to the main path, and no pause is recommended.",
          "A ⚠ priority-inversion signal recommends returning to the main path, and no pause is recommended anywhere."
         ]
        },
        "PS-E13": {
         "quotes": [
          "Blocked in each of cycles 2, 3 and 4 (stale threshold 3).",
          "Blocked in each of cycles 2, 3 and 4 (stale threshold 3).",
          "Blocked in each of cycles 2, 3 and 4 (stale threshold 3)."
         ],
         "reasons": [
          "The only mention of staleness is inside the blocked-path line for ledger-bom; no stale signal is raised for the task four cycles not done.",
          "No stale signal is raised for the task four cycles not done; 'stale threshold' appears only inside the blocked-path line about ledger-bom.",
          "The only mention of stale is inside the blocked-task line for ledger-bom; there is no stale signal for a task not done for four cycles."
         ]
        },
        "PS-E14": {
         "quotes": [
          "⚠ **BLOCKED**: `ledger-bom` is blocked on \"the ledger sample from the bank\"... Blocked in each of cycles 2, 3 and 4",
          "⚠ **BLOCKED**: `ledger-bom` is blocked on \"the ledger sample from the bank\" ... Blocked in each of cycles 2, 3 and 4",
          "⚠ **BLOCKED**: `ledger-bom` is blocked on \"the ledger sample from the bank\", with no ask behind it. Blocked in each of cycles 2, 3 and 4"
         ],
         "reasons": [
          "A ⚠ blocked-path signal names the task blocked three cycles.",
          "A ⚠ blocked-path signal names the task blocked for three cycles.",
          "A ⚠ blocked-path signal names the task blocked for three cycles."
         ]
        },
        "PS-E15": {
         "quotes": [
          "while the open work is mostly bug and refactor tasks (refactor: 0 of 4 done)",
          "(refactor: 0 of 4 done)",
          "while the open work is mostly bug and refactor tasks (refactor: 0 of 4 done)."
         ],
         "reasons": [
          "The refactor class appears only in passing in the inversion line; no anti-pattern signal names it as completing at under half the average rate (the ANTI-PATTERN line is about send-backs).",
          "Refactor's slow completion is mentioned only in passing inside the inversion line; the anti-pattern line is about rejection reasons, so no cycle anti-pattern names the slow class.",
          "The slow refactor class appears only as an aside in the inversion line; the ANTI-PATTERN line is about send-backs, so no anti-pattern signal names refactor as completing at under half the rate."
         ]
        },
        "PS-E16": {
         "quotes": [
          "share one reason: \"the test re-states the implementation instead of the scenario\"... Candidate `process` ask (see Process notes).",
          "share one reason: \"the test re-states the implementation instead of the scenario\". ... A process bug ... Candidate `process` ask",
          "share one reason: \"the test re-states the implementation instead of the scenario\". ... A process bug ... Candidate `process` ask (see Process notes)."
         ],
         "reasons": [
          "The recurring rejection reason is flagged as a process problem, named, and given as a candidate process ask.",
          "The recurring rejection reason is named as a process problem and as a candidate process ask.",
          "The recurring rejection reason is named as a process problem and as a candidate process ask."
         ]
        },
        "PS-E17": {
         "quotes": [
          "ask files added or changed: none",
          "ask files added or changed: none",
          "ask files added or changed: none"
         ],
         "reasons": [
          "No ask file was written; the candidates appear only in the digest.",
          "No ask file was written; the candidates appear only in the digest.",
          "No ask file was written; the candidates appear only in the digest, marked not yet filed."
         ]
        },
        "PS-E18": {
         "quotes": [
          "code: unchanged | task files changed: none",
          "code: unchanged | task files changed: none",
          "code: unchanged | task files changed: none"
         ],
         "reasons": [
          "The code and the task files are as the fixture had them.",
          "The code and every task file are as they were.",
          "The code and every task file are as the fixture had them."
         ]
        },
        "PS-E19": {
         "quotes": [
          "observed: {}",
          "observed: {}",
          "Restore the critical-path focus (`refuse-options`) and get tasks for G2 and G3 into the state."
         ],
         "reasons": [
          "No task's queue fields changed; re-prioritisation appears only as recommendations.",
          "No task's queue fields changed; the re-prioritisation is a recommendation in the digest.",
          "No task's fields changed in the state, and re-prioritisation appears only as recommendations in the digest."
         ]
        },
        "PS-E2": {
         "quotes": [
          "pm cycle-4: 8 signals (6 ⚠), 2 candidate questions",
          "pm cycle-4: 8 signals (6 ⚠), 2 candidate questions; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md",
          "pm cycle-4: 8 signals (6 ⚠), 2 candidate questions; .orchestrator/changes/json-report/cycles/cycle-4/pm-digest.md"
         ],
         "reasons": [
          "The line's form and path are right, but the digest's asks table has one row marked not yet filed; the process candidate sits in Process notes, not among the questions, so '2 candidate questions' is not the digest's count.",
          "The line has 113 characters, follows the PM's form and gives the path relative to the home; its counts match the digest: 8 marked lines, 6 of them ⚠, and two not-yet-filed candidates (the ledger-sample question and the 'Candidate `process` ask').",
          "The line has the right form, length and relative path, and its 8/6 matches the digest's marked lines, but the asks table holds only one row marked not yet filed, so '2 candidate questions' is not the digest's count (the process candidate sits only in the Process notes line)."
         ]
        },
        "PS-E20": {
         "quotes": [
          "agents added: []",
          "agents added: []",
          "agents added: []"
         ],
         "reasons": [
          "There was no Agent call and no implementor or reviewer was added to the record.",
          "No Agent call was made and no Implementor or Reviewer was added to the record.",
          "The main thread made no Agent call and no agent was added to the record."
         ]
        },
        "PS-E3": {
         "quotes": [
          "◦ **EXTERNALISATION**: ... so not monotonic and no pressure yet",
          "◦ **EXTERNALISATION**: ... not monotonic and no pressure yet",
          "✓ **CASCADE**: no task has more than the threshold of 3 follow-ups ... ◦ **EXTERNALISATION**: ... not monotonic and no pressure yet."
         ],
         "reasons": [
          "Each Signals line and the agent-settings line carries a mark that fits its words: ⚠ where action is called for, ✓ for the healthy cascade, ◦ for the trend not yet acted on.",
          "Each signal line has a mark that fits its words: ⚠ where it calls for action, ✓ on the healthy cascade, ◦ on the trend the PM judged not actionable.",
          "Each line's mark fits its words: ⚠ on the lines that call for action, ✓ on the healthy cascade, ◦ on the externalisation trend its words call not yet pressing."
         ]
        },
        "PS-E4": {
         "quotes": [
          "Rejections rose 0, 1, 2, 3 over the four cycles while per-agent spend fell from 10000 to 4000 and then rose to 7875.",
          "⚠ **AGENT SETTINGS**: ... called at opus/medium ... Rejections rose 0, 1, 2, 3 over the four cycles while per-agent spend fell from 10000 to 4000",
          "Rejections rose 0, 1, 2, 3 over the four cycles while per-agent spend fell from 10000 to 4000 and then rose to 7875."
         ],
         "reasons": [
          "The ⚠ line mentions opus/medium only as the called setting of report-json and reports spend as having risen this cycle; it never names opus/medium as the setting whose spend fell (30000, 19494, 12000, 7875) while rejections rose.",
          "The ⚠ agent-settings line names opus/medium and sets falling spend against rising rejections, though it reads spend per agent rather than by total cost weight.",
          "The line is ⚠ but reads per-agent spend, which it says rose this cycle; it never states that the Implementors' cost weight fell (30000, 19494, 12000, 7875) and never names opus/medium as the setting whose spend fell while rejections rose."
         ]
        },
        "PS-E5": {
         "quotes": [
          "⚠ **AGENT SETTINGS**: the one recorded implementor (report-json) was called at opus/medium but its transcript shows sonnet/medium (3 requests)",
          "the one recorded implementor (report-json) was called at opus/medium but its transcript shows sonnet/medium",
          "⚠ **AGENT SETTINGS**: the one recorded implementor (report-json) was called at opus/medium but its transcript shows sonnet/medium (3 requests)"
         ],
         "reasons": [
          "The line is marked ⚠ and identifies the single mismatched agent unambiguously, by its role and task, though not by its id.",
          "The ⚠ line flags the mismatch and identifies the only recorded agent unambiguously by role and task, though not by its id.",
          "The ⚠ agent-settings line flags the mismatch and identifies the one mismatched agent unambiguously by role and task, though not by its id."
         ]
        },
        "PS-E6": {
         "quotes": [
          "Recommend scaling Reviewer parallelism before starting more implementation.",
          "Recommend scaling Reviewer parallelism before starting more implementation.",
          "Recommend scaling Reviewer parallelism before starting more implementation."
         ],
         "reasons": [
          "The digest reads spend against quality and proposes no change to any agent's model or effort.",
          "No recommendation in the digest changes any agent's model or effort.",
          "The digest proposes no change to any agent's model or effort; its recommendations concern cadence, focus and state hygiene."
         ]
        },
        "PS-E7": {
         "quotes": [
          "Externalised per cycle was 1, 1, 1, 0, so not monotonic and no pressure yet.",
          "(ratio 2.0, threshold 1.5) ... **History window**: K=5 ... (stale threshold 3)",
          "Externalised per cycle was 1, 1, 1, 0, so not monotonic and no pressure yet."
         ],
         "reasons": [
          "Drainage, review balance, K=5 and stale threshold 3 are applied, but the backlog-growth default (three cycles of growth) is not applied: the signal is declined.",
          "The digest uses the defaults (drainage 1.0, review 1.5, K=5) and the configured stale threshold of 3; whether externalisation fired is judged under PS-E10.",
          "Drainage, review balance, the K=5 window and the configured stale threshold of 3 are applied, but the backlog-growth signal does not fire at its default of three cycles of growth."
         ]
        },
        "PS-E8": {
         "quotes": [
          "⚠ **THROUGHPUT**: drainage 0.60, 0.60, 0.75, 0.60: below 1.0 for 4 cycles.",
          "⚠ **THROUGHPUT**: drainage 0.60, 0.60, 0.75, 0.60: below 1.0 for 4 cycles.",
          "⚠ **THROUGHPUT**: drainage 0.60, 0.60, 0.75, 0.60: below 1.0 for 4 cycles."
         ],
         "reasons": [
          "A ⚠ throughput signal fires on drainage below 1.0 for at least three consecutive cycles.",
          "A ⚠ throughput signal fires on drainage below 1.0 across the consecutive cycles.",
          "A ⚠ throughput signal fires on drainage below 1.0 across consecutive cycles."
         ]
        },
        "PS-E9": {
         "quotes": [
          "Recommend scaling Reviewer parallelism before starting more implementation.",
          "Recommend scaling Reviewer parallelism before starting more implementation.",
          "⚠ **REVIEW-BALANCE**: 4 tasks need review against 2 in progress (ratio 2.0, threshold 1.5). ... Recommend scaling Reviewer parallelism"
         ],
         "reasons": [
          "A ⚠ review-starvation signal fires and recommends more Reviewers in parallel.",
          "A ⚠ review-starvation signal recommends more Reviewers in parallel.",
          "A ⚠ review-starvation signal fires and recommends more Reviewers in parallel."
         ]
        }
       },
       "milestones": [],
       "n_calls": 6,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the PM's agent pass, from its brief, to its hand-back",
   "title": "Case pm-thresholds: the PM's agent pass over a cycle whose counts cross every threshold but the cascade's",
   "trials": 3,
   "validated": "96c805dfc0cec9f18656cfc5108f9c7596bc9976 20261114T112830 t1 (skill-evals)"
  },
  {
   "capability": "record",
   "id": "record-close-refused",
   "items": [
    {
     "checks": [
      {
       "id": "CR-S1",
       "kind": "decide"
      },
      {
       "id": "CR-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "record § A cycle closes only after integrate's gate passes"
     ],
     "id": "CR-E1",
     "question": "With integrate's gate not passed, does the cycle stay open?"
    },
    {
     "checks": [
      {
       "id": "CR-S4",
       "kind": "decide"
      },
      {
       "id": "CR-S2",
       "kind": "decide"
      },
      {
       "id": "CR-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "project-manager § A failed prose pass leaves no digest, and is run again",
      "project-manager § The computed counts outlast a failed prose pass",
      "project-manager § The PM's agent pass runs at sonnet, medium"
     ],
     "id": "CR-E2",
     "question": "Is the failed prose pass run again, with no digest written from the computed output and the counts kept?"
    },
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
    },
    {
     "checks": [
      {
       "id": "CR-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "record § No role edits the state file by hand"
     ],
     "id": "CR-E4",
     "question": "Is the state written only through the state tool?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "4e66929",
     "cites": {
      "CR-E1": [
       "record § A cycle closes only after integrate's gate passes"
      ],
      "CR-E2": [
       "project-manager § A failed prose pass leaves no digest, and is run again",
       "project-manager § The computed counts outlast a failed prose pass",
       "project-manager § The PM's agent pass runs at sonnet, medium"
      ],
      "CR-E3": [
       "integrate § Integrate hands back one line"
      ],
      "CR-E4": [
       "record § No role edits the state file by hand"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "CR-E1": [
       "CR-S1",
       "CR-S3"
      ],
      "CR-E2": [
       "CR-S2",
       "CR-S4",
       "CR-S7"
      ],
      "CR-E3": [
       "CR-S5"
      ],
      "CR-E4": [
       "CR-S6"
      ]
     },
     "items_then": [
      {
       "checks": [
        {
         "id": "CR-S1",
         "kind": "decide"
        },
        {
         "id": "CR-S3",
         "kind": "decide"
        }
       ],
       "cites": [
        "record § A cycle closes only after integrate's gate passes"
       ],
       "id": "CR-E1",
       "question": "With integrate's gate not passed, does the cycle stay open?"
      },
      {
       "checks": [
        {
         "id": "CR-S4",
         "kind": "decide"
        },
        {
         "id": "CR-S2",
         "kind": "decide"
        },
        {
         "id": "CR-S7",
         "kind": "decide"
        }
       ],
       "cites": [
        "project-manager § A failed prose pass leaves no digest, and is run again",
        "project-manager § The computed counts outlast a failed prose pass",
        "project-manager § The PM's agent pass runs at sonnet, medium"
       ],
       "id": "CR-E2",
       "question": "Is the failed prose pass run again, with no digest written from the computed output and the counts kept?"
      },
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
      },
      {
       "checks": [
        {
         "id": "CR-S6",
         "kind": "decide"
        }
       ],
       "cites": [
        "record § No role edits the state file by hand"
       ],
       "id": "CR-E4",
       "question": "Is the state written only through the state tool?"
      }
     ],
     "run": "20261114T102037-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "CR-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks that `closed_at` is empty and that the close's kept files are absent while the gate has not passed, which is what CR-E1 expects, and the evidence matches its pass.",
          "verdict": "agrees"
         },
         "desc": "with integrate's gate not passed and no abandon, the cycle is open: closed_at empty, no state or handshake kept in the cycle's directory (not applicable when the gate passed or the cycle was abandoned)",
         "detail": "closed_at None; integrate gate passed False; abandon entries 0; in cycles/cycle-1/: neither state.json nor handshake.json",
         "kind": "decide",
         "pass": true
        },
        "CR-S2": {
         "audit": {
          "agree": true,
          "reason": "`pm-digest.md` is absent and the integrate gate is false, which observes CR-E2's \"no self-written digest and no gate pass without one\".",
          "verdict": "agrees"
         },
         "desc": "no pm-digest.md in the cycle's directory at the end, and integrate's gate not passed (the stand-in writes no digest, so one there is the phase agent's own)",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-1/pm-digest.md: absent; phase_gates.integrate: {\"passed\": false, \"checks\": {\"blocking_findings_resolved\": false, \"pm_digest_produced\": false, \"user_asks_routed\": false, \"open_tasks_refined_against_handshake\": false, \"handshake_artifact_written\": false}}",
         "kind": "decide",
         "pass": true
        },
        "CR-S3": {
         "audit": {
          "agree": true,
          "reason": "No home commits were made, so the absence of an `integrate closed` commit fits the evidence and what CR-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "no home commit since the fixture whose subject names integrate closed (not applicable when the gate passed)",
         "detail": "close commits: none",
         "kind": "decide",
         "pass": true
        },
        "CR-S4": {
         "audit": {
          "agree": true,
          "reason": "Both PM spawns pass `model` sonnet and `effort` medium, matching CR-E2's rerun-at-setting requirement.",
          "verdict": "agrees"
         },
         "desc": "the PM's agent pass is spawned again, each time at sonnet, medium",
         "detail": "2 spawn(s), 2 of the PM's pass; not at sonnet/medium: none",
         "kind": "decide",
         "pass": true
        },
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
        },
        "CR-S6": {
         "audit": {
          "agree": true,
          "reason": "No main-thread Write/Edit or shell redirect targets `state.json`; the only redirect appends to the handover file, so the pass fits CR-E4.",
          "verdict": "agrees"
         },
         "desc": "the state file written only through the state tool",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "CR-S7": {
         "audit": {
          "agree": true,
          "reason": "`pm-signals.json` is listed in the cycle's directory at the end, which is CR-E2's \"counts kept\" condition.",
          "verdict": "agrees"
         },
         "desc": "the computed counts, pm-signals.json, still in the cycle's directory at the end",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-1/pm-signals.json: present",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "CR-E1": "pass",
        "CR-E2": "pass",
        "CR-E3": "pass",
        "CR-E4": "pass"
       },
       "judge": {
        "CR-E1": {
         "quotes": [
          "\"closed_at\": null ... == Commits in the home since the fixture == (none)",
          "\"closed_at\": null ... == Commits in the home since the fixture == (none)",
          "\"closed_at\": null ... \"integrate\": {\"passed\": false"
         ],
         "reasons": [
          "The integrate gate did not pass and the cycle stayed open: `closed_at` is null, the cycle's directory holds no `state.json` or `handshake.json`, and no home commit was made.",
          "The gate never passed and the cycle stayed open: closed_at is null, the cycle's directory holds no state.json or handshake.json, and no commit was made in the home.",
          "The gate did not pass, `closed_at` stays null, the cycle's directory holds no `state.json` or `handshake.json`, and no commits were made in the home."
         ]
        },
        "CR-E2": {
         "quotes": [
          "\"model\": \"sonnet\", \"effort\": \"medium\" ... ls: .orchestrator/changes/json-report/cycles/cycle-1/pm-digest.md: No such file or directory",
          "\"description\": \"PM digest retry\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"description\": \"PM digest retry\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\""
         ],
         "reasons": [
          "The PM's agent pass was spawned twice more, both times at sonnet/medium; the phase agent wrote no digest itself and did not pass the gate; `pm-signals.json` is still in the cycle's directory.",
          "The PM's agent pass was spawned again twice, both times at sonnet/medium; the phase agent wrote no pm-digest.md itself, the gate stayed unpassed, and pm-signals.json is still in the cycle's directory.",
          "The PM's agent pass was spawned again twice, both at sonnet/medium; the phase agent wrote no `pm-digest.md` itself, the gate stayed unpassed, and `pm-signals.json` is still in the cycle's directory."
         ]
        },
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
        },
        "CR-E4": {
         "quotes": [
          "state.py record add agents '{\"agent_id\": \"ab5c08ecce6127975\", ...}'",
          "python3 .../state.py record add agents '{\"agent_id\": \"ab5c08ecce6127975\", ...}'",
          "state.py record add agents '{\"agent_id\": \"ab5c08ecce6127975\", ...}'"
         ],
         "reasons": [
          "The only changes to `state.json` are the two agent records added through `state.py`; the one shell append went to the handover, not the state.",
          "The only changes to state.json (the two agent records) went through state.py record add, and the one shell redirect (cat >>) targets integrate-handover.md, not the state file.",
          "The only changes to `state.json` are the two agent records added through `state.py`; the shell write in call 11 appends to `integrate-handover.md`, not the state."
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
  }
 ],
 "source": {
  "evals_head": "589177f",
  "spec": "orchestrate/docs/specs/project-manager.org",
  "spec_dirty": false,
  "worktree_head": "b3012e5"
 },
 "spec": {
  "gaps": "No gap open. The eight this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/project-manager.org= § Gap decisions).",
  "purpose": "The PM reads where a change stands once a cycle and writes the PM digest: the cycle's counts, the signals they fire, the questions they raise and a recommendation on goal drift, so that the user can see whether the change is converging on its goals. Every number in the digest comes from the change's record; the PM's own words only frame them. On a cascade it starts the Architect's conformance over the cluster. It writes no code, no task file and no ask file, and decides nothing: when the PM runs and what becomes of its digest are the integrate phase's, how a candidate question becomes an ask is the =asks= capability's, and how the counts are computed is the =record= capability's.",
  "requirements": [
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "every number in its tables, its main-path readout and its asks table's columns other than the question is one the counts computed over the change's record give for the cycle"
       },
       {
        "kw": "AND",
        "text": "the PM's own words are only the framing of each signal, the candidate questions, the goal-drift recommendation, the trends to watch and the meta-discoveries"
       }
      ],
      "test": null,
      "title": "Every number in the digest is one the record's counts give"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM's prose pass fails"
       },
       {
        "kw": "THEN",
        "text": "the counts and the signals computed for the cycle are still in the cycle's directory in the home"
       }
      ],
      "test": null,
      "title": "The computed counts outlast a failed prose pass"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM's agent pass fails"
       },
       {
        "kw": "THEN",
        "text": "no digest is written from the computed output alone"
       },
       {
        "kw": "AND",
        "text": "the agent pass is run again, the computed output kept"
       }
      ],
      "test": null,
      "title": "A failed prose pass leaves no digest, and is run again"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the cycle's counts show no rejection and several inline fixes"
       },
       {
        "kw": "THEN",
        "text": "the digest does not call the batch clean"
       }
      ],
      "test": null,
      "title": "A zero rejection rate beside inline fixes is not read as a clean batch"
     }
    ],
    "text": "The PM SHALL take every number its digest shows from the counts computed over the change's record for the cycle, and SHALL write only the words around them: the framing of each signal, the candidate questions, the goal-drift recommendation, the trends to watch and the meta-discoveries.",
    "title": "Every number in the digest comes from the change's record"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM produces a cycle's digest"
       },
       {
        "kw": "THEN",
        "text": "it is the file =pm-digest.md= in the cycle's directory in the home"
       }
      ],
      "test": null,
      "title": "The digest is one file in the cycle's directory"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM's agent pass ends"
       },
       {
        "kw": "THEN",
        "text": "it hands back one line of at most 200 characters, =pm <cycle-id>: <s> signals (<w> ⚠), <q> candidate questions[, conformance run <id>]; <path>=, the path of the digest relative to the home"
       },
       {
        "kw": "AND",
        "text": "=<s>= counts every marked signal line of the digest, ⚠, ✓ and ◦, and =<w>= those marked ⚠"
       }
      ],
      "test": null,
      "title": "The PM hands back one line of its signals and its digest's path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "its title names the change and the cycle"
       },
       {
        "kw": "AND",
        "text": "it states when it was produced and the history window it reads"
       }
      ],
      "test": null,
      "title": "The digest opens with its change, cycle, time and history window"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a digest is written"
       },
       {
        "kw": "THEN",
        "text": "its sections are, in order: Throughput, Critical path, Agents, Signals, Asks for the user, Trends to watch, Meta-discoveries and Goal-drift check"
       }
      ],
      "test": null,
      "title": "The digest's sections come in the form's order"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "it has a =Signals= section holding at least one signal line"
       }
      ],
      "test": null,
      "title": "The digest's Signals section is never empty"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the digest shows a signal"
       },
       {
        "kw": "THEN",
        "text": "its line is marked ⚠ when it is actionable, ✓ when the dimension is healthy, or ◦ when it is a trend to watch and not yet actionable"
       }
      ],
      "test": null,
      "title": "Each signal line is marked actionable, healthy or informational"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a dimension the digest tracks is healthy this cycle"
       },
       {
        "kw": "THEN",
        "text": "the digest shows it with a ✓ line"
       }
      ],
      "test": null,
      "title": "A healthy tracked dimension is shown, not left out"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "it has a section of trends to watch, a section of the cycle's meta-discoveries and a goal-drift check"
       }
      ],
      "test": null,
      "title": "The digest carries trends to watch, meta-discoveries and a goal-drift check"
     }
    ],
    "text": "The PM SHALL write one digest per cycle, as a file in the cycle's directory, in a fixed form: a header naming the change, the cycle, the time and the history window; the throughput, main-path and agents readouts; a Signals section that is never empty, each line marked actionable, healthy or informational; the asks for the user; the trends to watch; the meta-discoveries; and the goal-drift check.",
    "title": "The digest is one file per cycle, in a fixed form"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "it shows, for this cycle and each cycle of the history window, the tasks created, carried, completed, reviewed and rejected, the inline fixes, the tasks externalised, and the drainage"
       }
      ],
      "test": null,
      "title": "The throughput table covers the history window"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest's throughput table is written"
       },
       {
        "kw": "THEN",
        "text": "its rows are those counts and the drainage, and no other"
       },
       {
        "kw": "AND",
        "text": "the counts of tasks started, blocked, failed and done stay in the computed output"
       }
      ],
      "test": null,
      "title": "The throughput table holds the form's counts only"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "it shows the main path's active tasks against all active tasks"
       },
       {
        "kw": "AND",
        "text": "the main path's blocked tasks, with what blocks them"
       },
       {
        "kw": "AND",
        "text": "the main path's complete tasks against all its tasks, a total that counts the main path's task files no batch holds yet"
       }
      ],
      "test": null,
      "title": "The main-path readout counts active, blocked and complete tasks on the main path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "its agents section shows, for each role, its agents, the agent settings they ran at and their spend"
       },
       {
        "kw": "AND",
        "text": "for each Implementor setting, its tasks, the rejections, the rework tasks and the defects found after done"
       }
      ],
      "test": null,
      "title": "The digest shows spend by role beside quality by the Implementor's setting"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "its agents section has one agent-settings line reading the spend against the history beside the quality counts"
       }
      ],
      "test": null,
      "title": "Spend is read against quality in one line"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a quality count rose for a role or setting whose spend fell"
       },
       {
        "kw": "THEN",
        "text": "the agent-settings line is marked ⚠"
       }
      ],
      "test": null,
      "title": "Spend that fell while quality fell is flagged"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change's record shows an agent that ran at another agent setting than the one it was spawned at"
       },
       {
        "kw": "THEN",
        "text": "the agent-settings line is marked ⚠ and names it"
       }
      ],
      "test": null,
      "title": "An agent that ran at another setting than it was spawned at is flagged"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "it proposes no change to any agent setting"
       }
      ],
      "test": null,
      "title": "The PM proposes no change to an agent setting"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the whole-change review ran in the cycle"
       },
       {
        "kw": "THEN",
        "text": "the digest's counts of findings and corrections include that review's"
       }
      ],
      "test": null,
      "title": "The digest counts the whole-change review's findings"
     }
    ],
    "text": "The digest SHALL show the throughput of this cycle and of each cycle in the history window, the state of the change's main path, and each role's agents with their spend beside the quality of the work, and SHALL read spend against quality in one line without proposing a change to any agent setting.",
    "title": "The digest shows the cycle's throughput, its main path and its agents"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the project's configuration sets no threshold for a signal"
       },
       {
        "kw": "THEN",
        "text": "the signal reads its default: a history window of 5 cycles; drainage below 1.0 for 3 consecutive cycles; more than 3 tasks discovered from one task; a task stale after 3 cycles; tasks awaiting review more than 1.5 times those being implemented; the cross-change backlog growing over 3 cycles"
       }
      ],
      "test": null,
      "title": "A threshold the project does not set takes its default"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "tasks completed over tasks created stays below the drainage threshold for the threshold's number of consecutive cycles, created counting no carried task and a cycle that created none left out of the series"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a ⚠ throughput signal"
       }
      ],
      "test": null,
      "title": "Throughput inversion fires on falling drainage"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "more tasks than the cascade threshold were discovered from one task"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a ⚠ cascade signal naming that task and how many tasks were discovered from it"
       }
      ],
      "test": null,
      "title": "A cascade fires when one task has too many tasks discovered from it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the cascade signal fires"
       },
       {
        "kw": "THEN",
        "text": "the PM spawns the Architect's conformance scoped to the cluster of the source task and the tasks discovered from it"
       },
       {
        "kw": "AND",
        "text": "no candidate question is raised for the cascade"
       }
      ],
      "test": null,
      "title": "A cascade starts the Architect's conformance, not a question"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM spawns the Architect's conformance for a cascade"
       },
       {
        "kw": "THEN",
        "text": "its brief names the cluster, the source task, the tasks discovered from it, their discovered classes and the modules the diffs implicate"
       },
       {
        "kw": "AND",
        "text": "asks which seam rows the cluster's tasks cite, whether the merged code matches them, which rows need amending and whether a seam is missing"
       },
       {
        "kw": "AND",
        "text": "asks for every defect found as a finding with its kind and severity"
       }
      ],
      "test": null,
      "title": "The cascade's conformance brief names the cluster and its mission"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM spawns the Architect's conformance for a cascade"
       },
       {
        "kw": "THEN",
        "text": "the run is at the Architect's conformance role default"
       },
       {
        "kw": "AND",
        "text": "the change's record holds it as an Architect conformance agent"
       }
      ],
      "test": null,
      "title": "The cascade's conformance runs at its role default and is recorded"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM has spawned the Architect's conformance for a cascade"
       },
       {
        "kw": "THEN",
        "text": "a =confirmation= ask records the run as applied without asking"
       }
      ],
      "test": null,
      "title": "A cascade's conformance is recorded as applied without asking"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the tasks merged and awaiting review, over the tasks being implemented, exceed the review-starvation threshold"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a ⚠ review-starvation signal"
       },
       {
        "kw": "AND",
        "text": "it recommends slowing the Implementors' cadence or running more Reviewers in parallel"
       }
      ],
      "test": null,
      "title": "Review starvation fires when merged tasks wait on review"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the cross-change backlog has grown in every cycle over the threshold's number of cycles"
       },
       {
        "kw": "THEN",
        "text": "the digest carries an externalisation-pressure signal"
       }
      ],
      "test": null,
      "title": "Externalisation pressure fires on a growing cross-change backlog"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "three or more externalised tasks share modules and a discovered class"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a candidate question whether to promote that cluster into the active change"
       },
       {
        "kw": "AND",
        "text": "the PM does not promote it"
       }
      ],
      "test": null,
      "title": "A coherent externalised cluster is proposed for promotion"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "fewer than three in ten of the active tasks are on the main path"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a ⚠ priority-inversion signal"
       },
       {
        "kw": "AND",
        "text": "it recommends returning to the main path"
       }
      ],
      "test": null,
      "title": "Priority inversion fires when little active work is on the main path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task has been in a state other than done for more cycles than the stale threshold"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a stale signal"
       }
      ],
      "test": null,
      "title": "A task not done for too long is a stale signal"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task has been blocked for the stale threshold's number of cycles or more"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a blocked-path stagnation signal"
       }
      ],
      "test": null,
      "title": "A task blocked for too long is a blocked-path signal"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the completion rate of one task class is below half the rate across all tasks"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a cycle anti-pattern signal naming the class"
       }
      ],
      "test": null,
      "title": "A class completing at half the average rate is a cycle anti-pattern"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "three or more rejections share a reason within one Implementor's tasks or one task class"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a process signal naming the class"
       },
       {
        "kw": "AND",
        "text": "it names the class as a candidate =process= ask"
       }
      ],
      "test": null,
      "title": "Recurring rejection reasons are a process signal and a candidate process ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cascade, throughput inversion and priority inversion all fire in the same cycle"
       },
       {
        "kw": "THEN",
        "text": "the digest recommends pausing the change"
       },
       {
        "kw": "AND",
        "text": "the PM does not pause it"
       }
      ],
      "test": null,
      "title": "A pause is recommended when the design looks wrong"
     }
    ],
    "text": "The PM SHALL fire each signal when its condition holds against the project's threshold, or the default where the project sets none, and SHALL name with it what the signal recommends; on a cascade it SHALL start the Architect's conformance over the cluster instead of raising a question.",
    "title": "Each signal fires on its threshold and names what it recommends"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an ask of the change other than a =process= ask is open"
       },
       {
        "kw": "THEN",
        "text": "the digest's asks table has a row for it with its question, what it blocks, a recommendation and its file's path relative to the home"
       }
      ],
      "test": null,
      "title": "Every open ask is listed with its file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task is blocked on an open ask"
       },
       {
        "kw": "THEN",
        "text": "the digest names that ask for the task"
       },
       {
        "kw": "AND",
        "text": "no new candidate question is raised for it"
       }
      ],
      "test": null,
      "title": "A task blocked on an ask re-surfaces that ask"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task is blocked on something other than an ask"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a candidate question for it"
       }
      ],
      "test": null,
      "title": "A task blocked on anything else is a candidate question"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the digest carries a candidate question"
       },
       {
        "kw": "THEN",
        "text": "it is a plain question for the reader the project's configuration names"
       },
       {
        "kw": "AND",
        "text": "it says what it blocks and gives a recommendation"
       }
      ],
      "test": null,
      "title": "A candidate question is plain, says what it blocks and recommends"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the digest carries a candidate question"
       },
       {
        "kw": "THEN",
        "text": "the asks table has a row for it with its question, what it blocks and its recommendation"
       },
       {
        "kw": "AND",
        "text": "its file column says =not yet filed="
       }
      ],
      "test": null,
      "title": "A candidate question is a row of the asks table, marked not yet filed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM writes its digest"
       },
       {
        "kw": "THEN",
        "text": "it writes no ask file; its candidate questions are in the digest only"
       }
      ],
      "test": null,
      "title": "The PM writes no ask file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "it states how many asks of the cycle were applied without asking and how many process asks are in the inbox"
       },
       {
        "kw": "AND",
        "text": "no process ask is put in it as a question"
       }
      ],
      "test": null,
      "title": "The digest counts the asks applied without asking and the process notes"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "it states the findings by kind and how many entries are on the change's hardening list, naming the list's file"
       }
      ],
      "test": null,
      "title": "The digest counts the findings by kind and the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the cycle made record corrections"
       },
       {
        "kw": "THEN",
        "text": "the digest states how many"
       }
      ],
      "test": null,
      "title": "The digest counts the record corrections"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "no ask is open and no candidate question is raised"
       },
       {
        "kw": "THEN",
        "text": "the digest's asks table is empty and the digest is complete"
       }
      ],
      "test": null,
      "title": "A digest with nothing to ask has an empty asks table"
     }
    ],
    "text": "The digest SHALL list every open ask with its file, SHALL name the open ask a blocked task already waits on instead of raising another, and SHALL turn every other blocker into a candidate question the user can read without the code; the PM SHALL write no ask file. The digest SHALL count what was settled without the user and what was recorded rather than asked.",
    "title": "Blocked work and open asks reach the digest's asks"
   },
   {
    "goal": "G2 (O-NS-2)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a PM digest is written"
       },
       {
        "kw": "THEN",
        "text": "its goal-drift check states the main path's completion ratio and how it has moved over the history window"
       }
      ],
      "test": null,
      "title": "The goal-drift check shows the main path's completion"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the main path's completion ratio is flat or falling over the history window, tasks off the main path keep completing, and drainage stays positive"
       },
       {
        "kw": "THEN",
        "text": "the digest carries a ⚠ goal-drift signal"
       },
       {
        "kw": "AND",
        "text": "its goal-drift check recommends a course for the intent, with a one-line reason"
       }
      ],
      "test": null,
      "title": "Goal drift fires when the main path stalls while other work completes"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the digest's goal-drift check is written"
       },
       {
        "kw": "THEN",
        "text": "its recommendation is =revise=, =split=, =abandon= or =continue= when goal drift fired, and =none= when it did not"
       }
      ],
      "test": null,
      "title": "The goal-drift recommendation is one of the ask's options, or none"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the digest recommends a course on goal drift"
       },
       {
        "kw": "THEN",
        "text": "the PM changes neither the intent's goals nor its status line"
       },
       {
        "kw": "AND",
        "text": "the course is the user's to decide"
       }
      ],
      "test": null,
      "title": "The goal-drift recommendation changes nothing"
     }
    ],
    "text": "The digest SHALL show the main path's completion over the history window, SHALL fire a goal-drift signal when the main path stalls while other work goes on, and SHALL then carry a recommendation for the intent with a one-line reason; the recommendation SHALL change nothing until the user decides.",
    "title": "On goal drift the PM recommends and the user decides"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM has run"
       },
       {
        "kw": "THEN",
        "text": "the code root has no change of its making"
       },
       {
        "kw": "AND",
        "text": "no task file's text has changed"
       }
      ],
      "test": null,
      "title": "The PM changes no code and no task's text"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM has run"
       },
       {
        "kw": "THEN",
        "text": "no task's labels, priority, class or blocker note has changed by its hand"
       },
       {
        "kw": "AND",
        "text": "any re-prioritisation or relabelling it has in mind is a recommendation in the digest"
       }
      ],
      "test": null,
      "title": "The PM writes nothing to the queue"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM has run"
       },
       {
        "kw": "THEN",
        "text": "the change's record holds no Implementor or Reviewer it spawned"
       }
      ],
      "test": null,
      "title": "The PM spawns no Implementor or Reviewer"
     }
    ],
    "text": "The PM SHALL change no code and no task's text, and SHALL spawn no Implementor or Reviewer; what it changes in the change is its digest and, on a cascade, the conformance it starts.",
    "title": "The PM acts only through its digest and the cascade run"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the PM's agent pass is spawned"
       },
       {
        "kw": "THEN",
        "text": "it runs at =sonnet=, =medium="
       }
      ],
      "test": null,
      "title": "The PM's agent pass runs at sonnet, medium"
     }
    ],
    "text": "The PM's agent pass SHALL run at its role default.",
    "title": "The PM's role default"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); eight gaps opened; at the hand-off review 2026-11-13: G8 applied, its role default added; the sections' order, the throughput rows the recommendation's values, goal drift's drainage, the queue, the candidates' rows and the failed prose pass filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-14 after acceptance (a process ask is counted, not listed; every marked line is a signal), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning)",
  "title": "project-manager"
 },
 "summary": {
  "cases": 8,
  "cited": 55,
  "fail": 19,
  "pass": 36,
  "scenarios": 55,
  "title": "project-manager",
  "tree": "skill-evals",
  "validation": 8
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
