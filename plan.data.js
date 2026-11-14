window.SPEC_PAGE = {
 "capability": "plan",
 "cases": [
  {
   "capability": "plan",
   "id": "plan-candidates",
   "items": [
    {
     "checks": [
      {
       "id": "PC-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Each follow-up candidate becomes a task file",
      "plan § A follow-up's task starts with the reproduction"
     ],
     "id": "PC-E1",
     "question": "Does each candidate no open task covers become a task file, named as it names it, from its words, with its provenance and the reproduction first?"
    },
    {
     "checks": [
      {
       "id": "PC-S2",
       "kind": "decide"
      },
      {
       "id": "PC-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A candidate an open task covers is absorbed into that task"
     ],
     "id": "PC-E2",
     "question": "Is the candidate json-order covers absorbed into it, with one journal line?"
    },
    {
     "checks": [
      {
       "id": "PC-S4",
       "kind": "decide"
      },
      {
       "id": "PC-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A task drafted from a meta-discovery names its label",
      "plan § Scope no goal needs is not drafted without a demonstrated reason"
     ],
     "id": "PC-E3",
     "question": "Does the meta-discovery a goal needs make a task naming its label, and the one no goal needs make none?"
    },
    {
     "checks": [
      {
       "id": "PC-S6",
       "kind": "decide"
      },
      {
       "id": "PC-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § A task outside the change is placed in the cross-change backlog",
      "plan § An externalised task carries its provenance"
     ],
     "id": "PC-E4",
     "question": "Is the work outside the change placed in the cross-change backlog, externalised, with its provenance?"
    },
    {
     "checks": [
      {
       "id": "PC-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan does not draw tasks from the cross-change backlog"
     ],
     "id": "PC-E5",
     "question": "Does plan draw nothing from the cross-change backlog?"
    },
    {
     "checks": [
      {
       "id": "PC-S12",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § No task is drafted from the hardening list before harden"
     ],
     "id": "PC-E6",
     "question": "Is nothing drafted from the hardening list before harden?"
    },
    {
     "checks": [
      {
       "id": "PC-S13",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § An untrue sentence makes no task"
     ],
     "id": "PC-E7",
     "question": "Does the untrue sentence make no task?"
    },
    {
     "checks": [
      {
       "id": "PC-S19",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A follow-up is on the main path when it was routed blocking on it",
      "plan § A task from a user's decision is on the main path when the decision was routed there",
      "plan § Hardening, meta-discovery and scope tasks are off the main path",
      "record § A task on the main path is marked on its entry"
     ],
     "id": "PC-E8",
     "question": "Is each candidate's task marked on or off the main path by its source?"
    },
    {
     "checks": [
      {
       "id": "PC-S9",
       "kind": "signal"
      },
      {
       "id": "PC-S20",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A task whose source does not settle it is off the main path, with the doubt journaled"
     ],
     "id": "PC-E9",
     "question": "Is the task whose source does not settle it off the main path, with the doubt journaled?"
    },
    {
     "checks": [
      {
       "id": "PC-S10",
       "kind": "decide"
      },
      {
       "id": "PC-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Any other task is checked against HEAD before the batch is fixed",
      "plan § The premise check runs at the Architect's role default"
     ],
     "id": "PC-E10",
     "question": "Is every task not drafted from the design checked against HEAD, by the premise check at the Architect's role default?"
    },
    {
     "checks": [
      {
       "id": "PC-S14",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § A task override names its setting and a reason the task's text bears out",
      "plan § No agent of a task runs at Haiku",
      "plan § A task's Reviewer is never set below its role default"
     ],
     "id": "PC-E11",
     "question": "Does any task override name its setting and a reason the task's text bears out, never Haiku and never a Reviewer below its role default?"
    },
    {
     "checks": [
      {
       "id": "PC-S15",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor"
     ],
     "id": "PC-E12",
     "question": "Does plan spawn no Implementor?"
    }
   ],
   "runs": {},
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's third cycle, to its first spawn (the premise check)",
   "title": "Case plan-candidates: plan drafts the handshake's candidates by their source, places outside work in the backlog and has the rest checked",
   "trials": 3,
   "validated": ""
  },
  {
   "capability": "plan",
   "id": "plan-first",
   "items": [
    {
     "checks": [
      {
       "id": "PF-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A first cycle starts with no handshake",
      "record § A change's first cycle starts with no handshake",
      "record § The state names the change, the cycle and the phase",
      "record § The state is one file of the change's directory"
     ],
     "id": "PF-E1",
     "question": "Does the first cycle start with no handshake, nothing read in its place?"
    },
    {
     "checks": [
      {
       "id": "PF-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Generated task files are replaced by tasks drafted from the design"
     ],
     "id": "PF-E2",
     "question": "Are the generated task files deleted, the batch drafted from the design, with one journal line naming them?"
    },
    {
     "checks": [
      {
       "id": "PF-S2",
       "kind": "decide"
      },
      {
       "id": "PF-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Pending scenarios are drafted one task per seam",
      "plan § A task file holds its seams verbatim, its steps, its verification and its limits",
      "plan § An open task's file is in the change's directory"
     ],
     "id": "PF-E3",
     "question": "Is each seam drafted as one task, its file in the change's directory, holding the seam row verbatim with its steps, verification and limits?"
    },
    {
     "checks": [
      {
       "id": "PF-S11",
       "kind": "signal"
      },
      {
       "id": "PF-S16",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § Every task cites its seams, its scenarios and the tests it promotes",
      "plan § Every task traces to a scenario of the change or to where it came from"
     ],
     "id": "PF-E4",
     "question": "Does every task cite its seams, the scenarios it makes pass and the pending tests it promotes, and so trace to a scenario?"
    },
    {
     "checks": [
      {
       "id": "PF-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A task drafted from the design gets no premise check",
      "plan § Plan spawns no Implementor"
     ],
     "id": "PF-E5",
     "question": "Is there no premise check, and no agent spawned?"
    },
    {
     "checks": [
      {
       "id": "PF-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A design task is on the main path when a goal names a scenario it makes pass",
      "plan § A batch holds a main-path task when one is ready",
      "record § A task on the main path is marked on its entry"
     ],
     "id": "PF-E6",
     "question": "Are both tasks on the main path, since a goal names each scenario?"
    },
    {
     "checks": [
      {
       "id": "PF-S12",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § No agent of a task runs at Haiku",
      "plan § A task's Reviewer is never set below its role default"
     ],
     "id": "PF-E7",
     "question": "Does any task override keep away from Haiku and keep the Reviewer at its role default or above?"
    },
    {
     "checks": [
      {
       "id": "PF-S7",
       "kind": "decide"
      },
      {
       "id": "PF-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan closes with one commit in the home",
      "record § Each phase's close commits the record in the home",
      "record § On a first cycle, plan's gate asks for no handshake",
      "record § A passed gate is recorded with its checks and its time"
     ],
     "id": "PF-E8",
     "question": "Does plan pass its first-cycle gate and close with one commit in the home?"
    },
    {
     "checks": [
      {
       "id": "PF-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "PF-E9",
     "question": "Is the hand-back one line naming the outcome?"
    },
    {
     "checks": [
      {
       "id": "PF-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan writes no code"
     ],
     "id": "PF-E10",
     "question": "Does plan write no code?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "PF-E1": [
       "plan § A first cycle starts with no handshake",
       "record § A change's first cycle starts with no handshake",
       "record § The state names the change, the cycle and the phase",
       "record § The state is one file of the change's directory"
      ],
      "PF-E10": [
       "plan § Plan writes no code"
      ],
      "PF-E2": [
       "plan § Generated task files are replaced by tasks drafted from the design"
      ],
      "PF-E3": [
       "plan § Pending scenarios are drafted one task per seam",
       "plan § A task file holds its seams verbatim, its steps, its verification and its limits",
       "plan § An open task's file is in the change's directory"
      ],
      "PF-E4": [
       "plan § Every task cites its seams, its scenarios and the tests it promotes",
       "plan § Every task traces to a scenario of the change or to where it came from"
      ],
      "PF-E5": [
       "plan § A task drafted from the design gets no premise check",
       "plan § Plan spawns no Implementor"
      ],
      "PF-E6": [
       "plan § A design task is on the main path when a goal names a scenario it makes pass",
       "plan § A batch holds a main-path task when one is ready",
       "record § A task on the main path is marked on its entry"
      ],
      "PF-E7": [
       "plan § No agent of a task runs at Haiku",
       "plan § A task's Reviewer is never set below its role default"
      ],
      "PF-E8": [
       "plan § Plan closes with one commit in the home",
       "record § Each phase's close commits the record in the home",
       "record § On a first cycle, plan's gate asks for no handshake",
       "record § A passed gate is recorded with its checks and its time"
      ],
      "PF-E9": [
       "plan § Plan hands back one line naming its outcome"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "PF-E1": [
       "PF-S1"
      ],
      "PF-E10": [
       "PF-S10"
      ],
      "PF-E2": [
       "PF-S3"
      ],
      "PF-E3": [
       "PF-S2",
       "PF-S6"
      ],
      "PF-E4": [
       "PF-S11",
       "PF-S16"
      ],
      "PF-E5": [
       "PF-S4"
      ],
      "PF-E6": [
       "PF-S5"
      ],
      "PF-E7": [
       "PF-S12"
      ],
      "PF-E8": [
       "PF-S7",
       "PF-S8"
      ],
      "PF-E9": [
       "PF-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T123509",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PF-S1": {
         "audit": {
          "agree": true,
          "reason": "It reads the state fields the item names, and the state shown matches its pass.",
          "verdict": "agrees"
         },
         "desc": "the first cycle's state: no handshake, the change named, a cycle id of record's form",
         "detail": "prior_handshake None; change_name json-report; cycle_id cycle-1794677731",
         "kind": "decide",
         "pass": true
        },
        "PF-S10": {
         "audit": {
          "agree": true,
          "reason": "No branch commits and an empty uncommitted section confirm no code was changed.",
          "verdict": "agrees"
         },
         "desc": "nothing in the code root's source or tests changed",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "PF-S11": {
         "audit": {
          "agree": true,
          "reason": "Each task file names its row's test file, tests/test_json.py and tests/test_cli.py respectively.",
          "verdict": "agrees"
         },
         "desc": "each task file names the test of each seam row it quotes",
         "detail": "all named",
         "kind": "signal",
         "pass": true
        },
        "PF-S12": {
         "audit": {
          "agree": true,
          "reason": "No task file mentions haiku, which fits the absence of overrides.",
          "verdict": "agrees"
         },
         "desc": "no task file names haiku",
         "detail": "none",
         "kind": "signal",
         "pass": true
        },
        "PF-S16": {
         "audit": {
          "agree": true,
          "reason": "Both task files cite a scenario as reporting § ....",
          "verdict": "agrees"
         },
         "desc": "each task file cites a scenario of the change",
         "detail": "all cite one",
         "kind": "signal",
         "pass": true
        },
        "PF-S2": {
         "audit": {
          "agree": true,
          "reason": "Checking that each seam row's cells appear in some task file observes the verbatim quoting, and both task files hold their row.",
          "verdict": "agrees"
         },
         "desc": "each seam row of the design quoted whole by a task file under tasks/open/",
         "detail": "seam/json-report: json-output; seam/options: unknown-option",
         "kind": "decide",
         "pass": true
        },
        "PF-S3": {
         "audit": {
          "agree": true,
          "reason": "Both generated files are deleted and the single added journal entry names both, as observed.",
          "verdict": "agrees"
         },
         "desc": "the generated task files deleted, and a journal entry naming both",
         "detail": "still in tasks/open/: none; entries naming both: [{\"at\": \"2026-11-14T17:35:41Z\", \"kind\": \"note\", \"by\": null, \"ref\": null, \"text\": \"deleted 2 generated task files: split-json-output, split-unknown-option\", \"path\": null}]",
         "kind": "decide",
         "pass": true
        },
        "PF-S4": {
         "audit": {
          "agree": true,
          "reason": "It observes zero spawns and no premise-check file, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned and no premise check",
         "detail": "0 spawn(s); premise-check.md in: none",
         "kind": "decide",
         "pass": true
        },
        "PF-S5": {
         "audit": {
          "agree": true,
          "reason": "Both state entries have on_critical_path true, as observed.",
          "verdict": "agrees"
         },
         "desc": "every task of the batch on the main path",
         "detail": "json-output=True unknown-option=True",
         "kind": "decide",
         "pass": true
        },
        "PF-S6": {
         "audit": {
          "agree": true,
          "reason": "Each state entry's file exists at tasks/open/<task_name>.md in the change's directory.",
          "verdict": "agrees"
         },
         "desc": "each task's file is tasks/open/<task>.md in the change's directory",
         "detail": "entries: json-output unknown-option; without a file: none",
         "kind": "decide",
         "pass": true
        },
        "PF-S7": {
         "audit": {
          "agree": true,
          "reason": "There is exactly one commit with the required subject, and it adds state.json and both task files.",
          "verdict": "agrees"
         },
         "desc": "one close commit in the home, holding the state and the batch's task files",
         "detail": "Orchestrator: cycle-1794677731 plan closed; holds state.json: True; task files not in its tree: none",
         "kind": "decide",
         "pass": true
        },
        "PF-S8": {
         "audit": {
          "agree": true,
          "reason": "The state's plan gate shows passed true, four true checks and a passed_at.",
          "verdict": "agrees"
         },
         "desc": "the plan gate passed, every check true, with its time",
         "detail": "{\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T17:36:07Z\"}",
         "kind": "decide",
         "pass": true
        },
        "PF-S9": {
         "audit": {
          "agree": true,
          "reason": "The final text is one 146-character line with no absolute path.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters, with no absolute path",
         "detail": "146 characters; absolute paths: []: plan: passed, cycle-1794677731 batch of 2 (json-output, unknown-option, both on the critical path; 2 generated task files deleted); commit 5eafa55",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "PF-E1": "pass",
        "PF-E10": "pass",
        "PF-E2": "pass",
        "PF-E3": "pass",
        "PF-E4": "pass",
        "PF-E5": "pass",
        "PF-E6": "pass",
        "PF-E7": "n/a",
        "PF-E8": "pass",
        "PF-E9": "pass"
       },
       "judge": {
        "PF-E1": {
         "quotes": [
          "\"prior_handshake\": null, ... \"change_name\": \"json-report\", ... \"cycle_id\": \"cycle-1794677731\"",
          "\"prior_handshake\": null, ... \"change_name\": \"json-report\", ... \"cycle_id\": \"cycle-1794677731\"",
          "\"prior_handshake\": null, ... \"cycle_id\": \"cycle-1794677731\", ... \"change_name\": \"json-report\""
         ],
         "reasons": [
          "The state was created by `state.py init --change json-report --first-cycle` with no handshake, and no tool call reads a handshake or anything in its place.",
          "The state was opened with `state.py init --change json-report --first-cycle`, has a null handshake and a cycle-<ts> id, and no tool call reads any handshake or stand-in for one.",
          "The state was made by `state.py init --change json-report --first-cycle` with no handshake and a cycle-<unix-ts> id, and no tool call reads anything in a handshake's place."
         ]
        },
        "PF-E10": {
         "quotes": [
          "== The change branch json-report's commits since the fixture ... ==\n(none) ... --- uncommitted ---",
          "== The change branch json-report's commits since the fixture ... ==\n(none) ... --- uncommitted ---",
          "--- uncommitted ---\n(empty); change branch commits: (none)"
         ],
         "reasons": [
          "The change branch has no commits and the code root has no uncommitted changes, so nothing under tally/ or tests/ was touched.",
          "There are no commits on the change branch and nothing uncommitted in the code root, so tally/ and tests/ are untouched.",
          "Nothing under tally/ or tests/ was committed or left changed in the code root."
         ]
        },
        "PF-E2": {
         "quotes": [
          "\"text\": \"deleted 2 generated task files: split-json-output, split-unknown-option\"",
          "\"text\": \"deleted 2 generated task files: split-json-output, split-unknown-option\"",
          "\"text\": \"deleted 2 generated task files: split-json-output, split-unknown-option\""
         ],
         "reasons": [
          "Both generated files were git-rm'd, one journal line names both, and the new tasks were drafted after reading design.org, intent.org, the spec and the code.",
          "Both generated files were removed with git rm, one journal line names both, and the new task files are drafted from the design's D1–D4, not copied from the generated stubs.",
          "Both generated files are `git rm`'d, the new tasks are written after reading design.org and the code, and one journal line names both files."
         ]
        },
        "PF-E3": {
         "quotes": [
          "| seam/options | An option =tally= does not know ends the run with the usage status ... | =tests/test_cli.py= |",
          "A\t.orchestrator/changes/json-report/tasks/open/json-output.md ... A\t.orchestrator/changes/json-report/tasks/open/unknown-option.md",
          "| seam/json-report | The JSON report holds exactly the table's categories, in the table's order, each amount as the table renders it | ..."
         ],
         "reasons": [
          "There is one task per seam at tasks/open/<task>.md in the change's directory, and each holds a summary, the seam row verbatim, numbered steps, a verification command and an out-of-scope list.",
          "There is one task per seam in the change's tasks/open/, each with a summary, the seam row quoted verbatim, numbered implementation steps, a verification command and an Out-of-scope section.",
          "There is one task per seam (json-output, unknown-option), each at tasks/open/<task>.md in the change's directory, each with a summary, its seam row verbatim, numbered steps, a verification command and an Out-of-scope section."
         ]
        },
        "PF-E4": {
         "quotes": [
          "cites_scenarios:\n  - reporting § An unknown option is refused ... Promotes the pending test `tests/test_cli.py::Options::test_unknown_option_refused`",
          "cites_scenarios:\n  - reporting § An unknown option is refused ... Promotes the pending test `tests/test_cli.py::Options::test_unknown_option_refused`",
          "Promotes the pending test `tests/test_cli.py::Options::test_unknown_option_refused`"
         ],
         "reasons": [
          "Each task file names its seam and scenario in the frontmatter, and its verification names the pending test it promotes (test_json.py and test_cli.py respectively).",
          "Each task names its seam and its scenario in front matter, and its verification names the pending test it promotes (test_json.py and test_cli.py respectively).",
          "Each task file names its seams (cites_seams) and scenarios (cites_scenarios), and its Verification names the pending test it promotes in tests/test_json.py or tests/test_cli.py."
         ]
        },
        "PF-E5": {
         "quotes": [
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "No Agent call was made, and no premise-check file appears among the home's changes.",
          "No Agent call was made and no premise-check file appears in the commit or the files listed.",
          "No Agent call was made, and no premise-check file appears among the files added."
         ]
        },
        "PF-E6": {
         "quotes": [
          "on_critical_path: true ... \"on_critical_path\": true",
          "on_critical_path: true ... \"on_critical_path\": true, ... \"task_name\": \"unknown-option\"",
          "on_critical_path: true"
         ],
         "reasons": [
          "Both task files and both state entries mark the task as on the main path.",
          "Both task files and both state entries mark the task on the critical path.",
          "Both task files and both state entries have on_critical_path true."
         ]
        },
        "PF-E7": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "Neither task file nor any state entry writes a model or effort override.",
          "Neither task file nor any task entry carries a model or effort override, so plan wrote no task override.",
          "Neither task file nor any state entry carries a model or effort override, so plan wrote no task override."
         ]
        },
        "PF-E8": {
         "quotes": [
          "5eafa55 Orchestrator: cycle-1794677731 plan closed ... A .orchestrator/changes/json-report/state.json ... A .../tasks/open/json-output.md ... A .../tasks/open/unknown-option.md",
          "5eafa55 Orchestrator: cycle-1794677731 plan closed ... \"plan\": {\"passed\": true, ... \"passed_at\": \"2026-11-14T17:36:07Z\"}",
          "5eafa55 Orchestrator: cycle-1794677731 plan closed"
         ],
         "reasons": [
          "The plan gate is recorded as passed with all four checks and a passed_at, and one home commit with the required subject holds the state and both task files.",
          "The plan gate passed with no handshake, and the single home commit with the required subject holds state.json and both task files.",
          "The plan gate passed with all four checks true and a passed_at time, and one home commit with that subject holds state.json and both task files."
         ]
        },
        "PF-E9": {
         "quotes": [
          "plan: passed, cycle-1794677731 batch of 2 (json-output, unknown-option, both on the critical path; 2 generated task files deleted); commit 5eafa55",
          "plan: passed, cycle-1794677731 batch of 2 (json-output, unknown-option, both on the critical path; 2 generated task files deleted); commit 5eafa55",
          "plan: passed, cycle-1794677731 batch of 2 (json-output, unknown-option, both on the critical path; 2 generated task files deleted); commit 5eafa55"
         ],
         "reasons": [
          "The hand-back is one line under 200 characters, says the gate passed and contains no path.",
          "The hand-back is one 146-character line stating the gate passed, with no path.",
          "The hand-back is one line of 146 characters with no path, saying the gate passed."
         ]
        }
       },
       "milestones": [],
       "n_calls": 15,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's first cycle, to its hand-back",
   "title": "Case plan-first: the change's first plan drafts one task per seam from the design and passes with no premise check",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T123509 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-goal-drift-exact",
   "items": [
    {
     "checks": [
      {
       "id": "GE-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An exact option signs off the goal drift",
      "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
      "record § Plan's gate checks the handshake, the batch, its citations and the goal-drift answer",
      "record § The goal-drift check holds only on one of its four answers"
     ],
     "id": "GE-E1",
     "question": "Does the exact answer sign off the goal drift, so that plan passes its gate with every check?"
    },
    {
     "checks": [
      {
       "id": "GE-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
      "record § A later cycle's state names the handshake it started from"
     ],
     "id": "GE-E2",
     "question": "Is the cycle started from the prior handshake?"
    },
    {
     "checks": [
      {
       "id": "GE-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off"
     ],
     "id": "GE-E3",
     "question": "Does every task of the batch cite a seam?"
    },
    {
     "checks": [
      {
       "id": "GE-S3",
       "kind": "decide"
      },
      {
       "id": "GE-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan closes with one commit in the home",
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "GE-E5",
     "question": "Does plan close with one commit in the home and hand back one line?"
    },
    {
     "checks": [
      {
       "id": "GE-S7",
       "kind": "decide"
      },
      {
       "id": "GE-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan writes no code",
      "plan § Plan spawns no Implementor"
     ],
     "id": "GE-E6",
     "question": "Does plan write no code and spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "GE-E1": [
       "plan § An exact option signs off the goal drift",
       "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
       "record § Plan's gate checks the handshake, the batch, its citations and the goal-drift answer",
       "record § The goal-drift check holds only on one of its four answers"
      ],
      "GE-E2": [
       "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
       "record § A later cycle's state names the handshake it started from"
      ],
      "GE-E3": [
       "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off"
      ],
      "GE-E5": [
       "plan § Plan closes with one commit in the home",
       "plan § Plan hands back one line naming its outcome"
      ],
      "GE-E6": [
       "plan § Plan writes no code",
       "plan § Plan spawns no Implementor"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "GE-E1": [
       "GE-S1"
      ],
      "GE-E2": [
       "GE-S2"
      ],
      "GE-E3": [
       "GE-S4"
      ],
      "GE-E5": [
       "GE-S3",
       "GE-S6"
      ],
      "GE-E6": [
       "GE-S7",
       "GE-S8"
      ]
     },
     "items_then": [],
     "run": "20261114T114842",
     "trials": [
      {
       "calls": {},
       "checks": {
        "GE-S1": {
         "audit": {
          "agree": true,
          "reason": "It reads the new cycle's plan gate and its four checks plus passed_at, which is exactly what GE-E1 expects, and the state shown matches.",
          "verdict": "agrees"
         },
         "desc": "the new cycle's plan gate passed, every check true, with its time",
         "detail": "new cycle: cycle-1794674952; {\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T16:49:41Z\"}",
         "kind": "decide",
         "pass": true
        },
        "GE-S2": {
         "audit": {
          "agree": true,
          "reason": "It compares the new state's prior_handshake with the fixture's newest handshake (handshake-cycle-2.json), which is what GE-E2 asks.",
          "verdict": "agrees"
         },
         "desc": "a new cycle started from the newest handshake",
         "detail": "new cycle: cycle-1794674952; prior_handshake .orchestrator/changes/json-report/handshake-cycle-2.json; newest in the fixture: handshake-cycle-2.json",
         "kind": "decide",
         "pass": true
        },
        "GE-S3": {
         "audit": {
          "agree": true,
          "reason": "It finds the single close commit, confirms it changes state.json and that its tree holds the task file, which fits the evidence and GE-E5's commit half.",
          "verdict": "agrees"
         },
         "desc": "one close commit in the home, holding the state and the batch's task files",
         "detail": "Orchestrator: cycle-1794674952 plan closed; holds state.json: True; task files not in its tree: none",
         "kind": "decide",
         "pass": true
        },
        "GE-S4": {
         "audit": {
          "agree": true,
          "reason": "It checks that each non-externalised task file names a design seam; refuse-unknown-option names seam/options, which fits GE-E3.",
          "verdict": "agrees"
         },
         "desc": "every task of the batch not externalised cites a seam of the design",
         "detail": "tasks: refuse-unknown-option; citing none of ['seam/json-report', 'seam/options']: none",
         "kind": "decide",
         "pass": true
        },
        "GE-S6": {
         "audit": {
          "agree": true,
          "reason": "It measures the final text as one line under 200 characters with no absolute path, matching the hand-back half of GE-E5.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters, with no absolute path",
         "detail": "156 characters; absolute paths: []: plan: passed, cycle-1794674952: batch of 1 (refuse-unknown-option, critical, cites seam/options); ask-cycle-2-1 applied (continue); no premise check needed.",
         "kind": "decide",
         "pass": true
        },
        "GE-S7": {
         "audit": {
          "agree": true,
          "reason": "No change-branch commits, nothing uncommitted, and home commits touch only .orchestrator/, so its pass fits GE-E6's no-code half.",
          "verdict": "agrees"
         },
         "desc": "nothing in the code root's source or tests changed",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "GE-S8": {
         "audit": {
          "agree": true,
          "reason": "No Agent call was made at all, so no Implementor was spawned, which fits GE-E6's spawn half.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "GE-E1": "pass",
        "GE-E2": "pass",
        "GE-E3": "pass",
        "GE-E5": "pass",
        "GE-E6": "pass"
       },
       "judge": {
        "GE-E1": {
         "quotes": [
          "\"plan\": {\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T16:49:41Z\"}",
          "\"plan\": {\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T16:49:41Z\"}",
          "\"plan\": {\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T16:49:41Z\"}"
         ],
         "reasons": [
          "The new cycle's plan gate passed with all four checks true, including the goal-drift sign-off, after `gate check` and `gate pass` through the tool.",
          "The new cycle's plan gate passed with all four checks true, including the goal-drift sign-off after the exact answer `continue`.",
          "The new cycle's plan gate passed with all four checks true, including the goal-drift sign-off, after the answer `continue` was applied."
         ]
        },
        "GE-E2": {
         "quotes": [
          "\"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\"",
          "\"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\"",
          "\"cycle_id\": \"cycle-1794674952\", ... \"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\""
         ],
         "reasons": [
          "The new cycle-1794674952 state names cycle 2's handshake.",
          "The new cycle-1794674952 state names cycle 2's handshake as its prior handshake.",
          "The new cycle's state names cycle 2's handshake as the one it started from."
         ]
        },
        "GE-E3": {
         "quotes": [
          "\"task_name\": \"refuse-unknown-option\" ... \"cites_seams\": [\"seam/options\"]",
          "cites_seams:\n  - seam/options",
          "\"cites_seams\": [\"seam/options\"] ... cites_seams:\n  - seam/options"
         ],
         "reasons": [
          "The batch's only task cites seam/options in both its state entry and its task file.",
          "The batch's only task, refuse-unknown-option, cites seam/options in both its task file and its state entry.",
          "The batch's only task, refuse-unknown-option, cites seam/options in both its state entry and its task file."
         ]
        },
        "GE-E5": {
         "quotes": [
          "d28a864 Orchestrator: cycle-1794674952 plan closed ... M .orchestrator/changes/json-report/state.json; hand-back: \"plan: passed, cycle-1794674952: batch of 1 (refuse-unknown-option, critical, cites seam/options); ask-cycle-2-1 applied (continue); no premise check needed.\"",
          "d28a864 Orchestrator: cycle-1794674952 plan closed ... M .orchestrator/changes/json-report/state.json",
          "d28a864 Orchestrator: cycle-1794674952 plan closed ... M .orchestrator/changes/json-report/state.json; hand-back: \"plan: passed, cycle-1794674952: batch of 1 (refuse-unknown-option, critical, cites seam/options); ask-cycle-2-1 applied (continue); no premise check needed.\""
         ],
         "reasons": [
          "One commit carries the close subject and its tree holds the state and the task file; the task file itself was added in the earlier 'Apply ask-cycle-2-1' commit. The hand-back is one 156-character line with no paths.",
          "One commit carries the close subject with the state's cycle id, changes the state, and its tree holds the task file committed just before it; the hand-back is one 156-character line with no path, though plan also made a separate earlier 'Apply ask-cycle-2-1' commit.",
          "One commit carries the close subject, changes the state, and its tree holds the task file (committed earlier in 607a050), and the hand-back is one line of 156 characters with no paths."
         ]
        },
        "GE-E6": {
         "quotes": [
          "The change branch json-report's commits since the fixture: (none); --- uncommitted --- (empty); Agents spawned: (none)",
          "== Agents spawned (in order), each input in full ==\n(none)",
          "The change branch json-report's commits since the fixture: (none) ... Agents spawned: (none)"
         ],
         "reasons": [
          "No source or test file in the code root changed, and no agent was spawned.",
          "No commits on the change branch, nothing uncommitted in the code root, and no Agent call.",
          "Nothing in the code root changed and no agent was spawned."
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
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle, the prior integrate's goal-drift ask answered continue, to its hand-back",
   "title": "Case plan-goal-drift-exact: an exact option signs off the goal drift, and plan passes its gate and closes",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114842 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-goal-drift-free",
   "items": [
    {
     "checks": [
      {
       "id": "GF-S1",
       "kind": "decide"
      },
      {
       "id": "GF-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A free-text goal-drift answer is read back before it signs off",
      "asks § Any other answer is read back before it is applied"
     ],
     "id": "GF-E1",
     "question": "Is the free-text answer read back first, in the ask file, as a consequence?"
    },
    {
     "checks": [
      {
       "id": "GF-S3",
       "kind": "decide"
      },
      {
       "id": "GF-S4",
       "kind": "decide"
      },
      {
       "id": "GF-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A free-text goal-drift answer is read back before it signs off",
      "asks § Any other answer is read back before it is applied",
      "asks § An unsettled ask is in the change's inbox",
      "record § The goal-drift check holds only on one of its four answers"
     ],
     "id": "GF-E3",
     "question": "Does the ask stay open, the drift not signed off, so that plan does not pass its gate?"
    },
    {
     "checks": [
      {
       "id": "GF-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "GF-E4",
     "question": "Is the hand-back one line that it stopped, naming the ask?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "GF-E1": [
       "plan § A free-text goal-drift answer is read back before it signs off",
       "asks § Any other answer is read back before it is applied"
      ],
      "GF-E3": [
       "plan § A free-text goal-drift answer is read back before it signs off",
       "asks § Any other answer is read back before it is applied",
       "asks § An unsettled ask is in the change's inbox",
       "record § The goal-drift check holds only on one of its four answers"
      ],
      "GF-E4": [
       "plan § Plan hands back one line naming its outcome"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "GF-E1": [
       "GF-S1",
       "GF-S6"
      ],
      "GF-E3": [
       "GF-S3",
       "GF-S4",
       "GF-S7"
      ],
      "GF-E4": [
       "GF-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103542-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "GF-S1": {
         "audit": {
          "agree": true,
          "reason": "It found the non-empty 214-character readback in the goal-drift ask's file, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the free-text answer read back in the ask file, in at most 600 characters",
         "detail": "ask ask-cycle-2-1 at .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org; * Readback (214 characters): \"Continue: the goals stand and the JSON report stays as it is, amounts still strings. The next cycle's batch is the work on tally refusing an option it does not know, ahead of anything else.  … [cut, 325 characters]",
         "kind": "decide",
         "pass": true
        },
        "GF-S3": {
         "audit": {
          "agree": true,
          "reason": "The ask file is in the inbox with `:STATUS: open`, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the ask still unsettled, in the inbox",
         "detail": "at .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org; STATUS=open",
         "kind": "decide",
         "pass": true
        },
        "GF-S4": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no new cycle in the state and no plan-closed commit; the only commit is the readback.",
          "verdict": "agrees"
         },
         "desc": "the plan gate not passed, and no close commit",
         "detail": "new cycle: None; plan gate: n/a; close commits: none",
         "kind": "decide",
         "pass": true
        },
        "GF-S5": {
         "audit": {
          "agree": true,
          "reason": "The final text is one 167-character line containing the ask id and only a relative path.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters naming the ask, with no absolute path",
         "detail": "167 characters; absolute paths: []: plan: stopped, ask ask-cycle-2-1 blocks plan, waiting for the user to confirm the readback (no cycle opened); .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
         "kind": "decide",
         "pass": true
        },
        "GF-S6": {
         "audit": {
          "agree": true,
          "reason": "The Answer section still starts with the fixture's text and differs only by trailing whitespace, so the answer was left as written.",
          "verdict": "agrees"
         },
         "desc": "the user's answer kept as the user wrote it",
         "detail": "Answer before '\\n# An option label from above, or your own words; everything below this line is read as your answer.\\nKeep the JSON as it is but make the option check the next thing you do.\\n'; now '\\n# An option label from above, or your own words; everything below this line is read as your answer. … [cut, 378 characters]",
         "kind": "decide",
         "pass": true
        },
        "GF-S7": {
         "audit": {
          "agree": true,
          "reason": "No new cycle was named, so not applicable fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "a new cycle's goal-drift check does not hold on a free-text answer (not applicable without a new cycle)",
         "detail": "new cycle None; user_signed_off_goal_drift None",
         "kind": "decide",
         "pass": null
        }
       },
       "items": {
        "GF-E1": "pass",
        "GF-E3": "pass",
        "GF-E4": "pass"
       },
       "judge": {
        "GF-E1": {
         "quotes": [
          "* Readback\n\nContinue: the goals stand and the JSON report stays as it is, amounts still strings. The next cycle's batch is the work on tally refusing an option it does not know, ahead of anything else.",
          "* Readback\n\nContinue: the goals stand and the JSON report stays as it is, amounts still strings. The next cycle's batch is the work on tally refusing an option it does not know, ahead of anything else. Say yes here or in chat.",
          "* Readback\n\nContinue: the goals stand and the JSON report stays as it is, amounts still strings. The next cycle's batch is the work on tally refusing an option it does not know, ahead of anything else. Say yes here or in chat."
         ],
         "reasons": [
          "The ask file gained a 214-character `* Readback` that restates the free-text answer as its consequence (continue, amounts stay strings, the option check comes next), and the `* Answer` text is unchanged.",
          "The ask file gains a 214-character `* Readback` that maps the free-text answer to `continue` and states what follows from it; the `* Answer` text is unchanged apart from a trailing blank line.",
          "The ask file gained a 214-character Readback that restates the free-text answer as its consequence (the continue option, with G2's work first), and the Answer line is unchanged."
         ]
        },
        "GF-E3": {
         "quotes": [
          ":STATUS: open",
          ":STATUS: open",
          ":STATUS: open"
         ],
         "reasons": [
          "The ask stays open in the inbox, no new cycle or plan gate appears in the state (state.json is not among the changed files), and there is no plan-closed commit.",
          "The ask stays open in the inbox; state.json is unchanged (still cycle-2, closed); no new cycle or plan gate was written; no plan-closed commit exists.",
          "The ask stays open in the inbox, state.json is still cycle-2's with no new cycle or plan gate, and there is no plan-closed commit."
         ]
        },
        "GF-E4": {
         "quotes": [
          "plan: stopped, ask ask-cycle-2-1 blocks plan, waiting for the user to confirm the readback (no cycle opened); .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "plan: stopped, ask ask-cycle-2-1 blocks plan, waiting for the user to confirm the readback (no cycle opened); .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "plan: stopped, ask ask-cycle-2-1 blocks plan, waiting for the user to confirm the readback (no cycle opened); .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org"
         ],
         "reasons": [
          "The hand-back is one 167-character line saying plan stopped on the ask, naming ask-cycle-2-1, with a path relative to the home.",
          "One line of 167 characters says plan stopped on the ask, names it, and gives its path relative to the home.",
          "The hand-back is one 167-character line saying plan stopped on the ask, naming it, with a path relative to the home."
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
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle, the prior integrate's goal-drift ask answered in free text, to its hand-back",
   "title": "Case plan-goal-drift-free: a free-text goal-drift answer is read back, and the plan waits for the user's yes",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103542-1 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-goal-drift-open",
   "items": [
    {
     "checks": [
      {
       "id": "GO-S1",
       "kind": "decide"
      },
      {
       "id": "GO-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An unresolved goal-drift recommendation holds the plan",
      "record § The goal-drift check holds only on one of its four answers"
     ],
     "id": "GO-E1",
     "question": "Does the unanswered goal-drift recommendation hold the plan?"
    },
    {
     "checks": [
      {
       "id": "GO-S2",
       "kind": "decide"
      },
      {
       "id": "GO-S4",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § An unresolved goal-drift recommendation holds the plan",
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "GO-E2",
     "question": "Is the hand-back one line that it stopped, naming the goal-drift ask?"
    },
    {
     "checks": [
      {
       "id": "GO-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor"
     ],
     "id": "GO-E3",
     "question": "Does plan spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "GO-E1": [
       "plan § An unresolved goal-drift recommendation holds the plan",
       "record § The goal-drift check holds only on one of its four answers"
      ],
      "GO-E2": [
       "plan § An unresolved goal-drift recommendation holds the plan",
       "plan § Plan hands back one line naming its outcome"
      ],
      "GO-E3": [
       "plan § Plan spawns no Implementor"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "GO-E1": [
       "GO-S1",
       "GO-S3"
      ],
      "GO-E2": [
       "GO-S2",
       "GO-S4"
      ],
      "GO-E3": [
       "GO-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T123515",
     "trials": [
      {
       "calls": {},
       "checks": {
        "GO-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks for a passed plan gate in a new cycle and for close commits; the evidence shows neither.",
          "verdict": "agrees"
         },
         "desc": "the plan gate not passed, and no close commit",
         "detail": "new cycle: None; plan gate: n/a; close commits: none",
         "kind": "decide",
         "pass": true
        },
        "GO-S2": {
         "audit": {
          "agree": true,
          "reason": "It checks for one line of at most 200 characters, the ask id and no absolute path; the final text meets all three.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters naming the goal-drift ask, with no absolute path",
         "detail": "103 characters; absolute paths: []: plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
         "kind": "decide",
         "pass": true
        },
        "GO-S3": {
         "audit": {
          "agree": true,
          "reason": "It compares the ask's Answer section with the fixture, which is what the item needs; the file in the evidence still has only the template comment.",
          "verdict": "agrees"
         },
         "desc": "the goal-drift ask's answer left as the user left it (none)",
         "detail": "ask ask-cycle-2-1 at .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org; Answer before '\\n# An option label from above, or your own words; everything below this line is read as your answer.\\n'; now '\\n# An option label from above, or your own words; everything below this line is read as your  … [cut, 310 characters]",
         "kind": "decide",
         "pass": true
        },
        "GO-S4": {
         "audit": {
          "agree": true,
          "reason": "It checks for the 'stopped' part of the item, and the final text contains that word.",
          "verdict": "agrees"
         },
         "desc": "the hand-back says it stopped",
         "detail": "plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
         "kind": "signal",
         "pass": true
        },
        "GO-S5": {
         "audit": {
          "agree": true,
          "reason": "It looks for Agent calls that carry a task Summary, which is how an Implementor spawn would show; the evidence shows no Agent calls at all.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "GO-E1": "pass",
        "GO-E2": "pass",
        "GO-E3": "pass"
       },
       "judge": {
        "GO-E1": {
         "quotes": [
          "The home's commits since the fixture (subject, body, files) ==\n(none)",
          "== The home's commits since the fixture (subject, body, files) ==\n(none)",
          "== The home's commits since the fixture (subject, body, files) ==\n(none)"
         ],
         "reasons": [
          "Plan started no new cycle, passed no gate and made no close commit, and the ask's Answer section is unchanged, so the decision is left to the user.",
          "No new cycle was started and no gate was passed or plan closed, since the state is still cycle-2 at integrate; the ask's Answer section is unchanged, so it is left for the user.",
          "No new cycle state or plan gate was written, nothing was committed, and the ask's `* Answer` section is unchanged from the fixture."
         ]
        },
        "GO-E2": {
         "quotes": [
          "plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org"
         ],
         "reasons": [
          "The hand-back is one 103-character line that says plan stopped, names the goal-drift ask, and gives its path relative to the home.",
          "The hand-back is a single line of 103 characters that says plan stopped, names ask-cycle-2-1, and gives its path relative to the home.",
          "The hand-back is a single line of 103 characters that says plan stopped, names ask-cycle-2-1 and gives its path relative to the home."
         ]
        },
        "GO-E3": {
         "quotes": [
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "No Agent call was made, so no Implementor was spawned.",
          "No agent of any kind was spawned, so no Implementor was.",
          "No agent of any kind was spawned, so no Implementor was."
         ]
        }
       },
       "milestones": [],
       "n_calls": 4,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle, the prior integrate's goal-drift ask unanswered, to its hand-back",
   "title": "Case plan-goal-drift-open: an unanswered goal-drift recommendation holds the plan",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T123515 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-harden-batch",
   "items": [
    {
     "checks": [
      {
       "id": "HB-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Open tasks off the main path that a harden answer covers join the batch"
     ],
     "id": "HB-E1",
     "question": "Does the open task off the main path that the harden answer covers join the batch?"
    },
    {
     "checks": [
      {
       "id": "HB-S1",
       "kind": "decide"
      },
      {
       "id": "HB-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § After harden, a batch needs no main-path task",
      "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
      "record § A passed gate is recorded with its checks and its time",
      "record § A task on the main path is marked on its entry"
     ],
     "id": "HB-E2",
     "question": "Does the batch pass the gate with no task on the main path?"
    },
    {
     "checks": [
      {
       "id": "HB-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan closes with one commit in the home"
     ],
     "id": "HB-E3",
     "question": "Does plan close with one commit in the home holding the batch and the state?"
    },
    {
     "checks": [
      {
       "id": "HB-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "HB-E4",
     "question": "Is the hand-back one line naming the outcome?"
    },
    {
     "checks": [
      {
       "id": "HB-S6",
       "kind": "decide"
      },
      {
       "id": "HB-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan writes no code",
      "plan § Plan spawns no Implementor"
     ],
     "id": "HB-E5",
     "question": "Does plan write no code and spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "HB-E1": [
       "plan § Open tasks off the main path that a harden answer covers join the batch"
      ],
      "HB-E2": [
       "plan § After harden, a batch needs no main-path task",
       "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
       "record § A passed gate is recorded with its checks and its time",
       "record § A task on the main path is marked on its entry"
      ],
      "HB-E3": [
       "plan § Plan closes with one commit in the home"
      ],
      "HB-E4": [
       "plan § Plan hands back one line naming its outcome"
      ],
      "HB-E5": [
       "plan § Plan writes no code",
       "plan § Plan spawns no Implementor"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "HB-E1": [
       "HB-S2"
      ],
      "HB-E2": [
       "HB-S1",
       "HB-S3"
      ],
      "HB-E3": [
       "HB-S4"
      ],
      "HB-E4": [
       "HB-S5"
      ],
      "HB-E5": [
       "HB-S6",
       "HB-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T123506",
     "trials": [
      {
       "calls": {},
       "checks": {
        "HB-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks the passed gate, all four checks and a new passed_at, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the plan gate passed in the trial, every check true, with its time",
         "detail": "{\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T17:36:21Z\"}; the fixture's passed_at None",
         "kind": "decide",
         "pass": true
        },
        "HB-S2": {
         "audit": {
          "agree": true,
          "reason": "It confirms table-width has a batch entry with status ready, which is what HB-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "the covered open task in the batch, not blocked",
         "detail": "status ready",
         "kind": "decide",
         "pass": true
        },
        "HB-S3": {
         "audit": {
          "agree": true,
          "reason": "It confirms task entries exist and none is on the main path, the half of HB-E2 that S1 does not cover.",
          "verdict": "agrees"
         },
         "desc": "no task of the batch on the main path",
         "detail": "2 entries; on the main path: none",
         "kind": "decide",
         "pass": true
        },
        "HB-S4": {
         "audit": {
          "agree": false,
          "reason": "It confirms the one commit, its subject, state.json and the task files; it does not test for premise-check.md, but that file was already tracked and unchanged here, so the verdict fits.",
          "verdict": "agrees"
         },
         "desc": "one close commit in the home, holding the state and the batch's task files",
         "detail": "Orchestrator: cycle-3 plan closed; holds state.json: True; task files not in its tree: none",
         "kind": "decide",
         "pass": true
        },
        "HB-S5": {
         "audit": {
          "agree": false,
          "reason": "It checks one line, the length and that no path is absolute; the text does say the gate passed, so the verdict fits.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters, with no absolute path",
         "detail": "145 characters; absolute paths: []: plan: passed, cycle-3 batch of 2 (harden-refund, then table-width), both citing seam/json-report; closed in commit 0c8cfbb, phase is now execute.",
         "kind": "decide",
         "pass": true
        },
        "HB-S6": {
         "audit": {
          "agree": true,
          "reason": "No change-branch commits and nothing uncommitted in the code root, so no code was written.",
          "verdict": "agrees"
         },
         "desc": "nothing in the code root's source or tests changed",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "HB-S7": {
         "audit": {
          "agree": true,
          "reason": "No Agent call was made, so no Implementor was spawned.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "HB-E1": "pass",
        "HB-E2": "pass",
        "HB-E3": "pass",
        "HB-E4": "pass",
        "HB-E5": "pass"
       },
       "judge": {
        "HB-E1": {
         "quotes": [
          "\"task_name\": \"table-width\" ... \"status\": \"ready\"",
          "\"task_name\": \"table-width\" ... \"status\": \"ready\"",
          "\"task_name\": \"table-width\" ... \"status\": \"ready\" ... \"blocked_by\": [\"harden-refund\"]"
         ],
         "reasons": [
          "table-width is registered in cycle-3's batch with status ready; its blocked_by harden-refund only orders it after harden-refund within the batch.",
          "table-width is registered in cycle-3's state with status ready; its blocked_by harden-refund only orders it after a task in the same batch.",
          "table-width is registered in cycle-3's state with status ready; its blocked_by only orders it after its batch-mate harden-refund (\"harden-refund, then table-width\"), and its status is not blocked."
         ]
        },
        "HB-E2": {
         "quotes": [
          "\"plan\": {\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T17:36:21Z\"}",
          "\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}",
          "\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T17:36:21Z\""
         ],
         "reasons": [
          "The plan gate passed with every check true, and both batch tasks have on_critical_path false.",
          "The plan gate passed with every check true, and both batch tasks have on_critical_path false.",
          "The plan gate passed with every check true, and both batch tasks have \"on_critical_path\": false."
         ]
        },
        "HB-E3": {
         "quotes": [
          "0c8cfbb Orchestrator: cycle-3 plan closed ... M .orchestrator/changes/json-report/state.json ... tasks/open/harden-refund.md ... tasks/open/table-width.md",
          "0c8cfbb Orchestrator: cycle-3 plan closed ... M .orchestrator/changes/json-report/state.json ... tasks/open/harden-refund.md ... tasks/open/table-width.md",
          "0c8cfbb Orchestrator: cycle-3 plan closed ... M .orchestrator/changes/json-report/state.json / M ...harden-refund.md / M ...table-width.md"
         ],
         "reasons": [
          "One home commit with the required subject changes the state and both task files; the commit lists premise-check.md as neither added nor changed after `git add .orchestrator`, so it was already tracked and is in the tree.",
          "One home commit with the required subject holds the state and both task files; premise-check.md was already tracked from the fixture (`git add .orchestrator` did not add it, and it is not listed as changed), so the commit's tree holds it.",
          "One home commit with the required subject changes the state and both task files; premise-check.md, present in the fixture, is not among its changes despite `git add .orchestrator`, so it was already in the tree (unless it is gitignored, which the evidence does not show)."
         ]
        },
        "HB-E4": {
         "quotes": [
          "plan: passed, cycle-3 batch of 2 (harden-refund, then table-width), both citing seam/json-report; closed in commit 0c8cfbb, phase is now execute.",
          "plan: passed, cycle-3 batch of 2 (harden-refund, then table-width), both citing seam/json-report; closed in commit 0c8cfbb, phase is now execute.",
          "plan: passed, cycle-3 batch of 2 (harden-refund, then table-width), both citing seam/json-report; closed in commit 0c8cfbb, phase is now execute."
         ],
         "reasons": [
          "The hand-back is one line of 145 characters, says the gate passed, and has no absolute path.",
          "The hand-back is one line under 200 characters, says the gate passed, and has no absolute path.",
          "The hand-back is one line of 145 characters, says the gate passed, and contains no absolute path."
         ]
        },
        "HB-E5": {
         "quotes": [
          "The change branch json-report's commits since the fixture ... (none); Agents spawned ... (none)",
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== The change branch json-report's commits since the fixture ... == (none); --- uncommitted --- (empty); Agents spawned: (none)"
         ],
         "reasons": [
          "Nothing changed in the code root, and no agent was spawned.",
          "The change branch has no commits, nothing is uncommitted in the code root, and no agent was spawned.",
          "Nothing changed in the code root and no agent was spawned."
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
   "step": "the plan phase agent, fresh, from the dispatcher's fixed brief and a handover written after a harden cycle's clean premise check, to its hand-back",
   "title": "Case plan-harden-batch: after harden, plan batches the covered task off the main path and passes with no main-path task",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T123506 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-mvp-harden",
   "items": [
    {
     "checks": [
      {
       "id": "MH-S6",
       "kind": "decide"
      },
      {
       "id": "MH-S1",
       "kind": "decide"
      },
      {
       "id": "MH-S8",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § An MVP answer is applied before a cycle is opened",
      "plan § A harden answer starts the cycle and drafts from the hardening list",
      "asks § Any other answer is read back before it is applied",
      "asks § An ask whose answer is written is answered"
     ],
     "id": "MH-E1",
     "question": "Is the harden answer applied before a cycle is opened, and does it start the cycle?"
    },
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
    },
    {
     "checks": [
      {
       "id": "MH-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A harden answer leaves the intent at its MVP status"
     ],
     "id": "MH-E4",
     "question": "Is the intent left at its MVP status?"
    },
    {
     "checks": [
      {
       "id": "MH-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Any other task is checked against HEAD before the batch is fixed",
      "plan § The premise check runs at the Architect's role default"
     ],
     "id": "MH-E5",
     "question": "Is the hardening task checked against HEAD by the premise check at the Architect's role default?"
    },
    {
     "checks": [
      {
       "id": "MH-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor"
     ],
     "id": "MH-E6",
     "question": "Does plan spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "04cecfc",
     "cites": {
      "MH-E1": [
       "plan § An MVP answer is applied before a cycle is opened",
       "plan § A harden answer starts the cycle and drafts from the hardening list",
       "asks § Any other answer is read back before it is applied",
       "asks § An ask whose answer is written is answered"
      ],
      "MH-E2": [
       "plan § A harden answer starts the cycle and drafts from the hardening list",
       "plan § A hardening entry names the task drafted from it",
       "integrate § An entry a harden answer takes names its task, and one it leaves says so"
      ],
      "MH-E3": [
       "plan § Entries a partial harden answer leaves are marked left",
       "integrate § An entry a harden answer takes names its task, and one it leaves says so"
      ],
      "MH-E4": [
       "plan § A harden answer leaves the intent at its MVP status"
      ],
      "MH-E5": [
       "plan § Any other task is checked against HEAD before the batch is fixed",
       "plan § The premise check runs at the Architect's role default"
      ],
      "MH-E6": [
       "plan § Plan spawns no Implementor"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "MH-E1": [
       "MH-S1",
       "MH-S6",
       "MH-S8"
      ],
      "MH-E2": [
       "MH-S2"
      ],
      "MH-E3": [
       "MH-S3"
      ],
      "MH-E4": [
       "MH-S4"
      ],
      "MH-E5": [
       "MH-S5"
      ],
      "MH-E6": [
       "MH-S7"
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
        "MH-S1": {
         "audit": {
          "agree": true,
          "reason": "It observes that a new cycle was opened (cycle-1794677351), which is the cycle-start half of MH-E1, and the evidence matches.",
          "verdict": "agrees"
         },
         "desc": "a new cycle's state",
         "detail": "cycle cycle-2 -> cycle-1794677351",
         "kind": "decide",
         "pass": true
        },
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
        },
        "MH-S4": {
         "audit": {
          "agree": true,
          "reason": "No change under docs/ was made, consistent with the mvp value still being present at the tip and in the worktree.",
          "verdict": "agrees"
         },
         "desc": "the intent still at its MVP status",
         "detail": "the fixture's value: mvp 2026-11-03 164969e0fce4 ask-cycle-2-1; at the tip: 'mvp 2026-11-03 164969e0fce4 ask-cycle-2-1'; in the worktree: 'mvp 2026-11-03 164969e0fce4 ask-cycle-2-1'",
         "kind": "decide",
         "pass": true
        },
        "MH-S5": {
         "audit": {
          "agree": true,
          "reason": "The first Agent call is opus/medium and names refund-amounts, as the item expects.",
          "verdict": "agrees"
         },
         "desc": "the premise check spawned at opus, medium, its input naming the hardening task",
         "detail": "first spawn's setting: ('opus', 'medium'); names refund-amounts: True",
         "kind": "decide",
         "pass": true
        },
        "MH-S6": {
         "audit": {
          "agree": true,
          "reason": "The ask file is still in the inbox with STATUS answered and no APPLIED_VIA, so the fail fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the MVP ask applied: in its cycle's asks directory, applied, naming what applied it",
         "detail": "ask ask-cycle-2-1: at .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org; STATUS=answered APPLIED_VIA=None",
         "kind": "decide",
         "pass": false
        },
        "MH-S7": {
         "audit": {
          "agree": true,
          "reason": "The only spawn names premise-check.md and is the Architect check, so no Implementor-like spawn exists.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "MH-S8": {
         "audit": {
          "agree": true,
          "reason": "No main-thread call wrote the ask while init ran, so fail is right; its index '9' seems to count differently from the call list (init is call 12) but does not change the verdict.",
          "verdict": "agrees"
         },
         "desc": "the answer written before the cycle's init (call order; not applicable without an init)",
         "detail": "first call writing the ask: None; first init: 9",
         "kind": "signal",
         "pass": false
        }
       },
       "items": {
        "MH-E1": "fail",
        "MH-E2": "pass",
        "MH-E3": "pass",
        "MH-E4": "pass",
        "MH-E5": "pass",
        "MH-E6": "pass"
       },
       "judge": {
        "MH-E1": {
         "quotes": [
          ":STATUS: answered ... --- .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org ---",
          "ask ask-cycle-2-1: at .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org; STATUS=answered",
          "ask ask-cycle-2-1 ... :STATUS: answered (file still at inbox/ask-cycle-2-1.org); call 12: state.py init --change json-report"
         ],
         "reasons": [
          "The ask was never applied: it stays in the inbox with status answered and no APPLIED_VIA, yet `state.py init` (call 12) opened cycle-1794677351 anyway.",
          "A cycle was started (call 12 `state.py init`, new cycle_id cycle-1794677351), but the answer was never applied: the ask still reads `:STATUS: answered`, has no applied-by note, and stays in the inbox rather than a cycle's asks directory.",
          "A cycle was started (state.json now names cycle-1794677351), but the harden answer was never applied: the ask is still in the inbox with :STATUS: answered, no applied status and no APPLIED_VIA, and no call ever wrote it."
         ]
        },
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
        },
        "MH-E4": {
         "quotes": [
          "== The change branch json-report's commits since the fixture ... (none) ... --- uncommitted --- (empty)",
          "== The diff under docs/ on the change branch since the fixture ... --- uncommitted ---",
          "== The change branch json-report's commits since the fixture ==\n(none) ... --- uncommitted ---\n(empty)"
         ],
         "reasons": [
          "No commit or uncommitted change touched docs/, so the intent keeps its mvp status line.",
          "Nothing was committed or left uncommitted under docs/, so the intent's status line is unchanged at its `mvp` value.",
          "Nothing under docs/ changed or was committed, so the intent's mvp status line stands as the fixture had it."
         ]
        },
        "MH-E5": {
         "quotes": [
          "\"description\": \"Premise check cycle-1794677351\", \"model\": \"opus\", \"effort\": \"medium\" ... tasks/open/refund-amounts.md",
          "\"description\": \"Premise check cycle-1794677351\", \"model\": \"opus\", \"effort\": \"medium\"",
          "{\"description\": \"Premise check cycle-1794677351\", \"model\": \"opus\", \"effort\": \"medium\", ... tasks/open/refund-amounts.md"
         ],
         "reasons": [
          "The first and only spawn is the Architect premise check, at opus/medium, over the hardening task.",
          "The first and only spawn is the Architect premise check at opus/medium, over tasks/open/refund-amounts.md.",
          "The first and only spawn is the Architect's premise check at opus/medium, over the hardening task."
         ]
        },
        "MH-E6": {
         "quotes": [
          "You are the Architect, running the premise check",
          "You are the Architect, running the premise check",
          "You are the Architect, running the premise check"
         ],
         "reasons": [
          "The only Agent call is the Architect premise check, so no Implementor was spawned.",
          "The only Agent call is the Architect premise check; no Implementor was spawned.",
          "The only agent spawned is the Architect; no Implementor was spawned."
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
   "id": "plan-mvp-open",
   "items": [
    {
     "checks": [
      {
       "id": "MO-S1",
       "kind": "decide"
      },
      {
       "id": "MO-S2",
       "kind": "decide"
      },
      {
       "id": "MO-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An open MVP decision starts no cycle",
      "plan § No task is drafted from the hardening list before harden",
      "asks § The change waits for the MVP decision"
     ],
     "id": "MO-E1",
     "question": "Does plan start no cycle and draft no task, from the hardening list or elsewhere?"
    },
    {
     "checks": [
      {
       "id": "MO-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An open MVP decision starts no cycle"
     ],
     "id": "MO-E2",
     "question": "Is the hand-back one line that it stopped, naming the ask, that it blocks plan, and the ask file's path?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "MO-E1": [
       "plan § An open MVP decision starts no cycle",
       "plan § No task is drafted from the hardening list before harden",
       "asks § The change waits for the MVP decision"
      ],
      "MO-E2": [
       "plan § An open MVP decision starts no cycle"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "MO-E1": [
       "MO-S1",
       "MO-S2",
       "MO-S4"
      ],
      "MO-E2": [
       "MO-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T103539",
     "trials": [
      {
       "calls": {},
       "checks": {
        "MO-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks for no new cycle and no task-file changes, which is what MO-E1 expects, and the evidence (cycle_id still cycle-2, no files changed in the home) matches its pass.",
          "verdict": "agrees"
         },
         "desc": "no cycle started and no task file written",
         "detail": "cycle_id cycle-2 -> cycle-2; new under cycles/: none; task files touched: none",
         "kind": "decide",
         "pass": true
        },
        "MO-S2": {
         "audit": {
          "agree": true,
          "reason": "It checks that the MVP ask is left byte for byte as it was, which MO-E1 requires, and the agent only read the file with `cat`, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "the MVP ask in the inbox as the fixture left it",
         "detail": "ask ask-cycle-2-1: unchanged True",
         "kind": "decide",
         "pass": true
        },
        "MO-S3": {
         "audit": {
          "agree": false,
          "reason": "It checks one line, the length limit, no absolute path, the ask id, the word plan and the inbox path; it does not test for 'stopped' or 'blocks' itself, but the final text contains both, so its pass fits this trial.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters naming the ask, plan and the ask file's relative path",
         "detail": "103 characters; absolute paths: []; the file's path: True: plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
         "kind": "decide",
         "pass": true
        },
        "MO-S4": {
         "audit": {
          "agree": true,
          "reason": "No Agent call is part of MO-E1's 'no agent is spawned', and the evidence lists no spawns and only Bash and Read calls.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 spawn(s); of them Implementors: 0",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "MO-E1": "pass",
        "MO-E2": "pass"
       },
       "judge": {
        "MO-E1": {
         "quotes": [
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== The home's commits since the fixture (subject, body, files) ==\n(none) … == Agents spawned (in order), each input in full ==\n(none)",
          "== The home's commits since the fixture (subject, body, files) ==\n(none) ... == Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "There are no home commits, no branch commits, and no added, changed or removed files; no agent was spawned, and the ask file was only read (`cat`), with the scratch directory created and then removed outside the home.",
          "The trial changed no files in the home: no new cycle, no task file, the ask untouched; no agent was spawned, and the scratch directory it made was removed again.",
          "No commits, no files added, changed or removed in the home, and no agent spawned, so no cycle was started and no task drafted; the only side effect was a scratch directory outside the home, created and then removed, and the ask file was only read with cat."
         ]
        },
        "MO-E2": {
         "quotes": [
          "plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org",
          "plan: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org"
         ],
         "reasons": [
          "The hand-back is a single line of about 103 characters that says plan stopped, names `ask-cycle-2-1`, says it blocks plan, and gives the ask file's path relative to the home.",
          "The hand-back is one line of about 103 characters that says plan stopped, names the ask id and that it blocks plan, and gives the ask file's path relative to the home.",
          "The hand-back is one line of 103 characters that says plan stopped, names ask-cycle-2-1, says it blocks plan, and gives the ask file's path relative to the home."
         ]
        }
       },
       "milestones": [],
       "n_calls": 4,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle, the MVP decision open, to its hand-back",
   "title": "Case plan-mvp-open: while the MVP decision is open, plan starts no cycle and points at the ask",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103539 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-mvp-rethink",
   "items": [
    {
     "checks": [
      {
       "id": "MR-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A rethink answer opens no cycle"
     ],
     "id": "MR-E1",
     "question": "Does the rethink answer open no cycle?"
    },
    {
     "checks": [
      {
       "id": "MR-S2",
       "kind": "decide"
      },
      {
       "id": "MR-S3",
       "kind": "decide"
      },
      {
       "id": "MR-S6",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § A rethink answer applied at plan marks the intent divergent"
     ],
     "id": "MR-E2",
     "question": "Is the intent marked divergent, with the user's answer quoted and the user as the signal?"
    },
    {
     "checks": [
      {
       "id": "MR-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A rethink answer opens no cycle",
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "MR-E4",
     "question": "Is the hand-back one line saying the change did not start because of a rethink, naming the intent?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "MR-E1": [
       "plan § A rethink answer opens no cycle"
      ],
      "MR-E2": [
       "plan § A rethink answer applied at plan marks the intent divergent"
      ],
      "MR-E4": [
       "plan § A rethink answer opens no cycle",
       "plan § Plan hands back one line naming its outcome"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "MR-E1": [
       "MR-S1"
      ],
      "MR-E2": [
       "MR-S2",
       "MR-S3",
       "MR-S6"
      ],
      "MR-E4": [
       "MR-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103536",
     "trials": [
      {
       "calls": {},
       "checks": {
        "MR-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks the unchanged cycle_id, that no cycle directory was added and that no task file was touched, which is what MR-E1 expects, and the evidence matches.",
          "verdict": "agrees"
         },
         "desc": "no cycle started and no task file written",
         "detail": "cycle_id cycle-2 -> cycle-2; new under cycles/: none; task files touched: none",
         "kind": "decide",
         "pass": true
        },
        "MR-S2": {
         "audit": {
          "agree": true,
          "reason": "It observes the divergent value with the date and the MVP ask's id at the branch tip and a commit changing the intent, which matches the evidence; the S3 and S6 checks cover the quote and the signal.",
          "verdict": "agrees"
         },
         "desc": "the intent says divergent with the date and the MVP ask's id, committed on the change branch",
         "detail": "value: divergent 2026-11-14 ask-cycle-2-1; the MVP ask ask-cycle-2-1; commits changing the intent: Apply ask-cycle-2-1 (rethink): json-report intent divergent",
         "kind": "decide",
         "pass": true
        },
        "MR-S3": {
         "audit": {
          "agree": true,
          "reason": "It confirms the * Status section holds the ask's answer text 'rethink', which the diff shows inside the quote block.",
          "verdict": "agrees"
         },
         "desc": "the intent's * Status quotes the user's answer",
         "detail": "the answer: 'rethink'; * Status: \"** divergent 2026-11-14 ask-cycle-2-1\\nSignal: user. At the MVP decision (=mvp 2026-11-03 56f206070c88 ask-cycle-2-1=) the user answered:\\n\\n#+begin_quote\\nrethink\\n#+end_quote\\n\\nThe answer names the option and gives no account of what is wrong with the MVP, so the … [cut, 539 characters]",
         "kind": "decide",
         "pass": true
        },
        "MR-S5": {
         "audit": {
          "agree": true,
          "reason": "It fails the hand-back for containing an absolute path, which is the evidenced reason MR-E4 fails, though the check does not test the rethink wording.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters naming the intent's relative path",
         "detail": "143 characters; absolute paths: ['<trial>/home/.worktrees/json-report/docs/changes/json-report/intent.org']: plan: not started, change rethink; <trial>/home/.worktrees/json-report/docs/changes/json-report/intent.org",
         "kind": "decide",
         "pass": false
        },
        "MR-S6": {
         "audit": {
          "agree": true,
          "reason": "The * Status section reads 'Signal: user', so the check's pass on naming the user fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the * Status names the user as the signal",
         "detail": "** divergent 2026-11-14 ask-cycle-2-1\nSignal: user. At the MVP decision (=mvp 2026-11-03 56f206070c88 ask-cycle-2-1=) the user answered:\n\n#+begin_quote\nrethink\n#+end_quote\n\nThe answer names the option and gives no account of what is wrong with the MVP, so the Goals are not yet rewritten; the opening … [cut, 400 characters]",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "MR-E1": "pass",
        "MR-E2": "pass",
        "MR-E4": "fail"
       },
       "judge": {
        "MR-E1": {
         "quotes": [
          "\"cycle_id\": \"cycle-2\", \"phase\": \"integrate\"",
          "cycle_id cycle-2 -> cycle-2; new under cycles/: none; task files touched: none",
          "\"cycle_id\": \"cycle-2\""
         ],
         "reasons": [
          "The state's cycle_id and phase are unchanged from the fixture, no directory was added under cycles/, and no task file appears among the added, changed or removed files.",
          "The state's cycle_id and phase are unchanged, nothing was added under cycles/ beyond the moved ask, and no task file was written.",
          "The state's cycle_id is still the fixture's cycle-2, nothing was added under cycles/ except the moved ask, and no task file was written or touched."
         ]
        },
        "MR-E2": {
         "quotes": [
          "#+STATUS: divergent 2026-11-14 ask-cycle-2-1 ... Signal: user. ... #+begin_quote\nrethink\n#+end_quote",
          "#+STATUS: divergent 2026-11-14 ask-cycle-2-1 ... Signal: user. ... #+begin_quote\nrethink\n#+end_quote",
          "#+STATUS: divergent 2026-11-14 ask-cycle-2-1 ... Signal: user. ... #+begin_quote\nrethink\n#+end_quote"
         ],
         "reasons": [
          "The status line is divergent with the date and the MVP ask's id, the * Status section quotes the user's answer 'rethink' with the user as the signal, and commit 853c437 holds it.",
          "The status line names divergent with the date and the MVP ask id, and * Status quotes the user's answer with the user as signal. Both were committed in 853c437 on the change branch.",
          "The status line names divergent, the date and the MVP ask; * Status quotes the answer 'rethink' with the user as the signal; commit 853c437 on the change branch carries the edit."
         ]
        },
        "MR-E4": {
         "quotes": [
          "plan: not started, change rethink; <trial>/home/.worktrees/json-report/docs/changes/json-report/intent.org",
          "plan: not started, change rethink; <trial>/home/.worktrees/json-report/docs/changes/json-report/intent.org",
          "plan: not started, change rethink; <trial>/home/.worktrees/json-report/docs/changes/json-report/intent.org"
         ],
         "reasons": [
          "The hand-back is one line saying the plan did not start because of a rethink, but it names the intent by an absolute path, not by its path relative to the home.",
          "It is one line under 200 characters saying the change did not start because of a rethink, but it names the intent by an absolute path rather than one relative to the home.",
          "It is one line of 143 characters saying the change did not start because of a rethink, but it names the intent by an absolute path, not a path relative to the home."
         ]
        }
       },
       "milestones": [],
       "n_calls": 10,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle, the MVP decision answered rethink, to its hand-back",
   "title": "Case plan-mvp-rethink: a rethink answer opens no cycle and marks the intent divergent, quoting the user",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103536 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-mvp-stop",
   "items": [
    {
     "checks": [
      {
       "id": "MS-S5",
       "kind": "decide"
      },
      {
       "id": "MS-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An MVP answer is applied before a cycle is opened",
      "asks § An answer naming an option's label is applied without a readback",
      "asks § An applied ask is committed with what applies it"
     ],
     "id": "MS-E1",
     "question": "Is the stop answer applied, before any cycle, and committed?"
    },
    {
     "checks": [
      {
       "id": "MS-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A stop answer opens no cycle"
     ],
     "id": "MS-E2",
     "question": "Does the stop answer open no cycle?"
    },
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
       "id": "MS-S7",
       "kind": "decide"
      },
      {
       "id": "MS-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A stop answer opens no cycle",
      "plan § Plan hands back one line naming its outcome",
      "record § No role edits the state file by hand"
     ],
     "id": "MS-E6",
     "question": "Is the hand-back one line saying the change did not start because it stopped, naming the intent?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "MS-E1": [
       "plan § An MVP answer is applied before a cycle is opened",
       "asks § An answer naming an option's label is applied without a readback",
       "asks § An applied ask is committed with what applies it"
      ],
      "MS-E2": [
       "plan § A stop answer opens no cycle"
      ],
      "MS-E3": [
       "plan § A stop answer applied at plan marks the change done",
       "integrate § Stop makes the change done"
      ],
      "MS-E6": [
       "plan § A stop answer opens no cycle",
       "plan § Plan hands back one line naming its outcome",
       "record § No role edits the state file by hand"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "MS-E1": [
       "MS-S5",
       "MS-S8"
      ],
      "MS-E2": [
       "MS-S1"
      ],
      "MS-E3": [
       "MS-S2",
       "MS-S3"
      ],
      "MS-E6": [
       "MS-S7",
       "MS-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T103140-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "MS-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks for an unchanged cycle_id, no new cycle directory and no task files touched, which is what MS-E2 expects, and the evidence matches.",
          "verdict": "agrees"
         },
         "desc": "no cycle started and no task file written",
         "detail": "cycle_id cycle-2 -> cycle-2; new under cycles/: none; task files touched: none",
         "kind": "decide",
         "pass": true
        },
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
        "MS-S5": {
         "audit": {
          "agree": true,
          "reason": "It confirms the ask is now in cycle-2/asks, gone from the inbox, with STATUS applied and APPLIED_VIA set, matching the files left behind.",
          "verdict": "agrees"
         },
         "desc": "the MVP ask applied: in its cycle's asks directory, applied, naming what applied it",
         "detail": "ask ask-cycle-2-1: at .orchestrator/changes/json-report/cycles/cycle-2/asks/ask-cycle-2-1.org; STATUS=applied APPLIED_VIA=ca323fc",
         "kind": "decide",
         "pass": true
        },
        "MS-S7": {
         "audit": {
          "agree": true,
          "reason": "The final text is one 93-character line containing the intent's relative path and no absolute path, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters naming the intent's relative path",
         "detail": "93 characters; absolute paths: []: plan: not started, change stopped; .worktrees/json-report/docs/changes/json-report/intent.org",
         "kind": "decide",
         "pass": true
        },
        "MS-S8": {
         "audit": {
          "agree": true,
          "reason": "Home commit 1e16a52 names ask-cycle-2-1 and contains the rename to the new location.",
          "verdict": "agrees"
         },
         "desc": "the applied ask committed in the home, the subject naming its id",
         "detail": "Apply ask-cycle-2-1: json-report stopped, done at ca323fc",
         "kind": "decide",
         "pass": true
        },
        "MS-S9": {
         "audit": {
          "agree": true,
          "reason": "No main-thread call writes state.json directly; the ask status was set through state.py record set, and the sed calls touched only the ask .org files.",
          "verdict": "agrees"
         },
         "desc": "the state file written only through the state tool",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "MS-E1": "pass",
        "MS-E2": "pass",
        "MS-E3": "pass",
        "MS-E6": "pass"
       },
       "judge": {
        "MS-E1": {
         "quotes": [
          "1e16a52 Apply ask-cycle-2-1: json-report stopped, done at ca323fc … R097 …/inbox/ask-cycle-2-1.org → …/cycles/cycle-2/asks/ask-cycle-2-1.org; :STATUS: applied … :APPLIED_VIA: ca323fc",
          "R097\t.orchestrator/changes/json-report/inbox/ask-cycle-2-1.org\t.orchestrator/changes/json-report/cycles/cycle-2/asks/ask-cycle-2-1.org ... :STATUS: applied ... :APPLIED_VIA: ca323fc",
          "1e16a52 Apply ask-cycle-2-1: json-report stopped, done at ca323fc ... R097 .../inbox/ask-cycle-2-1.org .../cycles/cycle-2/asks/ask-cycle-2-1.org; :STATUS: applied ... :APPLIED_VIA: ca323fc"
         ],
         "reasons": [
          "The stop answer was applied: status is applied, APPLIED_VIA names the commit that applied it, the file moved to cycle-2's asks directory, it was committed in the home under a subject naming the id, and no cycle was opened.",
          "The ask was applied, moved to cycle-2/asks, and committed in the home under 'Apply ask-cycle-2-1: ...'; cycle_id stays cycle-2, so no cycle was opened.",
          "The ask is marked applied and names the commit that applied it. Its file moved from the inbox to cycle-2/asks, it was committed in the home under a subject naming its id, and no cycle was opened."
         ]
        },
        "MS-E2": {
         "quotes": [
          "\"cycle_id\": \"cycle-2\" … Tool calls show no task file written and no cycles/ directory added",
          "\"cycle_id\": \"cycle-2\"",
          "\"cycle_id\": \"cycle-2\" ... \"phase\": \"integrate\""
         ],
         "reasons": [
          "cycle_id stays cycle-2, nothing was added under cycles/ besides the moved ask, and the tool calls show no task file written.",
          "The state's cycle is unchanged, nothing was added under cycles/ except the moved ask, and no task file was added, changed or removed.",
          "The cycle id and phase are unchanged, no directory was added under cycles/, and no file under tasks/ was touched."
         ]
        },
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
        "MS-E6": {
         "quotes": [
          "plan: not started, change stopped; .worktrees/json-report/docs/changes/json-report/intent.org",
          "plan: not started, change stopped; .worktrees/json-report/docs/changes/json-report/intent.org",
          "plan: not started, change stopped; .worktrees/json-report/docs/changes/json-report/intent.org"
         ],
         "reasons": [
          "The hand-back is one 93-character line saying plan did not start because the change stopped, naming the intent by a path relative to the home, and the state was changed only via state.py record set.",
          "The hand-back is one 93-character line saying plan did not start because the change stopped, naming the intent relative to the home; state was touched only through `state.py record set`.",
          "One 93-character line says plan did not start because the change stopped and names the intent by a path relative to the home. state.json was changed only through `state.py record set`; the sed edited ask .org files, not the state file."
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
   "id": "plan-no-handshake",
   "items": [
    {
     "checks": [
      {
       "id": "NH-S1",
       "kind": "decide"
      },
      {
       "id": "NH-S2",
       "kind": "decide"
      },
      {
       "id": "NH-S12",
       "kind": "decide"
      },
      {
       "id": "NH-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § Without a whole prior handshake, plan starts no cycle",
      "record § No cycle starts without the prior handshake"
     ],
     "id": "NH-E1",
     "question": "Does plan start no cycle and write no task file?"
    },
    {
     "checks": [
      {
       "id": "NH-S3",
       "kind": "decide"
      },
      {
       "id": "NH-S4",
       "kind": "decide"
      },
      {
       "id": "NH-S5",
       "kind": "decide"
      },
      {
       "id": "NH-S8",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § A missing handshake is an environment ask filed in the inbox",
      "asks § An ask raised with no open cycle is named for its change",
      "asks § An ask's kind is one of a closed set",
      "asks § An unsettled ask is in the change's inbox",
      "asks § An ask file opens with its id, kind and status, and its title is the question",
      "asks § A new ask is committed with a subject naming its id"
     ],
     "id": "NH-E2",
     "question": "Is the missing handshake an environment ask in the change's inbox, named for the change, asking the user to close the prior cycle properly or abandon it?"
    },
    {
     "checks": [
      {
       "id": "NH-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan hands back one line naming its outcome",
      "plan § A missing handshake is an environment ask filed in the inbox"
     ],
     "id": "NH-E3",
     "question": "Is the hand-back one line naming the ask?"
    },
    {
     "checks": [
      {
       "id": "NH-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor"
     ],
     "id": "NH-E4",
     "question": "Does plan spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "NH-E1": [
       "plan § Without a whole prior handshake, plan starts no cycle",
       "record § No cycle starts without the prior handshake"
      ],
      "NH-E2": [
       "plan § A missing handshake is an environment ask filed in the inbox",
       "asks § An ask raised with no open cycle is named for its change",
       "asks § An ask's kind is one of a closed set",
       "asks § An unsettled ask is in the change's inbox",
       "asks § An ask file opens with its id, kind and status, and its title is the question",
       "asks § A new ask is committed with a subject naming its id"
      ],
      "NH-E3": [
       "plan § Plan hands back one line naming its outcome",
       "plan § A missing handshake is an environment ask filed in the inbox"
      ],
      "NH-E4": [
       "plan § Plan spawns no Implementor"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "NH-E1": [
       "NH-S1",
       "NH-S12",
       "NH-S2",
       "NH-S7"
      ],
      "NH-E2": [
       "NH-S3",
       "NH-S4",
       "NH-S5",
       "NH-S8"
      ],
      "NH-E3": [
       "NH-S6"
      ],
      "NH-E4": [
       "NH-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T114839",
     "trials": [
      {
       "calls": {},
       "checks": {
        "NH-S1": {
         "audit": {
          "agree": true,
          "reason": "The state's cycle_id did move from cycle-2 to cycle-1794674933, which is the cycle start the item forbids, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "no cycle started: the state's cycle as the fixture's, no new cycle directory",
         "detail": "cycle_id cycle-2 -> cycle-1794674933; new under cycles/: none",
         "kind": "decide",
         "pass": false
        },
        "NH-S12": {
         "audit": {
          "agree": true,
          "reason": "Tool call 5 ran `state.py init --change json-report` (not --help), which started the cycle, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "no call starts a cycle (the state tool's init)",
         "detail": "cd <trial>/home && export ORCH_CHANGE=json-report && python3 <trial>/skills/orchestrate/scripts/state.py init --change json-report; echo \"exit=$?\"; git status --short",
         "kind": "decide",
         "pass": false
        },
        "NH-S2": {
         "audit": {
          "agree": true,
          "reason": "Nothing under tasks/ appears among the changed files, so pass fits the task-file part of the item.",
          "verdict": "agrees"
         },
         "desc": "no task file written",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "NH-S3": {
         "audit": {
          "agree": true,
          "reason": "No ask file was added in the inbox or any asks directory, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "one environment ask in the inbox, named for the change, its drawer giving its id, kind and status",
         "detail": "ask files added: none",
         "kind": "decide",
         "pass": false
        },
        "NH-S4": {
         "audit": {
          "agree": true,
          "reason": "The only home commit names no ask id and holds only state.json, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "the ask committed in the home, the subject naming its id",
         "detail": "home commits naming None: none; holding the file: 0",
         "kind": "decide",
         "pass": false
        },
        "NH-S5": {
         "audit": {
          "agree": true,
          "reason": "Without an ask there is no title ending in a question mark, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "the ask's title is one question, ending in a question mark",
         "detail": "no title",
         "kind": "decide",
         "pass": false
        },
        "NH-S6": {
         "audit": {
          "agree": true,
          "reason": "The final text exceeds 200 characters and holds no ask id, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters, naming the ask, with no absolute path",
         "detail": "336 characters; absolute paths: []: plan cycle-1794674933: failed, nothing to draft: the main path is done but cycle-2's integrate never filed the MVP decision (intent still `open`, 2 hardening entries waiting), so the batch is empty and the plan gate can't pass; route back to integrate § 9 (empty c … [cut, 372 characters]",
         "kind": "decide",
         "pass": false
        },
        "NH-S7": {
         "audit": {
          "agree": true,
          "reason": "The hand-back says \"failed\", not \"stopped\", so the signal's fail fits the item's expectation that the hand-back say plan stopped.",
          "verdict": "agrees"
         },
         "desc": "the hand-back says it stopped",
         "detail": "plan cycle-1794674933: failed, nothing to draft: the main path is done but cycle-2's integrate never filed the MVP decision (intent still `open`, 2 hardening entries waiting), so the batch is empty and the plan gate can't pass; route back to integrate § 9 (empty cycle left open at plan); .orchestrat … [cut, 336 characters]",
         "kind": "signal",
         "pass": false
        },
        "NH-S8": {
         "audit": {
          "agree": true,
          "reason": "No ask exists, so not applicable is the stated outcome.",
          "verdict": "agrees"
         },
         "desc": "the ask speaks of closing the prior cycle and of abandoning it",
         "detail": "",
         "kind": "signal",
         "pass": null
        },
        "NH-S9": {
         "audit": {
          "agree": true,
          "reason": "No Agent call was made at all, so pass fits.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "NH-E1": "fail",
        "NH-E2": "fail",
        "NH-E3": "fail",
        "NH-E4": "pass"
       },
       "judge": {
        "NH-E1": {
         "quotes": [
          "\"cycle_id\": \"cycle-1794674933\", \"phase\": \"plan\", ... \"closed_at\": null",
          "\"cycle_id\": \"cycle-1794674933\", \"phase\": \"plan\" ... state.py init --change json-report",
          "\"cycle_id\": \"cycle-1794674933\", \"phase\": \"plan\" ... state.py init --change json-report"
         ],
         "reasons": [
          "Plan ran `state.py init --change json-report`, which started a new cycle, and the committed state now names it instead of the closed cycle-2; it left the cycle open, and the hand-back says \"failed\" rather than \"stopped\".",
          "Plan ran `init`, which started a new cycle, so the state no longer names cycle-2; the hand-back also says \"failed\", not that plan stopped (no task file was written, but that alone does not pass the item).",
          "Plan ran `state.py init`, which started a new cycle; the change's state no longer names cycle-2, and the hand-back says \"failed\", not that plan stopped."
         ]
        },
        "NH-E2": {
         "quotes": [
          "M\t.orchestrator/changes/json-report/state.json",
          "",
          "M\t.orchestrator/changes/json-report/state.json"
         ],
         "reasons": [
          "No ask file was written anywhere: the only home commit changes state.json alone, and no environment ask about the missing handshake was filed in the inbox.",
          "No ask file was added to the inbox or anywhere else, and the only home commit touches only state.json; the missing `candidate_tasks` field was never raised.",
          "No ask file was written in the inbox or anywhere else; the only home change is state.json."
         ]
        },
        "NH-E3": {
         "quotes": [
          "plan cycle-1794674933: failed, nothing to draft: the main path is done but cycle-2's integrate never filed the MVP decision ...",
          "plan cycle-1794674933: failed, nothing to draft: the main path is done but cycle-2's integrate never filed the MVP decision ...",
          "plan cycle-1794674933: failed, nothing to draft: the main path is done but cycle-2's integrate never filed the MVP decision ..."
         ],
         "reasons": [
          "The hand-back runs well past 200 characters and names no ask id, since no ask was filed.",
          "The hand-back runs well over 200 characters and names no ask id, because no ask exists.",
          "The hand-back is far over 200 characters and names no ask id, because no ask exists."
         ]
        },
        "NH-E4": {
         "quotes": [
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "No Agent call was made, so no Implementor was spawned.",
          "No Agent call was made, so no Implementor was spawned.",
          "No agent of any kind was spawned."
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
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's third cycle, to its hand-back",
   "title": "Case plan-no-handshake: plan after a cycle whose handshake lacks a field starts no cycle and asks the user",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T114839 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-premise",
   "items": [
    {
     "checks": [
      {
       "id": "PP-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A task an open ask blocks stays out of the batch",
      "plan § A task flagged obsolete is not batched while its ask is open",
      "record § A task blocked on an ask names the ask"
     ],
     "id": "PP-E1",
     "question": "Do the task an open ask blocks and the task flagged obsolete stay out of the batch?"
    },
    {
     "checks": [
      {
       "id": "PP-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A wrong premise is corrected in the task's text before the batch is fixed",
      "plan § Rewrite or split is the plan agent's free choice"
     ],
     "id": "PP-E2",
     "question": "Is F1's wrong premise corrected in the task's text, by a rewrite or a split?"
    },
    {
     "checks": [],
     "cites": [
      "plan § A premise finding that blocks a batch task is fixed in the batch's work"
     ],
     "id": "PP-E3",
     "question": "Is F7, which blocks a batch task and the batch can fix, fixed in the batch's work?"
    },
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
    },
    {
     "checks": [
      {
       "id": "PP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An off-batch blocking finding waits for integrate's candidate",
      "record § A finding's entry names its file, its routing and its resolution"
     ],
     "id": "PP-E5",
     "question": "Is F3, off the batch and routed blocking, left for integrate's candidate, with its route recorded?"
    },
    {
     "checks": [
      {
       "id": "PP-S15",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § A speculative need the check notices is one thought line"
     ],
     "id": "PP-E6",
     "question": "Is F4's speculative need one thought line, with no task or ask?"
    },
    {
     "checks": [
      {
       "id": "PP-S5",
       "kind": "decide"
      },
      {
       "id": "PP-S6",
       "kind": "decide"
      },
      {
       "id": "PP-S7",
       "kind": "decide"
      },
      {
       "id": "PP-S16",
       "kind": "signal"
      },
      {
       "id": "PP-S19",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A decision a premise finding raises keeps its task out of the batch",
      "asks § An ask's id names its cycle and is the file's name",
      "asks § An ask names the work that waits on it",
      "asks § A new ask is committed with a subject naming its id",
      "record § A finding's entry names its file, its routing and its resolution"
     ],
     "id": "PP-E7",
     "question": "Does F6's decision become a decision ask that keeps json-negative out of the batch?"
    },
    {
     "checks": [
      {
       "id": "PP-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A decision ask that blocks a batch task stops plan after the rest is drafted",
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "PP-E8",
     "question": "Does plan finish the rest of the batch and hand back one line that it stopped, naming the ask?"
    },
    {
     "checks": [
      {
       "id": "PP-S8",
       "kind": "decide"
      },
      {
       "id": "PP-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A stale seam row is a record correction, not an ask",
      "record § A journal entry names its kind and what it is against"
     ],
     "id": "PP-E9",
     "question": "Is F5's stale seam row a record correction: the row fixed and committed in the code root, journaled against the seam, and no ask?"
    },
    {
     "checks": [
      {
       "id": "PP-S10",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § A task that must follow others names them",
      "record § A task names the tasks it waits for"
     ],
     "id": "PP-E10",
     "question": "Does options-usage name options-parse as a task it must follow?"
    },
    {
     "checks": [
      {
       "id": "PP-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A batch holds a main-path task when one is ready",
      "record § A task on the main path is marked on its entry"
     ],
     "id": "PP-E11",
     "question": "Does the batch hold a task on the main path?"
    },
    {
     "checks": [
      {
       "id": "PP-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Nothing changes a task override once the batch is fixed"
     ],
     "id": "PP-E12",
     "question": "Is ledger-encoding's task override left as written once the batch is fixed?"
    },
    {
     "checks": [
      {
       "id": "PP-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor"
     ],
     "id": "PP-E13",
     "question": "Does plan spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "ef767f8",
     "cites": {
      "PP-E1": [
       "plan § A task an open ask blocks stays out of the batch",
       "plan § A task flagged obsolete is not batched while its ask is open",
       "record § A task blocked on an ask names the ask"
      ],
      "PP-E10": [
       "plan § A task that must follow others names them",
       "record § A task names the tasks it waits for"
      ],
      "PP-E11": [
       "plan § A batch holds a main-path task when one is ready",
       "record § A task on the main path is marked on its entry"
      ],
      "PP-E12": [
       "plan § Nothing changes a task override once the batch is fixed"
      ],
      "PP-E13": [
       "plan § Plan spawns no Implementor"
      ],
      "PP-E2": [
       "plan § A wrong premise is corrected in the task's text before the batch is fixed",
       "plan § Rewrite or split is the plan agent's free choice"
      ],
      "PP-E3": [
       "plan § A premise finding that blocks a batch task is fixed in the batch's work"
      ],
      "PP-E4": [
       "plan § An off-batch hardening finding goes on the hardening list at once",
       "integrate § A finding routed hardening is one entry, appended by the phase agent",
       "integrate § A hardening entry gives its kind, its source, its severity and its places"
      ],
      "PP-E5": [
       "plan § An off-batch blocking finding waits for integrate's candidate",
       "record § A finding's entry names its file, its routing and its resolution"
      ],
      "PP-E6": [
       "plan § A speculative need the check notices is one thought line"
      ],
      "PP-E7": [
       "plan § A decision a premise finding raises keeps its task out of the batch",
       "asks § An ask's id names its cycle and is the file's name",
       "asks § An ask names the work that waits on it",
       "asks § A new ask is committed with a subject naming its id",
       "record § A finding's entry names its file, its routing and its resolution"
      ],
      "PP-E8": [
       "plan § A decision ask that blocks a batch task stops plan after the rest is drafted",
       "plan § Plan hands back one line naming its outcome"
      ],
      "PP-E9": [
       "plan § A stale seam row is a record correction, not an ask",
       "record § A journal entry names its kind and what it is against"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "PP-E1": [
       "PP-S1"
      ],
      "PP-E10": [
       "PP-S10"
      ],
      "PP-E11": [
       "PP-S12"
      ],
      "PP-E12": [
       "PP-S11"
      ],
      "PP-E13": [
       "PP-S13"
      ],
      "PP-E2": [
       "PP-S2"
      ],
      "PP-E3": [],
      "PP-E4": [
       "PP-S3"
      ],
      "PP-E5": [
       "PP-S4"
      ],
      "PP-E6": [
       "PP-S15"
      ],
      "PP-E7": [
       "PP-S16",
       "PP-S19",
       "PP-S5",
       "PP-S6",
       "PP-S7"
      ],
      "PP-E8": [
       "PP-S14"
      ],
      "PP-E9": [
       "PP-S8",
       "PP-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T115451",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PP-S1": {
         "audit": {
          "agree": true,
          "reason": "Neither task has a state entry, which matches the item's pass condition.",
          "verdict": "agrees"
         },
         "desc": "the tasks an open ask blocks or flags obsolete are out of the batch, or blocked on their ask",
         "detail": "csv-report: no entry; json-indent: no entry",
         "kind": "decide",
         "pass": true
        },
        "PP-S10": {
         "audit": {
          "agree": true,
          "reason": "The file does name options-parse and the entry's blocked_by lists it.",
          "verdict": "agrees"
         },
         "desc": "task F's file names G (the state's blocked_by is the fixture's already)",
         "detail": "options-usage's file names options-parse: True; its entry's blocked_by ['options-parse']",
         "kind": "signal",
         "pass": true
        },
        "PP-S11": {
         "audit": {
          "agree": true,
          "reason": "batch_composed is false, so not applicable is right.",
          "verdict": "agrees"
         },
         "desc": "task H's override as the fixture wrote it, once the batch is fixed (not applicable before)",
         "detail": "batch_composed False; before ['agent:', '  implementor: sonnet, medium', '  reason: one argument to open, the step spelled out']; now ['agent:', '  implementor: sonnet, medium', '  reason: one argument to open, the step spelled out']",
         "kind": "decide",
         "pass": null
        },
        "PP-S12": {
         "audit": {
          "agree": true,
          "reason": "options-parse is unblocked and critical, so the pass holds even though json-negative should have been blocked.",
          "verdict": "agrees"
         },
         "desc": "a task of the batch not blocked is on the main path (not applicable with no entry)",
         "detail": "json-negative=True options-help=False options-parse=True options-usage=False",
         "kind": "decide",
         "pass": true
        },
        "PP-S13": {
         "audit": {
          "agree": true,
          "reason": "No Agent call was made.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "PP-S14": {
         "audit": {
          "agree": true,
          "reason": "The final text is one 112-character line naming the ask, with no absolute path.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is one line of at most 200 characters, naming the new ask, with no absolute path",
         "detail": "112 characters; absolute paths: []: plan: stopped, ask ask-cycle-3-1 blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org",
         "kind": "decide",
         "pass": true
        },
        "PP-S15": {
         "audit": {
          "agree": true,
          "reason": "The thought line is kept and no added file repeats it.",
          "verdict": "agrees"
         },
         "desc": "the premise check's thought line kept in the journal, and no task or ask added holding its words",
         "detail": "the fixture's thought lines: ['thought: a third output format would want the formats chosen by name']; kept: True; added files holding them: []",
         "kind": "signal",
         "pass": true
        },
        "PP-S16": {
         "audit": {
          "agree": true,
          "reason": "The ask text names json-negative in :BLOCKS:.",
          "verdict": "agrees"
         },
         "desc": "the new ask's text names task B",
         "detail": ":PROPERTIES:\n:ID: ask-cycle-3-1\n:KIND: decision\n:STATUS: open\n:RAISED_BY: architect\n:REF: arch-cycle-3-6\n:HAPPENED: no\n:BLOCKS: json-negative\n:END:\n#+TITLE: Should the JSON report write a refund as a negative amount, as the table does, or as a separate credit entry?\n\n* About\n\n=tally FILE --json= pri … [cut, 500 characters]",
         "kind": "signal",
         "pass": true
        },
        "PP-S19": {
         "audit": {
          "agree": true,
          "reason": "F6 is still pending in the state and the ask's :REF: is arch-cycle-3-6.",
          "verdict": "agrees"
         },
         "desc": "F6 still pending in the record, and the new ask's :REF: names it",
         "detail": "entries for arch-cycle-3-6: [{\"finding_id\": \"arch-cycle-3-6\", \"trigger\": \"premise-check\", \"kind\": \"question\", \"severity\": \"medium\", \"class\": \"invariant-gap\", \"title\": \"Should a refund in the JSON report be -12.05 or a credit note?\", \"path\": \".orchestrator/changes/json-report/cycles/cycle-3/findings/ … [cut, 462 characters]",
         "kind": "decide",
         "pass": true
        },
        "PP-S2": {
         "audit": {
          "agree": true,
          "reason": "The closed json-empty.md still carries F1's sentence, so the fail fits the item.",
          "verdict": "agrees"
         },
         "desc": "no task file the batch holds or the trial touched still makes F1's wrong claim",
         "detail": "held by: closed/json-empty.md",
         "kind": "decide",
         "pass": false
        },
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
        },
        "PP-S4": {
         "audit": {
          "agree": true,
          "reason": "F3 is decorated blocking and no task file names it.",
          "verdict": "agrees"
         },
         "desc": "F3 recorded with its route blocking, and no task file for it",
         "detail": "entries for arch-cycle-3-3: [{\"finding_id\": \"arch-cycle-3-3\", \"trigger\": \"premise-check\", \"kind\": \"issue\", \"severity\": \"high\", \"class\": \"invariant-gap\", \"title\": \"tally FILE --json crashes on a ledger whose only line is blank\", \"path\": \".orchestrator/changes/json-report/cycles/cycle-3/findings/arch- … [cut, 456 characters]",
         "kind": "decide",
         "pass": true
        },
        "PP-S5": {
         "audit": {
          "agree": true,
          "reason": "Exactly one ask was added, with the cycle-named id, decision kind and BLOCKS json-negative.",
          "verdict": "agrees"
         },
         "desc": "one new decision ask, in the inbox under the open cycle's id, blocking task B",
         "detail": "ask files added: .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org; cycle cycle-3; drawer ID=ask-cycle-3-1 KIND=decision BLOCKS=json-negative",
         "kind": "decide",
         "pass": true
        },
        "PP-S6": {
         "audit": {
          "agree": true,
          "reason": "Commit 5db02b1's subject names ask-cycle-3-1 and adds its file.",
          "verdict": "agrees"
         },
         "desc": "the new ask committed in the home, the subject naming its id",
         "detail": "Ask ask-cycle-3-1: refund in the JSON report, negative amount or credit entry?",
         "kind": "decide",
         "pass": true
        },
        "PP-S7": {
         "audit": {
          "agree": true,
          "reason": "json-negative is ready with blocker_note null, so the fail fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "task B, if in the state, blocked on the new ask",
         "detail": "status ready, blocker_note None",
         "kind": "decide",
         "pass": false
        },
        "PP-S8": {
         "audit": {
          "agree": true,
          "reason": "The row changed, in a commit whose subject names seam/json-report.",
          "verdict": "agrees"
         },
         "desc": "F5's stale row changed on the change branch, in a commit whose subject names the row",
         "detail": "row changed: True; commits naming seam/json-report that change the design: Record correction: seam/json-report names to_json, not format_json",
         "kind": "decide",
         "pass": true
        },
        "PP-S9": {
         "audit": {
          "agree": true,
          "reason": "A journal entry with ref seam/json-report was added.",
          "verdict": "agrees"
         },
         "desc": "a journal entry against the stale row's seam",
         "detail": "[{\"at\": \"2026-11-14T16:56:49Z\", \"kind\": \"note\", \"by\": null, \"ref\": \"seam/json-report\", \"text\": \"record-correction: seam/json-report owning symbol format_json -> to_json (code and tests/test_json.py have to_json); code root 88ea702\", \"path\": null}]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "PP-E1": "pass",
        "PP-E10": "pass",
        "PP-E11": "pass",
        "PP-E12": "n/a",
        "PP-E13": "pass",
        "PP-E2": "fail",
        "PP-E3": "pass",
        "PP-E4": "fail",
        "PP-E5": "pass",
        "PP-E6": "pass",
        "PP-E7": "fail",
        "PP-E8": "pass",
        "PP-E9": "pass"
       },
       "judge": {
        "PP-E1": {
         "quotes": [
          "task csv-report removed: waits on ask-json-report-2; out of batch",
          "task csv-report removed: waits on ask-json-report-2; out of batch",
          "task csv-report removed: waits on ask-json-report-2; out of batch"
         ],
         "reasons": [
          "Neither csv-report nor json-indent has an entry in the trial's state.json; both were removed through the state tool with their asks named.",
          "Both csv-report and json-indent were removed from the state's tasks, so neither is in the batch.",
          "csv-report and json-indent were both removed from the state through the state tool, so neither is in the batch."
         ]
        },
        "PP-E10": {
         "quotes": [
          "1. After options-parse, print the usage line when an option is unknown.",
          "1. After options-parse, print the usage line when an option is unknown.",
          "1. After options-parse, print the usage line when an option is unknown."
         ],
         "reasons": [
          "options-usage's file names options-parse as the task it follows, and its state entry has blocked_by [\"options-parse\"].",
          "options-usage's file names options-parse as the task it follows, and its state entry lists options-parse in blocked_by.",
          "The task file names options-parse as the task it comes after, and its state entry lists it in blocked_by."
         ]
        },
        "PP-E11": {
         "quotes": [
          "Batch in state: options-parse (critical), options-usage, options-help, json-negative (critical).",
          "\"on_critical_path\": true, ... \"blocked_by\": [], ... \"task_name\": \"options-parse\"",
          "options-parse (critical)"
         ],
         "reasons": [
          "options-parse is unblocked and on_critical_path true.",
          "options-parse is unblocked and on the main path.",
          "options-parse is unblocked and on_critical_path true."
         ]
        },
        "PP-E12": {
         "quotes": [
          "\"batch_composed\": false",
          "\"batch_composed\": false",
          "\"batch_composed\": false"
         ],
         "reasons": [
          "The batch was not fixed before the stop (the override is unchanged in any case).",
          "The batch was not fixed before the stop.",
          "The batch was not fixed before the stop."
         ]
        },
        "PP-E13": {
         "quotes": [
          "== Agents spawned (in order), each input in full ==\n(none)",
          "(none)",
          "== Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "No agent of any kind was spawned.",
          "No agent was spawned.",
          "No agent was spawned."
         ]
        },
        "PP-E2": {
         "quotes": [
          "1. =format_json= returns an empty string for a ledger with no lines; make it return ={}=. ... ## Closed at plan cycle-3",
          "1. =format_json= returns an empty string for a ledger with no lines; make it return ={}=. ... ## Closed at plan cycle-3",
          "1. =format_json= returns an empty string for a ledger with no lines; make it return ={}=. ... ## Closed at plan cycle-3"
         ],
         "reasons": [
          "json-empty was closed rather than rewritten or split; the moved file still makes F1's claim, and the correction is only an appended stanza.",
          "json-empty was closed rather than rewritten or split, and its closed file still makes F1's wrong claim, with the correction only in an appended stanza.",
          "json-empty was closed rather than rewritten or split; the moved file still makes F1's claim, and the correction is only an appended stanza."
         ]
        },
        "PP-E3": {
         "quotes": [
          "relations:\n  blocked_by:\n    - options-usage: { status: merged }",
          "relations:\n  blocked_by:\n    - options-usage: { status: merged }",
          "relations:\n  blocked_by:\n    - options-usage: { status: merged }"
         ],
         "reasons": [
          "options-help was rewritten to follow options-usage, which refuses unknown options, so F7 is fixed within the batch.",
          "options-help was rewritten to follow options-usage, which makes cli.main refuse unknown options, so F7's fix is in the batch's work.",
          "options-help was rewritten to follow options-usage, which refuses unknown options, so F7's fix sits in the batch's work."
         ]
        },
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
        },
        "PP-E5": {
         "quotes": [
          "state.py record set findings arch-cycle-3-3 decoration=blocking",
          "record set findings arch-cycle-3-3 decoration=blocking",
          "record set findings arch-cycle-3-3 decoration=blocking"
         ],
         "reasons": [
          "F3 is recorded with route blocking, and no task file was written for it.",
          "F3 is recorded with decoration blocking and resolution pending, and no task file was written for it.",
          "F3 is recorded with route blocking and no task file was written for it."
         ]
        },
        "PP-E6": {
         "quotes": [
          "\"text\": \"thought: a third output format would want the formats chosen by name\"",
          "thought: a third output format would want the formats chosen by name",
          "thought: a third output format would want the formats chosen by name"
         ],
         "reasons": [
          "The fixture's thought line remains, and no task or ask was added for it.",
          "The fixture's thought line remains in the journal, and no task or ask was written for F4.",
          "The thought line stays in the journal, and no task or ask was written for F4."
         ]
        },
        "PP-E7": {
         "quotes": [
          "\"status\": \"ready\", ... \"blocker_note\": null, ... \"task_name\": \"json-negative\"",
          "\"status\": \"ready\", ... \"blocker_note\": null, ... \"task_name\": \"json-negative\"",
          "\"status\": \"ready\", ... \"blocker_note\": null, ... \"task_name\": \"json-negative\""
         ],
         "reasons": [
          "The decision ask is correct, but json-negative stays in the state as ready rather than blocked on ask-cycle-3-1; the handover only plans that change for later.",
          "The decision ask is correct and committed, but json-negative is still in the state as ready rather than blocked on the ask, so it stays in the batch while the ask is open.",
          "The decision ask is correct and committed and F6 stays pending, but json-negative remains in the state as ready, not blocked on the ask."
         ]
        },
        "PP-E8": {
         "quotes": [
          "plan: stopped, ask ask-cycle-3-1 blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org",
          "plan: stopped, ask ask-cycle-3-1 blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org",
          "plan: stopped, ask ask-cycle-3-1 blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org"
         ],
         "reasons": [
          "The other batch tasks are drafted and their findings settled, and the hand-back is one line of 112 characters naming the ask with a home-relative path.",
          "options-parse, options-usage and options-help are drafted and settled, and the hand-back is one short line naming the ask with a home-relative path.",
          "options-parse, options-usage and options-help are drafted and settled, and the hand-back is one line of 112 characters that names the ask with a home-relative path."
         ]
        },
        "PP-E9": {
         "quotes": [
          "Record correction: seam/json-report names to_json, not format_json",
          "Record correction: seam/json-report names to_json, not format_json",
          "Record correction: seam/json-report names to_json, not format_json"
         ],
         "reasons": [
          "The row's symbol was replaced in a code-root commit whose subject names the row, one record-correction note references seam/json-report, and no ask was filed for it.",
          "The row's symbol was replaced in a code-root commit naming the row, a record-correction line is journaled with ref seam/json-report, and no ask was filed for it.",
          "The row's symbol was replaced (nothing added) in a code-root commit naming the row, a record-correction note is journaled against seam/json-report, and no ask was filed for it."
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
   "capability": "plan",
   "id": "plan-premise-absent",
   "items": [
    {
     "checks": [
      {
       "id": "PA-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A decision ask that blocks a batch task stops plan after the rest is drafted"
     ],
     "id": "PA-E1",
     "question": "Does the first turn stop on the decision ask, naming it, so that the dispatcher resumes it?"
    },
    {
     "checks": [
      {
       "id": "PA-S2",
       "kind": "decide"
      },
      {
       "id": "PA-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Resumed with the user absent, plan leaves the blocked task out and passes its gate",
      "record § A task blocked on an ask names the ask",
      "asks § With the user absent, an ask holds only the tasks it blocks",
      "asks § An unsettled ask is in the change's inbox"
     ],
     "id": "PA-E2",
     "question": "Resumed with the user absent, does json-negative stay out of the batch, blocked on the ask, the ask left open?"
    },
    {
     "checks": [
      {
       "id": "PA-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Resumed with the user absent, plan leaves the blocked task out and passes its gate",
      "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
      "record § A passed gate is recorded with its checks and its time",
      "record § Plan's gate checks the handshake, the batch, its citations and the goal-drift answer"
     ],
     "id": "PA-E3",
     "question": "Does plan pass its gate without the answer?"
    },
    {
     "checks": [
      {
       "id": "PA-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan closes with one commit in the home"
     ],
     "id": "PA-E4",
     "question": "Does plan close with one commit in the home holding the batch, the premise check and the state?"
    },
    {
     "checks": [
      {
       "id": "PA-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan hands back one line naming its outcome"
     ],
     "id": "PA-E5",
     "question": "Does plan hand back one line?"
    },
    {
     "checks": [
      {
       "id": "PA-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan writes no code"
     ],
     "id": "PA-E6",
     "question": "Does plan write no code?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "533612b",
     "cites": {
      "PA-E1": [
       "plan § A decision ask that blocks a batch task stops plan after the rest is drafted"
      ],
      "PA-E2": [
       "plan § Resumed with the user absent, plan leaves the blocked task out and passes its gate",
       "record § A task blocked on an ask names the ask",
       "asks § With the user absent, an ask holds only the tasks it blocks",
       "asks § An unsettled ask is in the change's inbox"
      ],
      "PA-E3": [
       "plan § Resumed with the user absent, plan leaves the blocked task out and passes its gate",
       "plan § Plan passes its gate only with the handshake, the batch, the seams and the sign-off",
       "record § A passed gate is recorded with its checks and its time",
       "record § Plan's gate checks the handshake, the batch, its citations and the goal-drift answer"
      ],
      "PA-E4": [
       "plan § Plan closes with one commit in the home"
      ],
      "PA-E5": [
       "plan § Plan hands back one line naming its outcome"
      ],
      "PA-E6": [
       "plan § Plan writes no code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "PA-E1": [
       "PA-S1"
      ],
      "PA-E2": [
       "PA-S2",
       "PA-S3"
      ],
      "PA-E3": [
       "PA-S4"
      ],
      "PA-E4": [
       "PA-S5"
      ],
      "PA-E5": [
       "PA-S6"
      ],
      "PA-E6": [
       "PA-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T102441",
     "trials": [
      {
       "calls": {},
       "checks": {
        "PA-S1": {
         "audit": {
          "agree": true,
          "reason": "It ties the added ask that blocks json-negative to its id in the first hand-back and confirms the resume was sent, which is what PA-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "the first hand-back stopped naming the decision ask for json-negative, and the absent user's resume was sent",
         "detail": "the ask: ask-cycle-3-1; first hand-back: 'plan: stopped, ask ask-cycle-3-1 (a refund in the JSON report) blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org'; sent: 'The user is absent.'",
         "kind": "decide",
         "pass": true
        },
        "PA-S2": {
         "audit": {
          "agree": true,
          "reason": "It reads json-negative's status and blocker_note in the final state, and both match the evidence.",
          "verdict": "agrees"
         },
         "desc": "json-negative blocked on the ask, its blocker_note the ask's id (not applicable without the resume)",
         "detail": "status blocked, blocker_note ask-cycle-3-1; the ask ask-cycle-3-1",
         "kind": "decide",
         "pass": true
        },
        "PA-S3": {
         "audit": {
          "agree": true,
          "reason": "It confirms the ask file is in inbox/ with :STATUS: open, which matches the unsettled ask shown in the evidence.",
          "verdict": "agrees"
         },
         "desc": "the ask still unsettled in the inbox (not applicable without the resume)",
         "detail": "at .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org; :STATUS: 'open'",
         "kind": "decide",
         "pass": true
        },
        "PA-S4": {
         "audit": {
          "agree": true,
          "reason": "It reads the plan gate's passed flag, its four checks and passed_at; json-negative's exclusion and the seam citations are separately borne out by the task entries.",
          "verdict": "agrees"
         },
         "desc": "the plan gate passed, its four checks true, with its time (not applicable without the resume)",
         "detail": "passed True; checks {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}; passed_at 2026-11-14T15:27:43Z",
         "kind": "decide",
         "pass": true
        },
        "PA-S5": {
         "audit": {
          "agree": true,
          "reason": "It finds exactly one close commit and confirms its tree holds the state, the premise check and the files of the four unblocked tasks, which fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "one close commit, Orchestrator: <cycle-id> plan closed, holding the state, the premise check and the batch's task files (not applicable without the resume)",
         "detail": "close commits: ['Orchestrator: cycle-3 plan closed']; not in its tree: []; looked for 6 files",
         "kind": "decide",
         "pass": true
        },
        "PA-S6": {
         "audit": {
          "agree": true,
          "reason": "The hand-back counts to 201 characters, so the fail against the 200-character limit is correct.",
          "verdict": "agrees"
         },
         "desc": "the second hand-back one line of at most 200 characters, no absolute path (not applicable without the resume)",
         "detail": "201 characters; absolute paths: []: 'plan: passed, batch of 4 (json-empty, options-parse, options-usage, options-help); json-negative blocked on ask-cycle-3-1, csv-report and json-indent on ask-json-report-2 and ask-json-report-3; 4267fe4'",
         "kind": "decide",
         "pass": false
        },
        "PA-S7": {
         "audit": {
          "agree": true,
          "reason": "Only docs/changes/json-report/design.org changed and nothing is uncommitted, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "nothing changed in the code root's source or test tree",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "PA-E1": "pass",
        "PA-E2": "pass",
        "PA-E3": "pass",
        "PA-E4": "pass",
        "PA-E5": "fail",
        "PA-E6": "pass"
       },
       "judge": {
        "PA-E1": {
         "quotes": [
          "plan: stopped, ask ask-cycle-3-1 (a refund in the JSON report) blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org",
          "plan: stopped, ask ask-cycle-3-1 (a refund in the JSON report) blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org",
          "plan: stopped, ask ask-cycle-3-1 (a refund in the JSON report) blocks json-negative; .orchestrator/changes/json-report/inbox/ask-cycle-3-1.org"
         ],
         "reasons": [
          "The first hand-back says plan stopped and names the new ask, whose file has :KIND: decision and :BLOCKS: json-negative, and the host then sent 'The user is absent.'",
          "The first hand-back says plan stopped and names ask-cycle-3-1, whose file has :KIND: decision and :BLOCKS: json-negative, and the host then sent 'The user is absent.'",
          "The first hand-back says plan stopped and names the new decision ask, whose file has :KIND: decision and :BLOCKS: json-negative; the host then sent 'The user is absent.'"
         ]
        },
        "PA-E2": {
         "quotes": [
          "\"status\": \"blocked\", ... \"blocker_note\": \"ask-cycle-3-1\", ... \"task_name\": \"json-negative\"",
          "\"status\": \"blocked\", ... \"blocker_note\": \"ask-cycle-3-1\", ... \"task_name\": \"json-negative\"",
          "\"status\": \"blocked\", ... \"blocker_note\": \"ask-cycle-3-1\", ... \"task_name\": \"json-negative\""
         ],
         "reasons": [
          "json-negative is blocked on the ask's id, the ask file is still in inbox/ with :STATUS: open and an empty Answer, and json-empty and the options tasks stay ready.",
          "json-negative is blocked on the ask's id, the ask file is still in inbox/ with :STATUS: open and an empty Answer, and json-empty, options-parse, options-usage and options-help stay ready.",
          "json-negative is blocked on the ask's id; inbox/ask-cycle-3-1.org stays :STATUS: open with an empty Answer; json-empty, options-parse, options-usage and options-help go on as ready."
         ]
        },
        "PA-E3": {
         "quotes": [
          "\"plan\": {\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T15:27:43Z\"}",
          "\"plan\": {\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T15:27:43Z\"}",
          "\"passed\": true, \"checks\": {\"prior_integrate_consumed\": true, \"batch_composed\": true, \"briefs_cite_seams\": true, \"user_signed_off_goal_drift\": true}, \"passed_at\": \"2026-11-14T15:27:43Z\""
         ],
         "reasons": [
          "The gate passed with all four checks true and a time; the batch (json-empty, options-parse, options-usage, options-help) leaves out json-negative, and each of its tasks cites seam/json-report or seam/options.",
          "The plan gate passed with all four checks true and a passed_at; the batch (json-empty, options-parse, options-usage, options-help) leaves out json-negative, and each of its tasks cites seam/json-report or seam/options.",
          "The plan gate passed with all four checks true and a time; the batch excludes json-negative, and each batch task cites seam/json-report or seam/options."
         ]
        },
        "PA-E4": {
         "quotes": [
          "4267fe4 Orchestrator: cycle-3 plan closed",
          "4267fe4 Orchestrator: cycle-3 plan closed ... M .orchestrator/changes/json-report/state.json",
          "4267fe4 Orchestrator: cycle-3 plan closed"
         ],
         "reasons": [
          "There is one close commit with the required subject, and its tree holds state.json, the unchanged cycles/cycle-3/premise-check.md and the four batch task files; the other home commit (b6d086f) only commits the ask.",
          "Exactly one home commit has the close subject for cycle-3, and its tree holds state.json, the premise check and the batch's task files; the other home commit is the first turn's ask commit, not a close.",
          "There is one close commit with the required subject; its tree holds state.json, the cycle's premise-check.md and the four batch task files. The other home commit, b6d086f, is the ask's own commit, which also carried the ledger-encoding deletion."
         ]
        },
        "PA-E5": {
         "quotes": [
          "plan: passed, batch of 4 (json-empty, options-parse, options-usage, options-help); json-negative blocked on ask-cycle-3-1, csv-report and json-indent on ask-json-report-2 and ask-json-report-3; 4267fe4",
          "plan: passed, batch of 4 (json-empty, options-parse, options-usage, options-help); json-negative blocked on ask-cycle-3-1, csv-report and json-indent on ask-json-report-2 and ask-json-report-3; 4267fe4",
          "plan: passed, batch of 4 (json-empty, options-parse, options-usage, options-help); json-negative blocked on ask-cycle-3-1, csv-report and json-indent on ask-json-report-2 and ask-json-report-3; 4267fe4"
         ],
         "reasons": [
          "The line names the passed gate and has no absolute path, but it is 201 characters, one over the 200 limit.",
          "The second hand-back is one line naming the outcome, but it is 201 characters, one over the 200 limit.",
          "The line names the outcome and has no absolute path, but it is 201 characters, one over the 200 limit."
         ]
        },
        "PA-E6": {
         "quotes": [
          "M\tdocs/changes/json-report/design.org ... --- uncommitted ---",
          "M\tdocs/changes/json-report/design.org ... --- uncommitted ---",
          "M\tdocs/changes/json-report/design.org ... --- uncommitted ---"
         ],
         "reasons": [
          "The only change on the change branch is the record correction under docs/, and nothing is left uncommitted in the code root.",
          "The only change on the change branch is the record correction under docs/, and nothing is left uncommitted in the code root.",
          "The only change on the change branch is the record correction under docs/, and nothing is left uncommitted in the code root."
         ]
        }
       },
       "milestones": [],
       "n_calls": 22,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "plan-premise's plan phase agent, from the same brief and handover, to its first hand-back (stopped on the decision ask); then resumed by message with the dispatcher's words for an absent user, to its second hand-back",
   "title": "Case plan-premise-absent: resumed with the user absent, plan leaves the blocked task out and passes its gate",
   "trials": 3,
   "validated": "533612b55a393a520b74b776f6c6a4de7afcb29b 20261114T102441 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-refine",
   "items": [
    {
     "checks": [
      {
       "id": "PR-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A cycle starts from the prior cycle's newest handshake",
      "plan § An empty field is not a missing one",
      "record § A later cycle's state names the handshake it started from"
     ],
     "id": "PR-E1",
     "question": "Does the cycle start from the newest handshake, though some of its fields are empty lists?"
    },
    {
     "checks": [
      {
       "id": "PR-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An impact that makes a task's prose false is absorbed in place"
     ],
     "id": "PR-E2",
     "question": "Is the impact that makes a sentence false absorbed in place?"
    },
    {
     "checks": [
      {
       "id": "PR-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Any other impact is appended as an update for the cycle"
     ],
     "id": "PR-E3",
     "question": "Is any other impact appended as an update for the cycle?"
    },
    {
     "checks": [
      {
       "id": "PR-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A wrong-premise impact rewrites the step that made the claim"
     ],
     "id": "PR-E4",
     "question": "Does the wrong-premise impact rewrite the step that made the claim?"
    },
    {
     "checks": [
      {
       "id": "PR-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A task flagged obsolete names the ask that decides it"
     ],
     "id": "PR-E5",
     "question": "Does the task flagged obsolete name the ask that decides it?"
    },
    {
     "checks": [
      {
       "id": "PR-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Each refined task leaves one journal line"
     ],
     "id": "PR-E6",
     "question": "Does each refined task leave one journal line, saying in place or appended?"
    },
    {
     "checks": [
      {
       "id": "PR-S7",
       "kind": "decide"
      },
      {
       "id": "PR-S17",
       "kind": "decide"
      },
      {
       "id": "PR-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § An answered ask is applied before the batch is drafted",
      "asks § An answer naming an option's label is applied without a readback",
      "asks § An applied ask is committed with what applies it"
     ],
     "id": "PR-E7",
     "question": "Is the inbox's answer applied before any task of the batch is drafted?"
    },
    {
     "checks": [
      {
       "id": "PR-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A task carried over after a failure keeps its setting"
     ],
     "id": "PR-E8",
     "question": "Does the task carried after its failure keep its setting?"
    },
    {
     "checks": [
      {
       "id": "PR-S10",
       "kind": "decide"
      },
      {
       "id": "PR-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Any other task is checked against HEAD before the batch is fixed",
      "plan § The premise check runs at the Architect's role default"
     ],
     "id": "PR-E9",
     "question": "Are the carried tasks checked against HEAD, by the premise check at the Architect's role default?"
    },
    {
     "checks": [
      {
       "id": "PR-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § With no goal-drift recommendation, nothing is signed off"
     ],
     "id": "PR-E10",
     "question": "With no goal-drift recommendation, is nothing asked?"
    },
    {
     "checks": [
      {
       "id": "PR-S13",
       "kind": "decide"
      },
      {
       "id": "PR-S14",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor",
      "plan § No agent of a task runs at Haiku",
      "plan § A task's Reviewer is never set below its role default"
     ],
     "id": "PR-E11",
     "question": "Does plan spawn no Implementor, and set no task's agent to Haiku or its Reviewer below its role default?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "ff44329",
     "cites": {
      "PR-E1": [
       "plan § A cycle starts from the prior cycle's newest handshake",
       "plan § An empty field is not a missing one",
       "record § A later cycle's state names the handshake it started from"
      ],
      "PR-E10": [
       "plan § With no goal-drift recommendation, nothing is signed off"
      ],
      "PR-E11": [
       "plan § Plan spawns no Implementor",
       "plan § No agent of a task runs at Haiku",
       "plan § A task's Reviewer is never set below its role default"
      ],
      "PR-E2": [
       "plan § An impact that makes a task's prose false is absorbed in place"
      ],
      "PR-E3": [
       "plan § Any other impact is appended as an update for the cycle"
      ],
      "PR-E4": [
       "plan § A wrong-premise impact rewrites the step that made the claim"
      ],
      "PR-E5": [
       "plan § A task flagged obsolete names the ask that decides it"
      ],
      "PR-E6": [
       "plan § Each refined task leaves one journal line"
      ],
      "PR-E7": [
       "plan § An answered ask is applied before the batch is drafted",
       "asks § An answer naming an option's label is applied without a readback",
       "asks § An applied ask is committed with what applies it"
      ],
      "PR-E8": [
       "plan § A task carried over after a failure keeps its setting"
      ],
      "PR-E9": [
       "plan § Any other task is checked against HEAD before the batch is fixed",
       "plan § The premise check runs at the Architect's role default"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "PR-E1": [
       "PR-S1"
      ],
      "PR-E10": [
       "PR-S12"
      ],
      "PR-E11": [
       "PR-S13",
       "PR-S14"
      ],
      "PR-E2": [
       "PR-S2"
      ],
      "PR-E3": [
       "PR-S3"
      ],
      "PR-E4": [
       "PR-S4"
      ],
      "PR-E5": [
       "PR-S5"
      ],
      "PR-E6": [
       "PR-S6"
      ],
      "PR-E7": [
       "PR-S17",
       "PR-S7",
       "PR-S8"
      ],
      "PR-E8": [
       "PR-S9"
      ],
      "PR-E9": [
       "PR-S10",
       "PR-S11"
      ]
     },
     "items_then": [],
     "run": "20261114T115445",
     "trials": [
      {
       "calls": {
        "11": {
         "error": false,
         "text": "cat tests/test_cli.py; python3 -m unittest discover -s tests -t . 2>&1 | tail -5; python3 <trial>/skills/orchestrate/scripts/coverage.py --help 2>&1 | head -20",
         "tool": "Bash"
        },
        "23": {
         "error": true,
         "text": "Premise check json-report batch",
         "tool": "Agent"
        }
       },
       "checks": {
        "PR-S1": {
         "audit": {
          "agree": true,
          "reason": "The new cycle id and the prior_handshake value match the evidence in state.json.",
          "verdict": "agrees"
         },
         "desc": "a new cycle, its state naming the newest handshake",
         "detail": "new cycle: cycle-1794675315; prior_handshake .orchestrator/changes/json-report/handshake-cycle-2.json; newest in the fixture: handshake-cycle-2.json",
         "kind": "decide",
         "pass": true
        },
        "PR-S10": {
         "audit": {
          "agree": true,
          "reason": "The first Agent call carries model opus and effort medium.",
          "verdict": "agrees"
         },
         "desc": "the premise check spawned at opus, medium",
         "detail": "first spawn's setting: ('opus', 'medium')",
         "kind": "decide",
         "pass": true
        },
        "PR-S11": {
         "audit": {
          "agree": true,
          "reason": "The prompt names all four carried task files.",
          "verdict": "agrees"
         },
         "desc": "the first spawn's input names the refined tasks and the carried one",
         "detail": "not named: none",
         "kind": "decide",
         "pass": true
        },
        "PR-S12": {
         "audit": {
          "agree": true,
          "reason": "No new ask file appears in the home.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "PR-S13": {
         "audit": {
          "agree": true,
          "reason": "The only spawn is the premise check, and its prompt names premise-check.md.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "PR-S14": {
         "audit": {
          "agree": true,
          "reason": "No open task file mentions haiku.",
          "verdict": "agrees"
         },
         "desc": "no task file names haiku",
         "detail": "none",
         "kind": "signal",
         "pass": true
        },
        "PR-S17": {
         "audit": {
          "agree": true,
          "reason": "No home commits exist since the fixture.",
          "verdict": "agrees"
         },
         "desc": "the applied ask committed in the home, the subject naming its id",
         "detail": "none",
         "kind": "decide",
         "pass": false
        },
        "PR-S2": {
         "audit": {
          "agree": true,
          "reason": "json-zero still holds its original summary sentence, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "T1's false sentence gone from its file",
         "detail": "still holds it: True",
         "kind": "decide",
         "pass": false
        },
        "PR-S3": {
         "audit": {
          "agree": true,
          "reason": "json-help's original lines are kept and the file grew with the update stanza.",
          "verdict": "agrees"
         },
         "desc": "T2's text kept whole, and the file longer",
         "detail": "lines of the fixture's file not kept: 0 []; length 473 -> 740",
         "kind": "decide",
         "pass": true
        },
        "PR-S4": {
         "audit": {
          "agree": true,
          "reason": "json-sign's step 1 text is unchanged in the trial's file.",
          "verdict": "agrees"
         },
         "desc": "T3's wrong step gone from its file",
         "detail": "still holds it: True",
         "kind": "decide",
         "pass": false
        },
        "PR-S5": {
         "audit": {
          "agree": true,
          "reason": "json-pretty's stanza names ask-cycle-2-1, the ask whose BLOCKS names it.",
          "verdict": "agrees"
         },
         "desc": "T4's file names the ask that decides it",
         "detail": "the ask blocking json-pretty: ask-cycle-2-1; in its file: True",
         "kind": "decide",
         "pass": true
        },
        "PR-S6": {
         "audit": {
          "agree": true,
          "reason": "Exactly one journal note names each of the four tasks.",
          "verdict": "agrees"
         },
         "desc": "a journal entry against each refined task",
         "detail": "json-zero: 1 json-help: 1 json-sign: 1 json-pretty: 1",
         "kind": "decide",
         "pass": true
        },
        "PR-S7": {
         "audit": {
          "agree": true,
          "reason": "The ask is still in the inbox with STATUS answered and no APPLIED_VIA.",
          "verdict": "agrees"
         },
         "desc": "the answered ask applied: in its cycle's asks directory, applied, naming what applied it",
         "detail": "ask ask-cycle-2-2: at .orchestrator/changes/json-report/inbox/ask-cycle-2-2.org; STATUS=answered APPLIED_VIA=None",
         "kind": "decide",
         "pass": false
        },
        "PR-S8": {
         "audit": {
          "agree": true,
          "reason": "The ask was never applied while task files were written, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "the answer applied before any task file was written or changed",
         "detail": "the ask was not applied (at .orchestrator/changes/json-report/inbox/ask-cycle-2-2.org, STATUS=answered); the first task file written at 1794675379.8956666",
         "kind": "decide",
         "pass": false
        },
        "PR-S9": {
         "audit": {
          "agree": true,
          "reason": "The agent override lines were removed from json-escape's frontmatter.",
          "verdict": "agrees"
         },
         "desc": "the task carried after its failure keeps its setting",
         "detail": "before ['agent:', '  implementor: opus, high', '  reason: it failed once at the role default; the escaping has three cases to test']; now []",
         "kind": "decide",
         "pass": false
        }
       },
       "items": {
        "PR-E1": "pass",
        "PR-E10": "pass",
        "PR-E11": "pass",
        "PR-E2": "fail",
        "PR-E3": "pass",
        "PR-E4": "fail",
        "PR-E5": "pass",
        "PR-E6": "pass",
        "PR-E7": "fail",
        "PR-E8": "fail",
        "PR-E9": "pass"
       },
       "judge": {
        "PR-E1": {
         "quotes": [
          "\"cycle_id\": \"cycle-1794675315\", ... \"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\"",
          "\"cycle_id\": \"cycle-1794675315\" ... \"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\"",
          "\"cycle_id\": \"cycle-1794675315\", ... \"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\""
         ],
         "reasons": [
          "A new cycle was opened and its state names cycle 2's handshake, the newest one, even though several of its fields are empty lists.",
          "A new cycle was opened through `state.py init`, and its state names cycle 2's handshake as the one it started from, even though some of that handshake's fields are empty lists.",
          "A new cycle was opened, and its state names cycle 2's handshake even though some of that handshake's fields are empty lists."
         ]
        },
        "PR-E10": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "No goal-drift question or new ask appears anywhere in the trial.",
          "No new ask file was added and no goal-drift question appears anywhere in the evidence.",
          "No new ask file was added and no goal-drift question was raised."
         ]
        },
        "PR-E11": {
         "quotes": [
          "You are the Architect, running the premise check",
          "\"description\": \"Premise check json-report batch\"",
          "\"description\": \"Premise check json-report batch\""
         ],
         "reasons": [
          "The only spawn is the Architect; no Implementor was spawned, and no task file sets Haiku or lowers a Reviewer.",
          "The only spawn is the premise check, and no task file sets Haiku or lowers a Reviewer's setting.",
          "The only spawn is the premise check, and no task file sets Haiku or lowers a Reviewer."
         ]
        },
        "PR-E2": {
         "quotes": [
          "format_json leaves a category with a zero total out; keep it, written 0.00. ... ### Cited seams amended this cycle",
          "refined json-zero: append (design-diff impact on seam/json-report; the row is unchanged)",
          "format_json leaves a category with a zero total out; keep it, written 0.00. ... \"refined json-zero: append (design-diff impact on seam/json-report; the row is unchanged)\""
         ],
         "reasons": [
          "json-zero's summary sentence is left as it was; the agent only appended an update stanza, and its journal line says \"append\".",
          "json-zero's Summary sentence \"format_json leaves a category with a zero total out\" still stands; the agent appended a stanza instead of rewriting the sentence.",
          "json-zero's Summary sentence is left as it was, and only an appended stanza is added; it was not rewritten in place."
         ]
        },
        "PR-E3": {
         "quotes": [
          "## Cycle 3 updates (cycle-1794675315)\n\n### User-resolved decisions\n- `ask-cycle-2-2`",
          "## Cycle 3 updates (cycle-1794675315)\n\n### User-resolved decisions\n- `ask-cycle-2-2` ...: Yes.",
          "## Cycle 3 updates (cycle-1794675315)\n\n### User-resolved decisions\n- `ask-cycle-2-2`"
         ],
         "reasons": [
          "json-help keeps all of its original text and gains a section of updates for this cycle that carries the ask's impact.",
          "json-help keeps its original text and gains this cycle's update section, which carries the ask's impact.",
          "json-help keeps its original text and gains a cycle-updates section that carries the ask's impact."
         ]
        },
        "PR-E4": {
         "quotes": [
          "1. Call _money from format_json to render each amount. ... **Implication for this task**: step 1 may be dead",
          "1. Call _money from format_json to render each amount.",
          "1. Call _money from format_json to render each amount."
         ],
         "reasons": [
          "The step that made the wrong claim is unchanged; a note was appended and the question was left to the premise check.",
          "The step that made the wrong claim is unchanged; the agent appended a note and left the matter to the premise check.",
          "json-sign's step 1 is unchanged; the agent appended a note and left the matter to the premise check instead of rewriting the step."
         ]
        },
        "PR-E5": {
         "quotes": [
          "### Obsolescence flag\n> Cycle 3 claims this task may now be wholly obsolete ... The ask file `ask-cycle-2-1`",
          "Cycle 3 claims this task may now be wholly obsolete ... The ask file `ask-cycle-2-1` (kind `decision`; `:BLOCKS:` this task",
          "### Obsolescence flag\n> Cycle 3 claims this task may now be wholly obsolete ... The ask file `ask-cycle-2-1`"
         ],
         "reasons": [
          "json-pretty gained an obsolescence stanza that names the open ask deciding it.",
          "json-pretty gains an obsolescence stanza that names the ask which decides it.",
          "json-pretty gains an obsolescence stanza that names the blocking ask, ask-cycle-2-1."
         ]
        },
        "PR-E6": {
         "quotes": [
          "refined json-zero: append ...; refined json-help: append ...; refined json-sign: append ...; refined json-pretty: append ...",
          "refined json-zero: append ... refined json-help: append ... refined json-sign: append ... refined json-pretty: append",
          "refined json-pretty: append ... refined json-help: append ... refined json-sign: append ... refined json-zero: append"
         ],
         "reasons": [
          "Each of the four tasks has exactly one journal line, and each line says whether the change was made in place or appended.",
          "Each of the four tasks has exactly one journal line, and each line says whether its change was made in place or appended.",
          "Each of the four tasks has exactly one journal line, and each line says whether the change was made in place or appended."
         ]
        },
        "PR-E7": {
         "quotes": [
          ":STATUS: answered (file still at inbox/ask-cycle-2-2.org); home commits: (none)",
          ":STATUS: answered (still at inbox/ask-cycle-2-2.org); home commits: (none)",
          "\"status\": \"answered\", ... \"path\": \".orchestrator/changes/json-report/inbox/ask-cycle-2-2.org\""
         ],
         "reasons": [
          "The answered ask was never applied: it is still in the inbox with status answered, has no APPLIED_VIA, was not moved and was not committed, yet task files were changed.",
          "The answered ask was never set to applied, never moved to the cycle's asks directory, and never committed, yet task files were changed anyway.",
          "The answered ask stays in the inbox with status answered, is not moved or committed, and json-help's stanza cites a cycle asks path that does not exist."
         ]
        },
        "PR-E8": {
         "quotes": [
          "refined json-escape: in-place (removed its agent override ...)",
          "refined json-escape: in-place (removed its agent override: its reason cited the cycle-2 failure; a retry keeps its setting)",
          "refined json-escape: in-place (removed its agent override: its reason cited the cycle-2 failure; a retry keeps its setting)"
         ],
         "reasons": [
          "json-escape's agent setting (opus, high) was removed instead of kept.",
          "json-escape's `agent: implementor: opus, high` override was removed from its frontmatter, so it does not keep the setting it had.",
          "json-escape's agent override (opus, high) was removed, so the task does not keep the setting it had."
         ]
        },
        "PR-E9": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"medium\" ... json-escape.md, json-help.md, json-sign.md, json-zero.md",
          "\"model\": \"opus\", \"effort\": \"medium\" ... Candidate task files to check ...: json-escape.md, json-help.md, json-sign.md, json-zero.md",
          "\"model\": \"opus\", \"effort\": \"medium\" ... json-escape.md, json-help.md, json-sign.md, json-zero.md. ... json-pretty.md is out of the batch"
         ],
         "reasons": [
          "The first spawn is the Architect premise check at opus/medium over the carried tasks, with json-pretty left out while its ask is open.",
          "The first Agent call is the Architect premise check at opus/medium over the carried tasks, with json-pretty excluded while its ask is open.",
          "The first spawn is the Architect's premise check at opus/medium, over the carried tasks, with json-pretty left out while its ask is open."
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
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 23,
         "candidates": [
          23
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Premise check json-report batch",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Premise check json-report batch",
         "scenarios": [],
         "sig": "spawn agent json-report"
        }
       ],
       "n_calls": 23,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's third cycle, to its first spawn (the premise check)",
   "title": "Case plan-refine: plan starts from the newest handshake, absorbs each task's impact, applies the answer first, and has the carried tasks checked",
   "trials": 3,
   "validated": "ff443296d8e7f23f28599396370d00d81a86662e 20261114T115445 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-rethought",
   "items": [
    {
     "checks": [
      {
       "id": "RT-S3",
       "kind": "signal"
      },
      {
       "id": "RT-S4",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § After rethink every open task is placed again"
     ],
     "id": "RT-E1",
     "question": "Is every open task placed on or off the main path again, against the new goals?"
    },
    {
     "checks": [
      {
       "id": "RT-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § A cycle starts from the prior cycle's newest handshake",
      "record § A later cycle's state names the handshake it started from"
     ],
     "id": "RT-E2",
     "question": "Does the cycle start from the newest handshake?"
    },
    {
     "checks": [
      {
       "id": "RT-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Any other task is checked against HEAD before the batch is fixed",
      "plan § The premise check runs at the Architect's role default"
     ],
     "id": "RT-E3",
     "question": "Are the carried tasks checked against HEAD, by the premise check at the Architect's role default?"
    },
    {
     "checks": [
      {
       "id": "RT-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor"
     ],
     "id": "RT-E4",
     "question": "Does plan spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "90cd21f",
     "cites": {
      "RT-E1": [
       "plan § After rethink every open task is placed again"
      ],
      "RT-E2": [
       "plan § A cycle starts from the prior cycle's newest handshake",
       "record § A later cycle's state names the handshake it started from"
      ],
      "RT-E3": [
       "plan § Any other task is checked against HEAD before the batch is fixed",
       "plan § The premise check runs at the Architect's role default"
      ],
      "RT-E4": [
       "plan § Plan spawns no Implementor"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "RT-E1": [
       "RT-S3",
       "RT-S4"
      ],
      "RT-E2": [
       "RT-S1"
      ],
      "RT-E3": [
       "RT-S2"
      ],
      "RT-E4": [
       "RT-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T113837",
     "trials": [
      {
       "calls": {
        "18": {
         "error": true,
         "text": "Premise check json-report",
         "tool": "Agent"
        },
        "8": {
         "error": false,
         "text": "cat tests/test_json.py tally/report.py; python3 -m unittest discover -s tests -t . 2>&1 | tail -15; cat <trial>/home/.claude/orchestrator/priors.md; ls <trial>/home/.claude/orchestrator/",
         "tool": "Bash"
        }
       },
       "checks": {
        "RT-S1": {
         "audit": {
          "agree": true,
          "reason": "It compares the new cycle id and prior_handshake with the fixture's newest handshake, which matches the evidence and the item.",
          "verdict": "agrees"
         },
         "desc": "a new cycle started from the newest handshake",
         "detail": "new cycle: cycle-1794674347; prior_handshake .orchestrator/changes/json-report/handshake-cycle-2.json; newest in the fixture: handshake-cycle-2.json",
         "kind": "decide",
         "pass": true
        },
        "RT-S2": {
         "audit": {
          "agree": true,
          "reason": "The spawn's setting is opus/medium, but neither the prompt nor the json-grand-total.md file it names mentions table-align, so the fail fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the premise check spawned at opus, medium, its input naming both open tasks",
         "detail": "first spawn's setting: ('opus', 'medium'); not named: table-align",
         "kind": "decide",
         "pass": false
        },
        "RT-S3": {
         "audit": {
          "agree": true,
          "reason": "json-grand-total.md has \"on_critical_path: true\"; as a signal it checks the line the item expects.",
          "verdict": "agrees"
         },
         "desc": "the task the new goals put on the main path is marked so in its file",
         "detail": "json-grand-total: on_critical_path: true",
         "kind": "signal",
         "pass": true
        },
        "RT-S4": {
         "audit": {
          "agree": true,
          "reason": "table-align.md has \"on_critical_path: false\" after the sed edit; as a signal it checks the line the item expects.",
          "verdict": "agrees"
         },
         "desc": "the task the new goals take off the main path is marked so in its file",
         "detail": "table-align: on_critical_path: false",
         "kind": "signal",
         "pass": true
        },
        "RT-S5": {
         "audit": {
          "agree": true,
          "reason": "The only Agent call names premise-check.md and is an Architect spawn, so its pass correctly reflects that no Implementor was spawned.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RT-E1": "pass",
        "RT-E2": "pass",
        "RT-E3": "fail",
        "RT-E4": "pass"
       },
       "judge": {
        "RT-E1": {
         "quotes": [
          "json-grand-total.md: \"on_critical_path: true\" … \"(intent G1; design D5)\"; table-align.md: \"on_critical_path: false\"; journal: \"on_critical_path false after rethink: intent N2 leaves the column alignment\"",
          "refined table-align: in-place (on_critical_path false after rethink: intent N2 leaves the column alignment; kept open, not batched)",
          "json-grand-total.md: \"on_critical_path: true\" ... table-align.md: \"on_critical_path: false\"; journal: \"on_critical_path false after rethink: intent N2 leaves the column alignment\""
         ],
         "reasons": [
          "Both task files were re-placed, json-grand-total on the main path and table-align off it, and each placement cites the revised intent's goals (G1, N2).",
          "json-grand-total's file now reads on_critical_path: true, citing intent G1 and the new scenario, and table-align's reads on_critical_path: false, citing intent N2; both placements follow the new goals.",
          "Both task files were placed again by the new goals: json-grand-total moved onto the main path (citing intent G1 and the new scenario) and table-align moved off it (citing intent N2)."
         ]
        },
        "RT-E2": {
         "quotes": [
          "\"cycle_id\": \"cycle-1794674347\", … \"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\"",
          "\"cycle_id\": \"cycle-1794674347\", ... \"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\"",
          "\"cycle_id\": \"cycle-1794674347\", ... \"prior_handshake\": \".orchestrator/changes/json-report/handshake-cycle-2.json\""
         ],
         "reasons": [
          "A new cycle was opened, and its state names handshake-cycle-2.json, the fixture's newest handshake.",
          "A new cycle was opened, and its state names the fixture's newest handshake, cycle-2.",
          "A new cycle was opened through state.py init, and its state names the newest handshake, cycle-2's."
         ]
        },
        "RT-E3": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"medium\" … \"Candidate task (the only one): …/tasks/open/json-grand-total.md\"",
          "Candidate task (the only one): `.../tasks/open/json-grand-total.md`",
          "\"Candidate task (the only one): .../tasks/open/json-grand-total.md\""
         ],
         "reasons": [
          "The first spawn is the premise check at opus/medium, but it covers only json-grand-total; table-align was kept open without being batched and is not checked against HEAD.",
          "The first spawn is the premise check at opus/medium, but it covers only json-grand-total; table-align is open and carried, yet it was left out ('kept open, not batched').",
          "The first spawn is the premise check at opus/medium, but it covers only json-grand-total; table-align was left out (\"kept open, not batched\"), so the check is not over both open tasks."
         ]
        },
        "RT-E4": {
         "quotes": [
          "\"You are the Architect, premise-check run\"",
          "\"description\": \"Premise check json-report\"",
          "\"You are the Architect, premise-check run\""
         ],
         "reasons": [
          "The only Agent call is the Architect's premise check, so no Implementor is spawned.",
          "The only Agent call is the Architect premise check, and it says 'do not spawn agents'; no Implementor was spawned.",
          "The only Agent call is the Architect premise check; no Implementor was spawned."
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
         "result": "Ran 8 tests in 0.001s\nOK (expected failures=1)",
         "scenarios": [],
         "sig": "test run after the handover: green"
        },
        {
         "call": 18,
         "candidates": [
          18
         ],
         "checks": [],
         "hits": {},
         "label": "spawn: Premise check json-report",
         "lane": "spawn",
         "linked_by": "its text in the call",
         "match": "Premise check json-report",
         "scenarios": [],
         "sig": "spawn agent json-report"
        }
       ],
       "n_calls": 18,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's next cycle after a rethink, the intent revised and the design round run again, to its first spawn (the premise check)",
   "title": "Case plan-rethought: after rethink and the new goals, plan places every open task on or off the main path again",
   "trials": 3,
   "validated": "90cd21f13e26d68330d4bffb8ef2eba2dcd3fcfa 20261114T113837 t1 (skill-evals)"
  },
  {
   "capability": "plan",
   "id": "plan-unfinished-round",
   "items": [
    {
     "checks": [
      {
       "id": "UR-S1",
       "kind": "decide"
      },
      {
       "id": "UR-S2",
       "kind": "decide"
      },
      {
       "id": "UR-S3",
       "kind": "decide"
      },
      {
       "id": "UR-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan on an unfinished design round opens no cycle"
     ],
     "id": "UR-E1",
     "question": "Does plan open no cycle, draft no task and write no answer into the design?"
    },
    {
     "checks": [
      {
       "id": "UR-S5",
       "kind": "decide"
      },
      {
       "id": "UR-S6",
       "kind": "signal"
      }
     ],
     "cites": [
      "plan § Plan on an unfinished design round opens no cycle"
     ],
     "id": "UR-E2",
     "question": "Does plan hand back failed, naming the change's unfinished design round?"
    },
    {
     "checks": [
      {
       "id": "UR-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "plan § Plan spawns no Implementor"
     ],
     "id": "UR-E3",
     "question": "Does plan spawn no Implementor?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e349279",
     "cites": {
      "UR-E1": [
       "plan § Plan on an unfinished design round opens no cycle"
      ],
      "UR-E2": [
       "plan § Plan on an unfinished design round opens no cycle"
      ],
      "UR-E3": [
       "plan § Plan spawns no Implementor"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "UR-E1": [
       "UR-S1",
       "UR-S2",
       "UR-S3",
       "UR-S4"
      ],
      "UR-E2": [
       "UR-S5",
       "UR-S6"
      ],
      "UR-E3": [
       "UR-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T123512-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "UR-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks for the absence of state.json and of new cycles, which is UR-E1's no-cycle part, and the evidence shows no home changes at all.",
          "verdict": "agrees"
         },
         "desc": "no state file and no cycle directory in the change's directory",
         "detail": "state.json: False; cycles/: none",
         "kind": "decide",
         "pass": true
        },
        "UR-S2": {
         "audit": {
          "agree": true,
          "reason": "It checks that nothing under tasks/ changed, which is UR-E1's no-task part, and the evidence lists no home file changes.",
          "verdict": "agrees"
         },
         "desc": "no task file written",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "UR-S3": {
         "audit": {
          "agree": true,
          "reason": "It checks for no committed or uncommitted change under docs/ on the change branch or in the home, which is UR-E1's design-untouched part, and the evidence shows both empty.",
          "verdict": "agrees"
         },
         "desc": "nothing under docs/ changed: the design holds no answer, the specs and the intent as they were",
         "detail": " | uncommitted:  | home: ",
         "kind": "decide",
         "pass": true
        },
        "UR-S4": {
         "audit": {
          "agree": true,
          "reason": "It checks that both asks are byte-identical in the inbox, which is UR-E1's asks-not-applied-or-moved part, and this matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the round's ask files in the inbox, as the fixture left them",
         "detail": "ask-json-report-1: in the fixture True, now unchanged; ask-json-report-2: in the fixture True, now unchanged",
         "kind": "decide",
         "pass": true
        },
        "UR-S5": {
         "audit": {
          "agree": true,
          "reason": "It checks that the final text holds 'failed', which is the failed half of UR-E2, and the hand-back begins 'plan: failed'.",
          "verdict": "agrees"
         },
         "desc": "the hand-back says failed",
         "detail": "plan: failed, design round unfinished: Open Questions Q1 (amounts as strings or numbers) and Q2 (where `--json` may appear) have no Decision in design.org; the user's answers in `ask-json-report-1` and `ask-json-report-2` have not been written into the design; docs/changes/json-report/design.org",
         "kind": "decide",
         "pass": true
        },
        "UR-S6": {
         "audit": {
          "agree": true,
          "reason": "As a signal for naming the unfinished design round, it fits the hand-back, which says 'design round unfinished' and names the open questions.",
          "verdict": "agrees"
         },
         "desc": "the hand-back names the design round",
         "detail": "plan: failed, design round unfinished: Open Questions Q1 (amounts as strings or numbers) and Q2 (where `--json` may appear) have no Decision in design.org; the user's answers in `ask-json-report-1` and `ask-json-report-2` have not been written into the design; docs/changes/json-report/design.org",
         "kind": "signal",
         "pass": true
        },
        "UR-S7": {
         "audit": {
          "agree": true,
          "reason": "It looks for an Agent call carrying a task Summary, which is the sign of an Implementor spawn, and the evidence shows no Agent calls.",
          "verdict": "agrees"
         },
         "desc": "no Implementor spawned",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "UR-E1": "pass",
        "UR-E2": "pass",
        "UR-E3": "pass"
       },
       "judge": {
        "UR-E1": {
         "quotes": [
          "== Files added, changed or removed in the home since the fixture, each in full as the trial left it ==\n(none)",
          "== Files added, changed or removed in the home since the fixture, each in full as the trial left it ==\n(none)",
          "== Files added, changed or removed in the home since the fixture, each in full as the trial left it ==\n(none)"
         ],
         "reasons": [
          "No commits on the home or the change branch, no docs diff or uncommitted change, and no file added, changed or removed in the home; the only write was a scratch directory outside the home, which the agent later removed, so no state, cycle, task or design answer exists and both asks stay in the inbox unchanged.",
          "The step left no commits on the home or on the change branch, no docs diff and nothing uncommitted, so no state, cycle, task or design edit was made, and both asks are still in the inbox as the fixture had them.",
          "No commits on the home or the change branch, no docs diff or uncommitted changes, and no added files; the only tool calls were reads plus a scratch directory outside the home that was created and then removed, so no state, cycle, task or design edit was made and the asks stayed in the inbox unchanged."
         ]
        },
        "UR-E2": {
         "quotes": [
          "plan: failed, design round unfinished: Open Questions Q1 (amounts as strings or numbers) and Q2 (where `--json` may appear) have no Decision in design.org",
          "plan: failed, design round unfinished: Open Questions Q1 (amounts as strings or numbers) and Q2 (where `--json` may appear) have no Decision in design.org",
          "plan: failed, design round unfinished: Open Questions Q1 (amounts as strings or numbers) and Q2 (where `--json` may appear) have no Decision in design.org"
         ],
         "reasons": [
          "The hand-back says failed and names the unfinished design round of json-report through its two open questions that have no Decision.",
          "The hand-back says failed and names json-report's design round as unfinished, citing the open questions that have no Decision.",
          "The hand-back says failed and names the design round of json-report as unfinished because its open questions have no Decision."
         ]
        },
        "UR-E3": {
         "quotes": [
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "No Agent call was made, so no Implementor was spawned.",
          "No agent of any kind was spawned, so no Implementor was.",
          "No Agent call was made, so no Implementor was spawned."
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
   "step": "the plan phase agent, from the dispatcher's fixed brief for the change's first cycle, to its hand-back",
   "title": "Case plan-unfinished-round: plan on a design round with an open question answered but not written opens no cycle",
   "trials": 3,
   "validated": "e3492799e12c438734eeb5dd2239d44fd06588ce 20261114T123512-1 t1 (skill-evals)"
  }
 ],
 "source": {
  "evals_head": "589177f",
  "spec": "orchestrate/docs/specs/plan.org",
  "spec_dirty": false,
  "worktree_head": "73b676c"
 },
 "spec": {
  "gaps": "No gap open. The seven this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/plan.org= § Gap decisions).",
  "purpose": "The plan phase turns what the change's design still has to build, and what the prior cycle learned, into the batch the cycle's execute phase implements. It starts a cycle only from the handshake the prior cycle's integrate left, and starts none while the change's MVP decision waits for the user. It writes every task file: the tasks drafted from the design's seams and pending scenarios, the follow-up candidates the handshake carries, each open task's impact set, and, after the user chooses to harden, the hardening list's entries. It has the tasks not drafted from the design checked against the code root's HEAD before it fixes the batch, and it marks each task on or off the main path by where the task came from. It writes no code, spawns no Implementor and does not redesign; the premise check's output is the Architect's, the handshake's and the gates' form are the record's, and the handshake itself is integrate's.",
  "requirements": [
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan starts a cycle of a change whose prior cycle has closed"
       },
       {
        "kw": "THEN",
        "text": "the cycle is started from the newest handshake in the change's directory"
       },
       {
        "kw": "AND",
        "text": "the change's record names the handshake the cycle was started from"
       }
      ],
      "test": null,
      "title": "A cycle starts from the prior cycle's newest handshake"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan starts the first cycle of a change"
       },
       {
        "kw": "THEN",
        "text": "the cycle is started without a handshake, and nothing is read in its place"
       }
      ],
      "test": null,
      "title": "A first cycle starts with no handshake"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan runs for a change that has had a cycle, and the prior cycle's handshake is missing or lacks a field its form requires"
       },
       {
        "kw": "THEN",
        "text": "no cycle is started and no task file is written"
       },
       {
        "kw": "AND",
        "text": "the plan phase agent hands back that it stopped"
       }
      ],
      "test": null,
      "title": "Without a whole prior handshake, plan starts no cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan starts no cycle because the prior handshake is missing or lacks a field"
       },
       {
        "kw": "THEN",
        "text": "an =environment= ask in the change's inbox, its id naming the change, asks the user to close the prior cycle properly or abandon it"
       },
       {
        "kw": "AND",
        "text": "the hand-back names the ask"
       }
      ],
      "test": null,
      "title": "A missing handshake is an environment ask filed in the inbox"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the prior handshake holds every required field and some of them are empty lists"
       },
       {
        "kw": "THEN",
        "text": "plan starts the cycle from it"
       }
      ],
      "test": null,
      "title": "An empty field is not a missing one"
     }
    ],
    "text": "The plan phase SHALL start a cycle of a change only from the handshake the change's prior cycle left, holding every field the handshake's form requires, or with no handshake when the change has had no cycle. Without that handshake it SHALL start no cycle and SHALL hand back that it stopped.",
    "title": "A cycle starts only from the prior cycle's handshake"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the handshake gives an open task an impact that makes a sentence of its task file false or dead"
       },
       {
        "kw": "THEN",
        "text": "that sentence is rewritten where it stands, before the batch is fixed"
       }
      ],
      "test": null,
      "title": "An impact that makes a task's prose false is absorbed in place"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the handshake gives an open task an impact that makes no sentence of its task file false"
       },
       {
        "kw": "THEN",
        "text": "the task file gains a section of updates for this cycle that carries the impact"
       }
      ],
      "test": null,
      "title": "Any other impact is appended as an update for the cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an impact says that an open task's premise is wrong"
       },
       {
        "kw": "THEN",
        "text": "the step of the task file that made the claim is rewritten"
       }
      ],
      "test": null,
      "title": "A wrong-premise impact rewrites the step that made the claim"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the handshake flags an open task as possibly obsolete"
       },
       {
        "kw": "THEN",
        "text": "the task file gains a stanza saying so that names the ask the user decides it by"
       }
      ],
      "test": null,
      "title": "A task flagged obsolete names the ask that decides it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an open task's file carries an obsolescence stanza whose ask is unanswered"
       },
       {
        "kw": "THEN",
        "text": "the task is not in the batch"
       }
      ],
      "test": null,
      "title": "A task flagged obsolete is not batched while its ask is open"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan changes an open task's file to absorb its impact set"
       },
       {
        "kw": "THEN",
        "text": "the change's journal has one line against the task, saying whether the change was made in place or appended"
       }
      ],
      "test": null,
      "title": "Each refined task leaves one journal line"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the handshake carries a follow-up candidate that no open task covers"
       },
       {
        "kw": "THEN",
        "text": "a task file named as the candidate names it is drafted from the candidate's quoted words and evidence, where the candidate's placement puts it"
       },
       {
        "kw": "AND",
        "text": "the task file carries the candidate's provenance: its source, who found it and its class"
       },
       {
        "kw": "AND",
        "text": "it is marked on the main path when the candidate is marked critical"
       }
      ],
      "test": null,
      "title": "Each follow-up candidate becomes a task file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task from one of the handshake's meta-discoveries"
       },
       {
        "kw": "THEN",
        "text": "the task file's provenance names the meta-discovery's label"
       }
      ],
      "test": null,
      "title": "A task drafted from a meta-discovery names its label"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task from a follow-up candidate of a defect"
       },
       {
        "kw": "THEN",
        "text": "the first step of its task file is the reproduction"
       }
      ],
      "test": null,
      "title": "A follow-up's task starts with the reproduction"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the handshake carries a follow-up candidate whose work an open task already covers"
       },
       {
        "kw": "THEN",
        "text": "no new task file is written for it, and the open task absorbs it"
       },
       {
        "kw": "AND",
        "text": "the change's journal has one line saying so"
       }
      ],
      "test": null,
      "title": "A candidate an open task covers is absorbed into that task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an open ask file names a task among what it blocks"
       },
       {
        "kw": "THEN",
        "text": "that task is not in the batch until the ask is applied"
       }
      ],
      "test": null,
      "title": "A task an open ask blocks stays out of the batch"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an ask in the change's inbox has an answer when plan starts"
       },
       {
        "kw": "THEN",
        "text": "the answer is applied before any task of the batch is drafted"
       }
      ],
      "test": null,
      "title": "An answered ask is applied before the batch is drafted"
     }
    ],
    "text": "Before it fixes a batch, the plan phase SHALL apply to each open task the impact set the handshake gives it, SHALL turn each follow-up candidate the handshake carries into a task file or absorb it into the open task that covers it, SHALL apply each answer waiting in the inbox, and SHALL keep out of the batch every task an open ask blocks.",
    "title": "What the prior cycle learned is in the batch's task files"
   },
   {
    "goal": "G4",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan runs while the change's MVP decision ask is open"
       },
       {
        "kw": "THEN",
        "text": "no cycle is started and no task file is written"
       },
       {
        "kw": "AND",
        "text": "the plan phase agent hands back that it stopped, naming the ask, that it blocks plan, and the ask file's path"
       }
      ],
      "test": null,
      "title": "An open MVP decision starts no cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change's MVP decision has an answer that integrate did not apply"
       },
       {
        "kw": "THEN",
        "text": "plan applies the answer before it opens a cycle"
       }
      ],
      "test": null,
      "title": "An MVP answer is applied before a cycle is opened"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies the answer =stop= to the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "no cycle is started"
       },
       {
        "kw": "AND",
        "text": "the plan phase agent hands back that the change did not start because it stopped, naming the intent's path"
       }
      ],
      "test": null,
      "title": "A stop answer opens no cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies the answer =stop= to the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "the intent's status line says =done=, with the date and the code root's HEAD"
       },
       {
        "kw": "AND",
        "text": "the intent holds the change's closing summary"
       },
       {
        "kw": "AND",
        "text": "both are committed in the code root in a commit whose subject names the change"
       }
      ],
      "test": null,
      "title": "A stop answer applied at plan marks the change done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies the answer =rethink= to the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "no cycle is started"
       },
       {
        "kw": "AND",
        "text": "the plan phase agent hands back that the change did not start because of a rethink, naming the intent's path"
       }
      ],
      "test": null,
      "title": "A rethink answer opens no cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies the answer =rethink= to the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "the intent's status line says =divergent=, with the date and the ask's id"
       },
       {
        "kw": "AND",
        "text": "the intent's status section quotes the user's answer, with the user as the signal"
       }
      ],
      "test": null,
      "title": "A rethink answer applied at plan marks the intent divergent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies the answer =harden= to the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "plan starts the cycle"
       },
       {
        "kw": "AND",
        "text": "each hardening-list entry the answer covers is drafted into a task, entries that touch the same code sharing one task"
       }
      ],
      "test": null,
      "title": "A harden answer starts the cycle and drafts from the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task from a hardening-list entry"
       },
       {
        "kw": "THEN",
        "text": "the entry names the task"
       },
       {
        "kw": "AND",
        "text": "the task's provenance names the finding, review or discovery the entry refers to"
       }
      ],
      "test": null,
      "title": "A hardening entry names the task drafted from it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies the answer =harden= to the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "the intent's status line still says =mvp="
       }
      ],
      "test": null,
      "title": "A harden answer leaves the intent at its MVP status"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies a =harden= answer that covers only some of the hardening-list entries"
       },
       {
        "kw": "THEN",
        "text": "each entry it does not cover is marked left, naming the ask"
       }
      ],
      "test": null,
      "title": "Entries a partial harden answer leaves are marked left"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan applies a =harden= answer that covers open tasks off the main path"
       },
       {
        "kw": "THEN",
        "text": "those tasks are in the batch of the cycle it starts"
       }
      ],
      "test": null,
      "title": "Open tasks off the main path that a harden answer covers join the batch"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user has not answered =harden= to the change's MVP decision"
       },
       {
        "kw": "THEN",
        "text": "no task of the batch is drafted from the hardening list"
       }
      ],
      "test": null,
      "title": "No task is drafted from the hardening list before harden"
     }
    ],
    "text": "The plan phase SHALL start no cycle while the change's MVP decision is open, and SHALL apply an answer waiting in it before it opens a cycle: on =stop= or =rethink= it SHALL open no cycle; on =harden= it SHALL start the cycle and draft tasks from the hardening list. No task SHALL be drafted from the hardening list before the user chooses to harden.",
    "title": "While the MVP decision is unanswered, nothing runs on the change"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan runs and the change's design has no Seams section, a scenario of the change is uncited, or an open question has no Decision"
       },
       {
        "kw": "THEN",
        "text": "plan opens no cycle, drafts no task and writes no answer into the design"
       },
       {
        "kw": "AND",
        "text": "it hands back =failed=, naming the change's unfinished design round"
       }
      ],
      "test": null,
      "title": "Plan on an unfinished design round opens no cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts tasks from the change's pending scenarios"
       },
       {
        "kw": "THEN",
        "text": "the scenarios are grouped by the seam whose row cites them, one task to a seam or to two closely coupled seams"
       },
       {
        "kw": "AND",
        "text": "no task is drafted for a single test"
       }
      ],
      "test": null,
      "title": "Pending scenarios are drafted one task per seam"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task"
       },
       {
        "kw": "THEN",
        "text": "its task file names the seams it implements and the scenarios it makes pass"
       },
       {
        "kw": "AND",
        "text": "its verification lists the pending tests it promotes"
       }
      ],
      "test": null,
      "title": "Every task cites its seams, its scenarios and the tests it promotes"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "at the change's first plan the change's open tasks include task files a split agent or another tool generated"
       },
       {
        "kw": "THEN",
        "text": "those files are deleted and the batch is drafted from the design"
       },
       {
        "kw": "AND",
        "text": "the change's journal has one line naming the deleted files"
       }
      ],
      "test": null,
      "title": "Generated task files are replaced by tasks drafted from the design"
     }
    ],
    "text": "The plan phase SHALL open no cycle and draft no task while the change's design round has not finished, and SHALL write no answer into the design; it SHALL hand back =failed= naming the round. It SHALL draft the change's pending scenarios into tasks one seam at a time, and each task SHALL cite the seams it implements, the scenarios it makes pass and the pending tests it promotes.",
    "title": "The batch is drafted from a design whose round has finished"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan writes a task file for the change"
       },
       {
        "kw": "THEN",
        "text": "it is =tasks/open/<task>.md= in the change's directory in the home"
       }
      ],
      "test": null,
      "title": "An open task's file is in the change's directory"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan writes a task file"
       },
       {
        "kw": "THEN",
        "text": "it holds a summary of what the task accomplishes"
       },
       {
        "kw": "AND",
        "text": "each seam row it cites, quoted verbatim from the design"
       },
       {
        "kw": "AND",
        "text": "numbered steps, the commands that verify it, and what is deliberately not in it"
       }
      ],
      "test": null,
      "title": "A task file holds its seams verbatim, its steps, its verification and its limits"
     }
    ],
    "text": "Each task file the plan phase writes for the change SHALL be in the change's directory in the home, and SHALL hold the task's summary, each cited seam row verbatim, its steps, its verification and what is out of its scope.",
    "title": "A task file carries what its Implementor works from"
   },
   {
    "goal": "G3",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan has fixed the batch"
       },
       {
        "kw": "THEN",
        "text": "every task of the batch either cites a scenario of the change or carries the provenance of the finding, discovery, ask or hardening-list entry it came from"
       }
      ],
      "test": null,
      "title": "Every task traces to a scenario of the change or to where it came from"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task would add an option, a generalisation or a refactor for a future caller that no goal of the intent needs, and no demonstrated reason supports it (it happened, a prior, the user asked, a measured cost)"
       },
       {
        "kw": "THEN",
        "text": "no task is drafted for it"
       }
      ],
      "test": null,
      "title": "Scope no goal needs is not drafted without a demonstrated reason"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "filing a task in the change would mean the intent has to state a new outcome"
       },
       {
        "kw": "THEN",
        "text": "its task file is in the home's cross-change backlog, marked externalised, and not in the change's open tasks"
       }
      ],
      "test": null,
      "title": "A task outside the change is placed in the cross-change backlog"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan places a task in the cross-change backlog"
       },
       {
        "kw": "THEN",
        "text": "its task file names the source it came from, who found it and its class"
       }
      ],
      "test": null,
      "title": "An externalised task carries its provenance"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the home's cross-change backlog holds tasks and the user has not promoted any into the change"
       },
       {
        "kw": "THEN",
        "text": "no task of the batch is drawn from it"
       }
      ],
      "test": null,
      "title": "Plan does not draw tasks from the cross-change backlog"
     }
    ],
    "text": "The plan phase SHALL draft a task only for the change's main path or a defect's fix, or for scope that a goal needs or a demonstrated reason supports; a task whose work lies outside the change SHALL go to the cross-change backlog with its provenance, and plan SHALL draft nothing from that backlog on its own.",
    "title": "Work enters the batch only for a goal or a cited reason"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "every task of the batch was drafted at this plan from the design's seams and pending scenarios"
       },
       {
        "kw": "THEN",
        "text": "no Architect is spawned"
       },
       {
        "kw": "AND",
        "text": "the cycle has no premise check"
       }
      ],
      "test": null,
      "title": "A task drafted from the design gets no premise check"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task was carried over, or drafted from a follow-up candidate, the hardening list, a bug report or a task the user wrote"
       },
       {
        "kw": "THEN",
        "text": "an Architect checks the task's claims about the code against the code root's HEAD before the batch is fixed"
       },
       {
        "kw": "AND",
        "text": "the cycle's premise check covers that task"
       }
      ],
      "test": null,
      "title": "Any other task is checked against HEAD before the batch is fixed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check finds a claim of a task wrong"
       },
       {
        "kw": "THEN",
        "text": "before the batch is fixed the task's text no longer makes that claim"
       },
       {
        "kw": "AND",
        "text": "the correction is made in the text itself, not as an update stanza"
       }
      ],
      "test": null,
      "title": "A wrong premise is corrected in the task's text before the batch is fixed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the plan phase agent corrects a task for a wrong premise"
       },
       {
        "kw": "THEN",
        "text": "it may rewrite the task or split it, and either is a pass"
       },
       {
        "kw": "AND",
        "text": "the tasks it leaves make no wrong claim"
       }
      ],
      "test": null,
      "title": "Rewrite or split is the plan agent's free choice"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a premise-check finding routed blocking blocks a task of the batch"
       },
       {
        "kw": "THEN",
        "text": "before the batch is fixed its fix is in the batch: a new task, or a rewrite of the task it blocks, the plan agent's free choice"
       }
      ],
      "test": null,
      "title": "A premise finding that blocks a batch task is fixed in the batch's work"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check finds a defect outside every task's scope and it is routed to hardening"
       },
       {
        "kw": "THEN",
        "text": "it is an entry on the change's hardening list at once, so a plan that stops before it closes has it listed"
       }
      ],
      "test": null,
      "title": "An off-batch hardening finding goes on the hardening list at once"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check finds a defect outside every task's scope and it is routed blocking"
       },
       {
        "kw": "THEN",
        "text": "no task file is written for it at this plan"
       },
       {
        "kw": "AND",
        "text": "the change's record holds the finding with its route"
       }
      ],
      "test": null,
      "title": "An off-batch blocking finding waits for integrate's candidate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check notices a speculative need"
       },
       {
        "kw": "THEN",
        "text": "the change's journal has one =thought= line for it, and no task or ask"
       }
      ],
      "test": null,
      "title": "A speculative need the check notices is one thought line"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding of the premise check blocks a task of the batch and raises a decision only the user can make"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask file is written for it"
       },
       {
        "kw": "AND",
        "text": "the task it blocks is not in the batch while the ask is open"
       }
      ],
      "test": null,
      "title": "A decision a premise finding raises keeps its task out of the batch"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an ask raised during plan blocks a task of this batch and is unanswered"
       },
       {
        "kw": "THEN",
        "text": "plan finishes the rest of the batch"
       },
       {
        "kw": "AND",
        "text": "hands back that it stopped, naming the ask"
       }
      ],
      "test": null,
      "title": "A decision ask that blocks a batch task stops plan after the rest is drafted"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan is resumed with word that the user is absent, its batch task still blocked by the ask"
       },
       {
        "kw": "THEN",
        "text": "the task stays out of the batch, blocked on the ask"
       },
       {
        "kw": "AND",
        "text": "plan passes its gate without the answer"
       }
      ],
      "test": null,
      "title": "Resumed with the user absent, plan leaves the blocked task out and passes its gate"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the premise check finds a seam row stale while the code or the test tree is right"
       },
       {
        "kw": "THEN",
        "text": "the plan phase agent fixes the row and commits it in the code root, in a commit whose subject names the row"
       },
       {
        "kw": "AND",
        "text": "the change's journal has one record-correction line against the seam"
       },
       {
        "kw": "AND",
        "text": "no ask is filed for it, and the premise check itself has not edited the design"
       }
      ],
      "test": null,
      "title": "A stale seam row is a record correction, not an ask"
     }
    ],
    "text": "The plan phase SHALL have an Architect check every task not drafted at this plan from the design's seams and pending scenarios against the code root's HEAD before it fixes the batch, and SHALL spawn none when no task needs it. It SHALL correct each wrong premise in the task's text, SHALL route each defect the check finds outside every task's scope, and SHALL make a stale seam row a record correction.",
    "title": "Tasks not drafted from the design are checked against HEAD before the batch is fixed"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task on the main path is ready for the batch, and the change has not reached its MVP decision"
       },
       {
        "kw": "THEN",
        "text": "the batch holds at least one task on the main path"
       }
      ],
      "test": null,
      "title": "A batch holds a main-path task when one is ready"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a batch after the user answered harden on the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "the batch may hold no task on the main path"
       }
      ],
      "test": null,
      "title": "After harden, a batch needs no main-path task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task of the batch must follow other tasks of the batch"
       },
       {
        "kw": "THEN",
        "text": "its task file names those tasks as the ones it is blocked by"
       }
      ],
      "test": null,
      "title": "A task that must follow others names them"
     }
    ],
    "text": "The plan phase SHALL put at least one task on the main path in each batch while such a task is ready, until the change's MVP decision, and SHALL write in each task file the tasks of the batch it must follow.",
    "title": "The batch holds a main-path task and keeps each task's dependencies"
   },
   {
    "goal": "G4",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task from the design's seams and pending scenarios"
       },
       {
        "kw": "THEN",
        "text": "its task file marks it on the main path when a scenario it makes pass is a use a goal names"
       },
       {
        "kw": "AND",
        "text": "off the main path when no goal names any of them"
       }
      ],
      "test": null,
      "title": "A design task is on the main path when a goal names a scenario it makes pass"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task from a follow-up candidate of a finding or discovery"
       },
       {
        "kw": "THEN",
        "text": "its task file marks it on the main path when the defect was routed blocking as a defect on the main path, and off it otherwise"
       }
      ],
      "test": null,
      "title": "A follow-up is on the main path when it was routed blocking on it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a finding or discovery is an untrue sentence"
       },
       {
        "kw": "THEN",
        "text": "plan drafts no task for it"
       }
      ],
      "test": null,
      "title": "An untrue sentence makes no task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task from a decision of the user"
       },
       {
        "kw": "THEN",
        "text": "its task file marks it on the main path when the decision was routed to the main path, and off it otherwise"
       }
      ],
      "test": null,
      "title": "A task from a user's decision is on the main path when the decision was routed there"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan drafts a task from a hardening-list entry after =harden=, from a meta-discovery, or as scope a demonstrated reason supports"
       },
       {
        "kw": "THEN",
        "text": "its task file marks it off the main path"
       }
      ],
      "test": null,
      "title": "Hardening, meta-discovery and scope tasks are off the main path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task's source does not settle whether it is on the main path"
       },
       {
        "kw": "THEN",
        "text": "its task file marks it off the main path"
       },
       {
        "kw": "AND",
        "text": "the change's journal has one line against it naming the doubt"
       }
      ],
      "test": null,
      "title": "A task whose source does not settle it is off the main path, with the doubt journaled"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan runs after the user answered =rethink= and the goals were rewritten"
       },
       {
        "kw": "THEN",
        "text": "every open task is marked on or off the main path again against the new goals"
       }
      ],
      "test": null,
      "title": "After rethink every open task is placed again"
     }
    ],
    "text": "The plan phase SHALL mark each task on or off the change's main path when it adds the task, by the task's source alone: a task drafted from the design by the goals its scenarios serve; a follow-up by its finding's route; a task from a user's decision by that decision's route; a hardening, meta- discovery or scope task off it; and a task whose source does not settle it off it, with the doubt journaled.",
    "title": "A task is placed on or off the main path by where it came from"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan moves a task's Implementor or Reviewer off the role default"
       },
       {
        "kw": "THEN",
        "text": "the task file holds the new setting and a one-line reason a reader can check against the task's text"
       },
       {
        "kw": "AND",
        "text": "the override is written before the batch is fixed"
       }
      ],
      "test": null,
      "title": "A task override names its setting and a reason the task's text bears out"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan has fixed the batch"
       },
       {
        "kw": "THEN",
        "text": "no task's override changes after that point"
       }
      ],
      "test": null,
      "title": "Nothing changes a task override once the batch is fixed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan writes a task override"
       },
       {
        "kw": "THEN",
        "text": "neither the Implementor nor the Reviewer is set to Haiku"
       }
      ],
      "test": null,
      "title": "No agent of a task runs at Haiku"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan writes a task override that names the task's Reviewer"
       },
       {
        "kw": "THEN",
        "text": "the Reviewer's setting is at or above the Reviewer's role default"
       }
      ],
      "test": null,
      "title": "A task's Reviewer is never set below its role default"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task carried over failed, was rejected or was reworked in an earlier cycle"
       },
       {
        "kw": "THEN",
        "text": "its agent setting is the one it had"
       }
      ],
      "test": null,
      "title": "A task carried over after a failure keeps its setting"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan spawns the Architect for the premise check"
       },
       {
        "kw": "THEN",
        "text": "it runs at the Architect's role default for the premise check"
       },
       {
        "kw": "AND",
        "text": "the change's record holds the agent, its setting and that it ran the premise check"
       }
      ],
      "test": null,
      "title": "The premise check runs at the Architect's role default"
     }
    ],
    "text": "The plan phase SHALL leave each task at the role defaults unless it writes a task override in the task file, with the new setting and a one-line reason the task's text bears out, before the batch is fixed; nothing SHALL change the override after. No agent of a task SHALL run at Haiku, a task's Reviewer SHALL never be set below its role default, and a task carried over after a failure, a rejection or a rework SHALL keep its setting. The Architect's premise check SHALL run at its role default.",
    "title": "A task's agent setting moves only on a reason its text gives"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the prior integrate's digest carried a goal-drift recommendation and the handshake holds no resolution of it"
       },
       {
        "kw": "THEN",
        "text": "plan does not pass its gate"
       },
       {
        "kw": "AND",
        "text": "the plan phase agent hands back that it stopped, naming the goal-drift ask"
       }
      ],
      "test": null,
      "title": "An unresolved goal-drift recommendation holds the plan"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the goal-drift ask's answer is exactly one of its options"
       },
       {
        "kw": "THEN",
        "text": "the goal drift counts as signed off"
       }
      ],
      "test": null,
      "title": "An exact option signs off the goal drift"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the goal-drift ask's answer is free text"
       },
       {
        "kw": "THEN",
        "text": "it is read back to the user first"
       },
       {
        "kw": "AND",
        "text": "the goal drift does not count as signed off until the user confirms the readback"
       }
      ],
      "test": null,
      "title": "A free-text goal-drift answer is read back before it signs off"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the prior integrate carried no goal-drift recommendation"
       },
       {
        "kw": "THEN",
        "text": "plan raises no goal-drift question and the sign-off does not hold its gate"
       }
      ],
      "test": null,
      "title": "With no goal-drift recommendation, nothing is signed off"
     }
    ],
    "text": "When the prior integrate carried a goal-drift recommendation the user has not resolved, the plan phase SHALL not pass its gate until the user has answered the goal-drift ask with one of its options; a free-text answer SHALL be read back first. With no recommendation the step SHALL ask nothing.",
    "title": "A goal-drift recommendation is signed off before plan passes"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the plan of a cycle after the first passes its gate"
       },
       {
        "kw": "THEN",
        "text": "the cycle was started from the prior handshake, holding every field its form requires"
       },
       {
        "kw": "AND",
        "text": "the batch is fixed and its list of tasks explicit"
       },
       {
        "kw": "AND",
        "text": "every task of the batch that is not externalised cites at least one seam"
       },
       {
        "kw": "AND",
        "text": "any goal-drift recommendation of the prior integrate has the user's answer"
       }
      ],
      "test": null,
      "title": "Plan passes its gate only with the handshake, the batch, the seams and the sign-off"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan closes"
       },
       {
        "kw": "THEN",
        "text": "one commit in the home, subject =Orchestrator: <cycle-id> plan closed=, holds the batch's task files, the cycle's premise check when there is one, and the cycle's state"
       },
       {
        "kw": "AND",
        "text": "it holds the task files plan wrote in the cross-change backlog, when it wrote any"
       }
      ],
      "test": null,
      "title": "Plan closes with one commit in the home"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan passes its gate, stops on an ask, or does not start because of an MVP answer"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives one line of at most 200 characters"
       },
       {
        "kw": "AND",
        "text": "a path in it is relative to the home"
       }
      ],
      "test": null,
      "title": "Plan hands back one line naming its outcome"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan closes"
       },
       {
        "kw": "THEN",
        "text": "nothing in the code root's source or test tree was changed by plan"
       }
      ],
      "test": null,
      "title": "Plan writes no code"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "plan runs"
       },
       {
        "kw": "THEN",
        "text": "no Implementor is spawned"
       }
      ],
      "test": null,
      "title": "Plan spawns no Implementor"
     }
    ],
    "text": "The plan phase SHALL pass its gate only when the handshake was consumed, the batch is fixed, every task cites a seam and any goal drift is signed off; it SHALL close with one commit in the home and hand the dispatcher one line. It SHALL write no code and spawn no Implementor.",
    "title": "Plan passes its gate, closes with one commit and hands back one line"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); at the hand-off review 2026-11-13: G1, G5, G6, G9 and G10 applied, the obsolete flag carried from =implementor=; the premise corrections filled (a free choice); the main-path rule filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-14 after acceptance (an off-batch hardening finding is listed at once), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning)",
  "title": "plan"
 },
 "summary": {
  "cases": 16,
  "cited": 77,
  "fail": 19,
  "pass": 44,
  "scenarios": 77,
  "title": "plan",
  "tree": "skill-evals",
  "validation": 15
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
