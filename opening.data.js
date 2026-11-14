window.SPEC_PAGE = {
 "capability": "opening",
 "cases": [
  {
   "capability": "opening",
   "id": "opening-backlog",
   "items": [
    {
     "checks": [
      {
       "id": "OB-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § An unnamed change is named for its outcome",
      "opening § A new change's name is new to its repository"
     ],
     "id": "OB-E1",
     "question": "Is the unnamed change named for its outcome, the name new to the repository?"
    },
    {
     "checks": [
      {
       "id": "OB-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Opening reads no code"
     ],
     "id": "OB-E2",
     "question": "Does the opening read no code?"
    },
    {
     "checks": [
      {
       "id": "OB-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A new capability is a new spec file"
     ],
     "id": "OB-E3",
     "question": "Is the new capability a new spec file?"
    },
    {
     "checks": [
      {
       "id": "OB-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A removed behaviour is deleted and its reason given"
     ],
     "id": "OB-E4",
     "question": "Is the dropped behaviour deleted, and its reason given?"
    },
    {
     "checks": [
      {
       "id": "OB-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § One commit holds the intent and the spec edits"
     ],
     "id": "OB-E5",
     "question": "Does one commit hold the intent and the spec edits?"
    },
    {
     "checks": [
      {
       "id": "OB-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § An item of the cross-change backlog taken up is named, and left as it is"
     ],
     "id": "OB-E6",
     "question": "Is each backlog item taken up named by its path, and left as it is?"
    },
    {
     "checks": [
      {
       "id": "OB-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Opening writes no design and no task"
     ],
     "id": "OB-E7",
     "question": "Does the opening write no design and no task?"
    },
    {
     "checks": [
      {
       "id": "OB-S6",
       "kind": "decide"
      },
      {
       "id": "OB-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The hand-back names the change, its counts and the intent"
     ],
     "id": "OB-E8",
     "question": "Does the hand-back name the change, its counts and the intent?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "ce6dcb6",
     "cites": {
      "OB-E1": [
       "opening § An unnamed change is named for its outcome",
       "opening § A new change's name is new to its repository"
      ],
      "OB-E2": [
       "opening § Opening reads no code"
      ],
      "OB-E3": [
       "opening § A new capability is a new spec file"
      ],
      "OB-E4": [
       "opening § A removed behaviour is deleted and its reason given"
      ],
      "OB-E5": [
       "opening § One commit holds the intent and the spec edits"
      ],
      "OB-E6": [
       "opening § An item of the cross-change backlog taken up is named, and left as it is"
      ],
      "OB-E7": [
       "opening § Opening writes no design and no task"
      ],
      "OB-E8": [
       "opening § The hand-back names the change, its counts and the intent"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "OB-E1": [
       "OB-S1"
      ],
      "OB-E2": [
       "OB-S9"
      ],
      "OB-E3": [
       "OB-S2"
      ],
      "OB-E4": [
       "OB-S3"
      ],
      "OB-E5": [
       "OB-S5"
      ],
      "OB-E6": [
       "OB-S4"
      ],
      "OB-E7": [
       "OB-S8"
      ],
      "OB-E8": [
       "OB-S6",
       "OB-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T112828",
     "trials": [
      {
       "calls": {},
       "checks": {
        "OB-S1": {
         "audit": {
          "agree": true,
          "reason": "It observes the single Open: commit, the kebab-case name and the change directory's absence at the fixture, which is the mechanical part of OB-E1 and fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "one Open: <change> commit, the name kebab-case, its docs/changes/<change>/ new to the repository",
         "detail": "tally: 55a56011f0 Open: ledger-check-and-order",
         "kind": "decide",
         "pass": true
        },
        "OB-S2": {
         "audit": {
          "agree": true,
          "reason": "The added docs/specs/ledger-check.org is the new capability's spec file.",
          "verdict": "agrees"
         },
         "desc": "a new spec file under docs/specs/",
         "detail": "added: ['docs/specs/ledger-check.org']",
         "kind": "decide",
         "pass": true
        },
        "OB-S3": {
         "audit": {
          "agree": true,
          "reason": "The gone title 'Categories are ordered by total' is the dropped scenario, and it appears in Removals with its reason.",
          "verdict": "agrees"
         },
         "desc": "a requirement or scenario deleted, and each one deleted named under the intent's Removals",
         "detail": "gone: ['Categories are ordered by total']; Removals: - reporting § Categories are ordered by total: the users sort the table in their spreadsheet anyway and want the categories in the order they wrote them; one who relied on largest-first sorts the table by its total column there.\n\n",
         "kind": "decide",
         "pass": true
        },
        "OB-S4": {
         "audit": {
          "agree": true,
          "reason": "Both paths are in Why, and the home diff shows neither item file changed.",
          "verdict": "agrees"
         },
         "desc": "the Why names each item by its path, and each item's file unchanged",
         "detail": "items ['.tasks/ledger-check.md', '.tasks/ledger-order.md']; changed: []; Why: Two items set aside during the ledger-reading work, taken up together:\n\n- =.tasks/ledger-check.md=: \"=tally check FILE= reads a ledger file and names each malformed line by its line number (a missing field, an amount that  … [cut, 478 characters]",
         "kind": "decide",
         "pass": true
        },
        "OB-S5": {
         "audit": {
          "agree": true,
          "reason": "The commit's file list is exactly the intent plus docs/specs/*.org files, as the item expects.",
          "verdict": "agrees"
         },
         "desc": "the commit holds the intent and spec files only",
         "detail": "['docs/changes/ledger-check-and-order/intent.org', 'docs/specs/ledger-check.org', 'docs/specs/reporting.org']",
         "kind": "decide",
         "pass": true
        },
        "OB-S6": {
         "audit": {
          "agree": true,
          "reason": "The final text is one 162-character line in the form, ending in the path through the change's worktree.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is the opening's line for the change, its path the intent's relative to the home",
         "detail": "162 characters: open ledger-check-and-order: 2 capabilities, 8 scenarios, 1 open questions; .worktrees/tally/ledger-check-and-order/docs/changes/ledger-check-and-order/intent.org | paths that would do: ['.worktrees/tally/ledger-check-and-order/docs/changes/ledger-check-and-order/intent.org']",
         "kind": "decide",
         "pass": true
        },
        "OB-S7": {
         "audit": {
          "agree": true,
          "reason": "The 2/8/1 counts match the intent's capabilities, the commit's added or changed scenarios, and the open questions.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the intent's",
         "detail": "line 2/8/1; intent: 2 capabilities ['ledger-check', 'reporting'], 8 scenarios ['ledger-check § A line missing a field is named', 'ledger-check § A refund is not malformed', 'ledger-check § A well-formed ledger passes', 'ledger-check § A whole-dollar amount without cents is named', 'ledger-check § An … [cut, 522 characters]",
         "kind": "decide",
         "pass": true
        },
        "OB-S8": {
         "audit": {
          "agree": true,
          "reason": "There is no design.org in the commit and no tasks/ or cycles/ file in the home.",
          "verdict": "agrees"
         },
         "desc": "no design.org and no task file",
         "detail": "design: absent; tasks: []",
         "kind": "decide",
         "pass": true
        },
        "OB-S9": {
         "audit": {
          "agree": true,
          "reason": "The main-thread calls touch only docs, config, skill files and git metadata, with no read under tally/ or tests/ and no repo-wide Grep, so pass fits.",
          "verdict": "agrees"
         },
         "desc": "no file of the code read",
         "detail": "",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "OB-E1": "pass",
        "OB-E2": "pass",
        "OB-E3": "pass",
        "OB-E4": "pass",
        "OB-E5": "pass",
        "OB-E6": "pass",
        "OB-E7": "pass",
        "OB-E8": "pass"
       },
       "judge": {
        "OB-E1": {
         "quotes": [
          "state.py change open ledger-check-and-order",
          "state.py change open ledger-check-and-order … \"created\": true",
          "state.py change open ledger-check-and-order … \"created\": true"
         ],
         "reasons": [
          "The name is a kebab-case phrase for the outcome, a ledger check and a change to the ledger's ordering, and the change was newly created (\"created\": true) with no prior docs/changes/ledger-check-and-order/.",
          "The kebab-case name names the outcome, a ledger check plus the reordered report, and the earlier `ls -R repos/tally/docs` showed only `glossary.org` and `specs`, so `docs/changes/ledger-check-and-order/` is new.",
          "The kebab-case name covers both outcomes the change is for: ledger lines checked and categories in ledger order. The repository's docs held only glossary.org and specs at the fixture, so docs/changes/ledger-check-and-order/ is new."
         ]
        },
        "OB-E2": {
         "quotes": [
          "cat repos/tally/docs/specs/reporting.org; ... cat repos/tally/docs/glossary.org",
          "cat repos/tally/docs/specs/reporting.org; … cat repos/tally/docs/glossary.org",
          "cat repos/tally/docs/specs/reporting.org; echo ----; cat repos/tally/docs/glossary.org"
         ],
         "reasons": [
          "Every read was of the task items, specs, glossary, config, skill files or git metadata; no file of tally's code or tests was read.",
          "Every read was of docs, specs, templates, the config or git log, and no file under the repository's code or tests was opened.",
          "Every read is of the tasks, docs, specs, templates, config or git log, and no source or test file of tally is read."
         ]
        },
        "OB-E3": {
         "quotes": [
          "new file mode 100644 ... docs/specs/ledger-check.org",
          "new file mode 100644 … docs/specs/ledger-check.org",
          "new file mode 100644 … docs/specs/ledger-check.org"
         ],
         "reasons": [
          "The check capability (`tally check FILE`) was written as a new spec file, not folded into reporting.",
          "The `tally check FILE` capability went into a new spec file, `docs/specs/ledger-check.org`, rather than into `reporting`.",
          "A new spec ledger-check.org holds the =tally check FILE= capability, with a Purpose separate from reporting's."
         ]
        },
        "OB-E4": {
         "quotes": [
          "-*** Scenario: Categories are ordered by total ... * Removals\n- reporting § Categories are ordered by total: the users sort the table in their spreadsheet anyway and want the categories in the order they wrote them",
          "- reporting § Categories are ordered by total: the users sort the table in their spreadsheet anyway and want the categories in the order they wrote them",
          "-*** Scenario: Categories are ordered by total … * Removals\n- reporting § Categories are ordered by total: the users sort the table in their spreadsheet anyway"
         ],
         "reasons": [
          "The scenario is deleted from reporting.org (its requirement reworded to ledger order), and Removals names it with the item's reason.",
          "The scenario is deleted from `reporting.org` and its requirement text rewritten, and Removals names it with the item's reason.",
          "The scenario is deleted from reporting.org, and Removals names it with the item's reason (users sort in their spreadsheet and want ledger order)."
         ]
        },
        "OB-E5": {
         "quotes": [
          "docs/changes/ledger-check-and-order/intent.org\ndocs/specs/ledger-check.org\ndocs/specs/reporting.org",
          "55a5601 Open: ledger-check-and-order … docs/changes/ledger-check-and-order/intent.org docs/specs/ledger-check.org docs/specs/reporting.org",
          "docs/changes/ledger-check-and-order/intent.org\ndocs/specs/ledger-check.org\ndocs/specs/reporting.org"
         ],
         "reasons": [
          "The single Open: commit holds the intent and the two spec files and nothing else.",
          "One `Open:` commit holds the intent and the two spec files and nothing else.",
          "One Open: ledger-check-and-order commit holds the intent and the two spec files and nothing else."
         ]
        },
        "OB-E6": {
         "quotes": [
          "- =.tasks/ledger-check.md=: ... - =.tasks/ledger-order.md=:",
          "- =.tasks/ledger-check.md=: … - =.tasks/ledger-order.md=:",
          "- =.tasks/ledger-check.md=: … - =.tasks/ledger-order.md=:"
         ],
         "reasons": [
          "Why names both items by path, and the only home file changed outside the repos is change.json, so neither item was touched.",
          "The Why names both item paths, and the home's only changed file is `change.json`, so both item files are untouched.",
          "Why names both items by path, and the only home file added or changed is change.json, so neither task file was touched."
         ]
        },
        "OB-E7": {
         "quotes": [
          "docs/changes/ledger-check-and-order/intent.org",
          "docs/changes/ledger-check-and-order/intent.org",
          "design: absent; tasks: []"
         ],
         "reasons": [
          "The commit adds no design.org, and no task file was added in the home.",
          "The commit adds no `design.org`, and no task file was added in the home or the repository.",
          "The commit has no design.org, and no task file was written anywhere."
         ]
        },
        "OB-E8": {
         "quotes": [
          "open ledger-check-and-order: 2 capabilities, 8 scenarios, 1 open questions; .worktrees/tally/ledger-check-and-order/docs/changes/ledger-check-and-order/intent.org",
          "open ledger-check-and-order: 2 capabilities, 8 scenarios, 1 open questions; .worktrees/tally/ledger-check-and-order/docs/changes/ledger-check-and-order/intent.org",
          "open ledger-check-and-order: 2 capabilities, 8 scenarios, 1 open questions; .worktrees/tally/ledger-check-and-order/docs/changes/ledger-check-and-order/intent.org"
         ],
         "reasons": [
          "One line in the required form, under 200 characters; the path runs from the home through the worktree, and the counts match the intent's two capabilities, eight added or changed scenarios and one open question.",
          "The hand-back is one 162-character line in the required form, its path runs from the home through the worktree, and its counts match the intent's 2 capabilities, 8 scenarios (7 new, 1 changed) and 1 open question.",
          "One 162-character line in the form, with the path from the home through the worktree. Its counts match the intent: 2 capabilities, 7 new plus 1 changed scenario, and 1 question."
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
   "step": "the opening phase agent, from its brief (the source two paths of the cross-change backlog), to its hand-back",
   "title": "Case opening-backlog: the opening from two backlog items, no name given; a new capability and a removal",
   "trials": 3,
   "validated": "ce6dcb6e13272f7a4304285e44d1236f2ca1883a 20261114T112828 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-by-hand",
   "items": [
    {
     "checks": [
      {
       "id": "BH-S1",
       "kind": "decide"
      },
      {
       "id": "BH-S2",
       "kind": "decide"
      },
      {
       "id": "BH-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A change the user wrote is left as written"
     ],
     "id": "BH-E1",
     "question": "Is the change the user wrote left as written?"
    },
    {
     "checks": [
      {
       "id": "BH-S3",
       "kind": "decide"
      },
      {
       "id": "BH-S2",
       "kind": "decide"
      },
      {
       "id": "BH-S4",
       "kind": "decide"
      },
      {
       "id": "BH-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A change opened by hand is adopted as it is"
     ],
     "id": "BH-E2",
     "question": "Is the change opened by hand adopted as it is, its branch the integration branch?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "BH-E1": [
       "opening § A change the user wrote is left as written"
      ],
      "BH-E2": [
       "opening § A change opened by hand is adopted as it is"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "BH-E1": [
       "BH-S1",
       "BH-S2",
       "BH-S4"
      ],
      "BH-E2": [
       "BH-S2",
       "BH-S3",
       "BH-S4",
       "BH-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T101853-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "BH-S1": {
         "audit": {
          "agree": true,
          "reason": "No commit was made since the fixture, so no new `Open:` commit exists; the user's pre-existing one is correctly not counted.",
          "verdict": "agrees"
         },
         "desc": "no Open: commit made",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "BH-S2": {
         "audit": {
          "agree": true,
          "reason": "The end refs show `json-report` at a866a2cd..., the same commit as at the fixture.",
          "verdict": "agrees"
         },
         "desc": "the change's branch where the user left it",
         "detail": "a866a2cd567f (fixture a866a2cd567f)",
         "kind": "decide",
         "pass": true
        },
        "BH-S3": {
         "audit": {
          "agree": true,
          "reason": "change.json is written and its `branch` is `json-report`, as shown in full in the evidence.",
          "verdict": "agrees"
         },
         "desc": "change.json, if written, names the branch json-report as the change's",
         "detail": "{\"change\": \"json-report\", \"repo\": \"home\", \"branch\": \"json-report\", \"worktree\": \"<trial>/home/.worktrees/json-report\", \"base\": \"5a7e127a2a39b2991f0cb7b26e7959ef8456a430\", \"from\": \"main\", \"opened_at\": \"2026-11-14T15:19:07Z\", \"adopted\": true}",
         "kind": "decide",
         "pass": true
        },
        "BH-S4": {
         "audit": {
          "agree": true,
          "reason": "The home's only untracked entry is `.orchestrator/` (the record), and the agent wrote nothing into the worktree, so an empty observation fits.",
          "verdict": "agrees"
         },
         "desc": "nothing uncommitted in the repository or its worktrees, the home's record aside (no intent or spec edit written)",
         "detail": "{}",
         "kind": "decide",
         "pass": true
        },
        "BH-S5": {
         "audit": {
          "agree": true,
          "reason": "The final text is exactly `adopt json-report; .worktrees/json-report/docs/changes/json-report/intent.org`, which matches the record's worktree path relative to the home.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is adopt json-report; <the intent's path relative to the home, through its code root>",
         "detail": "77 characters: adopt json-report; .worktrees/json-report/docs/changes/json-report/intent.org | paths that would do: ['.worktrees/json-report/docs/changes/json-report/intent.org']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "BH-E1": "pass",
        "BH-E2": "pass"
       },
       "judge": {
        "BH-E1": {
         "quotes": [
          "Repository home: commits since the fixture on any branch (subject, body, files) ==\n(none)",
          "Repository home: commits since the fixture on any branch (subject, body, files) == (none)",
          "Repository home: commits since the fixture on any branch (subject, body, files) ==\n(none)"
         ],
         "reasons": [
          "The agent made no commit; `json-report` still names a866a2c, so the user's own `Open: json-report` commit, its intent and its spec edits are unchanged.",
          "The agent made no commits, and `json-report` still names `a866a2c`, so the existing `Open: json-report` commit, intent and spec edits are the user's own and remain as written.",
          "The only `Open: json-report` commit is the user's own `a866a2c`, already present at the fixture; no new commit was made and `json-report` still names `a866a2c`, so the intent and spec edits are untouched."
         ]
        },
        "BH-E2": {
         "quotes": [
          "adopt json-report; .worktrees/json-report/docs/changes/json-report/intent.org",
          "adopt json-report; .worktrees/json-report/docs/changes/json-report/intent.org",
          "\"branch\": \"json-report\", ... \"adopted\": true | adopt json-report; .worktrees/json-report/docs/changes/json-report/intent.org"
         ],
         "reasons": [
          "The record names branch `json-report` with `adopted: true`, no file in the branch was edited (`--from main` resolved the base mismatch without touching the intent), and the hand-back is the single required line with the intent's path relative to the home.",
          "The record names `\"branch\": \"json-report\"` with `\"adopted\": true`, no intent or spec was written (no commits; only `.orchestrator/` untracked), and the hand-back is the one required line with the intent's path relative to the home.",
          "The record names `json-report` as the change's branch and marks it adopted, no intent or spec file was written, and the hand-back is the single required line with the intent's path relative to the home."
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
   "step": "the opening phase agent, from its brief (the source the user's words), to its hand-back",
   "title": "Case opening-by-hand: a change the user wrote on its own branch is adopted as it is",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T101853-1 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-covered",
   "items": [
    {
     "checks": [
      {
       "id": "OC-S1",
       "kind": "decide"
      },
      {
       "id": "OC-S2",
       "kind": "decide"
      },
      {
       "id": "OC-S3",
       "kind": "decide"
      },
      {
       "id": "OC-S4",
       "kind": "signal"
      },
      {
       "id": "OC-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Work an open change's task covers opens no change"
     ],
     "id": "OC-E1",
     "question": "Is no change opened for work an open change's task already covers?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "OC-E1": [
       "opening § Work an open change's task covers opens no change"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "OC-E1": [
       "OC-S1",
       "OC-S2",
       "OC-S3",
       "OC-S4",
       "OC-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T102659",
     "trials": [
      {
       "calls": {},
       "checks": {
        "OC-S1": {
         "audit": {
          "agree": true,
          "reason": "The refs and worktree lists at the fixture and at the end are identical, so no branch or worktree was added or moved, as the item requires.",
          "verdict": "agrees"
         },
         "desc": "no branch and no worktree added to the code repository",
         "detail": "refs added or moved: none; worktrees added: none",
         "kind": "decide",
         "pass": true
        },
        "OC-S2": {
         "audit": {
          "agree": false,
          "reason": "The evidence shows no commits since the fixture on any branch, so there is no Open: commit and no intent committed.",
          "verdict": "agrees"
         },
         "desc": "no Open: commit and no intent committed on any branch since the fixture",
         "detail": "Open: commits: none; intents committed: none",
         "kind": "decide",
         "pass": true
        },
        "OC-S3": {
         "audit": {
          "agree": false,
          "reason": "No home files were added, and the change directories remain json-report alone, matching the item's 'still json-report alone'.",
          "verdict": "agrees"
         },
         "desc": "the home's changes are still json-report alone",
         "detail": "now: ['json-report']; at the fixture: ['json-report']",
         "kind": "decide",
         "pass": true
        },
        "OC-S4": {
         "audit": {
          "agree": false,
          "reason": "The final text names dollar-table; as a signal it fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the hand-back names the task that covers the work",
         "detail": "open: covered by json-report, task dollar-table; .orchestrator/changes/json-report/tasks/open/dollar-table.md",
         "kind": "signal",
         "pass": true
        },
        "OC-S5": {
         "audit": {
          "agree": false,
          "reason": "The final text is one 109-character line in exactly the required form, with the task path relative to the home matching the task file under tasks/open/.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is open: covered by json-report, task dollar-table; <its task file relative to the home>",
         "detail": "109 characters: open: covered by json-report, task dollar-table; .orchestrator/changes/json-report/tasks/open/dollar-table.md | the task file relative to the home: .orchestrator/changes/json-report/tasks/open/dollar-table.md",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "OC-E1": "pass"
       },
       "judge": {
        "OC-E1": {
         "quotes": [
          "open: covered by json-report, task dollar-table; .orchestrator/changes/json-report/tasks/open/dollar-table.md",
          "open: covered by json-report, task dollar-table; .orchestrator/changes/json-report/tasks/open/dollar-table.md",
          "open: covered by json-report, task dollar-table; .orchestrator/changes/json-report/tasks/open/dollar-table.md"
         ],
         "reasons": [
          "Refs, worktrees, home files and change directories are unchanged with no commits anywhere, and the hand-back is the exact one line naming the task file relative to the home.",
          "Refs, worktrees, home commits and home files are unchanged from the fixture, no Open: commit or intent was made, json-report is still the only change, and the hand-back is the one expected line with the task file's path relative to the home.",
          "Refs, worktrees and home files are unchanged and there are no commits, so no new change was opened; the hand-back is the required single line with the task file path relative to the home."
         ]
        }
       },
       "milestones": [],
       "n_calls": 5,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the opening phase agent, from its brief (the source the user's words), to its hand-back",
   "title": "Case opening-covered: the user asks for work an open change's task already covers, and no change is opened",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T102659 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-name-taken",
   "items": [
    {
     "checks": [
      {
       "id": "NT-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A name the user gave that is taken fails the opening",
      "opening § A failed opening says why"
     ],
     "id": "NT-E1",
     "question": "Does the opening hand back that the name the user gave is taken?"
    },
    {
     "checks": [
      {
       "id": "NT-S2",
       "kind": "decide"
      },
      {
       "id": "NT-S3",
       "kind": "decide"
      },
      {
       "id": "NT-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A name the user gave that is taken fails the opening",
      "opening § A failed opening leaves nothing behind"
     ],
     "id": "NT-E2",
     "question": "Does the failed opening create no branch and leave nothing behind?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "b770aec",
     "cites": {
      "NT-E1": [
       "opening § A name the user gave that is taken fails the opening",
       "opening § A failed opening says why"
      ],
      "NT-E2": [
       "opening § A name the user gave that is taken fails the opening",
       "opening § A failed opening leaves nothing behind"
      ]
     },
     "commit": "db6ad9d",
     "item_checks": {
      "NT-E1": [
       "NT-S1"
      ],
      "NT-E2": [
       "NT-S2",
       "NT-S3",
       "NT-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T001024",
     "trials": [
      {
       "calls": {},
       "checks": {
        "NT-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks the final text against the exact one-line hand-back the item expects, and the evidence shows that line.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is open: failed, name taken: json-report",
         "detail": "37 characters: open: failed, name taken: json-report",
         "kind": "decide",
         "pass": true
        },
        "NT-S2": {
         "audit": {
          "agree": true,
          "reason": "Comparing every ref and the worktree list against the fixture would catch any branch or worktree the opening left, and the evidence shows them unchanged.",
          "verdict": "agrees"
         },
         "desc": "the repository's branches and worktrees as the fixture left them",
         "detail": "refs now:\nrefs/heads/main a866a2cd567f24f3659952ab6ccf4b5bd2d847fc\nrefs at the fixture:\nrefs/heads/main a866a2cd567f24f3659952ab6ccf4b5bd2d847fc\nworktrees now:\nworktree <trial>/home\nHEAD a866a2cd567f24f3659952ab6ccf4b5bd2d847fc\nbranch refs/heads/main\nat the fixtu … [cut, 431 characters]",
         "kind": "decide",
         "pass": true
        },
        "NT-S3": {
         "audit": {
          "agree": true,
          "reason": "An unchanged HEAD and an empty porcelain status (untracked files included) show that nothing was committed on the checked-out branch or left uncommitted, which the evidence confirms.",
          "verdict": "agrees"
         },
         "desc": "HEAD unchanged and nothing uncommitted in the repository",
         "detail": "HEAD a866a2cd567f24f3659952ab6ccf4b5bd2d847fc (fixture a866a2cd567f24f3659952ab6ccf4b5bd2d847fc); status: clean",
         "kind": "decide",
         "pass": true
        },
        "NT-S4": {
         "audit": {
          "agree": true,
          "reason": "The absence of change.json and the home-wide snapshot diff, ignored files included, observe that no change record or leftover file exists, which fits the evidence of no files changed.",
          "verdict": "agrees"
         },
         "desc": "no change.json and no file added, changed or removed in the home (ignored files included)",
         "detail": "change.json: False; files: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "NT-E1": "pass",
        "NT-E2": "pass"
       },
       "judge": {
        "NT-E1": {
         "quotes": [
          "open: failed, name taken: json-report",
          "open: failed, name taken: json-report",
          "open: failed, name taken: json-report"
         ],
         "reasons": [
          "The final text is exactly the required one line, and no tool call opens the change under any other name.",
          "The hand-back is exactly the expected one line, and no tool call opens the change under any other name.",
          "The hand-back is exactly the required one line, and no tool call opened the change under any other name."
         ]
        },
        "NT-E2": {
         "quotes": [
          "refs/heads/main a866a2cd567f24f3659952ab6ccf4b5bd2d847fc ... git status at the end: (none) ... Commits on any branch since the fixture: (none)",
          "Commits on any branch since the fixture == (none)",
          "refs/heads/main a866a2cd567f24f3659952ab6ccf4b5bd2d847fc ... git status at the end (untracked files listed) ==\n(none)"
         ],
         "reasons": [
          "The refs and worktrees match the fixture, the status is clean, no home files were added or changed, and the scratch directory the agent made was removed with rmdir before the hand-back.",
          "Refs and worktrees match the fixture, git status is clean, no home file was added, changed or removed, and the scratch directory the agent made was removed with `rmdir` before the hand-back.",
          "Refs and worktrees match the fixture, no files were added, changed or removed in the home, and no commits were made; the only thing the agent wrote was the scratch directory outside the home, which it removed with `rmdir` before the hand-back."
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
   "step": "the opening phase agent, from its brief (the source the user's words), to its hand-back",
   "title": "Case opening-name-taken: the user names a change the repository already has, and the opening fails clean",
   "trials": 3,
   "validated": "b770aec0f26a759b9d86ca6f2c3fd3aa1ae229a3 20261114T001024 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-research",
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
      }
     ],
     "cites": [
      "opening § A change after research starts from the research's workspace integration branch",
      "opening § The intent carries the change's base"
     ],
     "id": "RS-E1",
     "question": "Does the change's integration branch start from the research's workspace integration branch, and the intent carry that base?"
    },
    {
     "checks": [
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
      "opening § A change from a research hand-off names the hand-off's scenarios"
     ],
     "id": "RS-E2",
     "question": "Does the intent name each of the hand-off's scenarios, and await the user's approval?"
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
       "id": "RS-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "opening § One commit holds the intent and the spec edits",
      "opening § Opening commits nothing in the home"
     ],
     "id": "RS-E3",
     "question": "Does one commit in the change's code root hold the intent and the spec files, and none land in the home?"
    },
    {
     "checks": [
      {
       "id": "RS-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The intent is written with status speculated"
     ],
     "id": "RS-E4",
     "question": "Is the intent written with status speculated?"
    },
    {
     "checks": [
      {
       "id": "RS-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Opening reads no code"
     ],
     "id": "RS-E5",
     "question": "Does the opening read no code?"
    },
    {
     "checks": [
      {
       "id": "RS-S9",
       "kind": "decide"
      },
      {
       "id": "RS-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The hand-back names the change, its counts and the intent",
      "opening § The hand-back's intent path is relative to the home",
      "opening § The hand-back's counts are the intent's"
     ],
     "id": "RS-E6",
     "question": "Does the hand-back name the change, its counts and the intent, the path relative to the home?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "9235569",
     "cites": {
      "RS-E1": [
       "opening § A change after research starts from the research's workspace integration branch",
       "opening § The intent carries the change's base"
      ],
      "RS-E2": [
       "opening § A change from a research hand-off names the hand-off's scenarios"
      ],
      "RS-E3": [
       "opening § One commit holds the intent and the spec edits",
       "opening § Opening commits nothing in the home"
      ],
      "RS-E4": [
       "opening § The intent is written with status speculated"
      ],
      "RS-E5": [
       "opening § Opening reads no code"
      ],
      "RS-E6": [
       "opening § The hand-back names the change, its counts and the intent",
       "opening § The hand-back's intent path is relative to the home",
       "opening § The hand-back's counts are the intent's"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RS-E1": [
       "RS-S1",
       "RS-S2"
      ],
      "RS-E2": [
       "RS-S3",
       "RS-S4"
      ],
      "RS-E3": [
       "RS-S11",
       "RS-S5",
       "RS-S6"
      ],
      "RS-E4": [
       "RS-S7"
      ],
      "RS-E5": [
       "RS-S8"
      ],
      "RS-E6": [
       "RS-S10",
       "RS-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T101851",
     "trials": [
      {
       "calls": {},
       "checks": {
        "RS-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks that the branch's first commit past the ws tip has that tip as its parent, which the refs and log show.",
          "verdict": "agrees"
         },
         "desc": "the branch json-report starts at ws/json-report's tip: that tip is in it, and its first commit's parent",
         "detail": "branch f0589a095150; ws tip ec729cb4609c in it: True; its first commit f0589a095150, parent ec729cb4609c",
         "kind": "decide",
         "pass": true
        },
        "RS-S10": {
         "audit": {
          "agree": true,
          "reason": "Counts of 1, 2 and 0 match the intent's single capability (reporting), the two scenarios added against main, and the absence of an Open Questions section.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the intent's",
         "detail": "line 1/2/0; intent: 1 capabilities ['reporting'], 2 scenarios ['reporting § An unknown option is refused', 'reporting § The JSON report maps each category to its total'], 0 open questions",
         "kind": "decide",
         "pass": true
        },
        "RS-S11": {
         "audit": {
          "agree": true,
          "reason": "The commit body '2 scenarios: 2 uncited' mentions scenarios.",
          "verdict": "agrees"
         },
         "desc": "the Open: commit's message carries a coverage summary (a body naming scenarios)",
         "detail": "2 scenarios: 2 uncited\n\nCo-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>",
         "kind": "signal",
         "pass": true
        },
        "RS-S2": {
         "audit": {
          "agree": true,
          "reason": "The intent's #+BASE: value is the full ws tip hash, as the commit shows.",
          "verdict": "agrees"
         },
         "desc": "the intent's #+BASE: names ws/json-report's tip",
         "detail": "#+BASE: ['ec729cb4609cb636ef3cc5b7f8cbf917a90eb820']; the tip ec729cb4609cb636ef3cc5b7f8cbf917a90eb820",
         "kind": "decide",
         "pass": true
        },
        "RS-S3": {
         "audit": {
          "agree": true,
          "reason": "Both computed hand-off scenarios each appear on exactly one 'stated, not met' line in the intent's Capabilities.",
          "verdict": "agrees"
         },
         "desc": "one Capabilities line <cap> § <title>: stated, not met for each scenario the hand-off's spec edits add or change",
         "detail": "the hand-off's scenarios and their lines: [('reporting § The JSON report maps each category to its total', 1), ('reporting § An unknown option is refused', 1)]; stated lines: ['reporting § The JSON report maps each category to its total', 'reporting § An unknown option is refused']",
         "kind": "decide",
         "pass": true
        },
        "RS-S4": {
         "audit": {
          "agree": true,
          "reason": "The intent at the Open: commit has no #+APPROVED: line.",
          "verdict": "agrees"
         },
         "desc": "the intent has no #+APPROVED: line",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RS-S5": {
         "audit": {
          "agree": true,
          "reason": "The only commit since the fixture is Open: json-report on json-report, and it holds only the intent file.",
          "verdict": "agrees"
         },
         "desc": "one Open: json-report commit, on the branch, holding the intent and spec files only",
         "detail": "tally: f0589a0951 Open: json-report | files: ['docs/changes/json-report/intent.org']",
         "kind": "decide",
         "pass": true
        },
        "RS-S6": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no commits in the home since the fixture.",
          "verdict": "agrees"
         },
         "desc": "no commit in the home",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "RS-S7": {
         "audit": {
          "agree": true,
          "reason": "The intent at the commit has #+STATUS: speculated at the expected path.",
          "verdict": "agrees"
         },
         "desc": "the intent at docs/changes/json-report/intent.org, its #+STATUS: line speculated",
         "detail": "#+STATUS: ['speculated']",
         "kind": "decide",
         "pass": true
        },
        "RS-S8": {
         "audit": {
          "agree": true,
          "reason": "No call reads a file under tally/ or tests/; ls-tree lists names only, and no Grep ran over the repository.",
          "verdict": "agrees"
         },
         "desc": "no file of the code read",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "RS-S9": {
         "audit": {
          "agree": true,
          "reason": "The final text is a single 129-character line in the form, ending with the worktree-relative intent path.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is the opening's line for json-report, its path the intent's relative to the home",
         "detail": "129 characters: open json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/tally/json-report/docs/changes/json-report/intent.org | paths that would do: ['.worktrees/tally/json-report/docs/changes/json-report/intent.org']",
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
        "RS-E6": "pass"
       },
       "judge": {
        "RS-E1": {
         "quotes": [
          "refs/heads/json-report f0589a09515031a77def26ad52691676c47054c7 … f0589a0 Open: json-report / ec729cb Research hand-off: json-report: the spec edits; #+BASE: ec729cb4609cb636ef3cc5b7f8cbf917a90eb820",
          "\"base\": \"ec729cb4609cb636ef3cc5b7f8cbf917a90eb820\", \"from\": \"ws/json-report\" … #+BASE: ec729cb4609cb636ef3cc5b7f8cbf917a90eb820",
          "refs/heads/json-report f0589a09... ; Open: json-report parent ec729cb; #+BASE: ec729cb4609cb636ef3cc5b7f8cbf917a90eb820"
         ],
         "reasons": [
          "The branch's only new commit sits directly on ws/json-report's tip ec729cb, not on main 5a7e127, and the intent's #+BASE: names ec729cb.",
          "The branch json-report's only new commit f0589a0 has the ws/json-report tip ec729cb as its parent, not main, and the intent's #+BASE: names that commit.",
          "The branch json-report was opened --from ws/json-report, its one commit sits directly on ws/json-report's tip ec729cb (not main's 5a7e127), and the intent's #+BASE: names that commit."
         ]
        },
        "RS-E2": {
         "quotes": [
          "- reporting § The JSON report maps each category to its total: stated, not met\n- reporting § An unknown option is refused: stated, not met",
          "- reporting § The JSON report maps each category to its total: stated, not met\n- reporting § An unknown option is refused: stated, not met",
          "- reporting § The JSON report maps each category to its total: stated, not met\n- reporting § An unknown option is refused: stated, not met"
         ],
         "reasons": [
          "The Capabilities section has one 'stated, not met' line for each of the two hand-off scenarios, and the intent has no #+APPROVED: line.",
          "Capabilities has one 'stated, not met' line for each of the hand-off's two scenarios, and the intent has no #+APPROVED: line.",
          "Both scenarios the branch's spec edits added are named once each as 'stated, not met', and the intent has no #+APPROVED: line."
         ]
        },
        "RS-E3": {
         "quotes": [
          "f0589a0 Open: json-report\n2 scenarios: 2 uncited … docs/changes/json-report/intent.org; Commits in the home since the fixture: (none)",
          "f0589a0 Open: json-report\n2 scenarios: 2 uncited … docs/changes/json-report/intent.org; Commits in the home since the fixture: (none)",
          "f0589a0 Open: json-report\n2 scenarios: 2 uncited ... docs/changes/json-report/intent.org; Commits in the home since the fixture: (none)"
         ],
         "reasons": [
          "One Open: json-report commit on the change's branch holds only the intent, its message carries the coverage summary, and the home has no new commit.",
          "There is one Open: commit on json-report holding only the intent; the spec edits already sit at its base, its body carries the coverage summary, and nothing was committed in the home.",
          "Exactly one Open: json-report commit on the change's branch holds only the intent, and its message carries the coverage summary; the home has no new commit."
         ]
        },
        "RS-E4": {
         "quotes": [
          "#+STATUS: speculated",
          "#+STATUS: speculated",
          "#+STATUS: speculated"
         ],
         "reasons": [
          "The intent is at docs/changes/json-report/intent.org in the change's worktree, and its status line is speculated.",
          "The intent is at docs/changes/json-report/intent.org in the change's worktree, and its status line is speculated.",
          "The intent is docs/changes/json-report/intent.org in the change's worktree, with status speculated."
         ]
        },
        "RS-E5": {
         "quotes": [
          "git ls-tree -r --name-only ws/json-report; git show ws/json-report:docs/specs/reporting.org",
          "git ls-tree -r --name-only ws/json-report; git show ws/json-report:docs/specs/reporting.org",
          "git ls-tree -r --name-only ws/json-report; git show ws/json-report:docs/specs/reporting.org; git diff main ws/json-report --stat"
         ],
         "reasons": [
          "The main thread read the brief, the spec, the skill's files and file-name listings, and opened no file under tally/ or tests/.",
          "The calls read only the brief, the spec, the skill's files and git metadata; the code files were listed by name only and never read.",
          "The only contents read were the research brief, the spec file and skill files; code paths were only listed by name, and no tally/ or tests/ file was opened."
         ]
        },
        "RS-E6": {
         "quotes": [
          "open json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/tally/json-report/docs/changes/json-report/intent.org",
          "open json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/tally/json-report/docs/changes/json-report/intent.org",
          "open json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/tally/json-report/docs/changes/json-report/intent.org"
         ],
         "reasons": [
          "One line of 129 characters in the required form, with nothing after the path; the path is relative to the home through the worktree, and the counts (1 capability, reporting; 2 scenarios; no open questions) match the intent.",
          "The hand-back is one 129-character line in the required form, with counts that match the intent and a home-relative path to the intent through the worktree.",
          "One 129-character line in the required form ends at the intent's home-relative path through the worktree, and its counts match the intent: one capability (reporting), two scenarios, no open questions."
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
   "step": "the opening phase agent on a research hand-off, from its brief, to its hand-back",
   "title": "Case opening-research: the opening on a research hand-off, its change cut from the hand-off's workspace integration branch",
   "trials": 3,
   "validated": "9235569e4d4ceb09131cc1d93a38120f2b50d3f5 20261114T101851 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-revise",
   "items": [
    {
     "checks": [
      {
       "id": "OR-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Opening reads no code"
     ],
     "id": "OR-E1",
     "question": "Does the revision read no code?"
    },
    {
     "checks": [
      {
       "id": "OR-S1",
       "kind": "decide"
      },
      {
       "id": "OR-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A revised change keeps its name, branch and base"
     ],
     "id": "OR-E2",
     "question": "Does the change keep its name, its branch, its code root and its base?"
    },
    {
     "checks": [
      {
       "id": "OR-S9",
       "kind": "signal"
      }
     ],
     "cites": [
      "opening § A revision rewrites the goals and the spec edits from the answer"
     ],
     "id": "OR-E3",
     "question": "Are the Goals, the Non-goals and the spec edits rewritten from the answer?"
    },
    {
     "checks": [
      {
       "id": "OR-S6",
       "kind": "decide"
      },
      {
       "id": "OR-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A revision keeps the prior goals under Status"
     ],
     "id": "OR-E4",
     "question": "Are the prior Goals kept under Status, and the status line speculated again?"
    },
    {
     "checks": [
      {
       "id": "OR-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A revision removes the goals' approval"
     ],
     "id": "OR-E5",
     "question": "Is the goals' approval removed?"
    },
    {
     "checks": [
      {
       "id": "OR-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A revision is one commit Revise"
     ],
     "id": "OR-E6",
     "question": "Is the revision one commit, Revise: <change>?"
    },
    {
     "checks": [
      {
       "id": "OR-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A revision hands back a line of its own",
      "opening § The hand-back's intent path is relative to the home"
     ],
     "id": "OR-E7",
     "question": "Does the revision hand back its own line, the intent's path relative to the home?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "OR-E1": [
       "opening § Opening reads no code"
      ],
      "OR-E2": [
       "opening § A revised change keeps its name, branch and base"
      ],
      "OR-E3": [
       "opening § A revision rewrites the goals and the spec edits from the answer"
      ],
      "OR-E4": [
       "opening § A revision keeps the prior goals under Status"
      ],
      "OR-E5": [
       "opening § A revision removes the goals' approval"
      ],
      "OR-E6": [
       "opening § A revision is one commit Revise"
      ],
      "OR-E7": [
       "opening § A revision hands back a line of its own",
       "opening § The hand-back's intent path is relative to the home"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "OR-E1": [
       "OR-S8"
      ],
      "OR-E2": [
       "OR-S1",
       "OR-S2"
      ],
      "OR-E3": [
       "OR-S9"
      ],
      "OR-E4": [
       "OR-S5",
       "OR-S6"
      ],
      "OR-E5": [
       "OR-S4"
      ],
      "OR-E6": [
       "OR-S3"
      ],
      "OR-E7": [
       "OR-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T101833",
     "trials": [
      {
       "calls": {},
       "checks": {
        "OR-S1": {
         "audit": {
          "agree": true,
          "reason": "change.json's branch, worktree and base are identical before and after, no branches were added or removed, and the code root is on json-report, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the change keeps its name, its branch and its code root: change.json's branch, worktree and base as they were, no branch added or removed, the code root on the change's branch",
         "detail": "change.json then ['json-report', '<trial>/home/.worktrees/json-report', '5a7e127a2a39b2991f0cb7b26e7959ef8456a430'], now ['json-report', '<trial>/home/.worktrees/json-report', '5a7e127a2a39b2991f0cb7b26e7959ef8456a430']; branc … [cut, 350 characters]",
         "kind": "decide",
         "pass": true
        },
        "OR-S2": {
         "audit": {
          "agree": true,
          "reason": "The intent's #+BASE: at the tip matches the fixture's, as the diff shows.",
          "verdict": "agrees"
         },
         "desc": "the intent's #+BASE: as it was",
         "detail": "then ['5a7e127a2a39b2991f0cb7b26e7959ef8456a430'], now ['5a7e127a2a39b2991f0cb7b26e7959ef8456a430']",
         "kind": "decide",
         "pass": true
        },
        "OR-S3": {
         "audit": {
          "agree": true,
          "reason": "The only new commit on json-report is 3e55730 'Revise: json-report'.",
          "verdict": "agrees"
         },
         "desc": "one commit on the change's branch, subject Revise: <change>",
         "detail": "3e5573041e77d3756bd503517439b92bb61e48c0 Revise: json-report",
         "kind": "decide",
         "pass": true
        },
        "OR-S4": {
         "audit": {
          "agree": true,
          "reason": "The final intent has no #+APPROVED: line.",
          "verdict": "agrees"
         },
         "desc": "the intent has no #+APPROVED: line",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "OR-S5": {
         "audit": {
          "agree": true,
          "reason": "The final intent's status line is '#+STATUS: speculated'.",
          "verdict": "agrees"
         },
         "desc": "the intent's #+STATUS: line is speculated",
         "detail": "#+TITLE: json-report | #+STATUS: speculated | #+BASE: 5a7e127a2a39b2991f0cb7b26e7959ef8456a430",
         "kind": "decide",
         "pass": true
        },
        "OR-S6": {
         "audit": {
          "agree": true,
          "reason": "Both prior goal texts appear verbatim under * Status in the final intent.",
          "verdict": "agrees"
         },
         "desc": "each prior goal's text is under the intent's * Status",
         "detail": "2 prior goals; not found under * Status: []",
         "kind": "decide",
         "pass": true
        },
        "OR-S7": {
         "audit": {
          "agree": true,
          "reason": "The final text is one 125-character line in the form, ending in the home-relative intent path the check lists as acceptable.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is the revise line, its path the intent's relative to the home",
         "detail": "125 characters: revise json-report: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/json-report/docs/changes/json-report/intent.org | paths that would do: ['.worktrees/json-report/docs/changes/json-report/intent.org']",
         "kind": "decide",
         "pass": true
        },
        "OR-S8": {
         "audit": {
          "agree": true,
          "reason": "No Read, cat or grep in the main thread touches tally/ or tests/, and no Grep ran over a whole repository, which matches the tool-call log.",
          "verdict": "agrees"
         },
         "desc": "no file of the code read",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "OR-S9": {
         "audit": {
          "agree": true,
          "reason": "G1 changed, so the Goals text differs; as a signal it fits, though it does not by itself show the rewrite follows the answer.",
          "verdict": "agrees"
         },
         "desc": "the Goals section differs from the prior one",
         "detail": "",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "OR-E1": "pass",
        "OR-E2": "pass",
        "OR-E3": "pass",
        "OR-E4": "pass",
        "OR-E5": "pass",
        "OR-E6": "pass",
        "OR-E7": "pass"
       },
       "judge": {
        "OR-E1": {
         "quotes": [
          "cat docs/specs/reporting.org; echo '--- priors'; cat ../../.claude/orchestrator/priors.md",
          "cat docs/specs/reporting.org; echo '--- priors'; cat ../../.claude/orchestrator/priors.md",
          "cat docs/specs/reporting.org; echo '--- priors'; cat ../../.claude/orchestrator/priors.md"
         ],
         "reasons": [
          "The main thread read only the intent, the ask, the spec, priors, templates, open.md, git metadata and change.json, with no Read or cat of anything under tally/ or tests/; the test citations come from coverage.py's output, not from the agent reading the files.",
          "Every read in the main thread is of the intent, the ask, the spec, the priors, the templates, git status/log or change.json, and none is under tally/ or tests/; the test citations come only from the coverage script's output.",
          "The main thread read only the intent, the ask, the spec, the priors, the templates, git log/status and change.json; nothing under tally/ or tests/ was read, and the only tests reference comes from the skill's coverage.py citation output."
         ]
        },
        "OR-E2": {
         "quotes": [
          "refs/heads/json-report 3e5573041e… ; #+BASE: 5a7e127a2a39b2991f0cb7b26e7959ef8456a430",
          "refs/heads/json-report 3e5573041e77d3756bd503517439b92bb61e48c0",
          "refs/heads/json-report 3e5573041e77d3756bd503517439b92bb61e48c0 ... #+BASE: 5a7e127a2a39b2991f0cb7b26e7959ef8456a430"
         ],
         "reasons": [
          "The branch set is unchanged apart from json-report advancing, the worktree is the same, and the intent's #+BASE: is as before, so no new change or branch was made.",
          "The same branches exist before and after, the commit lands on json-report in the same worktree, and the intent keeps #+BASE: 5a7e127a…",
          "The same branch json-report gained the commit in the same worktree, the base line is unchanged, and no branch was added or removed."
         ]
        },
        "OR-E3": {
         "quotes": [
          "G1. =tally FILE --json= prints one JSON object that gives the grand total first… N2. No change to the table's column alignment; widening the amount column waits.",
          "G1. =tally FILE --json= prints one JSON object that gives the grand total first ... N2. No change to the table's column alignment; widening the amount column waits.",
          "G1. =tally FILE --json= prints one JSON object that gives the grand total first ... N2. No change to the table's column alignment; widening the amount column waits."
         ],
         "reasons": [
          "The Goals now put the grand total first and the Non-goals defer column alignment, as the rethink answer asks; the spec's requirement and the new scenario 'The JSON report gives the grand total first' follow them.",
          "The Goals and Non-goals state the answer's grand-total-first and alignment-can-wait, and the spec's JSON requirement and scenarios are rewritten to give the grand total first.",
          "G1 and the new N2 state the answer's grand-total-first and alignment-can-wait, and the spec's requirement and scenarios are rewritten to put total first."
         ]
        },
        "OR-E4": {
         "quotes": [
          "#+STATUS: speculated … The prior Goals, verbatim:\n- G1. =tally FILE --json= prints the totals as one JSON object…",
          "#+STATUS: speculated ... The prior Goals, verbatim:\n- G1. =tally FILE --json= prints the totals as one JSON object",
          "#+STATUS: speculated ... The prior Goals, verbatim:\n- G1. =tally FILE --json= prints the totals as one JSON object"
         ],
         "reasons": [
          "Both prior goals are quoted verbatim under a new revised entry in * Status, and the status line is speculated.",
          "Both prior Goals are quoted verbatim under * Status, and the status line is speculated.",
          "Both prior goals are quoted verbatim under * Status, and the status line is speculated."
         ]
        },
        "OR-E5": {
         "quotes": [
          "-#+APPROVED: 2026-11-01",
          "-#+APPROVED: 2026-11-01",
          "-#+APPROVED: 2026-11-01"
         ],
         "reasons": [
          "The diff removes the #+APPROVED: line, and the final intent has none.",
          "The commit removes the #+APPROVED: line, and the final intent has none.",
          "The commit removes the #+APPROVED: line, and the final intent has none."
         ]
        },
        "OR-E6": {
         "quotes": [
          "3e55730 Revise: json-report",
          "3e55730 Revise: json-report",
          "3e55730 Revise: json-report"
         ],
         "reasons": [
          "The branch gained exactly one commit since the fixture, with subject 'Revise: json-report'.",
          "The branch gains exactly one commit, with subject Revise: json-report.",
          "Exactly one commit was added to json-report, with the subject Revise: json-report."
         ]
        },
        "OR-E7": {
         "quotes": [
          "revise json-report: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/json-report/docs/changes/json-report/intent.org",
          "revise json-report: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/json-report/docs/changes/json-report/intent.org",
          "revise json-report: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/json-report/docs/changes/json-report/intent.org"
         ],
         "reasons": [
          "The hand-back is one 125-character line in the required form, and its path is the intent's path relative to the home, through the code root.",
          "The hand-back is one line of 125 characters in the required form, ending in the intent's path relative to the home through the worktree.",
          "It is one line of 125 characters in the required form, ending with the intent's path relative to the home through the worktree."
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
   "step": "the opening phase agent revising a change in place, from its brief (the source the intent and the rethink answer), to its hand-back",
   "title": "Case opening-revise: on the user's rethink, the opening revises the change in place",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T101833 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-second",
   "items": [
    {
     "checks": [
      {
       "id": "OS-S1",
       "kind": "decide"
      },
      {
       "id": "OS-S3",
       "kind": "decide"
      },
      {
       "id": "OS-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The repository the user names takes the change",
      "opening § Work that needs two repositories is two changes"
     ],
     "id": "OS-E1",
     "question": "Is the second change opened in the second repository, its intent naming the first, the first repository untouched?"
    },
    {
     "checks": [
      {
       "id": "OS-S1",
       "kind": "decide"
      },
      {
       "id": "OS-S6",
       "kind": "signal"
      }
     ],
     "cites": [
      "opening § One commit holds the intent and the spec edits"
     ],
     "id": "OS-E2",
     "question": "Does one commit hold the intent and the spec edits, with the coverage summary in its message?"
    },
    {
     "checks": [
      {
       "id": "OS-S4",
       "kind": "decide"
      },
      {
       "id": "OS-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The hand-back names the change, its counts and the intent",
      "opening § The hand-back's intent path is relative to the home"
     ],
     "id": "OS-E3",
     "question": "Does the hand-back name the change, its counts and the intent, the path relative to the home?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "OS-E1": [
       "opening § The repository the user names takes the change",
       "opening § Work that needs two repositories is two changes"
      ],
      "OS-E2": [
       "opening § One commit holds the intent and the spec edits"
      ],
      "OS-E3": [
       "opening § The hand-back names the change, its counts and the intent",
       "opening § The hand-back's intent path is relative to the home"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "OS-E1": [
       "OS-S1",
       "OS-S2",
       "OS-S3"
      ],
      "OS-E2": [
       "OS-S1",
       "OS-S6"
      ],
      "OS-E3": [
       "OS-S4",
       "OS-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T101831",
     "trials": [
      {
       "calls": {},
       "checks": {
        "OS-S1": {
         "audit": {
          "agree": true,
          "reason": "It finds the single Open: commit in sheets with exactly the intent and a docs/specs file, which matches the evidence and what OS-E1 and OS-E2 expect.",
          "verdict": "agrees"
         },
         "desc": "one Open: <change> commit, in the second repository, holding the intent and spec files only",
         "detail": "sheets: eb13424387 Open: import-json-report | files: ['docs/changes/import-json-report/intent.org', 'docs/specs/import-totals.org']",
         "kind": "decide",
         "pass": true
        },
        "OS-S2": {
         "audit": {
          "agree": true,
          "reason": "Tally's refs and worktrees at the end are the same as at the fixture, which is what it means for the first repository to be untouched.",
          "verdict": "agrees"
         },
         "desc": "the first repository's branches and worktrees as the fixture left them",
         "detail": "refs differing: []; worktrees differing: []",
         "kind": "decide",
         "pass": true
        },
        "OS-S3": {
         "audit": {
          "agree": true,
          "reason": "The Why section at the commit contains =json-report=, which is the first change's name as OS-E1 requires.",
          "verdict": "agrees"
         },
         "desc": "the new intent's Why names the first change (not applicable without the Open: commit)",
         "detail": "The user: \"tally should print its totals as JSON, and sheets should read them as JSON instead of parsing the table.\" This is the second of two changes: the first, =json-report= in =tally=, gives =tally FILE --json=, which prints the totals as one JSON object (=tally='s =reporting § The JSON report m … [cut, 445 characters]",
         "kind": "decide",
         "pass": true
        },
        "OS-S4": {
         "audit": {
          "agree": true,
          "reason": "Its observed length, form, change name and path relative to the home through change.json's worktree all match the hand-back in the evidence.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is the opening's line for the new change, its path the intent's relative to the home",
         "detail": "151 characters: open import-json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/sheets/import-json-report/docs/changes/import-json-report/intent.org | change import-json-report; paths that would do: ['.worktrees/sheets/import-json-report/docs/changes/import-json-report/intent.org' … [cut, 301 characters]",
         "kind": "decide",
         "pass": true
        },
        "OS-S5": {
         "audit": {
          "agree": true,
          "reason": "The counts 1/2/0 match the intent's one Capabilities entry, the two scenarios the commit adds, and the intent having no Open Questions section.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the intent's: its capabilities, the change's scenarios, its open questions",
         "detail": "line 1/2/0; intent: 1 capabilities ['import-totals'], 2 scenarios ['import-totals § A negative amount is kept as written', 'import-totals § Each category in the report becomes one row'], 0 open questions",
         "kind": "decide",
         "pass": true
        },
        "OS-S6": {
         "audit": {
          "agree": true,
          "reason": "As a signal it only checks that the body mentions scenarios, and the body does carry the coverage summary line.",
          "verdict": "agrees"
         },
         "desc": "the Open: commit's message carries a coverage summary (a body naming scenarios)",
         "detail": "Coverage: 2 scenarios: 2 uncited\n\nCo-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "OS-E1": "pass",
        "OS-E2": "pass",
        "OS-E3": "pass"
       },
       "judge": {
        "OS-E1": {
         "quotes": [
          "refs/heads/import-json-report eb13424387983b495381326649739e2be910d224 ... This is the second of two changes: the first, =json-report= in =tally=",
          "refs/heads/import-json-report eb13424387983b495381326649739e2be910d224 ... This is the second of two changes: the first, =json-report= in =tally=",
          "This is the second of two changes: the first, =json-report= in =tally="
         ],
         "reasons": [
          "The change was opened in sheets, the repository the source names for reading the JSON. Its intent's Why names json-report, and tally's refs, worktrees and commits are the same at the end as at the fixture.",
          "The change was opened in sheets, the repository the source names for the second change; its Why names json-report; tally's refs, worktrees and commits are unchanged.",
          "The change import-json-report was opened in sheets (\"repo\": \"sheets\" in change.json, commit eb13424 on sheets), its Why names json-report, and tally's refs, worktrees and commits since the fixture are unchanged ((none))."
         ]
        },
        "OS-E2": {
         "quotes": [
          "eb13424 Open: import-json-report / Coverage: 2 scenarios: 2 uncited / docs/changes/import-json-report/intent.org / docs/specs/import-totals.org",
          "eb13424 Open: import-json-report / Coverage: 2 scenarios: 2 uncited / docs/changes/import-json-report/intent.org / docs/specs/import-totals.org",
          "eb13424 Open: import-json-report\nCoverage: 2 scenarios: 2 uncited ... docs/changes/import-json-report/intent.org\ndocs/specs/import-totals.org"
         ],
         "reasons": [
          "One commit in the change's worktree has the subject Open: import-json-report. It holds only the intent and the one spec file, and its message carries the coverage summary that coverage.py printed for the change's scenarios.",
          "One commit in the change's worktree holds exactly the intent and the one spec file it adds, and its message carries the change-scoped coverage summary.",
          "One commit in the change's worktree, subject Open: import-json-report, holds only the intent and the one spec file, and its body carries the coverage summary the coverage.py run printed for the change's scenarios."
         ]
        },
        "OS-E3": {
         "quotes": [
          "open import-json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/sheets/import-json-report/docs/changes/import-json-report/intent.org",
          "open import-json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/sheets/import-json-report/docs/changes/import-json-report/intent.org",
          "open import-json-report: 1 capabilities, 2 scenarios, 0 open questions; .worktrees/sheets/import-json-report/docs/changes/import-json-report/intent.org"
         ],
         "reasons": [
          "The hand-back is one line of 151 characters in the required form with nothing after the path. The path is the intent's, relative to the home through the change's worktree. The counts match the intent: 1 capability, 2 added scenarios, no Open Questions.",
          "The hand-back is one line of 151 characters in the required form with nothing after the path; the path is relative to the home through the change's worktree, and the counts match the intent (one capability, two scenarios, no Open Questions).",
          "The final text is one line of 151 characters in the required form, with nothing after the path; the path is the intent's, relative to the home through the change's worktree; the counts match the intent: one capability (import-totals), two scenarios and no Open Questions section."
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
   "step": "the second opening phase agent, from its brief (the source the same words and the first change's name), to its hand-back",
   "title": "Case opening-second: the second opening of two-repository work, in the second repository, naming the first change",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T101831 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-split",
   "items": [
    {
     "checks": [
      {
       "id": "SP-S1",
       "kind": "decide"
      },
      {
       "id": "SP-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A split opens a new change that names the old"
     ],
     "id": "SP-E1",
     "question": "Does the split open a new change whose intent names the old one?"
    },
    {
     "checks": [
      {
       "id": "SP-S3",
       "kind": "decide"
      },
      {
       "id": "SP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A split's opening names the old change for revision"
     ],
     "id": "SP-E2",
     "question": "Is the old change named for revision, its intent left as it was?"
    },
    {
     "checks": [
      {
       "id": "SP-S4",
       "kind": "decide"
      },
      {
       "id": "SP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The hand-back names the change, its counts and the intent"
     ],
     "id": "SP-E3",
     "question": "Does the hand-back name the new change, its counts and its intent?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "b770aec",
     "cites": {
      "SP-E1": [
       "opening § A split opens a new change that names the old"
      ],
      "SP-E2": [
       "opening § A split's opening names the old change for revision"
      ],
      "SP-E3": [
       "opening § The hand-back names the change, its counts and the intent"
      ]
     },
     "commit": "db6ad9d",
     "item_checks": {
      "SP-E1": [
       "SP-S1",
       "SP-S2"
      ],
      "SP-E2": [
       "SP-S3",
       "SP-S4"
      ],
      "SP-E3": [
       "SP-S4",
       "SP-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T001121",
     "trials": [
      {
       "calls": {},
       "checks": {
        "SP-S1": {
         "audit": {
          "agree": true,
          "reason": "It finds the single `Open: options-check` commit on the options-check branch, holding only the new intent, which matches the evidence and what SP-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "one Open: <new change> commit, on a new branch of its name, holding the intent and spec files only",
         "detail": "home: 6a9933b542 Open: options-check | branch options-check: True | files: ['docs/changes/options-check/intent.org']",
         "kind": "decide",
         "pass": true
        },
        "SP-S2": {
         "audit": {
          "agree": true,
          "reason": "The Why at that commit does contain json-report, as the committed diff shows.",
          "verdict": "agrees"
         },
         "desc": "the new intent's Why names the old change (not applicable without the Open: commit)",
         "detail": "Split from json-report (ask-cycle-2-1). Its goal G2 was \"=tally= refuses an option it does not know, so a typo is not read as a file\", and after two cycles no work on it had started. The user answered: \"split: refusing unknown options becomes its own change, options-check; json-report keeps the JSON … [cut, 504 characters]",
         "kind": "decide",
         "pass": true
        },
        "SP-S3": {
         "audit": {
          "agree": true,
          "reason": "json-report's tip equals the fixture's (6f83eed) and no commit or write touched its worktree, so the check's unchanged-intent and clean-status verdict fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the old change's branch and intent as they were, nothing uncommitted in its code root",
         "detail": "old branch tip 6f83eedf46 (fixture 6f83eedf46); intent unchanged: True; status: clean",
         "kind": "decide",
         "pass": true
        },
        "SP-S4": {
         "audit": {
          "agree": true,
          "reason": "The final text is a single 150-character line in the required form, with the new change's name, its intent path through the worktree, and `revise: json-report`.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is the opening's line for the new change, ending ; revise: <old change>",
         "detail": "150 characters: open options-check: 1 capabilities, 1 scenarios, 0 open questions; .worktrees/options-check/docs/changes/options-check/intent.org; revise: json-report | new change options-check; paths that would do: ['.worktrees/options-check/docs/changes/options-check/intent.org']",
         "kind": "decide",
         "pass": true
        },
        "SP-S5": {
         "audit": {
          "agree": true,
          "reason": "The counts 1/1/0 match the intent's one Capabilities entry, its one scenario named `stated, not met`, and its absent open questions.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the new intent's",
         "detail": "line 1/1/0; intent: 1 capabilities ['reporting'], 1 scenarios ['reporting § An unknown option is refused'], 0 open questions",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "SP-E1": "pass",
        "SP-E2": "pass",
        "SP-E3": "pass"
       },
       "judge": {
        "SP-E1": {
         "quotes": [
          "6a9933b Open: options-check ... docs/changes/options-check/intent.org ... * Why\nSplit from json-report (ask-cycle-2-1).",
          "6a9933b Open: options-check ... docs/changes/options-check/intent.org ... * Why\nSplit from json-report (ask-cycle-2-1).",
          "6a9933b Open: options-check … docs/changes/options-check/intent.org … * Why\nSplit from json-report (ask-cycle-2-1)."
         ],
         "reasons": [
          "A new change options-check has its own branch (refs/heads/options-check 6a9933b) and one `Open: options-check` commit holding its intent. The intent is drawn from json-report's G2 and the user's answer, and its Why names json-report; no spec edit was needed because the cited scenario already exists on the base it was cut from.",
          "A new change `options-check` has its own branch (`refs/heads/options-check 6a9933b`) and exactly one `Open: options-check` commit holding its intent; the intent draws on the old G2 and the user's answer, and its Why names `json-report`. No spec edits were needed because the scenario already exists on the json-report base.",
          "A new change `options-check` has its own branch and one `Open: options-check` commit holding its intent, and its Why names json-report and quotes the user's split answer."
         ]
        },
        "SP-E2": {
         "quotes": [
          "refs/heads/json-report 6f83eedf46b7c9f703e01a2c53c63924b2fec923 ... ; revise: json-report",
          "refs/heads/json-report 6f83eedf46... (unchanged at the end); hand-back ends \"; revise: json-report\"",
          "refs/heads/json-report 6f83eedf46b7c9f703e01a2c53c63924b2fec923 (fixture and end); hand-back \"…; revise: json-report\""
         ],
         "reasons": [
          "json-report's branch tip is the fixture's and the only new commit is on options-check. The agent wrote nothing in json-report's worktree, and the hand-back ends `; revise: json-report`.",
          "The json-report branch tip is the same at the fixture and at the end, no step wrote to its worktree, and the hand-back line ends `; revise: json-report`.",
          "The json-report branch tip is unchanged from the fixture, no commit or edit touched its worktree or intent, and the hand-back line ends `; revise: json-report`."
         ]
        },
        "SP-E3": {
         "quotes": [
          "open options-check: 1 capabilities, 1 scenarios, 0 open questions; .worktrees/options-check/docs/changes/options-check/intent.org; revise: json-report",
          "open options-check: 1 capabilities, 1 scenarios, 0 open questions; .worktrees/options-check/docs/changes/options-check/intent.org; revise: json-report",
          "open options-check: 1 capabilities, 1 scenarios, 0 open questions; .worktrees/options-check/docs/changes/options-check/intent.org; revise: json-report"
         ],
         "reasons": [
          "The line names the new change and the intent's path. Its counts match the intent: one capability (reporting), one scenario marked `stated, not met`, and no Open Questions section.",
          "The line names the new change and the intent's path; its counts match the intent: one capability (reporting), one scenario named stated-not-met, and no Open Questions section.",
          "The line names the new change, gives counts that match its intent (one capability, one scenario marked `stated, not met`, no open questions) and gives the intent's path."
         ]
        }
       },
       "milestones": [],
       "n_calls": 14,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the opening phase agent on a split answer, from its brief (the source the split ask's id and its change), to its hand-back",
   "title": "Case opening-split: on the user's split, the opening opens a new change that names the old, and names the old for revision",
   "trials": 3,
   "validated": "b770aec0f26a759b9d86ca6f2c3fd3aa1ae229a3 20261114T001121 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-two-repos",
   "items": [
    {
     "checks": [
      {
       "id": "TR-S1",
       "kind": "decide"
      },
      {
       "id": "TR-S2",
       "kind": "decide"
      },
      {
       "id": "TR-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Each opening opens one change, and names the second when there is one"
     ],
     "id": "TR-E1",
     "question": "Does the opening open one change only, and name the second change's repository in its line?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "TR-E1": [
       "opening § Each opening opens one change, and names the second when there is one"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "TR-E1": [
       "TR-S1",
       "TR-S2",
       "TR-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T101829",
     "trials": [
      {
       "calls": {},
       "checks": {
        "TR-S1": {
         "audit": {
          "agree": true,
          "reason": "Exactly one `Open:` commit (3eb8b76 in tally) appears across both repositories and sheets has none, which matches the item's one-change requirement.",
          "verdict": "agrees"
         },
         "desc": "exactly one Open: commit, in one repository",
         "detail": "tally: 3eb8b767dd Open: tally-totals-as-json",
         "kind": "decide",
         "pass": true
        },
        "TR-S2": {
         "audit": {
          "agree": true,
          "reason": "The final text is a single 179-character line in the required form, and it names sheets, a config.yaml repository that is not tally, which holds the `Open:` commit.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is the opening's line ending ; second change: <the other repository>",
         "detail": "179 characters: open tally-totals-as-json: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/tally/tally-totals-as-json/docs/changes/tally-totals-as-json/intent.org; second change: sheets | opened in tally; second named: sheets",
         "kind": "decide",
         "pass": true
        },
        "TR-S3": {
         "audit": {
          "agree": true,
          "reason": "Sheets' refs (main at 0d2a689) and its single worktree are identical at the fixture and at the end, so the other repository is untouched as the item expects.",
          "verdict": "agrees"
         },
         "desc": "the other repository has no branch or worktree the opening made (not applicable without one Open: commit)",
         "detail": "refs differing: {'sheets': []}; worktrees differing: {'sheets': []}",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "TR-E1": "pass"
       },
       "judge": {
        "TR-E1": {
         "quotes": [
          "open tally-totals-as-json: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/tally/tally-totals-as-json/docs/changes/tally-totals-as-json/intent.org; second change: sheets",
          "open tally-totals-as-json: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/tally/tally-totals-as-json/docs/changes/tally-totals-as-json/intent.org; second change: sheets",
          "open tally-totals-as-json: 1 capabilities, 4 scenarios, 1 open questions; .worktrees/tally/tally-totals-as-json/docs/changes/tally-totals-as-json/intent.org; second change: sheets"
         ],
         "reasons": [
          "One change was opened in tally with a single `Open: tally-totals-as-json` commit; sheets' refs and worktrees are unchanged from the fixture; and the hand-back line ends `; second change: sheets`.",
          "The run opened one change, in tally only, with a single commit `3eb8b76 Open: tally-totals-as-json`, left sheets' refs and worktrees unchanged with no commits since the fixture, and the hand-back line ends `; second change: sheets`.",
          "One change was opened in tally with a single 'Open: tally-totals-as-json' commit, sheets' refs and worktrees are unchanged, and the hand-back line ends '; second change: sheets'."
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
   "step": "the opening phase agent, from its brief (the source the user's words), to its hand-back",
   "title": "Case opening-two-repos: work that needs two repositories; the opening opens the first change and names the second",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T101829 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-which-repo",
   "items": [
    {
     "checks": [
      {
       "id": "WR-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § With several repositories and none named, opening asks which",
      "opening § A failed opening says why"
     ],
     "id": "WR-E1",
     "question": "Does the opening hand back which repository, naming the home's repositories?"
    },
    {
     "checks": [
      {
       "id": "WR-S2",
       "kind": "decide"
      },
      {
       "id": "WR-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § With several repositories and none named, opening asks which",
      "opening § A failed opening leaves nothing behind"
     ],
     "id": "WR-E2",
     "question": "Does the failed opening create no branch and leave nothing behind?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "WR-E1": [
       "opening § With several repositories and none named, opening asks which",
       "opening § A failed opening says why"
      ],
      "WR-E2": [
       "opening § With several repositories and none named, opening asks which",
       "opening § A failed opening leaves nothing behind"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "WR-E1": [
       "WR-S1"
      ],
      "WR-E2": [
       "WR-S2",
       "WR-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T101827-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "WR-S1": {
         "audit": {
          "agree": true,
          "reason": "It checks that the final text is the one 'open: failed, which repository:' line holding each repository named in config.yaml, which is what WR-E1 expects, and the evidence's final line matches.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is open: failed, which repository:, naming both repositories",
         "detail": "repositories ['sheets', 'tally']; line: open: failed, which repository: tally, sheets",
         "kind": "decide",
         "pass": true
        },
        "WR-S2": {
         "audit": {
          "agree": true,
          "reason": "It compares each repository's refs and worktrees against the build's record, which covers WR-E2's 'no branch or worktree' clause, and the evidence shows them identical.",
          "verdict": "agrees"
         },
         "desc": "no branch or worktree added, moved or removed in either repository",
         "detail": "refs differing: {'tally': [], 'sheets': []}; worktrees differing: {'tally': [], 'sheets': []}",
         "kind": "decide",
         "pass": true
        },
        "WR-S3": {
         "audit": {
          "agree": true,
          "reason": "It checks the home for any added, changed or removed file, HEAD movement or dirty status, which covers WR-E2's 'no change.json and nothing left' clause, and the evidence shows none; the scratch directory sits outside the home, but the agent removed it anyway.",
          "verdict": "agrees"
         },
         "desc": "the home as the fixture left it: its HEAD, its status, its files (no change.json)",
         "detail": "files: none; HEAD 37416061c87680d197d5da9ad4289a6b62a76765 (fixture 37416061c87680d197d5da9ad4289a6b62a76765); status: clean",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "WR-E1": "pass",
        "WR-E2": "pass"
       },
       "judge": {
        "WR-E1": {
         "quotes": [
          "open: failed, which repository: tally, sheets",
          "open: failed, which repository: tally, sheets",
          "open: failed, which repository: tally, sheets"
         ],
         "reasons": [
          "The hand-back is the single required line and names both repositories from the home's config.yaml, tally and sheets.",
          "The hand-back is exactly the one line `open: failed, which repository:` and names both repositories that config.yaml lists, tally and sheets.",
          "The hand-back is the single expected line, and it names both repositories the home's config lists (tally and sheets)."
         ]
        },
        "WR-E2": {
         "quotes": [
          "refs/heads/main 5a7e127a… (unchanged); refs/heads/main 0d2a689d… (unchanged); Files added, changed or removed in the home since the fixture: (none)",
          "refs/heads/main 5a7e127… (tally, unchanged); refs/heads/main 0d2a689… (sheets, unchanged); Files added, changed or removed in the home since the fixture: (none)",
          "refs/heads/main 5a7e127a… (fixture) … refs/heads/main 5a7e127a… (end); Files added, changed or removed in the home since the fixture … (none)"
         ],
         "reasons": [
          "Both repositories end with the same refs and worktrees as at the fixture, with no new commits; the home has no added files, no commits and no change.json; and the only thing the agent made, the scratch directory, was removed with rmdir.",
          "Neither repository gained a branch, worktree or commit, the home has no new or changed files (so no change.json), and the scratch directory the agent created was removed (`rmdir … scratch/phase-open/`).",
          "Both repositories keep the fixture's refs and worktrees with no new commits, the home has no changed files and no change.json, and the only file-system write was the scratch directory, which was created outside the home and then removed with rmdir."
         ]
        }
       },
       "milestones": [],
       "n_calls": 3,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the opening phase agent, from its brief (the source the user's words), to its hand-back",
   "title": "Case opening-which-repo: two repositories and words naming neither; the opening asks which and leaves nothing",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T101827-1 t1 (skill-evals)"
  },
  {
   "capability": "opening",
   "id": "opening-words",
   "items": [
    {
     "checks": [
      {
       "id": "OW-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A change takes the name the user gives it",
      "opening § A new change's name is new to its repository"
     ],
     "id": "OW-E1",
     "question": "Does the change take the name the user gave, new to the repository?"
    },
    {
     "checks": [
      {
       "id": "OW-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A home with one repository opens the change in it",
      "opening § The integration branch starts from the repository's current commit",
      "opening § The intent carries the change's base"
     ],
     "id": "OW-E2",
     "question": "Is the change in the home's one repository, started from its current commit, the intent carrying that base?"
    },
    {
     "checks": [
      {
       "id": "OW-S15",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Opening reads no code",
      "opening § A claim about the code reaches the Why as stated"
     ],
     "id": "OW-E3",
     "question": "Does the opening read no code, and carry the user's claim about it into the Why as stated?"
    },
    {
     "checks": [
      {
       "id": "OW-S9",
       "kind": "decide"
      },
      {
       "id": "OW-S20",
       "kind": "signal"
      }
     ],
     "cites": [
      "opening § An added or altered behaviour is a requirement with observable scenarios",
      "opening § A scenario names no symbol, guard or encoding"
     ],
     "id": "OW-E4",
     "question": "Is the new behaviour a requirement with observable scenarios that name nothing of the code?"
    },
    {
     "checks": [
      {
       "id": "OW-S3",
       "kind": "decide"
      },
      {
       "id": "OW-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The specs are edited in place",
      "opening § Existing titles are kept"
     ],
     "id": "OW-E5",
     "question": "Are the specs edited in place, every existing title kept?"
    },
    {
     "checks": [
      {
       "id": "OW-S6",
       "kind": "decide"
      },
      {
       "id": "OW-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A broken behaviour the spec states rightly is named, not edited"
     ],
     "id": "OW-E6",
     "question": "Is the ordering behaviour the spec states rightly named, not edited?"
    },
    {
     "checks": [
      {
       "id": "OW-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The intent is written with status speculated",
      "opening § An opened intent awaits the user's approval"
     ],
     "id": "OW-E7",
     "question": "Is the intent written with status speculated, awaiting the user's approval?"
    },
    {
     "checks": [
      {
       "id": "OW-S19",
       "kind": "signal"
      }
     ],
     "cites": [
      "opening § The Why quotes the user"
     ],
     "id": "OW-E8",
     "question": "Does the Why quote the user?"
    },
    {
     "checks": [
      {
       "id": "OW-S5",
       "kind": "decide"
      },
      {
       "id": "OW-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The intent states its goals, non-goals and capabilities",
      "opening § Capabilities names each spec the change edits"
     ],
     "id": "OW-E9",
     "question": "Does the intent state its Why, Goals, Non-goals and Capabilities, naming each spec it edits?"
    },
    {
     "checks": [
      {
       "id": "OW-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The intent and the spec edits use the glossary's terms",
      "opening § A term the glossary lacks is listed for agreement",
      "opening § Opening leaves the glossary as it is"
     ],
     "id": "OW-E10",
     "question": "Are the glossary's terms used, the new term listed for agreement, the glossary left as it is?"
    },
    {
     "checks": [
      {
       "id": "OW-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § A question the spec depends on is an open question",
      "opening § The spec is written to the recommendation",
      "opening § An open question says nothing of how to build",
      "opening § A case the user never raised is no open question"
     ],
     "id": "OW-E11",
     "question": "Is the question left to the user an open question, the spec written to its recommendation, and none raised for a case the user never raised?"
    },
    {
     "checks": [
      {
       "id": "OW-S18",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Opening asks the user nothing"
     ],
     "id": "OW-E12",
     "question": "Does the opening ask the user nothing?"
    },
    {
     "checks": [
      {
       "id": "OW-S3",
       "kind": "decide"
      },
      {
       "id": "OW-S21",
       "kind": "signal"
      }
     ],
     "cites": [
      "opening § One commit holds the intent and the spec edits"
     ],
     "id": "OW-E13",
     "question": "Does one commit hold the intent and the spec edits, with the coverage summary?"
    },
    {
     "checks": [
      {
       "id": "OW-S11",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Every scenario the intent names exists"
     ],
     "id": "OW-E14",
     "question": "Does every scenario the intent names exist?"
    },
    {
     "checks": [
      {
       "id": "OW-S14",
       "kind": "decide"
      },
      {
       "id": "OW-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § Opening commits nothing in the home",
      "opening § Opening writes no design and no task"
     ],
     "id": "OW-E15",
     "question": "Does the opening commit nothing in the home, and write no design and no task?"
    },
    {
     "checks": [
      {
       "id": "OW-S16",
       "kind": "decide"
      },
      {
       "id": "OW-S17",
       "kind": "decide"
      }
     ],
     "cites": [
      "opening § The hand-back names the change, its counts and the intent",
      "opening § The hand-back's intent path is relative to the home",
      "opening § The hand-back's counts are the intent's"
     ],
     "id": "OW-E16",
     "question": "Does the hand-back name the change, its counts and the intent, its path relative to the home?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "OW-E1": [
       "opening § A change takes the name the user gives it",
       "opening § A new change's name is new to its repository"
      ],
      "OW-E10": [
       "opening § The intent and the spec edits use the glossary's terms",
       "opening § A term the glossary lacks is listed for agreement",
       "opening § Opening leaves the glossary as it is"
      ],
      "OW-E11": [
       "opening § A question the spec depends on is an open question",
       "opening § The spec is written to the recommendation",
       "opening § An open question says nothing of how to build",
       "opening § A case the user never raised is no open question"
      ],
      "OW-E12": [
       "opening § Opening asks the user nothing"
      ],
      "OW-E13": [
       "opening § One commit holds the intent and the spec edits"
      ],
      "OW-E14": [
       "opening § Every scenario the intent names exists"
      ],
      "OW-E15": [
       "opening § Opening commits nothing in the home",
       "opening § Opening writes no design and no task"
      ],
      "OW-E16": [
       "opening § The hand-back names the change, its counts and the intent",
       "opening § The hand-back's intent path is relative to the home",
       "opening § The hand-back's counts are the intent's"
      ],
      "OW-E2": [
       "opening § A home with one repository opens the change in it",
       "opening § The integration branch starts from the repository's current commit",
       "opening § The intent carries the change's base"
      ],
      "OW-E3": [
       "opening § Opening reads no code",
       "opening § A claim about the code reaches the Why as stated"
      ],
      "OW-E4": [
       "opening § An added or altered behaviour is a requirement with observable scenarios",
       "opening § A scenario names no symbol, guard or encoding"
      ],
      "OW-E5": [
       "opening § The specs are edited in place",
       "opening § Existing titles are kept"
      ],
      "OW-E6": [
       "opening § A broken behaviour the spec states rightly is named, not edited"
      ],
      "OW-E7": [
       "opening § The intent is written with status speculated",
       "opening § An opened intent awaits the user's approval"
      ],
      "OW-E8": [
       "opening § The Why quotes the user"
      ],
      "OW-E9": [
       "opening § The intent states its goals, non-goals and capabilities",
       "opening § Capabilities names each spec the change edits"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "OW-E1": [
       "OW-S1"
      ],
      "OW-E10": [
       "OW-S12"
      ],
      "OW-E11": [
       "OW-S5"
      ],
      "OW-E12": [
       "OW-S18"
      ],
      "OW-E13": [
       "OW-S21",
       "OW-S3"
      ],
      "OW-E14": [
       "OW-S11"
      ],
      "OW-E15": [
       "OW-S13",
       "OW-S14"
      ],
      "OW-E16": [
       "OW-S16",
       "OW-S17"
      ],
      "OW-E2": [
       "OW-S2"
      ],
      "OW-E3": [
       "OW-S15"
      ],
      "OW-E4": [
       "OW-S20",
       "OW-S9"
      ],
      "OW-E5": [
       "OW-S3",
       "OW-S8"
      ],
      "OW-E6": [
       "OW-S6",
       "OW-S7"
      ],
      "OW-E7": [
       "OW-S4"
      ],
      "OW-E8": [
       "OW-S19"
      ],
      "OW-E9": [
       "OW-S5",
       "OW-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T112826-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "OW-S1": {
         "audit": {
          "agree": true,
          "reason": "The new branch and the directory absent at the fixture but present at the tip match the evidence.",
          "verdict": "agrees"
         },
         "desc": "the branch dollar-amounts and docs/changes/dollar-amounts/ are new, the directory on the branch",
         "detail": "branch tip 1d0b7407470e3770522d637b2237f6049caa172e; the directory at the fixture's HEAD: absent",
         "kind": "decide",
         "pass": true
        },
        "OW-S11": {
         "audit": {
          "agree": true,
          "reason": "Both names it extracted from the intent are scenario titles in the spec at the tip.",
          "verdict": "agrees"
         },
         "desc": "every scenario the intent names matches a scenario of the specs at the branch's tip",
         "detail": "named: ['reporting § Categories are ordered by total', 'reporting § A refund is shown as a negative amount']; matching none: []",
         "kind": "decide",
         "pass": true
        },
        "OW-S12": {
         "audit": {
          "agree": true,
          "reason": "The commit does not touch `docs/glossary.org`.",
          "verdict": "agrees"
         },
         "desc": "the glossary unchanged",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "OW-S13": {
         "audit": {
          "agree": true,
          "reason": "There is no `design.org` and no task file among the files added.",
          "verdict": "agrees"
         },
         "desc": "no design.org and no task file",
         "detail": "design.org: absent; task files: []",
         "kind": "decide",
         "pass": true
        },
        "OW-S14": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows no commits in the home.",
          "verdict": "agrees"
         },
         "desc": "no commit in the home",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "OW-S15": {
         "audit": {
          "agree": true,
          "reason": "No call reads a code path; the `find` searches the filesystem for `priors.md` and never reads code files.",
          "verdict": "agrees"
         },
         "desc": "no file of the code read",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "OW-S16": {
         "audit": {
          "agree": true,
          "reason": "The final line has the required form and length, and its path is the intent's, relative to the home.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is the opening's line for dollar-amounts, its path the intent's relative to the home",
         "detail": "138 characters: open dollar-amounts: 1 capabilities, 3 scenarios, 1 open questions; .worktrees/tally/dollar-amounts/docs/changes/dollar-amounts/intent.org | paths that would do: ['.worktrees/tally/dollar-amounts/docs/changes/dollar-amounts/intent.org']",
         "kind": "decide",
         "pass": true
        },
        "OW-S17": {
         "audit": {
          "agree": true,
          "reason": "Counts of 1/3/1 match the intent's one capability, the two added or changed scenarios plus the stated-not-met one, and its one question.",
          "verdict": "agrees"
         },
         "desc": "the line's counts are the intent's",
         "detail": "line 1/3/1; intent: 1 capabilities ['reporting'], 3 scenarios ['reporting § A category total is written with a dollar sign', 'reporting § A refund is shown as a negative amount', 'reporting § Categories are ordered by total'], 1 open questions",
         "kind": "decide",
         "pass": true
        },
        "OW-S18": {
         "audit": {
          "agree": true,
          "reason": "No ask file appears among the home's added files.",
          "verdict": "agrees"
         },
         "desc": "no ask filed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "OW-S19": {
         "audit": {
          "agree": true,
          "reason": "The Why contains long verbatim runs of the user's words.",
          "verdict": "agrees"
         },
         "desc": "the Why quotes the user (six words of theirs in a row)",
         "detail": "The user asked: \"Every amount in the totals table should carry the currency mark, a dollar sign before the number, the sum for each category included.\" They report that \"the amounts are rendered in one place, so this should be a small change\" (a claim about the code, for the round to check).\n\nThey a … [cut, 476 characters]",
         "kind": "signal",
         "pass": true
        },
        "OW-S2": {
         "audit": {
          "agree": true,
          "reason": "The first commit's parent and `#+BASE:` both equal the fixture's HEAD `bfb660bd`, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the branch starts from the repository's HEAD at the fixture, and #+BASE: names that commit",
         "detail": "first commit's parent bfb660bd3d02, the fixture's HEAD bfb660bd3d02; #+BASE: bfb660bd3d0266f033080dd4472762be05756d96",
         "kind": "decide",
         "pass": true
        },
        "OW-S20": {
         "audit": {
          "agree": true,
          "reason": "Two scenarios are added or changed (refund and the new dollar-sign one), and neither holds a code-like word.",
          "verdict": "agrees"
         },
         "desc": "the new or changed scenarios hold no code identifier",
         "detail": "2 scenarios read",
         "kind": "signal",
         "pass": true
        },
        "OW-S21": {
         "audit": {
          "agree": true,
          "reason": "The message body carries `coverage: 3 scenarios: 3 uncited`.",
          "verdict": "agrees"
         },
         "desc": "the Open: commit's message carries a coverage summary (a body naming scenarios)",
         "detail": "coverage: 3 scenarios: 3 uncited\n\nCo-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>",
         "kind": "signal",
         "pass": true
        },
        "OW-S3": {
         "audit": {
          "agree": true,
          "reason": "Exactly one commit holding only the intent and `reporting.org` matches the commit shown.",
          "verdict": "agrees"
         },
         "desc": "one commit on the branch, Open: dollar-amounts, holding the intent and spec files only",
         "detail": "commits: ['Open: dollar-amounts']; files: ['docs/changes/dollar-amounts/intent.org', 'docs/specs/reporting.org']",
         "kind": "decide",
         "pass": true
        },
        "OW-S4": {
         "audit": {
          "agree": true,
          "reason": "The status is speculated and there is no `#+APPROVED:` line, as in the intent.",
          "verdict": "agrees"
         },
         "desc": "the intent at docs/changes/dollar-amounts/intent.org, its #+STATUS: line speculated, no #+APPROVED:",
         "detail": "#+TITLE: dollar-amounts | #+STATUS: speculated | #+BASE: bfb660bd3d0266f033080dd4472762be05756d96",
         "kind": "decide",
         "pass": true
        },
        "OW-S5": {
         "audit": {
          "agree": true,
          "reason": "The intent indeed lacks a New terms section though the words give it an entry, so the fail fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the intent has Why, Goals, Non-goals and Capabilities, and Open Questions and New terms (the words give both), none of its optional sections empty",
         "detail": "sections: ['Why', 'Goals', 'Non-goals', 'Capabilities', 'Open Questions']; missing: ['New terms']; empty optional: []",
         "kind": "decide",
         "pass": false
        },
        "OW-S6": {
         "audit": {
          "agree": true,
          "reason": "Capabilities names `reporting` and has the exact `stated, not met` line.",
          "verdict": "agrees"
         },
         "desc": "Capabilities names reporting, and has reporting § Categories are ordered by total: stated, not met",
         "detail": "capabilities ['reporting']; stated, not met: [('reporting', 'Categories are ordered by total')]",
         "kind": "decide",
         "pass": true
        },
        "OW-S7": {
         "audit": {
          "agree": true,
          "reason": "The diff leaves the ordering scenario's text untouched, matching the then/now comparison.",
          "verdict": "agrees"
         },
         "desc": "the ordering scenario's text unchanged",
         "detail": "then: - *WHEN* a ledger has food 13.05 and rent 900.00 - *THEN* the table lists rent before food | now: - *WHEN* a ledger has food 13.05 and rent 900.00 - *THEN* the table lists rent before food",
         "kind": "decide",
         "pass": true
        },
        "OW-S8": {
         "audit": {
          "agree": true,
          "reason": "The diff only adds lines and changes one THEN line; no title is lost.",
          "verdict": "agrees"
         },
         "desc": "every existing requirement and scenario title kept",
         "detail": "gone: []",
         "kind": "decide",
         "pass": true
        },
        "OW-S9": {
         "audit": {
          "agree": true,
          "reason": "The added requirement `Amounts carry a dollar sign` says SHALL.",
          "verdict": "agrees"
         },
         "desc": "a requirement added whose text says SHALL",
         "detail": "added: ['reporting § Amounts carry a dollar sign']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "OW-E1": "pass",
        "OW-E10": "fail",
        "OW-E11": "pass",
        "OW-E12": "pass",
        "OW-E13": "pass",
        "OW-E14": "pass",
        "OW-E15": "pass",
        "OW-E16": "pass",
        "OW-E2": "pass",
        "OW-E3": "pass",
        "OW-E4": "pass",
        "OW-E5": "pass",
        "OW-E6": "pass",
        "OW-E7": "pass",
        "OW-E8": "pass",
        "OW-E9": "fail"
       },
       "judge": {
        "OW-E1": {
         "quotes": [
          "\"branch\": \"dollar-amounts\", \"worktree\": \".../.worktrees/tally/dollar-amounts\" ... new file mode 100644 ... docs/changes/dollar-amounts/intent.org",
          "refs/heads/dollar-amounts 1d0b7407470e3770522d637b2237f6049caa172e",
          "refs/heads/dollar-amounts 1d0b7407... ; new file mode 100644 docs/changes/dollar-amounts/intent.org"
         ],
         "reasons": [
          "The change, its branch and its directory all carry the name `dollar-amounts`, and the directory was created new by the opening commit.",
          "The branch, the change record and docs/changes/dollar-amounts/intent.org all carry the user's name, and neither the branch nor the directory existed at the fixture.",
          "The branch, the change state and the change directory all carry the name dollar-amounts, and the directory is a new file in the commit, absent at the fixture."
         ]
        },
        "OW-E10": {
         "quotes": [
          "Requirement: Amounts carry a dollar sign",
          "every category total included",
          "every category total included"
         ],
         "reasons": [
          "The glossary term category total is used and the glossary is unchanged, but the domain term the agent keeps (dollar sign / currency mark), which the glossary lacks, is not listed under New terms.",
          "The glossary's 'category total' is used and the glossary is unchanged, but 'dollar sign' / 'currency mark' is kept without being listed under New terms for agreement.",
          "The intent and the spec use the glossary's term category total and the glossary is unchanged, but the domain term the agent keeps (dollar sign, for the user's \"currency mark\") is not in the glossary and is not listed under New terms."
         ]
        },
        "OW-E11": {
         "quotes": [
          "Recommendation: =-$12.05=, because it is the most common plain-text form ... The spec is written to it. Affects: reporting § A refund is shown as a negative amount.",
          "Recommendation: =-$12.05=, because it is the most common plain-text form and keeps the minus sign the ledger itself uses. The spec is written to it. Affects: reporting § A refund is shown as a negative amount.",
          "Recommendation: =-$12.05=, because it is the most common plain-text form ... The spec is written to it. Affects: reporting § A refund is shown as a negative amount."
         ],
         "reasons": [
          "The negative-amount question is an open question with three plain options, what the user would notice with each, a recommendation with its reason, and the scenario it affects; the spec is written to the recommendation, nothing of how to build is said, and no question is raised on a case the user never raised.",
          "The question gives three plain options with what the user would notice, a recommendation with its reason, and the scenario it affects; the spec is written to the recommendation, and no build detail or unraised case appears.",
          "The negative-amount question is an open question with three options described by what the user would notice, a recommendation with its reason, the spec written to it and the affected scenario named; it says nothing of how to build and raises no case the user never raised."
         ]
        },
        "OW-E12": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "No ask file was added and no tool call asks the user; the question waits in the intent.",
          "No ask file was added and no call asked the user; the question waits in the intent.",
          "No ask file was added and no tool call asks the user anything; the question waits in the intent."
         ]
        },
        "OW-E13": {
         "quotes": [
          "1d0b740 Open: dollar-amounts\ncoverage: 3 scenarios: 3 uncited",
          "1d0b740 Open: dollar-amounts\ncoverage: 3 scenarios: 3 uncited",
          "Open: dollar-amounts\n\ncoverage: 3 scenarios: 3 uncited"
         ],
         "reasons": [
          "One commit in the code root holds only the intent and `reporting.org`, and its message carries the coverage summary.",
          "One commit holds exactly the intent and reporting.org, and its message carries the coverage summary.",
          "One commit in the code root holds only intent.org and reporting.org, and its message carries the coverage summary."
         ]
        },
        "OW-E14": {
         "quotes": [
          "reporting § Categories are ordered by total ... reporting § A refund is shown as a negative amount",
          "reporting § Categories are ordered by total ... reporting § A refund is shown as a negative amount",
          "reporting § Categories are ordered by total ... reporting § A refund is shown as a negative amount"
         ],
         "reasons": [
          "Both scenarios the intent names exist in `reporting.org` at the branch tip.",
          "Both scenarios the intent names exist in reporting.org at the branch's tip.",
          "Both scenarios the intent names exist in reporting.org at the branch's tip."
         ]
        },
        "OW-E15": {
         "quotes": [
          "== Commits in the home since the fixture ==\n(none)",
          "== Commits in the home since the fixture ==\n(none)",
          "Commits in the home since the fixture ==\n(none)"
         ],
         "reasons": [
          "There is no commit in the home, no `design.org` and no task file; the only file added in the home is `change.json`.",
          "There is no home commit, no design.org and no task file; the only file added in the home is the change.json record.",
          "Nothing is committed in the home, and no design.org or task file was written."
         ]
        },
        "OW-E16": {
         "quotes": [
          "open dollar-amounts: 1 capabilities, 3 scenarios, 1 open questions; .worktrees/tally/dollar-amounts/docs/changes/dollar-amounts/intent.org",
          "open dollar-amounts: 1 capabilities, 3 scenarios, 1 open questions; .worktrees/tally/dollar-amounts/docs/changes/dollar-amounts/intent.org",
          "open dollar-amounts: 1 capabilities, 3 scenarios, 1 open questions; .worktrees/tally/dollar-amounts/docs/changes/dollar-amounts/intent.org"
         ],
         "reasons": [
          "The hand-back is one line in the required form, the path is relative to the home through the worktree, and the counts match the intent: one capability, three scenarios, one question.",
          "It is one line of 138 characters in the required form; the counts match the intent (1 capability, 2 edited scenarios plus 1 stated-not-met, 1 question) and the path is relative to the home through the worktree.",
          "The hand-back is one line in the required form, its path is relative to the home through the code root, and its counts match the intent (reporting; three scenarios; one question)."
         ]
        },
        "OW-E2": {
         "quotes": [
          "\"repo\": \"tally\" ... \"base\": \"bfb660bd3d0266f033080dd4472762be05756d96\" ... #+BASE: bfb660bd3d0266f033080dd4472762be05756d96",
          "#+BASE: bfb660bd3d0266f033080dd4472762be05756d96",
          "\"base\": \"bfb660bd3d0266f033080dd4472762be05756d96\" ... #+BASE: bfb660bd3d0266f033080dd4472762be05756d96"
         ],
         "reasons": [
          "The branch is in the home's only repository and starts from main's commit `bfb660bd`, which the intent's `#+BASE:` names.",
          "The branch is in tally, the home's only repository, and starts from main's commit bfb660bd, which the intent's BASE names.",
          "The branch is in tally, the home's one repository, and starts from main's commit bfb660bd, which the intent's #+BASE names."
         ]
        },
        "OW-E3": {
         "quotes": [
          "They report that \"the amounts are rendered in one place, so this should be a small change\" (a claim about the code, for the round to check).",
          "They report that \"the amounts are rendered in one place, so this should be a small change\" (a claim about the code, for the round to check).",
          "(a claim about the code, for the round to check)"
         ],
         "reasons": [
          "The reads were state output, the flow, the glossary, the spec and the templates, never a code file, and the Why carries the user's claim as unchecked.",
          "Only the skill files, the glossary, the spec and the templates were read, with no code, and the Why carries the claim as the user's, unchecked.",
          "The main thread read only skill files, the glossary and the spec, no file of the code, and the Why carries the one-place rendering claim as the user's, unchecked."
         ]
        },
        "OW-E4": {
         "quotes": [
          "The report SHALL write every amount in the table with a dollar sign before the number ... *WHEN* a ledger has food 13.05 and rent 900.00 - *THEN* the table shows food as =$13.05= and rent as =$900.00=",
          "The report SHALL write every amount in the table with a dollar sign before the number ... *THEN* the table shows food as =$13.05= and rent as =$900.00=",
          "The report SHALL write every amount in the table with a dollar sign before the number ... *THEN* the table shows food as =$13.05= and rent as =$900.00="
         ],
         "reasons": [
          "The new requirement says SHALL, and its scenarios put a ledger in and show the table out, naming nothing of the code.",
          "The new requirement says what the table SHALL do, and its scenarios go from a ledger to the table's output with no code symbol.",
          "The new requirement says what the table SHALL do, and its scenarios go from a ledger in to the table out, naming nothing of the code."
         ]
        },
        "OW-E5": {
         "quotes": [
          "diff --git a/docs/specs/reporting.org b/docs/specs/reporting.org",
          "diff --git a/docs/specs/reporting.org b/docs/specs/reporting.org ... @@ -14,4 +14,11 @@",
          "@@ -14,4 +14,11 @@ The report SHALL give one total per category, largest first."
         ],
         "reasons": [
          "The reporting spec is edited in place in spec form, with no separate changes file, and every existing requirement and scenario title is kept.",
          "reporting.org is edited in place in the spec form, there is no separate changes file, and every existing title is kept.",
          "reporting.org is edited in spec form with no separate file of changes, and the diff keeps every existing requirement and scenario title."
         ]
        },
        "OW-E6": {
         "quotes": [
          "reporting § Categories are ordered by total: stated, not met",
          "- reporting § Categories are ordered by total: stated, not met",
          "reporting § Categories are ordered by total: stated, not met"
         ],
         "reasons": [
          "The ordering scenario's text is untouched in the diff, and Capabilities names it `stated, not met`.",
          "The ordering scenario's text is unchanged, and Capabilities names it stated, not met.",
          "The ordering scenario's text is untouched in the diff, and Capabilities names it stated, not met."
         ]
        },
        "OW-E7": {
         "quotes": [
          "#+STATUS: speculated",
          "#+STATUS: speculated",
          "#+STATUS: speculated"
         ],
         "reasons": [
          "The intent is at `docs/changes/dollar-amounts/intent.org` in the code root with status speculated and no `#+APPROVED:` line.",
          "The intent is at docs/changes/dollar-amounts/intent.org in the code root, with status speculated and no APPROVED line.",
          "The intent is at docs/changes/dollar-amounts/intent.org in the code root with status speculated and no #+APPROVED: line."
         ]
        },
        "OW-E8": {
         "quotes": [
          "The user asked: \"Every amount in the totals table should carry the currency mark, a dollar sign before the number, the sum for each category included.\"",
          "The user asked: \"Every amount in the totals table should carry the currency mark, a dollar sign before the number, the sum for each category included.\"",
          "The user asked: \"Every amount in the totals table should carry the currency mark, a dollar sign before the number...\""
         ],
         "reasons": [
          "The Why quotes the user's own words for what prompted the change.",
          "The Why quotes the user's own words for what prompted the change.",
          "The Why quotes the user's own words for what prompted the change."
         ]
        },
        "OW-E9": {
         "quotes": [
          "* Capabilities ... * Open Questions",
          "* Capabilities\n- reporting: every amount in the table carries a dollar sign",
          "* Capabilities ... * Open Questions"
         ],
         "reasons": [
          "Why, Goals, Non-goals, Capabilities (naming `reporting`) and Open Questions are present, but there is no New terms section, though the words give it an entry (currency mark / dollar sign, which the glossary lacks).",
          "The user's words call for a New terms entry (the glossary has nothing for the currency mark or dollar sign the intent uses), but the intent has no New terms section.",
          "Why, Goals, Non-goals, Capabilities (naming reporting) and Open Questions are all there, but the New terms section is missing even though the words give an entry for it (the currency mark)."
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
   "step": "the opening phase agent, from its brief (the source the user's words), to its hand-back",
   "title": "Case opening-words: the opening from the user's words, in a home with one repository and a glossary",
   "trials": 3,
   "validated": "0760f532e3d0a20b7f5629cf38b34b75135964a4 20261114T112826-1 t1 (skill-evals)"
  }
 ],
 "source": {
  "evals_head": "589177f",
  "spec": "orchestrate/docs/specs/opening.org",
  "spec_dirty": false,
  "worktree_head": "73b676c"
 },
 "spec": {
  "gaps": "No gap open. The nine this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/opening.org= § Gap decisions).",
  "purpose": "Opening turns what the user asked for into a change the design round can take: the change's intent and its edits to the specs, written from the user's words (or from items of the cross-change backlog, or from a goal-drift answer) and committed in the change's code root, on an integration branch of its own. It also revises a change in place when the user's answer calls for new goals. It reads no code and writes no design and no task, and it asks the user nothing: its open questions and its new terms wait in the intent for the design round's sitting, which is also where the user approves the goals. It hands the dispatcher one line.",
  "requirements": [
   {
    "goal": "G1 (inferred, from O-OP-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user has committed a change's intent and its spec edits themselves"
       },
       {
        "kw": "THEN",
        "text": "no =Open: <change>= commit is made for the change"
       },
       {
        "kw": "AND",
        "text": "the intent and the spec edits are as the user wrote them"
       }
      ],
      "test": null,
      "title": "A change the user wrote is left as written"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user opened a change by hand, its intent already committed on a branch of its own"
       },
       {
        "kw": "THEN",
        "text": "that branch is the change's integration branch"
       },
       {
        "kw": "AND",
        "text": "no intent and no spec edit is written for the change"
       },
       {
        "kw": "AND",
        "text": "the dispatcher receives one line of at most 200 characters, =adopt <change>; = followed by the intent's path relative to the home"
       }
      ],
      "test": null,
      "title": "A change opened by hand is adopted as it is"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user asks for work that a task of an open change already covers"
       },
       {
        "kw": "THEN",
        "text": "no change is opened for it"
       },
       {
        "kw": "AND",
        "text": "the dispatcher receives one line of at most 200 characters, =open: covered by <change>, task <task>; <task file>=, the path relative to the home"
       }
      ],
      "test": null,
      "title": "Work an open change's task covers opens no change"
     }
    ],
    "text": "Opening SHALL write a change's intent and spec edits only when the user has not written them, and SHALL open no change for work that a task of an open change covers.",
    "title": "Opening writes a change only where the user has not"
   },
   {
    "goal": "G1 (inferred, from O-OP-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user names the change"
       },
       {
        "kw": "THEN",
        "text": "the change, its integration branch and its documents' directory =docs/changes/<change>/= carry that name"
       }
      ],
      "test": null,
      "title": "A change takes the name the user gives it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user gives the change no name"
       },
       {
        "kw": "THEN",
        "text": "its name is a kebab-case phrase naming the outcome the change is for"
       }
      ],
      "test": null,
      "title": "An unnamed change is named for its outcome"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a new change is opened"
       },
       {
        "kw": "THEN",
        "text": "=docs/changes/<change>/= did not exist in its repository before the opening"
       }
      ],
      "test": null,
      "title": "A new change's name is new to its repository"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change's name is the opening agent's own and =docs/changes/<change>/= already exists in the repository"
       },
       {
        "kw": "THEN",
        "text": "the change is given another name, not yet used"
       },
       {
        "kw": "AND",
        "text": "the hand-back names the change by the name it was given"
       }
      ],
      "test": "none: the name is the agent's own choice, so no fixture can make the name it chooses be taken",
      "title": "A name the agent chose that is taken is changed to an unused one"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user named the change and =docs/changes/<change>/= already exists in the repository, and the change is not being revised in place"
       },
       {
        "kw": "THEN",
        "text": "opening hands back =open: failed, name taken: <change>="
       },
       {
        "kw": "AND",
        "text": "it creates no branch and writes nothing"
       }
      ],
      "test": null,
      "title": "A name the user gave that is taken fails the opening"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the home holds one repository"
       },
       {
        "kw": "THEN",
        "text": "the change belongs to that repository"
       }
      ],
      "test": null,
      "title": "A home with one repository opens the change in it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user's words name one of the home's repositories"
       },
       {
        "kw": "THEN",
        "text": "the change belongs to that repository"
       }
      ],
      "test": null,
      "title": "The repository the user names takes the change"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the home holds several repositories and the user's words name none of them"
       },
       {
        "kw": "THEN",
        "text": "opening hands back =open: failed, which repository: <the repositories>="
       },
       {
        "kw": "AND",
        "text": "it creates no branch and writes nothing"
       }
      ],
      "test": null,
      "title": "With several repositories and none named, opening asks which"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "what the user asks for needs two repositories"
       },
       {
        "kw": "THEN",
        "text": "two changes are opened, one in each repository"
       },
       {
        "kw": "AND",
        "text": "the second change's intent names the first in its Why"
       }
      ],
      "test": null,
      "title": "Work that needs two repositories is two changes"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "what the user asks for needs two repositories"
       },
       {
        "kw": "THEN",
        "text": "the opening opens the first change only"
       },
       {
        "kw": "AND",
        "text": "its hand-back line ends =; second change: <repository>="
       }
      ],
      "test": null,
      "title": "Each opening opens one change, and names the second when there is one"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened and no ref is given to start it from"
       },
       {
        "kw": "THEN",
        "text": "its integration branch starts from the commit its repository is at"
       }
      ],
      "test": null,
      "title": "The integration branch starts from the repository's current commit"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the research skill's hand-off made a workspace integration branch for the change"
       },
       {
        "kw": "THEN",
        "text": "the change's integration branch starts from that workspace integration branch"
       }
      ],
      "test": null,
      "title": "A change after research starts from the research's workspace integration branch"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened"
       },
       {
        "kw": "THEN",
        "text": "the intent's =#+BASE:= line names the commit the change's integration branch started from"
       }
      ],
      "test": null,
      "title": "The intent carries the change's base"
     }
    ],
    "text": "Opening SHALL give a new change a name not yet used in its repository, place the change in exactly one repository, and start its integration branch, named for the change, from the commit the repository is at or from the ref it is given; the intent SHALL carry that commit as the change's base.",
    "title": "A new change is named, placed in one repository and started from a written base"
   },
   {
    "goal": "G1 (inferred, from O-OP-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened"
       },
       {
        "kw": "THEN",
        "text": "the opening phase agent reads no file of the code"
       }
      ],
      "test": null,
      "title": "Opening reads no code"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user's words make a claim about the code"
       },
       {
        "kw": "THEN",
        "text": "the intent's Why states the claim as the user's words state it"
       }
      ],
      "test": null,
      "title": "A claim about the code reaches the Why as stated"
     }
    ],
    "text": "Opening SHALL write the intent and the spec edits without reading the code; a claim the user makes about the code SHALL reach the intent as the user states it, for the design round to check.",
    "title": "Opening works from the user's words, never from the code"
   },
   {
    "goal": "G1 (inferred, from O-OP-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change adds or alters a behaviour"
       },
       {
        "kw": "THEN",
        "text": "the spec of its capability holds a requirement stating what =The <subject> SHALL= do"
       },
       {
        "kw": "AND",
        "text": "its scenarios state what a user, an operator or a caller observes: the input and the output, the file left behind, the message shown"
       }
      ],
      "test": null,
      "title": "An added or altered behaviour is a requirement with observable scenarios"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening writes a scenario"
       },
       {
        "kw": "THEN",
        "text": "the scenario names no symbol, guard or encoding of the code"
       }
      ],
      "test": null,
      "title": "A scenario names no symbol, guard or encoding"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening edits a capability's spec"
       },
       {
        "kw": "THEN",
        "text": "it edits that capability's file under =docs/specs/=, in the spec form"
       },
       {
        "kw": "AND",
        "text": "it writes no separate file of changes to the spec"
       }
      ],
      "test": null,
      "title": "The specs are edited in place"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change adds a capability no spec states"
       },
       {
        "kw": "THEN",
        "text": "a new spec file under =docs/specs/= holds it"
       }
      ],
      "test": null,
      "title": "A new capability is a new spec file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user reports broken a behaviour that a spec already states rightly"
       },
       {
        "kw": "THEN",
        "text": "that spec's scenario is not edited"
       },
       {
        "kw": "AND",
        "text": "the intent's Capabilities names the scenario as =stated, not met="
       }
      ],
      "test": null,
      "title": "A broken behaviour the spec states rightly is named, not edited"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening runs on a research hand-off, its change cut from the hand-off's workspace integration branch"
       },
       {
        "kw": "THEN",
        "text": "the intent's Capabilities names, one line each, every scenario the hand-off's spec edits added or changed for the change, as =stated, not met="
       },
       {
        "kw": "AND",
        "text": "the intent carries no =#+APPROVED:= line until the user approves its goals"
       }
      ],
      "test": null,
      "title": "A change from a research hand-off names the hand-off's scenarios"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change removes a behaviour"
       },
       {
        "kw": "THEN",
        "text": "its requirement or scenario is deleted from the spec"
       },
       {
        "kw": "AND",
        "text": "the intent's Removals names it with the reason it goes"
       }
      ],
      "test": null,
      "title": "A removed behaviour is deleted and its reason given"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening edits a spec"
       },
       {
        "kw": "THEN",
        "text": "no existing requirement or scenario title is renamed"
       }
      ],
      "test": null,
      "title": "Existing titles are kept"
     }
    ],
    "text": "Opening SHALL edit the specs in place, in the spec form: each behaviour the change adds or alters as a requirement whose scenarios state what a user, an operator or a caller observes, and nothing of how it is built. It SHALL keep every existing title, and SHALL give each removal's reason in the intent.",
    "title": "The spec edits state what is observed, in place"
   },
   {
    "goal": "G1 (inferred, from O-OP-1); G2 (from O-NS-2)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened"
       },
       {
        "kw": "THEN",
        "text": "its intent is =docs/changes/<change>/intent.org= in the change's code root"
       },
       {
        "kw": "AND",
        "text": "its status line, the line =#+STATUS:=, is =speculated=; every later status the specs give the intent is written on that line"
       }
      ],
      "test": null,
      "title": "The intent is written with status speculated"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened from the user's words"
       },
       {
        "kw": "THEN",
        "text": "the intent's Why says what prompted the change in the user's terms, quoting their words"
       }
      ],
      "test": null,
      "title": "The Why quotes the user"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened"
       },
       {
        "kw": "THEN",
        "text": "its intent has a Why, Goals, Non-goals and Capabilities"
       },
       {
        "kw": "AND",
        "text": "it has Removals, New terms and Open Questions only when each has an entry"
       }
      ],
      "test": null,
      "title": "The intent states its goals, non-goals and capabilities"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change edits a capability's spec"
       },
       {
        "kw": "THEN",
        "text": "the intent's Capabilities names that capability"
       }
      ],
      "test": null,
      "title": "Capabilities names each spec the change edits"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the intent or the spec edits name a thing the glossary has a term for"
       },
       {
        "kw": "THEN",
        "text": "they use the glossary's term"
       }
      ],
      "test": null,
      "title": "The intent and the spec edits use the glossary's terms"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the intent or the spec edits use a domain term the glossary does not have"
       },
       {
        "kw": "THEN",
        "text": "the intent's New terms lists it with what it is, an example from the user's own use and the words to avoid"
       }
      ],
      "test": null,
      "title": "A term the glossary lacks is listed for agreement"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened"
       },
       {
        "kw": "THEN",
        "text": "the glossary is not changed"
       }
      ],
      "test": null,
      "title": "Opening leaves the glossary as it is"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening writes an intent"
       },
       {
        "kw": "THEN",
        "text": "the intent carries no =#+APPROVED:= line"
       }
      ],
      "test": null,
      "title": "An opened intent awaits the user's approval"
     }
    ],
    "text": "Opening SHALL write the change's intent in the intent form, with status =speculated=: why the change exists in the user's terms, its goals and non-goals, the capabilities it touches, and its removals and new terms when it has them, in the glossary's terms. It SHALL leave the approval of the goals and the agreement of new terms to the user.",
    "title": "The intent states why the change exists and what it commits to"
   },
   {
    "goal": "G1 (inferred, from O-OP-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user's words leave to the user a question the spec text depends on"
       },
       {
        "kw": "THEN",
        "text": "the intent's Open Questions holds it: the question in plain terms, two to four options each with the consequence the user would notice, a recommendation and why, and the requirements and scenarios it affects"
       }
      ],
      "test": null,
      "title": "A question the spec depends on is an open question"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the intent holds an open question"
       },
       {
        "kw": "THEN",
        "text": "the spec text is written in the form its recommendation implies"
       },
       {
        "kw": "AND",
        "text": "the open question says so"
       }
      ],
      "test": null,
      "title": "The spec is written to the recommendation"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the intent holds an open question"
       },
       {
        "kw": "THEN",
        "text": "the open question says nothing about how the change is to be built"
       }
      ],
      "test": null,
      "title": "An open question says nothing of how to build"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a question concerns a case the user's words never raised"
       },
       {
        "kw": "THEN",
        "text": "the intent's Open Questions does not hold it"
       }
      ],
      "test": null,
      "title": "A case the user never raised is no open question"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened"
       },
       {
        "kw": "THEN",
        "text": "no ask is filed for the change while the opening runs"
       },
       {
        "kw": "AND",
        "text": "its open questions wait in the intent for the design round's sitting"
       }
      ],
      "test": null,
      "title": "Opening asks the user nothing"
     }
    ],
    "text": "Opening SHALL write each question the user's words leave to the user, and the spec text depends on, as an open question in the intent, with its options and a recommendation the spec is written to; it SHALL write none for a case the user never raised, and SHALL put no question to the user.",
    "title": "A question left to the user waits in the intent"
   },
   {
    "goal": "G1 (inferred, from O-OP-1)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening completes"
       },
       {
        "kw": "THEN",
        "text": "one commit in the change's code root, subject =Open: <change>=, holds the intent and the spec files the change edits, and no other file"
       },
       {
        "kw": "AND",
        "text": "its message carries the coverage summary of the change's scenarios"
       }
      ],
      "test": null,
      "title": "One commit holds the intent and the spec edits"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening commits"
       },
       {
        "kw": "THEN",
        "text": "every scenario the intent names matches a scenario of the specs"
       }
      ],
      "test": null,
      "title": "Every scenario the intent names exists"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening completes"
       },
       {
        "kw": "THEN",
        "text": "the home has no commit made by the opening"
       }
      ],
      "test": null,
      "title": "Opening commits nothing in the home"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is opened from items of the cross-change backlog"
       },
       {
        "kw": "THEN",
        "text": "the intent's Why names each item by its path"
       },
       {
        "kw": "AND",
        "text": "each item's file is unchanged"
       }
      ],
      "test": null,
      "title": "An item of the cross-change backlog taken up is named, and left as it is"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening completes"
       },
       {
        "kw": "THEN",
        "text": "the change has no =design.org="
       },
       {
        "kw": "AND",
        "text": "no task file has been written for it"
       }
      ],
      "test": null,
      "title": "Opening writes no design and no task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening completes"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives one line of at most 200 characters, =open <change>: <c> capabilities, <s> scenarios, <q> open questions; = followed by the intent's path"
       }
      ],
      "test": null,
      "title": "The hand-back names the change, its counts and the intent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening hands back that it completed"
       },
       {
        "kw": "THEN",
        "text": "the intent's path in the line is relative to the home, through the change's code root"
       }
      ],
      "test": null,
      "title": "The hand-back's intent path is relative to the home"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening revises a change in place"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives one line of at most 200 characters, =revise <change>: <c> capabilities, <s> scenarios, <q> open questions; = followed by the intent's path relative to the home"
       }
      ],
      "test": null,
      "title": "A revision hands back a line of its own"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening hands back that it completed"
       },
       {
        "kw": "THEN",
        "text": "the line's capabilities are the number the intent's Capabilities names, its scenarios the number of scenarios the change's spec edits add or change together with those the intent names =stated, not met=, a scenario it deletes not counted, and its open questions the number of the intent's open questions"
       }
      ],
      "test": null,
      "title": "The hand-back's counts are the intent's"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening cannot complete"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives the line =open: failed, <why>="
       }
      ],
      "test": null,
      "title": "A failed opening says why"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening hands back =open: failed="
       },
       {
        "kw": "THEN",
        "text": "the repository has no branch or worktree the opening made"
       },
       {
        "kw": "AND",
        "text": "no change of that name exists in the home, and nothing the opening wrote is left uncommitted or committed"
       }
      ],
      "test": null,
      "title": "A failed opening leaves nothing behind"
     }
    ],
    "text": "Opening SHALL commit the intent and the spec edits in the change's code root in one commit, after checking that every scenario the intent names exists; it SHALL commit nothing in the home, write no design and no task, and hand the dispatcher one line.",
    "title": "Opening ends in one commit in the code root and one line"
   },
   {
    "goal": "G2 (from O-NS-2)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user answers =rethink= on the MVP decision or =revise= on a goal-drift ask"
       },
       {
        "kw": "THEN",
        "text": "the change keeps its name, its integration branch, its code root and its intent's =#+BASE:="
       }
      ],
      "test": null,
      "title": "A revised change keeps its name, branch and base"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is revised on the user's answer"
       },
       {
        "kw": "THEN",
        "text": "its Goals, Non-goals and spec edits are rewritten from the answer"
       }
      ],
      "test": null,
      "title": "A revision rewrites the goals and the spec edits from the answer"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is revised on the user's answer"
       },
       {
        "kw": "THEN",
        "text": "the prior Goals are quoted under the intent's =* Status="
       },
       {
        "kw": "AND",
        "text": "the status line is =speculated= again"
       }
      ],
      "test": null,
      "title": "A revision keeps the prior goals under Status"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change is revised on the user's answer"
       },
       {
        "kw": "THEN",
        "text": "the intent has no =#+APPROVED:= line, so the design round's sitting asks the user to approve the rewritten goals"
       }
      ],
      "test": null,
      "title": "A revision removes the goals' approval"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a revision completes"
       },
       {
        "kw": "THEN",
        "text": "the change's code root has a commit with subject =Revise: <change>="
       }
      ],
      "test": null,
      "title": "A revision is one commit Revise"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user answers =split= on a goal-drift ask"
       },
       {
        "kw": "THEN",
        "text": "a new change is opened from the old intent and the answer"
       },
       {
        "kw": "AND",
        "text": "the new intent's Why names the change it came from"
       }
      ],
      "test": null,
      "title": "A split opens a new change that names the old"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "opening opens a new change on a =split= answer"
       },
       {
        "kw": "THEN",
        "text": "it leaves the old change's intent as it was"
       },
       {
        "kw": "AND",
        "text": "its hand-back line ends =; revise: <old change>="
       }
      ],
      "test": null,
      "title": "A split's opening names the old change for revision"
     }
    ],
    "text": "On the user's =rethink= on the MVP decision or =revise= on a goal-drift ask, opening SHALL revise the change in place: rewrite its goals, non-goals and spec edits from the answer, keep the prior goals in the intent, and return the status to =speculated=, keeping the change's name, branch and base. On =split= it SHALL open a new change that names the one it came from.",
    "title": "A change is revised in place, or split, from the user's answer"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); nine gaps opened; at the hand-off review 2026-11-13: G8 applied (the path's root, a revision's line); the unnamed repository, a taken name, a failed opening, the second change, the research hand-off, the split's old change and the revision's approval filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-14 after acceptance (the intent's status line named; the scenarios the hand-back counts; lines for covered work and an adopted change; a taken name the agent chose marked not test-worthy), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning)",
  "title": "opening"
 },
 "summary": {
  "cases": 11,
  "cited": 56,
  "fail": 5,
  "pass": 51,
  "scenarios": 57,
  "title": "opening",
  "tree": "skill-evals",
  "validation": 11
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
