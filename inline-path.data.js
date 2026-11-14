window.SPEC_PAGE = {
 "capability": "inline-path",
 "cases": [
  {
   "capability": "inline-path",
   "id": "inline-path-batch-task",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      },
      {
       "id": "IP-S5",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "decide"
      },
      {
       "id": "IP-S8",
       "kind": "decide"
      },
      {
       "id": "IP-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A task already in a batch is not taken on the inline path"
     ],
     "id": "IP-E1",
     "question": "Is the batch's task left to its cycle, untouched by the inline path?"
    },
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A request the triage refuses is a bailout with nothing to revert",
      "inline-path § A bailout leaves the code root as it was"
     ],
     "id": "IP-E2",
     "question": "Is the refusal a bailout with nothing to revert, its reason in an open task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S7",
       "kind": "decide"
      },
      {
       "id": "IP-S9",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § A bailout hands back one line naming the task and why"
     ],
     "id": "IP-E3",
     "question": "Does the path hand back one line naming the task and why?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "742103b",
     "cites": {
      "IP-E1": [
       "inline-path § A task already in a batch is not taken on the inline path"
      ],
      "IP-E2": [
       "inline-path § A request the triage refuses is a bailout with nothing to revert",
       "inline-path § A bailout leaves the code root as it was"
      ],
      "IP-E3": [
       "inline-path § A bailout hands back one line naming the task and why"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1",
       "IP-S10",
       "IP-S2",
       "IP-S3",
       "IP-S4",
       "IP-S5",
       "IP-S6",
       "IP-S8"
      ],
      "IP-E2": [
       "IP-S1",
       "IP-S2",
       "IP-S6"
      ],
      "IP-E3": [
       "IP-S7",
       "IP-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T102303",
     "trials": [
      {
       "calls": {},
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "No branch moved, added or removed, which matches the empty commit evidence and the 'no commit' part of both items.",
          "verdict": "agrees"
         },
         "desc": "no commit in the code repository: every branch where the build left it, none added",
         "detail": "branches moved, added or removed: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "The task file's text is unchanged, which is IP-E1's 'no note added' condition, and no home file is shown as changed.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments's task file as the build left it (no note added)",
         "detail": "unchanged",
         "kind": "decide",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "Clean working trees in the code root and the task worktrees are what both items expect, and the git status evidence is empty.",
          "verdict": "agrees"
         },
         "desc": "the code root's and each task worktree's working tree as the build left them",
         "detail": "all as they were",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The before and after worktree lists in the evidence are identical.",
          "verdict": "agrees"
         },
         "desc": "no worktree added or removed",
         "detail": "before ['/tmp/skill-evals/20261114T102303/001/home/.worktrees/tally/json-report', '/tmp/skill-evals/20261114T102303/001/home/.worktrees/tally/task-dollar-table-1793000002', '/tmp/skill-evals/20261114T102303/001/home/.worktrees/tally/task-ledger-comments-1793000003', '/tmp/skill-evals/20261114T102303 … [cut, 647 characters]",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "None of the main thread's three tool calls is an Agent call, and no spawns were recorded.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "ledger-comments' state entry is unchanged and no entry was added, as IP-E1 requires, consistent with no home files changing.",
          "verdict": "agrees"
         },
         "desc": "the batch's entries in the cycle's state as the build left them, and no entry added",
         "detail": "entries changed: none; added: none; ledger-comments: {\"status\": \"completed\", \"merge_commit\": null, \"branch_name\": \"task-ledger-comments-1793000003\"}",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The task file still being in tasks/open/ is exactly the 'task stays open' and 'file's place' condition.",
          "verdict": "agrees"
         },
         "desc": "ledger-comments's task file still in the change's tasks/open/",
         "detail": ".orchestrator/changes/json-report/tasks/open/ledger-comments.md exists: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "The 170-character final line matches the required form, task and path, which is what IP-E3 expects.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: bailed out, <why>; <task file>`, one line of at most 200 characters, the task ledger-comments and the path its own task file, relative to the home (not applicable when stopped at a spawn)",
         "detail": "170 characters: inline ledger-comments: bailed out, task is already in cycle-1's batch (status completed, not yet merged); .orchestrator/changes/json-report/tasks/open/ledger-comments.md",
         "kind": "decide",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "No task file or candidate file was added, consistent with the empty list of home files changed and with IP-E1's 'no task file written from the request'.",
          "verdict": "agrees"
         },
         "desc": "no task file added and no follow-up candidate",
         "detail": "task files added: none; candidate files: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "The reason contains 'cycle' and 'batch', which signals the batch reason that IP-E3 expects.",
          "verdict": "agrees"
         },
         "desc": "the line's reason names the batch or its cycle",
         "detail": "task is already in cycle-1's batch (status completed, not yet merged)",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "== The home's commits since the build (subject, files) ==\n(none)",
          "Agents spawned (in order), each input in full ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "Nothing in the code repository or the home changed: no commits, no files, no worktree added or removed, and no Agent call; the only calls were a status read, a doc read, a read-only find and the scratch directory's create and delete.",
          "The evidence shows no commits on the change's branch or in the home, no changed or added files in the home (so state.json and the task file are untouched), the same worktree list before and after, and no Agent calls; the only calls were a status read, a doc read and a read-only find plus scratch cleanup.",
          "The evidence shows no commits on the branch or in the home, no changed files, an identical worktree list, no Agent call, and only read-only calls besides the scratch dir's create and delete; the script's checks confirm the state entry and the task file are untouched and no task file was added."
         ]
        },
        "IP-E2": {
         "quotes": [
          "inline ledger-comments: bailed out, task is already in cycle-1's batch (status completed, not yet merged); .orchestrator/changes/json-report/tasks/open/ledger-comments.md",
          "inline ledger-comments: bailed out, task is already in cycle-1's batch (status completed, not yet merged); .orchestrator/changes/json-report/tasks/open/ledger-comments.md",
          "inline ledger-comments: bailed out, task is already in cycle-1's batch (status completed, not yet merged); .orchestrator/changes/json-report/tasks/open/ledger-comments.md"
         ],
         "reasons": [
          "The code root's working tree is unchanged, the task's file is still in tasks/open/, and the path handed back its bailout line with the reason in the line, not in a note.",
          "The code root's git status is empty, the task file stays unchanged in tasks/open/, and the session hands back the bailout line carrying the reason rather than adding a note.",
          "The code root's git status is empty, the task file stays in tasks/open/ unchanged, and the path handed back its bailout line with the reason in the line rather than a note."
         ]
        },
        "IP-E3": {
         "quotes": [
          "inline ledger-comments: bailed out, task is already in cycle-1's batch (status completed, not yet merged); .orchestrator/changes/json-report/tasks/open/ledger-comments.md",
          "inline ledger-comments: bailed out, task is already in cycle-1's batch (status completed, not yet merged); .orchestrator/changes/json-report/tasks/open/ledger-comments.md",
          "inline ledger-comments: bailed out, task is already in cycle-1's batch (status completed, not yet merged); .orchestrator/changes/json-report/tasks/open/ledger-comments.md"
         ],
         "reasons": [
          "The last text is one line of the required form: it names ledger-comments, says it is in cycle-1's batch as the reason, and gives the task's own file relative to the home.",
          "The final text is one 170-character line in the required form; it names ledger-comments, gives being in cycle-1's batch as the reason, and ends with the task's own file path relative to the home.",
          "The final text is one 170-character line in the required form, naming ledger-comments, giving cycle-batch membership as the why, and giving its own task file relative to the home."
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-batch-task: a task of the open cycle's batch, not taken on the inline path",
   "trials": 3,
   "validated": "742103b 20261114T102303 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-design-question",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      },
      {
       "id": "IP-S10",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § A design in question is a decision ask that holds the commit",
      "asks § An ask raised with no open cycle is named for its change",
      "asks § An unsettled ask is in the change's inbox",
      "asks § An ask file opens with its id, kind and status, and its title is the question",
      "asks § An ask's kind is one of a closed set",
      "asks § A new ask is committed with a subject naming its id"
     ],
     "id": "IP-E1",
     "question": "Is the design in question a decision ask, filed in the change's inbox and committed?"
    },
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S9",
       "kind": "decide"
      },
      {
       "id": "IP-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A design in question is a decision ask that holds the commit"
     ],
     "id": "IP-E2",
     "question": "Does the ask hold the edit's commit, with nothing spawned?"
    },
    {
     "checks": [
      {
       "id": "IP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § Stopped on an ask, the inline path's line names it",
      "inline-path § A design in question is a decision ask that holds the commit"
     ],
     "id": "IP-E3",
     "question": "Does the path hand back that it stopped, naming the ask, without putting the question to the user?"
    },
    {
     "checks": [
      {
       "id": "IP-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E4",
     "question": "Is the request's task written and left open while the ask waits?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "3872349",
     "cites": {
      "IP-E1": [
       "inline-path § A design in question is a decision ask that holds the commit",
       "asks § An ask raised with no open cycle is named for its change",
       "asks § An unsettled ask is in the change's inbox",
       "asks § An ask file opens with its id, kind and status, and its title is the question",
       "asks § An ask's kind is one of a closed set",
       "asks § A new ask is committed with a subject naming its id"
      ],
      "IP-E2": [
       "inline-path § A design in question is a decision ask that holds the commit"
      ],
      "IP-E3": [
       "inline-path § Stopped on an ask, the inline path's line names it",
       "inline-path § A design in question is a decision ask that holds the commit"
      ],
      "IP-E4": [
       "inline-path § The inline path writes its own task file from the request",
       "record § The change's directory holds its files under fixed names"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S10",
       "IP-S2",
       "IP-S3",
       "IP-S4"
      ],
      "IP-E2": [
       "IP-S1",
       "IP-S7",
       "IP-S9"
      ],
      "IP-E3": [
       "IP-S5"
      ],
      "IP-E4": [
       "IP-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T101953",
     "trials": [
      {
       "calls": {
        "8": {
         "error": false,
         "text": "cd <trial>/home; H=.orchestrator/changes/json-report; mkdir -p $H/tasks/open $H/inbox\ncat > $H/inbox/ask-json-report-1.org <<'EOF'\n:PROPERTIES:\n:ID: ask-json-report-1\n:KIND: decision\n:STATUS: open\n:RAISED_BY: orchestrator\n:REF: inline-accept-amounts-without-cents\n:HAPPENED: no\n:BLOCKS: inline-accept-amounts-without-cents\n:END:\n#+TITLE: Should tally accept a ledger amount written without cents, such as 12 for 12.00?\n\n* About\n\nA ledger line is `date,category,amount`. Today tally refuses a line whose amount has no two-digit cents, and the design records that as a deliberate rule (D6, made in the  … [cut, 3442 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "The only added commit touches files under .orchestrator/ and no branch was removed, so 'no code commits' fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "no commit in the code repository changes a file outside the home's .orchestrator/ (no code committed), and no branch removed",
         "detail": "code commits: none; branches removed: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "The ask's About line names D6 and the cents rule, so the signal fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the ask names the design it puts in question (D6, or an amount's cents)",
         "detail": ":REF: inline-accept-amounts-without-cents | :BLOCKS: inline-accept-amounts-without-cents | #+TITLE: Should tally accept a ledger amount written without cents, such as 12 for 12.00? | A ledger line is `date,category,amount`. Today tally refuses a line whose amount has no two-digit cents, and the desi … [cut, 854 characters]",
         "kind": "signal",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "Exactly one ask file was added, at the expected inbox path with the change-named id.",
          "verdict": "agrees"
         },
         "desc": "one ask file added, in the change's inbox, named ask-json-report-<n>.org",
         "detail": "ask files added: ['.orchestrator/changes/json-report/inbox/ask-json-report-1.org']",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The drawer has ID equal to the file name, KIND decision and STATUS open, and the title ends with a question mark.",
          "verdict": "agrees"
         },
         "desc": "the ask file opens with a property drawer holding its id (the file's name), its kind decision and its status, and its title is a question",
         "detail": "drawer: {\"ID\": \"ask-json-report-1\", \"KIND\": \"decision\", \"STATUS\": \"open\", \"RAISED_BY\": \"orchestrator\", \"REF\": \"inline-accept-amounts-without-cents\", \"HAPPENED\": \"no\", \"BLOCKS\": \"inline-accept-amounts-without-cents\"}; title: Should tally accept a ledger amount written without cents, such as 12 for 12 … [cut, 304 characters]",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "The file is at HEAD and the home commit's subject names ask-json-report-1.",
          "verdict": "agrees"
         },
         "desc": "the ask file committed in the home, a commit's subject naming its id",
         "detail": "the file at the home's HEAD: True; home commits since the build: Inline task accept-amounts-without-cents: stopped, ask-json-report-1 (reverses D6)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "The final text matches the required form: task name, ask id and relative path, on one line under 200 characters.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: stopped, ask <id> blocks commit; <ask file>`, one line of at most 200 characters, the task its task file's, the id and path the added ask's, relative to the home (not applicable when stopped at a spawn)",
         "detail": "144 characters: inline accept-amounts-without-cents: stopped, ask ask-json-report-1 blocks commit; .orchestrator/changes/json-report/inbox/ask-json-report-1.org",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "Exactly one task file was added under tasks/open/, matching IP-E4.",
          "verdict": "agrees"
         },
         "desc": "one task file added, in the change's tasks/open/",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/accept-amounts-without-cents.md']",
         "kind": "decide",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "The main thread's tool calls are only Bash and Read, with no Agent call, matching 'nobody spawned'.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "The worktree lists before and after are identical and no branch was added, which is what IP-E2 expects.",
          "verdict": "agrees"
         },
         "desc": "no worktree and no branch added",
         "detail": "worktrees before ['/tmp/skill-evals/20261114T101953/001/home'], after ['/tmp/skill-evals/20261114T101953/001/home']; branches added: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "pass",
        "IP-E4": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          ":ID: ask-json-report-1\n:KIND: decision\n:STATUS: open ... #+TITLE: Should tally accept a ledger amount written without cents, such as 12 for 12.00?",
          ":ID: ask-json-report-1\n:KIND: decision\n:STATUS: open ... #+TITLE: Should tally accept a ledger amount written without cents, such as 12 for 12.00?",
          ":ID: ask-json-report-1\n:KIND: decision\n:STATUS: open ... #+TITLE: Should tally accept a ledger amount written without cents, such as 12 for 12.00?"
         ],
         "reasons": [
          "One decision ask sits in the change's inbox. Its name and drawer ID are ask-json-report-1 (the change-named form, since no cycle is open), its title asks whether to accept amounts without cents, which means giving up D6 (or declining), and it is committed under the subject \"Inline task accept-amounts-without-cents: stopped, ask-json-report-1 (reverses D6)\".",
          "There is one decision ask, ask-json-report-1, in the change's inbox, named for its id; its title asks whether to accept amounts without cents against D6, and it was committed with the subject \"Inline task accept-amounts-without-cents: stopped, ask-json-report-1 (reverses D6)\".",
          "There is one ask in `.orchestrator/changes/json-report/inbox/`, named by its change-scoped id, of kind decision with a status, titled as the question of accepting amounts without cents against D6, and committed in the home under the subject \"Inline task accept-amounts-without-cents: stopped, ask-json-report-1 (reverses D6)\"."
         ]
        },
        "IP-E2": {
         "quotes": [
          "inline path: nothing edited.",
          "inline path: nothing edited.",
          "Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "The only commit adds two files under .orchestrator/ and no code. The worktree list is unchanged, no branch was added, and there were no Agent calls.",
          "The only commit touches .orchestrator files, the working tree is clean, the worktree list is unchanged, and no Agent call was made.",
          "The only commit touches `.orchestrator/` files, the code tree is clean, the worktree list is unchanged, and no Agent call was made, so no code commit landed and nothing was spawned."
         ]
        },
        "IP-E3": {
         "quotes": [
          "inline accept-amounts-without-cents: stopped, ask ask-json-report-1 blocks commit; .orchestrator/changes/json-report/inbox/ask-json-report-1.org",
          "inline accept-amounts-without-cents: stopped, ask ask-json-report-1 blocks commit; .orchestrator/changes/json-report/inbox/ask-json-report-1.org",
          "inline accept-amounts-without-cents: stopped, ask ask-json-report-1 blocks commit; .orchestrator/changes/json-report/inbox/ask-json-report-1.org"
         ],
         "reasons": [
          "The final text is the required single line of 144 characters, with the path relative to the home. The question appears only in the ask file, not in the session's text.",
          "The final text is the required one line, well under 200 characters, with the path relative to the home; the question is in the ask file, not in the session's text.",
          "The final text is the prescribed one-line stopped form with the task name, the ask id and its home-relative path, and the question appears only in the file."
         ]
        },
        "IP-E4": {
         "quotes": [
          "status: blocked\nblocker_note: ask-json-report-1 ... User request, verbatim: \"Accept amounts without cents (12 for 12.00).\"",
          "User request, verbatim: \"Accept amounts without cents (12 for 12.00).\"",
          ".orchestrator/changes/json-report/tasks/open/accept-amounts-without-cents.md ... status: blocked\nblocker_note: ask-json-report-1"
         ],
         "reasons": [
          "One task file, written from the request, was added under the change's tasks/open/ and left blocked on the ask.",
          "One task file, written from the request, was added under the change's tasks/open/ with status blocked on the ask.",
          "One task file was written from the verbatim request in `tasks/open/` and left blocked on the ask, not marked done."
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
          "IP-S2",
          "IP-S3",
          "IP-S4",
          "IP-S5"
         ],
         "hits": {
          "IP-S2": [
           "ask-json-report-1",
           "ask-json-report-1.org"
          ],
          "IP-S3": [
           "ask-json-report-1"
          ],
          "IP-S4": [
           "ask-json-report-1"
          ],
          "IP-S5": [
           "ask-json-report-1",
           "ask-json-report-1.org"
          ]
         },
         "label": "ask ask-json-report-1.org (decision): Should tally accept a ledger amount written without cents, such as 12 for 12.00?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-json-report-1.org",
         "scenarios": [],
         "sig": "ask, decision"
        }
       ],
       "n_calls": 8,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-design-question: an edit that puts the design in question, stopped on a decision ask",
   "trials": 3,
   "validated": "3872349 20261114T101953 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-done",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E1",
     "question": "Does the phase agent write the task's file from the user's request, in the change's tasks, and commit it?"
    },
    {
     "checks": [
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The edit is made in the code root, with no worktree"
     ],
     "id": "IP-E2",
     "question": "Is the edit made and committed in the code root, with no worktree for the task?"
    },
    {
     "checks": [
      {
       "id": "IP-S4",
       "kind": "decide"
      },
      {
       "id": "IP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § No Implementor or Reviewer is spawned",
      "inline-path § A one-file edit the user asks for between cycles takes the inline path"
     ],
     "id": "IP-E3",
     "question": "Is the request taken on the inline path, with no Implementor or Reviewer?"
    },
    {
     "checks": [
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § Between cycles, the task goes straight to done"
     ],
     "id": "IP-E4",
     "question": "Does the task go straight to done, with no cycle started and no review state passed through?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The code and the task file are committed together"
     ],
     "id": "IP-E5",
     "question": "Are the code and the task file committed together?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      },
      {
       "id": "IP-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The verification run's output is kept beside the task"
     ],
     "id": "IP-E6",
     "question": "Is the verification run's output kept beside the task, and its command named in the commit?"
    },
    {
     "checks": [
      {
       "id": "IP-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § What the edit exposed is answered in the task file"
     ],
     "id": "IP-E7",
     "question": "Does the task file say what was checked for what the edit exposed, and what was found?"
    },
    {
     "checks": [
      {
       "id": "IP-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A finished inline path hands back done with its commit"
     ],
     "id": "IP-E8",
     "question": "Does the path hand back one line naming the task, its commit and its task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S13",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path's commit has a subject of its own"
     ],
     "id": "IP-E9",
     "question": "Does the inline commit have the subject the spec gives it?"
    },
    {
     "checks": [
      {
       "id": "IP-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E10",
     "question": "Does the path write no task for what it finds?"
    },
    {
     "checks": [],
     "cites": [
      "inline-path § No Implementor or Reviewer is spawned"
     ],
     "id": "IP-E11",
     "question": "Does the phase agent review its own edit before committing it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "a4e438a",
     "cites": {
      "IP-E1": [
       "inline-path § The inline path writes its own task file from the request",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E10": [
       "inline-path § The inline path writes no task for what it finds"
      ],
      "IP-E11": [
       "inline-path § No Implementor or Reviewer is spawned"
      ],
      "IP-E2": [
       "inline-path § The edit is made in the code root, with no worktree"
      ],
      "IP-E3": [
       "inline-path § No Implementor or Reviewer is spawned",
       "inline-path § A one-file edit the user asks for between cycles takes the inline path"
      ],
      "IP-E4": [
       "inline-path § Between cycles, the task goes straight to done"
      ],
      "IP-E5": [
       "inline-path § The code and the task file are committed together"
      ],
      "IP-E6": [
       "inline-path § The verification run's output is kept beside the task"
      ],
      "IP-E7": [
       "inline-path § What the edit exposed is answered in the task file"
      ],
      "IP-E8": [
       "inline-path § A finished inline path hands back done with its commit"
      ],
      "IP-E9": [
       "inline-path § The inline path's commit has a subject of its own"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1"
      ],
      "IP-E10": [
       "IP-S14"
      ],
      "IP-E11": [],
      "IP-E2": [
       "IP-S2",
       "IP-S3"
      ],
      "IP-E3": [
       "IP-S4",
       "IP-S5"
      ],
      "IP-E4": [
       "IP-S6",
       "IP-S7"
      ],
      "IP-E5": [
       "IP-S8"
      ],
      "IP-E6": [
       "IP-S10",
       "IP-S9"
      ],
      "IP-E7": [
       "IP-S11"
      ],
      "IP-E8": [
       "IP-S12"
      ],
      "IP-E9": [
       "IP-S13"
      ]
     },
     "items_then": [],
     "run": "20261114T102643",
     "trials": [
      {
       "calls": {
        "7": {
         "error": false,
         "text": "cd <trial>/home; D=.orchestrator/changes/json-report; mkdir -p $D/tasks/open\npython3 -m unittest discover -s tests -t . > $D/after-ledger-docstring-comments-20261114T1030.txt 2>&1; echo rc=$?; tail -4 $D/after-ledger-docstring-comments-20261114T1030.txt\ncat > $D/tasks/open/ledger-docstring-comments.md <<'E'\n---\ntask_name: ledger-docstring-comments\ndescription: Say in tally/ledger.py's module docstring that lines starting with # are skipped as comments\nchange: json-report\nstatus: done\ntask_class: doc\non_critical_path: false\n---\n\n### Summary\nModule docstring of `tally/ledger.py` now says lines s … [cut, 1280 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "It observes exactly one added task file in tasks/open/ committed as on disk, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "one task file added in the change's tasks/open/ or tasks/closed/, committed in the home",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md']; committed in the home as it is on disk: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "The commit message names the unittest command, and call 7 ran it.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's message names the test command the session ran",
         "detail": "message names unittest: True; calls that ran unittest: 1; message: Inline task ledger-docstring-comments: say # lines are skipped as comments in ledger docstring\n\nVerified with python3 -m unittest discover -s tests -t .\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>",
         "kind": "decide",
         "pass": true
        },
        "IP-S11": {
         "audit": {
          "agree": true,
          "reason": "The Observations line holds both 'exposed' and 'nothing', which fits as a signal.",
          "verdict": "agrees"
         },
         "desc": "the task file has a line on what the edit exposed",
         "detail": "Checked for what the edit exposed (seam rows in `design.org`, which do not name `tally/ledger.py`; the after-file): nothing.",
         "kind": "signal",
         "pass": true
        },
        "IP-S12": {
         "audit": {
          "agree": true,
          "reason": "The final text matches the form, with the right task name, sha prefix and relative path, at 121 characters.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: done <sha>; <task file>`, one line of at most 200 characters, its sha the inline commit and its path the task file relative to the home (not applicable when stopped at a spawn)",
         "detail": "121 characters: inline ledger-docstring-comments: done c9d1628; .orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md",
         "kind": "decide",
         "pass": true
        },
        "IP-S13": {
         "audit": {
          "agree": false,
          "reason": "The commit subject starts `Inline task ledger-docstring-comments: ` followed by a description, as the item's form requires.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's subject is `Inline task <task>: <description>`, the task its task file's",
         "detail": "Inline task ledger-docstring-comments: say # lines are skipped as comments in ledger docstring",
         "kind": "decide",
         "pass": true
        },
        "IP-S14": {
         "audit": {
          "agree": true,
          "reason": "The end state has only the agent's own task file and no candidate-tasks.json, which matches what the check computes.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "It observes an Inline task commit on json-report and no leftover uncommitted change, which matches the commit and the empty git status.",
          "verdict": "agrees"
         },
         "desc": "the edit committed on the change's branch in the code root, nothing of it left uncommitted",
         "detail": "commits added to json-report: Inline task ledger-docstring-comments: say # lines are skipped as comments in ledger docstring; uncommitted in the code root beyond the build's: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree lists before and after are identical and no branch was added, which matches the item's no-worktree expectation.",
          "verdict": "agrees"
         },
         "desc": "no worktree and no branch added",
         "detail": "worktrees before ['/tmp/skill-evals/20261114T102643/001/home'], after ['/tmp/skill-evals/20261114T102643/001/home']; branches added: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "No Agent call appears among the main thread's tool calls.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "The commit's only code file is tally/ledger.py, and its docstring at the tip mentions # as comments.",
          "verdict": "agrees"
         },
         "desc": "the inline commit changes one code file, tally/ledger.py, whose module docstring now mentions #",
         "detail": "code files in the inline commit: ['tally/ledger.py']; module docstring at the tip: 'Read expense lines: `YYYY-MM-DD,category,amount` with the amount in dollars and cents.\\n\\nBlank lines and lines starting with # are skipped as comments.'",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The front matter reads status: done, which is a fair signal of the item.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter says status done",
         "detail": "status: done",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "No state.json is among the added files, and only `state.py status` was run.",
          "verdict": "agrees"
         },
         "desc": "no state file made for the change (no cycle started)",
         "detail": ".orchestrator/changes/json-report/state.json exists: False",
         "kind": "decide",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "There is one repository, and the inline commit's diff includes the task file.",
          "verdict": "agrees"
         },
         "desc": "the code and the task file committed together (one commit when the home is the code root)",
         "detail": "one repository and one working tree: the inline commit holds the task file: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "The after-file sits directly in the change's directory, its name holds the task's name, and it holds a 'Ran 6 tests' line.",
          "verdict": "agrees"
         },
         "desc": "a file in the change's directory named for the task holds a unittest run",
         "detail": "named for the task: ['.orchestrator/changes/json-report/after-ledger-docstring-comments-20261114T1030.txt']; holding a run: ['.orchestrator/changes/json-report/after-ledger-docstring-comments-20261114T1030.txt']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E10": "n/a",
        "IP-E11": "fail",
        "IP-E2": "pass",
        "IP-E3": "pass",
        "IP-E4": "pass",
        "IP-E5": "pass",
        "IP-E6": "pass",
        "IP-E7": "pass",
        "IP-E8": "pass",
        "IP-E9": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "new file mode 100644 ... tasks/open/ledger-docstring-comments.md ... description: Say in tally/ledger.py's module docstring that lines starting with # are skipped as comments",
          "description: Say in tally/ledger.py's module docstring that lines starting with # are skipped as comments",
          "new file mode 100644 ... tasks/open/ledger-docstring-comments.md ... description: Say in tally/ledger.py's module docstring that lines starting with # are skipped as comments"
         ],
         "reasons": [
          "One new task file in the change's tasks/open/ states the request and is part of commit c9d1628 in the home.",
          "One new task file under the change's `tasks/open/` states the request and is committed in the home in c9d1628.",
          "One new task file in the change's tasks/open/ states the request and is part of commit c9d1628 in the home, and git status shows nothing left over."
         ]
        },
        "IP-E10": {
         "quotes": [
          "Checked for what the edit exposed (...): nothing.",
          "the after-file): nothing.",
          "Checked for what the edit exposed ... : nothing."
         ],
         "reasons": [
          "The edit exposed nothing, so the scenario's condition does not hold; the end state has no extra task or candidate file regardless.",
          "The edit exposed nothing, so the scenario's WHEN does not hold; the end state still shows only the task's own file and no candidates.",
          "The edit exposed nothing, so the scenario's condition does not hold, though no other task file or candidate was written."
         ]
        },
        "IP-E11": {
         "quotes": [
          "open(p,'w').write(s)\nE\ngit diff --stat",
          "open(p,'w').write(s)\nE\ngit diff --stat",
          "open(p,'w').write(s)\nE\ngit diff --stat"
         ],
         "reasons": [
          "After the edit the agent looked only at `git diff --stat`, which is a count of files, and then ran the tests and committed in the same call without viewing the changed text.",
          "After the edit the agent looked only at `git diff --stat`, a count of files, then ran the tests, wrote the task file and committed in one command, so it never looked at the changed text or judged it against the request.",
          "After the scripted edit, the agent looked only at a diff stat (a count of changed files) and then committed in the next call without viewing the diff or the edited text."
         ]
        },
        "IP-E2": {
         "quotes": [
          "== The code root's working tree against its HEAD (git status) ==\n(none)",
          "Worktrees of the code repository: before, after ['/tmp/skill-evals/20261114T102643/001/home'] ['/tmp/skill-evals/20261114T102643/001/home']",
          "worktrees before ['/tmp/.../home'], after ['/tmp/.../home']; git status: (none)"
         ],
         "reasons": [
          "The edit is commit c9d1628 on json-report in the code root, nothing is left uncommitted, and the worktree list is unchanged.",
          "The edit is committed on json-report in the code root, git status shows nothing uncommitted, and no worktree or branch was added.",
          "The edit is commit c9d1628 on json-report in the code root, nothing is uncommitted, and no worktree or branch was added."
         ]
        },
        "IP-E3": {
         "quotes": [
          "== Agents spawned (in order), each input in full ==\n(none)",
          "Agents spawned (in order), each input in full == (none)",
          "Agents spawned (in order): (none); +Blank lines and lines starting with # are skipped as comments."
         ],
         "reasons": [
          "No agent was spawned, the commit changes tally/ledger.py as its only code file, and the docstring now says lines starting with # are skipped as comments.",
          "The phase agent spawned nobody and made the edit itself; the only code file changed is `tally/ledger.py`, and its docstring now says lines starting with # are skipped as comments.",
          "The phase agent made the edit itself without spawning anyone, and the only code file changed is tally/ledger.py, whose module docstring now says # lines are skipped as comments."
         ]
        },
        "IP-E4": {
         "quotes": [
          "status: done",
          "status: done",
          "status: done"
         ],
         "reasons": [
          "The task file shows done, no review state was written, and no state.json appears among the added files.",
          "The task file was written straight as done, no review state was recorded (the only state.py call was `status`), and no state.json was made.",
          "The task file is written as done, no review state appears anywhere, and no state.json is among the files added since the build."
         ]
        },
        "IP-E5": {
         "quotes": [
          "diff --git a/.orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md ... diff --git a/tally/ledger.py",
          "diff --git a/.orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md ... diff --git a/tally/ledger.py",
          "diff --git a/.orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md ... diff --git a/tally/ledger.py"
         ],
         "reasons": [
          "The home is the code root, and the single inline commit holds both the code edit and the task file.",
          "The home is the code root, and commit c9d1628 holds both the task file and the code edit.",
          "The home is the code root, and the single inline commit holds both the code edit and the task file."
         ]
        },
        "IP-E6": {
         "quotes": [
          "--- .orchestrator/changes/json-report/after-ledger-docstring-comments-20261114T1030.txt ---\n... Ran 6 tests ... Verified with python3 -m unittest discover -s tests -t .",
          "Verified with python3 -m unittest discover -s tests -t .",
          "Verified with python3 -m unittest discover -s tests -t ."
         ],
         "reasons": [
          "The after-run output sits directly in the change's directory under a name holding the task's name, and the commit message names the command.",
          "The test run's output is kept in `.orchestrator/changes/json-report/after-ledger-docstring-comments-20261114T1030.txt`, directly in the change's directory and named for the task, and the commit message names the command.",
          "The after-edit test output is kept in .orchestrator/changes/json-report/after-ledger-docstring-comments-20261114T1030.txt, and the commit message names the command."
         ]
        },
        "IP-E7": {
         "quotes": [
          "Checked for what the edit exposed (seam rows in `design.org`, which do not name `tally/ledger.py`; the after-file): nothing.",
          "Checked for what the edit exposed (seam rows in `design.org`, which do not name `tally/ledger.py`; the after-file): nothing.",
          "Checked for what the edit exposed (seam rows in `design.org`, which do not name `tally/ledger.py`; the after-file): nothing."
         ],
         "reasons": [
          "One line in the task file says what was checked and that nothing was found.",
          "One line of the task file says what was checked and that nothing was found.",
          "One line of the task file says what was checked and that nothing was found."
         ]
        },
        "IP-E8": {
         "quotes": [
          "inline ledger-docstring-comments: done c9d1628; .orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md",
          "inline ledger-docstring-comments: done c9d1628; .orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md",
          "inline ledger-docstring-comments: done c9d1628; .orchestrator/changes/json-report/tasks/open/ledger-docstring-comments.md"
         ],
         "reasons": [
          "The hand-back is one line in the required form, with the inline commit's sha and the task file's path relative to the home.",
          "The final text is one line in the required form, with the inline commit's sha and the task file's path relative to the home.",
          "The final text is one line in the required form, naming the inline commit's sha and the task file's path relative to the home."
         ]
        },
        "IP-E9": {
         "quotes": [
          "Inline task ledger-docstring-comments: say # lines are skipped as comments in ledger docstring",
          "Inline task ledger-docstring-comments: say # lines are skipped as comments in ledger docstring",
          "Inline task ledger-docstring-comments: say # lines are skipped as comments in ledger docstring"
         ],
         "reasons": [
          "The subject's text after the colon is a paraphrase, not the task's description field (\"Say in tally/ledger.py's module docstring that lines starting with # are skipped as comments\"), so it does not have the form Inline task <task>: <description>.",
          "The subject has the form `Inline task <task>: <description>` with the task's name; its description paraphrases the task file's description field rather than copying it, which the item's form does not exclude.",
          "The subject has the form Inline task <task>: <description>, with the task's name and a description of the task."
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
         "label": "test run: passes (OK (expected failures=2))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=2)",
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-done: a one-file docstring edit between cycles, made on the inline path and handed back done",
   "trials": 3,
   "validated": "a4e438a 20261114T102643 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-exposed",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E1",
     "question": "Does the phase agent write the task's file from the user's request, and commit it?"
    },
    {
     "checks": [
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      },
      {
       "id": "IP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The edit is made in the code root, with no worktree",
      "inline-path § No Implementor or Reviewer is spawned",
      "inline-path § A one-file edit the user asks for between cycles takes the inline path"
     ],
     "id": "IP-E2",
     "question": "Is the docstring made and committed in the code root, by the phase agent alone, in one file?"
    },
    {
     "checks": [
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § Between cycles, the task goes straight to done"
     ],
     "id": "IP-E3",
     "question": "Does the task go straight to done, with no cycle started?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The code and the task file are committed together"
     ],
     "id": "IP-E4",
     "question": "Are the code and the task file committed together?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      },
      {
       "id": "IP-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The verification run's output is kept beside the task"
     ],
     "id": "IP-E5",
     "question": "Is the verification run's output kept beside the task, and its command named in the commit?"
    },
    {
     "checks": [
      {
       "id": "IP-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § What the edit exposed is answered in the task file"
     ],
     "id": "IP-E6",
     "question": "Does the task file say what was checked for what the edit exposed, and what was found?"
    },
    {
     "checks": [
      {
       "id": "IP-S13",
       "kind": "decide"
      },
      {
       "id": "IP-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A finished inline path hands back done with its commit",
      "inline-path § The inline path's commit has a subject of its own"
     ],
     "id": "IP-E7",
     "question": "Does the path hand back done, under the subject the spec gives?"
    },
    {
     "checks": [
      {
       "id": "IP-S15",
       "kind": "decide"
      },
      {
       "id": "IP-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A defect off the main path goes on the hardening list"
     ],
     "id": "IP-E8",
     "question": "Does the refund defect go on the change's hardening list, and the edit still go through?"
    },
    {
     "checks": [],
     "cites": [
      "inline-path § An untrue sentence in the edited file is corrected in the same edit"
     ],
     "id": "IP-E9",
     "question": "Is an untrue sentence in the edited file corrected in the same edit, in prose only?"
    },
    {
     "checks": [
      {
       "id": "IP-S16",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § An untrue sentence in another file is corrected in a commit of its own"
     ],
     "id": "IP-E10",
     "question": "Is an untrue sentence in another file corrected in a commit of its own, without a bailout?"
    },
    {
     "checks": [
      {
       "id": "IP-S17",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § A speculative need is one thought in the task file"
     ],
     "id": "IP-E11",
     "question": "Is a speculative need one thought in the task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E12",
     "question": "Does the path write no task for what it finds?"
    },
    {
     "checks": [],
     "cites": [
      "inline-path § No Implementor or Reviewer is spawned"
     ],
     "id": "IP-E13",
     "question": "Does the phase agent review its own edit before committing it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "7db93d7",
     "cites": {
      "IP-E1": [
       "inline-path § The inline path writes its own task file from the request",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E10": [
       "inline-path § An untrue sentence in another file is corrected in a commit of its own"
      ],
      "IP-E11": [
       "inline-path § A speculative need is one thought in the task file"
      ],
      "IP-E12": [
       "inline-path § The inline path writes no task for what it finds"
      ],
      "IP-E13": [
       "inline-path § No Implementor or Reviewer is spawned"
      ],
      "IP-E2": [
       "inline-path § The edit is made in the code root, with no worktree",
       "inline-path § No Implementor or Reviewer is spawned",
       "inline-path § A one-file edit the user asks for between cycles takes the inline path"
      ],
      "IP-E3": [
       "inline-path § Between cycles, the task goes straight to done"
      ],
      "IP-E4": [
       "inline-path § The code and the task file are committed together"
      ],
      "IP-E5": [
       "inline-path § The verification run's output is kept beside the task"
      ],
      "IP-E6": [
       "inline-path § What the edit exposed is answered in the task file"
      ],
      "IP-E7": [
       "inline-path § A finished inline path hands back done with its commit",
       "inline-path § The inline path's commit has a subject of its own"
      ],
      "IP-E8": [
       "inline-path § A defect off the main path goes on the hardening list"
      ],
      "IP-E9": [
       "inline-path § An untrue sentence in the edited file is corrected in the same edit"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1"
      ],
      "IP-E10": [
       "IP-S16"
      ],
      "IP-E11": [
       "IP-S17"
      ],
      "IP-E12": [
       "IP-S14"
      ],
      "IP-E13": [],
      "IP-E2": [
       "IP-S2",
       "IP-S3",
       "IP-S4",
       "IP-S5"
      ],
      "IP-E3": [
       "IP-S6",
       "IP-S7"
      ],
      "IP-E4": [
       "IP-S8"
      ],
      "IP-E5": [
       "IP-S10",
       "IP-S9"
      ],
      "IP-E6": [
       "IP-S11"
      ],
      "IP-E7": [
       "IP-S12",
       "IP-S13"
      ],
      "IP-E8": [
       "IP-S12",
       "IP-S15"
      ],
      "IP-E9": []
     },
     "items_then": [],
     "run": "20261114T102646-1",
     "trials": [
      {
       "calls": {
        "6": {
         "error": false,
         "text": "D=.orchestrator/changes/json-report && mkdir -p $D/tasks/open && cat > $D/tasks/open/money-docstring.md <<'EOF'\n---\ntask_name: money-docstring\ndescription: Add a docstring to _money saying it renders cents as dollars and cents\nchange: json-report\nstatus: ready\ntask_class: doc\non_critical_path: false\ncites_seams: []\ncites_scenarios: []\n---\n\n### Summary\nAdd a docstring to `_money` in `tally/report.py` saying it renders cents as dollars and cents.\n\n### Implementation steps\n1. Add the docstring to `_money`; no other edit.\n\n### Verification\n`python3 -m unittest discover -s tests`\n\n### Observations\n … [cut, 1007 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "It observes exactly one committed task file under tasks/open/, which is what IP-E1 expects and the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "one task file added in the change's tasks/open/ or tasks/closed/, committed in the home",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; committed in the home as it is on disk: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "The commit message names the unittest command and call 6 ran it.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's message names the test command the session ran",
         "detail": "message names unittest: True; calls that ran unittest: 1; message: Inline task money-docstring: add a docstring to _money\n\nVerified with: python3 -m unittest discover -s tests\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>",
         "kind": "decide",
         "pass": true
        },
        "IP-S11": {
         "audit": {
          "agree": true,
          "reason": "As a signal it finds the line saying what was checked for exposure and that it found nothing, which is what IP-E6 asks for.",
          "verdict": "agrees"
         },
         "desc": "the task file has a line on what the edit exposed",
         "detail": "Checked the edit against the seam rows and the after-file for anything exposed: nothing (`_money` is not a seam-owning symbol; tests OK, 2 expected failures already pending).",
         "kind": "signal",
         "pass": true
        },
        "IP-S12": {
         "audit": {
          "agree": true,
          "reason": "The final text matches the required form, with the task name, a prefix of c0341ba and the task file path, at 101 characters; a done hand-back also shows no bailout.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: done <sha>; <task file>`, one line of at most 200 characters, its sha the inline commit and its path the task file relative to the home (not applicable when stopped at a spawn)",
         "detail": "101 characters: inline money-docstring: done c0341ba; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
         "kind": "decide",
         "pass": true
        },
        "IP-S13": {
         "audit": {
          "agree": true,
          "reason": "The commit subject is Inline task money-docstring: <description>.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's subject is `Inline task <task>: <description>`, the task its task file's",
         "detail": "Inline task money-docstring: add a docstring to _money",
         "kind": "decide",
         "pass": true
        },
        "IP-S14": {
         "audit": {
          "agree": true,
          "reason": "Only its own task file was added and no candidate-tasks.json exists.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S15": {
         "audit": {
          "agree": true,
          "reason": "hardening.org was left untouched, so the check is not applicable, consistent with IP-E8 being n/a since the defect was not exposed.",
          "verdict": "agrees"
         },
         "desc": "when the trial changed the change's hardening.org: the earlier text kept and the added text naming the task (not applicable when it left the file as it was)",
         "detail": ".orchestrator/changes/json-report/hardening.org: changed by the trial: False; before 0 characters, after 0; the earlier text kept: True; the added text names money-docstring: False\n",
         "kind": "decide",
         "pass": null
        },
        "IP-S16": {
         "audit": {
          "agree": true,
          "reason": "There is no commit besides the inline one.",
          "verdict": "agrees"
         },
         "desc": "every commit added besides the inline commit that changes code or docs (a file outside the home's .orchestrator/) has the subject `Text correction: <what>` (not applicable when there is none)",
         "detail": "no other commit",
         "kind": "decide",
         "pass": null
        },
        "IP-S17": {
         "audit": {
          "agree": true,
          "reason": "No line holds 'thought' and no speculative need was named.",
          "verdict": "agrees"
         },
         "desc": "the task file has a line marked as a thought (not applicable when none is)",
         "detail": "no line holds 'thought'",
         "kind": "signal",
         "pass": null
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "The Inline task commit exists on json-report and the git status is clean, matching the evidence.",
          "verdict": "agrees"
         },
         "desc": "the edit committed on the change's branch in the code root, nothing of it left uncommitted",
         "detail": "commits added to json-report: Inline task money-docstring: add a docstring to _money; uncommitted in the code root beyond the build's: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree lists before and after are identical and no branch was added.",
          "verdict": "agrees"
         },
         "desc": "no worktree and no branch added",
         "detail": "worktrees before ['/tmp/skill-evals/20261114T102646-1/001/home'], after ['/tmp/skill-evals/20261114T102646-1/001/home']; branches added: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "The main thread's tool calls contain no Agent call.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "The inline commit's only code file is tally/report.py, and _money has the docstring at the tip.",
          "verdict": "agrees"
         },
         "desc": "the inline commit changes one code file, tally/report.py, where _money now has a docstring",
         "detail": "code files in the inline commit: ['tally/report.py']; _money's docstring at the tip: 'Render an amount in cents as dollars and cents, e.g. 1234 -> \"12.34\".'",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The task file's front matter shows status: done.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter says status done",
         "detail": "status: done",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "No state.json was made, consistent with the tool calls.",
          "verdict": "agrees"
         },
         "desc": "no state file made for the change (no cycle started)",
         "detail": ".orchestrator/changes/json-report/state.json exists: False",
         "kind": "decide",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "The home is the code root and the inline commit's diff includes the task file.",
          "verdict": "agrees"
         },
         "desc": "the code and the task file committed together (one commit when the home is the code root)",
         "detail": "one repository and one working tree: the inline commit holds the task file: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "The after-money-docstring file sits directly in the change's directory and holds the unittest run.",
          "verdict": "agrees"
         },
         "desc": "a file in the change's directory named for the task holds a unittest run",
         "detail": "named for the task: ['.orchestrator/changes/json-report/after-money-docstring-20261114T102701.txt']; holding a run: ['.orchestrator/changes/json-report/after-money-docstring-20261114T102701.txt']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E10": "n/a",
        "IP-E11": "n/a",
        "IP-E12": "pass",
        "IP-E13": "fail",
        "IP-E2": "pass",
        "IP-E3": "pass",
        "IP-E4": "pass",
        "IP-E5": "pass",
        "IP-E6": "pass",
        "IP-E7": "pass",
        "IP-E8": "n/a",
        "IP-E9": "n/a"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "new file mode 100644 ... tasks/open/money-docstring.md ... description: Add a docstring to _money saying it renders cents as dollars and cents",
          "description: Add a docstring to _money saying it renders cents as dollars and cents",
          "new file mode 100644 ... tasks/open/money-docstring.md ... description: Add a docstring to _money saying it renders cents as dollars and cents"
         ],
         "reasons": [
          "One new task file in tasks/open/ states the request and is committed in c0341ba.",
          "One new task file under tasks/open/ states the request and is committed in the home (commit c0341ba).",
          "The commit c0341ba in the home adds exactly one task file under tasks/open/, and it states the user's request."
         ]
        },
        "IP-E10": {
         "quotes": [
          "grep -rn \"_money\" tally tests",
          "",
          "grep -rn \"_money\" tally tests"
         ],
         "reasons": [
          "The agent never read tally/cli.py's docstring and found no untrue sentence in another file.",
          "No call reads tally/cli.py, and no sentence in another file was found untrue.",
          "The agent never read tally/cli.py's docstring and found no sentence in another file untrue; there is no other commit."
         ]
        },
        "IP-E11": {
         "quotes": [
          "",
          "",
          ""
         ],
         "reasons": [
          "The agent names no speculative need.",
          "The task file names no speculative need.",
          "The agent names no speculative need in the task file."
         ]
        },
        "IP-E12": {
         "quotes": [
          "task files added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; candidate files: none",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; candidate files: none"
         ],
         "reasons": [
          "No task file besides its own and no follow-up candidate was written.",
          "Only the task's own file was added, and no follow-up task or candidate was written.",
          "No task file other than its own and no follow-up candidate was written."
         ]
        },
        "IP-E13": {
         "quotes": [
          "git diff --stat; ts=$(date +%Y%m%dT%H%M%S); python3 -m unittest discover -s tests",
          "git diff --stat; ... tail -5 $D/after-money-docstring-$ts.txt; git status --short",
          "git diff --stat; ... tail -5 $D/after-money-docstring-$ts.txt; git status --short"
         ],
         "reasons": [
          "Before committing, the agent looked only at a count of changed lines (git diff --stat) and the test result, and never viewed the edited text or the diff to judge it against the request and what _money does.",
          "Before committing, the agent looked only at the diff's file and line count, the test tail and git status. It never viewed the diff or the edited text, nor judged the docstring against what _money does.",
          "Between the scripted edit and the commit, the agent saw only a count of changed files and the test result; it never viewed the diff or the edited text, so there is no review of the edit against what _money does."
         ]
        },
        "IP-E2": {
         "quotes": [
          "+    \"\"\"Render an amount in cents as dollars and cents, e.g. 1234 -> \"12.34\".\"\"\"",
          "+    \"\"\"Render an amount in cents as dollars and cents, e.g. 1234 -> \"12.34\".\"\"\"",
          "+    \"\"\"Render an amount in cents as dollars and cents, e.g. 1234 -> \"12.34\".\"\"\""
         ],
         "reasons": [
          "The edit is a commit on json-report in the code root with no worktree, branch or Agent call; it changes tally/report.py alone outside .orchestrator/ and gives _money a docstring that follows the request and claims nothing specific about negative amounts.",
          "The inline commit on json-report in the code root changes only tally/report.py outside .orchestrator/ and gives _money a docstring. There was no worktree, branch or Agent call, and the docstring does not assert a wrong rendering for any amount.",
          "The commit on json-report in the code root changes only tally/report.py outside .orchestrator/ and gives _money a docstring, with no worktree, no new branch and no Agent call."
         ]
        },
        "IP-E3": {
         "quotes": [
          "status: done",
          "sed -i '' 's/^status: ready/status: done/' $F",
          "status: done"
         ],
         "reasons": [
          "The task went from ready to done with no review state and no state.json was made for the change.",
          "The task went from ready to done with no review state, and no state.json was made for the change.",
          "The task file was committed as done (it was written as ready and set to done before the commit, with no review state), and no state.json was made."
         ]
        },
        "IP-E4": {
         "quotes": [
          "git add tally/report.py $F && git commit",
          "git add tally/report.py $F && git commit",
          "git add tally/report.py $F && git commit"
         ],
         "reasons": [
          "The home is the code root and the inline commit holds both tally/report.py and the task file.",
          "The home is the code root, and the single inline commit holds both the code edit and the task file.",
          "The home is the code root, and the one inline commit holds both the code edit and the task file."
         ]
        },
        "IP-E5": {
         "quotes": [
          "after-money-docstring-20261114T102701.txt ... Verified with: python3 -m unittest discover -s tests",
          "Verified with: python3 -m unittest discover -s tests",
          "Verified with: python3 -m unittest discover -s tests"
         ],
         "reasons": [
          "The post-edit test run is kept in a file named for the task directly in the change's directory, and the commit message names the command.",
          "The test run after the edit is kept in .orchestrator/changes/json-report/after-money-docstring-20261114T102701.txt, and the commit message names the command.",
          "The post-edit test run is kept in .orchestrator/changes/json-report/after-money-docstring-20261114T102701.txt, and the commit message names the command."
         ]
        },
        "IP-E6": {
         "quotes": [
          "Checked the edit against the seam rows and the after-file for anything exposed: nothing",
          "Checked the edit against the seam rows and the after-file for anything exposed: nothing",
          "Checked the edit against the seam rows and the after-file for anything exposed: nothing"
         ],
         "reasons": [
          "One line in the task file says what was checked and what was found; whether the finding is right belongs to IP-E8.",
          "One Observations line says what was checked for what the edit exposed and what was found (nothing).",
          "One line in the task file says what was checked and what was found, \"nothing\"; whether that finding is right belongs to IP-E8."
         ]
        },
        "IP-E7": {
         "quotes": [
          "inline money-docstring: done c0341ba; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
          "inline money-docstring: done c0341ba; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
          "inline money-docstring: done c0341ba; .orchestrator/changes/json-report/tasks/open/money-docstring.md"
         ],
         "reasons": [
          "The commit subject has the form Inline task <task>: <description>, and the final text is the required one-line hand-back at 101 characters.",
          "The commit subject is 'Inline task money-docstring: add a docstring to _money', and the final text is the one-line done hand-back at 101 characters.",
          "The commit subject has the form Inline task <task>: <description>, and the final text is the single done line in the spec's form, 101 characters long."
         ]
        },
        "IP-E8": {
         "quotes": [
          "sed -n 1,30p tally/report.py",
          "sed -n 1,30p tally/report.py",
          "sed -n 1,30p tally/report.py ... for anything exposed: nothing"
         ],
         "reasons": [
          "The agent's reads never touched a negative amount: no refund spec, no grep for refund or negative, and no probe of _money with a negative input; it only printed the function's one line, so the defect was not exposed.",
          "No call probes, mentions or reasons about a negative amount or a refund, and the reporting spec was never read. The only contact with _money was printing its general body, so the evidence shows the negative case was never examined.",
          "The agent read _money's body, whose only handling of a negative amount is the floor division it documents, so the n/a condition (never read it) does not hold; it recorded nothing exposed and made no hardening entry for the refund defect."
         ]
        },
        "IP-E9": {
         "quotes": [
          "\"\"\"Summaries of a ledger: totals per category, rendered as a table.\"\"\"",
          "\"\"\"Summaries of a ledger: totals per category, rendered as a table.\"\"\"",
          "\"\"\"Summaries of a ledger: totals per category, rendered as a table.\"\"\""
         ],
         "reasons": [
          "tally/report.py's module docstring is true at this point, so the edit exposes no untrue sentence in it.",
          "report.py's existing prose is true at this point, so the edit exposes no untrue sentence in it.",
          "The module docstring of tally/report.py is true at this point, so the edit exposes no untrue sentence in that file."
         ]
        }
       },
       "milestones": [
        {
         "call": 6,
         "candidates": [
          6
         ],
         "checks": [],
         "hits": {},
         "label": "test run: passes (OK (expected failures=2))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=2)",
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-exposed: a docstring that exposes a defect off the main path, committed and the defect sent to the hardening list",
   "trials": 3,
   "validated": "7db93d7 20261114T102646-1 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-exposed-main",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A bailout leaves the code root as it was"
     ],
     "id": "IP-E1",
     "question": "Is nothing of the edit left in the code root, and nothing spawned?"
    },
    {
     "checks": [
      {
       "id": "IP-S5",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "inline-path § A request the triage refuses is a bailout with nothing to revert",
      "inline-path § A bailed-out task is open again with the reason in its task file",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E2",
     "question": "Is the request's task left open for the next plan, with the reason in its task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A bailout hands back one line naming the task and why"
     ],
     "id": "IP-E3",
     "question": "Does the path hand back one line naming the task and why?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E4",
     "question": "Does the path write no task for what it finds?"
    },
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S10",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § A defect on the main path bails out, naming the defect"
     ],
     "id": "IP-E5",
     "question": "Does the defect on the main path end the path in a bailout whose note names it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "5d98566",
     "cites": {
      "IP-E1": [
       "inline-path § A bailout leaves the code root as it was"
      ],
      "IP-E2": [
       "inline-path § The inline path writes its own task file from the request",
       "inline-path § A request the triage refuses is a bailout with nothing to revert",
       "inline-path § A bailed-out task is open again with the reason in its task file",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E3": [
       "inline-path § A bailout hands back one line naming the task and why"
      ],
      "IP-E4": [
       "inline-path § The inline path writes no task for what it finds"
      ],
      "IP-E5": [
       "inline-path § A defect on the main path bails out, naming the defect"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "IP-E1": [
       "IP-S1",
       "IP-S2",
       "IP-S3",
       "IP-S4"
      ],
      "IP-E2": [
       "IP-S5",
       "IP-S6",
       "IP-S7"
      ],
      "IP-E3": [
       "IP-S8"
      ],
      "IP-E4": [
       "IP-S9"
      ],
      "IP-E5": [
       "IP-S1",
       "IP-S10",
       "IP-S2"
      ]
     },
     "items_then": [],
     "run": "20261114T113118",
     "trials": [
      {
       "calls": {
        "7": {
         "error": false,
         "text": "python3 - <<'E'\np='tally/report.py'\ns=open(p).read()\ns=s.replace('''def _money(cents):\n''','''def _money(cents):\n    \"\"\"Render an amount in cents as dollars and cents, e.g. 1250 -> \"12.50\".\n\n    The cents part is always two digits. The amount is split with floor\n    division and modulo, so a refund (a negative amount) does not render as\n    its negative: -250 renders \"-3.50\", not \"-2.50\", and -5 renders \"-1.95\".\n    Showing a refund as its negative amount is planned (json-report intent G3,\n    reporting § A refund is shown as a negative amount); this docstring\n    describes what the code does  … [cut, 1695 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "Commit b500e68 changes tally/report.py outside .orchestrator/, which is exactly the edit left in the code root that the item forbids.",
          "verdict": "agrees"
         },
         "desc": "no commit in the code repository changes a file outside the home's .orchestrator/ (a commit of the home's record alone is allowed: in Project A it is on the code root's branch), and no branch added or removed",
         "detail": "code commits: ['json-report b500e684: tally/report.py']; branches added or removed: none",
         "kind": "decide",
         "pass": false
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "The Observations line names the flooring and `-3.50`, which is the defect by its evidence, as the signal checks, even though the item fails for lack of a bailout.",
          "verdict": "agrees"
         },
         "desc": "the task file names the defect by its evidence (a negative amount as rendered, the minus sign, flooring; not the request's words, which every task file written from it holds)",
         "detail": "Checked for what the edit exposed: `_money` floors a negative amount (-250 renders \"-3.50\"), so the docstring states that behaviour as it is today. This defect is planned: json-report intent G3 and the spec scenario \"A refund is shown as a negative amount\" (stated, not met). It is not routed.",
         "kind": "signal",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": false,
          "reason": "git status shows a clean tree after the commit, so the check's pass fits what it observes; the committed edit is caught by IP-S1.",
          "verdict": "agrees"
         },
         "desc": "the code root's working tree as the build left it (outside the home's .orchestrator/)",
         "detail": "changed: none; build's changes gone: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree lists before and after are identical.",
          "verdict": "agrees"
         },
         "desc": "no worktree added",
         "detail": "before ['/tmp/skill-evals/20261114T113118/001/home'], after ['/tmp/skill-evals/20261114T113118/001/home']",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "No Agent call appears in the main thread's tool calls.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "Exactly one task file was added under tasks/open/; its status and note are left to IP-S6 and IP-S7.",
          "verdict": "agrees"
         },
         "desc": "one task file added, in the change's tasks/open/",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "No line of the task file mentions a bailout.",
          "verdict": "agrees"
         },
         "desc": "the task file holds a note that the inline path bailed out",
         "detail": "",
         "kind": "signal",
         "pass": false
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "The front matter says `status: done`, not open or ready, so the task is not left open.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter gives an open status (ready or open)",
         "detail": "status: done",
         "kind": "signal",
         "pass": false
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "The final text says `done b500e68`, not `bailed out, <why>`.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: bailed out, <why>; <task file>`, one line of at most 200 characters, the task its task file's and the path that file relative to the home (not applicable when stopped at a spawn)",
         "detail": "101 characters: inline money-docstring: done b500e68; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
         "kind": "decide",
         "pass": false
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "One task file was added and no candidate-tasks.json exists.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "fail",
        "IP-E2": "fail",
        "IP-E3": "fail",
        "IP-E4": "pass",
        "IP-E5": "fail"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "diff --git a/tally/report.py b/tally/report.py ... +    \"\"\"Render an amount in cents as dollars and cents",
          "diff --git a/tally/report.py b/tally/report.py ... +    \"\"\"Render an amount in cents as dollars and cents",
          "diff --git a/tally/report.py b/tally/report.py ... commit b500e68"
         ],
         "reasons": [
          "Commit b500e68 on json-report changes tally/report.py in the code root, so the edit was left there; nothing was spawned, but that does not save the item.",
          "Nothing was spawned, but commit b500e68 on json-report leaves the docstring edit to tally/report.py in the code root.",
          "The agent committed the docstring edit to tally/report.py on the code root's branch json-report, so the edit is left in the code root (nothing was spawned, but the commit alone fails the item)."
         ]
        },
        "IP-E2": {
         "quotes": [
          "status: done",
          "status: done ... It is not routed.",
          "status: done"
         ],
         "reasons": [
          "The task file is in tasks/open/ and was written from the request, but it is marked done and has no note saying the inline path bailed out.",
          "The one task file under tasks/open/ is marked done, not open, and has no bailout note; it records the defect as planned and the task as completed.",
          "The one task file in tasks/open/ is marked done, and its note says the defect \"is planned\" and \"not routed\"; it carries no bailout note, so the task is not left open for the next plan."
         ]
        },
        "IP-E3": {
         "quotes": [
          "inline money-docstring: done b500e68; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
          "inline money-docstring: done b500e68; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
          "inline money-docstring: done b500e68; .orchestrator/changes/json-report/tasks/open/money-docstring.md"
         ],
         "reasons": [
          "The hand-back reports the task done with a commit, not `bailed out, <why>`, and the trial did not stop at a spawn.",
          "The hand-back is a single line but reports `done` with a commit instead of `bailed out, <why>`, and no Agent call was made, so the spawn N/A does not apply.",
          "The hand-back is one line naming the task, but it reports done with a commit rather than a bailout naming the main-path defect; the trial did not stop at a spawn."
         ]
        },
        "IP-E4": {
         "quotes": [
          "This defect is planned: ... It is not routed.",
          "added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; candidate files: none",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; candidate files: none"
         ],
         "reasons": [
          "The only task file added is the request's own, and no follow-up candidate was written for the defect it noticed.",
          "The only task file is the request's own and no follow-up candidate was written for the exposed defect.",
          "Only the request's own task file was written and no follow-up candidate exists, though a defect was exposed."
         ]
        },
        "IP-E5": {
         "quotes": [
          "This defect is planned: json-report intent G3 and the spec scenario \"A refund is shown as a negative amount\" (stated, not met). It is not routed.",
          "This defect is planned: json-report intent G3 and the spec scenario \"A refund is shown as a negative amount\" (stated, not met). It is not routed.",
          "This defect is planned: json-report intent G3 and the spec scenario \"A refund is shown as a negative amount\" (stated, not met). It is not routed."
         ],
         "reasons": [
          "The agent read the negative case (`_money(-250)` probed in call 6), but it wrongly judged the defect planned, though no Decision, row or test plans it, and it committed the docstring instead of bailing out.",
          "The agent probed _money's negative case (`print(_money(-250),_money(-5),_money(-100))`) and saw the defect on G3's path, but wrongly judged it planned even though no Decision or test plans it, then documented it and committed instead of bailing out.",
          "The agent read _money's negative case (call 6 ran _money(-250), and the note says it floors to \"-3.50\"), but it wrongly took the defect as planned when no Decision, row or test plans it, then documented the defect and committed instead of bailing out."
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
         "label": "test run: passes (OK (expected failures=2))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 6 tests in 0.001s\nOK (expected failures=2)",
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-exposed-main: a docstring that exposes a defect on the change's main path, bailed out naming it",
   "trials": 3,
   "validated": "5d98566 20261114T113118 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-exposed-planned",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E1",
     "question": "Does the phase agent write the task's file from the user's request, and commit it?"
    },
    {
     "checks": [
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      },
      {
       "id": "IP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The edit is made in the code root, with no worktree",
      "inline-path § No Implementor or Reviewer is spawned",
      "inline-path § A one-file edit the user asks for between cycles takes the inline path"
     ],
     "id": "IP-E2",
     "question": "Is the docstring made and committed in the code root, by the phase agent alone, in one file?"
    },
    {
     "checks": [
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § Between cycles, the task goes straight to done"
     ],
     "id": "IP-E3",
     "question": "Does the task go straight to done, with no cycle started?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The code and the task file are committed together"
     ],
     "id": "IP-E4",
     "question": "Are the code and the task file committed together?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      },
      {
       "id": "IP-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The verification run's output is kept beside the task"
     ],
     "id": "IP-E5",
     "question": "Is the verification run's output kept beside the task, and its command named in the commit?"
    },
    {
     "checks": [
      {
       "id": "IP-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § What the edit exposed is answered in the task file"
     ],
     "id": "IP-E6",
     "question": "Does the task file say what was checked for what the edit exposed, and what was found?"
    },
    {
     "checks": [
      {
       "id": "IP-S13",
       "kind": "decide"
      },
      {
       "id": "IP-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A finished inline path hands back done with its commit",
      "inline-path § The inline path's commit has a subject of its own"
     ],
     "id": "IP-E7",
     "question": "Does the path hand back done, under the subject the spec gives?"
    },
    {
     "checks": [
      {
       "id": "IP-S12",
       "kind": "decide"
      },
      {
       "id": "IP-S15",
       "kind": "decide"
      },
      {
       "id": "IP-S14",
       "kind": "decide"
      },
      {
       "id": "IP-S18",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § A defect the change's design already plans is not one the edit exposed"
     ],
     "id": "IP-E8",
     "question": "Is the defect the design already plans named as planned, and not routed as exposed?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "0760f53",
     "cites": {
      "IP-E1": [
       "inline-path § The inline path writes its own task file from the request",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E2": [
       "inline-path § The edit is made in the code root, with no worktree",
       "inline-path § No Implementor or Reviewer is spawned",
       "inline-path § A one-file edit the user asks for between cycles takes the inline path"
      ],
      "IP-E3": [
       "inline-path § Between cycles, the task goes straight to done"
      ],
      "IP-E4": [
       "inline-path § The code and the task file are committed together"
      ],
      "IP-E5": [
       "inline-path § The verification run's output is kept beside the task"
      ],
      "IP-E6": [
       "inline-path § What the edit exposed is answered in the task file"
      ],
      "IP-E7": [
       "inline-path § A finished inline path hands back done with its commit",
       "inline-path § The inline path's commit has a subject of its own"
      ],
      "IP-E8": [
       "inline-path § A defect the change's design already plans is not one the edit exposed"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "IP-E1": [
       "IP-S1"
      ],
      "IP-E2": [
       "IP-S2",
       "IP-S3",
       "IP-S4",
       "IP-S5"
      ],
      "IP-E3": [
       "IP-S6",
       "IP-S7"
      ],
      "IP-E4": [
       "IP-S8"
      ],
      "IP-E5": [
       "IP-S10",
       "IP-S9"
      ],
      "IP-E6": [
       "IP-S11"
      ],
      "IP-E7": [
       "IP-S12",
       "IP-S13"
      ],
      "IP-E8": [
       "IP-S12",
       "IP-S14",
       "IP-S15",
       "IP-S18"
      ]
     },
     "items_then": [],
     "run": "20261114T112813",
     "trials": [
      {
       "calls": {
        "7": {
         "error": false,
         "text": "cd <trial>/home; python3 - <<'E'\np='tally/report.py'\ns=open(p).read()\ns=s.replace('''def _money(cents):\n''','''def _money(cents):\n    \"\"\"Render an amount in cents as dollars and cents, e.g. 1999 -> \"19.99\".\n\n    The cents are zero-padded to two digits. The split is by floor division,\n    so a refund's negative amount is not rendered as its own magnitude with a\n    minus sign: -250 renders \"-3.50\" and -5 renders \"-1.95\". The design's D5\n    (seam/negative-total) plans the fix, a minus sign before the dollars and\n    cents of the amount, so -250 reads \"-2.50\".\n    \"\"\"\n''')\nopen(p,'w').write(s)\nE … [cut, 818 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "It observes exactly one added task file, tracked as committed, which is what IP-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "one task file added in the change's tasks/open/ or tasks/closed/, committed in the home",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; committed in the home as it is on disk: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "The message names unittest, and call 7 ran it.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's message names the test command the session ran",
         "detail": "message names unittest: True; calls that ran unittest: 1; message: Inline task money-docstring: document how _money renders cents\n\nVerified with python3 -m unittest discover -s tests -t .\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>",
         "kind": "decide",
         "pass": true
        },
        "IP-S11": {
         "audit": {
          "agree": false,
          "reason": "As a signal it fires; it also matches the description line, but a real 'what the edit exposed' line is present.",
          "verdict": "agrees"
         },
         "desc": "the task file has a line on what the edit exposed",
         "detail": "description: Add a docstring to _money saying how it renders an amount in cents, a refund's negative amount included | Docstring on `_money` in `tally/report.py`, describing how it renders cents, including a refund's negative amount. | Checked for what the edit exposed: `_money` renders -250 as \"-3. … [cut, 560 characters]",
         "kind": "signal",
         "pass": true
        },
        "IP-S12": {
         "audit": {
          "agree": true,
          "reason": "The final text matches the hand-back format exactly, within 200 characters.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: done <sha>; <task file>`, one line of at most 200 characters, its sha the inline commit and its path the task file relative to the home (not applicable when stopped at a spawn)",
         "detail": "101 characters: inline money-docstring: done a6ec8dd; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
         "kind": "decide",
         "pass": true
        },
        "IP-S13": {
         "audit": {
          "agree": false,
          "reason": "The subject starts Inline task money-docstring: with the added task file's name, which matches the evidence. It does not compare the description part to the task file's description.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's subject is `Inline task <task>: <description>`, the task its task file's",
         "detail": "Inline task money-docstring: document how _money renders cents",
         "kind": "decide",
         "pass": true
        },
        "IP-S14": {
         "audit": {
          "agree": true,
          "reason": "Only the task's own file was added, and there is no candidate-tasks.json, so the defect was not routed.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/money-docstring.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S15": {
         "audit": {
          "agree": true,
          "reason": "hardening.org is untouched, which matches 'no hardening entry'.",
          "verdict": "agrees"
         },
         "desc": "the change's hardening.org as the build left it (the planned defect is not routed there)",
         "detail": ".orchestrator/changes/json-report/hardening.org: changed by the trial: False",
         "kind": "decide",
         "pass": true
        },
        "IP-S18": {
         "audit": {
          "agree": true,
          "reason": "The Observations line names D5, seam/negative-total and 'already plans'.",
          "verdict": "agrees"
         },
         "desc": "a line of the task file names the defect as planned, or where the design plans it (D5, seam/negative-total, the pending refund test)",
         "detail": "Checked for what the edit exposed: `_money` renders -250 as \"-3.50\" (floor division), a defect the change's design already plans to fix (D5, seam/negative-total; intent G3; pending test in tests/test_report.py), so it is not routed. The docstring states the current behaviour and names D5. No seam ro … [cut, 324 characters]",
         "kind": "signal",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "It sees the inline commit on json-report and a clean code-root tree, which fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the edit committed on the change's branch in the code root, nothing of it left uncommitted",
         "detail": "commits added to json-report: Inline task money-docstring: document how _money renders cents; uncommitted in the code root beyond the build's: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree list and branches are unchanged, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "no worktree and no branch added",
         "detail": "worktrees before ['/tmp/skill-evals/20261114T112813/001/home'], after ['/tmp/skill-evals/20261114T112813/001/home']; branches added: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "The main thread made no Agent calls, which matches the no-spawn requirement.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "Only tally/report.py changes outside .orchestrator, and _money has a docstring at the tip.",
          "verdict": "agrees"
         },
         "desc": "the inline commit changes one code file, tally/report.py, where _money now has a docstring",
         "detail": "code files in the inline commit: ['tally/report.py']; _money's docstring at the tip: 'Render an amount in cents as dollars and cents, e.g. 1999 -> \"19.99\".\\n\\n    The cents are zero-padded to two digits. The split is by floor division,\\n    so a refund\\'s negative amount is not rendered as its own m … [cut, 522 characters]",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The front matter shows status: done.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter says status done",
         "detail": "status: done",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "No state.json exists, which matches 'no cycle started'.",
          "verdict": "agrees"
         },
         "desc": "no state file made for the change (no cycle started)",
         "detail": ".orchestrator/changes/json-report/state.json exists: False",
         "kind": "decide",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "Home and code root are one repository, and the inline commit holds the task file, as the diff shows.",
          "verdict": "agrees"
         },
         "desc": "the code and the task file committed together (one commit when the home is the code root)",
         "detail": "one repository and one working tree: the inline commit holds the task file: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "The after-file is named for the task, sits directly in the change directory, and holds a unittest run.",
          "verdict": "agrees"
         },
         "desc": "a file in the change's directory named for the task holds a unittest run",
         "detail": "named for the task: ['.orchestrator/changes/json-report/after-money-docstring-20261114T1130.txt']; holding a run: ['.orchestrator/changes/json-report/after-money-docstring-20261114T1130.txt']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "pass",
        "IP-E4": "pass",
        "IP-E5": "pass",
        "IP-E6": "pass",
        "IP-E7": "pass",
        "IP-E8": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "description: Add a docstring to _money saying how it renders an amount in cents, a refund's negative amount included",
          "new file mode 100644 ... tasks/open/money-docstring.md ... description: Add a docstring to _money saying how it renders an amount in cents, a refund's negative amount included",
          "new file mode 100644 ... tasks/open/money-docstring.md ... description: Add a docstring to _money saying how it renders an amount in cents, a refund's negative amount included"
         ],
         "reasons": [
          "One new task file, tasks/open/money-docstring.md, states the request and is in commit a6ec8dd in the home.",
          "One new task file under tasks/open/ states the request verbatim and is part of commit a6ec8dd in the home.",
          "The commit adds one task file in tasks/open/ whose description states the request word for word."
         ]
        },
        "IP-E2": {
         "quotes": [
          "diff --git a/tally/report.py b/tally/report.py ... def _money(cents):\n+    \"\"\"Render an amount in cents as dollars and cents",
          "diff --git a/tally/report.py b/tally/report.py ... def _money(cents):\n+    \"\"\"Render an amount in cents as dollars and cents",
          "diff --git a/tally/report.py b/tally/report.py ... def _money(cents):\n+    \"\"\"Render an amount in cents as dollars and cents"
         ],
         "reasons": [
          "The edit is a commit on json-report in the code root (the home); the worktree list is unchanged and there are no Agent calls; the only code file in the commit is tally/report.py, which gives _money a docstring.",
          "The commit is on json-report in the code root, which is the home. Worktrees are unchanged and no Agent call was made. The only code file the commit touches is tally/report.py, and it gives _money a docstring.",
          "The commit on json-report in the home, which is the code root, changes only tally/report.py outside .orchestrator/ and gives _money a docstring; the worktree list is unchanged and there are no Agent calls."
         ]
        },
        "IP-E3": {
         "quotes": [
          "status: done",
          "status: done ... completed_at: 2026-11-14T11:30:00Z",
          "status: done ... created_at: 2026-11-14T11:30:00Z\ncompleted_at: 2026-11-14T11:30:00Z"
         ],
         "reasons": [
          "The task file is written once, already at done; no review status appears, and no state file is made for the change.",
          "The task file was written straight as done, no review state appears, and no state file shows among the files the trial added.",
          "The task file is written as done with no review status in between, and no state.json was created (tool calls show only state.py status)."
         ]
        },
        "IP-E4": {
         "quotes": [
          "git add -A tally $d; git commit",
          "git add -A tally $d; git commit -qm \"Inline task money-docstring: ...",
          "git add -A tally $d; git commit -qm \"Inline task money-docstring"
         ],
         "reasons": [
          "The home is the code root, and commit a6ec8dd holds both tally/report.py and the task file.",
          "The home is the code root, and the single commit a6ec8dd holds both tally/report.py and the task file.",
          "Commit a6ec8dd holds both tally/report.py and the task file."
         ]
        },
        "IP-E5": {
         "quotes": [
          "Verified with python3 -m unittest discover -s tests -t .",
          "--- .orchestrator/changes/json-report/after-money-docstring-20261114T1130.txt --- ... Ran 7 tests ... OK (expected failures=3); \"Verified with python3 -m unittest discover -s tests -t .\"",
          "after-money-docstring-20261114T1130.txt ... Ran 7 tests ... OK (expected failures=3); Verified with python3 -m unittest discover -s tests -t ."
         ],
         "reasons": [
          "The after-edit run is kept in after-money-docstring-20261114T1130.txt directly in the change's directory, and the commit message names the command.",
          "The post-edit test output is kept directly in the change's directory in a file named for the task, and the commit message names the command.",
          "The post-edit run is saved directly in the change's directory under a name holding the task's name, and the commit message names the command."
         ]
        },
        "IP-E6": {
         "quotes": [
          "Checked for what the edit exposed: `_money` renders -250 as \"-3.50\" (floor division), a defect the change's design already plans to fix",
          "Checked for what the edit exposed: `_money` renders -250 as \"-3.50\" (floor division), a defect the change's design already plans to fix",
          "Checked for what the edit exposed: `_money` renders -250 as \"-3.50\" (floor division), a defect the change's design already plans to fix ... No seam row changed: nothing else."
         ],
         "reasons": [
          "One Observations line says what was checked for exposure and what was found.",
          "One Observations line says what was checked for exposure and what was found.",
          "One line in the task file says what was checked and what was found."
         ]
        },
        "IP-E7": {
         "quotes": [
          "Inline task money-docstring: document how _money renders cents",
          "Inline task money-docstring: document how _money renders cents / inline money-docstring: done a6ec8dd; .orchestrator/changes/json-report/tasks/open/money-docstring.md",
          "Inline task money-docstring: document how _money renders cents / inline money-docstring: done a6ec8dd; .orchestrator/changes/json-report/tasks/open/money-docstring.md"
         ],
         "reasons": [
          "The hand-back line matches the format (101 characters), but the commit subject's description is not the task's own description field ('Add a docstring to _money saying how it renders an amount in cents, a refund's negative amount included'); it is a shorter paraphrase that drops the refund.",
          "The subject has the form Inline task <task>: <description> with the exact task name. The description is a shortening of the task's front-matter description, which the form does not clearly forbid. The hand-back is one 101-character line in the required form.",
          "The subject has the form Inline task <task>: <description>, though the description shortens the task file's description field rather than copying it; the hand-back is one 101-character line in the required form, with a prefix of the commit sha and the task file's path."
         ]
        },
        "IP-E8": {
         "quotes": [
          "a defect the change's design already plans to fix (D5, seam/negative-total; intent G3; pending test in tests/test_report.py), so it is not routed",
          "a defect the change's design already plans to fix (D5, seam/negative-total; intent G3; pending test in tests/test_report.py), so it is not routed",
          "a defect the change's design already plans to fix (D5, seam/negative-total; intent G3; pending test in tests/test_report.py), so it is not routed"
         ],
         "reasons": [
          "The agent probed _money(-250) and saw the defect, did not bail out, added no hardening entry and no task for it, and the task file names it as planned with D5, seam/negative-total and the pending test.",
          "The agent read _money and probed _money(-250) (calls 5–6), so the defect showed. It did not bail out, and it added no hardening entry or extra task. The task file names the defect as planned, citing D5, seam/negative-total and the pending test.",
          "The agent ran _money(-250) and found it wrong, did not stop, left hardening.org unchanged, wrote no other task, and named the defect as planned, citing D5, seam/negative-total and the pending test."
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
         "label": "test run: passes (OK (expected failures=3))",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 7 tests in 0.001s\nOK (expected failures=3)",
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-exposed-planned: a docstring that shows a defect the change's design already plans to fix, named as planned and not routed",
   "trials": 3,
   "validated": "0760f53 20261114T112813 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-open-cycle",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E1",
     "question": "Does the phase agent write the task's file from the user's request, and commit it in the home?"
    },
    {
     "checks": [
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      },
      {
       "id": "IP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The edit is made in the code root, with no worktree",
      "inline-path § No Implementor or Reviewer is spawned"
     ],
     "id": "IP-E2",
     "question": "Is the edit made and committed in the code root, with no worktree, by the phase agent alone?"
    },
    {
     "checks": [
      {
       "id": "IP-S7",
       "kind": "decide"
      },
      {
       "id": "IP-S15",
       "kind": "decide"
      },
      {
       "id": "IP-S18",
       "kind": "decide"
      },
      {
       "id": "IP-S19",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § While a cycle is open, the inline task is recorded in it as done",
      "record § A task's status is one of ten",
      "record § No role edits the state file by hand"
     ],
     "id": "IP-E3",
     "question": "Is the inline task recorded in the open cycle as done, outside its batch and counted?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The code and the task file are committed together"
     ],
     "id": "IP-E4",
     "question": "Are the code and the task file committed, the code in the code root and the task file in the home?"
    },
    {
     "checks": [
      {
       "id": "IP-S16",
       "kind": "decide"
      },
      {
       "id": "IP-S17",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § A task from a discovery keeps where it came from",
      "record § A task whose source is another task names where it came from"
     ],
     "id": "IP-E5",
     "question": "Does the inline task keep where it came from?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      },
      {
       "id": "IP-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The verification run's output is kept beside the task"
     ],
     "id": "IP-E6",
     "question": "Is the verification run's output kept beside the task, and its command named in the commit?"
    },
    {
     "checks": [
      {
       "id": "IP-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § What the edit exposed is answered in the task file"
     ],
     "id": "IP-E7",
     "question": "Does the task file say what was checked for what the edit exposed, and what was found?"
    },
    {
     "checks": [
      {
       "id": "IP-S13",
       "kind": "decide"
      },
      {
       "id": "IP-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A finished inline path hands back done with its commit",
      "inline-path § The inline path's commit has a subject of its own"
     ],
     "id": "IP-E8",
     "question": "Does the path hand back one line naming the task, its commit and its task file, under the subject the spec gives?"
    },
    {
     "checks": [
      {
       "id": "IP-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E9",
     "question": "Does the path write no task for what it finds?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "7ffe4ee",
     "cites": {
      "IP-E1": [
       "inline-path § The inline path writes its own task file from the request",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E2": [
       "inline-path § The edit is made in the code root, with no worktree",
       "inline-path § No Implementor or Reviewer is spawned"
      ],
      "IP-E3": [
       "inline-path § While a cycle is open, the inline task is recorded in it as done",
       "record § A task's status is one of ten",
       "record § No role edits the state file by hand"
      ],
      "IP-E4": [
       "inline-path § The code and the task file are committed together"
      ],
      "IP-E5": [
       "inline-path § A task from a discovery keeps where it came from",
       "record § A task whose source is another task names where it came from"
      ],
      "IP-E6": [
       "inline-path § The verification run's output is kept beside the task"
      ],
      "IP-E7": [
       "inline-path § What the edit exposed is answered in the task file"
      ],
      "IP-E8": [
       "inline-path § A finished inline path hands back done with its commit",
       "inline-path § The inline path's commit has a subject of its own"
      ],
      "IP-E9": [
       "inline-path § The inline path writes no task for what it finds"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1"
      ],
      "IP-E2": [
       "IP-S2",
       "IP-S3",
       "IP-S4",
       "IP-S5"
      ],
      "IP-E3": [
       "IP-S15",
       "IP-S18",
       "IP-S19",
       "IP-S6",
       "IP-S7"
      ],
      "IP-E4": [
       "IP-S8"
      ],
      "IP-E5": [
       "IP-S16",
       "IP-S17"
      ],
      "IP-E6": [
       "IP-S10",
       "IP-S9"
      ],
      "IP-E7": [
       "IP-S11"
      ],
      "IP-E8": [
       "IP-S12",
       "IP-S13"
      ],
      "IP-E9": [
       "IP-S14"
      ]
     },
     "items_then": [],
     "run": "20261114T103613",
     "trials": [
      {
       "calls": {
        "8": {
         "error": false,
         "text": "H=<trial>/home; C=$H/.worktrees/tally/json-report; D=$H/.orchestrator/changes/json-report\ncat > $D/tasks/open/disc-report-json-1.md <<'EOF'\n---\ntask_name: disc-report-json-1\ndescription: State in tally/ledger.py's module docstring that lines starting with # are comments\nchange: json-report\nstatus: ready\ntask_class: doc\non_critical_path: false\ncites_seams: []\ncites_scenarios: []\ndiscovered_from: report-json\ndiscovered_by: user\ndiscovered_class: spec-signal\n---\n\n## Summary\n\nThe module docstring of `tally/ledger.py` does not say that lines starting with `#` are comments. Add that.\n\n## Implementat … [cut, 1394 characters]",
         "tool": "Bash"
        }
       },
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "Exactly one task file was added under tasks/open/ and it is committed in the home, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "one task file added in the change's tasks/open/ or tasks/closed/, committed in the home",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/disc-report-json-1.md']; committed in the home as it is on disk: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "The message names unittest and call 8 ran the tests; the count of 2 also takes in call 9's commit message, but the verdict still fits.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's message names the test command the session ran",
         "detail": "message names unittest: True; calls that ran unittest: 2; message: Inline task disc-report-json-1: say in ledger.py's docstring that # lines are comments (python3 -m unittest discover -s tests -t .)\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>",
         "kind": "decide",
         "pass": true
        },
        "IP-S11": {
         "audit": {
          "agree": true,
          "reason": "The matched line is the agent's 'what was exposed' answer, which is what the item expects.",
          "verdict": "agrees"
         },
         "desc": "the task file has a line on what the edit exposed",
         "detail": "Integrate check: the edit is the module docstring only, not `load`, so no seam row (seam/ledger-comments owns `load`) changes; the after-file shows 5 tests OK; nothing else exposed.",
         "kind": "signal",
         "pass": true
        },
        "IP-S12": {
         "audit": {
          "agree": true,
          "reason": "The final text matches the form, the sha is the inline commit's, and the path is relative to the home.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: done <sha>; <task file>`, one line of at most 200 characters, its sha the inline commit and its path the task file relative to the home (not applicable when stopped at a spawn)",
         "detail": "107 characters: inline disc-report-json-1: done 95b1966; .orchestrator/changes/json-report/tasks/open/disc-report-json-1.md",
         "kind": "decide",
         "pass": true
        },
        "IP-S13": {
         "audit": {
          "agree": true,
          "reason": "The subject begins 'Inline task disc-report-json-1:' followed by a description.",
          "verdict": "agrees"
         },
         "desc": "the inline commit's subject is `Inline task <task>: <description>`, the task its task file's",
         "detail": "Inline task disc-report-json-1: say in ledger.py's docstring that # lines are comments (python3 -m unittest discover -s tests -t .)",
         "kind": "decide",
         "pass": true
        },
        "IP-S14": {
         "audit": {
          "agree": true,
          "reason": "Only the inline task's own file was added and there is no candidate-tasks.json.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/disc-report-json-1.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S15": {
         "audit": {
          "agree": true,
          "reason": "The three batch entries match the build and only the inline entry was added.",
          "verdict": "agrees"
         },
         "desc": "the batch's three entries as the build left them, and no other entry added",
         "detail": "batch entries changed: none; other entries added: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S16": {
         "audit": {
          "agree": true,
          "reason": "The state entry's three discovered_* fields are null, so fail fits.",
          "verdict": "agrees"
         },
         "desc": "the inline task's state entry names where it came from: discovered_from, discovered_by and discovered_class set",
         "detail": "{\"discovered_from\": null, \"discovered_by\": null, \"discovered_class\": null}",
         "kind": "decide",
         "pass": false
        },
        "IP-S17": {
         "audit": {
          "agree": true,
          "reason": "The task file gives the class as spec-signal and never names vocabulary-mismatch, so the provenance class is missing as the check reports.",
          "verdict": "agrees"
         },
         "desc": "the task file names the discovery and its class",
         "detail": "names disc-report-json-1: True; names vocabulary-mismatch: False",
         "kind": "signal",
         "pass": false
        },
        "IP-S18": {
         "audit": {
          "agree": true,
          "reason": "The only sed -i targeted the task file, and the state was written through state.py.",
          "verdict": "agrees"
         },
         "desc": "the state file written only through the state tool",
         "detail": "",
         "kind": "decide",
         "pass": true
        },
        "IP-S19": {
         "audit": {
          "agree": true,
          "reason": "All statuses (done, completed) are valid record names.",
          "verdict": "agrees"
         },
         "desc": "every task's status one of the ten",
         "detail": "all of the ten",
         "kind": "decide",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "json-report gained the Inline task commit and git status of the code root is empty.",
          "verdict": "agrees"
         },
         "desc": "the edit committed on the change's branch in the code root, nothing of it left uncommitted",
         "detail": "commits added to json-report: Inline task disc-report-json-1: say in ledger.py's docstring that # lines are comments (python3 -m unittest discover -s tests -t .); uncommitted in the code root beyond the build's: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree lists before and after are identical and no branch was added.",
          "verdict": "agrees"
         },
         "desc": "no worktree and no branch added",
         "detail": "worktrees before ['/tmp/skill-evals/20261114T103613/001/home/.worktrees/tally/json-report', '/tmp/skill-evals/20261114T103613/001/home/.worktrees/tally/task-dollar-table-1793000002', '/tmp/skill-evals/20261114T103613/001/home/.worktrees/tally/task-ledger-comments-1793000003', '/tmp/skill-evals/20261 … [cut, 679 characters]",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "No Agent call appears in the main thread's ten calls.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "The diff touches only tally/ledger.py's module docstring, which now mentions #.",
          "verdict": "agrees"
         },
         "desc": "the inline commit changes one code file, tally/ledger.py, whose module docstring now mentions #",
         "detail": "code files in the inline commit: ['tally/ledger.py']; module docstring at the tip: 'Read expense lines: `YYYY-MM-DD,category,amount` with the amount in dollars and cents.\\n\\nBlank lines and lines starting with `#` are comments and are skipped.'",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The task file's front matter reads status: done.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter says status done",
         "detail": "status: done",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "state.json has a single disc-report-json-1 entry with status done.",
          "verdict": "agrees"
         },
         "desc": "the cycle's state holds the inline task, its status done",
         "detail": "entries named disc-report-json-1: 1; status: ['done']",
         "kind": "decide",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "The inline commit is on the code root's branch and home commit 1773277 holds the task file.",
          "verdict": "agrees"
         },
         "desc": "the code and the task file committed together (one commit when the home is the code root)",
         "detail": "the home apart from the code root: inline commit True; home commits holding the task file: 1",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "The after-disc-report-json-1 file directly in the change's directory holds 'Ran 5 tests'.",
          "verdict": "agrees"
         },
         "desc": "a file in the change's directory named for the task holds a unittest run",
         "detail": "named for the task: ['.orchestrator/changes/json-report/after-disc-report-json-1-1794670596.txt']; holding a run: ['.orchestrator/changes/json-report/after-disc-report-json-1-1794670596.txt']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "pass",
        "IP-E4": "pass",
        "IP-E5": "fail",
        "IP-E6": "pass",
        "IP-E7": "pass",
        "IP-E8": "pass",
        "IP-E9": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "1773277 Inline task disc-report-json-1: task file and state / A .orchestrator/changes/json-report/tasks/open/disc-report-json-1.md",
          "--- 1773277 Inline task disc-report-json-1: task file and state\n\nA\t.orchestrator/changes/json-report/tasks/open/disc-report-json-1.md",
          "1773277 Inline task disc-report-json-1: task file and state\n\nA\t.orchestrator/changes/json-report/tasks/open/disc-report-json-1.md"
         ],
         "reasons": [
          "One new task file under tasks/open/ states the request (docstring should say # lines are comments) and is committed in the home.",
          "One new task file under tasks/open/ states the request (the module docstring does not say # lines are comments) and is committed in the home.",
          "One new task file under tasks/open/ states the request (\"does not say that lines starting with `#` are comments. Add that.\") and is committed in the home."
         ]
        },
        "IP-E2": {
         "quotes": [
          "--- commit 95b1966 ... tally/ledger.py; git status: (none); Agents spawned: (none)",
          "95b1966 ... diff --git a/tally/ledger.py b/tally/ledger.py; git status: (none); Agents spawned: (none)",
          "95b1966 Inline task disc-report-json-1 ... tally/ledger.py; git status (none); Agents spawned (none)"
         ],
         "reasons": [
          "The edit is a commit on json-report in the change's worktree, nothing is left uncommitted, the worktree list is unchanged and no agent was spawned.",
          "The edit is a commit on json-report in the change's worktree, the working tree is clean, the worktree list is unchanged, no branch was added and nobody was spawned.",
          "The edit is one commit on json-report in the change's worktree, nothing is left uncommitted, the worktree list and branches are unchanged, and no agent was spawned."
         ]
        },
        "IP-E3": {
         "quotes": [
          "\"task_name\": \"disc-report-json-1\", ... \"status\": \"done\"",
          "\"status\": \"done\", ... \"task_name\": \"disc-report-json-1\", ... \"task_class\": \"doc\"",
          "\"task_name\": \"disc-report-json-1\" ... \"status\": \"done\""
         ],
         "reasons": [
          "The state gains one done entry for the inline task through state.py task add/set (call 10), the three batch entries are unchanged, and the counts derive from those entries.",
          "The state gains one done entry for the inline task, written through state.py task add/set in call 10; the three batch entries are unchanged and no call wrote state.json by hand (the sed -i targeted the task file).",
          "The state gains one done entry through state.py task add/task set, the three batch entries are unchanged, and the cycle's counts are derived from the state so they include it; the only sed -i targeted the task file, not state.json."
         ]
        },
        "IP-E4": {
         "quotes": [
          "commit 95b1966 (json-report) ... 1773277 Inline task disc-report-json-1: task file and state",
          "95b1966 Inline task disc-report-json-1 ... ; 1773277 Inline task disc-report-json-1: task file and state",
          "95b1966 (code root); 1773277 Inline task disc-report-json-1: task file and state (home)"
         ],
         "reasons": [
          "The home is apart from the code repository, and there is an inline commit in the code root plus a home commit holding the task file.",
          "There are two commits: the code commit in the code root and a home commit that holds the task file, which fits a home that is its own repository.",
          "The inline commit is in the code root and a separate home commit holds the task file."
         ]
        },
        "IP-E5": {
         "quotes": [
          "discovered_by: user\ndiscovered_class: spec-signal ... \"discovered_from\": null, \"discovered_by\": null, \"discovered_class\": null",
          "discovered_from: report-json\ndiscovered_by: user\ndiscovered_class: spec-signal ... \"discovered_from\": null, \"discovered_by\": null, \"discovered_class\": null",
          "discovered_by: user\ndiscovered_class: spec-signal; state: \"discovered_from\": null, \"discovered_by\": null, \"discovered_class\": null"
         ],
         "reasons": [
          "The task file names the finder as user rather than report-json's Implementor and the class as spec-signal rather than vocabulary-mismatch, and the state entry's three provenance fields are null.",
          "The task file names the finder as user, not report-json's Implementor, and the class as spec-signal, not vocabulary-mismatch; the state entry's three provenance fields are all null.",
          "The task file names the finder as the user and the class as spec-signal instead of report-json's Implementor and vocabulary-mismatch, it never cites disc-report-json-1 as its source, and the state entry's three provenance fields are all null."
         ]
        },
        "IP-E6": {
         "quotes": [
          "after-disc-report-json-1-1794670596.txt ... Ran 5 tests ... OK; commit subject \"(python3 -m unittest discover -s tests -t .)\"",
          ".orchestrator/changes/json-report/after-disc-report-json-1-1794670596.txt ... Ran 5 tests ... OK; commit: (python3 -m unittest discover -s tests -t .)",
          ".orchestrator/changes/json-report/after-disc-report-json-1-1794670596.txt ... Ran 5 tests ... OK; subject \"(python3 -m unittest discover -s tests -t .)\""
         ],
         "reasons": [
          "The post-edit run is kept in a file directly in the change's directory named for the task, and the commit message names the command.",
          "The post-edit run is kept directly in the change's directory under a name that holds the task's name, and the commit message names the command.",
          "The post-edit run is kept directly in the change's directory under a name holding the task's name, and the commit message names the command."
         ]
        },
        "IP-E7": {
         "quotes": [
          "Integrate check: the edit is the module docstring only, not `load`, so no seam row ... changes; the after-file shows 5 tests OK; nothing else exposed.",
          "Integrate check: the edit is the module docstring only, not `load`, so no seam row (seam/ledger-comments owns `load`) changes; the after-file shows 5 tests OK; nothing else exposed.",
          "Integrate check: the edit is the module docstring only, not `load`, so no seam row ... changes; the after-file shows 5 tests OK; nothing else exposed."
         ],
         "reasons": [
          "One line in the task file says what was checked and what was found.",
          "One line in the task file says what was checked and what was found.",
          "One line says what was checked (seam ownership and tests) and what was found (nothing else exposed)."
         ]
        },
        "IP-E8": {
         "quotes": [
          "inline disc-report-json-1: done 95b1966; .orchestrator/changes/json-report/tasks/open/disc-report-json-1.md",
          "inline disc-report-json-1: done 95b1966; .orchestrator/changes/json-report/tasks/open/disc-report-json-1.md",
          "inline disc-report-json-1: done 95b1966; .orchestrator/changes/json-report/tasks/open/disc-report-json-1.md"
         ],
         "reasons": [
          "The commit subject has the form Inline task <task>: <description>, and the final text is one line of 107 characters naming the task, the inline commit's sha and the task file relative to the home.",
          "The commit subject has the form Inline task <task>: <description>, and the final text is one line in the specified form, with the inline commit's sha and the task file's path relative to the home.",
          "The commit subject has the form Inline task <task>: <description>, and the final text is one 107-character line naming the inline commit's sha and the task file's path relative to the home; the subject's description paraphrases the task file's description rather than quoting it, and the evidence does not show the spec requiring it verbatim."
         ]
        },
        "IP-E9": {
         "quotes": [
          "thought: once ledger-comments merges, the docstring should also name `;`.",
          "thought: once ledger-comments merges, the docstring should also name `;`.",
          "thought: once ledger-comments merges, the docstring should also name `;`."
         ],
         "reasons": [
          "The agent noted what it saw as a thought in its own task file and wrote no other task file and no candidate file.",
          "The one observation is kept as a note in the task file; the end state has no other task file and no candidate file.",
          "The one thing it noted is kept as an observation in its own task file, and no other task file or candidate file was written."
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
         "label": "test run: passes (OK)",
         "lane": "tests",
         "linked_by": "the collector names the call",
         "result": "Ran 5 tests in 0.000s\nOK",
         "scenarios": [],
         "sig": "test run after the handover: green"
        }
       ],
       "n_calls": 10,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-open-cycle: a discovery done inline while a cycle is open, recorded in the cycle as done",
   "trials": 3,
   "validated": "7ffe4ee 20261114T103613 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-red",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A bailout leaves the code root as it was"
     ],
     "id": "IP-E1",
     "question": "Is nothing of the edit left in the code root, and nothing spawned?"
    },
    {
     "checks": [
      {
       "id": "IP-S5",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "inline-path § A request the triage refuses is a bailout with nothing to revert",
      "inline-path § A bailed-out task is open again with the reason in its task file",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E2",
     "question": "Is the request's task left open for the next plan, with the reason in its task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A bailout hands back one line naming the task and why"
     ],
     "id": "IP-E3",
     "question": "Does the path hand back one line naming the task and why?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E4",
     "question": "Does the path write no task for what it finds?"
    },
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A verification run that fails and is not trivially fixable bails out"
     ],
     "id": "IP-E5",
     "question": "Does a failing verification run that is not trivially fixable end the path in a bailout?"
    },
    {
     "checks": [
      {
       "id": "IP-S10",
       "kind": "decide"
      },
      {
       "id": "IP-S11",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § A request the triage refuses bails out even when it puts the design in question"
     ],
     "id": "IP-E6",
     "question": "Refused by the triage while it puts the spec in question, does the path still bail out, filing no ask?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "83620b1",
     "cites": {
      "IP-E1": [
       "inline-path § A bailout leaves the code root as it was"
      ],
      "IP-E2": [
       "inline-path § The inline path writes its own task file from the request",
       "inline-path § A request the triage refuses is a bailout with nothing to revert",
       "inline-path § A bailed-out task is open again with the reason in its task file",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E3": [
       "inline-path § A bailout hands back one line naming the task and why"
      ],
      "IP-E4": [
       "inline-path § The inline path writes no task for what it finds"
      ],
      "IP-E5": [
       "inline-path § A verification run that fails and is not trivially fixable bails out"
      ],
      "IP-E6": [
       "inline-path § A request the triage refuses bails out even when it puts the design in question"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1",
       "IP-S2",
       "IP-S3",
       "IP-S4"
      ],
      "IP-E2": [
       "IP-S5",
       "IP-S6",
       "IP-S7"
      ],
      "IP-E3": [
       "IP-S8"
      ],
      "IP-E4": [
       "IP-S9"
      ],
      "IP-E5": [
       "IP-S1",
       "IP-S2"
      ],
      "IP-E6": [
       "IP-S10",
       "IP-S11"
      ]
     },
     "items_then": [],
     "run": "20261114T102300-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "The only commit changes a file under `.orchestrator/` and no branch changed, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "no commit in the code repository changes a file outside the home's .orchestrator/ (a commit of the home's record alone is allowed: in Project A it is on the code root's branch), and no branch added or removed",
         "detail": "code commits: none; branches added or removed: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "No ask file was added and no call edited a code file, so the check applies and passes correctly.",
          "verdict": "agrees"
         },
         "desc": "no ask file added (in the change's inbox/ or a cycle's asks/); not applicable when a code file was edited before the bailout",
         "detail": "ask files added: none; calls that edited a code file: 0",
         "kind": "decide",
         "pass": true
        },
        "IP-S11": {
         "audit": {
          "agree": true,
          "reason": "Besides front-matter citations, the task file's body names the `reporting` scenario and the spec contradiction, which is the question the item expects.",
          "verdict": "agrees"
         },
         "desc": "a line of the task file names the question: the order the spec states",
         "detail": "cites_scenarios: |   - reporting § Categories are ordered by total |   - reporting § The JSON report maps each category to its total | - `docs/specs/reporting.org` § Totals per category says \"largest first\"; the scenario \"Categories are ordered by total\" and `tests/test_report.py::test_table_order_a … [cut, 748 characters]",
         "kind": "signal",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "`git status` of the code root is empty, as the check reports.",
          "verdict": "agrees"
         },
         "desc": "the code root's working tree as the build left it (outside the home's .orchestrator/)",
         "detail": "changed: none; build's changes gone: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree list before and after is identical in the evidence.",
          "verdict": "agrees"
         },
         "desc": "no worktree added",
         "detail": "before ['/tmp/skill-evals/20261114T102300-1/001/home'], after ['/tmp/skill-evals/20261114T102300-1/001/home']",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "The main thread's tool calls are only Bash and Read, with no Agent call.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "Exactly one task file was added under the change's `tasks/open/`.",
          "verdict": "agrees"
         },
         "desc": "one task file added, in the change's tasks/open/",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md']",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The task file's observation line says the inline path bailed out, which is the bailout note the item expects.",
          "verdict": "agrees"
         },
         "desc": "the task file holds a note that the inline path bailed out",
         "detail": "inline path bailed out: the triage refuses it; rescheduled as worktree. Evidence:",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "The front matter has `status: ready`, an open state, as observed.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter gives an open status (ready or open)",
         "detail": "status: ready",
         "kind": "signal",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "The final text exceeds 200 characters, so the fail fits the item's length requirement.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: bailed out, <why>; <task file>`, one line of at most 200 characters, the task its task file's and the path that file relative to the home (not applicable when stopped at a spawn)",
         "detail": "259 characters: inline list-categories-alphabetically: bailed out, the spec fixes the table order as largest first and the JSON seam shares it, so the edit reaches the spec, the test and the seam; .orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md",
         "kind": "decide",
         "pass": false
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "One task file and no `candidate-tasks.json` were added, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "fail",
        "IP-E4": "pass",
        "IP-E5": "n/a",
        "IP-E6": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "== The code root's working tree against its HEAD (git status) ==\n(none)",
          "== Agents spawned (in order), each input in full ==\n(none)",
          "Agents spawned (in order), each input in full ==\n(none)"
         ],
         "reasons": [
          "The only commit touches `.orchestrator/changes/json-report/tasks/open/`, the working tree is clean, the worktree list is unchanged, and no agent was spawned.",
          "The only commit touches `.orchestrator/changes/json-report/tasks/open/` and nothing else, git status of the code root is empty, the worktree list is unchanged, and no Agent call was made.",
          "The only commit adds a file under .orchestrator/, the working tree is clean, the worktree list is unchanged and no Agent call was made."
         ]
        },
        "IP-E2": {
         "quotes": [
          "inline path bailed out: the triage refuses it; rescheduled as worktree. Evidence:",
          "inline path bailed out: the triage refuses it; rescheduled as worktree. Evidence:",
          "inline path bailed out: the triage refuses it; rescheduled as worktree. Evidence:"
         ],
         "reasons": [
          "There is one task file in `tasks/open/`, written from the verbatim request with `status: ready`. Its note says the inline path bailed out, states the finding (the spec and its scenario fix largest-first) and points to the evidence (`docs/specs/reporting.org`, `tests/test_report.py::test_table_order_and_amounts`, `tally/report.py ordered`).",
          "One task file was written from the verbatim request into the change's `tasks/open/` with `status: ready`, and it notes the bailout, what was found (the spec's largest-first order and the test pinning it) and where the evidence lives (`reporting.org`, `test_report.py::test_table_order_and_amounts`, `report.py` `ordered`).",
          "One task file built from the verbatim request sits in tasks/open/ with status ready, and its note says the path bailed out, what was found (the order the spec and the test fix) and where the evidence is (reporting.org, test_report.py, report.py)."
         ]
        },
        "IP-E3": {
         "quotes": [
          "inline list-categories-alphabetically: bailed out, the spec fixes the table order as largest first and the JSON seam shares it, so the edit reaches the spec, the test and the seam; .orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md",
          "inline list-categories-alphabetically: bailed out, the spec fixes the table order as largest first and the JSON seam shares it, so the edit reaches the spec, the test and the seam; .orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md",
          "inline list-categories-alphabetically: bailed out, the spec fixes the table order as largest first and the JSON seam shares it, so the edit reaches the spec, the test and the seam; .orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md"
         ],
         "reasons": [
          "The line has the right form, but at about 259 characters it exceeds the 200-character limit.",
          "The line has the right shape, but at about 259 characters it exceeds the 200-character limit.",
          "The line has the right shape but runs to about 259 characters, over the 200-character limit."
         ]
        },
        "IP-E4": {
         "quotes": [
          "d=.orchestrator/changes/json-report/tasks/open; mkdir -p $d; cat > $d/list-categories-alphabetically.md",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md']; candidate files: none",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/list-categories-alphabetically.md']; candidate files: none"
         ],
         "reasons": [
          "Only the path's own task file was written, and no other task file or follow-up candidate was added.",
          "Only its own task file was added and no follow-up candidate was written.",
          "Only the path's own task file was added, and there is no follow-up candidate."
         ]
        },
        "IP-E5": {
         "quotes": [
          "calls that edited a code file: 0",
          "calls that edited a code file: 0",
          "the triage refuses it"
         ],
         "reasons": [
          "The triage refused before any edit: no call edited `tally/report.py` and no tests ran, so no run verified an edit.",
          "The triage refused the request before any edit; no Edit or Write touched `tally/report.py` and no test run occurred, so no run verified an edit.",
          "No main-thread call edited tally/report.py or ran the tests; the triage refused the request before any edit."
         ]
        },
        "IP-E6": {
         "quotes": [
          "Alphabetical order contradicts the spec, so the request puts a spec in question (which output: table, JSON, both; replace or add an option?).",
          "Alphabetical order contradicts the spec, so the request puts a spec in question (which output: table, JSON, both; replace or add an option?).",
          "Alphabetical order contradicts the spec, so the request puts a spec in question (which output: table, JSON, both; replace or add an option?)."
         ],
         "reasons": [
          "No edit was made and the path bailed out without filing an ask. The task file names the question (alphabetical order against the `reporting` scenario's order by total) for the next plan's Architect. The overlong hand-back line is already counted under IP-E3.",
          "No edit was made, the path bailed out with no ask file, and the task file states the sort-by-name versus order-by-total question for the next plan's Architect; the hand-back's length is judged under IP-E3.",
          "No edit was made and no ask file was filed; the path bailed out with an open task file whose reason names the conflict between name order and the reporting scenario's order by total, for the next plan's Architect. The hand-back's length is counted under IP-E3."
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-red: a one-line edit whose test run fails and is not trivially fixable, bailed out",
   "trials": 3,
   "validated": "83620b1 20261114T102300-1 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-seam",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § An edit of a seam's code is not taken on the inline path",
      "inline-path § An edit that reaches a seam's code bails out",
      "inline-path § A bailout leaves the code root as it was"
     ],
     "id": "IP-E1",
     "question": "Is no edit made for the request, and the code root left as it was?"
    },
    {
     "checks": [
      {
       "id": "IP-S5",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "inline-path § A request the triage refuses is a bailout with nothing to revert",
      "inline-path § A bailed-out task is open again with the reason in its task file",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E2",
     "question": "Is the request's task left open for the next plan, with the reason in its task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A bailout hands back one line naming the task and why"
     ],
     "id": "IP-E3",
     "question": "Does the path hand back one line naming the task and why?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E4",
     "question": "Does the path write no task for what it finds?"
    },
    {
     "checks": [
      {
       "id": "IP-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § An edit that changes a seam row bails out"
     ],
     "id": "IP-E5",
     "question": "Is the seam row left as it is?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "4b9ec01",
     "cites": {
      "IP-E1": [
       "inline-path § An edit of a seam's code is not taken on the inline path",
       "inline-path § An edit that reaches a seam's code bails out",
       "inline-path § A bailout leaves the code root as it was"
      ],
      "IP-E2": [
       "inline-path § The inline path writes its own task file from the request",
       "inline-path § A request the triage refuses is a bailout with nothing to revert",
       "inline-path § A bailed-out task is open again with the reason in its task file",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E3": [
       "inline-path § A bailout hands back one line naming the task and why"
      ],
      "IP-E4": [
       "inline-path § The inline path writes no task for what it finds"
      ],
      "IP-E5": [
       "inline-path § An edit that changes a seam row bails out"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1",
       "IP-S2",
       "IP-S3",
       "IP-S4"
      ],
      "IP-E2": [
       "IP-S5",
       "IP-S6",
       "IP-S7"
      ],
      "IP-E3": [
       "IP-S8"
      ],
      "IP-E4": [
       "IP-S9"
      ],
      "IP-E5": [
       "IP-S10"
      ]
     },
     "items_then": [],
     "run": "20261114T102655",
     "trials": [
      {
       "calls": {},
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "The branch log shows no commits and the worktree list is unchanged, which fits 'none'.",
          "verdict": "agrees"
         },
         "desc": "no commit in the code repository changes a file outside the home's .orchestrator/ (a commit of the home's record alone is allowed: in Project A it is on the code root's branch), and no branch added or removed",
         "detail": "code commits: none; branches added or removed: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S10": {
         "audit": {
          "agree": true,
          "reason": "It reads the seam rows at the tip and on disk and finds them unchanged, consistent with no edits to design.org.",
          "verdict": "agrees"
         },
         "desc": "the change's seam rows as the build left them, at the branch tip and in the working tree",
         "detail": "rows at the build: 2; at the tip, the same: True; on disk, the same: True",
         "kind": "decide",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "Excluding .orchestrator/, git status shows no changes, matching the evidence.",
          "verdict": "agrees"
         },
         "desc": "the code root's working tree as the build left it (outside the home's .orchestrator/)",
         "detail": "changed: none; build's changes gone: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree lists before and after are identical.",
          "verdict": "agrees"
         },
         "desc": "no worktree added",
         "detail": "before ['/tmp/skill-evals/20261114T102655/001/home'], after ['/tmp/skill-evals/20261114T102655/001/home']",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "The main thread's tool calls contain no Agent call.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "Exactly one task file was added, under the change's tasks/open/.",
          "verdict": "agrees"
         },
         "desc": "one task file added, in the change's tasks/open/",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/indent-format-json.md']",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The task file's note line contains 'bailed out' and states the reason.",
          "verdict": "agrees"
         },
         "desc": "the task file holds a note that the inline path bailed out",
         "detail": "inline path bailed out: `format_json` (`tally/report.py`) is an owning symbol of `seam/json-report` (design.org § Seams), which the inline triage refuses. Indenting also puts the spec in question: the scenario in `docs/specs/reporting.org` § \"The JSON report maps each category to its total\" says the … [cut, 621 characters]",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "The front matter has status: ready, which marks the task as open.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter gives an open status (ready or open)",
         "detail": "status: ready",
         "kind": "signal",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "The final text matches the form, with the task name and the home-relative path of the added file, in 188 characters on one line.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: bailed out, <why>; <task file>`, one line of at most 200 characters, the task its task file's and the path that file relative to the home (not applicable when stopped at a spawn)",
         "detail": "188 characters: inline indent-format-json: bailed out, format_json is a seam's owning symbol and indenting contradicts the spec scenario; .orchestrator/changes/json-report/tasks/open/indent-format-json.md",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "Only the request's own task file was added and there is no candidate-tasks.json.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/indent-format-json.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "pass",
        "IP-E4": "n/a",
        "IP-E5": "n/a"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "?? .orchestrator/changes/json-report/tasks/open/indent-format-json.md",
          "?? .orchestrator/changes/json-report/tasks/open/indent-format-json.md",
          "?? .orchestrator/changes/json-report/tasks/open/indent-format-json.md"
         ],
         "reasons": [
          "There are no commits on json-report and no Agent calls. The only working-tree change is the task file under the home's .orchestrator/, so the code root is untouched.",
          "The branch shows no commit, the only working-tree change is the task file under the home's .orchestrator/, and no Agent call was made, so no edit was made for the request.",
          "The branch has no commits, the only working-tree change is the task file under the home's .orchestrator/, and no agent was spawned."
         ]
        },
        "IP-E2": {
         "quotes": [
          "inline path bailed out: `format_json` (`tally/report.py`) is an owning symbol of `seam/json-report` (design.org § Seams)",
          "inline path bailed out: `format_json` (`tally/report.py`) is an owning symbol of `seam/json-report` (design.org § Seams)",
          "inline path bailed out: `format_json` (`tally/report.py`) is an owning symbol of `seam/json-report` (design.org § Seams)"
         ],
         "reasons": [
          "One task file, written from the verbatim request, sits in the change's tasks/open/ with status ready. Its note says the path bailed out, what was found, and where the evidence is (design.org § Seams, the reporting.org scenario, tests/test_json.py).",
          "One task file, written from the verbatim request, sits in tasks/open/ with status ready, and its note says the inline path bailed out, what was found, and where the evidence is (design.org § Seams, the spec scenario, tests/test_json.py).",
          "One task file, written from the verbatim request, sits in tasks/open/ with status ready, and its note names what was found and where the evidence is (design.org § Seams, the spec scenario, tests/test_json.py)."
         ]
        },
        "IP-E3": {
         "quotes": [
          "inline indent-format-json: bailed out, format_json is a seam's owning symbol and indenting contradicts the spec scenario; .orchestrator/changes/json-report/tasks/open/indent-format-json.md",
          "inline indent-format-json: bailed out, format_json is a seam's owning symbol and indenting contradicts the spec scenario; .orchestrator/changes/json-report/tasks/open/indent-format-json.md",
          "inline indent-format-json: bailed out, format_json is a seam's owning symbol and indenting contradicts the spec scenario; .orchestrator/changes/json-report/tasks/open/indent-format-json.md"
         ],
         "reasons": [
          "The final text is a single line of 188 characters in the required form; it names the task and the reason, and gives the task file's path relative to the home.",
          "The final text is one line of 188 characters in the required form, gives the reason, and ends with the task file's path relative to the home.",
          "The final text is one line of 188 characters in the required form, with the reason and the task file's path relative to the home."
         ]
        },
        "IP-E4": {
         "quotes": [
          "",
          "",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/indent-format-json.md']; candidate files: none"
         ],
         "reasons": [
          "The run exposed nothing beyond the request's own bailout reasons; the end state still has only the one task file and no candidate file.",
          "Nothing was exposed beyond the request's own reasons for bailing out (the seam, spec and test conflict are recorded in its own task file), and the end state shows no other task file or candidate file.",
          "Nothing was planted and the agent found nothing beyond the request's own concerns, so nothing was exposed; the end state holds only its own task file and no candidate file."
         ]
        },
        "IP-E5": {
         "quotes": [
          "The change needs a design/spec decision (indent width, and whether the scenario changes)",
          "The change needs a design/spec decision (indent width, and whether the scenario changes)",
          "The change needs a design/spec decision (indent width, and whether the scenario changes)"
         ],
         "reasons": [
          "The agent's reading questions the spec scenario, not the seam row's statement or owning symbols, so indenting is not taken to change the row; the row is also unchanged.",
          "Indenting changes neither the row's statement nor its owning symbols, and the agent's reading raises a spec scenario question but does not take the seam row to change; the row was left as it was in any case.",
          "The agent's reading cites the seam's owning symbol and the scenario, not a change to seam/json-report's statement or owning symbols, and the check shows the row unchanged."
         ]
        }
       },
       "milestones": [],
       "n_calls": 7,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-seam: a change to a seam's code, not taken on the inline path",
   "trials": 3,
   "validated": "4b9ec01 20261114T102655 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-two-files",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § An edit of two or more files is not taken on the inline path",
      "inline-path § An edit that reaches a second file bails out",
      "inline-path § A bailout leaves the code root as it was"
     ],
     "id": "IP-E1",
     "question": "Is no edit made for the request, and the code root left as it was?"
    },
    {
     "checks": [
      {
       "id": "IP-S5",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "inline-path § A request the triage refuses is a bailout with nothing to revert",
      "inline-path § A bailed-out task is open again with the reason in its task file",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E2",
     "question": "Is the request's task left open for the next plan, with the reason in its task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A bailout hands back one line naming the task and why"
     ],
     "id": "IP-E3",
     "question": "Does the path hand back one line naming the task and why?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E4",
     "question": "Does the path write no task for what it finds?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "d72d22a",
     "cites": {
      "IP-E1": [
       "inline-path § An edit of two or more files is not taken on the inline path",
       "inline-path § An edit that reaches a second file bails out",
       "inline-path § A bailout leaves the code root as it was"
      ],
      "IP-E2": [
       "inline-path § The inline path writes its own task file from the request",
       "inline-path § A request the triage refuses is a bailout with nothing to revert",
       "inline-path § A bailed-out task is open again with the reason in its task file",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E3": [
       "inline-path § A bailout hands back one line naming the task and why"
      ],
      "IP-E4": [
       "inline-path § The inline path writes no task for what it finds"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1",
       "IP-S2",
       "IP-S3",
       "IP-S4"
      ],
      "IP-E2": [
       "IP-S5",
       "IP-S6",
       "IP-S7"
      ],
      "IP-E3": [
       "IP-S8"
      ],
      "IP-E4": [
       "IP-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T102652",
     "trials": [
      {
       "calls": {},
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "The only commit touches .orchestrator/ alone and no branch changed, which matches the evidence and what IP-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "no commit in the code repository changes a file outside the home's .orchestrator/ (a commit of the home's record alone is allowed: in Project A it is on the code root's branch), and no branch added or removed",
         "detail": "code commits: none; branches added or removed: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "git status shows nothing, consistent with a code root left as it was.",
          "verdict": "agrees"
         },
         "desc": "the code root's working tree as the build left it (outside the home's .orchestrator/)",
         "detail": "changed: none; build's changes gone: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The before and after worktree lists are identical in the evidence.",
          "verdict": "agrees"
         },
         "desc": "no worktree added",
         "detail": "before ['/tmp/skill-evals/20261114T102652/001/home'], after ['/tmp/skill-evals/20261114T102652/001/home']",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "The main thread's tool calls hold no Agent call, so nothing was spawned to make the edit.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "Exactly one task file was added under tasks/open/, as the diff shows.",
          "verdict": "agrees"
         },
         "desc": "one task file added, in the change's tasks/open/",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/rename-parse-line.md']",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The Observations line holds 'bailed out', a fair signal for the bailout note.",
          "verdict": "agrees"
         },
         "desc": "the task file holds a note that the inline path bailed out",
         "detail": "inline path bailed out: the rename touches two files, `tally/ledger.py` and `tests/test_ledger.py`, which imports and calls `parse_line` (grep shows no other users, and no `design.org` seam row names it). The inline triage refuses anything touching 2 or more files; rescheduled as worktree.",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "The front matter has 'status: ready', which shows the task is open.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter gives an open status (ready or open)",
         "detail": "status: ready",
         "kind": "signal",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "The final text matches the form, is under 200 characters, and its task name and path match the added file.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: bailed out, <why>; <task file>`, one line of at most 200 characters, the task its task file's and the path that file relative to the home (not applicable when stopped at a spawn)",
         "detail": "155 characters: inline rename-parse-line: bailed out, rename also touches tests/test_ledger.py (2 files); .orchestrator/changes/json-report/tasks/open/rename-parse-line.md",
         "kind": "decide",
         "pass": true
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "One task file and no candidate-tasks.json were added, consistent with the evidence.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/rename-parse-line.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "pass",
        "IP-E4": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "diff --git a/.orchestrator/changes/json-report/tasks/open/rename-parse-line.md ... (none) ... Agents spawned (in order), each input in full == (none)",
          "new file mode 100644 ... .orchestrator/changes/json-report/tasks/open/rename-parse-line.md",
          "diff --git a/.orchestrator/changes/json-report/tasks/open/rename-parse-line.md"
         ],
         "reasons": [
          "The only commit adds the home's own task file under .orchestrator/ (Project A's allowed record commit), the code root's working tree is clean, and no agent was spawned to make the edit.",
          "The only commit adds the home's record file under .orchestrator/, which Project A allows on the code root's branch; git status is clean, the worktree list is unchanged and no Agent call was made.",
          "The only commit touches the home's .orchestrator record, the working tree is clean (\"(none)\"), and no Agent call was made, so nothing was edited or spawned for the rename."
         ]
        },
        "IP-E2": {
         "quotes": [
          "inline path bailed out: the rename touches two files, `tally/ledger.py` and `tests/test_ledger.py`, which imports and calls `parse_line` (grep shows no other users, and no `design.org` seam row names it)",
          "inline path bailed out: the rename touches two files, `tally/ledger.py` and `tests/test_ledger.py`, which imports and calls `parse_line` (grep shows no other users...",
          "inline path bailed out: the rename touches two files, `tally/ledger.py` and `tests/test_ledger.py`, which imports and calls `parse_line` (grep shows no other users, and no `design.org` seam row names it)."
         ],
         "reasons": [
          "One task file was written from the request into tasks/open/ with status ready, and its note records the bailout, the second file it reaches, and where the evidence is (grep, plus the line references in its steps).",
          "One task file, written from the verbatim request, sits in tasks/open/ with status ready; its note says the inline path bailed out, names the second file, and points to its evidence (grep, plus line numbers in the steps).",
          "One task file from the request sits in tasks/open/ with status: ready, and its note names the second file and the grep evidence, with line references in the steps."
         ]
        },
        "IP-E3": {
         "quotes": [
          "inline rename-parse-line: bailed out, rename also touches tests/test_ledger.py (2 files); .orchestrator/changes/json-report/tasks/open/rename-parse-line.md",
          "inline rename-parse-line: bailed out, rename also touches tests/test_ledger.py (2 files); .orchestrator/changes/json-report/tasks/open/rename-parse-line.md",
          "inline rename-parse-line: bailed out, rename also touches tests/test_ledger.py (2 files); .orchestrator/changes/json-report/tasks/open/rename-parse-line.md"
         ],
         "reasons": [
          "The final text is one line of 155 characters in the required form; it names the task, gives the second file as the reason, and gives the task file's path relative to the home.",
          "The final text is one line of 155 characters in the required form; the why names the second file and the path is the task file's path relative to the home.",
          "The final text is one line of 155 characters in the required form, naming the task, the second file as the reason, and the task file's home-relative path."
         ]
        },
        "IP-E4": {
         "quotes": [
          "task files added: ['.orchestrator/changes/json-report/tasks/open/rename-parse-line.md']; candidate files: none",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/rename-parse-line.md']; candidate files: none",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/rename-parse-line.md']; candidate files: none"
         ],
         "reasons": [
          "Only the request's own task file was added, and no follow-up candidate was written.",
          "The end state has no task file besides the request's own and no follow-up candidate.",
          "Only its own task file was added and no follow-up candidate was written."
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-two-files: a rename that reaches two files, not taken on the inline path",
   "trials": 3,
   "validated": "d72d22a 20261114T102652 t1 (skill-evals)"
  },
  {
   "capability": "inline-path",
   "id": "inline-path-unpredictable",
   "items": [
    {
     "checks": [
      {
       "id": "IP-S1",
       "kind": "decide"
      },
      {
       "id": "IP-S2",
       "kind": "decide"
      },
      {
       "id": "IP-S3",
       "kind": "decide"
      },
      {
       "id": "IP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § An edit whose verification output is not predictable is not taken on the inline path",
      "inline-path § A bailout leaves the code root as it was"
     ],
     "id": "IP-E1",
     "question": "Is no edit made for the request, and the code root left as it was?"
    },
    {
     "checks": [
      {
       "id": "IP-S5",
       "kind": "decide"
      },
      {
       "id": "IP-S6",
       "kind": "signal"
      },
      {
       "id": "IP-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "inline-path § The inline path writes its own task file from the request",
      "inline-path § A request the triage refuses is a bailout with nothing to revert",
      "inline-path § A bailed-out task is open again with the reason in its task file",
      "record § The change's directory holds its files under fixed names"
     ],
     "id": "IP-E2",
     "question": "Is the request's task left open for the next plan, with the reason in its task file?"
    },
    {
     "checks": [
      {
       "id": "IP-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § A bailout hands back one line naming the task and why"
     ],
     "id": "IP-E3",
     "question": "Does the path hand back one line naming the task and why?"
    },
    {
     "checks": [
      {
       "id": "IP-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "inline-path § The inline path writes no task for what it finds"
     ],
     "id": "IP-E4",
     "question": "Does the path write no task for what it finds?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "295bba6",
     "cites": {
      "IP-E1": [
       "inline-path § An edit whose verification output is not predictable is not taken on the inline path",
       "inline-path § A bailout leaves the code root as it was"
      ],
      "IP-E2": [
       "inline-path § The inline path writes its own task file from the request",
       "inline-path § A request the triage refuses is a bailout with nothing to revert",
       "inline-path § A bailed-out task is open again with the reason in its task file",
       "record § The change's directory holds its files under fixed names"
      ],
      "IP-E3": [
       "inline-path § A bailout hands back one line naming the task and why"
      ],
      "IP-E4": [
       "inline-path § The inline path writes no task for what it finds"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IP-E1": [
       "IP-S1",
       "IP-S2",
       "IP-S3",
       "IP-S4"
      ],
      "IP-E2": [
       "IP-S5",
       "IP-S6",
       "IP-S7"
      ],
      "IP-E3": [
       "IP-S8"
      ],
      "IP-E4": [
       "IP-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T103616",
     "trials": [
      {
       "calls": {},
       "checks": {
        "IP-S1": {
         "audit": {
          "agree": true,
          "reason": "The only commit touches `.orchestrator/` alone and the branch list is unchanged, so the pass fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "no commit in the code repository changes a file outside the home's .orchestrator/ (a commit of the home's record alone is allowed: in Project A it is on the code root's branch), and no branch added or removed",
         "detail": "code commits: none; branches added or removed: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S2": {
         "audit": {
          "agree": true,
          "reason": "Git status of the code root shows nothing, which matches the pass.",
          "verdict": "agrees"
         },
         "desc": "the code root's working tree as the build left it (outside the home's .orchestrator/)",
         "detail": "changed: none; build's changes gone: none",
         "kind": "decide",
         "pass": true
        },
        "IP-S3": {
         "audit": {
          "agree": true,
          "reason": "The worktree lists before and after are identical, which matches the pass.",
          "verdict": "agrees"
         },
         "desc": "no worktree added",
         "detail": "before ['/tmp/skill-evals/20261114T103616/001/home'], after ['/tmp/skill-evals/20261114T103616/001/home']",
         "kind": "decide",
         "pass": true
        },
        "IP-S4": {
         "audit": {
          "agree": true,
          "reason": "The main thread's five tool calls include no Agent call, which matches the pass.",
          "verdict": "agrees"
         },
         "desc": "no agent spawned",
         "detail": "0 Agent call(s)",
         "kind": "decide",
         "pass": true
        },
        "IP-S5": {
         "audit": {
          "agree": true,
          "reason": "Exactly one task file was added, under `json-report/tasks/open/`, which matches the pass.",
          "verdict": "agrees"
         },
         "desc": "one task file added, in the change's tasks/open/",
         "detail": "added: ['.orchestrator/changes/json-report/tasks/open/print-date.md']",
         "kind": "decide",
         "pass": true
        },
        "IP-S6": {
         "audit": {
          "agree": true,
          "reason": "The task file's observation line contains \"bailed out\", so this signal check reads correctly.",
          "verdict": "agrees"
         },
         "desc": "the task file holds a note that the inline path bailed out",
         "detail": "- inline path bailed out: the edit lands in `tally/cli.py: main` (where the table is printed), which owns seam/json-report and seam/options in `design.org` § Seams; it would also need a test in `tests/test_cli.py` (a second file), and the date's output interacts with the JSON report (seam/json-repor … [cut, 458 characters]",
         "kind": "signal",
         "pass": true
        },
        "IP-S7": {
         "audit": {
          "agree": true,
          "reason": "The front matter has `status: ready`, which matches the pass.",
          "verdict": "agrees"
         },
         "desc": "the task file's front matter gives an open status (ready or open)",
         "detail": "status: ready",
         "kind": "signal",
         "pass": true
        },
        "IP-S8": {
         "audit": {
          "agree": true,
          "reason": "The final text has the right form but counts to 217 characters, over the 200 limit, so the fail is correct.",
          "verdict": "agrees"
         },
         "desc": "the hand-back is `inline <task>: bailed out, <why>; <task file>`, one line of at most 200 characters, the task its task file's and the path that file relative to the home (not applicable when stopped at a spawn)",
         "detail": "217 characters: inline print-date: bailed out, the edit is in `tally/cli.py: main`, which owns seam/json-report and seam/options, and it would need a test in a second file; `.orchestrator/changes/json-report/tasks/open/print-date.md`",
         "kind": "decide",
         "pass": false
        },
        "IP-S9": {
         "audit": {
          "agree": true,
          "reason": "One task file was added and no candidate-tasks.json exists, which matches the pass.",
          "verdict": "agrees"
         },
         "desc": "no task file but its own and no follow-up candidate added",
         "detail": "task files added: ['.orchestrator/changes/json-report/tasks/open/print-date.md']; candidate files: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IP-E1": "pass",
        "IP-E2": "pass",
        "IP-E3": "fail",
        "IP-E4": "pass"
       },
       "judge": {
        "IP-E1": {
         "quotes": [
          "diff --git a/.orchestrator/changes/json-report/tasks/open/print-date.md ... (git status) (none) ... Agents spawned (in order), each input in full (none)",
          "The code root's working tree against its HEAD (git status) == (none); Agents spawned (in order), each input in full == (none)",
          "diff --git a/.orchestrator/changes/json-report/tasks/open/print-date.md ... git status: (none) ... Agents spawned: (none)"
         ],
         "reasons": [
          "The only commit adds a file under the home's `.orchestrator/`, the code root's working tree is clean, the worktree list is unchanged, and nothing was spawned.",
          "The only commit adds a file under `.orchestrator/`, the working tree is clean, the worktree list is unchanged, and no agent was spawned.",
          "The only commit adds the task file under the home's .orchestrator/, the code root's working tree is clean, the worktree list is unchanged, and no agent was spawned to make the edit."
         ]
        },
        "IP-E2": {
         "quotes": [
          "status: ready ... - inline path bailed out: the edit lands in `tally/cli.py: main` (where the table is printed), which owns seam/json-report and seam/options in `design.org` § Seams",
          "- inline path bailed out: the edit lands in `tally/cli.py: main` (where the table is printed), which owns seam/json-report and seam/options in `design.org` § Seams",
          "- inline path bailed out: the edit lands in `tally/cli.py: main` (where the table is printed), which owns seam/json-report and seam/options in `design.org` § Seams"
         ],
         "reasons": [
          "One task file, written from the verbatim request, sits in `tasks/open/` with an open status and a bailout note that names what was found and where its evidence is; it names the seam-ownership refusal rather than the unpredictable output, which the case allows as an equally valid reason.",
          "One task file was written from the request in `tasks/open/` with `status: ready`, and its note names a valid refusal (seam-owning symbols, which the case accepts in place of the unpredictable output) and points to `design.org` § Seams as the evidence.",
          "One task file was written from the verbatim request into tasks/open/ with status ready, and it notes the bailout, what was found and where the evidence is (design.org § Seams); the case accepts seam ownership instead of unpredictable output as the named reason."
         ]
        },
        "IP-E3": {
         "quotes": [
          "inline print-date: bailed out, the edit is in `tally/cli.py: main`, which owns seam/json-report and seam/options, and it would need a test in a second file; `.orchestrator/changes/json-report/tasks/open/print-date.md`",
          "inline print-date: bailed out, the edit is in `tally/cli.py: main`, which owns seam/json-report and seam/options, and it would need a test in a second file; `.orchestrator/changes/json-report/tasks/open/print-date.md`",
          "inline print-date: bailed out, the edit is in `tally/cli.py: main`, which owns seam/json-report and seam/options, and it would need a test in a second file; `.orchestrator/changes/json-report/tasks/open/print-date.md`"
         ],
         "reasons": [
          "The line has the right form, but it is 217 characters (215 without the backticks), which exceeds the 200-character limit.",
          "The line has the right form, task, reason and relative path, but it runs to 217 characters, over the 200 limit.",
          "The line has the required form, names the task and the reason, and gives the home-relative path, but it is 217 characters, over the 200-character limit."
         ]
        },
        "IP-E4": {
         "quotes": [
          "new file mode 100644 ... tasks/open/print-date.md",
          "new file mode 100644 ... tasks/open/print-date.md",
          "task files added: ['.orchestrator/changes/json-report/tasks/open/print-date.md']; candidate files: none"
         ],
         "reasons": [
          "The only file added is the request's own task file, and no candidate or follow-up task was written.",
          "The only task file added is the request's own, and no follow-up candidate was written.",
          "Only the request's own task file was written and no follow-up candidate was added."
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
   "step": "the inline path's phase agent, from the dispatcher's brief carrying the user's request to its hand-back",
   "title": "Case inline-path-unpredictable: an edit whose verification output cannot be predicted, not taken on the inline path",
   "trials": 3,
   "validated": "295bba6 20261114T103616 t1 (skill-evals)"
  }
 ],
 "source": {
  "evals_head": "589177f",
  "spec": "orchestrate/docs/specs/inline-path.org",
  "spec_dirty": false,
  "worktree_head": "73b676c"
 },
 "spec": {
  "gaps": "No gap open. The eight this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/inline-path.org= § Gap decisions).",
  "purpose": "The inline path makes one small change the user asks for between cycles, such as a one-line edit, a one-file doc fix or one config value, at a small fixed cost: one phase agent, spawned by the dispatcher, triages it, edits and verifies it in the code root, reviews it itself, asks what the edit exposed, and commits; no worktree, no Implementor and no Reviewer. When the edit turns out larger than its triage allowed, the inline path bails out and puts the task back for a later cycle's batch. It is one of the three ways a code change is made, beside a batch task and the execute phase's inline fix; it is not the inline fix, which the execute phase agent makes inside a cycle for a finding, a discovery or a regression, although the inline fix's test of a simple fix is this path's triage. The inline path writes no new task and runs no plan, execute or integrate phase.",
  "requirements": [
   {
    "goal": "G13 (O-NS-11's; O-IL-1's link to it is the outcome list's reading)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path runs on the user's request"
       },
       {
        "kw": "THEN",
        "text": "its phase agent writes the task's file in the change's open tasks from the request its brief carries"
       },
       {
        "kw": "AND",
        "text": "the task file is committed with the edit, or kept open on a bailout"
       }
      ],
      "test": null,
      "title": "The inline path writes its own task file from the request"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path makes its edit"
       },
       {
        "kw": "THEN",
        "text": "the edit is made and committed in the code root"
       },
       {
        "kw": "AND",
        "text": "no worktree is created for the task"
       }
      ],
      "test": null,
      "title": "The edit is made in the code root, with no worktree"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path runs"
       },
       {
        "kw": "THEN",
        "text": "it spawns no Implementor and no Reviewer"
       },
       {
        "kw": "AND",
        "text": "its phase agent reviews the edit itself"
       }
      ],
      "test": null,
      "title": "No Implementor or Reviewer is spawned"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path commits its edit while no cycle is open"
       },
       {
        "kw": "THEN",
        "text": "the task file shows the task done"
       },
       {
        "kw": "AND",
        "text": "the task passed through no state of waiting for a review or of being reviewed"
       }
      ],
      "test": null,
      "title": "Between cycles, the task goes straight to done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path commits its edit while a cycle of the change is open"
       },
       {
        "kw": "THEN",
        "text": "the cycle's record holds the task as done, outside the cycle's batch, and the cycle's counts include it"
       },
       {
        "kw": "AND",
        "text": "the edit was made while no run after a merge was running in the code root"
       }
      ],
      "test": null,
      "title": "While a cycle is open, the inline task is recorded in it as done"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path commits"
       },
       {
        "kw": "THEN",
        "text": "the code is committed in the code root and the task file in the home"
       },
       {
        "kw": "AND",
        "text": "when the home is the code root, one working tree, both are in one commit; otherwise each is a commit of its own"
       }
      ],
      "test": null,
      "title": "The code and the task file are committed together"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path runs its verification"
       },
       {
        "kw": "THEN",
        "text": "the run's output is kept in a file in the change's directory, named for the task"
       },
       {
        "kw": "AND",
        "text": "the commit's message names the command that was run"
       }
      ],
      "test": null,
      "title": "The verification run's output is kept beside the task"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path commits"
       },
       {
        "kw": "THEN",
        "text": "its task file holds one line saying what was checked for what the edit exposed and what was found, \"nothing\" included"
       }
      ],
      "test": null,
      "title": "What the edit exposed is answered in the task file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path commits its edit"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives one line of at most 200 characters, =inline <task>: done <sha>; <task file>=, the path relative to the home"
       }
      ],
      "test": null,
      "title": "A finished inline path hands back done with its commit"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path commits its edit"
       },
       {
        "kw": "THEN",
        "text": "the commit's subject is =Inline task <task>: <description>="
       }
      ],
      "test": null,
      "title": "The inline path's commit has a subject of its own"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path's task came from a discovery"
       },
       {
        "kw": "THEN",
        "text": "its task file still names what it was discovered from, by whom, and the discovery's class"
       }
      ],
      "test": null,
      "title": "A task from a discovery keeps where it came from"
     }
    ],
    "text": "The inline path SHALL make its edit in the code root, with no task worktree, and SHALL spawn no Implementor and no Reviewer: its phase agent reviews its own edit. The task SHALL go from open to done with no review step between, and the code and the task file SHALL be committed together.",
    "title": "A small change is made by one phase agent, without a worktree, an Implementor or a Reviewer"
   },
   {
    "goal": "G13 (O-NS-11's; O-IL-1's link to it is the outcome list's reading)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user asks, between cycles, for a one-line edit, a one-file doc fix, the rename of one call site, the regeneration of one artifact, one config value, or a typo in a comment or docstring"
       },
       {
        "kw": "THEN",
        "text": "it is made on the inline path"
       },
       {
        "kw": "AND",
        "text": "no Implementor or Reviewer is spawned for it"
       }
      ],
      "test": null,
      "title": "A one-file edit the user asks for between cycles takes the inline path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an edit would touch two or more files"
       },
       {
        "kw": "THEN",
        "text": "the inline path makes no edit for it"
       },
       {
        "kw": "AND",
        "text": "it is made as a batch task of a later cycle"
       }
      ],
      "test": null,
      "title": "An edit of two or more files is not taken on the inline path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an edit would touch a seam's code"
       },
       {
        "kw": "THEN",
        "text": "the inline path makes no edit for it"
       },
       {
        "kw": "AND",
        "text": "it is made as a batch task of a later cycle, reviewed by a Reviewer"
       }
      ],
      "test": null,
      "title": "An edit of a seam's code is not taken on the inline path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "verifying an edit needs tests run with state set up"
       },
       {
        "kw": "THEN",
        "text": "the inline path makes no edit for it"
       },
       {
        "kw": "AND",
        "text": "it is made as a batch task of a later cycle"
       }
      ],
      "test": "none: the evals' fixture project has no test that needs state set up",
      "title": "An edit whose verification needs tests with state set up is not taken on the inline path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the output of the run that verifies an edit cannot be predicted"
       },
       {
        "kw": "THEN",
        "text": "the inline path makes no edit for it"
       },
       {
        "kw": "AND",
        "text": "it is made as a batch task of a later cycle"
       }
      ],
      "test": null,
      "title": "An edit whose verification output is not predictable is not taken on the inline path"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a task is in a cycle's batch, however small"
       },
       {
        "kw": "THEN",
        "text": "it is implemented by an Implementor and reviewed by a Reviewer in that cycle's execute"
       },
       {
        "kw": "AND",
        "text": "the inline path does not make it"
       },
       {
        "kw": "AND",
        "text": "asked for it, the inline path writes no task file and no note of its own: its bailout line names the batch task's file"
       }
      ],
      "test": null,
      "title": "A task already in a batch is not taken on the inline path"
     }
    ],
    "text": "The inline path SHALL take only an edit the user asks for between cycles that its triage keeps to one file: no seam's code, no tests that need state set up, and a verification run whose output is predictable. Any other edit SHALL be made as a batch task of a later cycle, and so SHALL a task already in a batch, however small. In doubt, the edit SHALL take the batch.",
    "title": "The triage keeps the inline path to one-file edits asked for between cycles"
   },
   {
    "goal": "G13 (O-NS-11's; O-IL-1's link to it is the outcome list's reading)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the triage refuses the request before any edit is made"
       },
       {
        "kw": "THEN",
        "text": "the task file is left open for the next plan, with the triage's reason in it"
       },
       {
        "kw": "AND",
        "text": "the inline path hands back =inline <task>: bailed out, <why>; <task file>="
       }
      ],
      "test": null,
      "title": "A request the triage refuses is a bailout with nothing to revert"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a request the triage refuses also puts the change's design or a spec in question"
       },
       {
        "kw": "THEN",
        "text": "the inline path bails out and files no ask"
       },
       {
        "kw": "AND",
        "text": "the reason in the task file names the question, for the next plan"
       }
      ],
      "test": null,
      "title": "A request the triage refuses bails out even when it puts the design in question"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit turns out to touch a second file"
       },
       {
        "kw": "THEN",
        "text": "the inline path bails out"
       }
      ],
      "test": null,
      "title": "An edit that reaches a second file bails out"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit turns out to touch a seam's code"
       },
       {
        "kw": "THEN",
        "text": "the inline path bails out"
       }
      ],
      "test": null,
      "title": "An edit that reaches a seam's code bails out"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit would change a seam row of the change's design, its statement or its owning symbols"
       },
       {
        "kw": "THEN",
        "text": "the inline path bails out, and the row is left as it is"
       }
      ],
      "test": null,
      "title": "An edit that changes a seam row bails out"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the run that verifies the edit fails in a way that is not trivially fixable"
       },
       {
        "kw": "THEN",
        "text": "the inline path bails out"
       }
      ],
      "test": null,
      "title": "A verification run that fails and is not trivially fixable bails out"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "making the edit needs more than a paragraph of context read again"
       },
       {
        "kw": "THEN",
        "text": "the inline path bails out"
       }
      ],
      "test": "none: how much context an edit needs is the agent's own reading, so no fixture can make the WHEN happen",
      "title": "An edit that needs more than a paragraph of context bails out"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path bails out before its commit"
       },
       {
        "kw": "THEN",
        "text": "the code root's working tree is as it was before the edit"
       },
       {
        "kw": "AND",
        "text": "the inline path has made no commit in the code root"
       }
      ],
      "test": null,
      "title": "A bailout leaves the code root as it was"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path bails out after its commit was made"
       },
       {
        "kw": "THEN",
        "text": "the commit is undone by a revert commit in the code root"
       },
       {
        "kw": "AND",
        "text": "the code root's history still holds the commit it reverts"
       }
      ],
      "test": "none: the inline path commits last, so a bailout after its commit needs an agent that commits out of order",
      "title": "A bailout after a commit reverts it with a commit of its own"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path bails out"
       },
       {
        "kw": "THEN",
        "text": "the task is open again, ready for the next plan"
       },
       {
        "kw": "AND",
        "text": "its task file holds a note that the inline path bailed out, saying what was found and where its evidence is"
       }
      ],
      "test": null,
      "title": "A bailed-out task is open again with the reason in its task file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path bails out"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher receives one line of at most 200 characters, =inline <task>: bailed out, <why>; <task file>="
       }
      ],
      "test": null,
      "title": "A bailout hands back one line naming the task and why"
     }
    ],
    "text": "When the inline path's edit turns out larger than its triage allowed, the inline path SHALL bail out: leave the code root as it was before the edit, keeping history when a commit was already made; put the task back, open, with the reason and its evidence in its task file; and hand the dispatcher one line naming the task and why.",
    "title": "An edit that grows past its triage is bailed out and put back as a task"
   },
   {
    "goal": "none stated (=north-star.org= * Disposition states it; no goal does)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit shows a defect that the change's design already plans to fix, by a Decision, a seam row or a pending test"
       },
       {
        "kw": "THEN",
        "text": "it is not routed as exposed and does not make the inline path bail out"
       },
       {
        "kw": "AND",
        "text": "the task file's line on what the edit exposed names it as planned, with where the design plans it"
       }
      ],
      "test": null,
      "title": "A defect the change's design already plans is not one the edit exposed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit exposes an untrue sentence in the file being edited"
       },
       {
        "kw": "THEN",
        "text": "the sentence is corrected in the same edit, in prose only"
       }
      ],
      "test": null,
      "title": "An untrue sentence in the edited file is corrected in the same edit"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit exposes an untrue sentence in another file"
       },
       {
        "kw": "THEN",
        "text": "the sentence is corrected, in prose only, in a commit of its own whose subject is =Text correction: <what>="
       },
       {
        "kw": "AND",
        "text": "that second file does not make the inline path bail out"
       }
      ],
      "test": null,
      "title": "An untrue sentence in another file is corrected in a commit of its own"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit exposes a defect in the code or text beside it that is off the change's main path"
       },
       {
        "kw": "THEN",
        "text": "it is one entry on the change's hardening list that refers to the task"
       },
       {
        "kw": "AND",
        "text": "the inline path does not bail out for it"
       }
      ],
      "test": null,
      "title": "A defect off the main path goes on the hardening list"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit exposes a defect on the change's main path"
       },
       {
        "kw": "THEN",
        "text": "the inline path bails out"
       },
       {
        "kw": "AND",
        "text": "the note in the task file names the defect and its evidence"
       }
      ],
      "test": null,
      "title": "A defect on the main path bails out, naming the defect"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit shows a need that would arise only after some future change"
       },
       {
        "kw": "THEN",
        "text": "the task file's Observations gain one line marked as a thought"
       }
      ],
      "test": null,
      "title": "A speculative need is one thought in the task file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit exposes a defect or a speculative need"
       },
       {
        "kw": "THEN",
        "text": "the inline path writes no new task file and no follow-up candidate"
       }
      ],
      "test": null,
      "title": "The inline path writes no task for what it finds"
     }
    ],
    "text": "The inline path SHALL route each defect its edit exposes in the code or text beside it: an untrue sentence SHALL be corrected, a defect off the change's main path SHALL go on the change's hardening list, a defect on the main path SHALL make the inline path bail out, and a speculative need SHALL be one thought in the task file. It SHALL write no new task for any of them.",
    "title": "What the edit exposed is routed, and the inline path writes no new task"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the edit puts the change's design in question"
       },
       {
        "kw": "THEN",
        "text": "a =decision= ask is filed in the change's inbox"
       },
       {
        "kw": "AND",
        "text": "the ask blocks the inline path's commit"
       },
       {
        "kw": "AND",
        "text": "the phase agent hands back that it stopped, naming the ask, and does not put the question to the user"
       }
      ],
      "test": null,
      "title": "A design in question is a decision ask that holds the commit"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the inline path stops on a =decision= ask"
       },
       {
        "kw": "THEN",
        "text": "its line is =inline <task>: stopped, ask <id> blocks commit; <ask file>=, the path relative to the home"
       }
      ],
      "test": null,
      "title": "Stopped on an ask, the inline path's line names it"
     }
    ],
    "text": "When the edit puts the change's design in question, so that only the user's choice settles it, the inline path SHALL file a =decision= ask, SHALL not commit its edit until the ask is answered, and SHALL hand back that it stopped, naming the ask; it SHALL never put the question to the user itself.",
    "title": "A design in question stops the inline path on a decision ask"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); at the hand-off review 2026-11-13: G7, G8 and G9 applied; the task file's source, the open cycle, the commit subject and the triage's refusal filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-14 after acceptance (one commit when the home is the code root; a batch task the triage refuses; a refused request that also questions the design; three scenarios marked not test-worthy), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning); amended 2026-11-14 after acceptance (a defect the design already plans is not exposed), from the user's decisions on round 2 of the evals (=research/skill-evals/round-2/plan.org= § Log › Decisions needed)",
  "title": "inline-path"
 },
 "summary": {
  "cases": 11,
  "cited": 34,
  "fail": 15,
  "pass": 19,
  "scenarios": 37,
  "title": "inline-path",
  "tree": "skill-evals",
  "validation": 11
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
