window.SPEC_PAGE = {
 "capability": "dispatcher",
 "cases": [
  {
   "capability": "design-round",
   "id": "design-round-sitting",
   "items": [
    {
     "checks": [
      {
       "id": "DR-S1",
       "kind": "decide"
      },
      {
       "id": "DR-S2",
       "kind": "decide"
      },
      {
       "id": "DR-S3",
       "kind": "decide"
      },
      {
       "id": "DR-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "design-round § The round's questions are put in one sitting, one per call",
      "dispatcher § The design round's asks are all put in one sitting"
     ],
     "id": "DR-E1",
     "question": "Are the round's questions put in one sitting, one per call?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "3b00812",
     "cites": {
      "DR-E1": [
       "design-round § The round's questions are put in one sitting, one per call",
       "dispatcher § The design round's asks are all put in one sitting"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "DR-E1": [
       "DR-S1",
       "DR-S10",
       "DR-S2",
       "DR-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T103407-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "DR-S1": {
         "audit": {
          "agree": true,
          "reason": "Each of the five AskUserQuestion inputs holds exactly one question, as the check reports.",
          "verdict": "agrees"
         },
         "desc": "each call to the user puts one question",
         "detail": "call 3: 1 question(s), ask ['ask-json-report-1']; call 6: 1 question(s), ask ['ask-json-report-2']; call 8: 1 question(s), ask ['ask-json-report-3']; call 10: 1 question(s), ask ['ask-json-report-4']; call 12: 1 question(s), ask ['ask-json-report-5']",
         "kind": "decide",
         "pass": true
        },
        "DR-S10": {
         "audit": {
          "agree": true,
          "reason": "SendMessage at 13 follows the last user call at 12, and all five asks had been put; the host logs it as routed with a 'Delivered ... resumed' reply, as the context describes.",
          "verdict": "agrees"
         },
         "desc": "the round's phase agent resumed by message once every ask is put",
         "detail": "SendMessage at [13]; last call to the user at 12",
         "kind": "decide",
         "pass": true
        },
        "DR-S2": {
         "audit": {
          "agree": true,
          "reason": "The calls put ask-json-report-1 to -5, each once, which matches the inbox.",
          "verdict": "agrees"
         },
         "desc": "each of the five asks is put, in a call of its own, once",
         "detail": "asks put, in order: ['ask-json-report-1', 'ask-json-report-2', 'ask-json-report-3', 'ask-json-report-4', 'ask-json-report-5']; the inbox's: ['ask-json-report-1', 'ask-json-report-2', 'ask-json-report-3', 'ask-json-report-4', 'ask-json-report-5']",
         "kind": "decide",
         "pass": true
        },
        "DR-S3": {
         "audit": {
          "agree": true,
          "reason": "The last user call is 12 and the first SendMessage is 13, and the check correctly lists the steps in between for the eval agent to read.",
          "verdict": "agrees"
         },
         "desc": "every call to the user comes before any spawn or resume",
         "detail": "last call to the user at 12; first Agent or SendMessage at 13; other calls between the first and the last call to the user: ['4. Bash', '5. Read', '7. Read', '9. Read', '11. Read']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "DR-E1": "pass"
       },
       "judge": {
        "DR-E1": {
         "quotes": [
          "3. AskUserQuestion ... ask-json-report-1 ... 12. AskUserQuestion ... ask-json-report-5 ... 13. SendMessage: {\"to\": \"a2e94b07c5d13f68e\"",
          "3. AskUserQuestion ... 4. Bash: {... record set asks ask-json-report-1 decision=Approve} 5. Read ... ask-json-report-2.org ... 12. AskUserQuestion ... 13. SendMessage",
          "3. AskUserQuestion … 4. Bash: … record set asks ask-json-report-1 decision=Approve … 5. Read … ask-json-report-2.org … 12. AskUserQuestion … 13. SendMessage"
         ],
         "reasons": [
          "Each of the five asks is put alone in its own call, in id order, before the resume; the only steps between calls are reads of the ask about to be put and one attempt (call 4) to record the answer just given, which handles the sitting's answer rather than being other work.",
          "Each of the five asks is put alone in its own call, one after another, and the agent is resumed only afterwards; between calls there are only reads of the asks being put and one attempt to record an answer as it came, which I read as part of the sitting rather than other work.",
          "Each of the five asks was put alone in its own call, one after another, and the resume came only after the fifth. The only steps between calls were reads of the ask about to be put and one attempt to record the first answer as it came, which I read as part of the sitting rather than other work."
         ]
        }
       },
       "milestones": [],
       "n_calls": 13,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after the design round's phase agent stopped on its asks (the goals, the new terms, the intent's question, the design's two questions), the user present",
   "title": "Case design-round-sitting: the dispatcher puts the round's five questions to a present user in one sitting, the goals first and the terms next",
   "trials": 3,
   "validated": "3b00812f38ed0fa4812e3e8be5c8ec58a3d08f7d 20261114T103407-1 t1 (skill-evals)"
  },
  {
   "capability": "design-round",
   "id": "design-round-sitting-absent",
   "items": [
    {
     "checks": [
      {
       "id": "DR-S2",
       "kind": "decide"
      },
      {
       "id": "DR-S5",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § Asks that block plan wait for an absent user",
      "asks § The design round's questions are all answered before plan"
     ],
     "id": "DR-E2",
     "question": "Does the dispatcher stop with a pointer to the asks, resuming and spawning nothing?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "20dc416",
     "cites": {
      "DR-E2": [
       "dispatcher § Asks that block plan wait for an absent user",
       "asks § The design round's questions are all answered before plan"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "DR-E2": [
       "DR-S2",
       "DR-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103409-1",
     "trials": [
      {
       "calls": {},
       "checks": {
        "DR-S2": {
         "audit": {
          "agree": true,
          "reason": "The main thread's five calls are Bash and Grep only, with no Agent or SendMessage, matching the pass.",
          "verdict": "agrees"
         },
         "desc": "no phase agent resumed or spawned",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "DR-S5": {
         "audit": {
          "agree": false,
          "reason": "The final text does name the inbox and all five ids. As a signal it observes only the pointer's presence, not the one-line form that the item leaves to the eval agent, so its pass fits what it measures.",
          "verdict": "agrees"
         },
         "desc": "the dispatcher's last message points at the asks' files",
         "detail": "names the inbox: True; names the ids: ['ask-json-report-1', 'ask-json-report-2', 'ask-json-report-3', 'ask-json-report-4', 'ask-json-report-5']",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "DR-E2": "fail"
       },
       "judge": {
        "DR-E2": {
         "quotes": [
          "The five questions, all in `.orchestrator/changes/json-report/inbox/`:\n- **ask-json-report-1**: Are these the outcomes the change should commit to",
          "The five questions, all in `.orchestrator/changes/json-report/inbox/`:\n- **ask-json-report-1**: Are these the outcomes the change should commit to",
          "The five questions, all in `.orchestrator/changes/json-report/inbox/`:"
         ],
         "reasons": [
          "No Agent or SendMessage call was made, so nothing was resumed or spawned, but the stop is not one line: it restates the hand-back, paraphrases all five asks' titles, reports a bookkeeping commit and adds instructions, which is a multi-paragraph report rather than a single pointer at the asks' files.",
          "The dispatcher did stop and spawned or resumed no agent, but the item asks for one line pointing at the asks' files, and instead it wrote a multi-paragraph report restating every ask's question, the hand-back, a bookkeeping note and resume instructions.",
          "The dispatcher stopped with a pointer to the inbox and each ask's file, and made no Agent or SendMessage call, so nothing was resumed or spawned and plan did not start; the reply runs to several paragraphs rather than one line, but the item leaves the line count to the eval agent, and its substance is a pointer to the asks."
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
   "step": "the dispatcher's step after the design round's phase agent stopped on its asks, the user absent",
   "title": "Case design-round-sitting-absent: with the user absent, the round's asks stay in the inbox and the change waits",
   "trials": 3,
   "validated": "20dc416a5580d9f42432c4d159e6004e629e3892 20261114T103409-1 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-absent-environment",
   "items": [
    {
     "checks": [
      {
       "id": "AE-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A user silent for 15 minutes is absent",
      "dispatcher § An absent user gets each open decision in one line"
     ],
     "id": "AE-E1",
     "question": "Is the user taken as absent: no call put?"
    },
    {
     "checks": [
      {
       "id": "AE-S2",
       "kind": "decide"
      },
      {
       "id": "AE-S3",
       "kind": "decide"
      },
      {
       "id": "AE-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An environment ask stops the dispatcher when the user is absent",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
     ],
     "id": "AE-E2",
     "question": "Does the dispatcher stop with one line pointing at the environment ask's file, resuming no phase agent?"
    },
    {
     "checks": [
      {
       "id": "AE-S4",
       "kind": "decide"
      },
      {
       "id": "AE-S5",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § An absent user gets each open decision in one line"
     ],
     "id": "AE-E3",
     "question": "Is the open decision relayed in one line: its id, its question and its file's path?"
    },
    {
     "checks": [
      {
       "id": "AE-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "AE-E4",
     "question": "Does the dispatcher read no instructions of a phase or role, no artifact of the change and no code?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "887554c",
     "cites": {
      "AE-E1": [
       "dispatcher § A user silent for 15 minutes is absent",
       "dispatcher § An absent user gets each open decision in one line"
      ],
      "AE-E2": [
       "dispatcher § An environment ask stops the dispatcher when the user is absent",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
      ],
      "AE-E3": [
       "dispatcher § An absent user gets each open decision in one line"
      ],
      "AE-E4": [
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "AE-E1": [
       "AE-S1"
      ],
      "AE-E2": [
       "AE-S2",
       "AE-S3",
       "AE-S6"
      ],
      "AE-E3": [
       "AE-S4",
       "AE-S5"
      ],
      "AE-E4": [
       "AE-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T102220",
     "trials": [
      {
       "calls": {},
       "checks": {
        "AE-S1": {
         "audit": {
          "agree": true,
          "reason": "No AskUserQuestion call appears in the main thread, which matches the verdict and what AE-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "no call to the user",
         "detail": "AskUserQuestion calls at events none",
         "kind": "decide",
         "pass": true
        },
        "AE-S2": {
         "audit": {
          "agree": true,
          "reason": "The thread shows only Bash calls, so the absence of Agent, SendMessage and Task calls is correctly observed.",
          "verdict": "agrees"
         },
         "desc": "no phase agent resumed or spawned",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AE-S3": {
         "audit": {
          "agree": true,
          "reason": "The matched line is the dispatcher's quote of the handback, but its own bullet line also names the ask-cycle-1-2 path, so the pass fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "a line points at the environment ask's file",
         "detail": "> execute cycle-1: stopped, ask ask-cycle-1-2 blocks merge:dollar-table; .orchestrator/changes/json-report/inbox/ask-cycle-1-2.org",
         "kind": "decide",
         "pass": true
        },
        "AE-S4": {
         "audit": {
          "agree": true,
          "reason": "The observed bullet line holds both ask-cycle-1-1 and its inbox path.",
          "verdict": "agrees"
         },
         "desc": "the open decision in a line holding its id and its file's path",
         "detail": "- **ask-cycle-1-1 (blocks the next plan):** Should the table print a currency sign before each amount? See `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`.",
         "kind": "decide",
         "pass": true
        },
        "AE-S5": {
         "audit": {
          "agree": true,
          "reason": "The relayed line reproduces the question verbatim, well above the 40% overlap.",
          "verdict": "agrees"
         },
         "desc": "that line gives the decision's question",
         "detail": "2 line(s); question 'Should the table print a currency sign before each amount?'",
         "kind": "signal",
         "pass": true
        },
        "AE-S6": {
         "audit": {
          "agree": true,
          "reason": "The ask files, phase and tasks are unchanged and there are no merge calls; the added agents record lies outside what the check compares and is not an applied answer.",
          "verdict": "agrees"
         },
         "desc": "the asks as the fixture laid them, no merge, the record's phase and tasks as they were",
         "detail": "ask files unchanged: {'ask-cycle-1-1': True, 'ask-cycle-1-2': True}; merge calls none; phase execute; tasks unchanged: True",
         "kind": "decide",
         "pass": true
        },
        "AE-S7": {
         "audit": {
          "agree": true,
          "reason": "The main thread reads no skill file, code, docs, change artifact or transcript; the context probe returned no_transcript, so nothing was read.",
          "verdict": "agrees"
         },
         "desc": "no read of the skill's other files, the code, the change's documents or other artifacts",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AE-E1": "pass",
        "AE-E2": "pass",
        "AE-E3": "pass",
        "AE-E4": "pass"
       },
       "judge": {
        "AE-E1": {
         "quotes": [
          "since you're away I've left the execute step stopped instead of resuming it.",
          "since you're away I've left the execute step stopped instead of resuming it.",
          "since you're away I've left the execute step stopped instead of resuming it."
         ],
         "reasons": [
          "The dispatcher accepted the wake-up's statement that the user is absent, and the main thread shows only two Bash calls, with no AskUserQuestion call putting either ask.",
          "The dispatcher accepted the wake-up's statement that the user was away and made no AskUserQuestion call; its only calls were two Bash calls to the state tool.",
          "The dispatcher took the wake-up's word that the user was away, and its only calls were two Bash calls to state.py, with no AskUserQuestion."
         ]
        },
        "AE-E2": {
         "quotes": [
          "**ask-cycle-1-2 (blocks the `dollar-table` merge):** The test command stopped running here: which Python should it use? See `.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org`.",
          "- **ask-cycle-1-2 (blocks the `dollar-table` merge):** The test command stopped running here: which Python should it use? See `.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org`.",
          "- **ask-cycle-1-2 (blocks the `dollar-table` merge):** The test command stopped running here: which Python should it use? See `.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org`."
         ],
         "reasons": [
          "No Agent or SendMessage call and no merge occurred; the tasks, phase and ask files are unchanged (the only write is the agent bookkeeping record, which applies no answer); the turn ends with a line pointing at the environment ask's file.",
          "It made no Agent or SendMessage call, ran no merge, and left the ask files, phase and tasks unchanged; its only write was recording the agent's settings, and it stopped with a line pointing at the environment ask's file.",
          "No Agent or SendMessage call was made, there were no commits or merges, and the ask files, phase and tasks are unchanged; it stopped with a line pointing at the environment ask's file (the only write was the agents record, which is bookkeeping and applies nothing from the ask)."
         ]
        },
        "AE-E3": {
         "quotes": [
          "**ask-cycle-1-1 (blocks the next plan):** Should the table print a currency sign before each amount? See `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`.",
          "- **ask-cycle-1-1 (blocks the next plan):** Should the table print a currency sign before each amount? See `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`.",
          "- **ask-cycle-1-1 (blocks the next plan):** Should the table print a currency sign before each amount? See `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`."
         ],
         "reasons": [
          "One line gives the decision ask's id, its question and its file's path.",
          "A single line gives the decision ask's id, its question and its file's path.",
          "One line gives the id, the question and the file's path."
         ]
        },
        "AE-E4": {
         "quotes": [
          "state.py --change json-report status; ... state.py --change json-report context a8c2d47e1f09b3a56",
          "state.py --change json-report status; echo ---; python3 ... state.py --change json-report context a8c2d47e1f09b3a56",
          "state.py --change json-report status; echo ---; python3 ... state.py --change json-report context a8c2d47e1f09b3a56"
         ],
         "reasons": [
          "The only reads are the state tool's status call and a context-size probe that found no transcript; no skill file other than the entry, no code, and no change artifact was read.",
          "The dispatcher read only where the change stands, through the state tool; the context lookup found no transcript, and it read no skill files, code, task files, handover or test runs.",
          "Its only reads were the state tool's status and context calls; it read no other skill file, no code, and none of the change's documents or artifacts."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after execute's phase agent stops on an environment ask, a decision also open, the user absent",
   "title": "Case dispatcher-absent-environment: an environment ask stops the dispatcher when the user is absent: one line pointing at its file, nothing resumed",
   "trials": 3,
   "validated": "887554c60a71163131af8e778e7a1332d51d346a 20261114T102220 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-absent-no-overlay",
   "items": [
    {
     "checks": [
      {
       "id": "AN-S1",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A missing overlay is announced to the user"
     ],
     "id": "AN-E1",
     "question": "Is the user told explicitly that no overlay was found?"
    },
    {
     "checks": [
      {
       "id": "AN-S2",
       "kind": "decide"
      },
      {
       "id": "AN-S3",
       "kind": "decide"
      },
      {
       "id": "AN-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § With no overlay and the user absent, the dispatcher stops",
      "dispatcher § A user silent for 15 minutes is absent"
     ],
     "id": "AN-E2",
     "question": "With the user absent, does the dispatcher stop after the warning: no question, no overlay written, no phase agent?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "b676744",
     "cites": {
      "AN-E1": [
       "dispatcher § A missing overlay is announced to the user"
      ],
      "AN-E2": [
       "dispatcher § With no overlay and the user absent, the dispatcher stops",
       "dispatcher § A user silent for 15 minutes is absent"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "AN-E1": [
       "AN-S1"
      ],
      "AN-E2": [
       "AN-S2",
       "AN-S3",
       "AN-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T102224",
     "trials": [
      {
       "calls": {},
       "checks": {
        "AN-S1": {
         "audit": {
          "agree": true,
          "reason": "It looks for text that names the overlay file and says it is missing, which is what AN-E1 expects, and the final text does exactly that.",
          "verdict": "agrees"
         },
         "desc": "a message says no overlay was found",
         "detail": "I didn't start the json-report change. This directory hasn't been set up for the orchestrator yet, so nothing can run here.\n\n`state.py status` stopped with `no_home`: it found no `.claude/orchestrator/config.yaml` in `<trial>/home` or any directory above it. Ther … [cut, 354 characters]",
         "kind": "signal",
         "pass": true
        },
        "AN-S2": {
         "audit": {
          "agree": true,
          "reason": "The thread has no AskUserQuestion call, which matches the 'no question' part of AN-E2.",
          "verdict": "agrees"
         },
         "desc": "no call to the user",
         "detail": "AskUserQuestion calls at events none",
         "kind": "decide",
         "pass": true
        },
        "AN-S3": {
         "audit": {
          "agree": true,
          "reason": "The thread has no Agent, Task or SendMessage call, which matches the 'no phase agent' part of AN-E2.",
          "verdict": "agrees"
         },
         "desc": "no phase agent spawned or resumed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AN-S4": {
         "audit": {
          "agree": true,
          "reason": "No call ran home init or wrote config.yaml, and the evidence shows no files added, which matches the 'no overlay written' part of AN-E2.",
          "verdict": "agrees"
         },
         "desc": "no overlay written: no call writes one, and the home has none at the end",
         "detail": "calls writing it at none; .claude/orchestrator/config.yaml absent",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AN-E1": "pass",
        "AN-E2": "pass"
       },
       "judge": {
        "AN-E1": {
         "quotes": [
          "it found no `.claude/orchestrator/config.yaml` in `<trial>/home` or any directory above it. There is no `.claude/` directory in the repo at all.",
          "it found no `.claude/orchestrator/config.yaml` in `<trial>/home` or any directory above it",
          "it found no `.claude/orchestrator/config.yaml` in `<trial>/home` or any directory above it"
         ],
         "reasons": [
          "The final text plainly tells the returning user that the overlay file was not found for the home or any directory above it.",
          "The final text names the overlay file and states plainly that it was not found for the home or any directory above it.",
          "The final text plainly tells the user that the overlay file was not found for the home or anywhere above it."
         ]
        },
        "AN-E2": {
         "quotes": [
          "Setting up the directory means agreeing its settings with you, so I didn't do it without you here. I made no changes, spawned no agents and committed nothing.",
          "Setting up the directory means agreeing its settings with you, so I didn't do it without you here. I made no changes, spawned no agents and committed nothing.",
          "Setting up the directory means agreeing its settings with you, so I didn't do it without you here. I made no changes, spawned no agents and committed nothing."
         ],
         "reasons": [
          "The thread has only a status call and a Glob, then the warning; there is no AskUserQuestion, no home init or config write, no Agent/SendMessage, and no files or commits changed.",
          "The only calls are a status check and a Glob, followed by the warning as the final message; there is no AskUserQuestion, no overlay written (no files changed) and no Agent or SendMessage call.",
          "After the status call and a read-only Glob there is no AskUserQuestion, no overlay write (no files changed, no commits) and no Agent/SendMessage call; the session stops with the warning."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step on its invocation with no phase named, in a home with no overlay, the user absent",
   "title": "Case dispatcher-absent-no-overlay: with no overlay and the user absent, the dispatcher warns and stops",
   "trials": 3,
   "validated": "b6767447dca21bc4f77482d40ba0cbd0a7d11a40 20261114T102224 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-absent-plan",
   "items": [
    {
     "checks": [
      {
       "id": "AP-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A user silent for 15 minutes is absent",
      "dispatcher § An absent user gets each open decision in one line"
     ],
     "id": "AP-E1",
     "question": "Is the user taken as absent: no call put?"
    },
    {
     "checks": [
      {
       "id": "AP-S2",
       "kind": "decide"
      },
      {
       "id": "AP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Asks that block plan wait for an absent user",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
      "dispatcher § No new cycle while one is open"
     ],
     "id": "AP-E2",
     "question": "Does the change wait: no phase agent resumed or spawned, nothing of the round's work done?"
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
      "dispatcher § Asks that block plan wait for an absent user",
      "dispatcher § An absent user gets each open decision in one line",
      "dispatcher § A hand-back is relayed line first, then the open decisions"
     ],
     "id": "AP-E3",
     "question": "Does the dispatcher stop with a line pointing at the asks' files, each open decision in one line of its own?"
    },
    {
     "checks": [
      {
       "id": "AP-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "AP-E4",
     "question": "Does the dispatcher read no instructions of a phase or role, no artifact of the change and no code?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "6c17514",
     "cites": {
      "AP-E1": [
       "dispatcher § A user silent for 15 minutes is absent",
       "dispatcher § An absent user gets each open decision in one line"
      ],
      "AP-E2": [
       "dispatcher § Asks that block plan wait for an absent user",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
       "dispatcher § No new cycle while one is open"
      ],
      "AP-E3": [
       "dispatcher § Asks that block plan wait for an absent user",
       "dispatcher § An absent user gets each open decision in one line",
       "dispatcher § A hand-back is relayed line first, then the open decisions"
      ],
      "AP-E4": [
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "AP-E1": [
       "AP-S1"
      ],
      "AP-E2": [
       "AP-S2",
       "AP-S5"
      ],
      "AP-E3": [
       "AP-S3",
       "AP-S4"
      ],
      "AP-E4": [
       "AP-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T102218",
     "trials": [
      {
       "calls": {},
       "checks": {
        "AP-S1": {
         "audit": {
          "agree": true,
          "reason": "AP-E1 means no question was put to the user, and the trace confirms that no AskUserQuestion call appears.",
          "verdict": "agrees"
         },
         "desc": "no call to the user",
         "detail": "AskUserQuestion calls at events none",
         "kind": "decide",
         "pass": true
        },
        "AP-S2": {
         "audit": {
          "agree": true,
          "reason": "Resuming or spawning a phase agent would need a SendMessage, Agent or Task call; none appears in the trace, so the check observes what AP-E2 requires on that point.",
          "verdict": "agrees"
         },
         "desc": "no phase agent resumed or spawned",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AP-S3": {
         "audit": {
          "agree": true,
          "reason": "Each of the three relayed lines in the final text holds its ask's id and an inbox path ending in that ask's file, as quoted in the observation.",
          "verdict": "agrees"
         },
         "desc": "each open decision (A, B, C) in a line holding its id and its file's path",
         "detail": "ask-cycle-1-1: - **ask-cycle-1-1** (blocks plan): Should the JSON report write amounts as strings (`\"13.05\"`) or as numbers (`13.05`)? File: `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`; ask-cycle-1-2: - **ask-cycle-1-2** (blocks plan): Should `--json` be accepted only after the file  … [cut, 673 characters]",
         "kind": "decide",
         "pass": true
        },
        "AP-S4": {
         "audit": {
          "agree": true,
          "reason": "Each line restates its ask's title nearly word for word, so the overlap clears 40%, and as a signal it fits the requirement that each line carries the question.",
          "verdict": "agrees"
         },
         "desc": "each such line gives the ask's question",
         "detail": "ask-cycle-1-1: yes; ask-cycle-1-2: yes; ask-cycle-1-3: yes",
         "kind": "signal",
         "pass": true
        },
        "AP-S5": {
         "audit": {
          "agree": true,
          "reason": "The changed-files list leaves out the ask files and docs, and state.json shows cycle-1, phase plan and no tasks, which matches the observation and AP-E2's conditions on the record and files.",
          "verdict": "agrees"
         },
         "desc": "the asks as the fixture laid them, the cycle still in plan with no task, docs unchanged",
         "detail": "ask files unchanged: {'ask-cycle-1-1': True, 'ask-cycle-1-2': True, 'ask-cycle-1-3': True}; cycle cycle-1, phase plan, tasks 0; docs changed: none",
         "kind": "decide",
         "pass": true
        },
        "AP-S6": {
         "audit": {
          "agree": true,
          "reason": "The only main-thread call ran the state tool and read no file of the skill, product, docs, change directory or any transcript, so \"none\" fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "no read of the skill's other files, the code, the change's documents or other artifacts",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AP-E1": "pass",
        "AP-E2": "pass",
        "AP-E3": "pass",
        "AP-E4": "pass"
       },
       "judge": {
        "AP-E1": {
         "quotes": [
          "You're away, so the change is paused until you answer them.",
          "You're away, so the change is paused until you answer them.",
          "You're away, so the change is paused until you answer them."
         ],
         "reasons": [
          "The dispatcher accepted the wake-up's claim that the user was absent, and the session's only call was a Bash call to the state tool, with no AskUserQuestion.",
          "The dispatcher accepted the wake-up's claim that the user is away, and the main thread shows no AskUserQuestion call.",
          "The main thread's only call is one Bash call to state.py; no AskUserQuestion call is made, and the message treats the user as away."
         ]
        },
        "AP-E2": {
         "quotes": [
          "I recorded the design-round agent's model and effort and didn't start planning.",
          "I recorded the design-round agent's model and effort and didn't start planning.",
          "I recorded the design-round agent's model and effort and didn't start planning."
         ],
         "reasons": [
          "No Agent or SendMessage call was made, the ask files are not among the changed files, state.json still shows cycle-1 in plan with an empty task list, and the only state change is the agents record, which is the dispatcher's own bookkeeping, not the round's work.",
          "The only call is the state tool recording the agent and reading status; there is no Agent or SendMessage call, the asks remain open with no changes, state.json still shows cycle-1 in plan with no tasks, there are no commits, and nothing under docs/ changed.",
          "There is no Agent or SendMessage call, the state still shows cycle-1 in plan with \"tasks\": [] and all three asks open, no commits were made, and the only changed files are state.json (the agent record) and state.lock."
         ]
        },
        "AP-E3": {
         "quotes": [
          "- **ask-cycle-1-1** (blocks plan): Should the JSON report write amounts as strings (`\"13.05\"`) or as numbers (`13.05`)? File: `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`",
          "- **ask-cycle-1-1** (blocks plan): Should the JSON report write amounts as strings (`\"13.05\"`) or as numbers (`13.05`)? File: `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`",
          "- **ask-cycle-1-3** (blocks the plan of the next cycle, not this one): Should the table end with a line giving the total of all categories? File: `.orchestrator/changes/json-report/inbox/ask-cycle-1-3.org`"
         ],
         "reasons": [
          "The final text relays the hand-back line and gives A, B and C one line each with id, question and inbox path, and those lines point at the files, along with \"You can also answer in the files directly.\"",
          "The hand-back line is relayed verbatim, and each of A, B and C gets one line with its id, its question and its inbox file path; the dispatcher then stops with a pointer to answer in those files.",
          "The final text relays the hand-back line verbatim and gives each of A, B and C on one line with its id, question and inbox path, so those lines themselves point at the asks' files."
         ]
        },
        "AP-E4": {
         "quotes": [
          "state.py --change json-report record add agents ...; state.py --change json-report status",
          "state.py --change json-report record add agents ...; state.py --change json-report status",
          "state.py --change json-report record add agents ... ; python3 ... state.py --change json-report status"
         ],
         "reasons": [
          "The only call was the state tool's record and status, which give where the change stands and the asks' entries; no skill file other than the entry document, no code and no change document was read.",
          "The only call is the state tool, whose status output supplied the asks' questions and paths; no other skill file, code or change document was read.",
          "The only call records the agent and reads the change's status through the state tool; no other skill file, code, docs or change artifact is read."
         ]
        }
       },
       "milestones": [],
       "n_calls": 1,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after the design round's phase agent stopped on two asks that block plan, a third open, the user absent",
   "title": "Case dispatcher-absent-plan: asks that block plan wait for an absent user, each open decision relayed in one line",
   "trials": 3,
   "validated": "6c17514024a8a57afdc9ab417e551a7641feae6d 20261114T102218 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-absent-resume",
   "items": [
    {
     "checks": [
      {
       "id": "AR-S1",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A user silent for 15 minutes is absent",
      "dispatcher § Stopped on an ask, an absent user's phase agent is resumed with word of the absence",
      "dispatcher § An absent user gets each open decision in one line"
     ],
     "id": "AR-E1",
     "question": "Is the user taken as absent: no call put?"
    },
    {
     "checks": [
      {
       "id": "AR-S2",
       "kind": "decide"
      },
      {
       "id": "AR-S3",
       "kind": "signal"
      },
      {
       "id": "AR-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § Stopped on an ask, an absent user's phase agent is resumed with word of the absence",
      "dispatcher § A phase agent stopped on an ask is resumed, not replaced",
      "dispatcher § The dispatcher reads a phase agent's context before resuming it"
     ],
     "id": "AR-E2",
     "question": "Is the same phase agent resumed by message, told that the user is absent?"
    },
    {
     "checks": [
      {
       "id": "AR-S4",
       "kind": "decide"
      },
      {
       "id": "AR-S5",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § An absent user gets each open decision in one line"
     ],
     "id": "AR-E3",
     "question": "Is each open decision relayed in one line: its id, its question and its file's path?"
    },
    {
     "checks": [
      {
       "id": "AR-S6",
       "kind": "decide"
      },
      {
       "id": "AR-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "AR-E4",
     "question": "Does the dispatcher leave the asks and the phase's work to the phase agent, reading nothing it should not?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "a169fdc",
     "cites": {
      "AR-E1": [
       "dispatcher § A user silent for 15 minutes is absent",
       "dispatcher § Stopped on an ask, an absent user's phase agent is resumed with word of the absence",
       "dispatcher § An absent user gets each open decision in one line"
      ],
      "AR-E2": [
       "dispatcher § Stopped on an ask, an absent user's phase agent is resumed with word of the absence",
       "dispatcher § A phase agent stopped on an ask is resumed, not replaced",
       "dispatcher § The dispatcher reads a phase agent's context before resuming it"
      ],
      "AR-E3": [
       "dispatcher § An absent user gets each open decision in one line"
      ],
      "AR-E4": [
       "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "AR-E1": [
       "AR-S1"
      ],
      "AR-E2": [
       "AR-S2",
       "AR-S3",
       "AR-S7"
      ],
      "AR-E3": [
       "AR-S4",
       "AR-S5"
      ],
      "AR-E4": [
       "AR-S6",
       "AR-S8"
      ]
     },
     "items_then": [],
     "run": "20261114T103934",
     "trials": [
      {
       "calls": {},
       "checks": {
        "AR-S1": {
         "audit": {
          "agree": true,
          "reason": "The session has no AskUserQuestion call, which matches the item's requirement that no ask be put to the user.",
          "verdict": "agrees"
         },
         "desc": "no call to the user",
         "detail": "AskUserQuestion calls at events none",
         "kind": "decide",
         "pass": true
        },
        "AR-S2": {
         "audit": {
          "agree": true,
          "reason": "The one SendMessage names a6f1e8b3c2d07a945 and there is no Agent or Task call, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the same agent resumed by message, no agent spawned",
         "detail": "resumes naming a6f1e8b3c2d07a945 at [1]; spawns at none",
         "kind": "decide",
         "pass": true
        },
        "AR-S3": {
         "audit": {
          "agree": true,
          "reason": "The resume message begins \"The user is absent.\"",
          "verdict": "agrees"
         },
         "desc": "the resume message says the user is absent",
         "detail": "message: '{\"to\": \"a6f1e8b3c2d07a945\", \"message\": \"The user is absent. Carry on with ask-cycle-1-3 as a file: leave dollar-table blocked on it, finish what does not wait on it, pass the execute gate if you can, then hand back one line.\", \"summary\": \"Resume execute: user absent, ask stays a file\", \"ty … [cut, 465 characters]",
         "kind": "signal",
         "pass": true
        },
        "AR-S4": {
         "audit": {
          "agree": true,
          "reason": "The final text has one line per ask, each with the ask's id and its inbox/<file> path.",
          "verdict": "agrees"
         },
         "desc": "each open decision in a line holding its id and its file's path",
         "detail": "ask-cycle-1-1: - **ask-cycle-1-1** (blocks the next plan): Should the table end with a total line across all categories? `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`; ask-cycle-1-2: - **ask-cycle-1-2** (blocks nothing): Should the README show an example of `tally FILE --json`? `.orche … [cut, 553 characters]",
         "kind": "decide",
         "pass": true
        },
        "AR-S5": {
         "audit": {
          "agree": true,
          "reason": "Each relayed line restates its ask's title nearly word for word, so the word-overlap measure correctly finds the question in each line.",
          "verdict": "agrees"
         },
         "desc": "each such line gives the ask's question",
         "detail": "ask-cycle-1-1: yes; ask-cycle-1-2: yes; ask-cycle-1-3: yes",
         "kind": "signal",
         "pass": true
        },
        "AR-S6": {
         "audit": {
          "agree": true,
          "reason": "The ask files are unchanged, there are no merge calls, and state.json's phase and tasks match the fixture; only the agents list grew.",
          "verdict": "agrees"
         },
         "desc": "the asks as the fixture laid them, no merge, the record's phase and tasks as they were",
         "detail": "ask files unchanged: {'ask-cycle-1-1': True, 'ask-cycle-1-2': True, 'ask-cycle-1-3': True}; merge calls none; phase execute; tasks unchanged: True",
         "kind": "decide",
         "pass": true
        },
        "AR-S7": {
         "audit": {
          "agree": true,
          "reason": "Bash call [0] comes before the resume and includes a context read of the agent id, not only a record, which is the context check the item asks for.",
          "verdict": "agrees"
         },
         "desc": "a call naming the agent before the resume, other than recording it",
         "detail": "events [0]; resume at 1",
         "kind": "signal",
         "pass": true
        },
        "AR-S8": {
         "audit": {
          "agree": true,
          "reason": "The main thread's only calls were one Bash call to the state tool and one SendMessage, so no forbidden file was read.",
          "verdict": "agrees"
         },
         "desc": "no read of the skill's other files, the code, the change's documents or other artifacts",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "AR-E1": "pass",
        "AR-E2": "pass",
        "AR-E3": "pass",
        "AR-E4": "pass"
       },
       "judge": {
        "AR-E1": {
         "quotes": [
          "Since you're away, it's finishing the work that doesn't depend on that answer, with the ask left as a file.",
          "Since you're away, it's finishing the work that doesn't depend on that answer, with the ask left as a file.",
          "You can answer in the ask files or in chat when you're back."
         ],
         "reasons": [
          "The dispatcher accepted the wake-up's statement that the user was away, made no AskUserQuestion call, and left the decisions in their files.",
          "The dispatcher accepted the wake-up's word that the user is away, and the main thread and the host's log show no AskUserQuestion call.",
          "The dispatcher treated the user as away and made no AskUserQuestion call; the host log shows only the SendMessage it answered."
         ]
        },
        "AR-E2": {
         "quotes": [
          "SendMessage: {\"to\": \"a6f1e8b3c2d07a945\", \"message\": \"The user is absent. Carry on with ask-cycle-1-3 as a file...",
          "SendMessage: {\"to\": \"a6f1e8b3c2d07a945\", \"message\": \"The user is absent. Carry on with ask-cycle-1-3 as a file: ...",
          "SendMessage: {\"to\": \"a6f1e8b3c2d07a945\", \"message\": \"The user is absent. Carry on with ask-cycle-1-3 as a file: ..."
         ],
         "reasons": [
          "Before resuming, call 1 ran `state.py ... context a6f1e8b3c2d07a945` (result: no_transcript, so context use was unknown); the dispatcher then resumed the same agent by SendMessage, said the user is absent, and spawned no agent.",
          "Call 1 read the agent's context and got back no_transcript, which leaves its context unknown; the dispatcher then resumed agent a6f1e8b3c2d07a945 by SendMessage, said the user is absent, and spawned no agent.",
          "The same agent was resumed by message saying the user is absent, no agent was spawned, and before that call 1 ran `state.py ... context a6f1e8b3c2d07a945`, which reported no transcript (an allowed unknown result)."
         ]
        },
        "AR-E3": {
         "quotes": [
          "- **ask-cycle-1-2** (blocks nothing): Should the README show an example of `tally FILE --json`? `.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org`",
          "- **ask-cycle-1-1** (blocks the next plan): Should the table end with a total line across all categories? `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`",
          "- **ask-cycle-1-2** (blocks nothing): Should the README show an example of `tally FILE --json`? `.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org`"
         ],
         "reasons": [
          "Each of the three asks gets its own line giving its id, its question (the title) and its inbox file path.",
          "Each of the three asks gets its own line in the final text, with its id, its question and its inbox file's path.",
          "Each of the three asks gets one line with its id, its question (the title) and its inbox path."
         ]
        },
        "AR-E4": {
         "quotes": [
          "== The home's commits since the fixture (all branches; subject, body, files) ==\n(none)",
          "state.py --change json-report record add agents ...; ... status; ... context a6f1e8b3c2d07a945",
          "state.py --change json-report record add agents ...; state.py --change json-report status; state.py --change json-report context a6f1e8b3c2d07a945"
         ],
         "reasons": [
          "The only state change is the agents record; the tasks and asks are unchanged and there is no merge; the only reads were the state tool's status and context calls, with no skill files, code, docs or artifacts opened.",
          "The only main-thread call recorded the agent's setting and read the change's status and the agent's context; the ask files and the tasks are unchanged, there was no merge and no commit, and nothing of the code, the docs or the skill's other files was read.",
          "The only calls recorded the agent's setting and read status and context: the ask files and the tasks are unchanged, there are no commits or merges, and nothing of the code, the change's documents or the skill's other files was read."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after execute's phase agent stops on a decision ask that blocks a merge, other decisions open, the user absent",
   "title": "Case dispatcher-absent-resume: an absent user's phase agent, stopped on an ask that blocks work now, is resumed with word of the absence, and each open decision relayed in one line",
   "trials": 3,
   "validated": "a169fdc8d1f818340ce1e1bc8b25075dbf4c0db2 20261114T103934 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-after-integrate",
   "items": [
    {
     "checks": [
      {
       "id": "AI-S1",
       "kind": "decide"
      },
      {
       "id": "AI-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § After integrate, a present user is put the open decisions one per call",
      "dispatcher § Open decisions are put one per call, the blocking ones first",
      "asks § A process ask is filed and never put to the user",
      "asks § A decision ask is put to the user as a question"
     ],
     "id": "AI-E1",
     "question": "Are the open decisions put one per call, the blocking ones first, the process ask never?"
    },
    {
     "checks": [
      {
       "id": "AI-S3",
       "kind": "decide"
      },
      {
       "id": "AI-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Every call offers Not now",
      "dispatcher § Every call carries the ask's recommendation",
      "dispatcher § The recommended option says so, with the file's reason"
     ],
     "id": "AI-E2",
     "question": "Does every call offer Not now and carry the file's recommendation, marked so?"
    },
    {
     "checks": [
      {
       "id": "AI-S5",
       "kind": "decide"
      },
      {
       "id": "AI-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "asks § The change waits for the MVP decision",
      "dispatcher § Not now on a blocking ask goes on as if the user were absent",
      "dispatcher § Asks that block plan wait for an absent user",
      "dispatcher § Not now leaves the ask open"
     ],
     "id": "AI-E3",
     "question": "With the MVP decision unanswered, does nothing more run on the change?"
    },
    {
     "checks": [
      {
       "id": "AI-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § Open decisions are never listed in chat while the user is present",
      "dispatcher § After integrate, a present user is put the open decisions one per call"
     ],
     "id": "AI-E4",
     "question": "Are the open decisions never listed in chat before they are put?"
    },
    {
     "checks": [
      {
       "id": "AI-S8",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A hand-back is relayed line first, then the open decisions",
      "dispatcher § Asks come before the phase's results"
     ],
     "id": "AI-E5",
     "question": "Is the hand-back relayed line first, and do the decisions come before the phase's results?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "36a4997",
     "cites": {
      "AI-E1": [
       "dispatcher § After integrate, a present user is put the open decisions one per call",
       "dispatcher § Open decisions are put one per call, the blocking ones first",
       "asks § A process ask is filed and never put to the user",
       "asks § A decision ask is put to the user as a question"
      ],
      "AI-E2": [
       "dispatcher § Every call offers Not now",
       "dispatcher § Every call carries the ask's recommendation",
       "dispatcher § The recommended option says so, with the file's reason"
      ],
      "AI-E3": [
       "asks § The change waits for the MVP decision",
       "dispatcher § Not now on a blocking ask goes on as if the user were absent",
       "dispatcher § Asks that block plan wait for an absent user",
       "dispatcher § Not now leaves the ask open"
      ],
      "AI-E4": [
       "dispatcher § Open decisions are never listed in chat while the user is present",
       "dispatcher § After integrate, a present user is put the open decisions one per call"
      ],
      "AI-E5": [
       "dispatcher § A hand-back is relayed line first, then the open decisions",
       "dispatcher § Asks come before the phase's results"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "AI-E1": [
       "AI-S1",
       "AI-S2"
      ],
      "AI-E2": [
       "AI-S3",
       "AI-S4"
      ],
      "AI-E3": [
       "AI-S5",
       "AI-S6"
      ],
      "AI-E4": [
       "AI-S7"
      ],
      "AI-E5": [
       "AI-S8"
      ]
     },
     "items_then": [],
     "run": "20261114T102453",
     "trials": [
      {
       "calls": {},
       "checks": {
        "AI-S1": {
         "audit": {
          "agree": true,
          "reason": "There were two AskUserQuestion calls, each holding exactly one question, as the check reports.",
          "verdict": "agrees"
         },
         "desc": "every call to the user holds one question",
         "detail": "2 calls",
         "kind": "decide",
         "pass": true
        },
        "AI-S2": {
         "audit": {
          "agree": true,
          "reason": "ask-cycle-2-3, a decision ask, had no call, so the fail fits the evidence and the item.",
          "verdict": "agrees"
         },
         "desc": "each decision ask put in exactly one call, the process ask in none, no call for anything else",
         "detail": "ask-cycle-2-1 (decision) 1 call(s); ask-cycle-2-2 (decision) 1 call(s); ask-cycle-2-3 (decision) 0 call(s); ask-cycle-2-4 (process) 0 call(s)",
         "kind": "decide",
         "pass": false
        },
        "AI-S3": {
         "audit": {
          "agree": true,
          "reason": "Both calls carry a \"Not now\" option, as the host's log shows.",
          "verdict": "agrees"
         },
         "desc": "every call offers Not now",
         "detail": "ask-cycle-2-1: Not now; ask-cycle-2-2: Not now",
         "kind": "decide",
         "pass": true
        },
        "AI-S4": {
         "audit": {
          "agree": true,
          "reason": "Each call marks exactly the file's recommended label with \"(Recommended)\", which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "each call's option labelled (Recommended) is the one its file recommends",
         "detail": "ask-cycle-2-1: marked ['harden (Recommended)'] (file: harden); ask-cycle-2-2: marked ['No total line (Recommended)'] (file: No total line)",
         "kind": "decide",
         "pass": true
        },
        "AI-S5": {
         "audit": {
          "agree": true,
          "reason": "The main thread shows no Agent, Task or SendMessage call.",
          "verdict": "agrees"
         },
         "desc": "no phase agent spawned or resumed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "AI-S6": {
         "audit": {
          "agree": true,
          "reason": "The host answered Not now, and ask-cycle-2-1 is unchanged (not among the changed files): open, with only a comment under * Answer.",
          "verdict": "agrees"
         },
         "desc": "the MVP decision, answered Not now, stays open in the inbox with nothing under its Answer",
         "detail": "host chose 'Not now'; in the inbox: True; status open; Answer ''",
         "kind": "decide",
         "pass": true
        },
        "AI-S7": {
         "audit": {
          "agree": true,
          "reason": "No message came before the last call, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "no message lists two or more decisions before they are put",
         "detail": "",
         "kind": "signal",
         "pass": true
        },
        "AI-S8": {
         "audit": {
          "agree": true,
          "reason": "No message preceded the first call, so the hand-back line was not relayed first and the fail fits.",
          "verdict": "agrees"
         },
         "desc": "the hand-back line shown before the first call",
         "detail": "0 message(s) before the first call",
         "kind": "signal",
         "pass": false
        }
       },
       "items": {
        "AI-E1": "fail",
        "AI-E2": "pass",
        "AI-E3": "pass",
        "AI-E4": "pass",
        "AI-E5": "fail"
       },
       "judge": {
        "AI-E1": {
         "quotes": [
          "After two \"Not now\" answers I didn't put this one, and it blocks nothing:\n\n- **ask-cycle-2-3**",
          "After two \"Not now\" answers I didn't put this one, and it blocks nothing:\n\n- **ask-cycle-2-3**",
          "After two \"Not now\" answers I didn't put this one, and it blocks nothing: - **ask-cycle-2-3**"
         ],
         "reasons": [
          "The MVP decision came first and ask-cycle-2-2 second, each in its own call, and the process ask was never put, but decision ask ask-cycle-2-3 was never put in a call.",
          "The MVP decision (call 5) and ask-cycle-2-2 (call 7) were each put on their own, in the right order, and the process ask was never put, but the third decision ask, ask-cycle-2-3, was never put to the user.",
          "The MVP decision and ask-cycle-2-2 each got their own call in the right order, and the process ask was never put, but the decision ask ask-cycle-2-3 was never put to the user."
         ]
        },
        "AI-E2": {
         "quotes": [
          "{\"label\": \"harden (Recommended)\", \"description\": \"... Recommended: a refund shown with the wrong amount is a wrong number in a script's sum.\"} ... {\"label\": \"Not now\" ...}",
          "\"label\": \"harden (Recommended)\", \"description\": \"... Recommended: a refund shown with the wrong amount is a wrong number in a script's sum.\"",
          "{\"label\": \"harden (Recommended)\", \"description\": \"... Recommended: a refund shown with the wrong amount is a wrong number in a script's sum.\"} ... {\"label\": \"Not now\", ...}"
         ],
         "reasons": [
          "Both calls offer Not now, and in each the file's recommended option ends \"(Recommended)\" with the file's reason in its description (call 2: \"No total line (Recommended)\" ... \"nothing observed needs it\").",
          "Both calls offer Not now, and each marks the file's recommended option (harden; No total line) with (Recommended) and a description giving the file's reason.",
          "Both calls offer Not now, and each marks the file's recommended option '(Recommended)' with the file's reason in its description (harden; No total line)."
         ]
        },
        "AI-E3": {
         "quotes": [
          "To pick up again, answer ask-cycle-2-1 here or in its file's `* Answer` section, then run `/dev:orchestrate`.",
          "To pick up again, answer ask-cycle-2-1 here or in its file's `* Answer` section, then run `/dev:orchestrate`.",
          "To pick up again, answer ask-cycle-2-1 here or in its file's `* Answer` section, then run `/dev:orchestrate`."
         ],
         "reasons": [
          "The dispatcher made no Agent, SendMessage or commit calls, ask-cycle-2-1 stays open and unchanged in the inbox, and the session ends pointing at the ask's file, whose path it gives in the same message.",
          "No Agent or SendMessage call was made, the MVP ask's file stays open and unanswered, the only files changed are agents-pending.json and state.lock, and the final message ends by pointing at the ask's file (its path is also given above).",
          "After Not now there was no Agent, SendMessage or commit call, the ask file is unchanged and open, and the session ends pointing at the ask's file."
         ]
        },
        "AI-E4": {
         "quotes": [
          "[call 1] Bash ... [call 7] AskUserQuestion ... [message] I recorded the integrate agent's setting",
          "",
          "[call 5] AskUserQuestion ... [call 7] AskUserQuestion ... [message] I recorded the integrate agent's setting"
         ],
         "reasons": [
          "The dispatcher's only message comes after its last call, so no decision ask was listed in chat before being put.",
          "The dispatcher wrote no message before its last call, so it listed no decision ask before putting it.",
          "The dispatcher wrote no message before its last call, so no decision asks were listed in chat before being put."
         ]
        },
        "AI-E5": {
         "quotes": [
          "[message] I recorded the integrate agent's setting (sonnet, high). json-report is stopped and waiting on you.\n\n**integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; ...**",
          "**integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; .orchestrator/changes/json-report/inbox/ask-cycle-2-1.org**",
          "[message] ... **integrate cycle-2: stopped, ask ask-cycle-2-1 blocks plan; ...**"
         ],
         "reasons": [
          "The hand-back line was shown to the user only in the final message, after the decisions had been put, not before them.",
          "The hand-back line was shown only in the final message, after both calls had been put, not first.",
          "The hand-back line was shown to the user only in the final message, after both calls, not first and before the decisions were put."
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
   "step": "the dispatcher's step after integrate's phase agent hands back, stopped on the MVP decision, with two other decision asks and a process ask open, the user present",
   "title": "Case dispatcher-after-integrate: after integrate stops on the MVP decision, the open decisions are put one per call and the change waits",
   "trials": 3,
   "validated": "36a499730f9abd6cb17200e240db056330b9252e 20261114T102453 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-chat",
   "items": [
    {
     "checks": [
      {
       "id": "CH-S1",
       "kind": "decide"
      },
      {
       "id": "CH-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An answer given in chat is written into the file and committed",
      "asks § An answer given in chat is written into the file"
     ],
     "id": "CH-E1",
     "question": "Is A's answer, given in chat, written into its file and committed?"
    },
    {
     "checks": [
      {
       "id": "CH-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A free-text answer is written for the phase agent to read back"
     ],
     "id": "CH-E3",
     "question": "Is B's answer, given in the user's own words in a call, written as given, its readback left to the phase agent?"
    },
    {
     "checks": [
      {
       "id": "CH-S5",
       "kind": "decide"
      },
      {
       "id": "CH-S6",
       "kind": "decide"
      },
      {
       "id": "CH-S7",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A readback is shown in chat and its confirmation written to the file",
      "asks § A yes given in chat is written under the readback"
     ],
     "id": "CH-E4",
     "question": "Is C's readback confirmed in the file from the user's yes in chat, and committed?"
    },
    {
     "checks": [
      {
       "id": "CH-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An answer given in a call is committed with the ask's id",
      "dispatcher § Stopped on an ask, a present user is asked and the agent resumed"
     ],
     "id": "CH-E5",
     "question": "Are the answers committed before the round's agent is resumed?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "c8c35b6",
     "cites": {
      "CH-E1": [
       "dispatcher § An answer given in chat is written into the file and committed",
       "asks § An answer given in chat is written into the file"
      ],
      "CH-E3": [
       "dispatcher § A free-text answer is written for the phase agent to read back"
      ],
      "CH-E4": [
       "dispatcher § A readback is shown in chat and its confirmation written to the file",
       "asks § A yes given in chat is written under the readback"
      ],
      "CH-E5": [
       "dispatcher § An answer given in a call is committed with the ask's id",
       "dispatcher § Stopped on an ask, a present user is asked and the agent resumed"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "CH-E1": [
       "CH-S1",
       "CH-S2"
      ],
      "CH-E3": [
       "CH-S4"
      ],
      "CH-E4": [
       "CH-S5",
       "CH-S6",
       "CH-S7"
      ],
      "CH-E5": [
       "CH-S8"
      ]
     },
     "items_then": [],
     "run": "20261114T112327",
     "trials": [
      {
       "calls": {
        "2": {
         "error": false,
         "text": "cat .orchestrator/changes/json-report/inbox/ask-cycle-1-1.org; echo ----; cat .orchestrator/changes/json-report/inbox/ask-cycle-1-2.org; echo ----; cat .orchestrator/changes/json-report/inbox/ask-cycle-1-3.org",
         "tool": "Bash"
        }
       },
       "checks": {
        "CH-S1": {
         "audit": {
          "agree": true,
          "reason": "Once the template comment line is set aside, A's Answer holds exactly Strings, as the item expects.",
          "verdict": "agrees"
         },
         "desc": "A's chat answer written under its file's Answer",
         "detail": "Answer 'Strings'",
         "kind": "decide",
         "pass": true
        },
        "CH-S2": {
         "audit": {
          "agree": true,
          "reason": "Commit 463a9af names ask-cycle-1-1 in its subject and changes A's file to hold Strings.",
          "verdict": "agrees"
         },
         "desc": "A's answer committed in the home, the subject naming its id",
         "detail": "commits ['463a9af']",
         "kind": "decide",
         "pass": true
        },
        "CH-S4": {
         "audit": {
          "agree": true,
          "reason": "The host gave free text, the Answer holds it verbatim, and there is no Readback section, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "B's free-text answer written as given, and no readback written by the dispatcher",
         "detail": "host gave free text: True; Answer 'After the file, and when --json comes first, say so in the error rather than just refusing.'; Readback section False",
         "kind": "decide",
         "pass": true
        },
        "CH-S5": {
         "audit": {
          "agree": true,
          "reason": "C's Readback now ends with \"yes\", which the fixture's readback did not have.",
          "verdict": "agrees"
         },
         "desc": "the yes written under C's Readback",
         "detail": "Readback 'The table gains a last line holding the total of all categories, printed only when the ledger has two categories or more; a ledger of one category prints as it does now.\\n\\nyes'",
         "kind": "decide",
         "pass": true
        },
        "CH-S6": {
         "audit": {
          "agree": true,
          "reason": "Commit 463a9af names ask-cycle-1-3 and changes C's file to its confirmed state.",
          "verdict": "agrees"
         },
         "desc": "C's confirmation committed in the home, the subject naming its id",
         "detail": "commits ['463a9af']",
         "kind": "decide",
         "pass": true
        },
        "CH-S7": {
         "audit": {
          "agree": true,
          "reason": "The final message restates the readback's key words (table, total, line, only, ledger, two, more, categories), so it plausibly reaches the half threshold as a signal that the readback was shown.",
          "verdict": "agrees"
         },
         "desc": "C's readback shown in chat",
         "detail": "messages at events [7]",
         "kind": "signal",
         "pass": true
        },
        "CH-S8": {
         "audit": {
          "agree": true,
          "reason": "The commit calls (calls 4 and 6) both come before the SendMessage (call 7), and no commit follows it.",
          "verdict": "agrees"
         },
         "desc": "the answers committed before the agent is resumed",
         "detail": "commit calls at [3, 5], resume at 6",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "CH-E1": "pass",
        "CH-E3": "pass",
        "CH-E4": "pass",
        "CH-E5": "pass"
       },
       "judge": {
        "CH-E1": {
         "quotes": [
          "463a9af ask-cycle-1-1, ask-cycle-1-3: answered (Strings; readback confirmed) ... * Answer\n\nStrings",
          "463a9af ask-cycle-1-1, ask-cycle-1-3: answered (Strings; readback confirmed) ... * Answer\n\nStrings",
          "463a9af ask-cycle-1-1, ask-cycle-1-3: answered (Strings; readback confirmed) ... * Answer\n\nStrings"
         ],
         "reasons": [
          "A's file has Strings under * Answer and is committed in the home in a commit whose subject names ask-cycle-1-1.",
          "A's file holds Strings under its Answer, and commit 463a9af, whose subject names ask-cycle-1-1, changes that file.",
          "A's file holds Strings under * Answer, and it was committed in the home under a subject naming ask-cycle-1-1."
         ]
        },
        "CH-E3": {
         "quotes": [
          "* Answer\n\nAfter the file, and when --json comes first, say so in the error rather than just refusing.",
          "* Answer\n\nAfter the file, and when --json comes first, say so in the error rather than just refusing.",
          "* Answer\n\nAfter the file, and when --json comes first, say so in the error rather than just refusing."
         ],
         "reasons": [
          "The host's free-text answer is written verbatim under B's * Answer, B has no * Readback, and the SendMessage leaves the readback to the phase agent.",
          "The host's free text is written verbatim under B's Answer, no Readback was written, and the dispatcher told the agent to \"write its readback per templates/ask.md\".",
          "The host's free text is written under B's * Answer verbatim, B has no * Readback section, and the SendMessage asks the phase agent to write the readback."
         ]
        },
        "CH-E4": {
         "quotes": [
          "your yes on the readback is recorded. The table will end with an overall total line, but only when the ledger has two or more categories.",
          "The table will end with an overall total line, but only when the ledger has two or more categories.",
          "The table will end with an overall total line, but only when the ledger has two or more categories."
         ],
         "reasons": [
          "The dispatcher restates C's readback in chat, writes \"yes\" under C's * Readback, and commits it in 463a9af, whose subject names ask-cycle-1-3.",
          "The readback is restated in chat, yes is written under C's Readback, and commit 463a9af, whose subject names ask-cycle-1-3, changes C's file.",
          "The final message restates C's readback in chat, yes is written under C's * Readback, and commit 463a9af, whose subject names ask-cycle-1-3, changes C's file."
         ]
        },
        "CH-E5": {
         "quotes": [
          "2a416bb ask-cycle-1-2: answer in the user's words, for readback",
          "2a416bb ask-cycle-1-2: answer in the user's words, for readback",
          "2a416bb ask-cycle-1-2: answer in the user's words, for readback"
         ],
         "reasons": [
          "A was committed in call 4 and B in call 6, and only after that was the agent resumed by SendMessage in call 7.",
          "A was committed in call 4 and B in call 6, and only after both did call 7 send the SendMessage resume; no commit follows the resume.",
          "A was committed in call 4 and B in call 6, both before the SendMessage resume in call 7; call 6's exit code 1 came from the later context command, after the commit had printed its hash."
         ]
        }
       },
       "milestones": [
        {
         "call": 2,
         "candidates": [
          2
         ],
         "checks": [
          "CH-S9"
         ],
         "hits": {
          "CH-S9": [
           "ask-cycle-1-1"
          ]
         },
         "label": "ask ask-cycle-1-1.org (decision): Should the JSON report write amounts as strings (\"13.05\") or as numbers (13.05)?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-1.org",
         "scenarios": [],
         "sig": "ask, decision"
        },
        {
         "call": 2,
         "candidates": [
          2
         ],
         "checks": [],
         "hits": {},
         "label": "ask ask-cycle-1-2.org (decision): Should --json be accepted only after the file name, or anywhere on the command line?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-2.org",
         "scenarios": [],
         "sig": "ask, decision"
        },
        {
         "call": 2,
         "candidates": [
          2
         ],
         "checks": [
          "CH-S9"
         ],
         "hits": {
          "CH-S9": [
           "ask-cycle-1-3"
          ]
         },
         "label": "ask ask-cycle-1-3.org (decision): Should the table end with a line giving the total of all categories?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-3.org",
         "scenarios": [],
         "sig": "ask, decision"
        }
       ],
       "n_calls": 7,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after the design round's phase agent stopped on two asks, the user's message answering one in chat, asking about the other and confirming a third ask's readback",
   "title": "Case dispatcher-chat: answers, a discussion turn and a readback's yes given in chat are written into the asks' files and committed; a free-text answer is written as given",
   "trials": 3,
   "validated": "c8c35b687f5d23efc9f0b6bb0ad2bd5cf6ac35e7 20261114T112327 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-context-limit",
   "items": [
    {
     "checks": [
      {
       "id": "CL-S2",
       "kind": "signal"
      },
      {
       "id": "CL-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads a phase agent's context before resuming it"
     ],
     "id": "CL-E1",
     "question": "Before the resume, is the agent's context read against the limit, never its transcript?"
    },
    {
     "checks": [
      {
       "id": "CL-S1",
       "kind": "decide"
      },
      {
       "id": "CL-S4",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § Over the limit, the message that resumes the agent asks it to hand over",
      "dispatcher § A hand-back that names a running worker is resumed"
     ],
     "id": "CL-E2",
     "question": "Does the message that resumes the agent ask it to hand over?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "8e96094",
     "cites": {
      "CL-E1": [
       "dispatcher § The dispatcher reads a phase agent's context before resuming it"
      ],
      "CL-E2": [
       "dispatcher § Over the limit, the message that resumes the agent asks it to hand over",
       "dispatcher § A hand-back that names a running worker is resumed"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "CL-E1": [
       "CL-S2",
       "CL-S3"
      ],
      "CL-E2": [
       "CL-S1",
       "CL-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T102831",
     "trials": [
      {
       "calls": {},
       "checks": {
        "CL-S1": {
         "audit": {
          "agree": true,
          "reason": "The first SendMessage (call 2) is addressed to a9d26e1b4f08c3a75 and no Agent or Task call occurs, which matches the evidence and the item's resume-not-spawn requirement.",
          "verdict": "agrees"
         },
         "desc": "the same agent resumed by message, no agent spawned before it",
         "detail": "first resume at 2 (to the agent: True); first spawn at None",
         "kind": "decide",
         "pass": true
        },
        "CL-S2": {
         "audit": {
          "agree": true,
          "reason": "Call 1 is a Bash call before the resume that names the id in `state.py ... context a9d26e1b4f08c3a75`, which is the context read the item expects.",
          "verdict": "agrees"
         },
         "desc": "a call naming the agent before the resume, other than recording it",
         "detail": "events [0]",
         "kind": "signal",
         "pass": true
        },
        "CL-S3": {
         "audit": {
          "agree": true,
          "reason": "No main-thread call opens the agent's .jsonl or any file named by its id (the transcript is read only inside state.py), so the pass fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "no read of the agent's transcript",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "CL-S4": {
         "audit": {
          "agree": true,
          "reason": "The resume message contains \"hand over\" and \"handover\", which is a fair signal of the handover request the item expects.",
          "verdict": "agrees"
         },
         "desc": "the resume message asks the agent to hand over",
         "detail": "message: '{\"to\": \"a9d26e1b4f08c3a75\", \"message\": \"Your context is over the hand-over threshold (436,520 of 400,000 tokens): hand over now. Start nothing new; let the running Reviewer of dollar-table finish and land what it clears, then write the handover file per flows/execute.md \\\\u00a7 Handing ove … [cut, 412 characters]",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "CL-E1": "pass",
        "CL-E2": "pass"
       },
       "judge": {
        "CL-E1": {
         "quotes": [
          "{\"ok\": true, \"agent\": \"a9d26e1b4f08c3a75\", \"context\": 436520, \"limit\": 400000, \"over\": true",
          "state.py --change json-report context a9d26e1b4f08c3a75 … \"context\": 436520, \"limit\": 400000, \"over\": true",
          "state.py --change json-report context a9d26e1b4f08c3a75 … \"context\": 436520, \"limit\": 400000, \"over\": true"
         ],
         "reasons": [
          "Before the SendMessage, the dispatcher ran `state.py context` for the agent, which showed it over the 400,000 limit, and no main-thread call read the agent's transcript.",
          "Before the SendMessage, the dispatcher read the agent's context in tokens through the state tool and learned it was over the 400,000 limit; no main-thread call opened the transcript itself.",
          "Before the SendMessage, the dispatcher read the agent's context in tokens through the state tool, learned it was over the 400,000 limit, and did not read the transcript file itself."
         ]
        },
        "CL-E2": {
         "quotes": [
          "hand over now. Start nothing new; let the running Reviewer of dollar-table finish and land what it clears, then write the handover file",
          "hand over now. Start nothing new; let the running Reviewer of dollar-table finish and land what it clears, then write the handover file",
          "hand over now. Start nothing new; let the running Reviewer of dollar-table finish and land what it clears, then write the handover file"
         ],
         "reasons": [
          "The dispatcher resumed the same agent id by SendMessage with no spawn, and the message asks it to start nothing new, let the running worker finish, and write a handover.",
          "The dispatcher resumed the same agent by SendMessage, made no Agent spawn, and the message asks it to start nothing new, let its running worker finish, and write a handover; the resume's failure ('No transcript found') comes from the harness, not from the message.",
          "The dispatcher resumed the same agent id by SendMessage with no spawn, and the message asks it to hand over, start nothing new, let the running worker finish and write a handover; the resume being refused afterwards is the trial's stop, not the dispatcher's choice."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after execute's phase agent hands back a line naming a wait, its context over the home's limit",
   "title": "Case dispatcher-context-limit: a phase agent over its context limit is resumed with a request to hand over",
   "trials": 3,
   "validated": "8e96094a5241d11d82c88a025658f52dc1047185 20261114T102831 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-dp3",
   "items": [
    {
     "checks": [
      {
       "id": "DP-S1",
       "kind": "decide"
      },
      {
       "id": "DP-S2",
       "kind": "decide"
      },
      {
       "id": "DP-S3",
       "kind": "decide"
      },
      {
       "id": "DP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A user who sent a message in the last 15 minutes is present",
      "dispatcher § The design round's asks are all put in one sitting",
      "dispatcher § Stopped on an ask, a present user is asked and the agent resumed",
      "design-round § The round's questions are put in one sitting, one per call"
     ],
     "id": "DP-E1",
     "question": "Are A and B put to the present user in one sitting, each in a call holding it alone, before the agent is resumed?"
    },
    {
     "checks": [
      {
       "id": "DP-S4",
       "kind": "decide"
      },
      {
       "id": "DP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Open decisions are put one per call, the blocking ones first",
      "dispatcher § A stopped agent is resumed before the asks that block nothing are put",
      "dispatcher § An ask that blocks no waiting work is put with a Not now option",
      "asks § An ask that blocks nothing waiting never stops the session"
     ],
     "id": "DP-E2",
     "question": "Are the asks that block plan put first, the agent resumed as soon as they are answered, and C put after the resume?"
    },
    {
     "checks": [
      {
       "id": "DP-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A phase agent stopped on an ask is resumed, not replaced",
      "dispatcher § Stopped on an ask, a present user is asked and the agent resumed"
     ],
     "id": "DP-E3",
     "question": "Is the stopped agent resumed by message, and no agent spawned in its place?"
    },
    {
     "checks": [
      {
       "id": "DP-S7",
       "kind": "decide"
      },
      {
       "id": "DP-S8",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § Every call carries the ask's recommendation",
      "dispatcher § The recommended option says so, with the file's reason"
     ],
     "id": "DP-E4",
     "question": "Does each call carry its ask's recommendation as an option labelled =(Recommended)=, with the file's reason?"
    },
    {
     "checks": [
      {
       "id": "DP-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Every call offers Not now",
      "dispatcher § An ask that blocks no waiting work is put with a Not now option"
     ],
     "id": "DP-E5",
     "question": "Does every call offer =Not now=?"
    },
    {
     "checks": [
      {
       "id": "DP-S10",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An answer given in the call is written into the ask's file"
     ],
     "id": "DP-E6",
     "question": "Is each answer given in a call written into its ask's file?"
    },
    {
     "checks": [
      {
       "id": "DP-S11",
       "kind": "decide"
      },
      {
       "id": "DP-S12",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An answer given in a call is committed with the ask's id"
     ],
     "id": "DP-E7",
     "question": "Is each answer committed in the home, the subject naming its ask's id, before the agent is resumed?"
    },
    {
     "checks": [
      {
       "id": "DP-S13",
       "kind": "decide"
      },
      {
       "id": "DP-S14",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher",
      "asks § An answered ask is handled before other work, by a phase agent",
      "asks § An unsettled ask is in the change's inbox",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
      "dispatcher § No new cycle while one is open"
     ],
     "id": "DP-E8",
     "question": "Does the dispatcher leave the answers for the phase agent and do none of the phase's work?"
    },
    {
     "checks": [
      {
       "id": "DP-S15",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Not now leaves the ask open",
      "asks § An unsettled ask is in the change's inbox"
     ],
     "id": "DP-E9",
     "question": "Does C, answered =Not now=, stay open?"
    },
    {
     "checks": [
      {
       "id": "DP-S16",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "DP-E10",
     "question": "Does the dispatcher read no instructions of a phase or role, no artifact of the change and no code?"
    },
    {
     "checks": [
      {
       "id": "DP-S17",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An ask file is read once, when it is put"
     ],
     "id": "DP-E11",
     "question": "Is each ask file read once, in the sitting that puts it, and no file of an ask that sitting does not put?"
    },
    {
     "checks": [
      {
       "id": "DP-S18",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § Open decisions are never listed in chat while the user is present"
     ],
     "id": "DP-E12",
     "question": "Are the open decisions never listed in chat before they are put?"
    },
    {
     "checks": [
      {
       "id": "DP-S19",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A hand-back is relayed line first, then the open decisions"
     ],
     "id": "DP-E13",
     "question": "Is the hand-back relayed, the line itself first, then the open decisions?"
    },
    {
     "checks": [
      {
       "id": "DP-S20",
       "kind": "decide"
      },
      {
       "id": "DP-S21",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher records a phase agent's setting at its first hand-back",
      "record § A spawned agent has one entry with its role, its cycle and its setting",
      "record § An agent's role and setting are from closed sets",
      "record § An agent at its role default gives that as its reason"
     ],
     "id": "DP-E14",
     "question": "Is the round's agent recorded at this, its first hand-back, with its role and the setting it was spawned at?"
    },
    {
     "checks": [
      {
       "id": "DP-S22",
       "kind": "signal"
      },
      {
       "id": "DP-S23",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads a phase agent's context before resuming it"
     ],
     "id": "DP-E15",
     "question": "Before resuming the agent, does the dispatcher read its context size, never its transcript?"
    }
   ],
   "runs": {
    "main": {
     "case_commit": "",
     "cites": {},
     "commit": "e356da7",
     "item_checks": {},
     "items_then": [],
     "run": "20261112T121348",
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
     "case_commit": "54a11c5",
     "cites": {
      "DP-E1": [
       "dispatcher § A user who sent a message in the last 15 minutes is present",
       "dispatcher § The design round's asks are all put in one sitting",
       "dispatcher § Stopped on an ask, a present user is asked and the agent resumed",
       "design-round § The round's questions are put in one sitting, one per call"
      ],
      "DP-E10": [
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ],
      "DP-E11": [
       "dispatcher § An ask file is read once, when it is put"
      ],
      "DP-E12": [
       "dispatcher § Open decisions are never listed in chat while the user is present"
      ],
      "DP-E13": [
       "dispatcher § A hand-back is relayed line first, then the open decisions"
      ],
      "DP-E14": [
       "dispatcher § The dispatcher records a phase agent's setting at its first hand-back",
       "record § A spawned agent has one entry with its role, its cycle and its setting",
       "record § An agent's role and setting are from closed sets",
       "record § An agent at its role default gives that as its reason"
      ],
      "DP-E15": [
       "dispatcher § The dispatcher reads a phase agent's context before resuming it"
      ],
      "DP-E2": [
       "dispatcher § Open decisions are put one per call, the blocking ones first",
       "dispatcher § A stopped agent is resumed before the asks that block nothing are put",
       "dispatcher § An ask that blocks no waiting work is put with a Not now option",
       "asks § An ask that blocks nothing waiting never stops the session"
      ],
      "DP-E3": [
       "dispatcher § A phase agent stopped on an ask is resumed, not replaced",
       "dispatcher § Stopped on an ask, a present user is asked and the agent resumed"
      ],
      "DP-E4": [
       "dispatcher § Every call carries the ask's recommendation",
       "dispatcher § The recommended option says so, with the file's reason"
      ],
      "DP-E5": [
       "dispatcher § Every call offers Not now",
       "dispatcher § An ask that blocks no waiting work is put with a Not now option"
      ],
      "DP-E6": [
       "dispatcher § An answer given in the call is written into the ask's file"
      ],
      "DP-E7": [
       "dispatcher § An answer given in a call is committed with the ask's id"
      ],
      "DP-E8": [
       "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher",
       "asks § An answered ask is handled before other work, by a phase agent",
       "asks § An unsettled ask is in the change's inbox",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
       "dispatcher § No new cycle while one is open"
      ],
      "DP-E9": [
       "dispatcher § Not now leaves the ask open",
       "asks § An unsettled ask is in the change's inbox"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "DP-E1": [
       "DP-S1",
       "DP-S2",
       "DP-S3",
       "DP-S5"
      ],
      "DP-E10": [
       "DP-S16"
      ],
      "DP-E11": [
       "DP-S17"
      ],
      "DP-E12": [
       "DP-S18"
      ],
      "DP-E13": [
       "DP-S19"
      ],
      "DP-E14": [
       "DP-S20",
       "DP-S21"
      ],
      "DP-E15": [
       "DP-S22",
       "DP-S23"
      ],
      "DP-E2": [
       "DP-S4",
       "DP-S5"
      ],
      "DP-E3": [
       "DP-S6"
      ],
      "DP-E4": [
       "DP-S7",
       "DP-S8"
      ],
      "DP-E5": [
       "DP-S9"
      ],
      "DP-E6": [
       "DP-S10"
      ],
      "DP-E7": [
       "DP-S11",
       "DP-S12"
      ],
      "DP-E8": [
       "DP-S13",
       "DP-S14"
      ],
      "DP-E9": [
       "DP-S15"
      ]
     },
     "items_then": [],
     "run": "20261114T112046",
     "trials": [
      {
       "calls": {
        "3": {
         "error": false,
         "text": "<trial>/home/.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org",
         "tool": "Read"
        },
        "6": {
         "error": false,
         "text": "<trial>/home/.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org",
         "tool": "Read"
        },
        "8": {
         "error": false,
         "text": "python3 <trial>/skills/orchestrate/scripts/state.py --change json-report record set asks ask-cycle-1-2 \"decision=After the file only\" status=answered && git add .orchestrator/changes/json-report/inbox/ask-cycle-1-1.org .orchestrator/changes/json-report/inbox/ask-cycle-1-2.org .orchestrator/changes/json-report/ && git commit -q -m \"Answer ask-cycle-1-1, ask-cycle-1-2: Strings; After the file only\n\nCo-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>\" && git log --oneline -1 && git status --short",
         "tool": "Bash"
        }
       },
       "checks": {
        "DP-S1": {
         "audit": {
          "agree": true,
          "reason": "Three calls were made, each holding exactly one question, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "every call holds one question",
         "detail": "3 calls",
         "kind": "decide",
         "pass": true
        },
        "DP-S10": {
         "audit": {
          "agree": true,
          "reason": "The files' Answer sections match the labels chosen in the host's log, with (Recommended) dropped.",
          "verdict": "agrees"
         },
         "desc": "the answers to A and B written under their files' Answer",
         "detail": "A: chose 'Strings (Recommended)', Answer 'Strings'; B: chose 'After the file only (Recommended)', Answer 'After the file only'",
         "kind": "decide",
         "pass": true
        },
        "DP-S11": {
         "audit": {
          "agree": true,
          "reason": "Commit 690b4e9 names both ids and changes both files.",
          "verdict": "agrees"
         },
         "desc": "each of A's and B's answers committed in the home, the subject naming its id",
         "detail": "A: ['690b4e9']; B: ['690b4e9'] | commits since the fixture: ['Answer ask-cycle-1-1, ask-cycle-1-2: Strings; After the file only']",
         "kind": "decide",
         "pass": true
        },
        "DP-S12": {
         "audit": {
          "agree": true,
          "reason": "The commit (call 8) precedes the resume (call 9), and no later commit exists.",
          "verdict": "agrees"
         },
         "desc": "the answers committed before the resume",
         "detail": "git commit calls at events [7], resume at 9",
         "kind": "decide",
         "pass": true
        },
        "DP-S13": {
         "audit": {
          "agree": true,
          "reason": "All three files are in the inbox, with no APPLIED_VIA and statuses answered.",
          "verdict": "agrees"
         },
         "desc": "the asks still in the inbox, A's and B's answers not applied",
         "detail": "in the inbox: ['A', 'B', 'C']; in a cycle's asks directory: none; status A answered, B answered; APPLIED_VIA A None, B None",
         "kind": "decide",
         "pass": true
        },
        "DP-S14": {
         "audit": {
          "agree": true,
          "reason": "Nothing under docs/ changed, and the state shows cycle-1 in plan with no tasks and the gate not passed.",
          "verdict": "agrees"
         },
         "desc": "the dispatcher did none of the phase's work: docs unchanged, no task, the cycle and phase as they were",
         "detail": "docs changed: none; cycle cycle-1, phase plan, tasks 0, plan gate passed False",
         "kind": "decide",
         "pass": true
        },
        "DP-S15": {
         "audit": {
          "agree": true,
          "reason": "C was answered Not now and its file remains unchanged with status open.",
          "verdict": "agrees"
         },
         "desc": "the ask answered Not now stays open, nothing under its Answer, in the inbox",
         "detail": "C chose 'Not now'; status open; Answer ''",
         "kind": "decide",
         "pass": true
        },
        "DP-S16": {
         "audit": {
          "agree": true,
          "reason": "The three reads are the ask files only.",
          "verdict": "agrees"
         },
         "desc": "no read of the skill's instructions or code, the product's code or the change's documents",
         "detail": "3 reads, none of them",
         "kind": "decide",
         "pass": true
        },
        "DP-S17": {
         "audit": {
          "agree": true,
          "reason": "The read and call ordering per sitting matches the evidence, with the cut at the resume.",
          "verdict": "agrees"
         },
         "desc": "each ask file read once, in the sitting that puts it, before its call; no file of an ask not put in it",
         "detail": "A: read at [2], its call at 3 (sitting 0); B: read at [5], its call at 6 (sitting 0); C: read at [10], its call at 11 (sitting 1) | sittings cut at events [9]",
         "kind": "decide",
         "pass": true
        },
        "DP-S18": {
         "audit": {
          "agree": true,
          "reason": "The one pre-final message lists no unput asks.",
          "verdict": "agrees"
         },
         "desc": "no message lists two or more asks before they are put",
         "detail": "",
         "kind": "signal",
         "pass": true
        },
        "DP-S19": {
         "audit": {
          "agree": true,
          "reason": "No message precedes the first call, so the line was not relayed; the fail fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the hand-back line shown before the first call",
         "detail": "0 message(s) before the first call; the line in one of them: False",
         "kind": "signal",
         "pass": false
        },
        "DP-S2": {
         "audit": {
          "agree": true,
          "reason": "Each of A, B and C has exactly one call and no other question was put.",
          "verdict": "agrees"
         },
         "desc": "each ask put in exactly one call, and no call for anything else",
         "detail": "calls per ask: A (ask-cycle-1-1) 1, B (ask-cycle-1-2) 1, C (ask-cycle-1-3) 1",
         "kind": "decide",
         "pass": true
        },
        "DP-S20": {
         "audit": {
          "agree": true,
          "reason": "One matching entry with the required fields is present.",
          "verdict": "agrees"
         },
         "desc": "one agent entry for the round's phase agent: role phase-design, its cycle, sonnet, medium",
         "detail": "1 entries for a7c41e09b2d35f86e: [{\"task\": null, \"cycle\": \"cycle-1\", \"source\": \"role-default\", \"reason\": \"role default\", \"agent_id\": \"a7c41e09b2d35f86e\", \"role\": \"phase-design\", \"model\": \"sonnet\", \"effort\": \"medium\", \"recorded_at\": \"2026-11-14T16:20:54Z\"}]",
         "kind": "decide",
         "pass": true
        },
        "DP-S21": {
         "audit": {
          "agree": true,
          "reason": "Source and reason match role-default and role default.",
          "verdict": "agrees"
         },
         "desc": "its source role-default, its reason `role default`",
         "detail": "source 'role-default', reason 'role default'",
         "kind": "decide",
         "pass": true
        },
        "DP-S22": {
         "audit": {
          "agree": true,
          "reason": "The context-read Bash call (call 2) names the id before the resume.",
          "verdict": "agrees"
         },
         "desc": "a call naming the agent before the resume, other than recording it",
         "detail": "events [1]",
         "kind": "signal",
         "pass": true
        },
        "DP-S23": {
         "audit": {
          "agree": true,
          "reason": "No transcript or agent-named file was read.",
          "verdict": "agrees"
         },
         "desc": "no read of the agent's transcript",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "DP-S3": {
         "audit": {
          "agree": true,
          "reason": "Calls 4 and 7 have only Bash and Read between them; there is no SendMessage or Agent call.",
          "verdict": "agrees"
         },
         "desc": "no resume or spawn between the calls for the asks that block plan",
         "detail": "calls for A at event 3, B at 6; between them: no SendMessage or Agent call",
         "kind": "decide",
         "pass": true
        },
        "DP-S4": {
         "audit": {
          "agree": true,
          "reason": "A and B precede C in the call order.",
          "verdict": "agrees"
         },
         "desc": "the asks that block plan put before the other",
         "detail": "events: A 3, B 6, C 11",
         "kind": "decide",
         "pass": true
        },
        "DP-S5": {
         "audit": {
          "agree": true,
          "reason": "The resume falls after B's call and before C's, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "the agent resumed once A and B are put, before C is put",
         "detail": "events: A 3, B 6, resume 9, C 11",
         "kind": "decide",
         "pass": true
        },
        "DP-S6": {
         "audit": {
          "agree": true,
          "reason": "There is one SendMessage naming the id and no Agent call.",
          "verdict": "agrees"
         },
         "desc": "the stopped agent resumed by message, no agent spawned",
         "detail": "SendMessage calls naming a7c41e09b2d35f86e: 1 (of 1); Agent calls: 0",
         "kind": "decide",
         "pass": true
        },
        "DP-S7": {
         "audit": {
          "agree": true,
          "reason": "In each call exactly one option is marked, and it matches the file's recommendation.",
          "verdict": "agrees"
         },
         "desc": "each call's recommended option is the file's, its label ending (Recommended)",
         "detail": "A: marked ['Strings (Recommended)'] (file recommends Strings); B: marked ['After the file only (Recommended)'] (file recommends After the file only); C: marked ['No total line (Recommended)'] (file recommends No total line)",
         "kind": "decide",
         "pass": true
        },
        "DP-S8": {
         "audit": {
          "agree": true,
          "reason": "The descriptions quote the files' reasons verbatim.",
          "verdict": "agrees"
         },
         "desc": "each recommended option's description gives the file's reason",
         "detail": "A: 9 of 9 words of the file's reason; B: 6 of 6 words of the file's reason; C: 5 of 5 words of the file's reason",
         "kind": "signal",
         "pass": true
        },
        "DP-S9": {
         "audit": {
          "agree": true,
          "reason": "All three calls show a Not now option.",
          "verdict": "agrees"
         },
         "desc": "every call offers Not now",
         "detail": "A: ['Not now']; B: ['Not now']; C: ['Not now']",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "DP-E1": "pass",
        "DP-E10": "pass",
        "DP-E11": "pass",
        "DP-E12": "pass",
        "DP-E13": "fail",
        "DP-E14": "pass",
        "DP-E15": "pass",
        "DP-E2": "pass",
        "DP-E3": "pass",
        "DP-E4": "pass",
        "DP-E5": "pass",
        "DP-E6": "pass",
        "DP-E7": "pass",
        "DP-E8": "pass",
        "DP-E9": "pass"
       },
       "judge": {
        "DP-E1": {
         "quotes": [
          "[call 4] AskUserQuestion ... ask-cycle-1-1 ... [call 7] AskUserQuestion ... ask-cycle-1-2 ... [call 9] SendMessage",
          "[call 4] AskUserQuestion ... ask-cycle-1-1 ... [call 7] AskUserQuestion ... ask-cycle-1-2 ... [call 9] SendMessage",
          "calls for A at event 3, B at 6; between them: no SendMessage or Agent call"
         ],
         "reasons": [
          "A and B were each put alone in consecutive calls with no resume or spawn between them, and the agent was resumed only after both were answered.",
          "A and B were each put alone, in consecutive calls with only a write between them, and the agent was resumed after both answers came back.",
          "A and B were each put alone in back-to-back AskUserQuestion calls (calls 4 and 7), and the SendMessage resume (call 9) came only after both were answered."
         ]
        },
        "DP-E10": {
         "quotes": [
          "[call 3] Read ... ask-cycle-1-1.org [call 6] Read ... ask-cycle-1-2.org [call 10] Read ... ask-cycle-1-3.org",
          "[call 3] Read ... ask-cycle-1-1.org; [call 6] Read ... ask-cycle-1-2.org; [call 10] Read ... ask-cycle-1-3.org",
          "[call 3] Read ask-cycle-1-1.org; [call 6] Read ask-cycle-1-2.org; [call 10] Read ask-cycle-1-3.org"
         ],
         "reasons": [
          "The only reads were the three ask files plus state-tool queries; no other skill file, product code, or change artifact was read.",
          "Apart from the state tool's status, the only reads were the three ask files; no skill files, code or change artifacts were read.",
          "The only reads were the three ask files and the state tool's status; no skill file other than the entry document, code, tests or change artifact was read."
         ]
        },
        "DP-E11": {
         "quotes": [
          "[call 3] Read ask-cycle-1-1 → [call 4] ask; [call 6] Read ask-cycle-1-2 → [call 7]; [call 10] Read ask-cycle-1-3 → [call 11]",
          "[call 10] Read: ... ask-cycle-1-3.org (after [call 9] SendMessage)",
          "A: read at [2], its call at 3 (sitting 0); B: read at [5], its call at 6 (sitting 0); C: read at [10], its call at 11 (sitting 1)"
         ],
         "reasons": [
          "Each ask file was read once, just before its own call and in its own sitting; C's file was read only after the resume.",
          "Each file was read once, just before its own call and in its own sitting, and the inbox was never read by a pattern.",
          "Each file was read once, just before its own call and in the sitting that put it, and C's file was read only after the resume."
         ]
        },
        "DP-E12": {
         "quotes": [
          "[message] Both blocking asks are answered; resuming the design-round agent.",
          "[message] Both blocking asks are answered; resuming the design-round agent.",
          "Both blocking asks are answered; resuming the design-round agent."
         ],
         "reasons": [
          "The only message before the last call lists no asks that had not yet been put.",
          "The only message before the last call lists none of the asks that had not yet been put.",
          "The only message before the last call lists no ask that had not yet been put."
         ]
        },
        "DP-E13": {
         "quotes": [
          "",
          "[call 1] Bash ... (no message before it); final text does not contain the hand-back line",
          "0 message(s) before the first call"
         ],
         "reasons": [
          "No message before the first call shows the hand-back line or relays the hand-back at all, and the final message does not show the line either; the dispatcher went straight to the calls.",
          "The dispatcher never showed the hand-back line; it went straight to the record and the calls.",
          "The dispatcher showed the user nothing of the hand-back (neither the line nor that the round stopped) before going straight into A's call."
         ]
        },
        "DP-E14": {
         "quotes": [
          "\"agent_id\": \"a7c41e09b2d35f86e\", \"role\": \"phase-design\", \"model\": \"sonnet\", \"effort\": \"medium\" ... \"source\": \"role-default\", \"reason\": \"role default\", \"cycle\": \"cycle-1\"",
          "\"cycle\": \"cycle-1\", \"source\": \"role-default\", \"reason\": \"role default\", \"agent_id\": \"a7c41e09b2d35f86e\", \"role\": \"phase-design\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"agent_id\": \"a7c41e09b2d35f86e\", \"role\": \"phase-design\", \"model\": \"sonnet\", \"effort\": \"medium\", \"source\": \"role-default\", \"reason\": \"role default\", \"cycle\": \"cycle-1\""
         ],
         "reasons": [
          "There is exactly one entry for the agent, recorded at the hand-back with the required role, cycle, setting, source and reason.",
          "There is one entry for the agent, with the expected role, cycle, setting, source and reason.",
          "Exactly one entry records the agent with the required role, cycle, setting, source and reason."
         ]
        },
        "DP-E15": {
         "quotes": [
          "[call 2] ... state.py --change json-report context a7c41e09b2d35f86e → \"error\": \"no_transcript\"",
          "[call 2] ... state.py --change json-report context a7c41e09b2d35f86e → \"error\": \"no_transcript\"",
          "[call 2] Bash: ... state.py --change json-report context a7c41e09b2d35f86e"
         ],
         "reasons": [
          "The context read was attempted before the resume and reported unknown, and no transcript was read.",
          "The context read was made before the resume and reported unknown; no transcript was read directly.",
          "The dispatcher made the context-size read before the resume (it returned no_transcript), and it read no transcript."
         ]
        },
        "DP-E2": {
         "quotes": [
          "[call 9] SendMessage ... [call 11] AskUserQuestion ... ask-cycle-1-3, does not block the current work",
          "[call 9] SendMessage ... [call 11] AskUserQuestion ... ask-cycle-1-3, does not block the current work",
          "[message] Both blocking asks are answered; resuming the design-round agent."
         ],
         "reasons": [
          "A and B were put first, the resume followed as soon as B was answered, and C was put in its own call after the resume.",
          "A and B came first, the resume followed straight after their answers without waiting on C, and C was put afterwards in its own call.",
          "A and B were put first, the agent was resumed at call 9 without waiting for C, and C was put alone afterwards at call 11."
         ]
        },
        "DP-E3": {
         "quotes": [
          "[call 9] SendMessage: {\"to\": \"a7c41e09b2d35f86e\", ...",
          "[call 9] SendMessage: {\"to\": \"a7c41e09b2d35f86e\", ...",
          "[call 9] SendMessage: {\"to\": \"a7c41e09b2d35f86e\", ..."
         ],
         "reasons": [
          "The round's agent was resumed by message and no Agent call appears anywhere in the session.",
          "The round's agent was resumed by message, and the thread has no Agent call.",
          "The round's agent was resumed by a message, and the session made no Agent call."
         ]
        },
        "DP-E4": {
         "quotes": [
          "\"label\": \"No total line (Recommended)\", \"description\": \"The table stays as it is. Recommended: nothing observed needs it and no goal of this change asks for it.\"",
          "\"label\": \"Strings (Recommended)\", \"description\": \"... Recommended: the spec's example and the table agree on strings; nothing observed needs numbers.\"",
          "\"label\": \"No total line (Recommended)\", \"description\": \"The table stays as it is. Recommended: nothing observed needs it and no goal of this change asks for it.\""
         ],
         "reasons": [
          "In each call only the file's recommended option is marked (Recommended), and its description carries the file's reason.",
          "In each call the option the file recommends is marked (Recommended) and its description gives the file's reason; no other option is marked.",
          "In each call only the file's recommended option is marked (Recommended), and its description gives the file's Recommendation reason."
         ]
        },
        "DP-E5": {
         "quotes": [
          "{\"label\": \"Not now\", \"description\": \"Leave the ask open; the change waits, plan drafts nothing until it is answered.\"}",
          "{\"label\": \"Not now\", ...} (calls 4, 7, 11)",
          "{\"label\": \"Not now\", \"description\": \"Leave the ask open; the change waits, plan drafts nothing until it is answered.\"}"
         ],
         "reasons": [
          "All three calls, including A's and B's, offer a Not now option.",
          "All three calls offer Not now.",
          "All three calls, including A's and B's, offer a Not now option."
         ]
        },
        "DP-E6": {
         "quotes": [
          "* Answer\n\nStrings ... * Answer\n\nAfter the file only",
          "* Answer\n\nStrings ... * Answer\n\nAfter the file only",
          "* Answer\n\nStrings / * Answer\n\nAfter the file only"
         ],
         "reasons": [
          "Both files hold the chosen labels under * Answer, without the (Recommended) suffix.",
          "Each file holds the chosen label under * Answer, without (Recommended).",
          "A's and B's inbox files hold the chosen labels, without (Recommended), under * Answer."
         ]
        },
        "DP-E7": {
         "quotes": [
          "690b4e9 Answer ask-cycle-1-1, ask-cycle-1-2: Strings; After the file only",
          "690b4e9 Answer ask-cycle-1-1, ask-cycle-1-2: Strings; After the file only",
          "690b4e9 Answer ask-cycle-1-1, ask-cycle-1-2: Strings; After the file only"
         ],
         "reasons": [
          "One commit, made in call 8 before the resume in call 9, holds both answered files, and its subject names both ids.",
          "One commit names both ids and holds both answered files, and it was made in call 8, before the resume in call 9.",
          "One commit naming both ids and holding both answered files was made at call 8, before the resume at call 9."
         ]
        },
        "DP-E8": {
         "quotes": [
          "\"cycle_id\": \"cycle-1\", \"phase\": \"plan\" ... \"tasks\": [] ... Changes under docs/ since the fixture: (none)",
          "Changes under docs/ since the fixture: (none); \"phase\": \"plan\", \"tasks\": [], plan \"passed\": false",
          "\"cycle_id\": \"cycle-1\", \"phase\": \"plan\", ... \"tasks\": []"
         ],
         "reasons": [
          "The asks stay in the inbox and are not applied, docs/ is untouched, and the record still shows cycle-1 in plan with no task and plan's gate not passed.",
          "Both asks stay in the inbox, no docs changed, and the record keeps cycle-1 in plan with no task and its gate not passed.",
          "The asks stay in the inbox with no APPLIED_VIA, docs/ is unchanged, and the record shows cycle-1 in plan with its gate not passed and no task."
         ]
        },
        "DP-E9": {
         "quotes": [
          "{\"id\": \"ask-cycle-1-3\", ... \"status\": \"open\"",
          "\"id\": \"ask-cycle-1-3\", ... \"status\": \"open\"",
          "\"id\": \"ask-cycle-1-3\", ... \"status\": \"open\""
         ],
         "reasons": [
          "C's file is unchanged from the fixture (status open, only the comment line under * Answer), and its state entry stays open.",
          "C was answered Not now; its file is not among the changed files (so it is still open with an empty Answer), and the state lists it as open.",
          "C was answered Not now, its file is not among the changed files (so it is still open with an empty Answer), and its state entry is open."
         ]
        }
       },
       "milestones": [
        {
         "call": 3,
         "candidates": [
          3,
          8
         ],
         "checks": [
          "DP-S11",
          "DP-S2"
         ],
         "hits": {
          "DP-S11": [
           "ask-cycle-1-1"
          ],
          "DP-S2": [
           "ask-cycle-1-1"
          ]
         },
         "label": "ask ask-cycle-1-1.org (decision): Should the JSON report write amounts as strings (\"13.05\") or as numbers (13.05)?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-1.org",
         "scenarios": [],
         "sig": "ask, decision"
        },
        {
         "call": 6,
         "candidates": [
          6,
          8
         ],
         "checks": [
          "DP-S11",
          "DP-S2"
         ],
         "hits": {
          "DP-S11": [
           "ask-cycle-1-2"
          ],
          "DP-S2": [
           "ask-cycle-1-2"
          ]
         },
         "label": "ask ask-cycle-1-2.org (decision): Should --json be accepted only after the file name, or anywhere on the command line?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-2.org",
         "scenarios": [],
         "sig": "ask, decision"
        }
       ],
       "n_calls": 11,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after a phase agent's hand-back: the design round's phase agent stopped on two asks that block plan, a third ask open, the user present",
   "title": "Case dispatcher-dp3: the dispatcher puts a stopped round's asks to a present user, one per call, and resumes it",
   "trials": 3,
   "validated": "54a11c53471774bf4b0da90042f0c699c025c3a3 20261114T112046 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-environment",
   "items": [
    {
     "checks": [
      {
       "id": "EV-S1",
       "kind": "decide"
      },
      {
       "id": "EV-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An environment ask is put at once, alone",
      "asks § An environment ask reaches the user at once"
     ],
     "id": "EV-E1",
     "question": "Is the environment ask put at once, alone, before anything else open?"
    },
    {
     "checks": [
      {
       "id": "EV-S3",
       "kind": "decide"
      },
      {
       "id": "EV-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Stopped on an ask, a present user is asked and the agent resumed",
      "dispatcher § An answer given in the call is written into the ask's file",
      "dispatcher § A phase agent stopped on an ask is resumed, not replaced"
     ],
     "id": "EV-E2",
     "question": "Is the answer written into the ask's file and the same agent resumed?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "8abd6de",
     "cites": {
      "EV-E1": [
       "dispatcher § An environment ask is put at once, alone",
       "asks § An environment ask reaches the user at once"
      ],
      "EV-E2": [
       "dispatcher § Stopped on an ask, a present user is asked and the agent resumed",
       "dispatcher § An answer given in the call is written into the ask's file",
       "dispatcher § A phase agent stopped on an ask is resumed, not replaced"
      ]
     },
     "commit": "8f6d316",
     "item_checks": {
      "EV-E1": [
       "EV-S1",
       "EV-S2"
      ],
      "EV-E2": [
       "EV-S3",
       "EV-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T112516",
     "trials": [
      {
       "calls": {
        "2": {
         "error": false,
         "text": "<trial>/home/.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org",
         "tool": "Read"
        }
       },
       "checks": {
        "EV-S1": {
         "audit": {
          "agree": true,
          "reason": "The first AskUserQuestion holds one question and it is ask-cycle-1-2; the script's event index differs from the transcript's call number, but it points at the same call.",
          "verdict": "agrees"
         },
         "desc": "the first call to the user puts the environment ask, alone",
         "detail": "first call at event 2: 1 question(s), ask ask-cycle-1-2",
         "kind": "decide",
         "pass": true
        },
        "EV-S2": {
         "audit": {
          "agree": true,
          "reason": "The decision was put (call 7) after the environment ask's call (call 3), which matches the observed ordering.",
          "verdict": "agrees"
         },
         "desc": "the environment ask put before the decision",
         "detail": "environment ask at [2], decision at [7]",
         "kind": "decide",
         "pass": true
        },
        "EV-S3": {
         "audit": {
          "agree": true,
          "reason": "The host log shows 'Use /usr/bin/python3 (Recommended)' and the file's * Answer holds 'Use /usr/bin/python3', which matches once the marker is set aside.",
          "verdict": "agrees"
         },
         "desc": "the environment ask's answer written under its file's Answer",
         "detail": "chose 'Use /usr/bin/python3 (Recommended)'; Answer 'Use /usr/bin/python3' (file in the inbox: True)",
         "kind": "decide",
         "pass": true
        },
        "EV-S4": {
         "audit": {
          "agree": true,
          "reason": "SendMessage naming the agent's id comes after the environment ask's call, and the transcript has no Agent or Task call.",
          "verdict": "agrees"
         },
         "desc": "the same agent resumed by message after the environment ask's answer, no agent spawned",
         "detail": "resume at 5; environment ask's call at [2]; spawns none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "EV-E1": "pass",
        "EV-E2": "pass"
       },
       "judge": {
        "EV-E1": {
         "quotes": [
          "[call 3] AskUserQuestion: {\"questions\": [{\"question\": \"json-report, ask-cycle-1-2: tally's test command (`python3 -m unittest`) exits 127",
          "[call 3] AskUserQuestion: {\"questions\": [{\"question\": \"json-report, ask-cycle-1-2: tally's test command (`python3 -m unittest`) exits 127",
          "[call 3] AskUserQuestion: {\"questions\": [{\"question\": \"json-report, ask-cycle-1-2: tally's test command ...\""
         ],
         "reasons": [
          "The first call to the user carries only the environment ask ask-cycle-1-2, and the earlier-filed decision ask-cycle-1-1 is put later, at call 7.",
          "The first call to the user holds only the environment ask, ask-cycle-1-2; the earlier-filed decision ask-cycle-1-1 is put later, at call 7, after the resume.",
          "The first call to the user holds only the environment ask, and the decision ask-cycle-1-1 is put later, at call 7, after the resume."
         ]
        },
        "EV-E2": {
         "quotes": [
          "* Answer\n\n# An option label from above, or your own words; everything below this line is read as your answer.\n\nUse /usr/bin/python3",
          "* Answer\n\nUse /usr/bin/python3",
          "* Answer\n\nUse /usr/bin/python3 ... [call 5] SendMessage: {\"to\": \"a5b38f2e0c71d9a46\""
         ],
         "reasons": [
          "The chosen label (without the Recommended marker) is written under the ask file's * Answer, and SendMessage to a5b38f2e0c71d9a46 at call 5 follows the ask, with no Agent or Task spawn anywhere.",
          "The chosen label is written under the ask file's * Answer heading and committed as eb4fad7; a SendMessage to a5b38f2e0c71d9a46 follows at call 5, and no Agent or Task call appears anywhere.",
          "The chosen label is written under the environment ask's * Answer, and the same agent id is then resumed by SendMessage with no Agent or Task call anywhere."
         ]
        }
       },
       "milestones": [
        {
         "call": 2,
         "candidates": [
          2
         ],
         "checks": [
          "EV-S1"
         ],
         "hits": {
          "EV-S1": [
           "ask-cycle-1-2"
          ]
         },
         "label": "ask ask-cycle-1-2.org (environment): The test command stopped running here: which Python should it use?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-2.org",
         "scenarios": [],
         "sig": "ask, environment"
        }
       ],
       "n_calls": 7,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after execute's phase agent stops on an environment ask that blocks its merges, a decision ask also open, the user present",
   "title": "Case dispatcher-environment: an environment ask is put at once, alone, before the open decisions, and the agent resumed",
   "trials": 3,
   "validated": "8abd6de948bedde1824cb8b389f97a3f5d0c30c0 20261114T112516 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-designed",
   "items": [
    {
     "checks": [
      {
       "id": "HD-S1",
       "kind": "decide"
      },
      {
       "id": "HD-S2",
       "kind": "decide"
      },
      {
       "id": "HD-S3",
       "kind": "decide"
      },
      {
       "id": "HD-S4",
       "kind": "decide"
      },
      {
       "id": "HD-S6",
       "kind": "signal"
      },
      {
       "id": "HD-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A designed change with no cycle starts a plan",
      "dispatcher § A passed gate is followed by the next phase",
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § The brief names the phase, the change, the cycle and the home",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "HD-E1",
     "question": "Is the plan phase agent spawned at its role default to start the first cycle, the dispatcher starting none?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "ca9e2f4",
     "cites": {
      "HD-E1": [
       "dispatcher § A designed change with no cycle starts a plan",
       "dispatcher § A passed gate is followed by the next phase",
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § The brief names the phase, the change, the cycle and the home",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "HD-E1": [
       "HD-S1",
       "HD-S2",
       "HD-S3",
       "HD-S4",
       "HD-S5",
       "HD-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T103208",
     "trials": [
      {
       "calls": {},
       "checks": {
        "HD-S1": {
         "audit": {
          "agree": true,
          "reason": "The Agent call is call 3, and no SendMessage precedes it, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 2, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "HD-S2": {
         "audit": {
          "agree": true,
          "reason": "The Agent input shows model \"opus\" and effort \"medium\", matching the plan role default the item expects.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes plan's role default, opus and medium",
         "detail": "model 'opus', effort 'medium'; brief 2109 chars: \"You are the phase agent for `plan` of change `json-report` in a new cycle, which has no id until your `init` opens it (this is the change's first cycle: `init --first-cycle`), and `<cycle-id>` in the scratch path below is `new`. Home: `/private/tmp/s … [cut, 651 characters]",
         "kind": "decide",
         "pass": true
        },
        "HD-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief contains `plan`, `json-report` and the full home path, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "the brief names plan, the change and the home",
         "detail": "plan: True; json-report: True; home: True",
         "kind": "decide",
         "pass": true
        },
        "HD-S4": {
         "audit": {
          "agree": true,
          "reason": "No Bash call runs init, and the changed files show no state.json (status reported not_initialized), so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "the dispatcher starts no cycle: no init, no state file",
         "detail": "init calls none; state.json False",
         "kind": "decide",
         "pass": true
        },
        "HD-S5": {
         "audit": {
          "agree": true,
          "reason": "Call 2 does read the home's .gitignore before the spawn, a file outside what the item allows, so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay",
         "detail": "event 1: <trial>/home/.gitignore (code)",
         "kind": "decide",
         "pass": false
        },
        "HD-S6": {
         "audit": {
          "agree": true,
          "reason": "The brief says \"in a new cycle, which has no id until your `init` opens it\", so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "the brief says the cycle plan runs in is new",
         "detail": "says so",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "HD-E1": "fail"
       },
       "judge": {
        "HD-E1": {
         "quotes": [
          "cat <trial>/home/.gitignore 2>/dev/null",
          "cat <trial>/home/.gitignore 2>/dev/null",
          "cat <trial>/home/.gitignore 2>/dev/null"
         ],
         "reasons": [
          "The spawn itself fits: plan at opus/medium, naming plan, json-report, the home and a new cycle, with no init run. But before the spawn the dispatcher listed the eval directory and read the home's .gitignore, which is neither its entry document, the state nor the overlay.",
          "The spawn is right: opus, medium, with a brief that names plan, json-report, the home and a new first cycle, and no init. But before the spawn the dispatcher listed the eval directory and read the home's .gitignore, which is none of its entry document, the state or the overlay.",
          "The spawn itself meets the item: it is at opus/medium, names plan, json-report and the home, says the cycle is new, and the dispatcher ran no init. But before the spawn the dispatcher also read the home's .gitignore (and listed the eval directory), which is not its entry document, the state or the overlay."
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
   "step": "the dispatcher's step after the design round's phase agent hands back gate passed, the change designed and no cycle started",
   "title": "Case dispatcher-hb-designed: a designed change with no cycle starts a plan",
   "trials": 3,
   "validated": "ca9e2f40218231f25ae038db1ebc8157b27ec506 20261114T103208 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-failed",
   "items": [
    {
     "checks": [
      {
       "id": "HF-S1",
       "kind": "signal"
      },
      {
       "id": "HF-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A failed phase is reported and nothing further is spawned",
      "dispatcher § A hand-back is relayed line first, then the open decisions"
     ],
     "id": "HF-E1",
     "question": "Is the failure told as the line says, and nothing further spawned?"
    },
    {
     "checks": [
      {
       "id": "HF-S3",
       "kind": "decide"
      },
      {
       "id": "HF-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
     ],
     "id": "HF-E2",
     "question": "Does the dispatcher look into nothing and do none of the phase's work?"
    },
    {
     "checks": [
      {
       "id": "HF-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher records a phase agent's setting at its first hand-back",
      "record § A spawned agent has one entry with its role, its cycle and its setting",
      "record § An agent's role and setting are from closed sets",
      "record § An agent at its role default gives that as its reason"
     ],
     "id": "HF-E3",
     "question": "Is the agent recorded at its first hand-back, with its role and setting?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "HF-E1": [
       "dispatcher § A failed phase is reported and nothing further is spawned",
       "dispatcher § A hand-back is relayed line first, then the open decisions"
      ],
      "HF-E2": [
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
      ],
      "HF-E3": [
       "dispatcher § The dispatcher records a phase agent's setting at its first hand-back",
       "record § A spawned agent has one entry with its role, its cycle and its setting",
       "record § An agent's role and setting are from closed sets",
       "record § An agent at its role default gives that as its reason"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "HF-E1": [
       "HF-S1",
       "HF-S2"
      ],
      "HF-E2": [
       "HF-S3",
       "HF-S4"
      ],
      "HF-E3": [
       "HF-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103409",
     "trials": [
      {
       "calls": {},
       "checks": {
        "HF-S1": {
         "audit": {
          "agree": true,
          "reason": "It only detects that the line or its why is present and cannot see the dispatcher's added diagnosis, so its pass does not fit the item, which fails here.",
          "verdict": "suspect"
         },
         "desc": "the dispatcher tells the user what failed, as the line says",
         "detail": "the line or its why in a message: True",
         "kind": "signal",
         "pass": true
        },
        "HF-S2": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows only a Bash call, with no Agent, Task or SendMessage call, matching the no-spawn half of the item.",
          "verdict": "agrees"
         },
         "desc": "no phase agent spawned or resumed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "HF-S3": {
         "audit": {
          "agree": true,
          "reason": "The only main-thread call is the state tool, with no reads of skill files, artifacts, docs or code, which fits its verdict.",
          "verdict": "agrees"
         },
         "desc": "nothing read but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "HF-S4": {
         "audit": {
          "agree": true,
          "reason": "state.json keeps phase execute and the fixture's task statuses, there are no commits, and no merge or init/close/phase/gate call was made, which matches the item's no-phase-work expectation.",
          "verdict": "agrees"
         },
         "desc": "none of the phase's work: tasks, phase and branches as they were, no merge or phase call",
         "detail": "phase execute; tasks {'report-json': 'done', 'dollar-table': 'completed', 'ledger-comments': 'completed'}; branches moved none; calls none",
         "kind": "decide",
         "pass": true
        },
        "HF-S5": {
         "audit": {
          "agree": true,
          "reason": "It checks exactly the fields HF-E3 requires, and the state.json entry confirms its verdict.",
          "verdict": "agrees"
         },
         "desc": "one agent entry for execute's phase agent: phase-execute, its cycle, sonnet, high, role default",
         "detail": "1 entries: [{\"task\": null, \"cycle\": \"cycle-1\", \"source\": \"role-default\", \"reason\": \"role default\", \"agent_id\": \"a4c86d1e3b07f2a59\", \"role\": \"phase-execute\", \"model\": \"sonnet\", \"effort\": \"high\", \"recorded_at\": \"2026-11-14T15:34:16Z\"}]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "HF-E1": "fail",
        "HF-E2": "pass",
        "HF-E3": "pass"
       },
       "judge": {
        "HF-E1": {
         "quotes": [
          "This is the only main-path task, and the only one merged so far, so \"before any merge\" means before the next one. ... The likely suspects are the report-json merge or something that changed in the environment.",
          "The likely suspects are the report-json merge or something that changed in the environment.",
          "This is the only main-path task, and the only one merged so far, so \"before any merge\" means before the next one. ... The likely suspects are the report-json merge or something that changed in the environment."
         ],
         "reasons": [
          "Nothing was spawned and the line is quoted, but the dispatcher adds its own diagnosis: it reinterprets the line's \"before any merge\" and names likely causes, which the item excludes.",
          "The dispatcher quotes the line and spawns nothing, but it also adds its own diagnosis of the cause and reinterprets the why (\"so 'before any merge' means before the next one\"), which the item excludes.",
          "The line is quoted and nothing was spawned, but the dispatcher adds its own diagnosis (likely suspects) and reinterprets the line's why (\"before any merge\" becomes \"before the next one\"), which the item rules out."
         ]
        },
        "HF-E2": {
         "quotes": [
          "state.py --change json-report record add agents ...; state.py --change json-report status",
          "state.py --change json-report record add agents ... ; state.py --change json-report status",
          "state.py --change json-report record add agents ...; state.py --change json-report status"
         ],
         "reasons": [
          "The only call is to the state tool (record and status); it reads no baseline, handover or code, runs no test or merge, and phase and gates are unchanged.",
          "Its only call ran the state tool to record and to show status; it read no baseline file, handover or code, ran no tests, merged nothing, and phase and gates are unchanged.",
          "The only call is to the state tool (record and status); no baseline file, handover or code is read, no test or merge is run, and phase and gates are unchanged."
         ]
        },
        "HF-E3": {
         "quotes": [
          "\"cycle\": \"cycle-1\", \"source\": \"role-default\", \"reason\": \"role default\", \"agent_id\": \"a4c86d1e3b07f2a59\", \"role\": \"phase-execute\", \"model\": \"sonnet\", \"effort\": \"high\"",
          "\"agent_id\": \"a4c86d1e3b07f2a59\", \"role\": \"phase-execute\", \"model\": \"sonnet\", \"effort\": \"high\"",
          "\"agent_id\": \"a4c86d1e3b07f2a59\", \"role\": \"phase-execute\", \"model\": \"sonnet\", \"effort\": \"high\", ... \"source\": \"role-default\", \"reason\": \"role default\""
         ],
         "reasons": [
          "state.json agents holds exactly one matching entry with every required field.",
          "state.json agents has exactly one entry for the agent, with role phase-execute, cycle cycle-1, sonnet, high, source role-default and reason role default.",
          "state.json's agents holds exactly one entry for the agent, with role phase-execute, cycle cycle-1, sonnet, high, source role-default and reason role default."
         ]
        }
       },
       "milestones": [],
       "n_calls": 1,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after execute's phase agent hands back failed",
   "title": "Case dispatcher-hb-failed: a failed phase is reported as its line says, and nothing further is spawned",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103409 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-gate-passed",
   "items": [
    {
     "checks": [
      {
       "id": "GP-S1",
       "kind": "decide"
      },
      {
       "id": "GP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An ask that blocks no waiting work is put with a Not now option",
      "dispatcher § Every call offers Not now",
      "dispatcher § Not now leaves the ask open"
     ],
     "id": "GP-E1",
     "question": "Is the open decision put with Not now, and left open?"
    },
    {
     "checks": [
      {
       "id": "GP-S2",
       "kind": "decide"
      },
      {
       "id": "GP-S3",
       "kind": "decide"
      },
      {
       "id": "GP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A passed gate is followed by the next phase",
      "dispatcher § An ask that blocks nothing never stops the change",
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § The brief names the phase, the change, the cycle and the home"
     ],
     "id": "GP-E2",
     "question": "Is integrate's phase agent spawned without waiting on the answer, and the finished agent not resumed?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "GP-E1": [
       "dispatcher § An ask that blocks no waiting work is put with a Not now option",
       "dispatcher § Every call offers Not now",
       "dispatcher § Not now leaves the ask open"
      ],
      "GP-E2": [
       "dispatcher § A passed gate is followed by the next phase",
       "dispatcher § An ask that blocks nothing never stops the change",
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § The brief names the phase, the change, the cycle and the home"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "GP-E1": [
       "GP-S1",
       "GP-S4"
      ],
      "GP-E2": [
       "GP-S2",
       "GP-S3",
       "GP-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103407",
     "trials": [
      {
       "calls": {},
       "checks": {
        "GP-S1": {
         "audit": {
          "agree": true,
          "reason": "Exactly one AskUserQuestion call carries ask-cycle-1-1's question, and it offers Not now, which is what the item requires.",
          "verdict": "agrees"
         },
         "desc": "the decision put in one call offering Not now",
         "detail": "calls for it: 1; host chose 'Not now'",
         "kind": "decide",
         "pass": true
        },
        "GP-S2": {
         "audit": {
          "agree": true,
          "reason": "The single Agent call has model sonnet and effort high, matching integrate's role default as the item expects; S3 covers which phase the brief names.",
          "verdict": "agrees"
         },
         "desc": "the next phase's agent spawned, at integrate's role default, sonnet and high",
         "detail": "spawn at event 1: model 'sonnet', effort 'high'; brief 2253 chars: 'You are the phase agent for `integrate` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`. `export ORCH_CHANGE=json-report`, then run `state.py status` (`state.py` is `python3 /pri … [cut, 669 characters]",
         "kind": "decide",
         "pass": true
        },
        "GP-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief literally contains integrate, json-report, cycle-1 and the home's full path, as the item asks.",
          "verdict": "agrees"
         },
         "desc": "the brief names integrate, the change, the cycle and the home",
         "detail": "integrate: True; json-report: True; cycle: True; home: True",
         "kind": "decide",
         "pass": true
        },
        "GP-S4": {
         "audit": {
          "agree": true,
          "reason": "The host chose Not now and the ask's file is absent from the changed files, so it keeps the fixture's `:STATUS: open` with only a comment line under `* Answer`.",
          "verdict": "agrees"
         },
         "desc": "the ask, answered Not now, stays open with nothing under its Answer",
         "detail": "host chose 'Not now'; status open; Answer ''",
         "kind": "decide",
         "pass": true
        },
        "GP-S5": {
         "audit": {
          "agree": true,
          "reason": "Resuming execute's agent would take a SendMessage, and the thread shows none, which fits the item's no-resume clause.",
          "verdict": "agrees"
         },
         "desc": "the finished agent not resumed",
         "detail": "SendMessage calls at none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "GP-E1": "pass",
        "GP-E2": "pass"
       },
       "judge": {
        "GP-E1": {
         "quotes": [
          "\"options\": [{\"label\": \"No total key (Recommended)\" ... {\"label\": \"Not now\", \"description\": \"Leave the ask open; it is asked again at the next plan.\"}] ... =\"Not now\"",
          "{\"label\": \"Not now\", \"description\": \"Leave the ask open; it is asked again at the next plan.\"} ... =\"Not now\"",
          "{\"label\": \"Not now\", \"description\": \"Leave the ask open; it is asked again at the next plan.\"}] ... =\"Not now\""
         ],
         "reasons": [
          "The decision went to the user in its own AskUserQuestion call offering Not now; the user answered Not now, and the ask's file was not changed while state.json still shows it `\"status\": \"open\"`.",
          "The decision was put in its own AskUserQuestion call offering Not now; it was answered Not now, and the ask's file was left unchanged with :STATUS: open, as was its asks_for_user entry.",
          "The decision was put in its own AskUserQuestion call offering Not now; after that answer the ask file was not among the changed files and state.json still lists it with \"status\": \"open\"."
         ]
        },
        "GP-E2": {
         "quotes": [
          "\"model\": \"sonnet\", \"effort\": \"high\", \"run_in_background\": true, \"prompt\": \"You are the phase agent for `integrate` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`.",
          "[call 2] Agent: {\"description\": \"Integrate phase json-report cycle-1\", ... \"model\": \"sonnet\", \"effort\": \"high\", ... \"You are the phase agent for `integrate` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`.",
          "\"model\": \"sonnet\", \"effort\": \"high\", \"run_in_background\": true, \"prompt\": \"You are the phase agent for `integrate` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`."
         ],
         "reasons": [
          "Integrate's phase agent was spawned at sonnet/high (call 2) before the ask was put (call 4), with a brief naming integrate, json-report, cycle-1 and the home, and no SendMessage resumed execute's finished agent.",
          "Integrate's phase agent was spawned at sonnet/high with a brief naming the phase, change, cycle and home, before the ask was put (call 2 before call 4), and no SendMessage resumed execute's agent.",
          "Integrate's phase agent was spawned at sonnet/high (call 2), before the ask was put (call 4), with a brief naming the phase, change, cycle and home; no SendMessage resumed execute's agent."
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
   "step": "the dispatcher's step after execute's phase agent hands back gate passed, a decision that blocks no work open, the user present",
   "title": "Case dispatcher-hb-gate-passed: a passed gate is followed by the next phase, an ask that blocks nothing put with Not now and not waited on",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103407 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-handover",
   "items": [
    {
     "checks": [
      {
       "id": "HV-S1",
       "kind": "decide"
      },
      {
       "id": "HV-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A handover is followed by a fresh agent for the same phase",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
     ],
     "id": "HV-E1",
     "question": "Is a fresh agent spawned for execute, at the replaced agent's setting?"
    },
    {
     "checks": [
      {
       "id": "HV-S3",
       "kind": "decide"
      },
      {
       "id": "HV-S4",
       "kind": "decide"
      },
      {
       "id": "HV-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A handover is followed by a fresh agent for the same phase",
      "dispatcher § The brief names the phase, the change, the cycle and the home",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "HV-E2",
     "question": "Is its brief the phase's, with the instruction to continue from the handover, the dispatcher not reading the handover?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "HV-E1": [
       "dispatcher § A handover is followed by a fresh agent for the same phase",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
      ],
      "HV-E2": [
       "dispatcher § A handover is followed by a fresh agent for the same phase",
       "dispatcher § The brief names the phase, the change, the cycle and the home",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "HV-E1": [
       "HV-S1",
       "HV-S2"
      ],
      "HV-E2": [
       "HV-S3",
       "HV-S4",
       "HV-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103413",
     "trials": [
      {
       "calls": {},
       "checks": {
        "HV-S1": {
         "audit": {
          "agree": true,
          "reason": "Call 2 is an Agent call and no SendMessage occurs before it, which is the fresh-spawn half of HV-E1.",
          "verdict": "agrees"
         },
         "desc": "a fresh agent spawned, and the stopped one not resumed before it",
         "detail": "first spawn at event 2, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "HV-S2": {
         "audit": {
          "agree": true,
          "reason": "The spawn's input shows model 'sonnet' and effort 'high', matching the replaced agent's setting as HV-E1 requires.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes the replaced agent's setting, sonnet and high",
         "detail": "model 'sonnet', effort 'high'; brief 2199 chars: 'You are the phase agent for `execute` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`. `export ORCH_CHANGE=json-report`, then run `state.py status` (`state.py` is `python3 /private/tmp/skill-evals … [cut, 651 characters]",
         "kind": "decide",
         "pass": true
        },
        "HV-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief names the handover path verbatim, and the evidence matches the check's verdict. It observes only the path, a necessary part of HV-E2's continue instruction.",
          "verdict": "agrees"
         },
         "desc": "the brief names the handover",
         "detail": ".orchestrator/changes/json-report/cycles/cycle-1/execute-handover.md named: True",
         "kind": "decide",
         "pass": true
        },
        "HV-S4": {
         "audit": {
          "agree": true,
          "reason": "The brief contains execute, json-report, cycle-1 and the home's absolute path, as observed.",
          "verdict": "agrees"
         },
         "desc": "the brief names execute, the change, the cycle and the home",
         "detail": "execute: True; json-report: True; cycle-1: True; home: True",
         "kind": "decide",
         "pass": true
        },
        "HV-S5": {
         "audit": {
          "agree": true,
          "reason": "The only main-thread call before the spawn is the state.py record/status Bash call, which reads no skill file, artifact, docs or code, so the 'none' observation fits.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay (not the handover)",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "HV-E1": "pass",
        "HV-E2": "pass"
       },
       "judge": {
        "HV-E1": {
         "quotes": [
          "[call 2] Agent: {\"description\": \"Execute cycle-1 json-report (continue)\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"high\"",
          "[call 2] Agent: {\"description\": \"Execute cycle-1 json-report (continue)\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"high\"",
          "[call 2] Agent: {\"description\": \"Execute cycle-1 json-report (continue)\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"high\""
         ],
         "reasons": [
          "The dispatcher made a new Agent call, not a SendMessage to a0d53f9c6e18b7a24, for execute at sonnet/high, the replaced agent's setting.",
          "The dispatcher made an Agent call, not a SendMessage to a0d53f9c6e18b7a24, and it returned a new id (a8e7a3896a19bc7e6), so this is a fresh execute agent at sonnet, high.",
          "The dispatcher made a new Agent call, not a SendMessage to a0d53f9c6e18b7a24, at sonnet/high, which matches the replaced agent's setting; the result shows a new id, a8e7a3896a19bc7e6."
         ]
        },
        "HV-E2": {
         "quotes": [
          "Continue from `.orchestrator/changes/json-report/cycles/cycle-1/execute-handover.md`: read it after `state.py status`, and redo nothing it lists as done.",
          "Continue from `.orchestrator/changes/json-report/cycles/cycle-1/execute-handover.md`: read it after `state.py status`, and redo nothing it lists as done.",
          "Continue from `.orchestrator/changes/json-report/cycles/cycle-1/execute-handover.md`: read it after `state.py status`, and redo nothing it lists as done."
         ],
         "reasons": [
          "The brief is execute's: it names execute, json-report, cycle-1 and the home. It adds the continue instruction with the handover's path, read after status, redoing nothing done. The only call before the spawn was state.py record/status, so the dispatcher never read the handover.",
          "The brief names execute, json-report, cycle-1 and the home, and adds the continue-from-handover instruction with its path, its read order and the no-redo clause; the dispatcher's only earlier call was state.py record/status, so it never read the handover.",
          "The brief is execute's, naming execute, json-report, cycle-1 and the home, plus the continue-from-handover instruction with its path, read after status and redoing nothing done; the only call before the spawn ran state.py record/status, so the dispatcher never read the handover."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after execute's phase agent hands back stopped with a handover",
   "title": "Case dispatcher-hb-handover: a handover is followed by a fresh agent for the same phase, at its setting, told to continue from the handover",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103413 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-opened",
   "items": [
    {
     "checks": [
      {
       "id": "HO-S6",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § An opened change goes on to its design round",
      "dispatcher § A hand-back is relayed line first, then the open decisions"
     ],
     "id": "HO-E1",
     "question": "Is the line relayed, giving the user the intent's path?"
    },
    {
     "checks": [
      {
       "id": "HO-S1",
       "kind": "decide"
      },
      {
       "id": "HO-S2",
       "kind": "decide"
      },
      {
       "id": "HO-S3",
       "kind": "signal"
      },
      {
       "id": "HO-S4",
       "kind": "decide"
      },
      {
       "id": "HO-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An opened change goes on to its design round",
      "dispatcher § A change with no seams yet goes to the design round",
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § The brief names the phase, the change, the cycle and the home"
     ],
     "id": "HO-E2",
     "question": "Is the design round's phase agent spawned for the change, at its role default, its brief naming the change alone and the home?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "e9690e4",
     "cites": {
      "HO-E1": [
       "dispatcher § An opened change goes on to its design round",
       "dispatcher § A hand-back is relayed line first, then the open decisions"
      ],
      "HO-E2": [
       "dispatcher § An opened change goes on to its design round",
       "dispatcher § A change with no seams yet goes to the design round",
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § The brief names the phase, the change, the cycle and the home"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "HO-E1": [
       "HO-S6"
      ],
      "HO-E2": [
       "HO-S1",
       "HO-S2",
       "HO-S3",
       "HO-S4",
       "HO-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103940",
     "trials": [
      {
       "calls": {},
       "checks": {
        "HO-S1": {
         "audit": {
          "agree": true,
          "reason": "The third call (event index 2) is the Agent call and no SendMessage call precedes it, as the check reports.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 2, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "HO-S2": {
         "audit": {
          "agree": true,
          "reason": "The Agent call's input carries model 'sonnet' and effort 'medium', matching the design round's role default the item expects.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes the design round's role default, sonnet and medium",
         "detail": "model 'sonnet', effort 'medium'; brief 2339 chars: 'You are the phase agent for the design round of `json-report`. Home: `<trial>/home`. `export ORCH_CHANGE=json-report`, then run `state.py status` (`state.py` is `python3 <run>/ … [cut, 653 characters]",
         "kind": "decide",
         "pass": true
        },
        "HO-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief opens with 'the phase agent for the design round of `json-report`' and later says 'design-round json-report', so it names the design round.",
          "verdict": "agrees"
         },
         "desc": "the brief names the design round",
         "detail": "named: True",
         "kind": "signal",
         "pass": true
        },
        "HO-S4": {
         "audit": {
          "agree": true,
          "reason": "The brief contains json-report and the home's full path, and no cycle-<id> appears in it.",
          "verdict": "agrees"
         },
         "desc": "the brief names the change and the home, and no cycle",
         "detail": "change True; home True; cycle ids []",
         "kind": "decide",
         "pass": true
        },
        "HO-S5": {
         "audit": {
          "agree": true,
          "reason": "Neither Bash call runs `init`, and the files added in the home are only agents-pending.json and state.lock, with no state.json.",
          "verdict": "agrees"
         },
         "desc": "no cycle started: no init, no state file",
         "detail": "init calls none; state.json False",
         "kind": "decide",
         "pass": true
        },
        "HO-S6": {
         "audit": {
          "agree": true,
          "reason": "Both the session's message and its final text hold the line with the intent's path, so the reported 2 messages fit the evidence.",
          "verdict": "agrees"
         },
         "desc": "a message relays the line or gives the intent's path",
         "detail": "2 message(s)",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "HO-E1": "pass",
        "HO-E2": "pass"
       },
       "judge": {
        "HO-E1": {
         "quotes": [
          "**What happened:** `open json-report: 1 capabilities, 4 scenarios, 0 open questions; .worktrees/tally/json-report/docs/changes/json-report/intent.org`.",
          "**What happened:** `open json-report: 1 capabilities, 4 scenarios, 0 open questions; .worktrees/tally/json-report/docs/changes/json-report/intent.org`.",
          "**What happened:** `open json-report: 1 capabilities, 4 scenarios, 0 open questions; .worktrees/tally/json-report/docs/changes/json-report/intent.org`."
         ],
         "reasons": [
          "The dispatcher's message and final text show the user the opening's line verbatim, including the intent's path, followed by the open decisions (none).",
          "After the spawn, the dispatcher's message and final text show the opening's line word for word, including the intent's path, before the open decisions.",
          "After the refused spawn, the dispatcher's message and final text show the opening's line verbatim, including the intent's path relative to the home."
         ]
        },
        "HO-E2": {
         "quotes": [
          "\"description\": \"Design round for json-report\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\", ... \"You are the phase agent for the design round of `json-report`. Home: `<trial>/home`.",
          "\"description\": \"Design round for json-report\", ... \"model\": \"sonnet\", \"effort\": \"medium\" ... \"You are the phase agent for the design round of `json-report`. Home: `<trial>/home`.",
          "\"description\": \"Design round for json-report\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\" ... \"You are the phase agent for the design round of `json-report`. Home: `<trial>/home`."
         ],
         "reasons": [
          "The dispatcher made the design round's spawn at sonnet/medium with a brief naming the design round, json-report and the home and no cycle, and it ran no `init` (the host's refusal of the spawn is by the case's design).",
          "The dispatcher spawned the design-round phase agent at sonnet/medium, and the brief names the design round, json-report and the home with no cycle; no init ran and no state.json was created (the host refused the spawn, but the call was made as the item asks).",
          "The dispatcher makes the Agent call for the design round at sonnet/medium; the brief names the design round, json-report and the home and no cycle; it runs no init and starts no cycle (the host's refusal is the case's setup, not the dispatcher's doing)."
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
   "step": "the dispatcher's step after the opening agent hands back that the change is opened",
   "title": "Case dispatcher-hb-opened: an opened change with no seams goes on to its design round",
   "trials": 3,
   "validated": "e9690e4cb8a155b95d0d9a7522837fa814452cd7 20261114T103940 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-rethink",
   "items": [
    {
     "checks": [
      {
       "id": "RT-S1",
       "kind": "decide"
      },
      {
       "id": "RT-S2",
       "kind": "decide"
      },
      {
       "id": "RT-S3",
       "kind": "signal"
      },
      {
       "id": "RT-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A rethink answer sends the change back to opening",
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § An opening agent's brief carries the source as given"
     ],
     "id": "RT-E1",
     "question": "Is the opening agent spawned for the change, with the intent and the answer as its source?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "8a774c6",
     "cites": {
      "RT-E1": [
       "dispatcher § A rethink answer sends the change back to opening",
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § An opening agent's brief carries the source as given"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RT-E1": [
       "RT-S1",
       "RT-S2",
       "RT-S3",
       "RT-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T104228",
     "trials": [
      {
       "calls": {},
       "checks": {
        "RT-S1": {
         "audit": {
          "agree": true,
          "reason": "The first spawn is at opus/medium and its brief names itself 'the phase agent for `open`', naming no other phase as its own, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "the opening agent spawned, at opus and medium",
         "detail": "spawn at event 1: model 'opus', effort 'medium'; brief 2629 chars: \"You are the phase agent for `open` of change `json-report` (a reopening: the user answered `rethink` on the change's MVP decision; the plan phase agent applied that answer and handed back `plan: not started, change rethink; .worktre … [cut, 669 characters]",
         "kind": "decide",
         "pass": true
        },
        "RT-S2": {
         "audit": {
          "agree": true,
          "reason": "The brief names `.worktrees/json-report/docs/changes/json-report/intent.org`, as the check observed.",
          "verdict": "agrees"
         },
         "desc": "its brief names the change's intent",
         "detail": ".worktrees/json-report/docs/changes/json-report/intent.org named: True",
         "kind": "decide",
         "pass": true
        },
        "RT-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief says `rethink` several times, which meets the check's disjunction even though the ask's id is absent and the answer's word overlap is low.",
          "verdict": "agrees"
         },
         "desc": "its brief carries the answer: the MVP decision's id, the answer's words, or rethink",
         "detail": "id False; answer's words 0.29",
         "kind": "signal",
         "pass": true
        },
        "RT-S4": {
         "audit": {
          "agree": true,
          "reason": "The only call before the spawn runs state.py record and status, which reads no skill file, change artifact, docs file, or code, and a spawn follows.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RT-E1": "pass"
       },
       "judge": {
        "RT-E1": {
         "quotes": [
          "You are the phase agent for `open` of change `json-report` (a reopening: the user answered `rethink` on the change's MVP decision ... Source for the brief: the rethink answer recorded with the MVP decision ask and the current intent at `.worktrees/json-report/docs/changes/json-report/intent.org`",
          "You are the phase agent for `open` of change `json-report` (a reopening: the user answered `rethink` on the change's MVP decision ... Source for the brief: the rethink answer recorded with the MVP decision ask and the current intent at `.worktrees/json-report/docs/changes/json-report/intent.org`",
          "You are the phase agent for `open` of change `json-report` (a reopening: the user answered `rethink` on the change's MVP decision ... Source for the brief: the rethink answer recorded with the MVP decision ask and the current intent at `.worktrees/json-report/docs/changes/json-report/intent.org`"
         ],
         "reasons": [
          "The dispatcher's only spawn is the opening phase agent for json-report at opus, medium, with a brief that names the intent's path and carries the rethink answer as its source; before the spawn it only ran state.py record/status, read neither the intent nor the ask, and spawned no plan.",
          "The only spawn is the opening phase agent at opus/medium for json-report. Its brief names the intent's path and carries the user's rethink answer as its source. The only prior call was state.py record/status, which reads neither the intent nor the ask, and no plan agent was spawned.",
          "The dispatcher's only spawn is the opening agent at opus/medium for json-report. Its brief names the intent's path and carries the rethink answer as plan's line gave it. The only calls before it were state.py's record and status (neither reads the intent nor the ask), and no plan agent was spawned."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after plan's phase agent hands back that the change did not start because the user answered rethink on the MVP decision",
   "title": "Case dispatcher-hb-rethink: a rethink answer sends the change back to opening, with the intent and the answer as its source",
   "trials": 3,
   "validated": "8a774c656f39e0a6c664188aa533ab5f5afd5d3e 20261114T104228 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-second-opening",
   "items": [
    {
     "checks": [
      {
       "id": "SO-S1",
       "kind": "decide"
      },
      {
       "id": "SO-S2",
       "kind": "decide"
      },
      {
       "id": "SO-S3",
       "kind": "signal"
      },
      {
       "id": "SO-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An opening that names a second change is followed by a second opening",
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § An opened change goes on to its design round"
     ],
     "id": "SO-E1",
     "question": "Is the opening agent spawned again for the second change, from the same words and the first change's name?"
    },
    {
     "checks": [
      {
       "id": "SO-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An opening agent's brief carries the source as given",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "SO-E2",
     "question": "Does the dispatcher write the brief from what it was given, reading nothing for it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "ddd6339",
     "cites": {
      "SO-E1": [
       "dispatcher § An opening that names a second change is followed by a second opening",
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § An opened change goes on to its design round"
      ],
      "SO-E2": [
       "dispatcher § An opening agent's brief carries the source as given",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "SO-E1": [
       "SO-S1",
       "SO-S2",
       "SO-S3",
       "SO-S5"
      ],
      "SO-E2": [
       "SO-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T103938",
     "trials": [
      {
       "calls": {},
       "checks": {
        "SO-S1": {
         "audit": {
          "agree": true,
          "reason": "The first spawn is the design round of json-report at sonnet, medium, and its brief does not name opening, so the check's fail matches the evidence and what SO-E1 expects of the first spawn.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned first, at opening's role default, opus and medium, and no resume before it",
         "detail": "opening named: False; model 'sonnet', effort 'medium'; brief 2022 chars: 'You are the phase agent for the design round of `json-report`. Home: `<trial>/home`. `export ORCH_CHANGE=json-report`, then run `state.py status` (`state.py` is `python3 /private/tmp/skill- … [cut, 675 characters]",
         "kind": "decide",
         "pass": false
        },
        "SO-S2": {
         "audit": {
          "agree": true,
          "reason": "Call 3's brief, the first opening spawn, quotes the user's request verbatim and names json-report, as the check reports.",
          "verdict": "agrees"
         },
         "desc": "the opening's brief holds the user's words verbatim and the first change's name",
         "detail": "opening spawn at event 2; words verbatim: True; first change 'json-report': True",
         "kind": "decide",
         "pass": true
        },
        "SO-S3": {
         "audit": {
          "agree": true,
          "reason": "Call 3's brief names the sheets repository (\"in the `sheets` repository\"), matching the pass.",
          "verdict": "agrees"
         },
         "desc": "the opening's brief names the second repository",
         "detail": "opening spawn at event 2; 'sheets': True",
         "kind": "signal",
         "pass": true
        },
        "SO-S4": {
         "audit": {
          "agree": true,
          "reason": "The only call before the opening spawn other than a spawn ran state.py, which reads no skill file, artifact, docs or code, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the opening's spawn but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "SO-S5": {
         "audit": {
          "agree": true,
          "reason": "After the stand-in answered, call 5 spawned the json-report design round at sonnet, medium, which is what this part of SO-E1 expects; the ordering failure is SO-S1's to catch.",
          "verdict": "agrees"
         },
         "desc": "after the second opening, the first change's design round spawned at sonnet, medium",
         "detail": "stand-in: open sheets-json: 1 capabilities, 1 scenarios, 0 open questions; .worktrees/sheets/sheets-json/docs/changes/sheets-json/intent.org; spawns after it at [4, 5]; model 'sonnet', effort 'medium'; brief 2022 chars: 'You are the phase agent for the design round of `json-report`. Home: `/private/ … [cut, 822 characters]",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "SO-E1": "fail",
        "SO-E2": "pass"
       },
       "judge": {
        "SO-E1": {
         "quotes": [
          "[call 2] Agent: {\"description\": \"Design round json-report\", ... \"model\": \"sonnet\", \"effort\": \"medium\"",
          "[call 2] Agent: {\"description\": \"Design round json-report\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\"",
          "[call 2] Agent: {\"description\": \"Design round json-report\", ... \"model\": \"sonnet\", \"effort\": \"medium\""
         ],
         "reasons": [
          "The dispatcher's first spawn was the first change's design round, before the second opening (call 3), and the item says a design-round spawn before the second opening fails it, even though the later opening and design spawns were otherwise correct.",
          "The dispatcher's first spawn was the first change's design round, before the second opening (call 3), which the item says fails it, even though the later opening brief and the post-opening design spawn were otherwise right.",
          "The dispatcher's first spawn was the design round for json-report at sonnet/medium; the second opening (call 3, opus/medium, sheets, verbatim request, naming json-report) came only after it, and the item fails a design-round spawn before the second opening."
         ]
        },
        "SO-E2": {
         "quotes": [
          "[call 1] Bash: ... state.py --change json-report record add agents ...; state.py where; ... state.py --change json-report status",
          "state.py --change json-report record add agents ...; state.py where; state.py --change json-report status",
          "state.py --change json-report record add agents ...; state.py where; state.py --change json-report status"
         ],
         "reasons": [
          "Before the second opening's spawn, the only main-thread call besides the denied design spawn ran the state tool, which shows where the change stands; nothing read the intent, code, instructions or an ask.",
          "Before either spawn the main thread only recorded the agent and queried where the change stands; it read no intent, code, instructions or ask to write the brief.",
          "Before either spawn the main thread ran only state.py record/where/status (where the change stands and the overlay's repos) and read neither the first change's intent nor any code, instruction or ask; the brief's content comes from the prompt and the hand-back line."
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
   "step": "the dispatcher's step after the opening agent hands back a line naming a second change and its repository",
   "title": "Case dispatcher-hb-second-opening: an opening that names a second change's repository is followed by a second opening from the same words",
   "trials": 3,
   "validated": "ddd6339913ad154d63772ce934d77b87f9484ed8 20261114T103938 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-split",
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
      },
      {
       "id": "SP-S3",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A split's opening is followed by the old change's revision",
      "dispatcher § Each phase agent runs at its phase's role default"
     ],
     "id": "SP-E1",
     "question": "Is the opening agent spawned to revise json-report in place, from the old intent, the split answer and the new change's name?"
    },
    {
     "checks": [
      {
       "id": "SP-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An opening agent's brief carries the source as given",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "SP-E2",
     "question": "Does the dispatcher write the brief from what it was given, reading nothing for it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "255376d",
     "cites": {
      "SP-E1": [
       "dispatcher § A split's opening is followed by the old change's revision",
       "dispatcher § Each phase agent runs at its phase's role default"
      ],
      "SP-E2": [
       "dispatcher § An opening agent's brief carries the source as given",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "SP-E1": [
       "SP-S1",
       "SP-S2",
       "SP-S3"
      ],
      "SP-E2": [
       "SP-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T103936",
     "trials": [
      {
       "calls": {},
       "checks": {
        "SP-S1": {
         "audit": {
          "agree": true,
          "reason": "The first spawn passes sonnet and calls itself 'the phase agent for the design round', so the fail on model and on the phase it names fits the evidence and what SP-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned first, at opening's role default, opus and medium, and no resume before it",
         "detail": "opening named: False; model 'sonnet', effort 'medium'; brief 2269 chars: 'You are the phase agent for the design round of `options-check`. Home: `<trial>/home`. `export ORCH_CHANGE=options-check`, then run `state.py status` (`state.py` is `python3 /private/tmp/sk … [cut, 675 characters]",
         "kind": "decide",
         "pass": false
        },
        "SP-S2": {
         "audit": {
          "agree": true,
          "reason": "The brief names options-check but gives no path to json-report's intent in any form, so the fail on the old intent fits.",
          "verdict": "agrees"
         },
         "desc": "its brief names the old change's intent and the new change",
         "detail": "old intent (.worktrees/json-report/docs/changes/json-report/intent.org) named: False; new change 'options-check': True",
         "kind": "decide",
         "pass": false
        },
        "SP-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief contains neither 'ask-cycle-2-1' nor the word 'split', only 'revise: json-report', so the fail fits.",
          "verdict": "agrees"
         },
         "desc": "its brief carries the split answer: the ask's id or the word split",
         "detail": "ask ask-cycle-2-1: False; 'split': False",
         "kind": "signal",
         "pass": false
        },
        "SP-S4": {
         "audit": {
          "agree": true,
          "reason": "The only pre-spawn call runs state.py commands and reads no skill file, artifact, docs or code, and a spawn exists, so the pass fits SP-E2.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "SP-E1": "fail",
        "SP-E2": "pass"
       },
       "judge": {
        "SP-E1": {
         "quotes": [
          "\"description\": \"Design round for options-check\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"You are the phase agent for the design round of `options-check`\" ... \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"You are the phase agent for the design round of `options-check`\" ... \"model\": \"sonnet\", \"effort\": \"medium\""
         ],
         "reasons": [
          "The first spawn is the new change's design round at sonnet, not the opening agent at opus, medium revising json-report in place, and its brief names neither json-report's intent nor the split answer.",
          "The first spawn is the new change's design round at sonnet, not an opening agent at opus, medium revising json-report in place; its brief also names neither json-report's intent nor the split answer.",
          "The first spawn ran the new change's design round at sonnet, not the opening agent at opus, medium revising json-report in place, and its brief names neither json-report's intent nor the split answer."
         ]
        },
        "SP-E2": {
         "quotes": [
          "state.py --change options-check record add agents ...; state.py --change options-check status; state.py where",
          "state.py --change options-check record add agents ...; state.py --change options-check status; state.py where",
          "state.py --change options-check record add agents ...; state.py --change options-check status; state.py where"
         ],
         "reasons": [
          "Before the spawn the only call is the state tool recording the agent and showing where the changes stand; no intent, ask file, flow doc or code is read, so the brief comes from the hand-back alone.",
          "Before the spawn, the main thread's only call is one Bash call that records the agent and queries state and where the changes stand; it reads no intent, ask file or code.",
          "Before the spawn the main thread only recorded the agent and queried where the changes stand; it read no intent, ask file, flow or role doc, or code."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after the opening agent hands back a line naming an old change to revise",
   "title": "Case dispatcher-hb-split: a split's opening is followed by the old change's revision, its source the old intent, the split answer and the new change",
   "trials": 3,
   "validated": "255376dd14b822bcf4b4599cbddc3fd4e87a14f4 20261114T103936 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-stopped",
   "items": [
    {
     "checks": [
      {
       "id": "ST-S1",
       "kind": "decide"
      },
      {
       "id": "ST-S2",
       "kind": "signal"
      },
      {
       "id": "ST-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A stopped change is relayed and nothing more runs",
      "dispatcher § A hand-back is relayed line first, then the open decisions"
     ],
     "id": "ST-E1",
     "question": "Is the line relayed, and nothing more run for the change?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "b642de9",
     "cites": {
      "ST-E1": [
       "dispatcher § A stopped change is relayed and nothing more runs",
       "dispatcher § A hand-back is relayed line first, then the open decisions"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "ST-E1": [
       "ST-S1",
       "ST-S2",
       "ST-S3"
      ]
     },
     "items_then": [],
     "run": "20261114T102455",
     "trials": [
      {
       "calls": {},
       "checks": {
        "ST-S1": {
         "audit": {
          "agree": true,
          "reason": "The main thread has one Bash call and no Agent, Task or SendMessage call, so its pass matches the item's requirement that no phase agent is spawned or resumed.",
          "verdict": "agrees"
         },
         "desc": "no phase agent spawned or resumed",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "ST-S2": {
         "audit": {
          "agree": true,
          "reason": "The dispatcher's message quotes the hand-back line verbatim, intent path included, which is the relay the item expects.",
          "verdict": "agrees"
         },
         "desc": "the hand-back line shown to the user",
         "detail": "1 message(s) holding the line",
         "kind": "signal",
         "pass": true
        },
        "ST-S3": {
         "audit": {
          "agree": true,
          "reason": "The only state tool subcommands run were `record add agents` and `status`, so no cycle was started or moved, as its pass says.",
          "verdict": "agrees"
         },
         "desc": "no cycle started or phase moved: no init, close or phase call",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "ST-E1": "pass"
       },
       "judge": {
        "ST-E1": {
         "quotes": [
          "> `plan: not started, change stopped; .worktrees/json-report/docs/changes/json-report/intent.org` ... Nothing else will run for this change.",
          "> `plan: not started, change stopped; .worktrees/json-report/docs/changes/json-report/intent.org` ... Nothing else will run for this change.",
          "> `plan: not started, change stopped; .worktrees/json-report/docs/changes/json-report/intent.org` ... Nothing else will run for this change."
         ],
         "reasons": [
          "The final message shows plan's line, including the intent's path, and the session's only call is a Bash run of the state tool's `record add agents` and `status`, with no spawn, no resume and no init, close or phase.",
          "The dispatcher showed the user plan's line with the intent's path, and its only calls were the state tool's `record add agents` and `status`: no spawn, no resume and no cycle start.",
          "The dispatcher showed the user plan's line with the intent's path, and its only call was a state-tool `record add agents` plus `status`; it spawned, resumed and initialised nothing, and no cycle was opened."
         ]
        }
       },
       "milestones": [],
       "n_calls": 1,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after plan's phase agent hands back that the change did not start because the user answered stop on the MVP decision",
   "title": "Case dispatcher-hb-stopped: plan hands back that the change stopped on the user's answer, and the dispatcher relays it and runs nothing more",
   "trials": 3,
   "validated": "b642de988318b43efaf66343ef0fe5f74ac40a7e 20261114T102455 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-unfinished-round",
   "items": [
    {
     "checks": [
      {
       "id": "UR-S1",
       "kind": "decide"
      },
      {
       "id": "UR-S3",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A design round plan finds unfinished is run before any cycle",
      "dispatcher § A failed phase is reported and nothing further is spawned",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
     ],
     "id": "UR-E1",
     "question": "Does the dispatcher spawn the change's design round, and not stop on the failure?"
    },
    {
     "checks": [
      {
       "id": "UR-S2",
       "kind": "decide"
      },
      {
       "id": "UR-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § The brief names the phase, the change, the cycle and the home"
     ],
     "id": "UR-E2",
     "question": "Does the spawn pass the design round's role default and a brief naming the change alone and the home?"
    },
    {
     "checks": [
      {
       "id": "UR-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A design round plan finds unfinished is run before any cycle",
      "dispatcher § No new cycle while one is open"
     ],
     "id": "UR-E3",
     "question": "Has no cycle been opened?"
    },
    {
     "checks": [
      {
       "id": "UR-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher"
     ],
     "id": "UR-E4",
     "question": "Are the answered asks left to the round's phase agent?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "ddb3222",
     "cites": {
      "UR-E1": [
       "dispatcher § A design round plan finds unfinished is run before any cycle",
       "dispatcher § A failed phase is reported and nothing further is spawned",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
      ],
      "UR-E2": [
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § The brief names the phase, the change, the cycle and the home"
      ],
      "UR-E3": [
       "dispatcher § A design round plan finds unfinished is run before any cycle",
       "dispatcher § No new cycle while one is open"
      ],
      "UR-E4": [
       "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher"
      ]
     },
     "commit": "db6ad9d",
     "item_checks": {
      "UR-E1": [
       "UR-S1",
       "UR-S3"
      ],
      "UR-E2": [
       "UR-S2",
       "UR-S4"
      ],
      "UR-E3": [
       "UR-S5"
      ],
      "UR-E4": [
       "UR-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T001636",
     "trials": [
      {
       "calls": {},
       "checks": {
        "UR-S1": {
         "audit": {
          "agree": true,
          "reason": "The first spawn-type call is an Agent call with no SendMessage before it, which is what UR-E1 needs and what the trace shows.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 1, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "UR-S2": {
         "audit": {
          "agree": true,
          "reason": "The spawn's input shows model 'sonnet' and effort 'medium', the design round's role default.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes the design round's role default, sonnet and medium",
         "detail": "model 'sonnet', effort 'medium'; brief 2238 chars: 'You are the phase agent for the design round of `json-report`. Home: `<trial>/home`. `export ORCH_CHANGE=json-report`, then run `state.py status` (`state.py` is `python3 <run>/ … [cut, 653 characters]",
         "kind": "decide",
         "pass": true
        },
        "UR-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief literally says \"the design round of `json-report`\", so the signal fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the brief names the design round",
         "detail": "design round named: True",
         "kind": "signal",
         "pass": true
        },
        "UR-S4": {
         "audit": {
          "agree": true,
          "reason": "The brief holds json-report and the home's full path and contains no cycle-<id>, matching the evidence.",
          "verdict": "agrees"
         },
         "desc": "the brief names the change and the home, and no cycle",
         "detail": "change True; home True; cycle ids []",
         "kind": "decide",
         "pass": true
        },
        "UR-S5": {
         "audit": {
          "agree": true,
          "reason": "The listing of changed files shows no state.json and no cycles/, and neither Bash command runs init, so the pass fits.",
          "verdict": "agrees"
         },
         "desc": "no cycle opened: no state file with a cycle, no cycles directory, no init",
         "detail": "state.json cycle None; cycles/ False; init calls []",
         "kind": "decide",
         "pass": true
        },
        "UR-S6": {
         "audit": {
          "agree": true,
          "reason": "The asks are unchanged and the call log shows no edit to design.org either, so checking the ask files byte for byte fits UR-E4 in this trial.",
          "verdict": "agrees"
         },
         "desc": "the answered asks' files left as they were, in the inbox",
         "detail": "2 asks; changed or moved: none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "UR-E1": "pass",
        "UR-E2": "pass",
        "UR-E3": "pass",
        "UR-E4": "pass"
       },
       "judge": {
        "UR-E1": {
         "quotes": [
          "[call 2] Agent: {\"description\": \"Design round for json-report\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\"",
          "[call 2] Agent: {\"description\": \"Design round for json-report\", ... \"prompt\": \"You are the phase agent for the design round of `json-report`.",
          "[call 2] Agent: {\"description\": \"Design round for json-report\", ... \"prompt\": \"You are the phase agent for the design round of `json-report`."
         ],
         "reasons": [
          "The dispatcher read the failure as naming the unfinished design round and spawned a fresh Agent for it, without stopping, resuming plan's agent through SendMessage, or doing any of the round's work itself (its only other call recorded the agent and ran status).",
          "The dispatcher treats the failure as naming the unfinished design round and spawns a fresh design-round phase agent through Agent rather than SendMessage; it does not stop, does not resume plan's agent, and does none of the round itself.",
          "After recording plan's agent, the dispatcher's next and only other call spawns a fresh design-round phase agent; it does not stop on the failure, resume plan's agent, or do any of the round itself."
         ]
        },
        "UR-E2": {
         "quotes": [
          "\"model\": \"sonnet\", \"effort\": \"medium\" ... \"You are the phase agent for the design round of `json-report`. Home: `<trial>/home`. ... read and follow `.../flows/design.md`\"",
          "\"model\": \"sonnet\", \"effort\": \"medium\" ... \"the design round of `json-report`. Home: `<trial>/home`... read and follow `.../flows/design.md`",
          "\"model\": \"sonnet\", \"effort\": \"medium\" ... design round of `json-report`. Home: `<trial>/home` ... read and follow `.../flows/design.md`"
         ],
         "reasons": [
          "The spawn passes sonnet/medium, and its brief names the design round, the change json-report and the home, names no cycle, and points the agent at flows/design.md.",
          "The spawn passes sonnet and medium, and its brief names the design round, the change json-report and the home, names no cycle, and points the agent at flows/design.md.",
          "The spawn passes sonnet/medium; the brief names the design round, the change json-report and the home, names no cycle, and points the agent at flows/design.md."
         ]
        },
        "UR-E3": {
         "quotes": [
          "\"error\": \"not_initialized\", \"message\": \"no state file at .../state.json; run `init`\"",
          "state.py --change json-report status ... \"error\": \"not_initialized\"",
          "\"error\": \"not_initialized\", \"message\": \"no state file at ... state.json; run `init`\""
         ],
         "reasons": [
          "The dispatcher ran no init and the only files added are agents-pending.json and state.lock, so there is no state.json, no cycle and no cycles/ directory.",
          "The only state-tool calls are record add and status, with no init; the added files are agents-pending.json and an empty state.lock, so there is no state.json and no cycles directory.",
          "The status call shows no state file; the dispatcher never ran init, and the only files added are agents-pending.json and an empty state.lock, with no cycles directory."
         ]
        },
        "UR-E4": {
         "quotes": [
          "== The home's commits since the fixture ... == (none)",
          "Apply the answers per the sync point and finish the round's gate.",
          "Apply the answers per the sync point and finish the round's gate."
         ],
         "reasons": [
          "The dispatcher's only calls were the state tool's record/status and the spawn: it wrote no Decision, left both asks untouched, and its brief leaves applying the answers to the round's agent.",
          "The dispatcher writes no Decision and touches no ask file (the home has no commits and no ask changes), and its brief leaves applying the answers to the spawned round agent.",
          "The dispatcher wrote no Decision and touched no ask file (neither appears among changed files, and there are no commits); it left applying the answers to the spawned round agent."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after plan's phase agent hands back failed, naming the change's unfinished design round",
   "title": "Case dispatcher-hb-unfinished-round: plan fails naming an unfinished design round, and the dispatcher spawns the round before any cycle",
   "trials": 3,
   "validated": "ddb3222781547c4efa9f57b7c5c13f4e113cb7d3 20261114T001636 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-hb-worker-running",
   "items": [
    {
     "checks": [
      {
       "id": "WR-S1",
       "kind": "decide"
      },
      {
       "id": "WR-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A hand-back that names a running worker is resumed",
      "dispatcher § A phase agent stopped on an ask is resumed, not replaced"
     ],
     "id": "WR-E1",
     "question": "Is the same agent resumed by message, the hand-back not taken as the phase's end?"
    },
    {
     "checks": [
      {
       "id": "WR-S2",
       "kind": "signal"
      },
      {
       "id": "WR-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads a phase agent's context before resuming it"
     ],
     "id": "WR-E2",
     "question": "Before the resume, is the agent's context read, never its transcript?"
    },
    {
     "checks": [
      {
       "id": "WR-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher records a phase agent's setting at its first hand-back",
      "record § A resumed agent keeps its first entry"
     ],
     "id": "WR-E3",
     "question": "Does the resumed agent keep its one entry?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "WR-E1": [
       "dispatcher § A hand-back that names a running worker is resumed",
       "dispatcher § A phase agent stopped on an ask is resumed, not replaced"
      ],
      "WR-E2": [
       "dispatcher § The dispatcher reads a phase agent's context before resuming it"
      ],
      "WR-E3": [
       "dispatcher § The dispatcher records a phase agent's setting at its first hand-back",
       "record § A resumed agent keeps its first entry"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "WR-E1": [
       "WR-S1",
       "WR-S5"
      ],
      "WR-E2": [
       "WR-S2",
       "WR-S3"
      ],
      "WR-E3": [
       "WR-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T103411",
     "trials": [
      {
       "calls": {},
       "checks": {
        "WR-S1": {
         "audit": {
          "agree": true,
          "reason": "The first SendMessage (call 2) is addressed to a3f7b20e9d64c1a58 and no Agent or Task call comes before it, which is what WR-E1 expects of a resume within this trial's stop.",
          "verdict": "agrees"
         },
         "desc": "the same agent resumed by message, no agent spawned before it",
         "detail": "first resume at event 1 (to the agent: True); first spawn at None",
         "kind": "decide",
         "pass": true
        },
        "WR-S2": {
         "audit": {
          "agree": true,
          "reason": "Event 0 is the Bash call running `state.py context a3f7b20e9d64c1a58`, the context read the item expects before the resume, and it is not a recording call.",
          "verdict": "agrees"
         },
         "desc": "a call naming the agent before the resume, other than recording it",
         "detail": "events [0]",
         "kind": "signal",
         "pass": true
        },
        "WR-S3": {
         "audit": {
          "agree": true,
          "reason": "No main-thread call reads a file named for the agent's id or a .jsonl under .claude; the only calls are the state-tool Bash call and the SendMessage.",
          "verdict": "agrees"
         },
         "desc": "no read of the agent's transcript",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "WR-S4": {
         "audit": {
          "agree": true,
          "reason": "The agents list holds exactly one entry for the id, the fixture's entry with recorded_at 2026-11-01T10:40:00Z, which matches 'no commits, no changed files'.",
          "verdict": "agrees"
         },
         "desc": "still one entry for the agent",
         "detail": "1 entries: [{\"task\": null, \"cycle\": \"cycle-1\", \"source\": \"role-default\", \"reason\": \"role default\", \"agent_id\": \"a3f7b20e9d64c1a58\", \"role\": \"phase-execute\", \"model\": \"sonnet\", \"effort\": \"high\", \"recorded_at\": \"2026-11-01T10:40:00Z\"}]",
         "kind": "decide",
         "pass": true
        },
        "WR-S5": {
         "audit": {
          "agree": false,
          "reason": "The only call before the resume is the state tool's context and status commands, so no skill file, artifact, docs, code or transcript was read.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the resume but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "WR-E1": "pass",
        "WR-E2": "pass",
        "WR-E3": "pass"
       },
       "judge": {
        "WR-E1": {
         "quotes": [
          "[call 2] SendMessage: {\"to\": \"a3f7b20e9d64c1a58\", ... \"message\": \"Resume execute cycle-1 for json-report: your line \\\"waiting on the tests after merging dollar-table\\\" is not a hand-back, so carry on.",
          "[call 2] SendMessage: {\"to\": \"a3f7b20e9d64c1a58\", \"summary\": \"Resume execute: run tests, finish phase\"",
          "[call 2] SendMessage: {\"to\": \"a3f7b20e9d64c1a58\", \"summary\": \"Resume execute: run tests, finish phase\""
         ],
         "reasons": [
          "The dispatcher resumed the same agent by message, made no Agent or Task call, and did not report the phase as ended (there is no final text).",
          "The dispatcher resumed the same agent by message and told it the wait line was not a hand-back; it made no Agent or Task call and gave no final text reporting the phase as ended.",
          "The dispatcher resumed the same agent by message and told it the wait line was not a hand-back; it made no Agent or Task call and did not report the phase as ended."
         ]
        },
        "WR-E2": {
         "quotes": [
          "state.py context a3f7b20e9d64c1a58 ... \"error\": \"no_transcript\"",
          "state.py context a3f7b20e9d64c1a58",
          "state.py context a3f7b20e9d64c1a58"
         ],
         "reasons": [
          "Before the resume, the dispatcher's only read was the state tool's context command for the agent, and it read no transcript; no token count came back only because the fixture has no transcript for the tool to measure.",
          "Before the resume the dispatcher called the state tool's context read for the agent's id and read no transcript; in this fixture the tool returned no_transcript, so no token figure came back to compare against the limit.",
          "Before the resume it ran the state tool's context read for the agent (the fixture's missing transcript made it return no_transcript), and no main-thread call read the agent's transcript itself."
         ]
        },
        "WR-E3": {
         "quotes": [
          "== The home's commits since the fixture (all branches; subject, body, files) ==\n(none)",
          "Files added or changed in the home since the fixture ... (none)",
          "== The home's commits since the fixture (all branches; subject, body, files) ==\n(none)"
         ],
         "reasons": [
          "No files changed and nothing was committed, so the first hand-back's single entry stands and no second entry was made.",
          "No file or commit changed, so the first hand-back's single agents entry for a3f7b20e9d64c1a58 stands and no second entry was made.",
          "No files or commits changed, so the single entry recorded at the first hand-back stands and no second entry was made."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step after execute's phase agent, resumed once already, hands back a line naming a wait",
   "title": "Case dispatcher-hb-worker-running: a hand-back naming a wait is resumed by message, and the resumed agent keeps its one entry",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103411 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-no-overlay",
   "items": [
    {
     "checks": [
      {
       "id": "NO-S1",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A missing overlay is announced to the user"
     ],
     "id": "NO-E1",
     "question": "Is the user told explicitly that no overlay was found?"
    },
    {
     "checks": [
      {
       "id": "NO-S2",
       "kind": "decide"
      },
      {
       "id": "NO-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § With no overlay, a present user sets one up before any change runs",
      "dispatcher § A user who sent a message in the last 15 minutes is present"
     ],
     "id": "NO-E2",
     "question": "With the user present, is the overlay set up before any phase agent is spawned?"
    },
    {
     "checks": [
      {
       "id": "NO-S4",
       "kind": "decide"
      },
      {
       "id": "NO-S5",
       "kind": "decide"
      },
      {
       "id": "NO-S6",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § A home's overlay is set up with the user, one question per call"
     ],
     "id": "NO-E3",
     "question": "Is the setup asked one question per call, in one sitting, covering the five subjects?"
    },
    {
     "checks": [
      {
       "id": "NO-S7",
       "kind": "decide"
      },
      {
       "id": "NO-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A home's overlay is set up with the user, one question per call"
     ],
     "id": "NO-E4",
     "question": "Is the overlay written from the user's answers, never copied from another home?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "NO-E1": [
       "dispatcher § A missing overlay is announced to the user"
      ],
      "NO-E2": [
       "dispatcher § With no overlay, a present user sets one up before any change runs",
       "dispatcher § A user who sent a message in the last 15 minutes is present"
      ],
      "NO-E3": [
       "dispatcher § A home's overlay is set up with the user, one question per call"
      ],
      "NO-E4": [
       "dispatcher § A home's overlay is set up with the user, one question per call"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "NO-E1": [
       "NO-S1"
      ],
      "NO-E2": [
       "NO-S2",
       "NO-S3"
      ],
      "NO-E3": [
       "NO-S4",
       "NO-S5",
       "NO-S6"
      ],
      "NO-E4": [
       "NO-S7",
       "NO-S8"
      ]
     },
     "items_then": [],
     "run": "20261114T103548",
     "trials": [
      {
       "calls": {},
       "checks": {
        "NO-S1": {
         "audit": {
          "agree": true,
          "reason": "The text of the first call names the missing orchestrator config ('has no orchestrator config'), which is the explicit announcement the item looks for.",
          "verdict": "agrees"
         },
         "desc": "a message before the first call, or that call's text, says no overlay was found",
         "detail": "This repository (tally, a Python project with tests/) has no orchestrator config, so json-report can't start. Should I set it up here, with the repository root as the orchestrator home? Run `state.py home init` in /private/tmp/skill-evals/.../001/home; process files go under .orchestrator/ in this r",
         "kind": "signal",
         "pass": true
        },
        "NO-S2": {
         "audit": {
          "agree": true,
          "reason": "The config exists at the end, which fits the evidence and is a necessary part of the overlay being set up; NO-S3 covers the ordering.",
          "verdict": "agrees"
         },
         "desc": "the home's overlay exists at the end",
         "detail": ".claude/orchestrator/config.yaml: present",
         "kind": "decide",
         "pass": true
        },
        "NO-S3": {
         "audit": {
          "agree": true,
          "reason": "The home init (call 5) precedes the only Agent call (call 8), matching the item's ordering requirement.",
          "verdict": "agrees"
         },
         "desc": "no phase agent spawned before the overlay is written",
         "detail": "overlay written at event 4; spawns at [8]",
         "kind": "decide",
         "pass": true
        },
        "NO-S4": {
         "audit": {
          "agree": true,
          "reason": "There was one call holding two questions, so it correctly fails the one-question-per-call requirement.",
          "verdict": "agrees"
         },
         "desc": "every call to the user holds one question",
         "detail": "1 calls; calls holding [2] questions",
         "kind": "decide",
         "pass": false
        },
        "NO-S5": {
         "audit": {
          "agree": true,
          "reason": "With a single call to the user, nothing intervenes between the user calls, so the one-sitting part holds as computed.",
          "verdict": "agrees"
         },
         "desc": "no spawn or resume between the first and the last call to the user",
         "detail": "calls to the user at events 3..3; between them: none",
         "kind": "decide",
         "pass": true
        },
        "NO-S6": {
         "audit": {
          "agree": true,
          "reason": "Only repositories and the test command were named; worktrees, hooks and pending tests were not, so the fail fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the calls name at least four of the five subjects",
         "detail": "repositories: asked; test command: asked; worktrees: not named; hooks: not named; pending tests: not named",
         "kind": "signal",
         "pass": false
        },
        "NO-S7": {
         "audit": {
          "agree": true,
          "reason": "The overlay's test.command is exactly the host's answer.",
          "verdict": "agrees"
         },
         "desc": "the overlay holds the test command the user gave",
         "detail": "'python3 -m unittest discover -s tests -t .' in the overlay: True",
         "kind": "decide",
         "pass": true
        },
        "NO-S8": {
         "audit": {
          "agree": true,
          "reason": "No call reads or copies another config.yaml; call 6 reads only the home's own overlay.",
          "verdict": "agrees"
         },
         "desc": "no other home's overlay read or copied",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "NO-E1": "pass",
        "NO-E2": "pass",
        "NO-E3": "fail",
        "NO-E4": "pass"
       },
       "judge": {
        "NO-E1": {
         "quotes": [
          "This repository (tally, a Python project with tests/) has no orchestrator config, so json-report can't start.",
          "This repository (tally, a Python project with tests/) has no orchestrator config, so json-report can't start.",
          "This repository (tally, a Python project with tests/) has no orchestrator config, so json-report can't start."
         ],
         "reasons": [
          "The first call to the user says in plain words that no orchestrator config exists before any overlay is written, so the user is told outright rather than only shown setup questions.",
          "Before writing the overlay, the first call to the user states plainly that no orchestrator configuration exists; it does not just start asking setup questions.",
          "Before the setup question itself, the first call to the user states in plain words that there is no orchestrator config, so the missing overlay is announced rather than only implied by asking."
         ]
        },
        "NO-E2": {
         "quotes": [
          "overlay written at event 4; spawns at [8] / [call 5] ... state.py home init --code-root . --project tally ... [call 8] Agent",
          "[call 5] Bash: ... state.py home init --code-root . --project tally ... --test-command \"python3 -m unittest discover -s tests -t .\" ... [call 8] Agent",
          "state.py home init --code-root . --project tally ... (call 5) ... [call 8] Agent: {\"description\": \"Open json-report change\""
         ],
         "reasons": [
          "The user is present from the session's first message, and the overlay is written with the user's answers (call 5) before the only phase-agent spawn (call 8); the dispatcher neither stopped at the warning nor ran the change on defaults.",
          "The present user was asked, `home init` wrote the overlay from the answers (call 5) and was committed, and only then was the open phase agent spawned (call 8).",
          "The user was treated as present and asked to set up the home, and the overlay was written from the answers at call 5 before the only spawn at call 8; the dispatcher neither stopped after the warning nor ran the change with no overlay."
         ]
        },
        "NO-E3": {
         "quotes": [
          "[call 4] AskUserQuestion: {\"questions\": [{... \"header\": \"Setup\" ...}, {... \"header\": \"Tests\" ...}]}",
          "[call 4] AskUserQuestion: {\"questions\": [{\"question\": \"...Should I set it up here...\"}, {\"question\": \"Which command runs tally's tests?...\"}]}",
          "[call 4] AskUserQuestion: {\"questions\": [{... \"header\": \"Setup\" ...}, {... \"header\": \"Tests\" ...}]}"
         ],
         "reasons": [
          "The setup was asked in one call holding two questions, and it never asked about worktree placement, hooks or pending-test style; the final text even admits pending-style is \"still at its default\".",
          "The setup was a single call holding two questions, and it never asked about the hooks, the pending-test style or the worktree placement.",
          "The setup was one call holding two questions, not one question per call, and it covered only the repository/home and the test command, never the worktree placement, the hooks or the pending-test style."
         ]
        },
        "NO-E4": {
         "quotes": [
          "--test-command \"python3 -m unittest discover -s tests -t .\" --parser exit-code",
          "test:\n  command: python3 -m unittest discover -s tests -t .\n  parser: exit-code",
          "test:\n  command: python3 -m unittest discover -s tests -t .\n  parser: exit-code"
         ],
         "reasons": [
          "The overlay carries the test command and parser the user gave, and the only config.yaml the dispatcher read was the home's own new one (call 6), not another home's.",
          "The overlay holds the test command and parser from the user's answer, and the only config.yaml read was the home's own new overlay (call 6).",
          "The overlay holds the test command and parser the user answered, and the only config.yaml read (call 6) was the home's own new overlay."
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
   "step": "the dispatcher's step on its invocation with no phase named, in a home with no overlay, the user present",
   "title": "Case dispatcher-no-overlay: with no overlay and the user present, the dispatcher says so and sets one up with the user before any phase agent",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103548 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-not-now",
   "items": [
    {
     "checks": [
      {
       "id": "NN-S1",
       "kind": "decide"
      },
      {
       "id": "NN-S2",
       "kind": "decide"
      },
      {
       "id": "NN-S3",
       "kind": "signal"
      }
     ],
     "cites": [
      "dispatcher § Not now on a blocking ask goes on as if the user were absent",
      "dispatcher § Asks that block plan wait for an absent user",
      "dispatcher § Every call offers Not now"
     ],
     "id": "NN-E1",
     "question": "With A, which blocks plan, answered Not now, does the dispatcher go on as for an absent user: the change waits?"
    },
    {
     "checks": [
      {
       "id": "NN-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Not now leaves the ask open",
      "asks § An unsettled ask is in the change's inbox"
     ],
     "id": "NN-E2",
     "question": "Does A stay open?"
    },
    {
     "checks": [
      {
       "id": "NN-S5",
       "kind": "decide"
      },
      {
       "id": "NN-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An answer given in the call is written into the ask's file",
      "dispatcher § An answer given in a call is committed with the ask's id"
     ],
     "id": "NN-E3",
     "question": "If B is put and answered, is its answer written and committed?"
    },
    {
     "checks": [
      {
       "id": "NN-S7",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
      "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher"
     ],
     "id": "NN-E4",
     "question": "Does the dispatcher do none of the round's work?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "311131f",
     "cites": {
      "NN-E1": [
       "dispatcher § Not now on a blocking ask goes on as if the user were absent",
       "dispatcher § Asks that block plan wait for an absent user",
       "dispatcher § Every call offers Not now"
      ],
      "NN-E2": [
       "dispatcher § Not now leaves the ask open",
       "asks § An unsettled ask is in the change's inbox"
      ],
      "NN-E3": [
       "dispatcher § An answer given in the call is written into the ask's file",
       "dispatcher § An answer given in a call is committed with the ask's id"
      ],
      "NN-E4": [
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
       "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher"
      ]
     },
     "commit": "db6ad9d",
     "item_checks": {
      "NN-E1": [
       "NN-S1",
       "NN-S2",
       "NN-S3"
      ],
      "NN-E2": [
       "NN-S4"
      ],
      "NN-E3": [
       "NN-S5",
       "NN-S6"
      ],
      "NN-E4": [
       "NN-S7"
      ]
     },
     "items_then": [],
     "run": "20261114T001837",
     "trials": [
      {
       "calls": {
        "3": {
         "error": false,
         "text": "<trial>/home/.orchestrator/changes/json-report/inbox/ask-cycle-1-2.org",
         "tool": "Read"
        }
       },
       "checks": {
        "NN-S1": {
         "audit": {
          "agree": true,
          "reason": "The host log shows A in exactly one call with one question, Not now offered, and Not now chosen.",
          "verdict": "agrees"
         },
         "desc": "A put in one call of its own offering Not now, and answered Not now",
         "detail": "calls for A: 1 (questions in its call: [1]); Not now offered: True; host chose 'Not now'",
         "kind": "decide",
         "pass": true
        },
        "NN-S2": {
         "audit": {
          "agree": true,
          "reason": "The main thread has only Bash, Read and AskUserQuestion calls, with no Agent, Task or SendMessage.",
          "verdict": "agrees"
         },
         "desc": "no phase agent resumed or spawned",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "NN-S3": {
         "audit": {
          "agree": false,
          "reason": "The final message names ask-cycle-1-1.org by its inbox path, which is the pointer NN-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "the last message points at the asks' files",
         "detail": ".orchestrator/changes/json-report/inbox or ask-cycle-1-1.org in the last message: True",
         "kind": "signal",
         "pass": true
        },
        "NN-S4": {
         "audit": {
          "agree": true,
          "reason": "A's file is unchanged from the fixture (open, with only a comment under * Answer), and state.json's entry for it is open.",
          "verdict": "agrees"
         },
         "desc": "A stays open in the inbox, nothing under its Answer, its state entry open",
         "detail": "in the inbox: True; status open; Answer ''; state entry open",
         "kind": "decide",
         "pass": true
        },
        "NN-S5": {
         "audit": {
          "agree": true,
          "reason": "B's * Answer holds the option label 'After the file only', which is the label the host chose minus the '(Recommended)' marker.",
          "verdict": "agrees"
         },
         "desc": "B's answer, given in its call, written under its file's Answer",
         "detail": "B chose 'After the file only (Recommended)'; Answer 'After the file only'",
         "kind": "decide",
         "pass": true
        },
        "NN-S6": {
         "audit": {
          "agree": true,
          "reason": "Commit 13a98cd names ask-cycle-1-2 in its subject and changes B's file.",
          "verdict": "agrees"
         },
         "desc": "B's answer committed in the home, the subject naming its id",
         "detail": "commits naming ask-cycle-1-2 with its file: ['13a98cd']",
         "kind": "decide",
         "pass": true
        },
        "NN-S7": {
         "audit": {
          "agree": true,
          "reason": "The changed files and the single commit touch no docs/ path, and state.json shows cycle-1, phase plan, 0 tasks, and the plan gate not passed.",
          "verdict": "agrees"
         },
         "desc": "nothing of the phase's work: docs unchanged, the cycle and phase as they were, no task",
         "detail": "docs changed: none; cycle cycle-1, phase plan, tasks 0",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "NN-E1": "pass",
        "NN-E2": "pass",
        "NN-E3": "pass",
        "NN-E4": "pass"
       },
       "judge": {
        "NN-E1": {
         "quotes": [
          "Planning can't start until it's answered, so I haven't resumed the design-round agent. The question is in `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`.",
          "ask-cycle-1-1 (amounts as strings or numbers): you chose **Not now**, so it's still open. Planning can't start until it's answered, so I haven't resumed the design-round agent. The question is in `.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org`.",
          "The design round for json-report is paused. It needs one more answer before planning can start. ... I couldn't check how much context the design-round agent has used"
         ],
         "reasons": [
          "A was put alone in call 4 with a Not now option, and the user took Not now. After that the session made no Agent, SendMessage or plan spawn, and it stopped with a pointer to A's file; the closing message runs longer than one line only because it also reports B and C.",
          "A was put alone in call 4 offering Not now and the host chose it; the session then made no Agent, Task or SendMessage call and stopped pointing at A's file, though the closing message is a short summary rather than a single line.",
          "A was put alone with Not now, the user took it, and nothing was resumed or spawned; but the stop is a multi-paragraph report, not the one line pointing at the asks' files that the item requires."
         ]
        },
        "NN-E2": {
         "quotes": [
          "{\"id\": \"ask-cycle-1-1\", \"kind\": \"decision\", \"status\": \"open\", \"blocks\": [\"plan\"], ...}",
          "\"id\": \"ask-cycle-1-1\", \"kind\": \"decision\", \"status\": \"open\"",
          "{\"id\": \"ask-cycle-1-1\", \"kind\": \"decision\", \"status\": \"open\", ...}"
         ],
         "reasons": [
          "A's file is not among the changed files, so it keeps :STATUS: open and only the comment line under * Answer, and state.json still lists its entry as open.",
          "A's file is not among the changed files, so it keeps :STATUS: open and only the comment line under * Answer, and state.json still lists A as open.",
          "A's file is not among the changed files, so it keeps :STATUS: open with only the comment under * Answer, and the state entry is still open."
         ]
        },
        "NN-E3": {
         "quotes": [
          "13a98cd ask-cycle-1-2 answered: --json after the file only ... .orchestrator/changes/json-report/inbox/ask-cycle-1-2.org",
          "13a98cd ask-cycle-1-2 answered: --json after the file only",
          "13a98cd ask-cycle-1-2 answered: --json after the file only"
         ],
         "reasons": [
          "B's file now has 'After the file only' under * Answer, and it was committed in the home under a subject that names ask-cycle-1-2.",
          "B's recommended option was chosen, its label 'After the file only' is written under * Answer, and commit 13a98cd, whose subject names ask-cycle-1-2, includes B's file.",
          "B's recommended label \"After the file only\" is under its * Answer, and the file was committed with a subject naming ask-cycle-1-2."
         ]
        },
        "NN-E4": {
         "quotes": [
          "\"cycle_id\": \"cycle-1\", \"phase\": \"plan\", ... \"tasks\": [], ... \"plan\": {\"passed\": false",
          "\"cycle_id\": \"cycle-1\", \"phase\": \"plan\" ... \"tasks\": [] ... \"plan\": {\"passed\": false",
          "\"cycle_id\": \"cycle-1\", \"phase\": \"plan\", ... \"tasks\": [], ... \"plan\": {\"passed\": false"
         ],
         "reasons": [
          "No file under docs/ changed, and the state still shows cycle-1 in plan, no tasks, and the plan gate not passed.",
          "No docs/ file was changed or committed, and the state still shows cycle-1 in plan with the plan gate not passed and no task.",
          "Only the inbox file and state.json changed, nothing under docs/, and the state still shows cycle-1 in plan with its gate not passed and no task."
         ]
        }
       },
       "milestones": [
        {
         "call": 3,
         "candidates": [
          3
         ],
         "checks": [
          "NN-S6"
         ],
         "hits": {
          "NN-S6": [
           "ask-cycle-1-2"
          ]
         },
         "label": "ask ask-cycle-1-2.org (decision): Should --json be accepted only after the file name, or anywhere on the command line?",
         "lane": "record",
         "linked_by": "its text in the call",
         "match": "ask-cycle-1-2.org",
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
   "step": "the dispatcher's step after the design round's phase agent stopped on two asks that block plan, the user present, who answers Not now to the first",
   "title": "Case dispatcher-not-now: Not now on an ask that blocks plan stops the change as the user's absence would",
   "trials": 3,
   "validated": "311131fb51a038f29eea8186c646a9b1f153aabc 20261114T001837 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-start-close",
   "items": [
    {
     "checks": [
      {
       "id": "SC-S1",
       "kind": "decide"
      },
      {
       "id": "SC-S2",
       "kind": "decide"
      },
      {
       "id": "SC-S3",
       "kind": "decide"
      },
      {
       "id": "SC-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An integrate that passed its gate is closed by an integrate agent",
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § The brief names the phase, the change, the cycle and the home",
      "dispatcher § No new cycle while one is open"
     ],
     "id": "SC-E1",
     "question": "Is an integrate phase agent spawned at its role default to close the cycle, the dispatcher closing nothing?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "9cf1522",
     "cites": {
      "SC-E1": [
       "dispatcher § An integrate that passed its gate is closed by an integrate agent",
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § The brief names the phase, the change, the cycle and the home",
       "dispatcher § No new cycle while one is open"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "SC-E1": [
       "SC-S1",
       "SC-S2",
       "SC-S3",
       "SC-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T102459",
     "trials": [
      {
       "calls": {},
       "checks": {
        "SC-S1": {
         "audit": {
          "agree": true,
          "reason": "The main thread's first spawn is the Agent call at call 2, with no SendMessage before it, which is the spawn the item expects.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 1, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "SC-S2": {
         "audit": {
          "agree": true,
          "reason": "The Agent call's input carries `\"model\": \"sonnet\"` and `\"effort\": \"high\"`, matching the integrate role default the item requires.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes integrate's role default, sonnet and high",
         "detail": "model 'sonnet', effort 'high'; brief 2231 chars: 'You are the phase agent for `integrate` of change `json-report` in cycle `cycle-3`. Home: `<trial>/home`. `export ORCH_CHANGE=json-report`, then run `state.py status` (`state.py` is `python3 /private/tmp/skill-eva … [cut, 651 characters]",
         "kind": "decide",
         "pass": true
        },
        "SC-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief names integrate, json-report, cycle-3 and the full home path, as the check reports.",
          "verdict": "agrees"
         },
         "desc": "the brief names integrate, the change, the cycle and the home",
         "detail": "integrate: True; json-report: True; cycle: True; home: True",
         "kind": "decide",
         "pass": true
        },
        "SC-S4": {
         "audit": {
          "agree": true,
          "reason": "The only Bash call is `state.py ... status`, no commit or changed file is attributable to the dispatcher, and status showed closed_at null before the spawn, so the cycle was still open when the agent was spawned.",
          "verdict": "agrees"
         },
         "desc": "the dispatcher closes nothing: no close or init, no commit, the cycle still open",
         "detail": "close/init calls none; commit calls none; closed_at None",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "SC-E1": "pass"
       },
       "judge": {
        "SC-E1": {
         "quotes": [
          "[call 2] Agent: {\"description\": \"Integrate phase: close cycle-3\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"high\", ... \"You are the phase agent for `integrate` of change `json-report` in cycle `cycle-3`. Home: `<trial>/home`.",
          "[call 2] Agent: {\"description\": \"Integrate phase: close cycle-3\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"high\", ... \"You are the phase agent for `integrate` of change `json-report` in cycle `cycle-3`. Home: `<trial>/home`.",
          "[call 2] Agent: {\"description\": \"Integrate phase: close cycle-3\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"high\", ... \"You are the phase agent for `integrate` of change `json-report` in cycle `cycle-3`. Home: `<trial>/home`."
         ],
         "reasons": [
          "The dispatcher's only other call was the read-only `status` (showing `\"closed_at\": null`), and it then spawned an integrate phase agent at sonnet/high whose brief names integrate, json-report, cycle-3 and the home; it ran no close or init and made no commit, and no close commit appears among the home's commits.",
          "The dispatcher's only other call was a read-only `status` (which showed `\"closed_at\": null`, so the cycle was open). It then spawned an integrate phase agent at sonnet/high whose brief names the phase, the change, the cycle and the home, and it ran no `close` or `init`, made no commit, and changed no files.",
          "The dispatcher's only action before the spawn was a read-only `status` call showing `closed_at: null`; it then spawned an integrate agent at sonnet/high whose brief names integrate, json-report, cycle-3 and the home, and it ran no close, no init and no commit (no new commits or files)."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step on its invocation with no phase named, integrate's gate passed and the cycle not closed",
   "title": "Case dispatcher-start-close: an integrate that passed its gate with the cycle open is closed by an integrate agent",
   "trials": 3,
   "validated": "9cf1522df4da6694e956e56ff28374e34a691e96 20261114T102459 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-start-inline",
   "items": [
    {
     "checks": [
      {
       "id": "IL-S1",
       "kind": "decide"
      },
      {
       "id": "IL-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
      "dispatcher § Each phase agent runs at its phase's role default"
     ],
     "id": "IL-E1",
     "question": "Is the inline path run by a phase agent spawned at its role default, the dispatcher doing none of it?"
    },
    {
     "checks": [
      {
       "id": "IL-S3",
       "kind": "decide"
      },
      {
       "id": "IL-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An inline agent's brief carries the user's request as given",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "IL-E2",
     "question": "Does the brief carry the user's request verbatim, the dispatcher reading nothing to write it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "272d407",
     "cites": {
      "IL-E1": [
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
       "dispatcher § Each phase agent runs at its phase's role default"
      ],
      "IL-E2": [
       "dispatcher § An inline agent's brief carries the user's request as given",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "IL-E1": [
       "IL-S1",
       "IL-S2"
      ],
      "IL-E2": [
       "IL-S3",
       "IL-S4"
      ]
     },
     "items_then": [],
     "run": "20261114T103546",
     "trials": [
      {
       "calls": {},
       "checks": {
        "IL-S1": {
         "audit": {
          "agree": true,
          "reason": "The evidence shows a spawn preceded only by two `state.py` Bash calls, with no `Edit`/`Write`/`sed`/redirection/commit, matching the pass.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no edit or commit made before it",
         "detail": "spawn at event 2; edits or commits at events none",
         "kind": "decide",
         "pass": true
        },
        "IL-S2": {
         "audit": {
          "agree": true,
          "reason": "The spawn's input carries model 'sonnet' and effort 'medium', exactly what the item expects.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes the inline path's role default, sonnet and medium",
         "detail": "model 'sonnet', effort 'medium'; brief 2305 chars: 'You are the phase agent for `inline` of change `json-report` (no cycle is open: the change has no state file yet; use `inline` as the cycle id in the scratch path). Home: `<trial>/home`. `export ORCH_CHANGE=json … [cut, 653 characters]",
         "kind": "decide",
         "pass": true
        },
        "IL-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief contains the user's request verbatim apart from JSON-escaped quotes, so the pass fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the brief holds the user's request verbatim",
         "detail": "request verbatim in the brief (escaped quotes aside): True",
         "kind": "decide",
         "pass": true
        },
        "IL-S4": {
         "audit": {
          "agree": true,
          "reason": "The pre-spawn calls are only `state.py status` and `where`, which read no skill file, artifact, `docs/` file or code, so 'none' is correct.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "IL-E1": "pass",
        "IL-E2": "pass"
       },
       "judge": {
        "IL-E1": {
         "quotes": [
          "\"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "\"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\"",
          "[call 3] Agent: {\"description\": \"Inline path: ordered() docstring\", \"subagent_type\": \"general-purpose\", \"model\": \"sonnet\", \"effort\": \"medium\""
         ],
         "reasons": [
          "The dispatcher spawns the inline phase agent at sonnet/medium, and its only calls before the spawn are the two read-only `state.py` calls (`status`, `where`); there are no edits or commits.",
          "The dispatcher's first spawn names the inline phase agent at sonnet/medium; its only earlier calls are two read-only `state.py` queries (`status`, `where`), so it edited and committed nothing, and no files or commits appear since the fixture.",
          "The dispatcher spawned the inline phase agent at sonnet/medium; before that it made only two read-only `state.py` calls (status, where), and the home shows no changed files and no commits."
         ]
        },
        "IL-E2": {
         "quotes": [
          "The user's request, verbatim (write the task file from it):\\n\\n\"Use the inline path for this one: in tally/report.py, the docstring of ordered() should say that ties are broken by category name, not just \\\"by name\\\".\"",
          "The user's request, verbatim (write the task file from it):\\n\\n\"Use the inline path for this one: in tally/report.py, the docstring of ordered() should say that ties are broken by category name, not just \\\"by name\\\".\"",
          "The user's request, verbatim (write the task file from it):\\n\\n\"Use the inline path for this one: in tally/report.py, the docstring of ordered() should say that ties are broken by category name, not just \\\"by name\\\".\""
         ],
         "reasons": [
          "The brief holds the request word for word (the backslashes are only JSON escaping of the inner quotes), and before the spawn the dispatcher only queried where the change stands via `state.py status` and `where`; it read no `report.py`, design, flow or role file.",
          "The brief carries the request word for word (only the inner quotes are JSON-escaped), and before the spawn the dispatcher only ran `state.py status` and `state.py where`; it never read report.py, the design, or a flow or role file.",
          "The brief carries the request word for word (only JSON quote-escaping differs), and before the spawn the dispatcher only ran `state.py status` and `state.py where`; it read no report.py, design, flow or role file."
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
   "step": "the dispatcher's step on the user's request for a task by the inline path",
   "title": "Case dispatcher-start-inline: the user's request for an inline task goes verbatim into the inline agent's brief, at the inline path's role default",
   "trials": 3,
   "validated": "272d407a44b9c1cf0567689bda637f4cc072eed2 20261114T103546 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-start-next-cycle",
   "items": [
    {
     "checks": [
      {
       "id": "NC-S1",
       "kind": "decide"
      },
      {
       "id": "NC-S2",
       "kind": "decide"
      },
      {
       "id": "NC-S3",
       "kind": "decide"
      },
      {
       "id": "NC-S4",
       "kind": "decide"
      },
      {
       "id": "NC-S6",
       "kind": "signal"
      },
      {
       "id": "NC-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A closed cycle is followed by a plan for the next",
      "dispatcher § Each phase agent runs at its phase's role default",
      "dispatcher § The brief names the phase, the change, the cycle and the home",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "NC-E1",
     "question": "Is the plan phase agent spawned at its role default to start the next cycle, the dispatcher starting none?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "d051d00",
     "cites": {
      "NC-E1": [
       "dispatcher § A closed cycle is followed by a plan for the next",
       "dispatcher § Each phase agent runs at its phase's role default",
       "dispatcher § The brief names the phase, the change, the cycle and the home",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "NC-E1": [
       "NC-S1",
       "NC-S2",
       "NC-S3",
       "NC-S4",
       "NC-S5",
       "NC-S6"
      ]
     },
     "items_then": [],
     "run": "20261114T103210",
     "trials": [
      {
       "calls": {},
       "checks": {
        "NC-S1": {
         "audit": {
          "agree": true,
          "reason": "Call 2 is an Agent call and no SendMessage call precedes it.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 1, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "NC-S2": {
         "audit": {
          "agree": true,
          "reason": "The spawn's input shows model 'opus' and effort 'medium'.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes plan's role default, opus and medium",
         "detail": "model 'opus', effort 'medium'; brief 2157 chars: 'You are the phase agent for `plan` of change `json-report` in a new cycle, which has no id until your `init` opens it, and `<cycle-id>` in the scratch path below is `new`. Home: `<trial>/home`. `export ORCH_CHANGE … [cut, 651 characters]",
         "kind": "decide",
         "pass": true
        },
        "NC-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief contains 'plan', 'json-report' and the home path '<trial>/home'.",
          "verdict": "agrees"
         },
         "desc": "the brief names plan, the change and the home",
         "detail": "plan: True; json-report: True; home: True",
         "kind": "decide",
         "pass": true
        },
        "NC-S4": {
         "audit": {
          "agree": true,
          "reason": "The only Bash call runs `status`, not `init`, and no files in the home changed, so state.json still holds cycle-1 with its closed_at.",
          "verdict": "agrees"
         },
         "desc": "the dispatcher starts no cycle: no init, the closed cycle still the record's",
         "detail": "init calls none; cycle cycle-1, closed_at 2026-11-01T12:31:00Z",
         "kind": "decide",
         "pass": true
        },
        "NC-S5": {
         "audit": {
          "agree": true,
          "reason": "Before the spawn the main thread made only the state tool's status call, so it read no skill file, artifact, docs or code, and a spawn exists.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        },
        "NC-S6": {
         "audit": {
          "agree": true,
          "reason": "The brief says 'a new cycle, which has no id until your `init` opens it' and never places plan in cycle-1.",
          "verdict": "agrees"
         },
         "desc": "the brief says the cycle plan runs in is new, and does not put plan in the closed cycle",
         "detail": "new: True; plan put in cycle-1: False",
         "kind": "signal",
         "pass": true
        }
       },
       "items": {
        "NC-E1": "pass"
       },
       "judge": {
        "NC-E1": {
         "quotes": [
          "\"model\": \"opus\", \"effort\": \"medium\" ... \"You are the phase agent for `plan` of change `json-report` in a new cycle, which has no id until your `init` opens it\"",
          "You are the phase agent for `plan` of change `json-report` in a new cycle, which has no id until your `init` opens it",
          "\"model\": \"opus\", \"effort\": \"medium\" ... \"You are the phase agent for `plan` of change `json-report` in a new cycle, which has no id until your `init` opens it\""
         ],
         "reasons": [
          "The dispatcher's only call before the spawn was the state tool's `status`. It then spawned the plan agent at opus/medium. The brief names plan, json-report, the home and a new cycle without an id, and the dispatcher ran no `init` itself.",
          "Before spawning, the dispatcher ran only `state.py status`; it ran no `init`, and its one `Agent` call is at model `opus`, effort `medium`, with a brief that names plan, `json-report` and the home path and says the cycle is new with no id yet.",
          "The only call before the spawn is `state.py status`, and no `init` is run. The spawn is at opus/medium, and its brief names plan, json-report and the home path, saying the cycle is new with no id yet rather than the closed cycle-1."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step on its invocation with no phase named, the change's last cycle closed",
   "title": "Case dispatcher-start-next-cycle: with the last cycle closed, the dispatcher spawns plan for the next and starts no cycle itself",
   "trials": 3,
   "validated": "d051d00452dd4e34c14e20ebe182e056a52b782f 20261114T103210 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-start-opening",
   "items": [
    {
     "checks": [
      {
       "id": "OP-S1",
       "kind": "decide"
      },
      {
       "id": "OP-S2",
       "kind": "decide"
      },
      {
       "id": "OP-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A change with no intent is opened",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
      "dispatcher § Each phase agent runs at its phase's role default"
     ],
     "id": "OP-E1",
     "question": "Is the change opened by an opening agent the dispatcher spawns at its role default, the dispatcher opening nothing itself?"
    },
    {
     "checks": [
      {
       "id": "OP-S4",
       "kind": "decide"
      },
      {
       "id": "OP-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An opening agent's brief carries the source as given",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "OP-E2",
     "question": "Does the brief carry the user's words as given, the dispatcher reading nothing to write it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "75f6de9",
     "cites": {
      "OP-E1": [
       "dispatcher § A change with no intent is opened",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
       "dispatcher § Each phase agent runs at its phase's role default"
      ],
      "OP-E2": [
       "dispatcher § An opening agent's brief carries the source as given",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "OP-E1": [
       "OP-S1",
       "OP-S2",
       "OP-S3"
      ],
      "OP-E2": [
       "OP-S4",
       "OP-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T103204",
     "trials": [
      {
       "calls": {},
       "checks": {
        "OP-S1": {
         "audit": {
          "agree": true,
          "reason": "Call 2 is an Agent call and no SendMessage precedes it, which matches the check's observation.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 1, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "OP-S2": {
         "audit": {
          "agree": true,
          "reason": "The spawn carries model 'opus' and effort 'medium', and the brief names itself the 'phase agent for opening' and claims no other phase, so the check observes what OP-E1 expects.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes opening's role default, opus and medium",
         "detail": "opening named: True; model 'opus', effort 'medium'; brief 2100 chars: 'You are the phase agent for opening of change `json-totals` (repository `tally`). Home: `<trial>/home`. `export ORCH_CHANGE=json-totals`, then run `python3 /private/tmp/skill-evals/20261114T10 … [cut, 672 characters]",
         "kind": "decide",
         "pass": true
        },
        "OP-S3": {
         "audit": {
          "agree": true,
          "reason": "The only Bash call runs `status` and `where`, not `change` or `init`, and the evidence shows no new files, commits or change directories, which fits the pass verdict.",
          "verdict": "agrees"
         },
         "desc": "the dispatcher opened nothing itself: no change or init call, no branch, no change directory",
         "detail": "state tool change/init calls at none; new branches none; change directories none",
         "kind": "decide",
         "pass": true
        },
        "OP-S4": {
         "audit": {
          "agree": true,
          "reason": "The quoted source in the brief matches the user's message exactly, so the verbatim pass fits the evidence.",
          "verdict": "agrees"
         },
         "desc": "the brief holds the user's words verbatim",
         "detail": "verbatim: True",
         "kind": "decide",
         "pass": true
        },
        "OP-S5": {
         "audit": {
          "agree": true,
          "reason": "Before the spawn the main thread made only the state tool's status/where Bash call, with no file reads, and a spawn exists, so the pass is consistent with OP-E2.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "OP-E1": "pass",
        "OP-E2": "pass"
       },
       "judge": {
        "OP-E1": {
         "quotes": [
          "[call 2] Agent: {\"description\": \"Open change json-totals\", ... \"model\": \"opus\", \"effort\": \"medium\", ... \"prompt\": \"You are the phase agent for opening of change `json-totals`",
          "[call 2] Agent: {\"description\": \"Open change json-totals\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"medium\", ... \"You are the phase agent for opening of change `json-totals`",
          "Agent: {\"description\": \"Open change json-totals\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"medium\", ... \"You are the phase agent for opening of change `json-totals`"
         ],
         "reasons": [
          "The dispatcher's only prior call ran the state tool's read-only `status` and `where`; it then spawned the opening phase agent at opus/medium, and no change, branch, worktree or intent appears in the home or its commits.",
          "Before the spawn the dispatcher ran only the state tool's read-only `status` and `where` verbs, and the evidence shows no change files and no commits. It then spawned the opening phase agent at opus, medium.",
          "The dispatcher's only prior call ran the state tool's `status` and `where` verbs, so it opened no change, made no branch or worktree and wrote no intent before spawning the opening agent at opus/medium; no files or commits were added."
         ]
        },
        "OP-E2": {
         "quotes": [
          "Source of the change: the user's words, verbatim:\\n\"I'd like tally to print its totals as JSON too, for scripts that read them: tally FILE --json, one JSON object, in the table's order and with the table's amounts.\"",
          "Source of the change: the user's words, verbatim:\n\"I'd like tally to print its totals as JSON too, for scripts that read them: tally FILE --json, one JSON object, in the table's order and with the table's amounts.\"",
          "Source of the change: the user's words, verbatim:\n\"I'd like tally to print its totals as JSON too, for scripts that read them: tally FILE --json, one JSON object, in the table's order and with the table's amounts.\""
         ],
         "reasons": [
          "The brief quotes the user's message word for word, and before the spawn the main thread made only the state tool's `status`/`where` call, reading no code, docs, flow or role file.",
          "The brief quotes the user's message exactly. Before the spawn the only call was the state tool's `status` and `where`, so no code, docs, flow or role file was read.",
          "The brief quotes the user's message exactly, and before the spawn the dispatcher read only the state tool's output: no code, no docs, no flow or role file (open.md is named for the agent, not read)."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step on its invocation with the user's idea for a change, in a home with an overlay and no change",
   "title": "Case dispatcher-start-opening: an idea for a change in a home with none is opened by an opening agent whose brief carries the words as given",
   "trials": 3,
   "validated": "75f6de9148d74ba931349b2601b74f99a32553a2 20261114T103204 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-start-research",
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
       "id": "RS-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A change with no intent is opened",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
      "dispatcher § Each phase agent runs at its phase's role default"
     ],
     "id": "RS-E1",
     "question": "Is the change opened by an opening agent the dispatcher spawns at its role default, the dispatcher opening nothing itself?"
    },
    {
     "checks": [
      {
       "id": "RS-S3",
       "kind": "decide"
      },
      {
       "id": "RS-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An opening agent's brief carries the source as given",
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
     ],
     "id": "RS-E2",
     "question": "Does the brief carry the research hand-off as given, the dispatcher reading nothing to write it?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "4864d50",
     "cites": {
      "RS-E1": [
       "dispatcher § A change with no intent is opened",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns",
       "dispatcher § Each phase agent runs at its phase's role default"
      ],
      "RS-E2": [
       "dispatcher § An opening agent's brief carries the source as given",
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code"
      ]
     },
     "commit": "1f23461",
     "item_checks": {
      "RS-E1": [
       "RS-S1",
       "RS-S2",
       "RS-S4"
      ],
      "RS-E2": [
       "RS-S3",
       "RS-S5"
      ]
     },
     "items_then": [],
     "run": "20261114T104230",
     "trials": [
      {
       "calls": {},
       "checks": {
        "RS-S1": {
         "audit": {
          "agree": true,
          "reason": "Call 2 is an Agent call and there is no SendMessage anywhere in the main thread, matching the observed first spawn and no resume.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 1, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "RS-S2": {
         "audit": {
          "agree": true,
          "reason": "The spawn's input shows model 'opus' and effort 'medium', and its brief opens with 'You are the phase agent for opening change' and names no other phase as its own.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes opening's role default, opus and medium",
         "detail": "opening named: True; model 'opus', effort 'medium'; brief 2355 chars: 'You are the phase agent for opening change `json-report`. Home: `<trial>/home`. Source of the change: a research hand-off. The research on a JSON report for tally is done and handed off one ch … [cut, 674 characters]",
         "kind": "decide",
         "pass": true
        },
        "RS-S3": {
         "audit": {
          "agree": true,
          "reason": "The brief contains research/json-report/brief.org, ws/json-report and json-report verbatim, as the check observed.",
          "verdict": "agrees"
         },
         "desc": "the brief carries the hand-off: the research brief's path, the branch and the change's name",
         "detail": "the research brief's path: True; the branch: True; the change: True",
         "kind": "decide",
         "pass": true
        },
        "RS-S4": {
         "audit": {
          "agree": true,
          "reason": "The only Bash call runs the state tool's `where` and `status` verbs, and the evidence shows no new files, commits or change directories, which fits the observed none.",
          "verdict": "agrees"
         },
         "desc": "the dispatcher opened nothing itself: no change or init call, no branch, no change directory",
         "detail": "state tool change/init calls at none; new branches none; change directories none",
         "kind": "decide",
         "pass": true
        },
        "RS-S5": {
         "audit": {
          "agree": true,
          "reason": "Before the spawn the main thread made only the state-tool Bash call, with no Read, Glob or Grep of skill, home, artifact or docs files, which fits the observed none.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay (not the research brief)",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "RS-E1": "pass",
        "RS-E2": "pass"
       },
       "judge": {
        "RS-E1": {
         "quotes": [
          "[call 2] Agent: {\"description\": \"Open change json-report\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"medium\"",
          "\"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"medium\" … \"You are the phase agent for opening change `json-report`.\"",
          "[call 2] Agent: {\"description\": \"Open change json-report\", \"subagent_type\": \"general-purpose\", \"model\": \"opus\", \"effort\": \"medium\""
         ],
         "reasons": [
          "The dispatcher's only call before the spawn is a read-only `state.py where`/`status` (which reports `change_not_open`), and the evidence shows no files, commits or branches added, so it opened nothing itself and spawned the opening phase agent at opus, medium.",
          "The only call before the spawn was the read-only state tool `where` and `status`, the home shows no added files, commits or change directory, and the dispatcher spawned the opening phase agent at opus, medium.",
          "The dispatcher's only spawn is the opening phase agent at opus, medium; before it, the dispatcher's one call ran the state tool's read-only `where` and `status`, and no files, commits or change directories appeared in the home."
         ]
        },
        "RS-E2": {
         "quotes": [
          "research brief `research/json-report/brief.org` (in the home), workspace integration branch `ws/json-report` (repository `tally`), change `json-report`",
          "research brief `research/json-report/brief.org` (in the home), workspace integration branch `ws/json-report` (repository `tally`), change `json-report`",
          "research brief `research/json-report/brief.org` (in the home), workspace integration branch `ws/json-report` (repository `tally`), change `json-report`"
         ],
         "reasons": [
          "The brief carries the hand-off's brief path, branch and change name as given, and before the spawn the main thread made only the state tool call, reading no research brief, code, docs, flow or role file.",
          "The brief carries the hand-off's brief path, branch and change name as given, and the only call before the spawn was the state tool, with no read of the research brief, code, docs, flow or role files.",
          "The brief carries the hand-off's path, branch and change name as given, and the dispatcher's only call before the spawn was the state tool, with no read of the research brief, the code, docs/ or any flow or role file."
         ]
        }
       },
       "milestones": [],
       "n_calls": 2,
       "trial": 1
      }
     ],
     "validation": true
    }
   },
   "step": "the dispatcher's step on its invocation with a research effort's hand-off, in a home with an overlay and no change",
   "title": "Case dispatcher-start-research: a research hand-off is opened by an opening agent whose brief carries the hand-off as given",
   "trials": 3,
   "validated": "4864d5095275fe93d2d5c579434c4b31179bb1f3 20261114T104230 t1 (skill-evals)"
  },
  {
   "capability": "dispatcher",
   "id": "dispatcher-take-up-execute",
   "items": [
    {
     "checks": [
      {
       "id": "TE-S1",
       "kind": "decide"
      },
      {
       "id": "TE-S2",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § An open phase is taken up by a phase agent for it",
      "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
     ],
     "id": "TE-E1",
     "question": "Is the open phase taken up by a phase agent the dispatcher spawns, the dispatcher doing none of it?"
    },
    {
     "checks": [
      {
       "id": "TE-S3",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § Each phase agent runs at its phase's role default"
     ],
     "id": "TE-E2",
     "question": "Does the spawn pass execute's role default?"
    },
    {
     "checks": [
      {
       "id": "TE-S4",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The brief names the phase, the change, the cycle and the home"
     ],
     "id": "TE-E3",
     "question": "Does the brief name the phase, the change, the cycle and the home, and point at the phase's instructions?"
    },
    {
     "checks": [],
     "cites": [
      "dispatcher § The brief carries the phase agent's standing instructions"
     ],
     "id": "TE-E4",
     "question": "Does the brief carry the phase agent's standing instructions?"
    },
    {
     "checks": [
      {
       "id": "TE-S5",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § No new cycle while one is open"
     ],
     "id": "TE-E5",
     "question": "Is no new cycle started while this one is open?"
    },
    {
     "checks": [
      {
       "id": "TE-S6",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher"
     ],
     "id": "TE-E6",
     "question": "Is the waiting answer left to the phase agent?"
    },
    {
     "checks": [
      {
       "id": "TE-S7",
       "kind": "signal"
      },
      {
       "id": "TE-S8",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § A drifted overlay is relayed once and stops nothing"
     ],
     "id": "TE-E7",
     "question": "Is the overlay's drift relayed once, and does it stop nothing?"
    },
    {
     "checks": [
      {
       "id": "TE-S9",
       "kind": "decide"
      }
     ],
     "cites": [
      "dispatcher § The dispatcher reads no phase or role instructions, artifact or code",
      "dispatcher § An ask file is read once, when it is put"
     ],
     "id": "TE-E8",
     "question": "Does the dispatcher read nothing but its entry document and where the change stands?"
    }
   ],
   "runs": {
    "skill-evals": {
     "case_commit": "311131f",
     "cites": {
      "TE-E1": [
       "dispatcher § An open phase is taken up by a phase agent for it",
       "dispatcher § Every phase runs in a phase agent the dispatcher spawns"
      ],
      "TE-E2": [
       "dispatcher § Each phase agent runs at its phase's role default"
      ],
      "TE-E3": [
       "dispatcher § The brief names the phase, the change, the cycle and the home"
      ],
      "TE-E4": [
       "dispatcher § The brief carries the phase agent's standing instructions"
      ],
      "TE-E5": [
       "dispatcher § No new cycle while one is open"
      ],
      "TE-E6": [
       "dispatcher § A waiting answer is handled by the phase agent, not the dispatcher"
      ],
      "TE-E7": [
       "dispatcher § A drifted overlay is relayed once and stops nothing"
      ],
      "TE-E8": [
       "dispatcher § The dispatcher reads no phase or role instructions, artifact or code",
       "dispatcher § An ask file is read once, when it is put"
      ]
     },
     "commit": "db6ad9d",
     "item_checks": {
      "TE-E1": [
       "TE-S1",
       "TE-S2"
      ],
      "TE-E2": [
       "TE-S3"
      ],
      "TE-E3": [
       "TE-S4"
      ],
      "TE-E4": [],
      "TE-E5": [
       "TE-S5"
      ],
      "TE-E6": [
       "TE-S6"
      ],
      "TE-E7": [
       "TE-S7",
       "TE-S8"
      ],
      "TE-E8": [
       "TE-S9"
      ]
     },
     "items_then": [],
     "run": "20261114T002026",
     "trials": [
      {
       "calls": {},
       "checks": {
        "TE-S1": {
         "audit": {
          "agree": true,
          "reason": "There is an Agent call at call 3 and no SendMessage before it, which matches the evidence.",
          "verdict": "agrees"
         },
         "desc": "a phase agent spawned, and no agent resumed before it",
         "detail": "first spawn at event 2, first resume at None",
         "kind": "decide",
         "pass": true
        },
        "TE-S2": {
         "audit": {
          "agree": true,
          "reason": "No Bash call merges, rebases, cherry-picks or commits, and no branch moved, which matches the main thread.",
          "verdict": "agrees"
         },
         "desc": "nothing merged or committed: the code repository's branches as they were, no merge or commit run",
         "detail": "branches moved: none; merge or commit calls at none",
         "kind": "decide",
         "pass": true
        },
        "TE-S3": {
         "audit": {
          "agree": true,
          "reason": "The spawn's model is 'sonnet' and its effort is 'high', exactly as the item expects.",
          "verdict": "agrees"
         },
         "desc": "the spawn passes execute's role default, sonnet and high",
         "detail": "model 'sonnet', effort 'high'; brief 2349 chars: 'You are the phase agent for `execute` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`. `export ORCH_CHANGE=json-report`, then run `state.py status` (`state.py` is `python3 /private/tmp/skill-evals … [cut, 651 characters]",
         "kind": "decide",
         "pass": true
        },
        "TE-S4": {
         "audit": {
          "agree": true,
          "reason": "The brief contains execute, json-report, cycle-1 and the home path; the pointer to execute.md that the check does not test is also present.",
          "verdict": "agrees"
         },
         "desc": "the brief names execute, the change, the cycle and the home",
         "detail": "execute: True; json-report: True; cycle-1: True; home: True",
         "kind": "decide",
         "pass": true
        },
        "TE-S5": {
         "audit": {
          "agree": true,
          "reason": "There are no init or close calls, and the state's cycle, phase and task statuses are unchanged, as the evidence shows.",
          "verdict": "agrees"
         },
         "desc": "no cycle started or closed, the record's cycle, phase and tasks as they were",
         "detail": "init or close calls at none; cycle cycle-1, phase execute; tasks {'report-json': 'done', 'dollar-table': 'completed', 'ledger-comments': 'completed'} (before {'report-json': 'done', 'dollar-table': 'completed', 'ledger-comments': 'completed'})",
         "kind": "decide",
         "pass": true
        },
        "TE-S6": {
         "audit": {
          "agree": true,
          "reason": "The ask file and its index entry are unchanged and no file was added or changed in the home, consistent with the dispatcher leaving the answer alone.",
          "verdict": "agrees"
         },
         "desc": "the answered ask's file and state entry left as they were, in the inbox",
         "detail": "file in the inbox and unchanged: True; entry unchanged: True ({'id': 'ask-cycle-1-1', 'kind': 'decision', 'status': 'answered', 'blocks': ['ledger-comments'], 'path': '.orchestrator/changes/json-report/inbox/ask-cycle-1-1.org'})",
         "kind": "decide",
         "pass": true
        },
        "TE-S7": {
         "audit": {
          "agree": true,
          "reason": "No message names `scaffolding` (the session has no text at all), so its fail correctly reflects that the drift was not relayed.",
          "verdict": "agrees"
         },
         "desc": "a message before the spawn names the drifted key",
         "detail": "messages naming it at events none",
         "kind": "signal",
         "pass": false
        },
        "TE-S8": {
         "audit": {
          "agree": true,
          "reason": "The overlay is unchanged and no file changed in the home, which fits the item's requirement to leave the overlay to the user.",
          "verdict": "agrees"
         },
         "desc": "the overlay left as it was",
         "detail": "unchanged: True",
         "kind": "decide",
         "pass": true
        },
        "TE-S9": {
         "audit": {
          "agree": false,
          "reason": "Before the spawn the only calls are `state.py status` and a metadata-only `git show --stat`, neither of which reads a skill file, artifact, ask, docs or code.",
          "verdict": "agrees"
         },
         "desc": "nothing read before the spawn but the entry document, the state and the overlay",
         "detail": "none",
         "kind": "decide",
         "pass": true
        }
       },
       "items": {
        "TE-E1": "pass",
        "TE-E2": "pass",
        "TE-E3": "pass",
        "TE-E4": "pass",
        "TE-E5": "pass",
        "TE-E6": "pass",
        "TE-E7": "fail",
        "TE-E8": "pass"
       },
       "judge": {
        "TE-E1": {
         "quotes": [
          "[call 3] Agent: {\"description\": \"Execute phase json-report cycle-1\", \"subagent_type\": \"general-purpose\"",
          "[call 3] Agent: {\"description\": \"Execute phase json-report cycle-1\", \"subagent_type\": \"general-purpose\" ...",
          "[call 3] Agent: {\"description\": \"Execute phase json-report cycle-1\", \"subagent_type\": \"general-purpose\", ..."
         ],
         "reasons": [
          "The dispatcher spawned a fresh general-purpose phase agent for execute; before it, it only ran `state.py status` and a read-only `git show --stat`, with no merge, commit or test.",
          "The dispatcher spawned a fresh general-purpose agent for execute; before it, it only ran `state.py status` and a `git show --stat`, with no merge, commit or test, and no commits appear in the home.",
          "The dispatcher's only calls before the spawn were `state.py ... status` and `git show --stat`; it ran no merge, commit or test, then spawned a fresh general-purpose agent for execute."
         ]
        },
        "TE-E2": {
         "quotes": [
          "\"model\": \"sonnet\", \"effort\": \"high\"",
          "\"model\": \"sonnet\", \"effort\": \"high\"",
          "\"model\": \"sonnet\", \"effort\": \"high\""
         ],
         "reasons": [
          "The spawn passes both a model and an effort, and they are execute's default, sonnet and high.",
          "The spawn passes both model sonnet and effort high.",
          "The spawn passes both a model and an effort, and they are sonnet and high."
         ]
        },
        "TE-E3": {
         "quotes": [
          "You are the phase agent for `execute` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`. ... read and follow `.../flows/execute.md` to its exit gate and close commit.",
          "You are the phase agent for `execute` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home` ... read and follow `.../flows/execute.md` to its exit gate and close commit.",
          "You are the phase agent for `execute` of change `json-report` in cycle `cycle-1`. Home: `<trial>/home`. ... read and follow `.../flows/execute.md` to its exit gate and close commit."
         ],
         "reasons": [
          "The brief names the phase, the change, the cycle and the home's path, and tells the agent to follow execute's flow to its gate and close commit.",
          "The brief names the phase, change, cycle and home path, and points at execute's flow up to its gate and close commit.",
          "The brief names the phase, change, cycle and home path, and points at execute's flow up to its gate and close commit."
         ]
        },
        "TE-E4": {
         "quotes": [
          "pass its `model` and `effort` on the `Agent` call ... record it ... You cannot ask the user: an ask ... is a file, and you hand back `stopped` naming it after finishing everything that does not wait on it. ... Asked to hand over, you stop at a clean point and hand over ... Hand back one line ... nev … [cut, 390 characters]",
          "pass its `model` and `effort` on the `Agent` call ... record it ... You cannot ask the user: an ask that would take the blocking call is a file, and you hand back `stopped` naming it after finishing everything that does not wait on it ... You hand back once ... one line ... never a report. Asked to  … [cut, 412 characters]",
          "pass its `model` and `effort` on the `Agent` call ... record it ... You cannot ask the user: an ask ... is a file, and you hand back `stopped` naming it after finishing everything that does not wait on it ... Asked to hand over, you stop at a clean point and hand over ... Hand back one line ... neve … [cut, 389 characters]"
         ],
         "reasons": [
          "All five standing instructions are present in sense: the spawn setting with recording, no asking the user, a single one-line hand-back, handing over at a clean point, and the glossary terms.",
          "All five standing instructions are present in sense: the spawn setting and recording, no asking the user, a one-line hand-back, handing over at a clean point, and the glossary.",
          "The brief carries all five standing instructions: setting and record, no asking the user, a one-line hand-back given once, hand over on request, and glossary terms."
         ]
        },
        "TE-E5": {
         "quotes": [
          "\"cycle_id\": \"cycle-1\", ... \"phase\": \"execute\"",
          "\"cycle_id\": \"cycle-1\" ... \"phase\": \"execute\"; home's commits since the fixture: (none)",
          "\"cycle_id\": \"cycle-1\", ... \"phase\": \"execute\""
         ],
         "reasons": [
          "No init or close was run, and no commits or file changes followed, so cycle-1 stays in execute with its tasks as the fixture left them.",
          "No init or close was run and nothing in the home changed, so cycle-1 stays in execute with its tasks as they were.",
          "There is no init or close call, no file in the home changed and no commit was made, so the record still shows cycle-1 in execute with its tasks as they were."
         ]
        },
        "TE-E6": {
         "quotes": [
          "An answer is waiting in the inbox (ask-cycle-1-1); handle it as the sync point `templates/ask.md` § Sync points says.",
          "An answer is waiting in the inbox (ask-cycle-1-1); handle it as the sync point `templates/ask.md` § Sync points says.",
          "An answer is waiting in the inbox (ask-cycle-1-1); handle it as the sync point `templates/ask.md` § Sync points says."
         ],
         "reasons": [
          "The dispatcher released no task, moved no status and changed no file, and the brief hands the waiting answer to the phase agent.",
          "The dispatcher changed no files or state and gave the answer to the phase agent to handle.",
          "The dispatcher changed no file and released no task, and it handed the waiting answer to the phase agent in the brief."
         ]
        },
        "TE-E7": {
         "quotes": [
          "== The session's final text ==\n(none)",
          "The session's final text: (none)",
          "== The session's final text ==\n(none)"
         ],
         "reasons": [
          "The dispatcher never told the user about the overlay's retired `scaffolding` key; there is no message at all before the spawn.",
          "The dispatcher never told the user about the retired `scaffolding` key; it emitted no message before the spawn.",
          "No message to the user before the spawn mentions the drifted overlay or the scaffolding key, so the drift was never relayed; the spawn did go ahead and the overlay was left alone."
         ]
        },
        "TE-E8": {
         "quotes": [
          "[call 2] Bash: {\"command\": \"git show --stat --format=%s 4ad58b2 c88dd4a\"",
          "[call 2] Bash: {\"command\": \"git show --stat --format=%s 4ad58b2 c88dd4a\", \"description\": \"List files touched by the handover and answer commits\"}",
          "[call 2] Bash: {\"command\": \"git show --stat --format=%s 4ad58b2 c88dd4a\", ...}"
         ],
         "reasons": [
          "Before the spawn it ran only the state tool and a `git show --stat` that lists commit subjects and file names without their contents; no flow, role, handover, task, ask, docs or code file was read.",
          "Before the spawn, the dispatcher read the home's git history for the handover and answer commits, which is outside the entry document, the state tool's output, the state and the overlay; it then used that to name the handover path in the brief.",
          "Before the spawn the dispatcher read only the state tool's output plus commit subjects and file lists via `--stat`; it read no flow, role, handover content, task file, ask, docs or code."
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
   "step": "the dispatcher's step on its invocation with no phase named, a cycle open in execute with its gate not passed, the overlay drifted, an answered ask in the inbox",
   "title": "Case dispatcher-take-up-execute: invoked with no phase named, the dispatcher spawns execute's phase agent with the fixed brief, leaving the answer and the overlay alone",
   "trials": 3,
   "validated": "311131fb51a038f29eea8186c646a9b1f153aabc 20261114T002026 t1 (skill-evals)"
  }
 ],
 "source": {
  "evals_head": "589177f",
  "spec": "orchestrate/docs/specs/dispatcher.org",
  "spec_dirty": false,
  "worktree_head": "73b676c"
 },
 "spec": {
  "gaps": "No gap open. The seven this draft had were filled 2026-11-13 at the hand-off review (=research/skill-evals/extraction/dispatcher.org= § Gap decisions).",
  "purpose": "The dispatcher is the session the user invokes the skill in. It takes a change up where it stands and spawns the phase agent the change calls for, with a fixed brief at that phase's role default; it receives each phase agent's one-line hand-back, relays it to the user and acts on its outcome; it is the only agent that puts an ask to the user, one per call, and writes the answer back into the ask's file; and it keeps its own context to pointers, asking a phase agent near its context limit to hand over. It does no phase's work: it reads no instructions of a phase or role, no artifact and no code, applies no answer and decides nothing the user decides. Its behaviour shows in its session (what it reads, the agents it spawns and their briefs, the calls it puts to the user and what it shows them) and in the ask files and the change's record it writes to.",
  "requirements": [
   {
    "goal": "G13",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher takes a change through any phase"
       },
       {
        "kw": "THEN",
        "text": "it reads none of the skill's instructions for a phase or a role"
       },
       {
        "kw": "AND",
        "text": "it reads no artifact of the change, other than an ask file it is putting to the user, and no code"
       }
      ],
      "test": null,
      "title": "The dispatcher reads no phase or role instructions, artifact or code"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher puts an open ask to the user"
       },
       {
        "kw": "THEN",
        "text": "it reads that ask's file once, in the sitting that puts it: before the sitting's first call or as it puts that ask"
       },
       {
        "kw": "AND",
        "text": "it reads no ask file it does not put to the user in that sitting"
       }
      ],
      "test": null,
      "title": "An ask file is read once, when it is put"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase is to run: opening, the design round, plan, execute, integrate or the inline path"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns a phase agent for it from a fresh context"
       },
       {
        "kw": "AND",
        "text": "does none of the phase's work itself"
       }
      ],
      "test": null,
      "title": "Every phase runs in a phase agent the dispatcher spawns"
     }
    ],
    "text": "The dispatcher SHALL read only the skill's entry document, where the change stands, a running phase agent's context size, and the file of each ask it puts to the user, in the sitting that puts it; it SHALL read no instructions of a phase or role, no artifact of the change and no code, and SHALL run every phase in a phase agent spawned from a fresh context.",
    "title": "The dispatcher's context holds pointers only"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher is invoked with no phase named for a change that has no intent"
       },
       {
        "kw": "THEN",
        "text": "it spawns the opening phase agent"
       }
      ],
      "test": null,
      "title": "A change with no intent is opened"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher spawns the opening phase agent"
       },
       {
        "kw": "THEN",
        "text": "the brief carries the source: the user's words verbatim, the paths of the cross-change backlog's items taken up, the split's ask and its change, or a research hand-off: the path of the research effort's brief, the workspace integration branch and the change's name as the hand-off gives them"
       },
       {
        "kw": "AND",
        "text": "the dispatcher reads nothing to write it"
       }
      ],
      "test": null,
      "title": "An opening agent's brief carries the source as given"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher spawns the inline path's phase agent"
       },
       {
        "kw": "THEN",
        "text": "the brief carries the user's request verbatim"
       },
       {
        "kw": "AND",
        "text": "the dispatcher reads nothing to write it"
       }
      ],
      "test": null,
      "title": "An inline agent's brief carries the user's request as given"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the opening phase agent hands back that the change is opened"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher relays the line, which gives the user the intent's path"
       },
       {
        "kw": "AND",
        "text": "spawns the design round's phase agent for the change"
       }
      ],
      "test": null,
      "title": "An opened change goes on to its design round"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the opening phase agent hands back a line naming a second change and its repository"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the opening phase agent again for the second change, its source the same words and the first change's name"
       },
       {
        "kw": "AND",
        "text": "spawns the first change's design round after it"
       }
      ],
      "test": null,
      "title": "An opening that names a second change is followed by a second opening"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the opening phase agent hands back a line naming an old change to revise"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the opening phase agent to revise that change in place, its source the old intent, the split answer and the new change's name"
       }
      ],
      "test": null,
      "title": "A split's opening is followed by the old change's revision"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change has its intent, no cycle has started, and its design has no Seams section or does not exist"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the design round's phase agent"
       }
      ],
      "test": null,
      "title": "A change with no seams yet goes to the design round"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a change has its intent and a design with its Seams section, and no cycle has started"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the plan phase agent, which starts the first cycle"
       }
      ],
      "test": null,
      "title": "A designed change with no cycle starts a plan"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cycle is open and its current phase's gate has not passed"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns a phase agent for that phase"
       }
      ],
      "test": null,
      "title": "An open phase is taken up by a phase agent for it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase's gate has passed and the next phase has not started"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the next phase's agent"
       }
      ],
      "test": null,
      "title": "A passed gate is followed by the next phase"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate's gate has passed and the cycle is not closed"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns an integrate phase agent, which closes the cycle and makes its commit"
       }
      ],
      "test": null,
      "title": "An integrate that passed its gate is closed by an integrate agent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change's last cycle is closed"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the plan phase agent, which starts a new cycle"
       }
      ],
      "test": null,
      "title": "A closed cycle is followed by a plan for the next"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a cycle of the change is open"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher starts no new cycle"
       }
      ],
      "test": null,
      "title": "No new cycle while one is open"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the change shows an ask whose answer is waiting"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher does not apply the answer"
       },
       {
        "kw": "AND",
        "text": "the phase agent it spawns or resumes next handles it"
       }
      ],
      "test": null,
      "title": "A waiting answer is handled by the phase agent, not the dispatcher"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the plan phase agent hands back =failed= naming the change's unfinished design round"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the design round's phase agent for the change"
       },
       {
        "kw": "AND",
        "text": "no cycle has been opened"
       }
      ],
      "test": null,
      "title": "A design round plan finds unfinished is run before any cycle"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the plan phase agent hands back that it did not start because the user answered stop on the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher relays the line"
       },
       {
        "kw": "AND",
        "text": "spawns no further phase agent for the change"
       }
      ],
      "test": null,
      "title": "A stopped change is relayed and nothing more runs"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the plan phase agent hands back that it did not start because the user answered rethink on the MVP decision"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the opening phase agent for the change, with the intent and the answer as its source"
       }
      ],
      "test": null,
      "title": "A rethink answer sends the change back to opening"
     }
    ],
    "text": "Invoked with no phase named, the dispatcher SHALL spawn the phase agent for the step the change stands at: opening, the design round, a first plan, the open phase, the next phase after a passed gate, the close of a cycle, or a plan for the next cycle; it SHALL never start a new cycle while one is open, and SHALL leave a waiting answer to the phase agent it spawns or resumes.",
    "title": "Invoked with no phase named, the dispatcher takes the change up where it stands"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher spawns a phase agent"
       },
       {
        "kw": "THEN",
        "text": "its brief names the phase, the change, the cycle (or, for the design round, the change alone; for a plan that starts a new cycle, that the cycle is new, since it has no id until plan opens it) and the home"
       },
       {
        "kw": "AND",
        "text": "points the agent at its phase's instructions, to follow to the phase's gate and close commit"
       }
      ],
      "test": null,
      "title": "The brief names the phase, the change, the cycle and the home"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher spawns a phase agent"
       },
       {
        "kw": "THEN",
        "text": "its brief tells it to spawn every agent at the setting its phase names, passing model and effort, and to record each one"
       },
       {
        "kw": "AND",
        "text": "that it cannot ask the user: an ask is a file, and it hands back that it stopped, naming the ask, after finishing what does not wait on it"
       },
       {
        "kw": "AND",
        "text": "that it hands back once, in one line, never a report"
       },
       {
        "kw": "AND",
        "text": "that asked to hand over, it stops at a clean point and writes a handover"
       },
       {
        "kw": "AND",
        "text": "that what the user reads uses the terms of the product's glossary, when it has one"
       }
      ],
      "test": null,
      "title": "The brief carries the phase agent's standing instructions"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher spawns a phase agent"
       },
       {
        "kw": "THEN",
        "text": "the spawn passes both a model and an effort"
       },
       {
        "kw": "AND",
        "text": "they are the phase's role default: opening =opus=, =medium=; design round =sonnet=, =medium=; plan =opus=, =medium=; execute =sonnet=, =high=; integrate =sonnet=, =high=; inline path =sonnet=, =medium="
       }
      ],
      "test": null,
      "title": "Each phase agent runs at its phase's role default"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back for the first time"
       },
       {
        "kw": "THEN",
        "text": "the change's record holds the agent, its phase and the setting it was spawned at"
       },
       {
        "kw": "AND",
        "text": "when that agent is resumed and hands back again, no second entry is made for it"
       }
      ],
      "test": null,
      "title": "The dispatcher records a phase agent's setting at its first hand-back"
     }
    ],
    "text": "The dispatcher SHALL give each phase agent the same short brief, naming its phase, change, cycle and home and carrying the phase agent's standing instructions, SHALL spawn it at its phase's role default, passing both model and effort, and SHALL record its setting in the change's record at its first hand-back.",
    "title": "A phase agent is spawned with the fixed brief, at its phase's role default"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher shows the user the line itself"
       },
       {
        "kw": "AND",
        "text": "then the open decisions"
       }
      ],
      "test": null,
      "title": "A hand-back is relayed line first, then the open decisions"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher shows the user a phase's results and there are open decisions"
       },
       {
        "kw": "THEN",
        "text": "the decisions come first"
       },
       {
        "kw": "AND",
        "text": "the results follow, or go in a message of their own"
       }
      ],
      "test": null,
      "title": "Asks come before the phase's results"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it failed, other than plan naming an unfinished design round"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher tells the user what failed, as the line says"
       },
       {
        "kw": "AND",
        "text": "spawns no further phase agent"
       }
      ],
      "test": null,
      "title": "A failed phase is reported and nothing further is spawned"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent's hand-back names a worker still running or a wait"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher resumes that same agent by message"
       },
       {
        "kw": "AND",
        "text": "does not take the hand-back as the phase's end"
       }
      ],
      "test": null,
      "title": "A hand-back that names a running worker is resumed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent stopped on an ask and the dispatcher goes on with it"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher resumes that same agent by message"
       },
       {
        "kw": "AND",
        "text": "spawns no new agent for the phase"
       }
      ],
      "test": null,
      "title": "A phase agent stopped on an ask is resumed, not replaced"
     }
    ],
    "text": "After each phase agent's hand-back the dispatcher SHALL show the user the line itself, then the open decisions, before the phase's results; it SHALL report a failure and stop, resume by message an agent that stopped on an ask or handed back while a worker ran, and spawn a fresh agent only for a phase that finished.",
    "title": "Each hand-back is relayed to the user and acted on by its outcome"
   },
   {
    "goal": "G10 (the outcome list's link, the drafter's reading, not yet reviewed)",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user's last message in the session is less than 15 minutes old"
       },
       {
        "kw": "THEN",
        "text": "the user is present"
       }
      ],
      "test": null,
      "title": "A user who sent a message in the last 15 minutes is present"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the session holds no message from the user in its last 15 minutes, or none at all"
       },
       {
        "kw": "THEN",
        "text": "the user is absent"
       }
      ],
      "test": null,
      "title": "A user silent for 15 minutes is absent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it stopped on an ask and the user is present"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher puts that ask to the user in a call holding it alone"
       },
       {
        "kw": "AND",
        "text": "writes the answer into the ask's file"
       },
       {
        "kw": "AND",
        "text": "resumes the same phase agent by message"
       }
      ],
      "test": null,
      "title": "Stopped on an ask, a present user is asked and the agent resumed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it stopped on an ask that blocks work waiting now, the ask is neither an =environment= ask nor one that blocks plan, and the user is absent"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher puts no call to the user"
       },
       {
        "kw": "AND",
        "text": "resumes the same phase agent by message, saying that the user is absent"
       }
      ],
      "test": null,
      "title": "Stopped on an ask, an absent user's phase agent is resumed with word of the absence"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it stopped on several asks that block plan, and the user is present"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher puts every one of them, one per call, in one sitting"
       },
       {
        "kw": "AND",
        "text": "resumes the phase agent once all of them are answered"
       }
      ],
      "test": null,
      "title": "The design round's asks are all put in one sitting"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it stopped on asks that block plan, and the user is absent"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher stops with one line pointing at the asks' files"
       },
       {
        "kw": "AND",
        "text": "the change waits: no phase agent is resumed or spawned for it"
       }
      ],
      "test": null,
      "title": "Asks that block plan wait for an absent user"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it stopped on an =environment= ask and the user is present"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher puts it to the user at once, in a call holding it alone"
       }
      ],
      "test": null,
      "title": "An environment ask is put at once, alone"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it stopped on an =environment= ask and the user is absent"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher stops with one line pointing at the ask's file"
       },
       {
        "kw": "AND",
        "text": "resumes no phase agent"
       }
      ],
      "test": null,
      "title": "An environment ask stops the dispatcher when the user is absent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent has handed back, the user is present and several =decision= asks are open"
       },
       {
        "kw": "THEN",
        "text": "each is put to the user in a call of its own"
       },
       {
        "kw": "AND",
        "text": "the asks that block work waiting now are put first, then the rest in the order the change lists them"
       }
      ],
      "test": null,
      "title": "Open decisions are put one per call, the blocking ones first"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "integrate's phase agent has handed back and the user is present"
       },
       {
        "kw": "THEN",
        "text": "the open =decision= asks are put one per call, the blocking ones first, as after any hand-back"
       },
       {
        "kw": "AND",
        "text": "none is printed as a list line"
       }
      ],
      "test": null,
      "title": "After integrate, a present user is put the open decisions one per call"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent stopped on asks that block its work, other =decision= asks that block nothing are open, and the user is present"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher resumes the agent as soon as the blocking asks are answered"
       },
       {
        "kw": "AND",
        "text": "puts the others after the resume"
       }
      ],
      "test": null,
      "title": "A stopped agent is resumed before the asks that block nothing are put"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user is present and two or more =decision= asks are open"
       },
       {
        "kw": "THEN",
        "text": "no message the dispatcher writes lists them before they are put"
       }
      ],
      "test": null,
      "title": "Open decisions are never listed in chat while the user is present"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher puts an ask to the user"
       },
       {
        "kw": "THEN",
        "text": "the call carries the ask's recommendation"
       }
      ],
      "test": null,
      "title": "Every call carries the ask's recommendation"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user is present and a =decision= ask blocks no work waiting now"
       },
       {
        "kw": "THEN",
        "text": "it is put after the asks that do, in a call of its own"
       },
       {
        "kw": "AND",
        "text": "the call offers =Not now="
       }
      ],
      "test": null,
      "title": "An ask that blocks no waiting work is put with a Not now option"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher puts a =decision= ask that has a recommendation"
       },
       {
        "kw": "THEN",
        "text": "the call's option for the recommended choice has a label ending =(Recommended)="
       },
       {
        "kw": "AND",
        "text": "its description gives the reason the ask's file gives"
       }
      ],
      "test": null,
      "title": "The recommended option says so, with the file's reason"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher puts any ask to the user"
       },
       {
        "kw": "THEN",
        "text": "the call offers =Not now=, whatever the ask blocks"
       }
      ],
      "test": null,
      "title": "Every call offers Not now"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user answers =Not now= to an ask that blocks work waiting now"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher goes on as it does for that ask when the user is absent"
       }
      ],
      "test": null,
      "title": "Not now on a blocking ask goes on as if the user were absent"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user answers a call with =Not now="
       },
       {
        "kw": "THEN",
        "text": "the ask stays open"
       }
      ],
      "test": null,
      "title": "Not now leaves the ask open"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent has handed back, the user is absent and =decision= asks are open"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher relays each in one line: its id, its question and its file's path"
       },
       {
        "kw": "AND",
        "text": "puts no call"
       }
      ],
      "test": null,
      "title": "An absent user gets each open decision in one line"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that its gate passed and the open asks block no work waiting now"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns the next phase's agent without waiting for their answers"
       }
      ],
      "test": null,
      "title": "An ask that blocks nothing never stops the change"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user answers a call with one of the ask's options"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher writes that option's label into the ask's file as its answer"
       }
      ],
      "test": null,
      "title": "An answer given in the call is written into the ask's file"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher writes an answer given in a call into the ask's file"
       },
       {
        "kw": "THEN",
        "text": "it commits the file in the home, the subject naming the ask's id"
       },
       {
        "kw": "AND",
        "text": "before it resumes or spawns the phase agent that applies it"
       }
      ],
      "test": null,
      "title": "An answer given in a call is committed with the ask's id"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user answers a call with words of their own rather than an option"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher writes the words into the ask's file as given"
       },
       {
        "kw": "AND",
        "text": "leaves the readback to the phase agent"
       }
      ],
      "test": null,
      "title": "A free-text answer is written for the phase agent to read back"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the user answers an ask in chat"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher writes the answer into the ask's file"
       },
       {
        "kw": "AND",
        "text": "commits the file in the home"
       }
      ],
      "test": null,
      "title": "An answer given in chat is written into the file and committed"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "an ask's answer has a readback waiting for the user"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher shows the readback to the user in chat"
       },
       {
        "kw": "AND",
        "text": "when the user confirms it in chat, writes the confirmation into the ask's file and commits it in the home"
       }
      ],
      "test": null,
      "title": "A readback is shown in chat and its confirmation written to the file"
     }
    ],
    "text": "The dispatcher SHALL decide whether the user is present by the time of their last message; SHALL put an ask to a present user one per call, never as a list in chat, the asks that block work waiting now first, each call carrying the ask's recommendation; SHALL stop the change for an ask only when it is an =environment= ask, when it blocks work waiting now while the user is present, or when it blocks plan; and SHALL write each answer back into the ask's file.",
    "title": "Asks reach the user only through the dispatcher, one per call"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a running phase agent hands back or ends a turn to wait, and the dispatcher is about to resume it"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher first reads the agent's context in tokens against the home's limit, 400,000 unless the overlay sets another"
       },
       {
        "kw": "AND",
        "text": "never reads the agent's transcript itself"
       }
      ],
      "test": null,
      "title": "The dispatcher reads a phase agent's context before resuming it"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent's context is over the limit"
       },
       {
        "kw": "THEN",
        "text": "the message that resumes it asks it to hand over"
       }
      ],
      "test": null,
      "title": "Over the limit, the message that resumes the agent asks it to hand over"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "a phase agent hands back that it stopped with a handover"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher spawns a fresh agent for the same phase, at the setting of the agent it replaces"
       },
       {
        "kw": "AND",
        "text": "its brief is the phase's brief with one more instruction: continue from the handover, read after where the change stands, and redo nothing it lists as done"
       }
      ],
      "test": null,
      "title": "A handover is followed by a fresh agent for the same phase"
     }
    ],
    "text": "Before resuming a running phase agent, the dispatcher SHALL read the agent's context size against the home's limit (400,000 tokens unless the overlay sets another), never its transcript; over the limit, it SHALL ask the agent to hand over, and on a handover SHALL spawn a fresh agent for the same phase at the same setting, given the handover.",
    "title": "A phase agent near its context limit hands over to a fresh one"
   },
   {
    "goal": "none stated",
    "scenarios": [
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher finds no overlay for the home"
       },
       {
        "kw": "THEN",
        "text": "it tells the user explicitly that none was found"
       }
      ],
      "test": null,
      "title": "A missing overlay is announced to the user"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher finds no overlay for the home and the user is present"
       },
       {
        "kw": "THEN",
        "text": "it sets up the home's overlay with the user before it spawns any phase agent"
       }
      ],
      "test": null,
      "title": "With no overlay, a present user sets one up before any change runs"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher finds no overlay for the home and the user is absent"
       },
       {
        "kw": "THEN",
        "text": "it stops after the warning"
       },
       {
        "kw": "AND",
        "text": "spawns no phase agent"
       }
      ],
      "test": null,
      "title": "With no overlay and the user absent, the dispatcher stops"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the home's overlay has drifted from the skill's form: a removed key, a retired concept, a path that climbs to the home"
       },
       {
        "kw": "THEN",
        "text": "the dispatcher tells the user once in the session"
       },
       {
        "kw": "AND",
        "text": "the change goes on, the overlay left for the user to correct by hand"
       }
      ],
      "test": null,
      "title": "A drifted overlay is relayed once and stops nothing"
     },
     {
      "steps": [
       {
        "kw": "WHEN",
        "text": "the dispatcher sets up a home's overlay"
       },
       {
        "kw": "THEN",
        "text": "it asks the user, one question per call in one sitting: the repositories and their checkouts, the test command and its parser, the worktree placement, the hooks and the pending-test style"
       },
       {
        "kw": "AND",
        "text": "the overlay is written from the answers, never copied from another home"
       }
      ],
      "test": null,
      "title": "A home's overlay is set up with the user, one question per call"
     }
    ],
    "text": "The dispatcher SHALL tell the user explicitly when no overlay is found for the home, SHALL relay an overlay's drift from the skill's form once and stop nothing for it, and SHALL set up a home's overlay with the user present, one question per call.",
    "title": "The user is told when the overlay is missing or has drifted"
   }
  ],
  "status": "draft 2026-11-13, extracted from the reference skill (=main= =e356da7=); at the hand-off review 2026-11-13: G4, G6 and G10 applied, the design round's asks no longer go to plan (G1, G5); the recommendation's form, the resume's order, the call's commit, the missing overlay and the stopped change filled; no gap open; accepted 2026-11-13 by the user at the hand-off review; amended 2026-11-13 after acceptance (a second opening for a change in a second repository; the old change's revision after a split), from =opening='s review; the inline brief's request, from =inline-path='s review; amended 2026-11-14 after acceptance (a sitting may read the files of the asks it puts before its first call; a new cycle's plan brief; a research hand-off as an opening's source), from the evals' build-out (=research/skill-evals/overnight/plan.org= § Decided with the user, 2026-11-14 morning)",
  "title": "dispatcher"
 },
 "summary": {
  "cases": 30,
  "cited": 62,
  "fail": 16,
  "pass": 46,
  "scenarios": 62,
  "title": "dispatcher",
  "tree": "skill-evals",
  "validation": 30
 },
 "trees": [
  "skill-evals",
  "main"
 ],
 "unknown_citations": []
};
