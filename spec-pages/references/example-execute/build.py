#!/usr/bin/env python3
"""Build a spec's page: what the capability does, scenario by scenario, and its proof.

  docs/pages/build.py                 every spec in docs/specs that an eval case is on
  docs/pages/build.py execute         one spec

Writes <capability>.data.js (generated, deterministic) and <capability>.html (from spec.template.html)
next to this file. Everything on the page is computed; nothing is authored (the research effort's
brief, research/document-as-you-go/brief.org, * Decided 2026-11-13).

Reads, and never writes:
- the spec, docs/specs/<capability>.org, at this worktree's HEAD (its working-tree text, marked dirty if changed);
- the eval cases on that capability (evals/cases/*/case.org with :CAPABILITY:), their expected.org items
  and the scenarios each item cites, now and at the case version a run used (git show in evals/);
- evals/history.jsonl: the latest run that counts, per case and tree, and each trial's item verdicts,
  check results and the scenarios each check cites;
- each trial's kept evidence (t<n>.artifact.txt) and transcript (t<n>.jsonl), for the milestones.

A milestone is an event a spec scenario or a check names: a merge, an inline fix, a test run with its
verdict, a journal entry, an ask, a spawn. Commits, journal entries, asks and spawns come from the end
state the case kept; test runs from the calls that ran the tests and returned a unittest summary
(research/document-as-you-go/probes/milestones-from-end-state.org). Each is linked to the call that
made it, and through the checks whose detail names it to the scenarios those checks cite.
"""
import hashlib, json, os, re, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
SPECS = os.path.normpath(f"{HERE}/../specs")
WORKTREE = os.path.normpath(f"{HERE}/../../..")
EVALS = os.environ.get("PAGES_EVALS", os.path.normpath(f"{WORKTREE}/../../evals"))
WORKSPACE = os.path.dirname(EVALS)
CUT = 600
# The trees a page shows: the one that implements the specs first, then the reference skill. state-layer is set
# aside until the specs and the eval design are done (the evals spec, D18).
TREES = ("skill-evals", "main")


def sh(*a, cwd=None):
    return subprocess.run(a, capture_output=True, text=True, cwd=cwd).stdout.strip()


# ---------------------------------------------------------------- the spec

def parse_spec(path):
    text = open(path).read()
    title = re.search(r"^#\+TITLE:\s*(.*)$", text, re.M).group(1).strip()
    status = (re.search(r"^#\+STATUS:\s*(.*)$", text, re.M) or [None, ""])[1].strip()
    top = re.split(r"^\* ", text, flags=re.M)
    sections = {s.split("\n", 1)[0].strip(): s.split("\n", 1)[1] if "\n" in s else "" for s in top[1:]}
    purpose = " ".join(sections.get("Purpose", "").split())
    reqs = []
    for block in re.split(r"^\*\* Requirement: ", sections.get("Requirements", ""), flags=re.M)[1:]:
        head, _, rest = block.partition("\n")
        goal = re.search(r"^:GOAL:\s*(.*)$", rest, re.M)
        parts = re.split(r"^\*\*\* Scenario: ", rest, flags=re.M)
        body = re.sub(r":PROPERTIES:.*?:END:\n?", "", parts[0], flags=re.S)
        scen = []
        for sb in parts[1:]:
            st, _, srest = sb.partition("\n")
            test = re.search(r"^:TEST:\s*(.*)$", srest, re.M)
            steps = [{"kw": k, "text": " ".join(t.split())}
                     for k, t in re.findall(r"^- \*(WHEN|THEN|AND|GIVEN)\*\s+(.*?)(?=^- \*|\Z)", srest, re.M | re.S)]
            scen.append({"title": st.strip(), "steps": steps, "test": test.group(1).strip() if test else None})
        reqs.append({"title": head.strip(), "goal": goal.group(1).strip() if goal else None,
                     "text": " ".join(body.split()), "scenarios": scen})
    gaps = " ".join(sections.get("Gaps", "").split())
    return {"title": title, "status": status, "purpose": purpose, "requirements": reqs, "gaps": gaps}


# ---------------------------------------------------------------- the cases

CITE = re.compile(r"([a-z][a-z0-9-]*) § ([^;]+)")


def parse_items(text):
    """expected.org's items: id, question, the scenarios cited, the checks listed under it."""
    out = []
    for block in re.split(r"^\*\* ", text, flags=re.M)[1:]:
        head, _, rest = block.partition("\n")
        m = re.match(r"([A-Z]+-E\d+)\.\s*(.*)", head)
        if not m:
            continue
        cites = re.search(r"^:CITES:\s*(.*)$", rest, re.M)
        checks = re.findall(r"^\s+- ([A-Z]+-S\d+) \((\w+)\)", rest, re.M)
        out.append({"id": m.group(1), "question": m.group(2).strip(),
                    "cites": [f"{c} § {t.strip()}" for c, t in CITE.findall(cites.group(1))] if cites else [],
                    "checks": [{"id": c, "kind": k} for c, k in dict(checks).items()]})
    return out


def case_props(text):
    blk = re.search(r":PROPERTIES:\n(.*?):END:", text, re.S)
    return dict(re.findall(r"^:([A-Z_]+):\s*(.*)$", blk.group(1), re.M)) if blk else {}


def load_cases(capability):
    cases = []
    root = f"{EVALS}/cases"
    for d in sorted(os.listdir(root)):
        p = f"{root}/{d}/case.org"
        if not os.path.exists(p):
            continue
        text = open(p).read()
        props = case_props(text)
        if props.get("CAPABILITY") != capability:
            continue
        exp = f"{root}/{d}/expected.org"
        cases.append({"id": d, "title": re.search(r"^#\+TITLE:\s*(.*)$", text, re.M).group(1).strip(),
                      "step": props.get("STEP", ""), "trials": int(props.get("TRIALS", "1")),
                      "validated": props.get("VALIDATED", ""),
                      "items": parse_items(open(exp).read()) if os.path.exists(exp) else []})
    return cases


# ---------------------------------------------------------------- results and milestones

def history():
    p = f"{EVALS}/history.jsonl"
    return [json.loads(l) for l in open(p) if l.strip()] if os.path.exists(p) else []


def latest_counting(rows, case):
    """{tree: [rows of the latest counting run on that tree, by trial]}."""
    by_tree = {}
    for r in rows:
        if r.get("case") == case and r.get("counts"):
            t = r["tree"]["label"]
            if t not in by_tree or r["run_id"] > by_tree[t][0]["run_id"]:
                by_tree[t] = [x for x in rows if x.get("case") == case and x.get("counts")
                              and x["run_id"] == r["run_id"] and x["tree"]["label"] == t]
    return {t: sorted(by_tree[t], key=lambda x: x["trial"]) for t in TREES if t in by_tree}


def sections(path):
    out, cur = {}, None
    for line in open(path):
        m = re.match(r"^== (.*?) ==$", line.rstrip("\n"))
        if m:
            cur = m.group(1)
            out[cur] = []
        elif cur:
            out[cur].append(line.rstrip("\n"))
    return out


def sect(secs, prefix):
    return next((v for k, v in secs.items() if k.startswith(prefix)), [])


def transcript_calls(path):
    uses, res = [], {}
    for line in open(path):
        e = json.loads(line)
        msg = e.get("message")
        c = msg.get("content") if isinstance(msg, dict) else None
        for x in c if isinstance(c, list) else []:
            if x.get("type") == "tool_use":
                uses.append(x)
            elif x.get("type") == "tool_result":
                r = x.get("content")
                r = r if isinstance(r, str) else " ".join(y.get("text", "") for y in r or [] if isinstance(y, dict))
                res[x["tool_use_id"]] = (r, bool(x.get("is_error")))
    out = []
    for n, u in enumerate(uses, 1):
        r, err = res.get(u["id"], ("", False))
        out.append({"n": n, "tool": u["name"], "input": u["input"], "raw": json.dumps(u["input"]),
                    "result": r, "error": err})
    return out


def cut(s, n):
    return s if len(s) <= n else s[:n] + f" … [cut, {len(s)} characters]"


def show_call(c):
    i = c["input"]
    s = i.get("command") or i.get("description") or i.get("file_path") or json.dumps(i)
    s = re.sub(r"/private/tmp/skill-evals/\d{8}T\d{6}/\d{3}/", "<trial>/", s)  # the trial's own directory
    return s if len(s) <= CUT else s[:CUT] + f" … [cut, {len(s)} characters]"


def task_of(text, tasks):
    return next((t for t in tasks if t in text), None)


def milestones(art, calls, tasks):
    s = sections(art)
    ms = []
    for l in sect(s, "The change's branch"):
        m = re.match(r"^([0-9a-f]{7,}) (.*)", l)
        if not m:
            continue
        sha, subj = m.groups()
        mt = re.match(r"Merge task ([\w-]+)", subj)
        fx = re.match(r"Inline fix after merging task ([\w-]+)", subj)
        sig = f"merge {mt.group(1)}" if mt else f"inline fix after {fx.group(1)}" if fx else "commit"
        ms.append({"lane": "git", "sig": sig, "label": subj, "keys": [sha[:7], subj[:40]], "how": "text", "find": subj[:40]})
    for l in sect(s, "The change's journal"):
        if l.startswith("{"):
            try:
                j = json.loads(l)
            except ValueError:
                continue
            ms.append({"lane": "record", "sig": f"journal {j.get('kind')} on {j.get('ref')}",
                       "label": f"journal, {j.get('kind')}: {j.get('text', '')}", "keys": [j.get("text", "")[:40]],
                       "how": "journal", "find": j.get("text", "")[:20]})
    cur = None
    for l in sect(s, "Ask files"):
        m = re.match(r"^(\S+\.org):$", l)
        if m:
            cur = {"lane": "record", "sig": "ask", "label": "ask filed: " + m.group(1).split("/")[-1],
                   "keys": [m.group(1)], "how": "file", "find": m.group(1).split("/")[-1]}
            ms.append(cur)
        elif cur and l.startswith(":KIND:"):
            cur["sig"] = f"ask, {l.split()[-1]}"
            cur["label"] += f" ({l.split()[-1]})"
        elif cur and l.startswith("#+TITLE:"):
            cur["label"] += ": " + l[8:].strip()
    for l in sect(s, "Agents spawned"):
        m = re.match(r'^\{"description": "([^"]+)"', l)
        if m:
            t = task_of(m.group(1), tasks)
            role = "Reviewer" if "eview" in m.group(1) else "Implementor" if "mplement" in m.group(1) else "agent"
            ms.append({"lane": "spawn", "sig": f"spawn {role} {t}", "label": f"spawn: {m.group(1)} (refused at the stop)",
                       "keys": [m.group(1), f"{role} spawn(s) for {t}"], "how": "agent", "find": m.group(1)})
    # link each end-state milestone to its call
    for m in ms:
        hits = []
        for c in calls:
            if m["how"] == "agent":
                ok = c["tool"] == "Agent" and m["find"] in c["raw"]
            elif m["how"] == "journal":
                ok = c["tool"] == "Bash" and "note" in c["raw"] and m["find"].replace('"', '\\"') in c["raw"]
            else:
                ok = m["find"].replace('"', '\\"') in c["raw"]
            if ok:
                hits.append(c["n"])
        m["linked_by"] = "its text in the call" if hits else None
        if not hits and m["lane"] == "git":  # a subject built from shell variables: the call that first printed the sha
            sha = m["keys"][0]
            hits = [c["n"] for c in calls if sha in c["result"]][:1]
            m["linked_by"] = "its sha in the call's output" if hits else None
        m["call"] = (hits[-1] if m["lane"] == "git" else hits[0]) if hits else None
        m["candidates"] = hits
    # test runs from the calls that ran the tests and returned a unittest summary
    for c in calls:
        if c["tool"] == "Bash" and re.search(r"python3? -m unittest", c["raw"]) and re.search(r"^Ran \d+ tests?", c["result"], re.M):
            red = bool(re.search(r"^FAILED \(", c["result"], re.M))
            files = re.findall(r"(after-[\w.$-]+\.txt)", c["input"].get("command", ""))
            ms.append({"lane": "tests", "sig": None, "red": red, "label": f"test run: {'fails' if red else 'passes'}",
                       "keys": files, "call": c["n"], "candidates": [c["n"]], "linked_by": "the call ran the tests"})
    ms.sort(key=lambda m: (m["call"] if m["call"] is not None else 10 ** 6))
    last = "the handover"
    for m in ms:  # a test run is named by what it follows
        if m["lane"] == "git":
            last = m["sig"]
        if m["lane"] == "tests":
            m["sig"] = f"test run after {last}: {'red' if m['red'] else 'green'}"
    return ms


def link_checks(ms, row, details):
    """A milestone is linked to a check whose detail names it; the check cites scenarios."""
    dets = {c["id"].split()[0]: str(c.get("detail") or "") for c in details.get("checks", [])}
    cites = row.get("cites") or {}
    for m in ms:
        ids = sorted({cid for cid, d in dets.items() for k in m["keys"] if k and len(k) >= 7 and k in d})
        m["checks"] = ids
        m["scenarios"] = sorted({s for cid in ids for s in cites.get(cid, [])})


def run_data(case, rows_by_tree, tasks):
    trees = {}
    for tree, rows in rows_by_tree.items():
        trials = []
        for r in rows:
            base = f"{WORKSPACE}/{os.path.dirname(r['details'])}" if r.get("details") else None
            art = f"{base}/t{r['trial']}.artifact.txt" if base else None
            tr = f"{base}/t{r['trial']}.jsonl" if base else None
            dp = f"{WORKSPACE}/{r['details']}" if r.get("details") else None
            det = json.load(open(dp)) if dp and os.path.exists(dp) else {}
            ms = []
            calls = []
            if art and os.path.exists(art) and os.path.exists(tr):
                calls = transcript_calls(tr)
                ms = milestones(art, calls, tasks)
                link_checks(ms, r, det)
            items = {i["item"]: i["verdict"] for i in r.get("rubric") or []}
            # each check as the run defined it: its words from the details (the check's id is followed by what it
            # observes), its kind and result from the row, its detail and the eval agent's audit of it
            desc = {c["id"].split()[0]: c["id"].split(" ", 1)[1] if " " in c["id"] else "" for c in det.get("checks", [])}
            cdet = {c["id"].split()[0]: str(c.get("detail") or "") for c in det.get("checks", [])}
            # the eval agent's audit: the most common verdict, and the reason of the first judge who gave it
            audit = {a["check"]: {"verdict": a.get("verdict"), "agree": a.get("agree"),
                                  "reason": next((r for v, r in zip(a.get("verdicts", []), a.get("reasons", []))
                                                  if v == a.get("verdict")), "")}
                     for a in det.get("audit", []) if a.get("check")}
            checks = {c["id"]: {"pass": c["pass"], "kind": c.get("kind"), "desc": desc.get(c["id"], ""),
                                "detail": cut(cdet.get(c["id"], ""), 300), "audit": audit.get(c["id"])}
                      for c in r.get("checks") or []}
            judge = {i["item"]: {"reasons": i.get("reasons", []), "quotes": [cut(q, 300) for q in i.get("quotes", [])]}
                     for i in det.get("rubric", []) if i.get("item")}
            trials.append({"trial": r["trial"], "items": items, "checks": checks, "judge": judge,
                           "milestones": [{k: m[k] for k in ("lane", "sig", "label", "call", "candidates", "linked_by", "checks", "scenarios")}
                                          for m in ms],
                           "calls": {str(c["n"]): {"tool": c["tool"], "text": show_call(c), "error": c["error"]}
                                     for c in calls if any(c["n"] in m["candidates"] for m in ms)},
                           "n_calls": len(calls)})
        r0 = rows[0]
        v = r0.get("versions") or {}
        old = sh("git", "show", f"{v['case_commit']}:cases/{case['id']}/expected.org", cwd=EVALS) if v.get("case_commit") else ""
        trees[tree] = {"run": r0["run_id"], "commit": r0["tree"]["commit"][:7], "case_commit": (v.get("case_commit") or "")[:7],
                       "items_then": parse_items(old) if old else [], "cites": r0.get("cites") or {}, "trials": trials}
    return trees


# ---------------------------------------------------------------- the page

def build(capability):
    spec_path = f"{SPECS}/{capability}.org"
    spec = parse_spec(spec_path)
    head = sh("git", "rev-parse", "--short", "HEAD", cwd=WORKTREE)
    dirty = bool(sh("git", "status", "--porcelain", "--", spec_path, cwd=WORKTREE))
    evals_head = sh("git", "rev-parse", "--short", "HEAD", cwd=EVALS)
    rows = history()
    cases = []
    for c in load_cases(capability):
        tasks = sorted(set(re.findall(r"=([a-z]+-[a-z]+(?:-[a-z]+)?)=", open(f"{EVALS}/cases/{c['id']}/case.org").read())))
        c["runs"] = run_data(c, latest_counting(rows, c["id"]), tasks)
        cases.append(c)
    titles = {f"{capability} § {s['title']}" for r in spec["requirements"] for s in r["scenarios"]}
    unknown = sorted({s for c in cases for it in c["items"] for s in it["cites"]
                      if s.startswith(capability + " § ") and s not in titles})
    data = {"capability": capability, "spec": spec, "trees": list(TREES),
            "source": {"spec": f"orchestrate/docs/specs/{capability}.org", "worktree_head": head, "spec_dirty": dirty,
                       "evals_head": evals_head},
            "cases": cases, "unknown_citations": unknown}
    js = f"window.SPEC_PAGE = {json.dumps(data, indent=1, sort_keys=True, ensure_ascii=False)};\n"
    open(f"{HERE}/{capability}.data.js", "w").write(js)
    tpl = open(f"{HERE}/spec.template.html").read()
    version = hashlib.sha1(js.encode()).hexdigest()[:10]  # a browser caches the data file; its hash busts that
    open(f"{HERE}/{capability}.html", "w").write(tpl.replace("{{capability}}", capability).replace("{{data_version}}", version))
    n = sum(len(r["scenarios"]) for r in spec["requirements"])
    cited = {s for c in cases for it in c["items"] for s in it["cites"]} & titles
    print(f"{capability}: {n} scenarios, {len(cited)} cited by {len(cases)} case(s); "
          f"{len(unknown)} citation(s) naming no scenario -> {capability}.html")
    return unknown


if __name__ == "__main__":
    caps = sys.argv[1:] or sorted({case_props(open(f"{EVALS}/cases/{d}/case.org").read()).get("CAPABILITY")
                                   for d in os.listdir(f"{EVALS}/cases") if os.path.exists(f"{EVALS}/cases/{d}/case.org")} - {None})
    bad = [u for cap in caps for u in build(cap)]
    sys.exit(1 if bad else 0)
