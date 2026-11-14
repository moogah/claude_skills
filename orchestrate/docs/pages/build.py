#!/usr/bin/env python3
"""Build a spec's page: what the capability does, scenario by scenario, and its proof.

  docs/pages/build.py                 every spec in docs/specs that an eval case is on
  docs/pages/build.py execute         one spec

Either way index.html is rebuilt from every <capability>.data.js in this directory.

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


def load_cases(capability, rows=()):
    """The cases on this capability, and every other case whose items cite one of its scenarios, now or in a run
    (a page keys on the spec's scenarios, not on the case's own capability)."""
    cases = []
    root = f"{EVALS}/cases"
    pre = capability + " § "
    then = {r.get("case") for r in rows if any(c.startswith(pre) for v in (r.get("cites") or {}).values() for c in v)}
    for d in sorted(os.listdir(root)):
        p = f"{root}/{d}/case.org"
        if not os.path.exists(p):
            continue
        text = open(p).read()
        props = case_props(text)
        exp = f"{root}/{d}/expected.org"
        items = parse_items(open(exp).read()) if os.path.exists(exp) else []
        if props.get("CAPABILITY") != capability and d not in then \
                and not any(c.startswith(pre) for it in items for c in it["cites"]):
            continue
        cases.append({"id": d, "title": re.search(r"^#\+TITLE:\s*(.*)$", text, re.M).group(1).strip(),
                      "capability": props.get("CAPABILITY", ""), "step": props.get("STEP", ""),
                      "trials": int(props.get("TRIALS", "1")), "validated": props.get("VALIDATED", ""),
                      "items": items})
    return cases


# ---------------------------------------------------------------- results and milestones

def history():
    p = f"{EVALS}/history.jsonl"
    return [json.loads(l) for l in open(p) if l.strip()] if os.path.exists(p) else []


def latest_counting(rows, case, validated=""):
    """{tree: [rows of the latest counting run on that tree, by trial]}. A tree with no counting run shows the
    validation trial the case's :VALIDATED: names, its rows marked (a validation does not count)."""
    by_tree = {}
    for r in rows:
        if r.get("case") == case and r.get("counts"):
            t = r["tree"]["label"]
            if t not in by_tree or r["run_id"] > by_tree[t][0]["run_id"]:
                by_tree[t] = [x for x in rows if x.get("case") == case and x.get("counts")
                              and x["run_id"] == r["run_id"] and x["tree"]["label"] == t]
    v = re.match(r"(\S+)\s+(\S+)\s+t(\d+)\s+\((.*?)\)", validated)
    if v and v.group(4) not in by_tree:
        _, run, n, t = v.groups()
        by_tree[t] = [dict(x, validation=True) for x in rows if x.get("case") == case and x["run_id"] == run
                      and x["trial"] == int(n) and x["tree"]["label"] == t]
    return {t: sorted(by_tree[t], key=lambda x: x["trial"]) for t in TREES if by_tree.get(t)}


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


def mask(s):
    """A trial's scratch directory becomes <trial>/, a run's <run>/, the user's home ~ (rule 8: deterministic and
    no machine paths, in calls, reasons, quotes and check details alike)."""
    s = re.sub(r"/private/tmp/skill-evals/\d{8}T\d{6}(?:-\d+)?/\d{3}/", "<trial>/", s)
    s = re.sub(r"/private/tmp/skill-evals/\d{8}T\d{6}(?:-\d+)?/(?:\d{3}\b)?", "<run>/", s)
    return re.sub(r"/Users/[^/\"\\ ]+/", "~/", s)


def cut(s, n):
    return s if len(s) <= n else s[:n] + f" … [cut, {len(s)} characters]"


def show_call(c):
    i = c["input"]
    s = i.get("command") or i.get("description") or i.get("file_path") or json.dumps(i)
    s = re.sub(r"/private/tmp/skill-evals/\d{8}T\d{6}/\d{3}/", "<trial>/", s)  # the trial's own directory
    return s if len(s) <= CUT else s[:CUT] + f" … [cut, {len(s)} characters]"


def task_of(text, tasks):
    return next((t for t in tasks if t in text), None)


def home_files(s):
    """The home's added or changed files, from the section that gives each in full: {path: [lines]}."""
    out, cur = {}, None
    for l in sect(s, "Files added or changed in the home"):
        m = re.match(r"^--- (\S+) ---$", l)
        if m:
            cur = m.group(1)
            out[cur] = []
        elif cur:
            out[cur].append(l)
    return out


def journal(s, files, since):
    """Journal entries: the state file's journal (since 2026-11-14), else the older artifacts' own section. The
    state file is given in full, so entries the fixture planted are left out by date: only those at or after the
    run's day."""
    st = next((v for k, v in files.items() if k.endswith("/state.json")), None)
    if st is not None:
        try:
            return [j for j in json.loads("\n".join(st)).get("journal") or [] if str(j.get("at", ""))[:10] >= since]
        except ValueError:
            return []
    out = []
    for l in sect(s, "The change's journal"):
        if l.startswith("{"):
            try:
                out.append(json.loads(l))
            except ValueError:
                pass
    return out


def commit_sig(subj):
    for pat, sig in ((r"Merge task ([\w-]+)", "merge {}"), (r"Inline fix after merging task ([\w-]+)", "inline fix after {}"),
                     (r'Revert "Merge task ([\w-]+)', "revert merge of {}")):
        m = re.match(pat, subj)
        if m:
            return sig.format(m.group(1))
    return "commit: " + (subj if len(subj) <= 60 else subj[:60] + "…")


def milestones(art, calls, tasks, since):
    s = sections(art)
    files = home_files(s)
    ms = []
    prev = ""  # the log gives each commit's subject, then its body: a commit starts after a blank line
    for l in sect(s, "The change's branch"):
        m = re.match(r"^([0-9a-f]{7,40}) (.*)", l)
        if m and not prev.strip():
            sha, subj = m.groups()
            ms.append({"lane": "git", "sig": commit_sig(subj), "label": subj, "keys": [sha[:7], subj[:40]],
                       "how": "text", "find": subj[:40]})
        prev = l
    for j in journal(s, files, since):
        ms.append({"lane": "record", "sig": f"journal {j.get('kind')} on {j.get('ref')}",
                   "label": f"journal, {j.get('kind')}: {j.get('text', '')}", "keys": [j.get("text", "")[:40]],
                   "how": "journal", "find": j.get("text", "")[:20]})
    for path, lines in sorted(files.items()):  # asks: files in the change's inbox
        if re.search(r"/inbox/ask-[\w-]+\.org$", path):
            name = path.split("/")[-1]
            kind = next((l.split()[-1] for l in lines if l.startswith(":KIND:")), None)
            title = next((l[8:].strip() for l in lines if l.startswith("#+TITLE:")), "")
            ms.append({"lane": "record", "sig": f"ask, {kind}" if kind else "ask",
                       "label": f"ask {name}{f' ({kind})' if kind else ''}{': ' + title if title else ''}",
                       "keys": [name[:-4], name], "how": "file", "find": name})
    cur = None
    for l in sect(s, "Ask files"):  # the artifacts before 2026-11-14
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
            role = ("Reviewer" if "eview" in m.group(1) else "Implementor" if "mplement" in m.group(1)
                    else "Implementor (rework)" if "ework" in m.group(1) else "agent")
            ms.append({"lane": "spawn", "sig": f"spawn {role} {t}", "label": f"spawn: {m.group(1)}",
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
        m["match"] = m["find"] if hits else None
        if not hits and m["lane"] == "git":  # a subject built from shell variables: the call that first printed the sha
            sha = m["keys"][0]
            hits = [c["n"] for c in calls if sha in c["result"]][:1]
            m["linked_by"] = "its sha in the call's output" if hits else None
            m["match"] = None
        m["call"] = (hits[-1] if m["lane"] == "git" else hits[0]) if hits else None
        m["candidates"] = hits
    # test runs: the calls the collector names as having run the tests, one per unittest summary in its result;
    # the artifacts before 2026-11-14 have no such section, and the calls that returned a summary stand in
    ran = sect(s, "What the calls that ran the tests")
    n = None
    for i, l in enumerate(ran):
        m = re.match(r"^call (\d+):$", l)
        if m:
            n = int(m.group(1))
        elif n is not None and re.match(r"^Ran \d+ tests?", l):
            verdict = next((x for x in ran[i + 1:i + 4] if re.match(r"^(OK|FAILED)\b", x)), "")
            red = verdict.startswith("FAILED")
            ms.append({"lane": "tests", "sig": None, "red": red, "label": f"test run: {'fails' if red else 'passes'} ({verdict or 'no verdict'})",
                       "keys": [], "call": n, "candidates": [n], "linked_by": "the collector names the call", "match": None,
                       "result": "\n".join([l, verdict]) if verdict else l})
    for c in calls if not ran else []:
        if c["tool"] == "Bash" and re.search(r"python3? -m unittest", c["raw"]) and re.search(r"^Ran \d+ tests?", c["result"], re.M):
            red = bool(re.search(r"^FAILED \(", c["result"], re.M))
            files = re.findall(r"(after-[\w.$-]+\.txt)", c["input"].get("command", ""))
            summ = re.findall(r"^(Ran \d+ tests?.*|OK\b.*|FAILED \(.*)$", c["result"], re.M)
            ms.append({"lane": "tests", "sig": None, "red": red, "label": f"test run: {'fails' if red else 'passes'}",
                       "keys": files, "call": c["n"], "candidates": [c["n"]], "linked_by": "the call ran the tests",
                       "match": None, "result": "\n".join(summ[-2:])})
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
        hits = {cid: sorted({k for k in m["keys"] if k and len(k) >= 7 and k in d}) for cid, d in dets.items()}
        hits = {cid: ks for cid, ks in hits.items() if ks}
        ids = sorted(hits)
        m["checks"] = ids
        m["hits"] = hits
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
                rid = r["run_id"]
                ms = milestones(art, calls, tasks, f"{rid[:4]}-{rid[4:6]}-{rid[6:8]}")
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
                           "milestones": [{k: m[k] for k in ("lane", "sig", "label", "call", "candidates", "linked_by", "checks", "scenarios", "match", "hits", "result") if m.get(k) is not None}
                                          for m in ms],
                           "calls": {str(c["n"]): {"tool": c["tool"], "text": show_call(c), "error": c["error"]}
                                     for c in calls if any(c["n"] in m["candidates"] for m in ms)},
                           "n_calls": len(calls)})
        r0 = rows[0]
        v = r0.get("versions") or {}
        old = sh("git", "show", f"{v['case_commit']}:cases/{case['id']}/expected.org", cwd=EVALS) if v.get("case_commit") else ""
        trees[tree] = {"run": r0["run_id"], "validation": bool(r0.get("validation")), "commit": r0["tree"]["commit"][:7], "case_commit": (v.get("case_commit") or "")[:7],
                       "items_then": parse_items(old) if old else [], "cites": r0.get("cites") or {}, "trials": trials,
                       # the checks under each rubric item: since 2026-11-14 a check cites no scenario of its own,
                       # and reaches one through the item it sits under
                       "item_checks": {i["item"]: sorted(i.get("checks") or []) for i in r0.get("rubric") or [] if i.get("item")}}
    return trees


# ---------------------------------------------------------------- the page

def summary(capability, spec, cases):
    """The page header's coverage line, computed as the page computes it (spec.template.html, status()), for the
    index: a scenario passes when every trial of every item citing it passed on the first tree with a run."""
    tree = next((t for t in TREES if any(c["runs"].get(t) for c in cases)), None)
    n = cited = passing = failing = 0
    for r in spec["requirements"]:
        for sc in r["scenarios"]:
            key = f"{capability} § {sc['title']}"
            n += 1
            cited += any(key in it["cites"] for c in cases for it in c["items"])
            p = k = 0
            for c in cases:
                run = c["runs"].get(tree)
                if not run:
                    continue
                for iid, cs in run["cites"].items():
                    if re.search(r"-E\d+$", iid) and key in cs:
                        for t in run["trials"]:
                            if iid in t["items"]:
                                k += 1
                                p += t["items"][iid] == "pass"
            passing += bool(k) and p == k
            failing += bool(k) and p < k
    return {"title": spec["title"], "tree": tree, "scenarios": n, "cited": cited, "pass": passing, "fail": failing,
            "cases": len(cases), "validation": sum(1 for c in cases if (c["runs"].get(tree) or {}).get("validation"))}


def build_index():
    """index.html: every page in this directory, its spec's title and coverage line, from each data file's summary."""
    pages = []
    for f in sorted(os.listdir(HERE)):
        if f.endswith(".data.js") and f != "index.data.js":
            d = json.loads(open(f"{HERE}/{f}").read()[len("window.SPEC_PAGE = "):-2])
            pages.append(dict(d["summary"], capability=d["capability"]))
    js = f"window.SPEC_INDEX = {json.dumps({'pages': pages}, indent=1, sort_keys=True, ensure_ascii=False)};\n"
    open(f"{HERE}/index.data.js", "w").write(js)
    tpl = open(f"{HERE}/index.template.html").read()
    open(f"{HERE}/index.html", "w").write(tpl.replace("{{data_version}}", hashlib.sha1(js.encode()).hexdigest()[:10]))
    print(f"index: {len(pages)} page(s) -> index.html")


def build(capability):
    spec_path = f"{SPECS}/{capability}.org"
    spec = parse_spec(spec_path)
    head = sh("git", "rev-parse", "--short", "HEAD", cwd=WORKTREE)
    dirty = bool(sh("git", "status", "--porcelain", "--", spec_path, cwd=WORKTREE))
    evals_head = sh("git", "rev-parse", "--short", "HEAD", cwd=EVALS)
    rows = history()
    cases = []
    for c in load_cases(capability, rows):
        tasks = sorted(set(re.findall(r"=([a-z]+-[a-z]+(?:-[a-z]+)?)=", open(f"{EVALS}/cases/{c['id']}/case.org").read())))
        c["runs"] = run_data(c, latest_counting(rows, c["id"], c["validated"]), tasks)
        cases.append(c)
    titles = {f"{capability} § {s['title']}" for r in spec["requirements"] for s in r["scenarios"]}
    unknown = sorted({s for c in cases for it in c["items"] for s in it["cites"]
                      if s.startswith(capability + " § ") and s not in titles})
    data = {"capability": capability, "spec": spec, "trees": list(TREES),
            "source": {"spec": f"orchestrate/docs/specs/{capability}.org", "worktree_head": head, "spec_dirty": dirty,
                       "evals_head": evals_head},
            "cases": cases, "unknown_citations": unknown}
    data["summary"] = summary(capability, spec, cases)
    js = f"window.SPEC_PAGE = {mask(json.dumps(data, indent=1, sort_keys=True, ensure_ascii=False))};\n"
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
    build_index()
    sys.exit(1 if bad else 0)
