#!/usr/bin/env python3
"""orchestrate home tool: the home, its repositories and the changes open in it.

Outside the state layer (docs/specs/state-layer.org § Integration Points): it writes the home's
config.yaml, a change's worktree and change.json, and .orchestrator/.gitignore, and says where
things are. It reads no cycle and checks no gate; `state.py status` does that.

Every verb prints JSON (`where --sh` and `worktree --sh` print shell). A refusal prints
{"ok": false, "error": "<slug>", "message": "..."} on stderr and exits 1.

Usage: python3 home.py [--home PATH] [--change NAME] <verb> ...   (home.py -h lists the verbs)

The home is the directory holding .claude/orchestrator/config.yaml (found by walking up from
cwd); it holds .orchestrator/ and .tasks/. A change's code root is its worktree, recorded in
<home>/.orchestrator/changes/<change>/change.json.
"""

import argparse
import glob
import json
import os
import re
import shlex
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from state import Refusal, find_change, now_iso, parse_yaml_subset  # noqa: E402  one resolution for every tool
import state as state_layer  # noqa: E402


def sibling_script(name):
    """Path of a script beside this one, ~-collapsed under $HOME: the installed tree gives
    ~/.claude/skills/orchestrate/scripts/<name>, a dev worktree gives its own, so what `home init`
    writes into config.yaml runs the tools of the tree that wrote it."""
    p = os.path.join(os.path.dirname(os.path.abspath(__file__)), name)
    home = os.path.expanduser("~")
    return "~" + p[len(home):] if p.startswith(home + os.sep) else p


def find_home(explicit=None):
    """(home, found): the home state.py finds (the nearest ancestor of cwd with
    .claude/orchestrator/config.yaml, or --home); (cwd, False) when there is none, which only `init` accepts."""
    try:
        return state_layer.find_home(explicit), True
    except Refusal:
        return os.getcwd(), False


def resolve_change(home, explicit=None):
    return find_change(home, explicit)


def config_path(home):
    return os.path.join(home, ".claude", "orchestrator", "config.yaml")


def read_config(home):
    """config.yaml as a dict; {} when the home has none."""
    p = config_path(home)
    if not os.path.exists(p):
        return {}
    return parse_yaml_subset(open(p, encoding="utf-8").read())


def read_code_root(home):
    """(raw value, absolute path) of config.yaml's top-level `code-root:`, the one-repository
    shorthand; (None, None) when absent or empty. A list is refused: several repositories go
    under `repos:` (overlay.md § Repositories)."""
    p = config_path(home)
    if not os.path.exists(p):
        return None, None
    lines = open(p, encoding="utf-8").read().split("\n")
    for i, line in enumerate(lines):
        m = re.match(r"^code-root:\s*(.*?)\s*$", line)
        if not m:
            continue
        raw = re.split(r"\s+#", m.group(1))[0].strip()
        if raw.startswith("#"):
            raw = ""
        nxt = lines[i + 1] if i + 1 < len(lines) else ""
        if raw.startswith("[") or (raw == "" and re.match(r"^\s*-\s", nxt)):
            raise Refusal("bad_code_root", "code-root takes one path, the one-repository shorthand; "
                          "several repositories are named under repos: (overlay.md § Repositories)")
        raw = raw.strip("\"'")
        if not raw:
            return None, None
        return raw, os.path.abspath(os.path.join(home, os.path.expanduser(raw)))
    return None, None


WORKTREE_PLACEHOLDERS = ("<repo>", "<branch>")


def resolve_home_config(home, cfg=None):
    """The home's shape and its repositories, from config.yaml (overlay.md § Repositories):
    {shape: workspace | code-root | repo-root, repos: {name: {path, from_config, language, build,
    test, worktree}}, worktrees: {parent, from_config, pattern}, code_root_raw, problems}.
    `repos:` names each repository; `code-root:` is the one-repository shorthand; neither means
    the home is the repository. Worktrees are placed from the home, never from a code root."""
    cfg = read_config(home) if cfg is None else cfg
    problems = []

    def absolute(raw):
        return os.path.abspath(os.path.join(home, os.path.expanduser(str(raw))))

    def merged(spec):
        out = {}
        for k in ("build", "test", "worktree"):
            base = cfg.get(k) if isinstance(cfg.get(k), dict) else {}
            own = spec.get(k) if isinstance(spec.get(k), dict) else {}
            out[k] = dict(base, **own)
        return out

    repos, code_root_raw = {}, None
    if cfg.get("repos") is not None:
        shape = "workspace"
        block = cfg.get("repos")
        if not isinstance(block, dict) or not block:
            problems.append(("bad_repos", "repos: is a map, <name>: {path: <dir>, ...} (overlay.md § Repositories)"))
        else:
            for name, spec in block.items():
                spec = spec if isinstance(spec, dict) else {}
                if not spec.get("path"):
                    problems.append(("bad_repo", f"repos.{name} needs path: <dir>, relative to the home"))
                    continue
                repos[name] = {"path": absolute(spec["path"]), "from_config": spec["path"],
                               "language": spec.get("language") or cfg.get("language"), **merged(spec)}
    else:
        code_root_raw, path = read_code_root(home)
        shape = "code-root" if path else "repo-root"
        path = path or home
        name = os.path.basename(os.path.realpath(path)) or "repo"
        repos[name] = {"path": path, "from_config": code_root_raw, "language": cfg.get("language"), **merged({})}
    wt = cfg.get("worktrees") if isinstance(cfg.get("worktrees"), dict) else {}
    parent_raw = wt.get("parent") or ".worktrees"
    pattern = wt.get("pattern") or ("<repo>/<branch>" if shape == "workspace" else "<branch>")
    if "<branch>" not in pattern:
        problems.append(("bad_worktree_pattern", f"worktrees.pattern {pattern!r} must contain <branch>"))
    if len(repos) > 1 and "<repo>" not in pattern:
        problems.append(("bad_worktree_pattern", f"worktrees.pattern {pattern!r} must contain <repo> when the "
                         "home has several repositories (two of them may both have a main)"))
    return {"shape": shape, "repos": repos, "code_root_raw": code_root_raw,
            "worktrees": {"parent": absolute(parent_raw), "from_config": parent_raw, "pattern": pattern},
            "problems": problems}


def worktree_path_for(hc, repo, branch):
    rel = hc["worktrees"]["pattern"].replace("<repo>", repo).replace("<branch>", branch)
    return os.path.normpath(os.path.join(hc["worktrees"]["parent"], rel))


def _same_path(a, b):
    return os.path.realpath(a) == os.path.realpath(b)


def changes_root(home):
    return os.path.join(home, ".orchestrator", "changes")


def change_json_path(home, name):
    return os.path.join(changes_root(home), name, "change.json")


def read_change(home, name):
    p = change_json_path(home, name)
    if not os.path.exists(p):
        return None
    try:
        rec = json.load(open(p, encoding="utf-8"))
    except (json.JSONDecodeError, OSError):
        return None
    return rec if isinstance(rec, dict) and rec.get("change") == name else None


def list_changes(home):
    out = []
    for p in sorted(glob.glob(os.path.join(changes_root(home), "*", "change.json"))):
        rec = read_change(home, os.path.basename(os.path.dirname(p)))
        if rec:
            out.append(rec)
    return out


def _atomic_write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        f.write(text)
        f.flush()
        os.fsync(f.fileno())
    os.replace(tmp, path)


def _git(repo_root, *args):
    try:
        r = subprocess.run(["git", "-C", repo_root, *args], capture_output=True, text=True)
        return r.returncode, r.stdout
    except OSError:
        return None, ""


def _git_ignores(repo_root, rel):
    rc, _ = _git(repo_root, "check-ignore", "-q", "--", rel)
    return rc == 0


def _git_branch(code_root):
    """The code root's current branch (a linked worktree's .git is a file, so ask git)."""
    rc, out = _git(code_root, "rev-parse", "--abbrev-ref", "HEAD")
    if rc != 0 or not out.strip():
        return "main"
    ref = out.strip()
    if ref == "HEAD":
        rc, sha = _git(code_root, "rev-parse", "--short=12", "HEAD")
        return sha.strip() if rc == 0 and sha.strip() else "main"
    return ref


def _is_worktree_root(path):
    rc, out = _git(path, "rev-parse", "--show-toplevel")
    return rc == 0 and out.strip() and _same_path(out.strip(), path)


def repo_problems(hc):
    """[(slug, message)] about the repositories config.yaml names; empty for the home itself."""
    out = list(hc["problems"])
    slug = "code_root" if hc["shape"] == "code-root" else "repo"  # the shorthand keeps its old slugs
    for name, r in hc["repos"].items():
        if r.get("from_config") is None:
            continue  # the home is the repository; nothing to check
        key = "code-root" if hc["shape"] == "code-root" else f"repos.{name}.path"
        if not os.path.isdir(r["path"]):
            out.append((f"{slug}_missing", f"config.yaml {key}: {r['from_config']} -> {r['path']} is not a directory"))
            continue
        rc, _ = _git(r["path"], "rev-parse", "--show-toplevel")
        if rc != 0:
            out.append((f"{slug}_not_git", f"repository {name} at {r['path']} is not a git work tree"))
    return out


def change_problems(store):
    """[(slug, message)] about the resolved change's code root, its worktree; empty when it is the home."""
    if not store.split:
        return []
    cr, home = store.code_root, store.home
    if not os.path.isdir(cr):
        return [("code_root_missing", f"change {store.change}: worktree {cr} is not a directory; "
                 "`change open` again, or fix change.json")]
    rc, _ = _git(cr, "rev-parse", "--show-toplevel")
    out = []
    if rc != 0:
        out.append(("code_root_not_git", f"code root {cr} is not a git work tree (a linked worktree is fine)"))
    if os.path.exists(os.path.join(cr, ".orchestrator")):
        out.append(("misplaced_artifacts", f".orchestrator/ found in the code root {cr}; artifacts live at "
                    f"the home {home}. Move it there (a plain mv; the code repo's .gitignore usually "
                    "ignores it) and open the change again"))
    if os.path.exists(config_path(cr)):
        out.append(("two_homes", f"the code root {cr} has its own .claude/orchestrator/config.yaml; one "
                    f"home per code root. Move that overlay to {config_path(home)} or drop the repository "
                    "from the home's config.yaml"))
    return out


def home_warnings(store):
    out = []
    if os.path.isdir(os.path.join(store.home, "openspec", "specs")):
        out.append(f"openspec/specs/ is still in the home {store.home}; this skill reads the record from "
                   "docs/ in the code root (templates/spec.md). Convert it first with the openspec-convert "
                   "skill")
    hc = store.hc
    if not hc or _git(store.home, "rev-parse", "--show-toplevel")[0] != 0:
        return out
    nested = [(name, r["path"]) for name, r in hc["repos"].items() if r.get("from_config") is not None]
    if os.path.isdir(hc["worktrees"]["parent"]):
        nested.append(("worktrees", hc["worktrees"]["parent"]))
    for name, path in nested:
        rel = os.path.relpath(path, store.home)
        if rel.startswith("..") or rel == ".":
            continue
        if not _git_ignores(store.home, rel):
            what = "the worktrees folder" if name == "worktrees" else f"the repository {name}"
            out.append(f"the home repo does not ignore {what} {rel}/; add it to {store.home}/.gitignore "
                       "so no code checkout appears as an embedded repository in the home's history")
    return out


def where_sh(result, hc):
    lines = [f"export ORCH_HOME={shlex.quote(result['home'])}"]
    if hc and hc["worktrees"].get("parent"):
        lines.append(f"export ORCH_WORKTREES={shlex.quote(hc['worktrees']['parent'])}")
    if result.get("change"):
        lines += [f"export ORCH_CHANGE={shlex.quote(result['change'])}",
                  f"export ORCH_CHANGE_DIR={shlex.quote(result['change_dir'])}",
                  f"export ORCH_REPO={shlex.quote(result['repo'])}",
                  f"export CODE_ROOT={shlex.quote(result['code_root'])}"]
    return "\n".join(lines)


def verb_worktree(store, args):
    """The path a worktree of the change's repository takes for a branch (overlay.md § Worktrees)."""
    _require_change(store)
    path = worktree_path_for(store.hc, store.repo, args.branch)
    return {"ok": True, "path": path, "repo": store.repo, "branch": args.branch,
            "parent": store.hc["worktrees"]["parent"], "pattern": store.hc["worktrees"]["pattern"],
            "exists": os.path.exists(path)}


def _require_change(store):
    if not store.hc:
        raise Refusal("no_home", f"no .claude/orchestrator/config.yaml found walking up from {os.getcwd()}; "
                      "create the overlay at the intended home (the repo, or the workspace above it) or "
                      "pass --home")
    if not store.change:
        names = [c["change"] for c in list_changes(store.home)]
        raise Refusal("no_change", "which change? pass --change <name>, set ORCH_CHANGE, or run from inside "
                      f"its worktree; open ones: {names or 'none (`change open <name>` opens one)'}",
                      changes=names)
    if not store.change_rec:
        raise Refusal("change_not_open", f"no change {store.change!r} under {changes_root(store.home)}; "
                      "`change open <name>` opens one (or adopts its worktree with --worktree)")


def _git_full(repo_root, *args):
    try:
        r = subprocess.run(["git", "-C", repo_root, *args], capture_output=True, text=True)
        return r.returncode, r.stdout, r.stderr
    except OSError as e:
        return None, "", str(e)


def verb_change_open(store, args):
    """Open a change: its worktree of one repository, on a branch named after it, and its
    directory under .orchestrator/changes/<change>/ with change.json. With --worktree an existing
    checkout is adopted as it is (its branch is recorded). Refuses while the change is open."""
    if not store.hc:
        _require_change(store)
    name = args.name
    if not re.match(r"^[A-Za-z0-9][A-Za-z0-9._-]*$", name):
        raise Refusal("bad_change_name", "a change name is one path segment: letters, digits, . _ -")
    if read_change(store.home, name):
        raise Refusal("change_exists", f"change {name} is already open: {change_json_path(store.home, name)}")
    hc = store.hc
    problems = repo_problems(hc)
    if problems:
        raise Refusal(problems[0][0], problems[0][1], problems=[{"error": a, "message": b} for a, b in problems])
    repos = hc["repos"]
    repo = args.repo
    if not repo:
        if len(repos) != 1:
            raise Refusal("which_repo", f"pass --repo <name>; this home has {sorted(repos)}")
        repo = next(iter(repos))
    if repo not in repos:
        raise Refusal("unknown_repo", f"no repository {repo!r} in config.yaml; it has {sorted(repos)}")
    r = repos[repo]
    adopted, created, from_ref = False, False, None
    if args.worktree:
        path = os.path.abspath(os.path.expanduser(args.worktree))
        if not os.path.isdir(path):
            raise Refusal("worktree_missing", f"--worktree {path} is not a directory")
        adopted, from_ref = True, args.from_ref  # recorded, not acted on: the branch exists
    else:
        path = worktree_path_for(hc, repo, name)
        if os.path.exists(path):
            if not _is_worktree_root(path):
                raise Refusal("worktree_occupied", f"{path} exists and is not a git work tree root")
            if _git_branch(path) != name:
                raise Refusal("worktree_occupied", f"{path} is checked out on {_git_branch(path)!r}, not {name!r}; "
                              "adopt it with --worktree, or remove it")
            adopted, from_ref = True, args.from_ref
        else:
            from_ref = args.from_ref or "HEAD"
            os.makedirs(os.path.dirname(path), exist_ok=True)
            rc, _o, err = _git_full(r["path"], "rev-parse", "--verify", "--quiet", f"refs/heads/{name}")
            if rc == 0:
                rc, _o, err = _git_full(r["path"], "worktree", "add", path, name)
            else:
                rc, _o, err = _git_full(r["path"], "worktree", "add", "-b", name, path, from_ref)
            if rc != 0:
                raise Refusal("git_failed", f"git worktree add failed in {r['path']}: {err.strip()}")
            created = True
    if _same_path(path, store.home) and hc["shape"] != "repo-root":
        raise Refusal("worktree_is_home", "the home is not a code root unless it is the repository "
                      "(config.yaml without repos: or code-root:)")
    if not _same_path(path, store.home):
        if os.path.exists(os.path.join(path, ".orchestrator")):
            raise Refusal("misplaced_artifacts", f".orchestrator/ found in {path}; artifacts live at the home "
                          f"{store.home}. Move it there and open the change again")
        if os.path.exists(config_path(path)):
            raise Refusal("two_homes", f"{path} has its own .claude/orchestrator/config.yaml; one home per "
                          "code root. Move that overlay to the home or drop it")
    base = _change_base(path, from_ref)
    intent = os.path.join(path, "docs", "changes", name, "intent.org")
    if base and os.path.isfile(intent):
        m = re.search(r"^#\+BASE:\s*([0-9a-fA-F]{7,40})", open(intent, encoding="utf-8").read(), re.M)
        if m and not (base.startswith(m.group(1)) or m.group(1).startswith(base)):
            raise Refusal("base_mismatch", f"{intent} says #+BASE: {m.group(1)} but the branch's base is "
                          f"{base} (the merge-base with {from_ref or 'HEAD'}); correct the intent, or pass "
                          "the --from ref the change was cut from", intent_base=m.group(1), base=base)
    rec = {"change": name, "repo": repo, "branch": _git_branch(path), "worktree": path,
           "base": base, "from": from_ref, "opened_at": now_iso(), "adopted": adopted}
    os.makedirs(os.path.dirname(change_json_path(store.home, name)), exist_ok=True)
    _atomic_write(change_json_path(store.home, name), json.dumps(rec, indent=2) + "\n")
    gitignore_written = _write_orchestrator_gitignore(store)
    new = Store(store.home, name, path, repo)
    new.hc, new.change_rec = hc, rec
    init_hook = _run_worktree_init(hc, r, name, store.home, path) if not getattr(args, "no_init", False) else None
    return {"ok": True, **rec, "created": created, "path": change_json_path(store.home, name),
            "init_hook": init_hook, "gitignore_written": gitignore_written,
            "problems": [{"error": a, "message": b} for a, b in change_problems(new)],
            "warnings": home_warnings(new)}


def _change_base(path, from_ref):
    """The commit the change starts from: the merge-base of its branch with the ref it was cut from
    (an adopted branch may already carry its hand-off commit, so HEAD is not it), else HEAD. The
    record: `change open --from workspace-notes-location` wrote the hand-off commit as the base
    while intent.org held the fork point; coverage.py --since then saw 3 scenarios, not 34."""
    rc, sha = _git(path, "rev-parse", "HEAD")
    head = sha.strip() if rc == 0 and sha.strip() else None
    if head and from_ref and from_ref != "HEAD":
        rc, mb = _git(path, "merge-base", from_ref, "HEAD")
        if rc == 0 and mb.strip():
            return mb.strip()
    return head


def _run_worktree_init(hc, repo_spec, change, home, path):
    """Run the overlay's worktree.init hook inside the change's worktree (flows/execute.md § 1 runs it
    for task worktrees; the design round's probes run in this one). The hook sees ORCH_HOME,
    ORCH_CHANGE and CODE_ROOT. A failure is reported, not a refusal: the change is open."""
    hook = (repo_spec.get("worktree") or {}).get("init") or (hc.get("worktree") or {}).get("init")
    if not hook:
        return None
    env = dict(os.environ, ORCH_HOME=home, ORCH_CHANGE=change, CODE_ROOT=path)
    try:
        r = subprocess.run(hook, shell=True, cwd=path, env=env, capture_output=True, text=True)
        return {"hook": hook, "rc": r.returncode, "stderr_tail": r.stderr[-400:]}
    except OSError as e:
        return {"hook": hook, "rc": None, "stderr_tail": str(e)}


OVERLAY_VERSION = 1


# overlay.md § config.yaml schema. A key not here is drift: either removed (REMOVED_KEYS says why)
# or never part of the schema.
OVERLAY_KEYS = {
    "overlay-version": None, "project": None, "language": None,
    "repos": {"*": {"path", "language", "build", "test", "worktree"}},
    "code-root": None,
    "build": {"pre-commit"},
    "test": {"command", "parser", "compare", "pending-style"},
    "worktrees": {"parent", "pattern"},
    "worktree": {"init", "needs-submodules"},
    "artifacts": {"tasks-root", "externalised-root", "specs-root", "intent-pattern", "design-pattern"},
    "taxonomy": None,
    "thresholds": {"phase-agent-context"},
    "critical-path": {"inference", "override-tasks", "override-labels"},
    "architect": {"drift-hotspots"},
    "reviewer": {"checks"},
    "asks": {"reader"},
    "priors": None,
}


REMOVED_KEYS = {
    "scaffolding": "scaffolding.md and the scaffolding gates were removed (2026-10-09); the design round's pending tests replaced them",
    "artifacts.register": "the interfaces register was replaced by design.org's Seams table (2026-09-28); the record lives under docs/",
    "architect.interfaces-document": "the interfaces register was replaced by design.org's Seams table (2026-09-28)",
    "architect.severity-overrides": "severity is the reporter's and routing is by the change's main path (templates/architect-finding.md, 2026-10-14)",
    "roles-overlay": "role overlays are found by name under roles/ (overlay.md § Layout); the key named nothing",
    "worktree.parent": "moved to worktrees.parent, relative to the home, with worktrees.pattern (2026-10-15)",
    "thresholds.history-window": "the PM digest's signals were retired with it (2026-10-28); delete it",
    "thresholds.drainage-trigger-ratio": "the PM digest's signals were retired with it (2026-10-28); delete it",
    "thresholds.drainage-trigger-consecutive-cycles": "the PM digest's signals were retired with it (2026-10-28); delete it",
    "thresholds.cascade-trigger-followup-count": "the PM digest's signals were retired with it (2026-10-28); delete it",
    "thresholds.stale-task-cycles": "the PM digest's signals were retired with it (2026-10-28); delete it",
    "thresholds.review-starvation-ratio": "the PM digest's signals were retired with it (2026-10-28); delete it",
    "thresholds.externalisation-pressure-growth": "the PM digest's signals were retired with it (2026-10-28); delete it",
}


# Words in a role overlay or priors file that name a retired concept; the file is the user's and
# is reviewed by hand, so this is a warning naming the words.
RETIRED_CONCEPTS = ["interfaces register", "interfaces.org", "register entry", "register entries", "entry_tier",
                    "entry tier", "load-bearing", "scaffolding", "scaffold", "register_touched", "architecture.md",
                    "cites_register_entries"]


def overlay_drift(home, cfg):
    """Warnings about config.yaml and the role overlays against the skill's schema."""
    out = []
    if not cfg:
        return out
    v = cfg.get("overlay-version")
    if v is None:
        out.append(f"config.yaml has no overlay-version; the skill's is {OVERLAY_VERSION}. Check its keys against "
                   "overlay.md (the warnings below), then add `overlay-version: 1` (overlay.md § Drift)")
    elif str(v) != str(OVERLAY_VERSION):
        out.append(f"config.yaml is overlay-version {v}; the skill's is {OVERLAY_VERSION} (overlay.md § Drift)")

    def removed(path):
        why = REMOVED_KEYS.get(path)
        return f"config.yaml {path}: {why}; delete it" if why else None

    for key, val in cfg.items():
        if key in REMOVED_KEYS:
            out.append(removed(key))
            continue
        if key not in OVERLAY_KEYS:
            out.append(f"config.yaml {key}: not in overlay.md's schema; delete it or move it")
            continue
        sub = OVERLAY_KEYS[key]
        if not sub or not isinstance(val, dict):
            continue
        for k2, v2 in val.items():
            path = f"{key}.{k2}"
            if path in REMOVED_KEYS:
                out.append(removed(path))
            elif "*" in sub:
                if isinstance(v2, dict):
                    for k3 in v2:
                        if k3 not in sub["*"]:
                            out.append(f"config.yaml {key}.{k2}.{k3}: not in overlay.md's schema")
            elif k2 not in sub:
                out.append(f"config.yaml {path}: not in overlay.md's schema; delete it or move it")
    # a path that climbs from a code root to the home
    for key in ("build", "test", "worktree"):
        blocks = [(key, cfg.get(key))] + [(f"repos.{n}.{key}", (r or {}).get(key))
                                          for n, r in (cfg.get("repos") or {}).items()
                                          if isinstance(cfg.get("repos"), dict)]
        for name, block in blocks:
            if not isinstance(block, dict):
                continue
            for k2, v2 in block.items():
                if isinstance(v2, str) and re.search(r"(^|\s|=)\.\./", v2):
                    out.append(f"config.yaml {name}.{k2} climbs to the home with ../ ({v2!r}); the code root's depth is "
                               "the workspace's, not the skill's: write $ORCH_HOME/… (hooks see it) or ship the tool")
    # role overlays and priors naming retired concepts
    d = os.path.join(home, ".claude", "orchestrator")
    files = sorted(glob.glob(os.path.join(d, "roles", "*.md"))) + [os.path.join(d, "priors.md")]
    for f in files:
        if not os.path.exists(f):
            continue
        text = open(f, encoding="utf-8", errors="replace").read().lower()
        hits = sorted({w for w in RETIRED_CONCEPTS if w in text})
        if hits:
            out.append(f"{os.path.relpath(f, home)} names retired concepts ({', '.join(hits)}); rewrite those "
                       "parts against the current roles/*.md (overlay.md § Drift)")
    return [w for w in out if w]


def render_config(args, shape, repos):
    """config.yaml text for `home init`: the schema's required keys filled, the optional ones as comments."""
    lines = ["# Orchestrator overlay (overlay.md). Written by `home.py init`; edited by hand.",
             "# `home.py where` warns when a key here drifts from the skill's schema.",
             f"overlay-version: {OVERLAY_VERSION}", f"project: {args.project}", f"language: {args.language}", ""]
    tc = args.test_command or "make test"
    if shape == "workspace":
        lines += ["# The repositories: a change belongs to one and is worked in its own worktree of it",
                  "# (overlay.md § Repositories). path: a checkout of the repository, relative to the home or absolute.",
                  "repos:"]
        for name, path in repos:
            lines += [f"  {name}:", f"    path: {path}"]
        lines.append("")
    elif shape == "code-root":
        lines += ["# The one repository's checkout, relative to the home (overlay.md § Repositories).",
                  f"code-root: {repos[0][1]}", ""]
    else:
        lines += ["# No repos: or code-root: — this directory is the repository and the home (overlay.md § Repositories).", ""]
    lines += ["build:",
              "  # pre-commit: ./bin/tangle-org.sh   # OPTIONAL, run in the worktree before each commit",
              "test:",
              f"  command: {tc}",
              f"  parser: {args.parser}   # emacs-results-txt | exit-code | junit-xml | tap",
              ("  compare: python3 " + sibling_script("compare.py") + " --parser " + args.parser
               if args.parser != "exit-code" else
               "  # compare: python3 " + sibling_script("compare.py") + " --parser <parser>   # needed when the baseline is red; exit-code cannot compare"),
              "  # pending-style: xfail-strict | xit   # how the design round marks a pending test (default by runner)",
              "",
              "# Where worktrees go, from the home (overlay.md § Worktrees). The pattern is the workspace's convention.",
              "worktrees:",
              f"  parent: {args.worktrees_parent}",
              f"  pattern: {args.worktrees_pattern}",
              "",
              "worktree:",
              "  # init: ./bin/init-worktree-runtime.sh   # OPTIONAL, run inside a new worktree",
              "  # needs-submodules: false",
              "",
              "# OPTIONAL blocks, as overlay.md gives them: artifacts, taxonomy, thresholds, critical-path,",
              "# architect (drift-hotspots), reviewer (checks), asks (reader), priors.",
              "# taxonomy:",
              "#   - feature",
              "#   - test",
              "#   - doc",
              "#   - refactor",
              "#   - bug",
              "#   - contract",
              "#   - infrastructure",
              "# architect:",
              "#   drift-hotspots:",
              "#     - <module>: <what to watch>",
              "# reviewer:",
              "#   checks:",
              "#     - <check>",
              ""]
    return "\n".join(lines)


def verb_home_init(store, args):
    """Write a new home's .claude/orchestrator/config.yaml from the skill's schema (overlay.md
    § Bootstrap). Refuses when one exists. The home is --home or the cwd."""
    home = os.path.abspath(args.home or (store.home if store.home_found else os.getcwd()))
    cfg = config_path(home)
    if os.path.exists(cfg):
        raise Refusal("overlay_exists", f"{cfg} exists; edit it, or move it away to start over")
    repos = []
    for spec in args.repo or []:
        if "=" not in spec:
            raise Refusal("bad_repo_arg", "--repo takes <name>=<path>")
        name, path = spec.split("=", 1)
        if not re.match(r"^[A-Za-z0-9][A-Za-z0-9._-]*$", name) or not path:
            raise Refusal("bad_repo_arg", f"--repo {spec!r}: <name> is one path segment, <path> non-empty")
        repos.append((name, path))
    if args.code_root and repos:
        raise Refusal("bad_repo_arg", "--code-root is the one-repository shorthand; with --repo use neither")
    if args.code_root:
        shape, repos = "code-root", [(os.path.basename(os.path.normpath(args.code_root)), args.code_root)]
    elif repos:
        shape = "workspace"
    else:
        shape = "repo-root"
    for name, path in repos:
        p = os.path.abspath(os.path.join(home, os.path.expanduser(path)))
        if not os.path.isdir(p):
            raise Refusal("repo_missing", f"repository {name}: {path} -> {p} is not a directory")
        if _git(p, "rev-parse", "--show-toplevel")[0] != 0:
            raise Refusal("repo_not_git", f"repository {name}: {p} is not a git work tree")
    if shape == "repo-root" and _git(home, "rev-parse", "--show-toplevel")[0] != 0:
        raise Refusal("home_not_git", f"{home} is not a git work tree; a repository-root home is the repository. "
                      "Name the repositories with --repo <name>=<path> for a workspace")
    if not args.worktrees_pattern:
        args.worktrees_pattern = "<repo>/<branch>" if shape == "workspace" else "<branch>"
    if not args.project:
        args.project = os.path.basename(os.path.normpath(home))
    text = render_config(args, shape, repos)
    os.makedirs(os.path.dirname(cfg), exist_ok=True)
    os.makedirs(os.path.join(os.path.dirname(cfg), "roles"), exist_ok=True)
    _atomic_write(cfg, text)
    hc = resolve_home_config(home)
    gitignore = []
    if _git(home, "rev-parse", "--show-toplevel")[0] == 0:
        for name, r in hc["repos"].items():
            rel = os.path.relpath(r["path"], home)
            if r.get("from_config") is not None and not rel.startswith("..") and not _git_ignores(home, rel):
                gitignore.append(rel + "/")
        rel = os.path.relpath(hc["worktrees"]["parent"], home)
        if not rel.startswith("..") and not _git_ignores(home, rel):
            gitignore.append(rel + "/")
    return {"ok": True, "home": home, "config": cfg, "shape": shape,
            "repos": {n: r["path"] for n, r in hc["repos"].items()},
            "worktrees": hc["worktrees"], "problems": [{"error": a, "message": b} for a, b in hc["problems"]],
            "add_to_home_gitignore": gitignore,
            "next": "review config.yaml with the user (test command, parser, pending-style, hooks, taxonomy, "
                    "drift hot spots, reviewer checks); add roles/<role>.md and priors.md as overlay.md says; "
                    "then `home.py open <name>`"}


ORCHESTRATOR_GITIGNORE = """# Written by home.py open. Test output and scratch stay out of git; every other
# orchestration artifact (the event log, inbox, cycles, handshakes, the test runs' exit codes
# in *.status and *.compare, which `state.py check done` reads) is tracked.
baseline-*.txt
after-*.txt
*.tmp
"""


def _write_orchestrator_gitignore(store):
    gi = os.path.join(store.root, ".gitignore")
    os.makedirs(store.root, exist_ok=True)
    if os.path.exists(gi):
        return False
    _atomic_write(gi, ORCHESTRATOR_GITIGNORE)
    return True


def _change_summary(store, rec):
    out = {"name": rec["change"], "repo": rec.get("repo"), "branch": rec.get("branch"),
           "worktree": rec.get("worktree"), "base": rec.get("base"), "from": rec.get("from")}
    return out


def verb_where(store, args):
    """Where am I: the home, its repositories, its changes, the resolved change's code root and
    anything wrong with them."""
    cfg = config_path(store.home)
    hc = store.hc or {"shape": None, "repos": {}, "worktrees": {}, "problems": []}
    problems = repo_problems(hc) if store.hc else []
    if store.change and store.change_rec:
        problems += change_problems(store)
    elif store.change:
        problems.append(("change_not_open", f"no change {store.change!r} under {changes_root(store.home)}; "
                         "`change open <name>` opens one"))
    out = {"ok": True, "home": store.home, "home_found": store.home_found,
           "config": cfg if os.path.exists(cfg) else None,
           "shape": hc["shape"],
           "repos": {n: {"path": r["path"], "from_config": r.get("from_config"),
                         "test_command": r.get("test", {}).get("command")} for n, r in hc["repos"].items()},
           "worktrees": {"parent": hc["worktrees"].get("parent"), "pattern": hc["worktrees"].get("pattern")},
           "changes": [_change_summary(store, rec) for rec in list_changes(store.home)],
           "change": store.change if store.change_rec else None,
           "change_resolved_by": store.change_how if store.change_rec else None,
           "repo": store.repo if store.change_rec else None,
           "code_root": store.code_root if store.change_rec else None,
           "change_dir": store.dir if store.change_rec else None,
           "split": store.split if store.change_rec else None,
           "problems": [{"error": a, "message": b} for a, b in problems],
           "warnings": home_warnings(store) + overlay_drift(store.home, read_config(store.home))}
    return out

class Store:
    """The resolved home and change, as the verbs above read them."""

    def __init__(self, home, change=None, code_root=None, repo=None):
        self.home = home
        self.change = change
        self.repo = repo
        self.code_root = code_root or home
        self.split = not _same_path(self.code_root, self.home)
        self.home_found = True
        self.code_root_raw = None
        self.hc = None
        self.change_rec = None
        self.change_how = None
        self.root = os.path.join(home, ".orchestrator")
        self.dir = os.path.join(self.root, "changes", change) if change else self.root


def build_parser():
    p = argparse.ArgumentParser(prog="home.py", description=__doc__.split("\n\n")[0])
    p.add_argument("--home", help="the home (default: walk up from cwd for .claude/orchestrator/config.yaml)")
    p.add_argument("--change", dest="change_sel",
                   help="the change (default: $ORCH_CHANGE, the worktree holding cwd, the only one)")
    sub = p.add_subparsers(dest="verb", required=True)

    s = sub.add_parser("where", help="the home, its repositories, its open changes and the resolved change")
    s.add_argument("--sh", action="store_true", help="print export lines for the flows' bash blocks")
    s.set_defaults(fn=verb_where)
    s = sub.add_parser("init", help="write a new home's config.yaml from the skill's schema (overlay.md § Bootstrap)")
    s.add_argument("--repo", action="append", help="<name>=<path>, once per repository (a workspace home)")
    s.add_argument("--code-root", help="the one repository's checkout (the shorthand)")
    s.add_argument("--project")
    s.add_argument("--language", default="unknown")
    s.add_argument("--test-command")
    s.add_argument("--parser", default="exit-code", choices=["emacs-results-txt", "exit-code", "junit-xml", "tap"])
    s.add_argument("--worktrees-parent", default=".worktrees")
    s.add_argument("--worktrees-pattern")
    s.set_defaults(fn=verb_home_init)
    s = sub.add_parser("open", help="open a change: its worktree, branch and change.json")
    s.add_argument("name")
    s.add_argument("--repo")
    s.add_argument("--from", dest="from_ref", help="the ref a new branch is cut from (default HEAD)")
    s.add_argument("--worktree", help="adopt this checkout instead of creating one")
    s.add_argument("--no-init", action="store_true", help="skip the overlay's worktree.init hook")
    s.set_defaults(fn=verb_change_open)
    s = sub.add_parser("worktree", help="the path a worktree of the change's repository takes for a branch")
    s.add_argument("branch")
    s.add_argument("--sh", action="store_true", help="print WORKTREE_PATH=…")
    s.set_defaults(fn=verb_worktree)
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    hc = None
    try:
        home, found = find_home(args.home)
        if args.verb == "init":
            found = os.path.exists(config_path(home))
        hc = resolve_home_config(home) if found else None
        change, how = resolve_change(home, args.change_sel) if args.verb != "init" else (None, None)
        rec = read_change(home, change) if change else None
        store = Store(home, change, rec.get("worktree") if rec else None, rec.get("repo") if rec else None)
        store.hc, store.change_rec, store.change_how = hc, rec, how
        store.home_found, store.code_root_raw = found, (hc or {}).get("code_root_raw")
        result = args.fn(store, args)
    except Refusal as r:
        print(json.dumps({"ok": False, "error": r.slug, "message": r.message, **r.extra}, indent=2), file=sys.stderr)
        return 1
    if args.verb == "where" and args.sh:
        print(where_sh(result, hc))
        return 0
    if args.verb == "worktree" and args.sh:
        print(f"WORKTREE_PATH={shlex.quote(result['path'])}")
        return 0
    print(json.dumps(result, indent=2))
    return 0 if result.get("ok", True) else 1


if __name__ == "__main__":
    sys.exit(main())
