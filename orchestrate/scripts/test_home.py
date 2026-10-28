"""pytest suite for home.py (stdlib + pytest). Run: python3 -m pytest test_home.py (from this directory)."""

import importlib.util
import json
import os
import pathlib
import subprocess

import pytest

HERE = pathlib.Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location("home", HERE / "home.py")
home_py = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(home_py)

CH = ".orchestrator/changes/chg"


def _parse(text):
    text = (text or "").strip()
    try:
        return json.loads(text) if text else None
    except json.JSONDecodeError:
        return None


def _g(cwd, *args):
    env = {**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
           "GIT_COMMITTER_EMAIL": "t@t"}
    return subprocess.run(["git", "-C", str(cwd), *args], capture_output=True, text=True, check=True, env=env)


@pytest.fixture
def repo(tmp_path, capsys):
    """A repository-root home with the change `chg` adopted on the home's own checkout."""
    (tmp_path / ".claude" / "orchestrator").mkdir(parents=True)
    (tmp_path / ".claude" / "orchestrator" / "config.yaml").write_text("overlay-version: 1\nproject: scratch\n")
    _g(tmp_path, "init", "-q", "-b", "main")
    _g(tmp_path, "commit", "-q", "--allow-empty", "-m", "init")
    code = home_py.main(["--home", str(tmp_path), "open", "chg", "--worktree", str(tmp_path)])
    assert code == 0, capsys.readouterr().err
    capsys.readouterr()
    return tmp_path


def runner(home, capsys):
    def run(*argv):
        code = home_py.main(["--home", str(home), *argv])
        cap = capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)
    return run


def test_where_single_repo(repo, capsys):
    run = runner(repo, capsys)
    code, out, _ = run("where")
    assert code == 0 and out["split"] is False and out["home_found"] is True
    assert out["home"] == out["code_root"] == str(repo) and out["problems"] == [] and out["config"]
    assert out["shape"] == "repo-root" and list(out["repos"].values())[0]["path"] == str(repo)
    assert out["change"] == "chg" and out["change_resolved_by"] == "only"
    assert [c["name"] for c in out["changes"]] == ["chg"] and out["changes"][0]["worktree"] == str(repo)
    assert out["worktrees"] == {"parent": str(repo / ".worktrees"), "pattern": "<branch>"}
    assert out["warnings"] == []
    assert (repo / ".orchestrator" / ".gitignore").exists()
    (repo / "openspec" / "specs").mkdir(parents=True)
    code, out, _ = run("where")
    assert len(out["warnings"]) == 1 and "openspec-convert" in out["warnings"][0]


def test_where_sh_and_worktree_sh(repo, capsys):
    code = home_py.main(["--home", str(repo), "where", "--sh"])
    assert code == 0
    assert capsys.readouterr().out.splitlines() == [
        f"export ORCH_HOME={repo}", f"export ORCH_WORKTREES={repo / '.worktrees'}", "export ORCH_CHANGE=chg",
        f"export ORCH_CHANGE_DIR={repo / CH}", f"export ORCH_REPO={repo.name}", f"export CODE_ROOT={repo}"]
    home_py.main(["--home", str(repo), "worktree", "task-a-1", "--sh"])
    assert capsys.readouterr().out.strip() == f"WORKTREE_PATH={repo / '.worktrees' / 'task-a-1'}"


def test_read_code_root_parsing(repo):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    base = "overlay-version: 1\nproject: scratch\nworktree:\n  code-root: nested-not-this\n"
    cases = {
        "code-root: sub\n": ("sub", str(repo / "sub")),
        "code-root: 'quoted dir'  # comment\n": ("quoted dir", str(repo / "quoted dir")),
        'code-root: "/abs/elsewhere"\n': ("/abs/elsewhere", "/abs/elsewhere"),
        "code-root:\n": (None, None),
        "code-root: .\n": (".", str(repo)),
        "": (None, None),
    }
    for text, want in cases.items():
        cfg.write_text(base + text)
        assert home_py.read_code_root(str(repo)) == want, text
    cfg.write_text(base + "code-root: ~/x\n")
    assert home_py.read_code_root(str(repo))[1] == os.path.expanduser("~/x")
    for text in ("code-root: [a, b]\n", "code-root:\n  - a\n  - b\n"):
        cfg.write_text(base + text)
        with pytest.raises(home_py.Refusal) as e:
            home_py.read_code_root(str(repo))
        assert e.value.slug == "bad_code_root"


def test_parse_yaml_subset_and_resolve_repos(repo):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    cfg.write_text("""# a workspace home
project: ws
language: elisp
repos:
  emacs:
    path: ~/emacs
    build:
      pre-commit: ./bin/tangle.sh
    test:
      command: ./bin/run-tests.sh   # trailing comment
      pending-style: xit
  api:
    path: "repos/api"
    language: go
    test: {command: go test ./..., parser: exit-code}
test:
  parser: tap
  compare: python3 cmp.py
worktrees:
  parent: .worktrees
  pattern: <repo>/<branch>
architect:
  drift-hotspots:
    - module-system: "jf/resolve: contract"
    - tangle: watch it
taxonomy: [feature, test]
reviewer:
  checks:
    - cl-lib-vs-seq
asks:
  reader: "a PM"
""")
    cfg_d = home_py.read_config(str(repo))
    assert cfg_d["repos"]["emacs"]["test"] == {"command": "./bin/run-tests.sh", "pending-style": "xit"}
    assert cfg_d["repos"]["api"]["test"] == "{command: go test ./..., parser: exit-code}"  # flow maps stay strings
    assert cfg_d["architect"]["drift-hotspots"] == [{"module-system": "jf/resolve: contract"}, {"tangle": "watch it"}]
    assert cfg_d["taxonomy"] == ["feature", "test"] and cfg_d["reviewer"]["checks"] == ["cl-lib-vs-seq"]
    assert cfg_d["asks"]["reader"] == "a PM"
    hc = home_py.resolve_home_config(str(repo))
    assert hc["shape"] == "workspace" and sorted(hc["repos"]) == ["api", "emacs"]
    assert hc["repos"]["emacs"]["path"] == os.path.expanduser("~/emacs")
    assert hc["repos"]["emacs"]["test"] == {"parser": "tap", "compare": "python3 cmp.py",
                                            "command": "./bin/run-tests.sh", "pending-style": "xit"}
    assert hc["repos"]["api"]["path"] == str(repo / "repos" / "api") and hc["repos"]["api"]["language"] == "go"
    assert hc["worktrees"] == {"parent": str(repo / ".worktrees"), "from_config": ".worktrees", "pattern": "<repo>/<branch>"}
    assert home_py.worktree_path_for(hc, "api", "task-x-1") == str(repo / ".worktrees" / "api" / "task-x-1")
    assert hc["problems"] == []
    cfg.write_text("repos:\n  a:\n    path: a\n  b:\n    path: b\nworktrees:\n  pattern: <branch>\n")
    assert [p[0] for p in home_py.resolve_home_config(str(repo))["problems"]] == ["bad_worktree_pattern"]
    cfg.write_text("repos:\n  a:\n    language: go\n")
    assert [p[0] for p in home_py.resolve_home_config(str(repo))["problems"]] == ["bad_repo"]
    cfg.write_text("repos: []\n")
    assert [p[0] for p in home_py.resolve_home_config(str(repo))["problems"]] == ["bad_repos"]


def _workspace(tmp_path, extra=""):
    home = tmp_path / "ws"
    (home / ".claude" / "orchestrator").mkdir(parents=True)
    origin = tmp_path / "origin"
    origin.mkdir()
    _g(origin, "init", "-q", "-b", "main")
    (origin / "f.txt").write_text("x\n")
    _g(origin, "add", ".")
    _g(origin, "commit", "-qm", "init")
    _g(origin, "branch", "integration")
    (home / ".claude" / "orchestrator" / "config.yaml").write_text(
        f"project: ws\nrepos:\n  app:\n    path: {origin}\n    test:\n      command: make test\n" + extra +
        "worktrees:\n  parent: .worktrees\n  pattern: <repo>/<branch>\n")
    return home, origin


def test_open_creates_worktree_in_workspace(tmp_path, capsys):
    home, origin = _workspace(tmp_path)
    run = runner(home, capsys)
    code, out, _ = run("where")
    assert code == 0 and out["shape"] == "workspace" and out["changes"] == [] and out["change"] is None
    assert out["code_root"] is None and out["problems"] == []
    code, out, err = run("open", "feat-a", "--from", "integration")
    assert code == 0, err
    wt = home / ".worktrees" / "app" / "feat-a"
    assert out["created"] is True and out["worktree"] == str(wt) and out["branch"] == "feat-a"
    assert out["repo"] == "app" and out["from"] == "integration" and len(out["base"]) == 40
    assert wt.is_dir() and (wt / ".git").is_file() and _g(wt, "branch", "--show-current").stdout.strip() == "feat-a"
    assert json.loads((home / ".orchestrator" / "changes" / "feat-a" / "change.json").read_text())["change"] == "feat-a"
    code, _, err = run("open", "feat-a")
    assert code == 1 and err["error"] == "change_exists"
    code, out, err = run("open", "feat-b")
    assert code == 0, err
    code, _, err = run("worktree", "x")
    assert code == 1 and err["error"] == "no_change" and err["changes"] == ["feat-a", "feat-b"]
    code, out, _ = run("--change", "feat-b", "worktree", "task-t-9")
    assert out["path"] == str(home / ".worktrees" / "app" / "task-t-9")
    cwd = os.getcwd()
    os.chdir(wt)
    try:
        home_py.main(["where"])
        out = _parse(capsys.readouterr().out)
        assert out["home"] == str(home) and out["change"] == "feat-a" and out["change_resolved_by"] == "cwd"
    finally:
        os.chdir(cwd)
    code, out, err = run("open", "feat-c", "--worktree", str(origin), "--from", "integration")
    assert code == 0 and out["adopted"] is True and out["branch"] == "main" and out["created"] is False
    assert [c["from"] for c in run("where")[1]["changes"]] == ["integration", "HEAD", "integration"]
    code, _, err = run("open", "feat-d", "--worktree", str(origin / "nope"))
    assert code == 1 and err["error"] == "worktree_missing"
    code, _, err = run("open", "bad/name")
    assert code == 1 and err["error"] == "bad_change_name"
    code, _, err = run("open", "feat-e", "--repo", "nope")
    assert code == 1 and err["error"] == "unknown_repo"


def test_open_refuses_misplaced_artifacts_then_two_homes(tmp_path, capsys):
    home, origin = _workspace(tmp_path)
    run = runner(home, capsys)
    wt = tmp_path / "feat-m"
    _g(origin, "worktree", "add", "-q", "-b", "feat-m", str(wt), "integration")
    (wt / ".orchestrator").mkdir()
    code, _, err = run("open", "feat-m", "--worktree", str(wt))
    assert code == 1 and err["error"] == "misplaced_artifacts" and str(home) in err["message"]
    (wt / ".orchestrator").rmdir()
    (wt / ".claude" / "orchestrator").mkdir(parents=True)
    (wt / ".claude" / "orchestrator" / "config.yaml").write_text("name: old\n")
    code, _, err = run("open", "feat-m", "--worktree", str(wt))
    assert code == 1 and err["error"] == "two_homes"


def test_init_writes_config_for_each_shape(tmp_path, capsys):
    def run(home, *argv):
        return runner(home, capsys)(*argv)

    ws = tmp_path / "ws"
    ws.mkdir()
    for name in ("app", "api"):
        r = ws / "checkouts" / name
        r.mkdir(parents=True)
        _g(r, "init", "-q")
    code, out, err = run(ws, "init", "--repo", "app=checkouts/app", "--repo", "api=checkouts/api",
                         "--test-command", "make check", "--parser", "tap", "--language", "go")
    assert code == 0, err
    assert out["shape"] == "workspace" and sorted(out["repos"]) == ["api", "app"] and out["problems"] == []
    assert out["worktrees"]["pattern"] == "<repo>/<branch>" and out["add_to_home_gitignore"] == []
    assert out["next"].endswith("then `home.py open <name>`")
    text = (ws / ".claude" / "orchestrator" / "config.yaml").read_text()
    assert "overlay-version: 1" in text and "project: ws" in text and "  command: make check" in text
    assert "Written by `home.py init`" in text
    assert (ws / ".claude" / "orchestrator" / "roles").is_dir()
    code, out, _ = run(ws, "where")
    assert out["shape"] == "workspace" and out["warnings"] == [] and out["problems"] == []
    code, _, err = run(ws, "init")
    assert code == 1 and err["error"] == "overlay_exists"
    code, out, err = run(ws, "open", "c1", "--repo", "app")
    assert code == 1 and err["error"] == "git_failed"  # an empty repository has no HEAD to branch from
    r = ws / "checkouts" / "app"
    (r / "f").write_text("x")
    _g(r, "add", ".")
    _g(r, "commit", "-qm", "i")
    code, out, err = run(ws, "open", "c1", "--repo", "app")
    assert code == 0, err
    assert out["worktree"] == str(ws / ".worktrees" / "app" / "c1")

    rr = tmp_path / "rr"
    rr.mkdir()
    _g(rr, "init", "-q")
    code, out, err = run(rr, "init", "--test-command", "pytest")
    assert code == 0, err
    assert out["shape"] == "repo-root" and list(out["repos"].values()) == [str(rr)]
    assert out["worktrees"]["pattern"] == "<branch>" and out["add_to_home_gitignore"] == [".worktrees/"]

    cr = tmp_path / "cr"
    (cr / "emacs").mkdir(parents=True)
    _g(cr / "emacs", "init", "-q")
    _g(cr, "init", "-q")
    code, out, err = run(cr, "init", "--code-root", "emacs", "--parser", "emacs-results-txt")
    assert code == 0, err
    assert out["shape"] == "code-root" and out["repos"] == {"emacs": str(cr / "emacs")}
    assert out["add_to_home_gitignore"] == ["emacs/", ".worktrees/"]

    nogit = tmp_path / "nogit"
    nogit.mkdir()
    code, _, err = run(nogit, "init")
    assert code == 1 and err["error"] == "home_not_git"
    code, _, err = run(nogit, "init", "--repo", "x=missing")
    assert code == 1 and err["error"] == "repo_missing"
    code, _, err = run(nogit, "init", "--repo", "bad")
    assert code == 1 and err["error"] == "bad_repo_arg"


def test_where_warns_on_overlay_drift(repo, capsys):
    cfg = repo / ".claude" / "orchestrator" / "config.yaml"
    cfg.write_text("""project: p
language: elisp
code-root: .
test:
  command: ./t
  compare: python3 ../../.orchestrator/tools/compare-baseline.py
scaffolding:
  enabled: false
artifacts:
  register: interfaces.org
architect:
  interfaces-document: interfaces.org
roles-overlay:
  implementor: roles/implementor.md
worktree:
  parent: .worktrees
mystery: 1
thresholds:
  history-window: 5
  phase-agent-context: 300000
  nope: 2
""")
    roles = repo / ".claude" / "orchestrator" / "roles"
    roles.mkdir()
    (roles / "architect.md").write_text("Check the interfaces register and interfaces.org for load-bearing entries.\n")
    home_py.main(["--home", str(repo), "where"])
    w = "\n".join(_parse(capsys.readouterr().out)["warnings"])
    for needle in ("no overlay-version", "scaffolding:", "artifacts.register:", "architect.interfaces-document:",
                   "roles-overlay:", "worktree.parent:", "mystery:", "thresholds.nope:",
                   "thresholds.history-window: the PM digest", "test.compare climbs",
                   "roles/architect.md names retired concepts (interfaces register, interfaces.org, load-bearing)"):
        assert needle in w, needle
    assert "phase-agent-context" not in w
    cfg.write_text("overlay-version: 1\nproject: p\ntest:\n  command: ./t\n")
    (roles / "architect.md").write_text("fine\n")
    home_py.main(["--home", str(repo), "where"])
    assert _parse(capsys.readouterr().out)["warnings"] == []


def test_open_base_is_merge_base_and_runs_init_hook(tmp_path, capsys):
    """An adopted branch that already carries its hand-off commit has the fork point as its base;
    an intent whose #+BASE: disagrees refuses; the overlay's worktree.init runs in a new worktree."""
    home, origin = _workspace(tmp_path, "worktree:\n  init: printf hook > init-ran.txt\n")
    run = runner(home, capsys)
    fork = _g(origin, "rev-parse", "integration").stdout.strip()
    wt = tmp_path / "feat-x"
    _g(origin, "worktree", "add", "-q", "-b", "feat-x", str(wt), "integration")
    (wt / "docs" / "changes" / "feat-x").mkdir(parents=True)
    (wt / "docs" / "changes" / "feat-x" / "intent.org").write_text(f"#+TITLE: feat-x\n#+BASE: {fork}\n")
    _g(wt, "add", ".")
    _g(wt, "commit", "-qm", "Record: feat-x intent")
    code, out, err = run("open", "feat-x", "--worktree", str(wt), "--from", "integration")
    assert code == 0, err
    assert out["base"] == fork and out["adopted"] is True
    assert out["init_hook"]["rc"] == 0 and (wt / "init-ran.txt").read_text() == "hook"
    wt2 = tmp_path / "feat-y"
    _g(origin, "worktree", "add", "-q", "-b", "feat-y", str(wt2), "integration")
    (wt2 / "docs" / "changes" / "feat-y").mkdir(parents=True)
    (wt2 / "docs" / "changes" / "feat-y" / "intent.org").write_text("#+TITLE: feat-y\n#+BASE: 0123456789ab\n")
    _g(wt2, "add", ".")
    _g(wt2, "commit", "-qm", "Record: feat-y intent")
    code, _, err = run("open", "feat-y", "--worktree", str(wt2), "--from", "integration")
    assert code == 1 and err["error"] == "base_mismatch" and err["base"] == fork and err["intent_base"] == "0123456789ab"
    code, out, err = run("open", "feat-z", "--from", "integration", "--no-init")
    assert code == 0, err
    assert out["base"] == fork and out["created"] is True and out["init_hook"] is None
