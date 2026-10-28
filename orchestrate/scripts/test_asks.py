"""pytest suite for asks.py (stdlib + pytest). Run: python3 -m pytest test_asks.py (from this directory)."""

import importlib.util
import json
import pathlib
import subprocess

import pytest

HERE = pathlib.Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location("asks", HERE / "asks.py")
asks = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(asks)

FIX = HERE / "fixtures" / "asks"
CH = ".orchestrator/changes/chg"
# A complete decision ask as templates/ask.md requires one: about, observed, two to four options.
ASK = {"kind": "decision", "raised_by": {"role": "pm", "ref": None}, "question": "Which way?",
       "about": "A thing the user sees.", "observed": {"happened": True, "evidence": "seen in a run"},
       "options": [{"label": "Do it", "consequence": "it is done"},
                   {"label": "Do nothing", "consequence": "stays as is"}]}


def _parse(text):
    text = (text or "").strip()
    try:
        return json.loads(text) if text else None
    except json.JSONDecodeError:
        return None


def _g(cwd, *args):
    return subprocess.run(["git", "-C", str(cwd), "-c", "user.email=t@t", "-c", "user.name=t", *args],
                          capture_output=True, text=True)


@pytest.fixture
def repo(tmp_path):
    """A git home with the change `chg` open on the home itself and cycle-1 started in its log."""
    (tmp_path / ".claude" / "orchestrator").mkdir(parents=True)
    (tmp_path / ".claude" / "orchestrator" / "config.yaml").write_text("overlay-version: 1\nproject: t\n")
    _g(tmp_path, "init", "-q", "-b", "main")
    _g(tmp_path, "commit", "-q", "--allow-empty", "-m", "init")
    d = tmp_path / CH
    d.mkdir(parents=True)
    (d / "change.json").write_text(json.dumps({"change": "chg", "branch": "main", "worktree": str(tmp_path)}))
    (d / "events.jsonl").write_text(json.dumps(
        {"seq": 1, "at": "2026-10-28T00:00:00Z", "cycle": "cycle-1", "kind": "cycle-started"}) + "\n")
    return tmp_path


@pytest.fixture
def run(repo, capsys):
    def _run(*argv):
        code = asks.main(["--home", str(repo), "--change", "chg", *argv])
        cap = capsys.readouterr()
        return code, _parse(cap.out), _parse(cap.err)
    return _run


def test_ask_org_roundtrip_matches_golden():
    rec = json.loads((FIX / "download-video.json").read_text())
    text = (FIX / "download-video.org").read_text()
    assert asks.ask_to_org(rec) == text
    assert asks.ask_from_org(asks.AskDoc(text)) == rec


def test_add_from_org_draft_keeps_prose(run, repo):
    draft = repo / "draft.org"
    text = (FIX / "download-video.org").read_text()
    text = text.replace(":ID: ask-cycle-1790026644-9\n", "").replace("* About\n", "* About\n\nMy own words.\n")
    draft.write_text(text)
    code, out, err = run("add", f"@{draft}")
    assert code == 0, err
    assert out["id"] == "ask-cycle-1-1" and out["path"] == f"{CH}/inbox/ask-cycle-1-1.org"
    body = (repo / out["path"]).read_text()
    assert ":ID: ask-cycle-1-1" in body and "My own words." in body and body.count("* Answer") == 1


def test_add_terminal_lands_in_cycle_dir_and_seq_skips_files(run, repo):
    doc = {"kind": "doc-correction", "raised_by": {"role": "orchestrator", "ref": None}, "question": "Fix D4?",
           "status": "applied", "decision": "Applied without asking: stage numbers corrected.",
           "applied_via": "design.md D4"}
    code, out, _ = run("add", json.dumps(doc))
    assert code == 0 and out["path"] == f"{CH}/cycles/cycle-1/asks/ask-cycle-1-1.org"
    assert (repo / out["path"]).exists() and not (repo / CH / "inbox").exists()
    (repo / CH / "inbox").mkdir()
    (repo / CH / "inbox" / "ask-cycle-1-2.org").write_text("stray\n")
    code, out, _ = run("add", json.dumps(ASK))
    assert code == 0 and out["id"] == "ask-cycle-1-3"  # a file already there is never overwritten
    code, _, err = run("add", json.dumps({**ASK, "id": "ask-cycle-1-2"}))
    assert code == 1 and err["error"] == "duplicate_id"


def test_add_needs_a_cycle(run, repo):
    (repo / CH / "events.jsonl").unlink()
    code, _, err = run("add", json.dumps(ASK))
    assert code == 1 and err["error"] == "no_cycle"
    code, out, err = run("add", json.dumps(ASK), "--cycle", "cycle-9")
    assert code == 0 and out["id"] == "ask-cycle-9-1"


def test_set_updates_drawer_and_moves(run, repo):
    run("add", json.dumps(ASK))
    code, _, err = run("set", "ask-cycle-1-1", "status=answered")
    assert code == 1 and err["error"] == "answer_required"
    code, out, _ = run("set", "ask-cycle-1-1", "decision=Do it", 'blocks=["t1"]')
    assert code == 0 and out["status"] == "open" and out["changed"] == ["blocks", "decision"]
    text = (repo / out["path"]).read_text()
    assert ":BLOCKS: t1" in text and "\nDo it\n" in text and "# An option label" not in text
    code, _, err = run("set", "ask-cycle-1-1", "status=applied")
    assert code == 1 and err["error"] == "applied_via_required"
    code, out, _ = run("set", "ask-cycle-1-1", "status=applied", "applied_via=commit abc")
    assert code == 0 and out["moved_via"] == "rename"
    assert out["path"] == f"{CH}/cycles/cycle-1/asks/ask-cycle-1-1.org"
    assert not (repo / CH / "inbox" / "ask-cycle-1-1.org").exists()
    assert ":APPLIED_VIA: commit abc" in (repo / out["path"]).read_text()
    code, _, err = run("set", "ask-cycle-1-1", "path=x")
    assert code == 1 and err["error"] == "not_settable"


def test_set_git_mv_when_tracked(run, repo):
    run("add", json.dumps(ASK))
    _g(repo, "add", ".")
    _g(repo, "commit", "-qm", "x")
    code, out, _ = run("set", "ask-cycle-1-1", "decision=Do nothing", "status=declined")
    assert code == 0 and out["moved_via"] == "git mv"
    tracked = _g(repo, "ls-files").stdout
    assert out["path"] in tracked and f"{CH}/inbox/ask-cycle-1-1.org" not in tracked


def test_add_revised_from_supersedes(run, repo):
    run("add", json.dumps(ASK))
    code, out, _ = run("add", json.dumps({**ASK, "revised_from": "ask-cycle-1-1"}))
    assert code == 0 and out["id"] == "ask-cycle-1-2" and out["superseded"]["id"] == "ask-cycle-1-1"
    old = repo / CH / "cycles" / "cycle-1" / "asks" / "ask-cycle-1-1.org"
    assert ":STATUS: superseded" in old.read_text()
    assert ":REVISED_FROM: ask-cycle-1-1" in (repo / out["path"]).read_text()
    code, _, err = run("add", json.dumps({**ASK, "revised_from": "ask-cycle-1-7"}))
    assert code == 1 and err["error"] == "no_such_ask"


def test_inbox_reports_answers_and_tolerates_edits(run, repo):
    for _ in range(4):
        run("add", json.dumps(ASK))
    inbox = repo / CH / "inbox"

    def answer(n, text):
        p = inbox / f"ask-cycle-1-{n}.org"
        p.write_text(p.read_text().replace("* Answer\n", f"* Answer\n\n{text}\n"))
    answer(1, "do it.")  # exact after normalisation
    answer(2, "Do nothin")  # fuzzy
    answer(3, "Half of it; see ask-cycle-1-4 for the rest")  # free text that names another ask
    # 4: lowercase title before the drawer, wrapped prose, a Discussion with a ** turn placed
    # before Answer, the answer written in the discussion, CRLF line ends
    p = inbox / "ask-cycle-1-4.org"
    t = p.read_text()
    t = t.replace("* About\n\nA thing the user sees.\n", "* About\n\nA thing\nthe user sees.\n")
    t = t.replace("* Answer\n\n# An option", "* Discussion\n\n** 2026-10-09 user\n\nDo it\n\n* Answer\n\n# An option")
    t = "#+title: Which way?\n" + t.replace("#+TITLE: Which way?\n", "")
    p.write_text(t.replace("\n", "\r\n"))
    code, out, _ = run("inbox")
    assert code == 0 and out["answered"] == ["ask-cycle-1-1", "ask-cycle-1-2", "ask-cycle-1-3"]
    by = {e["id"]: e for e in out["asks"]}
    assert by["ask-cycle-1-1"]["answer"] == {"text": "do it.", "label": "Do it", "match": "exact"}
    a2 = by["ask-cycle-1-2"]["answer"]
    assert a2["match"] == "fuzzy" and a2["label"] == "Do nothing"
    a3 = by["ask-cycle-1-3"]
    assert a3["answer"]["match"] == "none" and a3["mentions"] == ["ask-cycle-1-4"]
    e4 = by["ask-cycle-1-4"]
    assert e4["answer"] is None and e4["problems"] == [] and e4["question"] == "Which way?"
    assert e4["answer_candidate"] == {"section": "Discussion", "text": "Do it"}
    assert out["problems"] == [] and not any(e["tracked"] for e in out["asks"])
    code, out, _ = run("set", "ask-cycle-1-4", "decision=Do it", "status=answered")
    assert code == 0
    kept = (repo / out["path"]).read_text()
    assert "** 2026-10-09 user" in kept and "A thing\nthe user sees." in kept and "\r" not in kept


def test_inbox_reports_a_file_in_the_wrong_place(run, repo):
    run("add", json.dumps(ASK))
    p = repo / CH / "inbox" / "ask-cycle-1-1.org"
    p.write_text(p.read_text().replace(":STATUS: open", ":STATUS: applied"))
    code, out, _ = run("inbox")
    assert out["ok"] is False and "belongs in cycles/<id>/asks/" in out["problems"][0]


def test_set_readback_confirmed(run, repo):
    code, out, err = run("add", json.dumps(ASK))
    aid = out["id"]
    p = repo / CH / "inbox" / f"{aid}.org"
    p.write_text(p.read_text().replace("* Answer\n", "* Answer\n\nSomething else entirely\n"))
    code, _, err = run("set", aid, "readback=confirmed")
    assert code == 1 and err["error"] == "no_readback"
    assert run("set", aid, "decision_readback=It will be done.")[0] == 0
    code, out, err = run("set", aid, "readback=confirmed")
    assert code == 0, err
    assert "readback" in out["changed"] and "It will be done.\n\nyes\n" in p.read_text()
    assert run("inbox")[1]["asks"][0]["readback"] == {"present": True, "confirmed": True}
    code, _, err = run("set", aid, "readback=maybe")
    assert code == 1 and err["error"] == "bad_value"


def test_add_takes_ref_and_string_raised_by(run, repo):
    rec = {k: v for k, v in ASK.items() if k != "raised_by"}
    code, out, err = run("add", json.dumps({**rec, "raised_by": "reviewer", "ref": "cycles/cycle-1/reviews/t.md"}))
    assert code == 0, err
    drawer = asks.AskDoc((repo / out["path"]).read_text())
    assert (drawer.prop("RAISED_BY"), drawer.prop("REF")) == ("reviewer", "cycles/cycle-1/reviews/t.md")
    code, _, err = run("add", json.dumps({**rec, "raised_by": {"role": "pm", "ref": "a"}, "ref": "b"}))
    assert code == 1 and err["error"] == "bad_record"


def test_state_layer_reads_what_asks_writes(run, repo):
    """The state layer's reader and this writer agree on kind, status, blocks and the answer."""
    run("add", json.dumps({**ASK, "blocks": ["t1"]}))
    run("set", "ask-cycle-1-1", "decision=Do it")
    a = asks.state_layer.Ask(str(repo / CH / "inbox" / "ask-cycle-1-1.org"))
    assert (a.id, a.kind, a.status, a.blocks, a.answer_label()) == ("ask-cycle-1-1", "decision", "open", ["t1"], "Do it")
