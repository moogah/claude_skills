import json

import cost


def write(path, recs):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(r) + "\n" for r in recs))


def asst(mid, ts, out, content=None):
    usage = {"input_tokens": 1, "cache_creation_input_tokens": 10, "cache_read_input_tokens": 100,
             "output_tokens": out}
    return {"type": "assistant", "timestamp": ts,
            "message": {"id": mid, "usage": usage, "content": content or []}}


def user(ts, content, meta=False):
    return {"type": "user", "timestamp": ts, "isMeta": meta, "message": {"content": content}}


ASK = [{"type": "tool_use", "name": "AskUserQuestion", "input": {"questions": [{}, {}]}}]


def session(tmp_path):
    proj = tmp_path / "proj"
    write(proj / "s1.jsonl", [
        user("2026-10-20T10:00:00Z", "<command-name>/model</command-name>"),
        user("2026-10-20T10:00:01Z", "Write the skill"),
        asst("m1", "2026-10-20T10:01:00Z", 5),
        asst("m1", "2026-10-20T10:01:00Z", 5, ASK),  # same message, second record
        user("2026-10-20T10:20:00Z", [{"type": "tool_result", "content": "x"}]),
        user("2026-10-20T10:30:00Z", [{"type": "text", "text": "yes"}]),
        asst("m2", "2026-10-20T11:00:00Z", 7),
    ])
    write(proj / "s1" / "subagents" / "agent-a.jsonl", [asst("a1", "2026-10-20T10:10:00Z", 3)])
    return proj


def test_message_counted_once_and_subagents_added(tmp_path, capsys):
    cost.main(["--project", str(session(tmp_path))])
    machine, users = capsys.readouterr().out.splitlines()
    # three messages, each 111 in + output 5, 7, 3
    assert "348 tokens" in machine and "output 15" in machine
    assert "60 min wall clock" in machine and "1 agents spawned" in machine
    assert "2 prompts typed" in users and "1 question calls (2 questions)" in users


def test_since_and_reading(tmp_path, capsys):
    f = tmp_path / "p.org"
    f.write_text("one two\nthree\n")
    cost.main(["--project", str(session(tmp_path)), "--since", "2026-10-20T10:25:00Z",
               "--reading", str(f)])
    machine, users = capsys.readouterr().out.splitlines()
    assert "118 tokens" in machine and "0 agents spawned" in machine
    assert "1 prompts typed" in users and "2 lines, 3 words" in users


def test_skill_command_and_mid_turn_message_are_typed(tmp_path, capsys):
    # the record of 2026-10-20: a /process-improvement run counted 0 prompts where there were 2
    proj = tmp_path / "proj"
    write(proj / "s2.jsonl", [
        user("2026-10-20T10:00:00Z", "<command-message>process-improvement</command-message>\n"
             "<command-name>/process-improvement</command-name>\n<command-args>go</command-args>"),
        user("2026-10-20T10:00:01Z", [{"type": "text", "text": "Base directory for this skill"}],
             meta=True),
        {"type": "queue-operation", "operation": "enqueue", "timestamp": "2026-10-20T10:05:00Z",
         "content": "stop before making changes"},
        {"type": "attachment", "timestamp": "2026-10-20T10:05:30Z",
         "attachment": {"type": "queued_command", "prompt": "stop before making changes"}},
        {"type": "attachment", "timestamp": "2026-10-20T10:06:00Z",
         "attachment": {"type": "prompt_snapshot"}},
        asst("m1", "2026-10-20T10:07:00Z", 5),
    ])
    cost.main(["--project", str(proj)])
    _, users = capsys.readouterr().out.splitlines()
    assert "2 prompts typed" in users
