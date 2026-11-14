#!/usr/bin/env python3
"""Count a session's cost in the two currencies, for a proposals file's `* Cost` section
(SKILL.md § Counting the two currencies).

    cost.py [--project <dir>] [--session <id-prefix>] [--since <ISO time>] [--reading <file> ...]

Machine: tokens of the parent transcript and its subagents, each assistant message counted
once (a message is written as several records, each carrying the same usage), split into
input, cache write, cache read and output; wall clock from the first to the last record of the
parent, and working time: the wall clock less the waits on the user (each question call's wait
for its answer, and each gap before a message the user typed); subagents spawned. The user's:
typed prompts and question-tool calls (and the questions in them) in the parent; lines and words
of each `--reading` file; and the attention cost (ATTENTION below).

A typed prompt is a message the user wrote. Since Claude Code records where a message came from
(`origin.kind`), only `human` counts: a background agent's hand-back (`peer`), a task notification
and a skill's text are delivered to the session, not typed, and the wait before them is the
session's own. Transcripts without `origin` fall back to the shape of the message.

With no --project, the project directory of the cwd under ~/.claude/projects/; with no
--session, the most recently written transcript there (the running session). --since counts
only records at or after that time.
"""

import argparse
import datetime
import json
import pathlib
import re
import sys

PROJECTS = pathlib.Path.home() / ".claude" / "projects"
USAGE = (("input", "input_tokens"), ("cache write", "cache_creation_input_tokens"),
         ("cache read", "cache_read_input_tokens"), ("output", "output_tokens"))


def when(s):
    return datetime.datetime.fromisoformat(s.replace("Z", "+00:00"))


def records(path, since):
    with open(path) as fh:
        for line in fh:
            try:
                o = json.loads(line)
            except json.JSONDecodeError:
                continue
            ts = o.get("timestamp")
            if since and (not ts or when(ts) < since):
                continue
            yield o


def tokens(path, since):
    seen, sums, stamps = set(), {k: 0 for k, _ in USAGE}, []
    for o in records(path, since):
        if o.get("timestamp"):
            stamps.append(when(o["timestamp"]))
        if o.get("type") != "assistant":
            continue
        m = o.get("message") or {}
        mid = m.get("id") or o.get("uuid")
        if mid in seen or not m.get("usage"):
            continue
        seen.add(mid)
        for k, field in USAGE:
            sums[k] += m["usage"].get(field) or 0
    return sums, stamps


ATTENTION = """The attention cost, as the evals state it (evals/lib/attention.py, approved 2026-11-14):
the sum over responses of 1 + reading minutes + avoidability, plus the handed reading's minutes, at
238 words a minute. A response here is a question call or a typed prompt; what the user read for it
is the session's text since the previous response (and the call's questions and options). The
handed reading is the text after the last response and the --reading files. Avoidability is 0:
nothing rates it outside the evals. Permission prompts leave no record in a transcript and are not
counted."""
WPM = 238


def origin(o):
    a = o.get("attachment") or {}
    return (a.get("origin") if o.get("type") == "attachment" else o.get("origin")) or None


def typed(o):
    """A prompt the user typed: a message, a command that sends a prompt (`<command-message>`, a
    skill's `/name args`), or a message queued mid-turn (a `queued_command` attachment); not meta,
    a local command's echo, a reminder, a tool result, or a message delivered to the session (an
    agent's hand-back, a task notification), which `origin` tells apart where it is recorded."""
    org = origin(o)
    if o.get("type") == "attachment":
        if (o.get("attachment") or {}).get("type") != "queued_command":
            return False
        return org is None or org.get("kind") == "human"
    if o.get("type") != "user":
        return False
    if org is not None:
        return org.get("kind") == "human"
    if o.get("isMeta"):
        return False
    c = (o.get("message") or {}).get("content")
    texts = [c] if isinstance(c, str) else [b.get("text", "") for b in c or []
                                             if isinstance(b, dict) and b.get("type") == "text"]
    return any(not t.lstrip().startswith("<") or t.lstrip().startswith("<command-message>")
               for t in texts)


def user_side(path, since):
    prompts = calls = questions = 0
    for o in records(path, since):
        if typed(o):
            prompts += 1
        if o.get("type") == "assistant":
            for b in (o.get("message") or {}).get("content") or []:
                if isinstance(b, dict) and b.get("type") == "tool_use" and b.get("name") == "AskUserQuestion":
                    calls += 1
                    questions += len((b.get("input") or {}).get("questions") or [1])
    return prompts, calls, questions


def words(text):
    return len(re.findall(r"\S+", text or ""))


def attention(path, since, reading_words=0):
    """Working-time waits and the attention cost's parts from the parent transcript. Returns
    (seconds waited on the user, responses [{kind, words}], handed words)."""
    asked, waited, out, text, last = {}, 0.0, [], [], None
    for o in records(path, since):
        ts = when(o["timestamp"]) if o.get("timestamp") else None
        if o.get("type") == "assistant":
            for b in (o.get("message") or {}).get("content") or []:
                if not isinstance(b, dict):
                    continue
                if b.get("type") == "text":
                    text.append(b.get("text", ""))
                elif b.get("type") == "tool_use" and b.get("name") == "AskUserQuestion":
                    if b.get("id"):
                        asked[b["id"]] = ts
                    qs = (b.get("input") or {}).get("questions") or []
                    shown = "\n".join(text + [q.get("question", "") + " " + " ".join(
                        f"{x.get('label', '')} {x.get('description', '')}" for x in q.get("options") or [])
                        for q in qs])
                    out.append({"kind": "question", "words": words(shown)}); text = []
        elif o.get("type") == "user":
            c = (o.get("message") or {}).get("content")
            for b in c if isinstance(c, list) else []:
                tid = b.get("tool_use_id") if isinstance(b, dict) else None
                if b.get("type") == "tool_result" and tid and tid in asked:
                    t0 = asked.pop(tid)
                    if t0 and ts:
                        waited += (ts - t0).total_seconds()
            if typed(o):
                if last and ts:
                    waited += (ts - last).total_seconds()
                out.append({"kind": "message", "words": words("\n".join(text))}); text = []
        elif typed(o):                  # a message queued mid-turn: a response, but the session was working
            out.append({"kind": "message", "words": words("\n".join(text))}); text = []
        if ts and o.get("type") == "assistant":   # the session's last word; local commands and
            last = ts                               # attachments between it and a typed prompt are the user's time
    return waited, out, words("\n".join(text)) + reading_words


def attention_cost(responses, handed):
    return sum(1 + r["words"] / WPM for r in responses) + handed / WPM


def project_dir(arg):
    if arg:
        return pathlib.Path(arg).expanduser()
    return PROJECTS / re.sub(r"[^A-Za-z0-9]", "-", str(pathlib.Path.cwd()))


def session_path(project, prefix):
    found = sorted(project.glob("*.jsonl"), key=lambda p: p.stat().st_mtime)
    if prefix:
        found = [p for p in found if p.name.startswith(prefix)]
        if len(found) != 1:
            sys.exit(f"session prefix {prefix!r} matches {len(found)} transcripts in {project}")
    if not found:
        sys.exit(f"no transcript in {project}; name the directory with --project")
    return found[-1]


def fmt(n):
    return f"{n / 1e6:.1f}M" if n >= 1e6 else f"{n / 1e3:.0f}k" if n >= 1e3 else str(n)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--project")
    ap.add_argument("--session")
    ap.add_argument("--since", type=when)
    ap.add_argument("--reading", nargs="*", default=[])
    a = ap.parse_args(argv)

    parent = session_path(project_dir(a.project), a.session)
    total, stamps = tokens(parent, a.since)
    agents = 0
    for sub in sorted((parent.parent / parent.stem / "subagents").glob("*.jsonl")):
        sums, sub_stamps = tokens(sub, a.since)
        if sub_stamps:
            agents += 1
            for k in total:
                total[k] += sums[k]
    minutes = (max(stamps) - min(stamps)).total_seconds() / 60 if stamps else 0
    prompts, calls, questions = user_side(parent, a.since)
    reading, rwords = [], 0
    for f in a.reading:
        text = pathlib.Path(f).read_text()
        reading.append(f"{f} {len(text.splitlines())} lines, {len(text.split())} words")
        rwords += words(text)
    waited, responses, handed = attention(parent, a.since, rwords)
    working = max(minutes - waited / 60, 0)

    split = ", ".join(f"{k} {fmt(v)}" for k, v in total.items())
    print(f"- Machine :: {fmt(sum(total.values()))} tokens ({split}); "
          f"{minutes:.0f} min wall clock, {working:.0f} min working; {agents} agents spawned "
          f"(session {parent.stem[:8]}).")
    print(f"- The user's :: {prompts} prompts typed; {calls} question calls ({questions} questions)"
          + (f"; reading handed over: {'; '.join(reading)}" if reading else "")
          + f"; attention cost {attention_cost(responses, handed):.1f} ({len(responses)} responses, "
          f"{sum(r['words'] for r in responses) + handed} words read).")


if __name__ == "__main__":
    main()
