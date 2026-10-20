#!/usr/bin/env python3
"""Count a session's cost in the two currencies, for a proposals file's `* Cost` section
(SKILL.md § Counting the two currencies).

    cost.py [--project <dir>] [--session <id-prefix>] [--since <ISO time>] [--reading <file> ...]

Machine: tokens of the parent transcript and its subagents, each assistant message counted
once (a message is written as several records, each carrying the same usage), split into
input, cache write, cache read and output; wall clock from the first to the last record of the
parent; subagents spawned. The user's: typed prompts and question-tool calls (and the questions
in them) in the parent; lines and words of each `--reading` file.

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


def typed(o):
    """A prompt the user typed: not meta, not a command echo, a reminder or a tool result."""
    if o.get("type") != "user" or o.get("isMeta"):
        return False
    c = (o.get("message") or {}).get("content")
    if isinstance(c, str):
        return not c.lstrip().startswith("<")
    return any(b.get("type") == "text" and not b.get("text", "").lstrip().startswith("<")
               for b in c or [] if isinstance(b, dict))


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

    split = ", ".join(f"{k} {fmt(v)}" for k, v in total.items())
    print(f"- Machine :: {fmt(sum(total.values()))} tokens ({split}); "
          f"{minutes:.0f} min wall clock; {agents} agents spawned (session {parent.stem[:8]}).")
    reading = []
    for f in a.reading:
        text = pathlib.Path(f).read_text()
        reading.append(f"{f} {len(text.splitlines())} lines, {len(text.split())} words")
    print(f"- The user's :: {prompts} prompts typed; {calls} question calls ({questions} questions)"
          + (f"; reading handed over: {'; '.join(reading)}." if reading else "."))


if __name__ == "__main__":
    main()
