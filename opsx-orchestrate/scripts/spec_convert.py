#!/usr/bin/env python3
"""Convert the markdown OpenSpec record to the org living record (roadmap 2.1).

A spec file becomes org by the record's form (templates/spec.md): `#+TITLE: <capability>`,
markdown `##`..`#####` headings one level up (`## Requirements` -> `* Requirements`,
`### Requirement:` -> `** Requirement:`, `#### Scenario:` -> `*** Scenario:`), heading text
byte for byte. Body text: `**x**` -> `*x*`, `*x*` -> `/x/`, inline code -> `=x=` (`~x~`
when it holds `=`), fences -> `#+begin_src <lang>` / `#+begin_example` (code lines escaped
as org escapes them), tables -> org tables, `>` quotes -> `#+begin_quote`, `---` -> `-----`,
relative `.md` links -> `[[file:….org]]`. Anything kept as written but worth an eye is
listed in the report.

Verbs (each prints a plain-text report on stdout; exit 1 on a refusal):
  tree SPECS_DIR OUT_DIR       openspec/specs/ -> docs/specs/: a capability directory with
                               one file becomes <cap>.org, with several <cap>/<file>.org
  change CHANGE_DIR OUT_DIR    proposal.md -> intent.org, design.md -> design.org
  apply CHANGE_DIR SPECS_ORG [--base BASE_ORG]
                               write a change's delta specs (CHANGE_DIR/specs/<cap>/spec.md)
                               into the org specs in place: ADDED appended under
                               `* Requirements`, MODIFIED replaced; when BASE_ORG (the org
                               form of the specs the delta was written against) holds a
                               MODIFIED block that differs from SPECS_ORG's, the block is a
                               three-way merge (git merge-file), reported, conflicts marked
  file SRC OUT [--cap ID] [--kind spec|change]
  count PATH...                requirements and scenarios per file (.md or .org)
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile


class Refusal(Exception):
    pass


# ---------------------------------------------------------------- inline

_PH = "\x00{}\x00"
_PH_RE = re.compile("\x00(\\d+)\x00")
# Emphasis opens after start, space or opening punctuation and closes before space or
# closing punctuation, holding no `*`; bare glob stars (`/workspace/**/*.el`) stay literal.
_PRE = r"(?:(?<=^)|(?<=[\s(\[\"'\x00]))"
_POST = r"(?=$|[\s.,;:!?)\]\"'\x00])"
_BOLD = re.compile(_PRE + r"\*\*(?=[^\s*])([^*]+?)(?<=[^\s*])\*\*" + _POST, re.M)
_ITALIC = re.compile(_PRE + r"\*(?=[^\s*])([^*]+?)(?<=[^\s*])\*" + _POST, re.M)
_LINK = re.compile(r"\[([^\]\n]+)\]\(([^)\s]+)\)")


def _code_spans(text):
    """Yield (start, end, content) for CommonMark backtick code spans in TEXT."""
    i, n = 0, len(text)
    while i < n:
        if text[i] != "`":
            i += 1
            continue
        j = i
        while j < n and text[j] == "`":
            j += 1
        run = j - i
        k = j
        close = None
        while k < n:
            if text[k] == "`":
                m = k
                while m < n and text[m] == "`":
                    m += 1
                if m - k == run:
                    close = (k, m)
                    break
                k = m
            else:
                k += 1
        if close is None:
            i = j
            continue
        content = text[j:close[0]]
        if run > 1 and len(content) > 2 and content[0] == " " and content[-1] == " " and content.strip():
            content = content[1:-1]
        yield i, close[1], content
        i = close[1]


def org_verbatim(content, notes, where):
    """CONTENT as an org verbatim (=x=) or code (~x~) object."""
    if "=" in content and "~" in content:
        notes.append(("code with both = and ~", where, content))
    marker = "~" if "=" in content else "="
    if content != content.strip():
        notes.append(("code with edge space", where, content))
    if "\n" in content:
        notes.append(("code across a line break", where, content.replace("\n", "⏎")))
    return marker + content + marker


def inline(text, notes, where, link_ext=True):
    """Convert one markdown paragraph's inline markup to org."""
    held = []

    def hold(s):
        held.append(s)
        return _PH.format(len(held) - 1)

    out, last = [], 0
    for start, end, content in _code_spans(text):
        out.append(text[last:start])
        out.append(hold(org_verbatim(content, notes, where)))
        last = end
    out.append(text[last:])
    text = "".join(out)

    def link(m):
        desc, target = m.group(1), m.group(2)
        if link_ext and "://" not in target and target.endswith(".md"):
            new = target[:-3] + ".org"
            if desc == target:
                desc = new
            notes.append(("relative .md link", where, target))
            target = new
        return hold(f"[[file:{target}][{desc}]]")

    text = _LINK.sub(link, text)

    def restore(s):
        while _PH_RE.search(s):
            s = _PH_RE.sub(lambda m: held[int(m.group(1))], s)
        return s

    def emphasis(marker, body):
        if body.count("\n") > 1:
            notes.append(("emphasis over more than two lines (org shows it plain)", where,
                          restore(body).split("\n")[0][:60]))
        return hold(marker + body + marker)

    text = _BOLD.sub(lambda m: emphasis("*", m.group(1)), text)
    text = _ITALIC.sub(lambda m: emphasis("/", m.group(1)), text)
    lone = [k for k, ln in enumerate(text.split("\n")) if "*" in ln]
    text = restore(text)
    for k in lone:
        notes.append(("* kept literal in prose", where, text.split("\n")[k].strip()[:90]))
    return text


# ---------------------------------------------------------------- blocks

_HEADING = re.compile(r"^(#{1,6})\s+(.*?)\s*$")
_FENCE = re.compile(r"^(\s*)(`{3,})\s*([\w+-]*)\s*$")
_RULE = re.compile(r"^(?:-{3,}|\*{3,}|_{3,})\s*$")
_TABLE_SEP = re.compile(r"^\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)*\|?\s*$")
_ORG_ESCAPE = re.compile(r"^([ \t]*)(,*(?:\*|#\+))")
_STEP_NOBULLET = re.compile(r"^\*\*(WHEN|THEN|AND|BUT|OR|GIVEN)\*\*")


def _split_row(line):
    """Cells of a markdown table row, `\\|` kept as a literal bar."""
    body = line.strip()
    if body.startswith("|"):
        body = body[1:]
    if body.endswith("|") and not body.endswith("\\|"):
        body = body[:-1]
    cells, cur, i = [], [], 0
    while i < len(body):
        if body[i] == "\\" and i + 1 < len(body) and body[i + 1] == "|":
            cur.append("|")
            i += 2
            continue
        if body[i] == "|":
            cells.append("".join(cur).strip())
            cur = []
        else:
            cur.append(body[i])
        i += 1
    cells.append("".join(cur).strip())
    return cells


def _org_table(rows, notes, where):
    conv = []
    for kind, cells in rows:
        if kind == "sep":
            conv.append(None)
            continue
        new = []
        for c in cells:
            o = inline(c, notes, where)
            if "|" in o:
                notes.append(("| in a table cell -> \\vert{}", where, c))
                o = o.replace("|", "\\vert{}")
            new.append(o)
        conv.append(new)
    width = max(len(r) for r in conv if r is not None)
    cols = [0] * width
    for r in conv:
        if r is not None:
            for i, c in enumerate(r):
                cols[i] = max(cols[i], len(c))
    lines = []
    for r in conv:
        if r is None:
            lines.append("|" + "+".join("-" * (w + 2) for w in cols) + "|")
        else:
            r = r + [""] * (width - len(r))
            lines.append("| " + " | ".join(c.ljust(cols[i]) for i, c in enumerate(r)) + " |")
    return lines


def md_to_org(text, cap, kind="spec", src="?"):
    """Return (org_text, notes). KIND spec renames `ADDED Requirements` to `Requirements`."""
    lines = text.split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    notes = []
    out = [f"#+TITLE: {cap}"]
    title_seen = False
    para, para_start = [], 0
    i = 0

    def flush():
        nonlocal para
        if para:
            out.extend(inline("\n".join(para), notes, f"{src}:{para_start}").split("\n"))
            para = []

    while i < len(lines):
        line, ln = lines[i], i + 1
        m = _FENCE.match(line)
        if m:
            flush()
            indent, ticks, lang = m.group(1), m.group(2), m.group(3)
            body = []
            j = i + 1
            while j < len(lines) and not re.match(r"^\s*" + ticks + r"`*\s*$", lines[j]):
                body.append(lines[j])
                j += 1
            if j == len(lines):
                raise Refusal(f"{src}:{ln}: unclosed fence")
            if indent:
                notes.append(("indented fence", f"{src}:{ln}", f"{len(indent)} spaces, {lang or 'no tag'}"))
            out.append(f"{indent}#+begin_src {lang}" if lang else f"{indent}#+begin_example")
            for k, b in enumerate(body):
                e = _ORG_ESCAPE.sub(r"\1,\2", b)
                if e != b:
                    notes.append(("code line escaped with ,", f"{src}:{i + 2 + k}", b.strip()[:60]))
                out.append(e)
            out.append(f"{indent}#+end_src" if lang else f"{indent}#+end_example")
            i = j + 1
            continue
        m = _HEADING.match(line)
        if m:
            flush()
            level, title = len(m.group(1)), m.group(2)
            if level == 1:
                if title_seen or out[1:]:
                    notes.append(("level-1 heading not on line 1", f"{src}:{ln}", title))
                    out.append(f"* {title}")
                else:
                    notes.append(("file title dropped for #+TITLE", f"{src}:{ln}", title))
                title_seen = True
            else:
                if kind == "spec" and level == 2 and title == "ADDED Requirements":
                    notes.append(("ADDED Requirements -> Requirements", f"{src}:{ln}", ""))
                    title = "Requirements"
                if level == 5:
                    notes.append(("level-5 heading kept at level 4", f"{src}:{ln}", title))
                if re.search(r"\s:[\w@#%:]+:$", title):
                    notes.append(("heading text org reads as tags", f"{src}:{ln}", title))
                if i > 0 and lines[i - 1].strip() and not _HEADING.match(lines[i - 1]):
                    notes.append(("heading directly after text", f"{src}:{ln}", lines[i - 1].strip()[:60]))
                out.append("*" * (level - 1) + " " + title)
            i += 1
            continue
        if line.startswith("|"):
            flush()
            rows, j = [], i
            while j < len(lines) and lines[j].startswith("|"):
                rows.append(("sep", None) if _TABLE_SEP.match(lines[j]) else ("row", _split_row(lines[j])))
                j += 1
            notes.append(("table", f"{src}:{ln}", f"{j - i} lines"))
            out.extend(_org_table(rows, notes, f"{src}:{ln}"))
            i = j
            continue
        if line.startswith(">"):
            flush()
            body, j = [], i
            while j < len(lines) and lines[j].startswith(">"):
                body.append(re.sub(r"^> ?", "", lines[j]))
                j += 1
            notes.append(("block quote", f"{src}:{ln}", body[0][:60]))
            out.append("#+begin_quote")
            out.extend(inline("\n".join(body), notes, f"{src}:{ln}").split("\n"))
            out.append("#+end_quote")
            i = j
            continue
        if _RULE.match(line):
            flush()
            out.append("-----")
            i += 1
            continue
        if not line.strip():
            flush()
            out.append("")
            i += 1
            continue
        if line.startswith("+ ") and para:
            notes.append(("prose line starting '+ ' joined to the line before", f"{src}:{ln}", line[:60]))
            para[-1] = para[-1] + " " + line
            i += 1
            continue
        if _STEP_NOBULLET.match(line):
            notes.append(("step line with no bullet", f"{src}:{ln}", line[:60]))
        if re.match(r"^</?\w+>\s*$", line):
            notes.append(("html-like line", f"{src}:{ln}", line))
        if not para:
            para_start = ln
        para.append(line)
        i += 1
    flush()
    hazards(out, notes, src)
    return "\n".join(out) + "\n", notes


def hazards(out, notes, src):
    """Lines org would read as something the markdown did not mean."""
    in_block = False
    for k, line in enumerate(out[1:], start=2):
        s = line.strip()
        if re.match(r"#\+begin_", s):
            in_block = True
            continue
        if re.match(r"#\+end_", s):
            in_block = False
            continue
        if in_block:
            continue
        if re.match(r"^\*+ ", line):
            continue  # a heading
        if re.match(r"^\s*#(\s|$)", line):
            notes.append(("org comment line", f"{src} (org:{k})", s[:60]))
        if re.match(r"^\s*:\S+:\s*$", line) or re.match(r"^\s*: ", line):
            notes.append(("org drawer or fixed-width line", f"{src} (org:{k})", s[:60]))
        if re.match(r"^\s+\*+ ", line) or re.match(r"^\s*\+ ", line):
            notes.append(("org list item not in the markdown", f"{src} (org:{k})", s[:60]))


# ---------------------------------------------------------------- counting

_REQ = re.compile(r"^(?:#{3}|\*{2}) Requirement: (.*)$")
_SCEN = re.compile(r"^(?:#{4,5}|\*{3,4}) Scenario: (.*)$")


def titles(text):
    """(requirement titles, scenario titles) of a spec in either form, outside blocks."""
    reqs, scens, block = [], [], None
    for line in text.split("\n"):
        s = line.strip()
        if block is None and re.match(r"^`{3,}", s):
            block = "`"
            continue
        if block == "`" and re.match(r"^`{3,}\s*$", s):
            block = None
            continue
        if block is None and re.match(r"(?i)^#\+begin_", s):
            block = "org"
            continue
        if block == "org" and re.match(r"(?i)^#\+end_", s):
            block = None
            continue
        if block:
            continue
        m = _REQ.match(line)
        if m:
            reqs.append(m.group(1))
        m = _SCEN.match(line)
        if m:
            scens.append(m.group(1))
    return reqs, scens


# ---------------------------------------------------------------- tree / change

def capability_files(specs_dir):
    """[(src path, out relpath, capability id)] by the P2 rule."""
    plan = []
    for name in sorted(os.listdir(specs_dir)):
        p = os.path.join(specs_dir, name)
        if os.path.isfile(p) and name.endswith(".md"):
            stem = name[:-3]
            plan.append((p, stem + ".org", stem))
        elif os.path.isdir(p):
            mds = sorted(f for f in os.listdir(p) if f.endswith(".md"))
            if len(mds) == 1:
                plan.append((os.path.join(p, mds[0]), name + ".org", name))
            else:
                for f in mds:
                    plan.append((os.path.join(p, f), f"{name}/{f[:-3]}.org", f"{name}/{f[:-3]}"))
    return plan


def _write(path, text):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(text)


def _read(path):
    with open(path, encoding="utf-8") as fh:
        return fh.read()


def print_notes(notes):
    by = {}
    for kind, where, detail in notes:
        by.setdefault(kind, []).append((where, detail))
    for kind in sorted(by):
        items = by[kind]
        print(f"  {kind}: {len(items)}")
        for where, detail in items:
            print(f"    {where}" + (f"  {detail}" if detail else ""))


def cmd_tree(specs_dir, out_dir):
    plan = capability_files(specs_dir)
    notes, rows, problems = [], [], []
    tot = [0, 0]
    for src, rel, cap in plan:
        md = _read(src)
        org, n = md_to_org(md, cap, "spec", os.path.relpath(src, specs_dir))
        notes.extend(n)
        _write(os.path.join(out_dir, rel), org)
        a, b = titles(md), titles(org)
        if a != b:
            problems.append(f"{rel}: requirement or scenario titles differ after conversion")
        rows.append((rel, len(b[0]), len(b[1])))
        tot[0] += len(b[0])
        tot[1] += len(b[1])
    print(f"converted {len(plan)} files from {specs_dir} into {out_dir}")
    for rel, r, s in rows:
        print(f"  {rel:55} {r:4} requirements {s:5} scenarios")
    print(f"  total: {tot[0]} requirements, {tot[1]} scenarios; titles identical: {not problems}")
    for p in problems:
        print("  PROBLEM " + p)
    roots = [cap for src, rel, cap in plan if os.path.dirname(src) == specs_dir.rstrip("/")]
    for cap in roots:
        report_overlap(cap, plan)
    print("report:")
    print_notes(notes)
    return 1 if problems else 0


def report_overlap(cap, plan):
    """Scenario titles a root-level spec file shares with each other file."""
    mine = None
    others = {}
    for src, rel, c in plan:
        s = set(titles(_read(src))[1])
        if c == cap:
            mine = s
        else:
            others[rel] = s
    print(f"root-level spec file {cap!r}: {len(mine)} scenario titles; shared with:")
    for rel, s in sorted(others.items(), key=lambda kv: -len(kv[1] & mine)):
        if s & mine:
            print(f"  {rel}: {len(s & mine)}")


def cmd_change(change_dir, out_dir):
    name = os.path.basename(os.path.normpath(change_dir))
    notes = []
    for src, dst in (("proposal.md", "intent.org"), ("design.md", "design.org")):
        p = os.path.join(change_dir, src)
        if not os.path.exists(p):
            print(f"{src}: not present")
            continue
        org, n = md_to_org(_read(p), name, "change", f"{name}/{src}")
        notes.extend(n)
        _write(os.path.join(out_dir, dst), org)
        print(f"{src} -> {os.path.join(out_dir, dst)}")
    print("report:")
    print_notes(notes)
    return 0


# ---------------------------------------------------------------- apply

_ORG_H = re.compile(r"^(\*+) (.*)$")


def org_headings(lines):
    """[(index, level, title)] of org headings outside blocks."""
    hs, block = [], False
    for i, line in enumerate(lines):
        s = line.strip().lower()
        if s.startswith("#+begin_"):
            block = True
        elif s.startswith("#+end_"):
            block = False
        elif not block:
            m = _ORG_H.match(line)
            if m:
                hs.append((i, len(m.group(1)), m.group(2)))
    return hs


def requirement_blocks(lines):
    """{title: (start, end)} of `** Requirement:` subtrees; END excludes trailing blank/rule lines."""
    hs = org_headings(lines)
    blocks = {}
    for k, (i, level, title) in enumerate(hs):
        if level == 2 and title.startswith("Requirement: "):
            end = len(lines)
            for j, lv, _ in hs[k + 1:]:
                if lv <= 2:
                    end = j
                    break
            while end > i + 1 and (not lines[end - 1].strip() or lines[end - 1].strip() == "-----"):
                end -= 1
            t = title[len("Requirement: "):]
            if t in blocks:
                raise Refusal(f"requirement {t!r} appears twice")
            blocks[t] = (i, end)
    return blocks


def delta_sections(lines):
    """{'ADDED': [block lines…], 'MODIFIED': [...]} from a delta spec in org form."""
    hs = org_headings(lines)
    out = {"ADDED": [], "MODIFIED": []}
    current = None
    for k, (i, level, title) in enumerate(hs):
        if level == 1:
            m = re.match(r"^(ADDED|MODIFIED|REMOVED|RENAMED) Requirements$", title)
            if not m:
                raise Refusal(f"delta section {title!r} is not ADDED/MODIFIED Requirements")
            if m.group(1) not in out:
                raise Refusal(f"{m.group(1)} Requirements is not supported")
            current = m.group(1)
    blocks = requirement_blocks(lines)
    for t, (s, e) in blocks.items():
        sec = None
        for i, level, title in hs:
            if i > s:
                break
            if level == 1:
                sec = title.split()[0]
        out[sec].append((t, lines[s:e]))
    return out


def _merge3(ours, base, theirs):
    """Three-way merge of line lists by git merge-file; (lines, conflicts)."""
    with tempfile.TemporaryDirectory() as d:
        paths = []
        for name, ls in (("ours", ours), ("base", base), ("theirs", theirs)):
            p = os.path.join(d, name)
            _write(p, "\n".join(ls) + "\n")
            paths.append(p)
        r = subprocess.run(["git", "merge-file", "-p", "--diff3",
                            "-L", "specs (current)", "-L", "base (delta written against)",
                            "-L", "delta", *paths], capture_output=True, text=True)
        if r.returncode < 0 or r.returncode > 127:
            raise Refusal("git merge-file failed: " + r.stderr)
        return r.stdout.rstrip("\n").split("\n"), r.returncode


def apply_delta(delta_md, cap, target_text, base_text=None):
    """Return (new org text, report lines) for one capability's delta."""
    delta_org, _ = md_to_org(delta_md, cap, "delta", f"delta {cap}")
    sections = delta_sections(delta_org.rstrip("\n").split("\n"))
    report = []
    if target_text is None:
        if sections["MODIFIED"]:
            raise Refusal(f"{cap}: MODIFIED requirements but no spec file")
        lines = [f"#+TITLE: {cap}", "", "* Requirements"]
        report.append(f"{cap}: new capability file (no Purpose section)")
    else:
        lines = target_text.rstrip("\n").split("\n")
    base_blocks = None
    if base_text is not None:
        base_lines = base_text.rstrip("\n").split("\n")
        base_blocks = {t: base_lines[s:e] for t, (s, e) in requirement_blocks(base_lines).items()}
    for title, block in sections["MODIFIED"]:
        blocks = requirement_blocks(lines)
        if title not in blocks:
            raise Refusal(f"{cap}: MODIFIED {title!r} not in the spec")
        s, e = blocks[title]
        current = lines[s:e]
        if base_blocks is not None and title in base_blocks and base_blocks[title] != current:
            merged, conflicts = _merge3(current, base_blocks[title], block)
            report.append(f"{cap}: MODIFIED {title!r}: three-way merge, "
                          + (f"{conflicts} conflict(s)" if conflicts else "clean"))
            block = merged
        else:
            report.append(f"{cap}: MODIFIED {title!r}: replaced")
        lines[s:e] = block
    for title, block in sections["ADDED"]:
        if title in requirement_blocks(lines):
            raise Refusal(f"{cap}: ADDED {title!r} already in the spec")
        hs = org_headings(lines)
        req = [h for h in hs if h[1] == 1 and h[2] == "Requirements"]
        if not req:
            raise Refusal(f"{cap}: no * Requirements section")
        end = len(lines)
        for i, level, _ in hs:
            if i > req[0][0] and level == 1:
                end = i
                break
        while end > 0 and not lines[end - 1].strip():
            end -= 1
        insert = ([""] if lines[end - 1].strip() else []) + block + [""]
        lines[end:end] = insert
        report.append(f"{cap}: ADDED {title!r}")
    return "\n".join(lines).rstrip("\n") + "\n", report


def cmd_apply(change_dir, specs_org, base_org=None):
    specs_md = os.path.join(change_dir, "specs")
    caps = sorted(os.listdir(specs_md))
    for cap in caps:
        delta = os.path.join(specs_md, cap, "spec.md")
        if not os.path.exists(delta):
            raise Refusal(f"{cap}: no spec.md in the delta")
        if os.path.isdir(os.path.join(specs_org, cap)):
            raise Refusal(f"{cap}: a capability directory; a delta names one file")
        target = os.path.join(specs_org, cap + ".org")
        before = _read(target) if os.path.exists(target) else None
        base = None
        if base_org and os.path.exists(os.path.join(base_org, cap + ".org")):
            base = _read(os.path.join(base_org, cap + ".org"))
        new, report = apply_delta(_read(delta), cap, before, base)
        _write(target, new)
        b = titles(before or "")
        a = titles(new)
        for line in report:
            print(line)
        print(f"{cap}: requirements {len(b[0])} -> {len(a[0])}, scenarios {len(b[1])} -> {len(a[1])}")
    return 0


def cmd_count(paths):
    for p in paths:
        r, s = titles(_read(p))
        print(f"{p}\t{len(r)}\t{len(s)}")
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="verb", required=True)
    t = sub.add_parser("tree")
    t.add_argument("specs_dir")
    t.add_argument("out_dir")
    c = sub.add_parser("change")
    c.add_argument("change_dir")
    c.add_argument("out_dir")
    a = sub.add_parser("apply")
    a.add_argument("change_dir")
    a.add_argument("specs_org")
    a.add_argument("--base")
    f = sub.add_parser("file")
    f.add_argument("src")
    f.add_argument("out")
    f.add_argument("--cap")
    f.add_argument("--kind", default="spec", choices=["spec", "change"])
    n = sub.add_parser("count")
    n.add_argument("paths", nargs="+")
    args = ap.parse_args(argv)
    try:
        if args.verb == "tree":
            return cmd_tree(args.specs_dir, args.out_dir)
        if args.verb == "change":
            return cmd_change(args.change_dir, args.out_dir)
        if args.verb == "apply":
            return cmd_apply(args.change_dir, args.specs_org, args.base)
        if args.verb == "file":
            cap = args.cap or os.path.splitext(os.path.basename(args.src))[0]
            org, notes = md_to_org(_read(args.src), cap, args.kind, args.src)
            _write(args.out, org)
            print_notes(notes)
            return 0
        if args.verb == "count":
            return cmd_count(args.paths)
    except Refusal as e:
        print(f"refused: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
