"""pytest suite for spec_convert.py (stdlib + pytest). Run: python3 -m pytest test_spec_convert.py  (from this directory)."""

import importlib.util
import pathlib

import pytest

HERE = pathlib.Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location("spec_convert", HERE / "spec_convert.py")
sc = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(sc)


def conv(md, kind="spec", cap="cap"):
    org, notes = sc.md_to_org(md, cap, kind, "f.md")
    return org, {k for k, _, _ in notes}, notes


def body(org):
    return org.split("\n", 1)[1]


# ---------------------------------------------------------------- headings

def test_title_and_heading_levels():
    org, kinds, _ = conv("# Workspaces\n\n## Purpose\n\nText.\n\n## Requirements\n\n"
                         "### Requirement: R\nThe system SHALL.\n\n#### Scenario: S\n- **WHEN** x\n- **THEN** y\n")
    assert org.startswith("#+TITLE: cap\n")
    assert "* Purpose\n" in org and "* Requirements\n" in org
    assert "** Requirement: R\n" in org and "*** Scenario: S\n" in org
    assert "file title dropped for #+TITLE" in kinds


def test_heading_text_byte_for_byte():
    md = "## Requirements\n\n### Requirement: R\n\n#### Scenario: No `:security` key in **the** plist\n- **WHEN** a\n- **THEN** b\n"
    org, _, _ = conv(md)
    assert "*** Scenario: No `:security` key in **the** plist\n" in org


def test_added_requirements_renamed_in_a_spec_kept_in_a_change():
    md = "## ADDED Requirements\n\n### Requirement: R\n"
    assert "* Requirements\n" in conv(md)[0]
    assert "* ADDED Requirements\n" in conv(md, kind="change")[0]


def test_level_five_scenario_kept_and_noted():
    org, kinds, _ = conv("### Requirement: R\n\n#### session.org format\n\n##### Scenario: S\n- **WHEN** a\n")
    assert "**** Scenario: S\n" in org
    assert "level-5 heading kept at level 4" in kinds
    assert sc.titles(org) == (["R"], ["S"])


# ---------------------------------------------------------------- steps and emphasis

def test_step_bullets_and_continuation_lines():
    md = "#### Scenario: S\n- **WHEN** the user\n  does a thing\n- **THEN** it works\n- **AND** nothing else\n"
    assert body(conv(md)[0]) == ("*** Scenario: S\n- *WHEN* the user\n  does a thing\n"
                                 "- *THEN* it works\n- *AND* nothing else\n")


def test_unbulleted_step_line_kept_and_noted():
    org, kinds, _ = conv("**WHEN** resolving `x`\n**THEN** the result is y\n")
    assert body(org) == "*WHEN* resolving =x=\n*THEN* the result is y\n"
    assert "step line with no bullet" in kinds


def test_italic_and_wrapped_italic():
    org, _, _ = conv("A *registry name* and (see *Buffer reincarnation across\n  restart*).\n")
    assert body(org) == "A /registry name/ and (see /Buffer reincarnation across\n  restart/).\n"


def test_emphasis_over_more_than_two_lines_noted():
    _, kinds, _ = conv("*one\ntwo\nthree*\n")
    assert "emphasis over more than two lines (org shows it plain)" in kinds


def test_glob_stars_stay_literal():
    md = '- **WHEN** matching "/workspace/src/foo.el" against "/workspace/**/*.el"\n- **THEN** path matches (** matches zero segments)\n'
    org, kinds, _ = conv(md)
    assert body(org) == ('- *WHEN* matching "/workspace/src/foo.el" against "/workspace/**/*.el"\n'
                         "- *THEN* path matches (** matches zero segments)\n")
    assert "* kept literal in prose" in kinds


# ---------------------------------------------------------------- inline code

def test_inline_code_equals_and_tilde():
    org, _, _ = conv("Use `~/org/` and `from-id = <id>` and `**not bold**`.\n")
    assert body(org) == "Use =~/org/= and ~from-id = <id>~ and =**not bold**=.\n"


def test_inline_code_across_a_line_break():
    org, kinds, _ = conv("Call `(file-name-nondirectory\n(directory-file-name :home))` now.\n")
    assert body(org) == "Call =(file-name-nondirectory\n(directory-file-name :home))= now.\n"
    assert "code across a line break" in kinds


def test_inline_code_edge_space_noted():
    org, kinds, _ = conv("Type `M-x ` then.\n")
    assert body(org) == "Type =M-x = then.\n"
    assert "code with edge space" in kinds


def test_double_backtick_span():
    org, _, _ = conv("See `` data-spec.el::\"origin `user'\" `` here.\n")
    assert body(org) == "See =data-spec.el::\"origin `user'\"= here.\n"


# ---------------------------------------------------------------- blocks

def test_fence_with_language_and_escapes():
    md = "```org\n* Initial exploration\n#+begin_user\nhi\n#+end_user\n```\n"
    org, _, notes = conv(md)
    assert body(org) == "#+begin_src org\n,* Initial exploration\n,#+begin_user\nhi\n,#+end_user\n#+end_src\n"
    assert sum(1 for k, _, _ in notes if k == "code line escaped with ,") == 3


def test_fence_without_tag_is_an_example():
    org, _, _ = conv("```\nplain **text** `x`\n```\n")
    assert body(org) == "#+begin_example\nplain **text** `x`\n#+end_example\n"


def test_indented_fence_in_a_list_item():
    md = "- **THEN** it SHALL contain:\n  ```yaml\n  parent: main\n  ```\n"
    org, kinds, _ = conv(md)
    assert body(org) == "- *THEN* it SHALL contain:\n  #+begin_src yaml\n  parent: main\n  #+end_src\n"
    assert "indented fence" in kinds


def test_heading_inside_a_fence_is_not_counted():
    md = "```markdown\n#### Scenario: not one\n```\n#### Scenario: real\n"
    org, _, _ = conv(md)
    assert sc.titles(md)[1] == ["real"] and sc.titles(org)[1] == ["real"]


def test_unclosed_fence_refused():
    with pytest.raises(sc.Refusal):
        conv("```elisp\n(foo)\n")


def test_table():
    md = "| Identifier | Resolves to |\n|---|---|\n| `\"transient\"` | `config/transient.el` |\n"
    org, _, _ = conv(md)
    assert body(org) == ("| Identifier    | Resolves to           |\n"
                         "|---------------+-----------------------|\n"
                         "| =\"transient\"= | =config/transient.el= |\n")


def test_escaped_bar_in_a_table_cell():
    org, kinds, _ = conv("| grammar |\n|---|\n| `live {tools \\| call}` |\n")
    assert "=live {tools \\vert{} call}=" in org
    assert "| in a table cell -> \\vert{}" in kinds


def test_block_quote():
    org, _, _ = conv("> **Note:** The replace-on-save\n> semantics.\n")
    assert body(org) == "#+begin_quote\n*Note:* The replace-on-save\nsemantics.\n#+end_quote\n"


def test_rule():
    assert body(conv("- **THEN** done\n\n---\n\n### Requirement: R\n")[0]) == "- *THEN* done\n\n-----\n\n** Requirement: R\n"


def test_relative_md_link():
    org, _, _ = conv("**Supersedes:** [command-handlers.md](command-handlers.md), [x](https://e.org/a.md)\n")
    assert body(org) == ("*Supersedes:* [[file:command-handlers.org][command-handlers.org]], "
                         "[[file:https://e.org/a.md][x]]\n")


def test_prose_line_starting_plus_joined():
    org, kinds, _ = conv("supersedes `activities.el`\n+ `ext` (shell\n+ tabs)\n")
    assert body(org) == "supersedes =activities.el= + =ext= (shell + tabs)\n"
    assert "prose line starting '+ ' joined to the line before" in kinds


def test_html_like_line_noted():
    assert "html-like line" in conv("</content>\n")[1]


# ---------------------------------------------------------------- tree

def _tree(tmp_path, files):
    for rel, text in files.items():
        p = tmp_path / "specs" / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text)
    return tmp_path / "specs"


def test_capability_files_by_p2(tmp_path):
    specs = _tree(tmp_path, {"workspaces/spec.md": "x", "core/module-system.md": "x",
                             "bash-parser/core.md": "x", "bash-parser/security.md": "x", "spec.md": "x"})
    plan = {(rel, cap) for _, rel, cap in sc.capability_files(str(specs))}
    assert plan == {("workspaces.org", "workspaces"), ("core.org", "core"),
                    ("bash-parser/core.org", "bash-parser/core"),
                    ("bash-parser/security.org", "bash-parser/security"), ("spec.org", "spec")}


def test_tree_counts_and_overlap(tmp_path, capsys):
    scen = "### Requirement: R\n\n#### Scenario: Shared\n- **WHEN** a\n- **THEN** b\n"
    specs = _tree(tmp_path, {"a/spec.md": "## Requirements\n\n" + scen, "spec.md": "## ADDED Requirements\n\n" + scen})
    assert sc.cmd_tree(str(specs), str(tmp_path / "out")) == 0
    out = capsys.readouterr().out
    assert "total: 2 requirements, 2 scenarios; titles identical: True" in out
    assert "a.org: 1" in out
    assert (tmp_path / "out" / "a.org").read_text().startswith("#+TITLE: a\n")


# ---------------------------------------------------------------- apply

SPEC = ("#+TITLE: ws\n\n* Purpose\n\nP.\n\n* Requirements\n\n"
        "** Requirement: Keep\nk.\n\n*** Scenario: K\n- *WHEN* a\n- *THEN* b\n\n-----\n\n"
        "** Requirement: Change\nold line one\nold line two\n\n*** Scenario: C\n- *WHEN* a\n- *THEN* b\n")


def test_apply_added_and_modified():
    delta = ("## ADDED Requirements\n\n### Requirement: New\nn.\n\n#### Scenario: N\n- **WHEN** a\n- **THEN** b\n\n"
             "## MODIFIED Requirements\n\n### Requirement: Change\nnew line one\nold line two\n\n"
             "#### Scenario: C\n- **WHEN** a\n- **THEN** b\n")
    new, report = sc.apply_delta(delta, "ws", SPEC)
    assert sc.titles(new) == (["Keep", "Change", "New"], ["K", "C", "N"])
    assert "new line one\nold line two" in new and "old line one" not in new
    assert "-----\n\n** Requirement: Change" in new
    assert report == ["ws: MODIFIED 'Change': replaced", "ws: ADDED 'New'"]


def test_apply_three_way_merge_clean():
    current = SPEC.replace("- *THEN* b\n", "- *THEN* c\n").replace("- *THEN* c\n", "- *THEN* b\n", 1)
    delta = ("## MODIFIED Requirements\n\n### Requirement: Change\nnew line one\nold line two\n\n"
             "#### Scenario: C\n- **WHEN** a\n- **THEN** b\n")
    new, report = sc.apply_delta(delta, "ws", current, SPEC)
    assert new.endswith("** Requirement: Change\nnew line one\nold line two\n\n*** Scenario: C\n- *WHEN* a\n- *THEN* c\n")
    assert report == ["ws: MODIFIED 'Change': three-way merge, clean"]


def test_apply_three_way_merge_conflict_marked():
    current = SPEC.replace("old line one", "current line one")
    delta = ("## MODIFIED Requirements\n\n### Requirement: Change\ndelta line one\nold line two\n\n"
             "#### Scenario: C\n- **WHEN** a\n- **THEN** b\n")
    new, report = sc.apply_delta(delta, "ws", current, SPEC)
    assert "<<<<<<< specs (current)\ncurrent line one" in new and ">>>>>>> delta" in new
    assert report == ["ws: MODIFIED 'Change': three-way merge, 1 conflict(s)"]


def test_apply_new_capability():
    new, report = sc.apply_delta("## ADDED Requirements\n\n### Requirement: A\n\n#### Scenario: S\n- **WHEN** a\n",
                                 "fresh", None)
    assert new.startswith("#+TITLE: fresh\n\n* Requirements\n\n** Requirement: A\n")
    assert report[0] == "fresh: new capability file (no Purpose section)"


@pytest.mark.parametrize("delta", [
    "## MODIFIED Requirements\n\n### Requirement: Missing\n",
    "## ADDED Requirements\n\n### Requirement: Keep\n",
    "## REMOVED Requirements\n\n### Requirement: Keep\n",
])
def test_apply_refusals(delta):
    with pytest.raises(sc.Refusal):
        sc.apply_delta(delta, "ws", SPEC)


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q"]))


def test_count_walks_a_directory_and_totals(tmp_path, capsys):
    d = tmp_path / "specs" / "cap"
    d.mkdir(parents=True)
    (d / "spec.md").write_text("## Requirements\n\n### Requirement: R\n\n#### Scenario: S\n- **WHEN** x\n- **THEN** y\n")
    assert sc.main(["count", str(tmp_path / "specs")]) == 0
    out = capsys.readouterr().out
    assert out.rstrip().endswith("total: 1 requirements, 1 scenarios in 1 files")
