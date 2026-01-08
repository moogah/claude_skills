#!/usr/bin/env python3
"""
Skill Initializer - Creates a new skill from template

Usage:
    init_skill.py <skill-name> --path <path>

Examples:
    init_skill.py my-new-skill --path skills/public
    init_skill.py my-api-helper --path skills/private
    init_skill.py custom-skill --path /custom/location
"""

import sys
from pathlib import Path


SKILL_TEMPLATE = """---
name: {skill_name}
description: "[TODO: Complete and informative explanation of what the skill does and when to use it. Include WHEN to use this skill - specific scenarios, file types, or tasks that trigger it.]"
---

# {skill_title}

## Overview

[TODO: 1-2 sentences explaining what this skill enables]

## [TODO: First Main Section]

[TODO: Add primary workflow, guidance, or examples here.

Tips for a v1 skill:
- Start minimal - 50-80 lines total is typical
- Include only core workflow and one concrete example
- Remember: Claude is smart, focus on what's non-obvious
- Let the skill grow through iteration based on real usage]

## [TODO: Second Main Section (if needed)]

[TODO: Add secondary content here if needed, or delete this section]

<!--
RESOURCE DIRECTORIES (Optional):

Create these directories only when you have concrete files to add:

- scripts/ : Executable code (Python/Bash/etc.) that can be run directly
  Example: scripts/rotate_pdf.py for deterministic PDF operations

- references/ : Documentation to be loaded into context when Claude needs it
  Example: references/api_docs.md for detailed API specifications

- assets/ : Files used in output (templates, images, fonts, boilerplate)
  Example: assets/template.pptx for PowerPoint starting templates

Most v1 skills don't need resource directories. Add them only when required.
Delete this comment block before finalizing the skill.
-->
"""



def title_case_skill_name(skill_name):
    """Convert hyphenated skill name to Title Case for display."""
    return ' '.join(word.capitalize() for word in skill_name.split('-'))


def init_skill(skill_name, path):
    """
    Initialize a new skill directory with template SKILL.md.

    Args:
        skill_name: Name of the skill
        path: Path where the skill directory should be created

    Returns:
        Path to created skill directory, or None if error
    """
    # Determine skill directory path
    skill_dir = Path(path).resolve() / skill_name

    # Check if directory already exists
    if skill_dir.exists():
        print(f"❌ Error: Skill directory already exists: {skill_dir}")
        return None

    # Create skill directory
    try:
        skill_dir.mkdir(parents=True, exist_ok=False)
        print(f"✅ Created skill directory: {skill_dir}")
    except Exception as e:
        print(f"❌ Error creating directory: {e}")
        return None

    # Create SKILL.md from template
    skill_title = title_case_skill_name(skill_name)
    skill_content = SKILL_TEMPLATE.format(
        skill_name=skill_name,
        skill_title=skill_title
    )

    skill_md_path = skill_dir / 'SKILL.md'
    try:
        skill_md_path.write_text(skill_content)
        print("✅ Created SKILL.md")
    except Exception as e:
        print(f"❌ Error creating SKILL.md: {e}")
        return None

    # Print next steps
    print(f"\n✅ Skill '{skill_name}' initialized successfully at {skill_dir}")
    print("\nNext steps:")
    print("1. Edit SKILL.md to complete the TODOs")
    print("2. Start minimal - most v1 skills are 50-80 lines with no resource directories")
    print("3. Add scripts/, references/, or assets/ directories only if needed")
    print("4. Validate when ready:")
    print("   source ~/.claude/skills/skill-creator/.venv/bin/activate")
    print(f"   ~/.claude/skills/skill-creator/scripts/quick_validate.py {skill_dir}")

    return skill_dir


def main():
    if len(sys.argv) < 4 or sys.argv[2] != '--path':
        print("Usage: init_skill.py <skill-name> --path <path>")
        print("\nSkill name requirements:")
        print("  - Hyphen-case identifier (e.g., 'data-analyzer')")
        print("  - Lowercase letters, digits, and hyphens only")
        print("  - Max 40 characters")
        print("  - Must match directory name exactly")
        print("\nExamples:")
        print("  init_skill.py my-new-skill --path skills/public")
        print("  init_skill.py my-api-helper --path skills/private")
        print("  init_skill.py custom-skill --path /custom/location")
        sys.exit(1)

    skill_name = sys.argv[1]
    path = sys.argv[3]

    print(f"🚀 Initializing skill: {skill_name}")
    print(f"   Location: {path}")
    print()

    result = init_skill(skill_name, path)

    if result:
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
