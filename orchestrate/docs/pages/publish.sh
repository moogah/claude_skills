#!/usr/bin/env bash
# Build the spec pages and publish them to GitHub Pages: https://moogah.github.io/claude_skills/
#
#   orchestrate/docs/pages/publish.sh
#
# The generated pages (*.html, *.data.js) are not kept in git: they are built from the evals repo's runs, which
# this repository does not hold (decided 2026-11-14). This script runs build.py, then replaces gh-pages with one
# commit holding the generated pages and an empty .nojekyll; its message names the commits the build read. Old
# versions are not kept, so the repository does not grow with each publish. It pushes gh-pages only, never the
# branch you are on. build.py and the templates must be committed, so the commit named is what built the pages.
# The site is public; the pages quote the eval trials of the private evals repo (decided 2026-11-14).
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
dir=orchestrate/docs/pages
[ -z "$(git status --porcelain -- "$dir" orchestrate/docs/specs)" ] \
  || { echo "uncommitted changes in $dir or the specs; commit them first" >&2; exit 1; }
python3 "$dir/build.py"
evals=$(sed -n 's/^ *"evals_head": "\(.*\)",$/\1/p' "$dir"/execute.data.js | head -1)
idx=$(mktemp)
trap 'rm -f "$idx"' EXIT
rm -f "$idx"
add() { GIT_INDEX_FILE="$idx" git update-index --add --cacheinfo "100644,$(git hash-object -w "$1"),$2"; }
add /dev/null .nojekyll
for f in "$dir"/*.data.js "$dir"/*.html; do
  case "$f" in *.template.html) continue ;; esac
  add "$f" "$(basename "$f")"
done
tree=$(GIT_INDEX_FILE="$idx" git write-tree)
if git fetch -q origin gh-pages 2>/dev/null && [ "$(git rev-parse origin/gh-pages^{tree})" = "$tree" ]; then
  echo "gh-pages already has these pages"; exit 0
fi
commit=$(git commit-tree "$tree" -m "spec pages built from $(git rev-parse --abbrev-ref HEAD) $(git rev-parse --short HEAD), evals ${evals:-unknown}")
git push -q --force origin "${commit}:refs/heads/gh-pages"
echo "published ${commit:0:7} -> https://moogah.github.io/claude_skills/ (live in a minute or two)"
