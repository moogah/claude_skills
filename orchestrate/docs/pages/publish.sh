#!/usr/bin/env bash
# Publish the committed pages to GitHub Pages: https://moogah.github.io/claude_skills/
#
#   orchestrate/docs/pages/publish.sh            publishes this folder as committed at HEAD
#
# gh-pages holds this folder's tree at HEAD plus an empty .nojekyll, one commit on top of the last; it pushes
# gh-pages only, never the branch you are on. Build and commit first: what is not committed is not published.
# The site is public; the pages quote the eval trials of the private evals repo (decided 2026-11-14).
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
prefix=orchestrate/docs/pages
[ -z "$(git status --porcelain -- "$prefix")" ] || { echo "uncommitted changes in $prefix; commit them first" >&2; exit 1; }
idx=$(mktemp)
trap 'rm -f "$idx"' EXIT
rm -f "$idx"
GIT_INDEX_FILE="$idx" git read-tree "HEAD:$prefix"
GIT_INDEX_FILE="$idx" git update-index --add --cacheinfo "100644,$(git hash-object -w /dev/null),.nojekyll"
tree=$(GIT_INDEX_FILE="$idx" git write-tree)
git fetch -q origin gh-pages
parent=$(git rev-parse origin/gh-pages)
if [ "$(git rev-parse "$parent^{tree}")" = "$tree" ]; then echo "gh-pages already has these pages"; exit 0; fi
commit=$(git commit-tree "$tree" -p "$parent" -m "spec pages from $(git rev-parse --abbrev-ref HEAD) $(git rev-parse --short HEAD):$prefix")
git push -q origin "${commit}:refs/heads/gh-pages"
echo "published ${commit:0:7} -> https://moogah.github.io/claude_skills/ (live in a minute or two)"
