#!/usr/bin/env bash
# push_to_github.sh
# ------------------------------------------------------------------
# Alternative to push_to_github.R using plain git + GitHub CLI (gh).
# Requires: git, R, and `gh` (https://cli.github.com) logged in via
#   gh auth login
#
# Usage (from the genmediation folder):
#   bash push_to_github.sh <github-username> [public|private]
# ------------------------------------------------------------------
set -euo pipefail

USER="${1:?Usage: bash push_to_github.sh <github-username> [public|private]}"
VIS="${2:-public}"
PKG="genmediation"

[ -f DESCRIPTION ] || { echo "Run from the package folder (DESCRIPTION not found)"; exit 1; }

# 1. Build docs (man/, NAMESPACE)
Rscript -e 'if (!requireNamespace("roxygen2", quietly=TRUE)) install.packages("roxygen2"); roxygen2::roxygenise()'

# 2. Git init + commit
if [ ! -d .git ]; then
  git init -b main
fi
git add -A
git commit -m "Initial commit of $PKG" || echo "Nothing new to commit"

# 3. Create repo on GitHub and push
gh repo create "$USER/$PKG" --"$VIS" --source=. --remote=origin --push \
  --description "Genetic Mediation Analysis via Longitudinal SEM"

echo
echo "Done: https://github.com/$USER/$PKG"
echo "Install with:  pak::pak(\"$USER/$PKG\")"
