#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

vers=( $(ghcup --no-verbose --offline list -t ghc -c installed -r | cut -d' ' -f2) )

first=0
for ver in "${vers[@]}"; do
  [[ first -eq 0 ]] && first=1 || echo
  printf "\x1B[1m[mkallbuildscripts] === Creating for GHC $ver ===\x1B[0m\n"
  ./mkbuildscript.sh "$ver"
done
