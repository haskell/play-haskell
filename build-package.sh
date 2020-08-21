#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
outfile="$PWD/dist-newstyle/tmp/pastebin-haskell.tar.gz"

tempdir="$(mktemp -d)"
trap "rm -rf '$tempdir'" EXIT

cabal install --installdir="$tempdir"

cp "$(realpath "$tempdir/pastebin-haskell")" "$tempdir/pastebin-haskell-exec"
mv "$tempdir/pastebin-haskell-exec" "$tempdir/pastebin-haskell"

cp -t "$tempdir" $(grep '^extra-source-files:' pastebin-haskell.cabal | cut -d: -f2)

(
	cd "$tempdir"
	tar czf "$outfile" *
)

echo "Written to:"
echo "$outfile"
