#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"

IFS="" read -r command

IFS="" read -r version

# Check that there are only version-like characters
if [[ "${version//[0-9.]/}" != "" ]]; then
	echo >&2 "Invalid version"
	exit 1
fi

if [[ ! -f "$HOME/.ghcup/bin/ghc-${version}" ]]; then
	echo >&2 "Version not available"
	exit 1
fi

case "$command" in
	run)
		cat >input.hs
		"$HOME/.ghcup/bin/runhaskell-${version}" input.hs
		;;

	*)
		echo >&2 "Unknown command"
		exit 1
		;;
esac
