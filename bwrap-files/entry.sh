#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"

IFS="" read -r command
IFS="" read -r opt
IFS="" read -r version

# Check that there are only version-like characters
if [[ "${version//[0-9.]/}" != "" ]]; then
	echo >&2 "Invalid version"
	exit 1
fi

if ! ghcup --offline whereis ghc ${version} 2>/tmp/null 1>/tmp/null ; then
	echo >&2 "Version ${version} not available"
	exit 1
fi

case "$command" in
	run)
		cat >input.hs
		ghcup --offline run --ghc ${version} -- ghc -o Main "${opt}" input.hs 2>/tmp/null 1>/tmp/null
		./Main
		;;

	*)
		echo >&2 "Unknown command"
		exit 1
		;;
esac
