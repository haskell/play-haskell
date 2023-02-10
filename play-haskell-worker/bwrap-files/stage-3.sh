#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"

# Read ghc output FD from command-line arguments
if [[ $# -ne 1 ]]; then
	echo >&2 "Usage: $0 <ghc_out_fd>"
	echo >&2 "This script should be run within bwrap by ./stage-2.sh"
	exit 1
fi
ghc_out_fd="$1"

IFS="" read -r command
IFS="" read -r opt
IFS="" read -r version

# Check that there are only version-like characters
if [[ "${version//[0-9.]/}" != "" ]]; then
	echo >&2 "Invalid version"
	exit 1
fi

if ! ghcup --no-verbose --offline whereis ghc "${version}" 2>/tmp/null 1>/tmp/null ; then
	echo >&2 "Version ${version} not available"
	exit 1
fi

function ghcup_run() {
	ghcup --no-verbose --offline run --ghc "$version" -- "$@" >/tmp/null 2>&"$ghc_out_fd"
}

# arguments: ghc options (excluding -O*)
function write_cabal_project() {
	cat >cabal.project <<EOF
packages: .
package sandbox
    ghc-options: ${opt} $@
EOF
}

cp -rv dist-newstyle-bind dist-newstyle
cp -rv dot-cabal-bind ~/.cabal

case "$command" in
	run)
		cat >Main.hs
		cpp -traditional -DAS_EXECUTABLE sandbox.cabal.in | grep -v '^#' >sandbox.cabal
		write_cabal_project
		ghcup_run cabal build --offline
		"$(ghcup_run cabal list-bin thing)" +RTS -M500m -RTS
		;;
	core)
		cat >Main.hs
		write_cabal_project -ddump-simpl -ddump-to-file
		cpp -traditional -DAS_EXECUTABLE sandbox.cabal.in | grep -v '^#' >sandbox.cabal
		ghcup_run cabal build --offline
		cat Main.dump-simpl
		;;
	asm)
		cat >Main.hs
		write_cabal_project -ddump-asm -ddump-to-file
		cpp -traditional -DAS_EXECUTABLE sandbox.cabal.in | grep -v '^#' >sandbox.cabal
		ghcup_run cabal build --offline
		cat Main.dump-asm
		;;

	*)
		echo >&2 "Unknown command"
		exit 1
		;;
esac

# vim: set sw=4 ts=4 noet:
