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
if [[ "${version//[0-9.alpha-]/}" != "" ]]; then
	echo >&2 "Invalid version"
	exit 1
fi

if [[ ! -f /builders/build-"${version}".sh ]]; then
	echo >&2 "Version ${version} not available"
	exit 1
fi

core_flags=()
asm_flags=()

# If the GHC version is >=8.10, we have -dno-typeable-binds
if [[ "${version:0:5}" = "8.10." || "${version:0:2}" = "9." ]]; then
	core_flags[${#core_flags[@]}]=-dno-typeable-binds
	asm_flags[${#asm_flags[@]}]=-dno-typeable-binds
fi

case "$command" in
	run)
		cat >Main.hs
		/builders/build-"${version}".sh "${opt}" Main.hs >/tmp/null 2>&"$ghc_out_fd"
		if [[ -f Main ]]; then
			./Main +RTS -M500m -RTS
		else
			echo >&2 "Compilation succeeded, but no executable was produced. Perhaps your module is not called 'Main'?"
		fi
		;;
	core)
		cat >input.hs
		/builders/build-"${version}".sh -ddump-simpl -ddump-to-file "${core_flags[@]}" "${opt}" input.hs >/tmp/null 2>&"$ghc_out_fd"
		cat input.dump-simpl
		;;
	asm)
		cat >input.hs
		/builders/build-"${version}".sh -ddump-asm -ddump-to-file "${asm_flags[@]}" "${opt}" input.hs >/tmp/null 2>&"$ghc_out_fd"
		cat input.dump-asm
		;;

	*)
		echo >&2 "Unknown command"
		exit 1
		;;
esac
