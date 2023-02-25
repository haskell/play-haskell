#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"

# Read ghc output FD from command-line arguments
if [[ $# -ne 1 ]]; then
  echo >&2 "Usage: $0 <ghc_out_fd>"
  echo >&2 "stage-2 needs bwrap >=v0.7.0."
  echo >&2 "Do not kill this script using SIGKILL; it needs to be able to clean up the systemd unit on exit."
  exit 1
fi
ghc_out_fd="$1"

# Close all open file descriptors other than 0,1,2 and the ghc output FD
close_cmdline="exec"
for fd in $(ls /proc/$$/fd); do
  if [[ "$fd" -gt 2 && "$fd" -ne "$ghc_out_fd" ]]; then
    close_cmdline="$close_cmdline $fd>&-"
  fi
done
eval "$close_cmdline"

tmpdir=$(mktemp -d)

trap "rm -r '$tmpdir'" EXIT

mkfifo "${tmpdir}/ghc-out"
# This cat will exit automatically once the stage-2 exits (closes the fifo).
( cat <"${tmpdir}/ghc-out" >&"$ghc_out_fd" ) &
ghc_out_catpid=$!

# Tricky part: we generate a hopefully unique unit name, tell systemd-run to
# use that unit name, and kill that unit on exit. This requires that stage-1 is
# not killed using SIGKILL.
#
# Another property that makes this work: this stage-1 script is not yet subject
# to the memory limit in the systemd unit, hence we can more _more_ sure (not
# fully, even) that we won't get killed on OOM.
unit_name="play-haskell-sandbox-u$(date +%s-%N)-$SRANDOM"

# If we make it to the =0 assignment later, the systemd unit apparently exited
# already, so no need to kill it manually.
we_were_killed=1

trap "[[ \$we_were_killed = 1 ]] && ./systemd-kill-shim '$unit_name'" EXIT

# Don't exit the script on error here!
set +e
./systemd-run-shim "$unit_name" ./stage-2.sh "${tmpdir}/ghc-out"
exitstatus=$?
we_were_killed=0

exit "$exitstatus"
