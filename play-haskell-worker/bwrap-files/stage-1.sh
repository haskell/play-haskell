#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"

# Read ghc output FD from command-line arguments
if [[ $# -ne 1 ]]; then
  echo >&2 "Usage: $0 <ghc_out_fd>"
  echo >&2 "stage-2 needs to run bwrap with tomsmeding/tmpfs-size patch."
  echo >&2 "Build https://github.com/tomsmeding/bubblewrap/tree/tmpfs-size and"
  echo >&2 "put the executable in this directory as 'bwrap'. If such an executable"
  echo >&2 "exists, stage-2 will use it instead of the 'bwrap' from PATH."
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

args=(
  --user
  --description='play-haskell-worker cpuquota'
  --pipe
  --wait
  --collect
  --same-dir
  --service-type=exec
  --setenv=PATH="$PATH"
  --quiet
  --property=CPUQuota=100%
  # Limit memory to 600 MiB. Note that the compiled program gets a 500 MiB memory
  # limit via the GHC RTS, so this limit is 1. to constrain GHC itself (including
  # any TH code), and 2. as a second-layer defense.
  --property=MemoryMax=600M
  --property=TasksMax=50
  --property=LimitCORE=0
)

systemd-run "${args[@]}" -- ./stage-2.sh "${tmpdir}/ghc-out"
