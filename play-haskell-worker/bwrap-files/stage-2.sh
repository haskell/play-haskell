#!/bin/bash
set -euo pipefail

# Read ghc output FD from command-line arguments
if [[ $# -ne 1 ]]; then
  echo >&2 "Usage: $0 <ghc_out_fifo>"
  echo >&2 "This script should be run within ./systemd-run-shim by ./stage-1.sh"
  exit 1
fi
ghc_out_fifo="$1"

ghcup_base=$(ghcup whereis basedir)

chroot="ubuntu-base"

# We will connect this fd to the ghc_out_fifo for bwrap
ghc_out_fd=100

args=(
  # Using my bwrap fork: https://github.com/tomsmeding/bubblewrap/tree/tmpfs-size
  --size $((100 * 1024 * 1024)) --tmpfs /tmp
  --ro-bind "${chroot}/bin" /bin
  --ro-bind "${chroot}/usr/bin" /usr/bin
  --ro-bind "${chroot}/usr/lib" /usr/lib
  --ro-bind "${chroot}/usr/include" /usr/include
  --ro-bind "${chroot}/lib" /lib
  --ro-bind "${chroot}/lib64" /lib64
  --dir "${ghcup_base}"
  --ro-bind "${ghcup_base}/bin"   "${ghcup_base}/bin"
  --ro-bind "${ghcup_base}/ghc"   "${ghcup_base}/ghc"
  --ro-bind "${ghcup_base}/cache" "${ghcup_base}/cache"
  --setenv PATH "/bin:/usr/bin:${ghcup_base}/bin"
  --setenv GHCUP_INSTALL_BASE_PREFIX "$(dirname "${ghcup_base}")"
  --setenv GHCUP_SKIP_UPDATE_CHECK ""
  --proc /proc
  --chdir "/tmp"
  --new-session
  --unshare-all
  --die-with-parent
  --file 4 "/tmp/stage-3.sh"
  /bin/bash "/tmp/stage-3.sh" "$ghc_out_fd"
)

# Turn off core files
ulimit -c 0

# Limit on the number of processes
ulimit -u 10000

# Limit memory to 600 MiB. Note that the compiled program gets a 500 MiB memory
# limit via the GHC RTS, so this limit is 1. to constrain GHC itself (including
# any TH code), and 2. as a second-layer defense.
ulimit -d $(( 600 * 1024 ))

[[ -x ./bwrap ]] && BWRAP=./bwrap || BWRAP=bwrap

# Need to do this under eval because otherwise the fd-redirect syntax is not
# recognised.
eval exec "$ghc_out_fd"'>"$ghc_out_fifo"'

exec $BWRAP "${args[@]}" 4<stage-3.sh
