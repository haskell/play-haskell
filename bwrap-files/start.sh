#!/bin/bash
set -euo pipefail

filesdir="$(dirname "$0")"
cd "$filesdir"

ghcup_base=$(ghcup whereis basedir)

chroot="${filesdir}/ubuntu-base"

args=(
  --tmpfs /tmp
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
  --setenv GHCUP_INSTALL_BASE_PREFIX "$(dirname ${ghcup_base})"
  --proc /proc
  --chdir "/tmp"
  --new-session
  --unshare-all
  --die-with-parent
  --file 4 "/tmp/entry.sh"
  /bin/bash "/tmp/entry.sh"
)

# Turn off core files
ulimit -c 0

# Limit on the number of processes
ulimit -u 10000

# Limit memory to 600 MiB. Note that the compiled program gets a 500 MiB memory
# limit via the GHC RTS, so this limit is 1. to constrain GHC itself (including
# any TH code), and 2. as a second-layer defense.
ulimit -d $(( 600 * 1024 ))

exec bwrap "${args[@]}" 4<"${filesdir}/entry.sh"
