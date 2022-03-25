#!/bin/bash
set -euo pipefail

filesdir="$(dirname "$0")"
cd "$filesdir"

homedir="$1"

args=(
  --ro-bind /bin/bash /bin/bash
  --ro-bind /bin/gcc /bin/gcc
  --ro-bind /bin/dirname /bin/dirname
  --ro-bind /bin/cat /bin/cat
  --ro-bind /usr/lib /usr/lib
  --ro-bind /lib /lib
  --ro-bind /lib64 /lib64
  --ro-bind "${homedir}/.ghcup/bin" "${homedir}/.ghcup/bin"
  --ro-bind "${homedir}/.ghcup/ghc" "${homedir}/.ghcup/ghc"
  --symlink bash /bin/sh
  --setenv PATH "/bin"
  --tmpfs /tmp
  --proc /proc
  --dir "${homedir}/workdir"
  --new-session
  --unshare-all
  --die-with-parent
  --file 4 "${homedir}/workdir/entry.sh"
  /bin/bash "${homedir}/workdir/entry.sh"
)

exec bwrap "${args[@]}" 4<"${filesdir}/entry.sh"
