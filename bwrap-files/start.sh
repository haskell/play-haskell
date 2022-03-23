#!/bin/bash
set -euo pipefail

filesdir="$(dirname "$0")"
cd "$filesdir"

homedir="$1"

args=(
  --ro-bind /bin /bin
  --ro-bind /usr/bin /usr/bin
  --ro-bind /usr/lib /usr/lib
  --ro-bind /lib /lib
  --ro-bind /lib64 /lib64
  --ro-bind /etc /etc
  --ro-bind "${homedir}/.ghcup/bin" "${homedir}/.ghcup/bin"
  --ro-bind "${homedir}/.ghcup/ghc" "${homedir}/.ghcup/ghc"
  --tmpfs /tmp
  --dev /dev
  --proc /proc
  --dir "${homedir}/workdir"
  --new-session
  --unshare-all
  --die-with-parent
  --perms 700 --file 4 "${homedir}/workdir/entry.sh"
  "${homedir}/workdir/entry.sh"
)

exec bwrap "${args[@]}" 4<"${filesdir}/entry.sh"
