#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

workdir="builderprojs"
outdir="builders"

function usage() {
  echo >&2 "Usage: $0 [-t] <yul-dsl-monorepo dir>"
  echo >&2 "Will create temporary cabal project in $workdir/ghc-<version>/ and"
  echo >&2 "write build script to $outdir/build-<version>.sh"
  echo >&2 "If [dependencies] is given, these override the dependency list in"
  echo >&2 "this script."
  echo >&2 "  -t   Test mode: stop after writing a freeze file"
  exit 1
}

test_mode=0
ghcversion="9.10.1"

function parse_args() {
  local num_wordargs_given=0

  for arg; do
    case "$arg" in
      -t)
        test_mode=1; ;;
      *)
        case "$num_wordargs_given" in
          0) yuldsldir="$arg"; ;;
          *) usage; ;;
        esac
        num_wordargs_given=$((num_wordargs_given + 1))
        ;;
    esac
  done

  if [[ $num_wordargs_given -lt 1 ]]; then usage; fi
}

parse_args "$@"

projdir="$workdir/yul-dsl-proj"
outscript="$outdir/build-Yolc-dev.sh"
#cabaldir_name="yul-dsl-cabal"
#cabaldir="${workdir}/${cabaldir_name}"

printf "\x1B[1m[mkyolcscript] Setting up cabal project directory in %s\x1B[0m\n" "$projdir"

rsync --verbose --archive --exclude=dist-newstyle --delete "$yuldsldir"/ "$projdir"

printf "\x1B[1m[mkyolcscript] Building cabal project\x1B[0m\n"

# TODO: deduplicate between this code and stage-2.sh

chroot="ubuntu-base"
ghcup_base=$(ghcup whereis basedir)

# These are symlinks to the corresponding folder in /usr in the ubuntu chroot.
# Just --ro-bind'ing them doesn't work because then the readlink() that gcc
# does on /bin will fail, and its include path detection will not work
# correctly (and for example miss c++ include paths, necessary to build
# simdjson in the Haskell 'text' library). So handle these symlinks correctly.
#
# Actually, this should also include /lib64. But if we include that (and remove
# the corresponding --ro-bind below in args[]), then nothing works: launching
# /bin/bash says ENOENT, probably referring to the loader not finding some
# library. I dunno, but it works like this now. Magic.
chroot_symlinks=( /bin /lib )
for f in "${chroot_symlinks[@]}"; do
  if [[ ! -L "${chroot}$f" ]]; then
    printf >&2 "\x1B[31;1m[mkyolcscript] Expected %s to be a symlink (to %s), but it isn't.\x1B[0m\n" "$chroot$f" "usr$f"
    exit 1
  fi
  if [[ ! "$(readlink "${chroot}$f")" = "usr$f" ]]; then
    printf >&2 "\x1B[31;1m[mkyolcscript] Expected %s to (be a symlink and) point to %s, but it doesn't.\x1B[0m\n" "$chroot$f" "usr$f"
    exit 1
  fi
done

symlink_bindargs=()
for f in "${chroot_symlinks[@]}"; do
  symlink_bindargs[${#symlink_bindargs[@]}]=--symlink
  symlink_bindargs[${#symlink_bindargs[@]}]=/usr$f
  symlink_bindargs[${#symlink_bindargs[@]}]=$f
done

args=(
  # Need bwrap >=v0.7.0 for --size
  --size $((100 * 1024 * 1024)) --tmpfs /tmp
  --ro-bind "${chroot}/usr/bin" /usr/bin
  --ro-bind "${chroot}/usr/lib" /usr/lib
  --ro-bind "${chroot}/usr/include" /usr/include
  --ro-bind "${chroot}/usr/lib64" /lib64  # This is wrong because it's a symlink in the actual image. But if we symlink, nothing works; see above.
  "${symlink_bindargs[@]}"
  --ro-bind "${chroot}/etc/alternatives" /etc/alternatives
  --ro-bind /etc/resolv.conf /etc/resolv.conf
  --dir "${ghcup_base}"
  --ro-bind "${ghcup_base}/bin"   "${ghcup_base}/bin"
  --ro-bind "${ghcup_base}/ghc"   "${ghcup_base}/ghc"
  --ro-bind "${ghcup_base}/cache" "${ghcup_base}/cache"
  --ro-bind "${HOME}/.cabal/bin" "${HOME}/.cabal/bin"
  --ro-bind "${HOME}/.cabal/store" "${HOME}/.cabal/store"
  --bind "${HOME}/.cabal/packages" "${HOME}/.cabal/packages"  # should be safe to modify this? Just stores downloads
  --dev-bind /dev/null /dev/null
  --bind "${workdir}" /builderprojs
  --bind "${projdir}" /project
  --setenv PATH "/bin:/usr/bin:${ghcup_base}/bin"
  --setenv GHCUP_INSTALL_BASE_PREFIX "$(dirname "${ghcup_base}")"
  --setenv GHCUP_SKIP_UPDATE_CHECK ""
  --proc /proc
  --chdir "/tmp"
  --new-session
  --unshare-all
  --share-net
  --die-with-parent
  --file 4 "/tmp/script.sh"
  /bin/bash "/tmp/script.sh"
)

GHCUP_RUN=(ghcup --no-verbose --offline run --ghc $ghcversion --)

bwrap "${args[@]}" 4<<EOF
set -xe
cd /project
rm -rf ~/.ghc
test -f cabal.project.freeze && rm cabal.project.freeze
[[ $test_mode -eq 1 ]] && exit
${GHCUP_RUN[@]} cabal \\
  --store-dir=/builderprojs/ghc-'$ghcversion'-cabal/store \\
  --logs-dir=/builderprojs/ghc-'$ghcversion'-cabal/logs \\
  build all --enable-tests -j2

# keep the latest yolc libraries
rm -rf /builderprojs/ghc-'$ghcversion'-cabal/store/{yul-dsl-*,yol-*,eth-abi-*}
rm -rf /builderprojs/ghc-'$ghcversion'-cabal/store/*/{yul-dsl-*,yol-*,eth-abi-*}
rm -rf /builderprojs/ghc-'$ghcversion'-cabal/store/*/package.db/{yul-dsl-*,yol-*,eth-abi-*}
${GHCUP_RUN[@]} ghc-pkg --package-db /builderprojs/ghc-'$ghcversion'-cabal/store/*/package.db recache

${GHCUP_RUN[@]} cabal \\
  --store-dir=/builderprojs/ghc-'$ghcversion'-cabal/store \\
  --logs-dir=/builderprojs/ghc-'$ghcversion'-cabal/logs \\
  install all --lib
EOF

printf "\x1B[1m[mkyolcscript] Collecting dependencies from cabal plan.json\x1B[0m\n"

#depends=( $( jq -r '.["install-plan"] | .[] | select(.["style"] == "local") | .depends | "-package-id " + .[]' "$projdir"/dist-newstyle/cache/plan.json ) )

depends=( $(
  for i in $(
    "${GHCUP_RUN[@]}" ghc-pkg --global list --show-unit-ids --simple-output 2>/dev/null
    "${GHCUP_RUN[@]}" ghc-pkg --package-db builderprojs/ghc-${ghcversion}-cabal/store/ghc-${ghcversion}-*/package.db list --show-unit-ids --simple-output 2>/dev/null
  ); do echo "-package-id " "$i"; done) )

printf "\x1B[1m[mkyolcscript] Writing %s\x1B[0m\n" "$outscript"

cat >"$outscript" <<EOF
#!/bin/sh
ghcup --no-verbose --offline run --ghc '$ghcversion' -- ghc \\
  -static \\
  -hide-all-packages \\
  -Wmissing-home-modules \\
  -no-user-package-db \\
  -XGHC2024 -XBlockArguments -XQualifiedDo -XOverloadedStrings -XUnicodeSyntax -XRebindableSyntax -XImpredicativeTypes -XLinearTypes -XTemplateHaskell \\
  -package-db /builderprojs/ghc-'$ghcversion'-cabal/store/ghc-'$ghcversion'-*/package.db \\
  ${depends[@]} \\
  -rtsopts \\
  "\$@"
EOF

chmod +x "$outscript"
