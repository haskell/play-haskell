#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

workdir="builderprojs"
outdir="builders"

if [[ $# -ne 1 ]]; then
  echo >&2 "Usage: $0 <ghc version>"
  echo >&2 "Will create temporary cabal project in $workdir/ghc-<version>/ and"
  echo >&2 "write build script to $outdir/build-<version>.sh"
  exit 1
fi

ghcversion="$1"
projdir="$workdir/ghc-$ghcversion-proj"
cabaldir="$workdir/ghc-${ghcversion}-cabal"
outscript="$outdir/build-$ghcversion.sh"

printf "\x1B[1m[mkbuildscript] Setting up cabal project directory in %s\x1B[0m\n" "$projdir"

mkdir -p "$projdir"
mkdir -p "$cabaldir"
mkdir -p "$outdir"

function build_depends_for() {
  case "$1" in
    8.6.5)
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytestring, Cabal, case-insensitive, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contravariant, copilot >=3, copilot-c99, copilot-core, copilot-interpreter, copilot-language, copilot-libraries, copilot-prettyprinter, copilot-theorem, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, dunai, effectful, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, hashable, hashtables, haskeline, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, libiserv, megaparsec, monad-control, MonadRandom, mtl, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, prettyprinter, primitive, process, profunctors, QuickCheck, random, reflection, rts, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, strict, tagged, template-haskell, temporary, terminfo, text, text-short, th-abstraction, these, th-lift, th-lift-instances, time, time-compat, transformers, transformers-base, transformers-compat, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, what4, witherable, xhtml, xml, Yampa, zlib"
      ;;
    9.6.*)
      echo "array, base, binary, bytestring, Cabal, containers, deepseq, directory, exceptions, filepath, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, haskeline, hpc, integer-gmp, libiserv, mtl, parsec, pretty, prettyprinter, primitive, process, random, rts, semigroups, stm, template-haskell, terminfo, text, time, transformers, transformers-base, unix, vector, xhtml, extra, bitvec, tagged, vector-algorithms, transformers-compat, linear-base, linear-generics, storable-tuple, text-builder-linear, void"
      ;;
    *-alpha*)
      echo "array, base, binary, bytestring, Cabal, containers, deepseq, directory, exceptions, filepath, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, haskeline, hpc, integer-gmp, mtl, parsec, pretty, prettyprinter, primitive, process, random, rts, semigroups, stm, template-haskell, terminfo, text, time, transformers, transformers-base, unix, vector, xhtml, extra, bitvec, tagged, vector-algorithms, transformers-compat, void"
      ;;
    *)
      # For 9* versions, add linear-base
      case "$1" in
        9*)
          echo -n "linear-base, linear-generics, storable-tuple, text-builder-linear, "
          ;;
      esac
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytestring, Cabal, case-insensitive, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contravariant, copilot >=3, copilot-c99, copilot-core, copilot-interpreter, copilot-language, copilot-libraries, copilot-prettyprinter, copilot-theorem, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, dunai, effectful, effectful-core, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, hashable, hashtables, haskeline, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, libiserv, megaparsec, monad-control, MonadRandom, mtl, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, prettyprinter, primitive, process, profunctors, QuickCheck, random, reflection, rts, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, strict, tagged, template-haskell, temporary, terminfo, text, text-short, th-abstraction, these, th-lift, th-lift-instances, time, time-compat, transformers, transformers-base, transformers-compat, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, what4, witherable, xhtml, xml, Yampa, zlib"
      ;;
  esac
}

cat >"$projdir/sandbox.cabal" <<EOF
cabal-version: 2.0
name:          sandbox
version:       0.1.0.0
build-type:    Simple
executable thing
  main-is:          Main.hs
  hs-source-dirs:   .
  default-language: Haskell2010
  ghc-options:      -Wall -rtsopts
  build-depends: $(build_depends_for "$ghcversion")
  -- shorter dep list that only builds splitmix and random:
  -- build-depends: array, base, binary, bytestring, Cabal, containers, deepseq, directory, exceptions, filepath, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, haskeline, hpc, integer-gmp, libiserv, mtl, parsec, pretty, process, random, rts, stm, template-haskell, terminfo, text, time, transformers, unix, xhtml
EOF

cat >"$projdir/Main.hs" <<EOF
module Main where
main :: IO () ; main = return ()
EOF

cat >"$projdir/cabal.project" <<EOF
packages: .
EOF

printf "\x1B[1m[mkbuildscript] Building cabal project\x1B[0m\n"

# TODO: deduplicate between this code and stage-2.sh

chroot="ubuntu-base"
ghcup_base=$(ghcup whereis basedir)

mkdir -p "${cabaldir}/store" "${cabaldir}/logs"

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
    printf >&2 "\x1B[31;1m[mkbuildscript] Expected %s to be a symlink (to %s), but it isn't.\x1B[0m\n" "$chroot$f" "usr$f"
    exit 1
  fi
  if [[ ! "$(readlink "${chroot}$f")" = "usr$f" ]]; then
    printf >&2 "\x1B[31;1m[mkbuildscript] Expected %s to (be a symlink and) point to %s, but it doesn't.\x1B[0m\n" "$chroot$f" "usr$f"
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

bwrap "${args[@]}" 4<<EOF
# set -x
cd /project
ghcup --no-verbose --offline run --ghc '$ghcversion' -- \\
  cabal --store-dir=/builderprojs/ghc-'$ghcversion'-cabal/store --logs-dir=/builderprojs/ghc-'$ghcversion'-cabal/logs freeze
ghcup --no-verbose --offline run --ghc '$ghcversion' -- \\
  cabal --store-dir=/builderprojs/ghc-'$ghcversion'-cabal/store --logs-dir=/builderprojs/ghc-'$ghcversion'-cabal/logs build
EOF

printf "\x1B[1m[mkbuildscript] Collecting dependencies from cabal plan.json\x1B[0m\n"

depends=( $(
    jq -r '.["install-plan"] | .[] | select(.["style"] == "local") | .depends | .[]' "$projdir"/dist-newstyle/cache/plan.json |
    sed 's/^/-package-id /'
  ) )

printf "\x1B[1m[mkbuildscript] Writing %s\x1B[0m\n" "$outscript"

cat >"$outscript" <<EOF
#!/bin/sh
ghcup --no-verbose --offline run --ghc '$ghcversion' -- ghc \\
  -static \\
  -hide-all-packages \\
  -Wmissing-home-modules \\
  -no-user-package-db \\
  -package-db /builderprojs/ghc-'$ghcversion'-cabal/store/ghc-'$ghcversion'/package.db \\
  ${depends[@]} \\
  -rtsopts \\
  "\$@"
EOF

chmod +x "$outscript"
