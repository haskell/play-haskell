#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

workdir="builderprojs"
outdir="builders"

function usage() {
  echo >&2 "Usage: $0 [-t] <ghc version> [dependencies]"
  echo >&2 "Will create temporary cabal project in $workdir/ghc-<version>/ and"
  echo >&2 "write build script to $outdir/build-<version>.sh"
  echo >&2 "If [dependencies] is given, these override the dependency list in"
  echo >&2 "this script."
  echo >&2 "  -t   Test mode: stop after writing a freeze file"
  exit 1
}

test_mode=0
ghcversion=""
dependencies_override=""

function parse_args() {
  local num_wordargs_given=0

  for arg; do
    case "$arg" in
      -t)
        test_mode=1; ;;
      *)
        case "$num_wordargs_given" in
          0) ghcversion="$arg"; ;;
          1) dependencies_override="$arg"; ;;
          *) usage; ;;
        esac
        num_wordargs_given=$((num_wordargs_given + 1))
        ;;
    esac
  done

  if [[ $num_wordargs_given -lt 1 ]]; then usage; fi
}

parse_args "$@"

projdir="$workdir/ghc-$ghcversion-proj"
cabaldir="$workdir/ghc-$ghcversion-cabal"
outscript="$outdir/build-$ghcversion.sh"

printf "\x1B[1m[mkbuildscript] Setting up cabal project directory in %s\x1B[0m\n" "$projdir"

mkdir -p "$projdir"
mkdir -p "$cabaldir"
mkdir -p "$outdir"

# wishlist:
# adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytestring, Cabal, case-insensitive, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contravariant, copilot >=3.19, copilot-c99 >=3.19, copilot-core >=3.19, copilot-interpreter >=3.19, copilot-language >=3.19, copilot-libraries >=3.19, copilot-prettyprinter >=3.19, copilot-theorem >=3.19, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, dunai >=0.13, effectful, effectful-core, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-experimental, ghc-heap, ghc-internal, ghci, ghc-prim, hashable, hashtables, haskeline, haskell-src-exts, haskell-src-meta, HList, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, libiserv, linear-base, linear-generics, megaparsec, monad-control, MonadRandom, mtl, natural-arithmetic, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, pretty-simple, prettyprinter, primitive, primitive-addr, primitive-offset, process, profunctors, QuickCheck, random, reflection, rts, safe, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, storable-tuple, strict, syb, tagged, template-haskell, temporary, terminfo, text, text-builder-linear, text-short, th-abstraction, these, th-expand-syns, th-lift, th-lift-instances, th-orphans, th-reify-many, time, time-compat, torsor, transformers, transformers-base, transformers-compat, tuples, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, what4 >= 1.5, wide-word, witherable, xhtml, xml, Yampa, zigzag, zlib

function build_depends_for() {
  if [[ -n $dependencies_override ]]; then
    echo "$dependencies_override"
    return
  fi

  case "$1" in
    8.4.4)
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytestring, Cabal, case-insensitive, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contravariant, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, dunai, effectful, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghci, ghc-prim, hashable, hashtables, haskeline, haskell-src-exts, haskell-src-meta, HList, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, megaparsec, monad-control, MonadRandom, mtl, natural-arithmetic, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, pretty-simple, prettyprinter, primitive, primitive-addr, primitive-offset, process, profunctors, QuickCheck, random, reflection, rts, safe, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, storable-tuple, strict, syb, tagged, template-haskell, temporary, terminfo, text, text-short, th-abstraction, these, th-expand-syns, th-lift, th-lift-instances, th-orphans, th-reify-many, time, time-compat, torsor, transformers, transformers-base, transformers-compat, tuples, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, wide-word, witherable, xhtml, xml, Yampa, zigzag, zlib"
      ;;
    8.6.5)
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytebuild, byteslice, bytesmith, bytestring, Cabal, case-insensitive, chronos, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contiguous, contravariant, copilot >=3.19, copilot-c99, copilot-core >=3.19, copilot-interpreter >=3.19, copilot-language >=3.19, copilot-libraries >=3.19, copilot-prettyprinter >=3.19, copilot-theorem >=3.19, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, dunai, effectful, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, hashable, hashtables, haskeline, haskell-src-exts, haskell-src-meta, HList, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, libiserv, megaparsec, monad-control, MonadRandom, mtl, natural-arithmetic, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, pretty-simple, prettyprinter, primitive, primitive-addr, primitive-offset, primitive-unlifted, process, profunctors, QuickCheck, random, reflection, rts, run-st, safe, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, storable-tuple, strict, syb, tagged, template-haskell, temporary, terminfo, text, text-short, th-abstraction, these, th-expand-syns, th-lift, th-lift-instances, th-orphans, th-reify-many, time, time-compat, torsor, transformers, transformers-base, transformers-compat, tuples, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, what4 >= 1.5, wide-word, witherable, xhtml, xml, Yampa, zigzag, zlib"
      ;;
    9.6.*)
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytestring, Cabal, case-insensitive, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contravariant, copilot >=3.19, copilot-c99 >=3.19, copilot-core >=3.19, copilot-interpreter >=3.19, copilot-language >=3.19, copilot-libraries >=3.19, copilot-prettyprinter >=3.19, copilot-theorem >=3.19, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, effectful, effectful-core, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, hashable, hashtables, haskeline, haskell-src-exts, haskell-src-meta, HList, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, libiserv, linear-base, linear-generics, megaparsec, monad-control, MonadRandom, mtl, natural-arithmetic, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, pretty-simple, prettyprinter, primitive, primitive-addr, primitive-offset, process, profunctors, QuickCheck, random, reflection, rts, safe, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, storable-tuple, strict, syb, tagged, template-haskell, temporary, terminfo, text, text-builder-linear, text-short, th-abstraction, these, th-expand-syns, th-lift, th-lift-instances, th-orphans, th-reify-many, time, time-compat, torsor, transformers, transformers-base, transformers-compat, tuples, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, what4 >= 1.5, wide-word, witherable, xhtml, xml, Yampa, zigzag, zlib"
      ;;
    9.8.*)
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytestring, Cabal, case-insensitive, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contravariant, copilot >=3.19, copilot-c99 >=3.19, copilot-core >=3.19, copilot-interpreter >=3.19, copilot-language >=3.19, copilot-libraries >=3.19, copilot-prettyprinter >=3.19, copilot-theorem >=3.19, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, effectful, effectful-core, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, hashable, hashtables, haskeline, haskell-src-exts, haskell-src-meta, HList, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, linear-base, linear-generics, megaparsec, monad-control, MonadRandom, mtl, natural-arithmetic, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, pretty-simple, prettyprinter, primitive, primitive-addr, primitive-offset, process, profunctors, QuickCheck, random, reflection, rts, safe, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, storable-tuple, strict, syb, tagged, template-haskell, temporary, terminfo, text, text-builder-linear, text-short, th-abstraction, these, th-expand-syns, th-lift, th-lift-instances, th-orphans, th-reify-many, time, time-compat, torsor, transformers, transformers-base, transformers-compat, tuples, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, what4 >= 1.5, wide-word, witherable, xhtml, xml, Yampa, zigzag, zlib"
      ;;
    9.10.*)
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bytestring, Cabal, case-insensitive, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contravariant, copilot-core >=3.19, copilot-interpreter >=3.19, copilot-prettyprinter >=3.19, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, effectful, effectful-core, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-experimental, ghc-heap, ghc-internal, ghci, ghc-prim, hashable, hashtables, haskeline, haskell-src-exts, haskell-src-meta, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, linear-base, linear-generics, megaparsec, monad-control, MonadRandom, mtl, natural-arithmetic, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parsec, parser-combinators, pretty, pretty-simple, prettyprinter, primitive, primitive-addr, primitive-offset, process, profunctors, QuickCheck, random, reflection, rts, safe, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, storable-tuple, strict, syb, tagged, template-haskell, temporary, terminfo, text, text-builder-linear, text-short, th-abstraction, these, th-expand-syns, th-lift, th-lift-instances, th-orphans, th-reify-many, time, time-compat, torsor, transformers, transformers-base, transformers-compat, tuples, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, wide-word, witherable, xhtml, xml, Yampa, zigzag, zlib"
      ;;
    *)
      # For 9* versions, add linear-base
      case "$1" in
        9*)
          echo -n "linear-base, linear-generics, storable-tuple, text-builder-linear, "
          ;;
      esac
      echo "adjunctions, aeson, array, assoc, async, attoparsec, base, base16, base64, bifunctors, bimap, binary, bitvec, bitwise, BoundedChan, bv-sized, bytebuild, byteslice, bytesmith, bytestring, Cabal, case-insensitive, chronos, clock, colour, comonad, concurrent-extra, config-value, constraints, containers, contiguous, contravariant, copilot >=3.19, copilot-c99, copilot-core >=3.19, copilot-interpreter >=3.19, copilot-language >=3.19, copilot-libraries >=3.19, copilot-prettyprinter >=3.19, copilot-theorem >=3.19, data-array-byte, data-default, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, data-fix, data-reify, deepseq, deriving-compat, directory, distributive, dlist, dunai, effectful, effectful-core, exceptions, extra, filepath, fingertree, free, generically, generic-deriving, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, hashable, hashtables, haskeline, haskell-src-exts, haskell-src-meta, HList, hpc, ieee754, indexed-traversable, indexed-traversable-instances, integer-gmp, integer-logarithms, invariant, io-streams, kan-extensions, language-c99, language-c99-simple, language-c99-util, lens, libBF, libiserv, megaparsec, monad-control, MonadRandom, mtl, natural-arithmetic, old-locale, OneTuple, optparse-applicative, ordered-containers, parallel, parameterized-utils, parsec, parser-combinators, pretty, pretty-simple, prettyprinter, primitive, primitive-addr, primitive-offset, primitive-unlifted, process, profunctors, QuickCheck, random, reflection, rts, run-st, safe, safe-exceptions, s-cargot, scientific, semialign, semigroupoids, semigroups, simple-affine-space, splitmix, StateVar, stm, storable-tuple, strict, syb, tagged, template-haskell, temporary, terminfo, text, text-short, th-abstraction, these, th-expand-syns, th-lift, th-lift-instances, th-orphans, th-reify-many, time, time-compat, torsor, transformers, transformers-base, transformers-compat, tuples, unbounded-delays, unix, unliftio, unliftio-core, unordered-containers, utf8-string, uuid-types, vector, vector-algorithms, vector-stream, versions, void, what4 >= 1.5, wide-word, witherable, xhtml, xml, Yampa, zigzag, zlib"
      ;;
  esac
}

cat >"$projdir/sandbox.cabal" <<EOF
cabal-version: 2.0
name:          sandbox
version:       0.1.0.0
build-type:    Simple
executable thing
  main-is: Main.hs
  hs-source-dirs: .
  default-language: Haskell2010
  ghc-options: -Wall -rtsopts
  build-depends: $(build_depends_for "$ghcversion")
EOF
# shorter dep list that only builds splitmix and random:
# build-depends: array, base, binary, bytestring, Cabal, containers, deepseq, directory, exceptions, filepath, ghc-boot, ghc-boot-th, ghc-heap, ghci, ghc-prim, haskeline, hpc, integer-gmp, libiserv, mtl, parsec, pretty, process, random, rts, stm, template-haskell, terminfo, text, time, transformers, unix, xhtml

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
set -e
cd /project
test -f cabal.project.freeze && rm cabal.project.freeze
ghcup --no-verbose --offline run --ghc '$ghcversion' -- \\
  cabal --store-dir=/builderprojs/ghc-'$ghcversion'-cabal/store --logs-dir=/builderprojs/ghc-'$ghcversion'-cabal/logs --minimize-conflict-set freeze
[[ $test_mode -eq 1 ]] && exit
ghcup --no-verbose --offline run --ghc '$ghcversion' -- \\
  cabal --store-dir=/builderprojs/ghc-'$ghcversion'-cabal/store --logs-dir=/builderprojs/ghc-'$ghcversion'-cabal/logs build -j1
EOF

printf "\x1B[1m[mkbuildscript] Collecting dependencies from cabal plan.json\x1B[0m\n"

depends=( $( jq -r '.["install-plan"] | .[] | select(.["style"] == "local") | .depends | "-package-id " + .[]' "$projdir"/dist-newstyle/cache/plan.json ) )

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
