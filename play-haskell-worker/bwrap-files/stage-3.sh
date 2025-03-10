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
# if [[ "${version//[0-9.alpha-]/}" != "" ]]; then
#   echo >&2 "Invalid version: $version"
#   exit 1
# fi

if [[ ! -f /builders/build-"${version}".sh ]]; then
  echo >&2 "Version ${version} not available"
  exit 1
fi

yolc_main() {
echo '
-- base
import Prelude
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
-- text
import qualified Data.Text.Lazy.IO as TIO
-- yol-suite
import YolSuite.YOLC.RunYolModes
--
import Project

main :: IO ()
main = yulObjectMode object >>= \case
  Left  err -> TIO.hPutStrLn stderr err >> exitFailure
  Right out -> TIO.putStrLn out >> exitSuccess
'
}

case "$command" in
  run)
    export YOLC_DEBUG_LEVEL=0
    cat > Project.hs
    yolc_main > Main.hs
    time /builders/build-"${version}".sh "${opt}" Project.hs Main.hs >/tmp/null 2>&"$ghc_out_fd"
    if [[ -f Main ]]; then
      ./Main +RTS -M500m -RTS
    else
      echo >&2 "Compilation succeeded, but no executable was produced. Perhaps your module is not called 'Main'?"
    fi
    ;;
  core)
    cat >input.hs
    /builders/build-"${version}".sh -ddump-simpl -ddump-to-file "${opt}" input.hs >/tmp/null 2>&"$ghc_out_fd"
    cat input.dump-simpl
    ;;
  asm)
    cat >input.hs
    /builders/build-"${version}".sh -ddump-asm -ddump-to-file "${opt}" input.hs >/tmp/null 2>&"$ghc_out_fd"
    cat input.dump-asm
    ;;
  *)
    echo >&2 "Unknown command"
    exit 1
    ;;
esac
