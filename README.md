# Haskell Playground

There's lots still to do, see
[TODO.txt](https://github.com/tomsmeding/play-haskell/blob/master/TODO.txt).
If you want to contribute, perhaps connect with me (either via an issue or on
[IRC](https://wiki.haskell.org/IRC_channel)) before writing lots of code.

## GHCup target platform

Because the GHCup installation from the host machine will be used as-is in the
containers of the workers, and because said containers run Ubuntu, the desired
GHC versions must be installed as follows with `ghcup`:

    ghcup install ghc -p x86_64-deb10-linux 8.10.7

This ensures that the GHCs will work in the Ubuntu container. Note that
currently (2022-08), Ubuntu GHCs seem to work fine on Arch Linux, for example.

## Installation

### System setup: Ubuntu

```bash
# Note: earlyoom is only advised if you're deploying; not necessary on your local machine
sudo apt update && sudo apt install earlyoom bubblewrap make npm jq
# Change "-r 60" to "-r 3600" in /etc/default/earlyoom (less spammy logs)
sudo systemctl restart earlyoom

sudo apt install build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 pkg-config
# Install ghcup: https://www.haskell.org/ghcup/ (skip HLS and stack)
# Open a new terminal to get ghcup in PATH
```

### System setup: Arch Linux

```bash
# Note: earlyoom is only advised if you're deploying; not necessary on your local machine
sudo pacman -Syu earlyoom bubblewrap make npm jq
sudo systemctl enable --now earlyoom

sudo pacman -S base-devel
# Install ghcup: https://www.haskell.org/ghcup/ (skip HLS and stack)
# Open a new terminal to get ghcup in PATH
# MAKE SURE TO INSTALL GHC FOR TARGET x86_64-deb10-linux
# See above in the 'GHCup target platform' section
```

### Building the applications

```bash
git clone https://github.com/tomsmeding/play-haskell --recurse-submodules
cd play-haskell
```

To build the server (that hosts the website but doesn't run any user code):
```bash
cd play-haskell-server
make
cabal build
```

To build the worker (that the server will connect to, and that runs user code (in a sandbox)):
```bash
cd play-haskell-worker
make  # Or equivalently:
      #   make chroot ; make bwrap-files/systemd-run-shim ; make builders
      # 'make chroot' is interactive, select en_US.UTF-8.
      # 'make builders' takes a long time because it builds all packages that should
      #   be available on the playground with all GHCs you have installed with GHCup.
      #   If you want just a few of those GHCs to work in the playground, manually run:
      #     bwrap-files/mkbuildscript.sh 9.2.5
      #     bwrap-files/mkbuildscript.sh 8.10.7
      #   etc., once for each version you want to be available. Change available
      # packages in bwrap-files/mkbuildscript.sh .
cabal build
```

Optionally, if the machine you're building on does not have enough RAM, do this and use `cabal build -j1`:

```bash
sudo fallocate -l 4G /swapfile
sudo chmod go-rw /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
```

During development:

- `make frontend-dependencies` in `play-haskell-server/` whenever [play-haskell-server/static/package.json](https://github.com/tomsmeding/play-haskell/blob/master/play-haskell-server/static/package.json) changes
- `make frontend` in `play-haskell-server/` whenever some `*.ts` file for the frontend changes
- `make reload-pages` in `play-haskell-server/` (or re-run `cabal run play-haskell-server`) to reload mustache pages (or send `SIGUSR1`)

### Running the applications

To run the server: (all in `play-haskell-server/`)
1. Put a good password in `adminpass.txt` (filename does not matter, but adjust below)
2. `cabal run gen-secret-key -- secretkey.txt` (filename does not matter, but adjust below)
    - Remember the public key that it prints, say `$SERVER_PUBKEY`
3. Optional: put one or more lines of the form `{http://}hostname{:port} $WORKER1_PUBKEY` in `preload-workers.txt`, where the `{}` parts are optional (if not given, uses https on port 443); that `$WORKER1_PUBKEY` is produced by the steps to run a worker, see below
4. `cabal run play-haskell-server -- --adminpassfile adminpass.txt --secretkey secretkey.txt --preloadworkers preload-workers.txt` (omit the `--preloadworkers` if you didn't do that step)
5. See `cabal run play-haskell-server -- --help` for more info on options; the server listens with http on port 8123 by default

To run a worker: (all in `play-haskell-worker/`)
1. `cabal run gen-secret-key -- secretkey.txt` (filename does not matter, but adjust below)
    - Remember the public key that it prints, say `$WORKER1_PUBKEY`
2. Put one or more `$SERVER_PUBKEY` in `trustedkeys.txt` (filename does not matter, but adjust below)
2. `cabal run play-haskell-worker -- --secretkey secretkey.txt --trustedkeys trustedkeys.txt +RTS -N`
3. See `cabal run play-haskell-worker -- --help` for more info on options; the worker listens with http on port 8124 by default, use a reverse proxy ssl terminator to get https

### Server admin interface

On `example.com/admin` the server exposes a simple admin interface through which you can add and remove workers. If you don't want to add workers this way, use the `--preloadworkers` flag described above.

## Storage

Pasted snippets are stored in an SQLite database in the file `pastes.db`.
