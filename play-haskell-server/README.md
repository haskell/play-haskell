# Playground

There's lots still to do, see
[TODO.txt](https://github.com/tomsmeding/pastebin-haskell/blob/play/TODO.txt).
Currently I'm working on a horizontally scalable backend on the `scalable-play`
branch, so for backend PRs, perhaps connect with me (either via an issue or on
[IRC](https://wiki.haskell.org/IRC_channel)) before writing lots of code.

## Installation

On ubuntu (though 20.04 doesn't work because libarchive is too old)

```bash
# Note: earlyoom is only advised if you're deploying; not necessary on your local machine
sudo apt update && sudo apt install earlyoom bubblewrap make npm
# Change "-r 60" to "-r 3600" in /etc/default/earlyoom (less spammy logs)
sudo systemctl restart earlyoom

sudo apt install build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
# Install ghcup: https://www.haskell.org/ghcup/ (skip HLS and stack)
# Open a new terminal to get ghcup in PATH

sudo apt install zlib1g-dev pkg-config libarchive-dev
make  # equivalent to `make chroot frontend-dependencies frontend`; note, `make chroot` is interactive
cabal run play-haskell-server
```

Or on arch linux:

```bash
# Note: earlyoom is only advised if you're deploying; not necessary on your local machine
sudo pacman -Syu earlyoom bubblewrap make npm
sudo systemctl enable --now earlyoom

sudo pacman -S base-devel
# Install ghcup: https://www.haskell.org/ghcup/ (skip HLS and stack)
# Open a new terminal to get ghcup in PATH

make  # equivalent to `make chroot frontend-dependencies frontend`; note, `make chroot` is interactive
cabal run play-haskell-server
```

Optionally, if the machine you're building on does not have enough RAM, do this and use `cabal build -j1`:

```bash
sudo fallocate -l 4G /swapfile
sudo chmod go-rw /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
```

During development:

- `make frontend-dependencies` whenever [static/package.json](https://github.com/tomsmeding/pastebin-haskell/blob/play/static/package.json) changes
- `make frontend` whenever some `*.ts` file for the frontend changes
- `make reload-pages` (or re-run `cabal run play-haskell-server`) to reload mustache pages (or send `SIGUSR1`)

## Storage

Pasted snippets are stored in an SQLite database in the file `pastes.db`.
