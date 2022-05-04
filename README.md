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
cabal run pastebin-haskell
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
cabal run pastebin-haskell
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
- `make reload-pages` (or re-run `cabal run pastebin-haskell`) to reload mustache pages


# Trivial pastebin service

    18:30 <sm[m]> also: a new paste bin customized/structured for #haskell to elicit more info ? eg cabal or yaml file, stack yaml file, command, output, platform..
    18:32 <tomsmeding> sm[m]: building that pastebin service would be trivial, question is who'd host it
    18:32 <sm[m]> trivial eh :)
    18:33 <sm[m]> if you build it I'll host it :)
    18:33 <tomsmeding> sure lol

This is a simple pastebin service built for the `#haskell` channel on Freenode.
There is little documentation except the, of course, self-documenting code.


## Page reloading

The server reloads the HTML pages in memory upon receipt of SIGUSR1.

## Storage

Pastes are stored in an SQLite database in the file `pastes.db`.

## Pasting from your terminal

The following command:

    curl -d 'name1=a.txt' --data-urlencode 'code1@file.txt' https://url.of.the.pastebin.example.com/paste

will create a paste with one file called `a.txt` containing the contents of
`file.txt` in your current directory. To read from stdin, use `/dev/stdin`
instead of `file.txt`. To post more files, give <code>name<i>N</i></code> and
<code>code<i>N</i></code> parameters for each file for _N_ from 1 up to the
number of files to paste. An empty filename results in a file without a name on
the paste read page.

Please don't abuse, but then the whole service is "please don't abuse".
