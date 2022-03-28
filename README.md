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


# Playground

    sudo apt update && sudo apt install earlyoom bubblewrap make npm
    # Change "-r 60" to "-r 3600" in /etc/default/earlyoom (less spammy logs)
    sudo systemctl restart earlyoom

    # Install ghcup: https://www.haskell.org/ghcup/ (skip HLS and stack)
    # Open a new terminal to get ghcup in PATH

    make  # equivalent to `make chroot frontend-dependencies frontend`
    cabal run
