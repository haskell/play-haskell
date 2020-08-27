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
