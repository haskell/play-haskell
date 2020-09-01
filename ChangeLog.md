# Revision history for pastebin-haskell

## 0.2.5.0 -- 2020-09-01

* Don't store completely empty pastes
* Restyle, better copy on index.html
* Robots.txt disallowing indexing

## 0.2.4.0 -- 2020-08-31

* Pasting multiple files

## 0.2.3.0 -- 2020-08-27

* Smaller paste size limit (64 KiB)
* Use SQLite for storage instead of in-memory map: better reliability
* Source-IP rate limiting (at most 5 pastes per 20 seconds)

## 0.2.2.0 -- 2020-08-25

* Paste link is now not `/paste/abcdefgh` but `/abcdefgh` for conciseness

## 0.2.1.0 -- 2020-08-24

* Interface:
  * Some CSS
  * Bundle highlight.js with only Haskell support for lighter webpage
  * Skip paste response page and immediately redirect to the paste itself
* Server:
  * Maximum paste size of 4MB
  * Persist pastes to disk for seamless server restarts

## 0.2.0.0 -- 2020-08-24

* Version bounds for dependencies
* Better text on paste creation page
* "Make another paste" link on paste read page
* Reload HTML pages without server restart on SIGUSR1

## 0.1.0.0 -- 2020-08-21

* First version. Released on an unsuspecting world.
