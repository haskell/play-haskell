# Revision history for pastebin-haskell

## 0.2.13.0 -- 2021-02-26

* Dark mode using CSS `prefers-color-scheme` query (suggested by my eyes)
* Fix file anchor links

## 0.2.12.0 -- 2021-02-25

* Allow pastes to expire (though not by default) (suggested by `geekosaur`, discussed with `__monty__`)

## 0.2.11.0 -- 2021-01-12

* Line numbers! (New year, more numbers)
* Somewhat simpler "spam detection" module
* Bug fixes: fix line heights, don't error on invalid utf8

## 0.2.10.0 -- 2020-09-21

* Download pastes as `.tar.gz` (suggested by `sm`)
* Show paste key in read page title

## 0.2.9.0 -- 2020-09-19

* Apply some accessibility guidelines on the html pages
  * Slightly higher contrast on submit button, ARIA attributes and roles, keyboard-selectable delete-file button (using tabindex)
* Reword "Edit this paste" button to "Clone and edit this paste"
* Track parents: the read page now shows the paste that was edited to produce
  this one, if any. (suggested by `sm`)

## 0.2.8.0 -- 2020-09-05

* Reading pastes
  * Enable syntax highlighting by default if you've never visited the page before
  * Use "#" for files without filename (thanks `sm`)
  * "Raw" links to plain-text file contents
* Store redirect response now contains paste URL in body to aid pasting using curl
  * `curl -d 'name1=' --data-urlencode 'code1@/dev/stdin' https://your.pastebin.example.com/paste`
* Paste source IP is now stored for future spam tracking

## 0.2.7.0 -- 2020-09-03

* Looks
  * Revamp and shorten landing page text; thanks `sm` and `geekosaur` for discussion
  * Header `#haskell` now links to [IRC wiki page](https://wiki.haskell.org/IRC_channel)
* Reading pastes
  * Files names on read page are anchor links pointing to that file
* Making pastes
  * Keyboard shortcut: ctrl-enter (or meta-enter) immediately submits paste
  * Autofocus first textarea on index page

## 0.2.6.0 -- 2020-09-03

* Edit existing pastes via a button on the read page
* (Also, internal refactoring)

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
