# Revision history for kaas

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
