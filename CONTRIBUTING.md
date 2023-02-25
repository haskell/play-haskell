# Contributing guidelines

Thank you for wanting to make the Haskell Playground better!
Here are some guidelines to make sure I (@tomsmeding) can help you most effectively.

Please also read and acknowledge the [Code of Conduct](https://github.com/haskell/play-haskell/blob/master/CODE_OF_CONDUCT.md).

## Tooling and code style

Please use cabal-install version 3.6 or higher to compile the project.
The version of GHC to use when compiling the playground can be found in the [cabal.project file](https://github.com/haskell/play-haskell/blob/master/cabal.project) (check the `with-compiler:` line).

Build instructions can be found in the [readme](https://github.com/haskell/play-haskell/blob/master/README.md).

Code style: please imitate the style of the code around the place you are editing.
I have not (yet) found a code formatter that does quite what I want.

Testing: No proper tests are yet in place.
Please run a server and a worker and manually verify that the playground still works as it should.
(If you want to work on tests, [please](https://github.com/haskell/play-haskell/issues/4)!)

## Bug reports

Found an issue, or something that does not work?

- If the issue you found seems to be a problem with the hosted instance at [play.haskell.org](https://play.haskell.org) and not with the code itself, please contact me via one of the methods listed on [my website](https://tomsmeding.com).
- If you think the issue is actually with the code, please open an issue using the [bug report template][templ-bug_report].
  Thanks for reporting!

## Feature requests

Ideas for new functionality are always welcome!
Feel free to open an issue on the issue tracker using the [feature request template][templ-feature_request].

## I want to help with design

If you feel that something about the design or user interface of the playground can be improved (probably!), please post your suggestions under [issue #3](https://github.com/haskell/play-haskell/issues/3).
If you see an existing feature request in the issue list that you like but where you feel that the design could be improved, please chip in!
Like most programmers, I'm not a designer, and knowledgeable input here is very welcome.

## I want to help with coding

Thanks!

- If you have an idea of a feature that you want to implement, and implementing it takes more than 5 minutes, **please first** open an issue using the [feature request template][templ-feature_request].
  This way we can discuss whether the feature lies on the roadmap for the project, and we can also discuss design choices.
  The goal is to prevent wasted effort on your side!
- If you want to fix a bug, please open a pull request!
  Link to the issue that describes the bug if such an issue exists.
- If there is a feature request issue on which I said that I'm happy to take contributions, and you want to try implementing that feature, then please do so!
  Please open a [draft pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request#creating-the-pull-request) already _while_ you're still busy implementing, so that others can see that you are working on it.


[templ-bug_report]: https://github.com/haskell/play-haskell/issues/new?template=bug_report.md
[templ-feature_request]: https://github.com/haskell/play-haskell/issues/new?template=feature_request.md
