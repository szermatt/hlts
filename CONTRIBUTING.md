## Reporting issues

Please report bugs to the [issue tracker][issues].

If you're having issue with completion, please include enough 
information to reproduce the issue.

## Suggesting features

Add feature suggestions to the [issue tracker][issues].

## Asking questions

Open an issue in the [issue tracker][issues] with your question.

## Documentation contributions

Contribution to the documentation, either the README.md or code
comments are very welcome. Please open a [pull request][pulls] with
your proposed modifications.

## Code contributions

To contribute code to the project, open a [pull request][pulls].

Before you do that, please make sure the any new features is covered
by tests and that the tests pass. 

To run the tests, install and setup
[cask], then run them with 
```bash
make test
```

Tests can also be run from inside Emacs,
using `M-x ert-run-tests-interactively` but when you do so, be aware
that there might be unexpected interaction with your Emacs
configurations; Tests passing when run from Cask is what matters.

After you've sent your pull request, please check the result of
[GitHub actions][actions] running tests on your pull request. GitHub
actions run the same tests on multiple versions of Emacs so is likely
to highlight version-specific issues you might not have noticed when
running tests on your machine.

For larger features, it's a good idea to first open an 
[issue][issues] that describes the feature and mention that you're
thinking about working on it. This gives an opportunity to discuss the
new feature and its possible implementations.

[cask]: https://github.com/szermatt/hlts/issues
[issues]: https://github.com/szermatt/hlts/issues
[actions]: https://github.com/szermatt/hlts/actions
[pulls]: https://github.com/szermatt/hlts/pulls
