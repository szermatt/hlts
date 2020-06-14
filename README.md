# hlts  [![test](https://github.com/szermatt/hlts/workflows/test/badge.svg)](https://github.com/szermatt/hlts/actions)

This package highlights the symbol at point in the current buffer if
it appears more than once on the screen. Symbols are highlighted with
the face `hlts-face` after `hlts-idle-timeout` seconds of inactivity.

It relies on font-lock to avoid highlighting symbols in keywords,
strings or documentation. This is controlled by
`hlts-disable-for-faces`.

The variables above are customizable, try `M-x customize-group hlts`.

The approach taken by this package to highlight symbols avoids
slowdowns when displaying very large buffers, as only the visible area
of the buffer matters.

## Installation

`hlts` requires Emacs 26.1.

Add the file `hlts.el` to your load path, then call:
```elisp
(require 'hlts)
(add-hook 'prog-mode-hook 'hlts-mode)
```

The above enables symbol highlighting on all buffers showing code.

Here's a more complete example that uses
[use-package](https://github.com/jwiegley/use-package) and
[straight](https://github.com/raxod502/straight.el):

```elisp
(use-package hlts
  :straight (:type git :repo "https://github.com/szermatt/hlts")
  :defer t
  :hook ((prog-mode . hlts-mode)))
```

## Testing

Install [Cask](https://github.com/cask/cask) and run tests with:

```sh
cask exec ert-runner
```

## License

This project is licensed under the GPLv2 - see the [license](license)
file for details
