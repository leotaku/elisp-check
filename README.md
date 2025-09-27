# A GitHub Action for Emacs Lisp CI 📜

[![GitHub License](https://img.shields.io/github/license/leotaku/elisp-check?color=blueviolet&logo=spdx&logoColor=white&style=flat-square)](https://spdx.org/licenses/GPL-3.0-or-later.html)
[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/leotaku/elisp-check/test.yml?branch=master&logo&style=flat-square)](https://github.com/leotaku/elisp-check/actions)
[![GitHub Release](https://img.shields.io/github/v/release/leotaku/elisp-check?include_prereleases&sort=semver&style=flat-square)](https://github.com/leotaku/elisp-check/releases)

Provides a zero-config CI solution for Emacs Lisp packages.

Most Emacs packages currently use either no or an ad-hoc script based CI setup.
This GitHub Action aims to change that.

Adding this Action to your Emacs package repository immediately provides you with a default suite of code style checks that are required to get your package accepted into MELPA.
Using only one additional line of YAML, you can also run your ERT tests on GitHub Actions.

See the [Actions tab](https://github.com/leotaku/elisp-check-action/actions) for runs of this Action! 🚀

See the [COMPARISON.md](/COMPARISON.md) file for a more thorough comparison with other solutions for Emacs Lisp CI. 🛒

### Supported Checks

* [melpa](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org) :: Most checks required for MELPA <sup>(load-file, byte-compile, checkdoc, package-lint)</sup>
* [load-file](https://www.gnu.org/software/emacs/manual/html_node/eintr/Loading-Files.html) :: Load files into Emacs
* [byte-compile](https://www.gnu.org/software/emacs/manual/html_node/elisp/Byte-Compilation.html) :: Byte-compile files
* [checkdoc](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html#Documentation-Tips) :: Check documentation style
* [package-lint](https://github.com/purcell/package-lint) :: Check coding practices
* [ert](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html) :: Run ERT tests

### Features

* Require zero configuration for great results
* Support accepted Emacs Lisp coding standards and test frameworks
* Expose helpful code annotations
* Support running checks on your local machine (`elisp-check-run`)
* Support older Emacs versions (24.4 and up, 24.1 and up without package-lint)

### Non-Features

* Support esoteric practices not accepted by MELPA
* Define or use opinionated style checks

## Usage

### Linting

``` yaml
uses: leotaku/elisp-check@master
with:
  file: main-file.el
```

### Tests

``` yaml
uses: leotaku/elisp-check@master
with:
  check: ert
  file: test-file.el
```

Note that this Action does not install a suitable `emacs` executable by itself.
For this purpose, I recommend using Steve Purcell's excellent [setup-emacs](https://github.com/purcell/setup-emacs/blob/master/README.md) GitHub Action.

For an example of real-life usage, see the [Actions config for my `theist-mode` package](https://github.com/leotaku/theist-mode/blob/master/.github/workflows/check.yml).

## Advanced usage

``` yaml
uses: leotaku/elisp-check@master
with:
  check: melpa
  file: '[!.]*.el'
  ignore_warnings: false
  warnings_as_errors: false
```

The above yaml code block shows the default configuration values for this GitHub Action.

| Name                 | Description                         | Type                                 |
|----------------------|-------------------------------------|--------------------------------------|
| `check`              | Emacs Lisp check to execute         | [Supported check](#supported-checks) |
| `file`               | Entry file for Emacs Lisp check     | File with globbing                   |
| `ignore_warnings`    | Whether to ignore warnings          | Boolean                              |
| `warnings_as_errors` | Whether to treat warnings as errors | Boolean                              |

Users are encouraged to make use of [GitHub Actions matrix feature](https://help.github.com/en/actions/reference/workflow-syntax-for-github-actions#jobsjob_idstrategy) to run different checks, check different entry files and test compatibility with different versions of Emacs.

### Running checks locally

It is entirely possible to use your preferred Emacs package manager, or simply `load-file`, to load `elisp-check.el` into your local Emacs instance.
The function `elisp-check-run` may then be used to run any supported check locally.
Errors and warnings are emitted to the Emacs `*Messages*` buffer, so both batch and interactive usage is supported.

---

This project and its documentation were heavily inspired by and at times derive from [Steve Purcell's](https://github.com/purcell) work.

If possible, please support him and his efforts.
