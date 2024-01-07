# Comparison to other CI solutions 🛒

***Disclaimer***

Even though I try to be as objective as possible, I am obviously biased in favor of my own work.
If you feel I have misrepresented your, or any, project in this comparison, please open an issue or pull request.

I have deliberately excluded some lesser-known and seemingly unmaintained projects from this comparison.
If you think some project deserves a spot, please open a issue or pull request.

## Summary

Here is a summary of reasons you may, or may not, want to use `elisp-check`.

### Advantages of `elisp-check`

* Very little setup required, only a single YAML file
* Integration with GitHub Actions, which provides free CI for open source projects
* Code annotations, which also integrate with GitHub pull requests
* Large range of supported Emacs versions (24.1 and up)
* All checks can be run locally using a reasonably intuitive API (`elisp-check-run`)
* No dependencies outside of a supported Emacs versions

### Disadvantages of `elisp-check`

* Currently only GitHub Actions is supported as a CI service
* Some lesser-used linters and test frameworks are currently unsupported
  * [check-declare](https://www.gnu.org/software/emacs/manual/html_node/elisp/Declaring-Functions.html) :: Correct `declare-function` statements
  * [indent-lint](https://github.com/conao3/indent-lint.el) :: Indentation best practices
  * [relint](https://github.com/mattiase/relint) :: REGEXP best practices
  * [Elsa](https://github.com/emacs-elsa/Elsa) :: Emacs Lisp static analysis
  * [buttercup](https://github.com/jorgenschaefer/emacs-buttercup) :: Alternative test framework
* Does not (and likely will never) support Emacs versions lower than 24

If you are interested in support for an alternative CI service, please open an issue.
If possible, also include links to documentation, especially relating to API support for code annotations.

If you are interested in support for an alternative linter or test framework, please open an issue.
If possible, also include links to documentation.

## Comparison to [`melpazoid`](https://github.com/riscy/melpazoid)

### Advantages of `melpazoid`

* Uses MELPA-compatible recipes to setup packages
* Implements some custom checks not supported by `elisp-check`
* Implements a license checker
* Includes sample configurations for GitHub Actions and Travis CI
* Includes a Dockerfile
* Can be run continuously to check many recipes

Some of these features might especially be interesting to people who review MELPA pull requests, as melpazoid has specifically been built to support their work.

### Disadvantages of `melpazoid`

* Does not seem to support some new versions of Emacs (builds crash)
* Reports can be very hard to read and understand
* Some lesser-used linters are unsupported
* No support for any test frameworks or running tests
* No automatic integration with any CI service (manual setup is documented)
* No code annotations
* Depends on Python 3.x

## Comparison to [`makem.sh`](https://github.com/alphapapa/makem.sh)

The makem.sh project also includes a [comparison section](https://github.com/alphapapa/makem.sh#comparisons) in its README.

### Advantages of `makem.sh`

* Supports quite a few additional checks not supported by `elisp-check`
* Includes a script and Makefile for running tests locally
* Includes a sample configuration for GitHub Actions
* Terminal output is color-coded which improves readability

### Disadvantages of `makem.sh`

* No automatic integration with any CI service (manual setup is documented)
* No code annotations
* Depends on Bash (Make is optional)

## Comparison to [Cask](https://github.com/cask/cask)

### Advantages of Cask

* Well known in the Emacs community
* Offers many additional features for Emacs Lisp projects

### Disadvantages of Cask

* Requires quite a bit of configuration by the package author
* No native support for any linters or test frameworks (manual setup is needed)
* No automatic integration with any CI service (manual setup is needed)
* No code annotations
* Depends on Python 3.x

## Comparison to [Eldev](https://github.com/doublep/eldev)

### Advantages of Eldev

* Offers many additional features for Emacs Lisp projects
* Offers support for code coverage reports
* Allows extending core functionality for highly specialized setups
* No dependencies outside of a supported Emacs versions

### Disadvantages of Eldev

* Requires quite a bit of configuration by the package author
* No automatic integration with any CI service (manual setup is documented)
* No code annotations

## Comparison to a manual setup

### Advantages of a manual setup

* Allows configuring CI with the full power of a turing-complete programming language
* If simple, adds no external dependencies to your project

### Disadvantages of a manual setup

* Potentially requires huge amount of code and configuration by the package author

---

The structure of this comparison section was heavily inspired by [Radon Rosborough's](https://github.com/raxod502) work.

If possible, please support him and his efforts.
