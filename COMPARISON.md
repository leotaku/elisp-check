# Comparison to other CI solutions 🛒

***Disclaimer***

Even though I try to be as objective as possible, I am obviously biased in favor of my own work.
If you feel I have misrepresented your, or any, project in this comparison, please open an issue or pull request.

I have deliberately excluded some lesser-known and seemingly unmaintained projects from this comparison.
If you think some project deserves a spot, please open a issue or pull request.

## Summary

Here is a summary of reasons you may, or may not, want to use elisp-check.

### Advantages of elisp-check

* Very little setup required, only a single YAML file
* Integrates well with GitHub pull requests
* Large range of supported Emacs versions (24.1 and up)
* All checks can be run locally using a reasonably intuitive API (`elisp-check-run`)
* No dependencies outside of a supported Emacs versions

### Disadvantages of elisp-check

* Currently only GitHub Actions is supported as a CI provider
* Some more obscure linters and test frameworks are unsupported
* Does not (and likely will never) support Emacs versions lower than 24

If you are interested in support for an alternative CI provider, please open an issue.
If possible, also include links to its documentation, especially relating to API support for code annotations.

If you are interested in support for an alternative linter or test frameworks, please open an issue.
If possible, also include links to its documentation.

## Comparison to [melpazoid](https://github.com/riscy/melpazoid)

### Advantages of melpazoid

* Uses MELPA-compatible recipes to setup packages
* Implements some custom checks not supported by elisp-check
* Implements a license checker
* Can be run continuously to check many recipes

Some of these features might especially be interesting to people who review MELPA pull requests, as melpazoid has specifically been built to support their work.

### Disadvantages of melpazoid

* Does not seem to support some new versions of Emacs (builds crash)
* Reports can be very hard to read and understand
* No integration with any CI service (manual setup is needed)
* No code annotations
* Depends on Python 3.x

## Comparison to [makem.sh](https://github.com/alphapapa/makem.sh)

The makem.sh project also includes a [comparison section](https://github.com/alphapapa/makem.sh#comparisons) in its README.

### Advantages of makem.sh

* Supports some additional checks not supported by elisp-check
* Included Makefile makes it easy to run tests locally

### Disadvantages of makem.sh

* Requires adding a >1000 line shell script and Makefile to your project
* No integration with any CI service (manual setup is needed)
* No code annotations
* Depends on Bash and (optionally) Make

## Comparison to [Cask](https://github.com/cask/cask)

### Advantages of Cask

* Well known in the Emacs community
* Offers many additional features apart from CI

### Disadvantages of Cask

* Requires quite a bit of configuration by the package author
* Does not natively support any linters, so manual configuration is required
* No integration with any CI service (manual setup is needed)
* No code annotations
* Depends on Python 3.x

## Comparison to a manual setup

### Advantages of a manual setup

* Allows configuring your CI with the full power of a turing-complete programming language

### Disadvantages of a manual setup

* Requires huge amount of code and configuration by the package author

---

The structure of this comparison section was heavily inspired by [Radon Rosborough's](https://github.com/raxod502) work.

If possible, please support him and his efforts.
