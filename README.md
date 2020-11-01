# About

This Project implements an Emacs library for parsing
[JSON5](https://json5.org/), as commonly used for VSCode's configuration files.

# Usage

The main entry points are `json5-parse-file`, `json5-parse-string` and
`json5-parse-buffer`. There is also a function to convert a JSON5 string to
normal JSON, `json5-to-json`.

# Limitations

To have to do less work, I decided against implementing a real JSON5 parser.
Instead, this library works by first taking a JSON5 string, converting it to
normal JSON using a set of transformations based on regular expressions (remove
trailing commas, comments, wrap keywords in quotes, …) and then parsing the
result using a normal JSON parser (the builtin json.el).

This causes a problem though: Infinity and NaN cannot be parsed, even though
they represent valid JSON5. Aside from that, most of JSON5 is handled. (Infinity
and NaN are rare in configuration files, though).

# Test suite

There is an `ert-runner` based test-suite that leverages the [official JSON5
test suite](https://github.com/json5/json5-tests). Currently, not all tests pass
for the following reasons:

- Decimals ending in 0 get converted to integers by Node, but not by this
  library, so there are many failures like `(should (equal 5.0 5))`. Doing that
  using `truncate` caused other failures due to loss of precision.
- Infinity and NaN aren't handled by json.el
- Duplicate entries aren't removed

To run it, the `json5` npm package has to be installed and in the PATH. Then, it
can be run using `cask exec ert-runner` as usual (after `cask install`).
