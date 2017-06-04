# purify

Reproducible builds for PureScript, inspired by Haskell's `stack` tool. This is a fork of [chrisdone/purify](https://github.com/chrisdone/purify).

See [purify-template](https://github.com/chrisdone/purify-template)
for a template repo that you can clone and build in 5 minutes.

See [purify-sets](https://github.com/chrisdone/purify-sets)
for sets of packages that are known to build together.

## Commands

* `purify build` - Build the current project.
* `purify ide` - Launch the PureScript IDE server (for Emacs integration).

See examples of these commands at [purify-template](https://github.com/chrisdone/purify-template).

## Changes implemented

* Repositories are fetched in parallel using [pooled-io](http://hackage.haskell.org/package/pooled-io). Even building the full [purify-sets](https://github.com/chrisdone/purify-sets) collection doesn't take very long.

* Preliminary support for package names leaving off the initial `purescript-` prefix using the `--implicit-prefix` switch for `purify add-deps`: for instance, you can now do

```
$ purify add-deps --implicit-prefix eff
```

* Pretty output using the [prettyprint](https://github.com/quchen/prettyprint) library.

* A `--verbose` switch for `git` output.

