**This is a work in progress and not stable or feature-complete.**

If you’re like most people, the first thought you have when you
wake up each morning is that Haskell does not have enough alternative
preludes.

This _experimental_ tool offers that and more, providing an alternative
`base` that is “batteries-included”, so you don’t need to add a
long list of dependencies to access fundamental, established packages.


##  Overview

This tool is intended for application development.  It should not
be used for libraries, and it’s unclear if it’s suitable for
command-line tools.

When `basemaker` is executed, it will generate a small (about 4K)
tarball, usually `mybase.tar.gz`, and provide a SHA256 checksum.

In your `build-depends` in your Cabal file, just add `mybase` and
remove `base`.  Then make sure `mybase` is installed or available.
For example, in Stack add to your `extra-deps` in `stack.yaml`:

```yaml
extra-deps:
- archive: /path/to/mybase.tar.gz
  sha256: …
```

The `sha256` line (using the checksum output by `basemaker`) is
usually desired so Stack can detect when you update the tarball.

That is all!  The new prelude will be used in all your Haskell
modules.  Any packages included in `mybase` can also be removed from
`build-depends`.


##  Criteria

The prelude does not try to be “ahead of the curve”, e.g.,
by abolishing `String` or partial functions or re-slicing `Num`
or going all-in on effects.  Nor does the expanded `base` try to
be innovative.  The goal is to make it more convenient to work with
the Haskell ecosystem as it currently stands.

Criteria for inclusion:

1.  Fundamental.  `ByteString`, `DeepSeq`, `Vector`, and so on are
today almost part of Haskell.

2.  Widespread.  Even if you don’t use some of these packages,
you’re likely to transitively depend on them anyway.

3.  Accepted.  There is general consensus that these packages represent
the right solutions or abstractions.

4.  Conceptually important.  Packages such as `comonad` may not be
as widely used, but fill a role in functional programming theory.

5.  Lightweight.  Many packages aren’t as widely used, but are
small dependencies.

No package is going to meet all these, but one can score them and
decide which make the cut.

Similarly, a prelude should contain a large set of basic functions
such as `fromMaybe`, `ord`, `traverse`, `when`, and so on, that are
more or less basic Haskell.


##  Rebase and rerebase

The closest existing project to this conception is
[rebase](https://github.com/nikita-volkov/rebase), which says, “The
policy behind the package is only to reexport the non-ambiguous and
non-controversial APIs, which the community has obviously settled
on”, and hosts a well-curated list of packages and prelude exports.
So, for now I’m using it as a starting point.

A derived project, [rerebase](https://github.com/nikita-volkov/rebase),
re-exports modules from these packages with their original names.

However, since the list of re-exports is fixed, while the modules
provided in packages are always changing, some modules may be missing,
with tricky workarounds needed to access them.  This problem prevented
me from being able to use this approach.


##  How to use

`basemaker --help` lists options, which are fairly straightforward.
Decide what packages to load and re-export, and decide what versions
of them will be used.  For the latter, you can fix the versions, or
get the latest off Hackage, or use a Stack snapshot.  It will include
a `Prelude.hs` file in the tarball, which you can write yourself.
The original `Prelude` is available as `Prelude.Base`.

There is an option to use the package list and prelude from `rebase`.


