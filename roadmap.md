# Roadmap

**CURRENT PROJECT** PureScript Backend

Potential next (immediate) avenues of development:

## Macro Ergonomics

Macros are currently a bit clunky to write. For example, there's no syntax for `syntax-quote`, only regular `quote`.
Racket's work in this area may be useful to take inspiration from.

## Hygenic Macros

The macro system is currently unhygenic. I don't know what it would take to make it hygenic, so this would mean learning about other Lisps' approaches (e.g. Scheme's and Racket's), as well as implementing a new, hygenic system.

## PureScript Backend

Implement a PureScript backend. An appreciable amount of the code is Haskell-specific, even though in theory it can be generalized to other Haskell-like languages (a la PureScript or Eta). As such, implement a non-Haskell backend would mean learning how to implement "dependency injection" in a medium-sized Haskell project.
(Also, PureScript doesn't have a metaprogramming system last I checked, so this would also mean giving new functionality in the form of a PureScript dialect.)

## Tests

This might be an auxiliary part of the other next steps, but it's important nonetheless. The test suite is currently very small, and I'm not personally sure how best to expand it. So, creating a comprehensive test suite would involve learning about unit testing, etc. in a medium-sized Haskell project. (Hopefully, property tests could be emphasized, since they're such a distinct and powerful part of the Haskell ecosystem.)

## Haskell -> Axel Converter

There is currently a Haskell to Axel converter that uses `haskell-src-exts` under the hood, but it doesn't work on many files with uses of infix operators. It needs to be migrated to `ghc-lib-parser`, a package that exposes the GHC API for external use. This will likely be much more tedious than educational, since the basic idea is the same as what is currently implemented.

## Performance

Transpilation is currently pretty slow, especially because of the many round-trips necessary when making roundtrips from ghcid. I should investigate ways to speed up this process, if possible. (It's worth noting that, as far as I can tell, profiling is particularly difficult in this case due to ghcid's multithreadedness.)

## Build Output

There are spurious warnings (e.g. Axel prelude macros being unnecessarily imported) that the end-user shouldn't be bothered by.

## Bootstrap

Rewrite the transpiler in Axel itself (this has been started but is not very far along). This would provide a way to debug Axel in a real-world setting.

## Haskell Backend

The Haskell backend is not fully complete (e.g. guards, at-patterns, `default` declarations, list comprehensions, etc.). While finishing up the Haskell backend, it would be nice to lift restrictions on syntax on the Axel side (and let the backend handle it, instead of 1) duplicating the effort, with both the redundancy and error-proneness that would mean, and 2) making normalization much harder in general).

## "Fancy" Macros

Implement Haskell extensions (and otherwise) as macros, such as idiom brackets, the effectful library from Scala, etc.

## Editor Integrations

Develop e.g. a custom editor mode for Emacs. (Integrations for e.g. Scheme are mostly compatible with Axel, but not 100%.)

## REPL

Allow the use of GHCi with Axel syntax.
