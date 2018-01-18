# Lihsp [WIP]

Haskell + Lisp + JVM/Node/... = Profit!

Three great things for a language to have are:

  1. Haskell semantics, like Haskell
  2. Lisp syntax, like Clojure
  3. A large ecosystem such as the JVM's or Node's

Each two of these features already exists in a different language:

  - 1 and 2: Hackett
  - 1 and 3: Eta
  - 2 and 3: Clojure (JVM)/ClojureScript (Node)/...

But, there's nothing (that I'm aware of) for all three.

Hence, meet Lihsp! It's a Lisp-like language (with macros and all) that transpiles to Eta, fulfilling all the requirements listed above. :tada:

## TODO

  - [ ] Quasiquoting
  - [ ] Compile-time module system
  - [ ] Test suite
  - [ ] CLI
    - [ ] Error message reporting
  - [ ] Backends
    - [ ] Java (Eta)
    - [ ] JavaScript (PureScript)
  - [ ] Cleanup
    - [ ] Use qualified imports exclusively (?)
    - [ ] Standardize use of `($)`
    - [ ] Rename `AST` to `Target` (?) and `Parse` to `Lisp` (?)
    - [ ] Merge `AST.Literal` into `AST.Expression`
    - [ ] Rename `Normalize` to something more appropriate

## Style

Use `hindent` to format code and `hlint` to catch errors.

## Structure

The closest thing the app has to an entry point right now is `transpileFile` inside `src/Lihsp/Tranpsile.hs`.

## Running

Run `scripts/build.sh` to build the project, and `stack exec lihsp-exe` to run it.
