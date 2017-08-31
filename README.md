# Lihsp [WIP]

Haskell + Lisp + JVM = Profit!

Three great things for a language to have are:

  1. Haskell semantics, like Haskell
  2. Lisp syntax, like Clojure
  3. A large ecosystem such as the JVM or the wide world of JavaScript packages

Each two of these features already exists in a different language:

  - 1 and 2: Hackett
  - 1 and 3: Eta
  - 2 and 3: Clojure

But, there's nothing (that I'm aware of) for all three.

Hence, meet Lihsp! It's a Lisp-like language (with macros and all) that transpiles to Eta, fulfilling all the requirements listed above. :tada:

## Future Plans
  - Finish the JVM backend
  - Add a PureScript backend as well (or maybe first, instead of targeting the JVM initially) to alternatively take advantage of the JavaScript ecosystem.

## Style

Use `hindent` to format code and `hlint` to catch errors.

## Structure

The closest thing the app has to an entry point right now is `transpileFile` inside `src/Lihsp/Tranpsile.hs`.

## Running

Run `scripts/build.sh` to build the project.
