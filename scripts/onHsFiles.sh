find app src test -iname "*.hs" ! -path "app/Main.hs" ! -path "src/Axel/Parse/Args.hs" ! -path "src/Axel.hs" ! -path "src/Axel/Haskell/Macros.hs" ! -name "golden_*.hs" -print0 | xargs -0 "$@"
