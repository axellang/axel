find app src test -type f | grep -e '^.*\.hs$' | grep -v 'src/Axel/Parse/Args.hs' | grep -v 'src/Axel.hs' | grep -v 'src/Axel/Haskell/Macros.hs' | xargs -L 1 hindent
