if [ "$1" == "test" ]; then
    ghcid --command "stack ghci axel:lib axel:test:axel-test --ghc-options='-Wno-missing-import-lists -O0'" --test "main"
elif [ -z "$1" ]; then
    ghcid --command "stack ghci --ghc-options='-O0' axel"
fi
