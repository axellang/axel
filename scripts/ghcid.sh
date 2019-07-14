if [ "$1" == "test" ]; then
    ghcid --command "stack ghci axel:lib axel:test:axel-test" --test "main"
elif [ -z "$1" ]; then
    ghcid --command "stack ghci axel"
fi
