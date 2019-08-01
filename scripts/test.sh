set -eu
set -o pipefail

# Sometimes, when macro expansion tests are run, `ghc` will stay alive
# and consume ridiculous amounts of memory and CPU.
trap "killall axel-test ghc" EXIT

stack test --fast --ghc-options "-g" --test-arguments "--hide-successes" "$@"
