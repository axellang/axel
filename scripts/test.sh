set -eu
set -o pipefail

# Sometimes, when macro expansion tests are run, `ghc` will stay alive
# and consume ridiculous amounts of memory and CPU.
trap "killall axel-test ghc" EXIT

cabal test --ghc-options="-g" --test-options="--hide-successes --num-threads=1" --test-show-details=direct "$@"
