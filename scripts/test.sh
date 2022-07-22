set -eu
set -o pipefail

# Sometimes, when macro expansion tests are run, `ghc` will stay alive
# and consume ridiculous amounts of memory and CPU.
# TODO Only kill `ghc`s spawned by this process.
trap "killall axel-test ghc" EXIT

cabal test --test-options="--hide-successes --num-threads=1" --test-show-details=direct "$@"
