set -eu
set -o pipefail

cabal new-install tasty-discover

./scripts/build.sh
cabal new-test
