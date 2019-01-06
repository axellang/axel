set -eu
set -o pipefail

./scripts/build.sh
stack test
