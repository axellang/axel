set -eu
set -o pipefail

echo "[WIP] Transpile *.axel files..."
# If we run this in the context of the current project, Cabal will try to globally symlink the local package (which is not what we want).
cd ~
cabal new-install axel --overwrite-policy=always
cd -
axel-exe project
echo "[WIP] *.axel files transpiled!\n"

stack build --ghc-options "-g"
