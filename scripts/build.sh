set -eu
set -o pipefail

echo "Copying macro header..."
cat resources/macros/Imports.hs > resources/macros/Header.hs
cat src/Lihsp/Parse/AST.hs >> resources/macros/Header.hs
echo "Macro header copied!"

stack build
