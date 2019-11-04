#!/bin/bash

# From https://raw.githubusercontent.com/ChrisPenner/dotfiles/master/bin/haddock-up

# Adapted from script by Dimitri Sabadie <dimitri.sabadie@gmail.com>

dist=$(stack path --dist-dir --stack-yaml ./stack.yaml)
packagename=$(awk '/^name:\s*(.*)/{ print $2 }' ./*.cabal)
packageversion=$(awk '/^version:\s*(.*)/{ print $2 }' ./*.cabal)

echo -e "\033[1;36mGenerating documentation for $packagename-$packageversion\033[0m"

if ! stack haddock; then
  echo -e "\033[1;31m'stack haddock failed, are you in a stack project?\033[0m"
  exit 1
fi

echo "uploading docs to $packagename-$packageversion"
docdir=$dist/doc/html
cd "$docdir" || (echo "$docdir does not exist!"; exit 1)
doc="$packagename-$packageversion-docs"
echo -e "Compressing documentation from \033[1;34m$docdir\033[0m for \033[1;35m$packagename\033[0m-\033[1;33m$packageversion\033[1;30m"
cp -r "$packagename" "$doc"
tar -c -v -z --format=ustar -f "$doc.tar.gz" "$doc"
echo -e "\033[1;32mUploading to Hackage...\033[0m"
read -r -p "Hackage username: " username
read -r -p "Hackage password: " -s password
echo ""
curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@$doc.tar.gz" "https://$username:$password@hackage.haskell.org/package/$packagename-$packageversion/docs" > /dev/null
exit $?

