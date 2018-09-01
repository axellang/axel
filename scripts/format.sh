#!/bin/zsh
for f in {app,src,test}/**/*.hs; do
  hindent "$f"
done
