#!/bin/sh

mkdir -p ./docs
rm -rf ./docs/*
cabal build --enable-documentation --haddock-hyperlink-source
cp -r dist-newstyle/build/x86_64-linux/ghc-8.10.7/refined-simple-0.1.0.0/doc/html/refined-simple/* ./docs
