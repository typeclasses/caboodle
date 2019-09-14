#!/usr/bin/env bash

nix-shell ./tools/shell.nix --run 'haddock caboodle.hs --odir docs --html --pretty-html'
