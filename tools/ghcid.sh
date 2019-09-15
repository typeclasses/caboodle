#!/usr/bin/env bash

nix-shell ./tools/shell.nix --run 'ghcid caboodle.hs tutorial.hs generators.hs --test=Caboodle.Tutorial.testTutorial'
