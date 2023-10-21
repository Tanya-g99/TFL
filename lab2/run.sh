#!/bin/sh
cabal install --lib random
runghc Test.hs
