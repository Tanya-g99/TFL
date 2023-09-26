#!/bin/sh
ghc Main.hs
./Main
z3 -smt2 file.smt2
