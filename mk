#!/bin/sh

stack build && stack runhaskell --package clash-shake -- Shakefile.hs $@
