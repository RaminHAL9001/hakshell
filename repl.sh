#!/bin/sh -x
exec stack repl --ghc-options -fobject-code -- hakshell
