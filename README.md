# Minesweeper

## Running Project
Run the following command to run the program  
```shell
stack build && stack exec minesweeper
```

To debug do something like this
```shell
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && $(pwd)/.stack-work/install/aarch64-osx/ebb99203899ef1333b197585e039ff8842186159561103eeb0196841c3fce1c7/8.10.7/bin/minesweeper +RTS -xc
```