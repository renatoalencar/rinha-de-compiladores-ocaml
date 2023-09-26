#!/bin/bash

export PATH=$HOME/.cargo/bin/:$PATH
export LLC='llc-16'
export LLC_FLAGS=-O3
export LD=musl-gcc
export LD_FLAGS=-L.
export RINHACC=./_build/install/default/bin/rinha
#export RINHACC='dune exec ./bin/main.exe'

# time rinha $1 > /tmp/tmp.json
# time $RINHACC /tmp/tmp.json a.out
# time ./a.out

rinha $1 | $RINHACC - a.out && ./a.out