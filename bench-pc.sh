#!/bin/sh

RUN="cabal run --"

$RUN examples/luigi/PC1k.core -v0
$RUN examples/luigi/PC10k.core -v0
$RUN examples/luigi/PC100k.core -v0
$RUN examples/luigi/PC1m.core -v0
$RUN examples/luigi/PC10m.core -v0

