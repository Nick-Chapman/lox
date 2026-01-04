#!/bin/bash

while read abs; do

d=$(basename $(dirname $abs))
x=$d-$(basename $abs .lox)

cat <<EOF

$x.lox : $abs : ln $abs $x.lox

$x.expect : $x.lox
   ~/other/craftinginterpreters/jlox $x.lox > $x.expect 2>&1 || true

$x.actual : $x.lox src/lox.exe
  ./lox.exe $x.lox > $x.actual 2>&1 || true

*test: $x.expect $x.actual
  git diff --color $x.expect $x.actual

EOF

done
