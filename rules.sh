#!/bin/bash

while read abs; do

d=$(basename $(dirname $abs))
x=$d-$(basename $abs .lox)

cat <<EOF

$x.lox : $abs : ln $abs $x.lox

$x.expect : $x.lox
   ~/other/craftinginterpreters/clox $x.lox > $x.expect 2>&1 || true

$x.actual : $x.lox src/lox.exe removeCol.sh
  ./lox.exe $x.lox > $x.actual 2>&1 || true

*test-$x: $x.expect $x.actual removeCol.sh
  cat $x.actual | ./removeCol.sh | git diff --color $x.expect -

EOF

done
