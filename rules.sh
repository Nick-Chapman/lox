#!/bin/bash

while read abs; do

d=$(basename $(dirname $abs))
x=$d-$(basename $abs .lox)

cat <<EOF

$x.lox : $abs : ln $abs $x.lox

$x.expect : $x.lox looseErrorDetails.sh
  ~/other/craftinginterpreters/clox $x.lox 2>&1 | ./looseErrorDetails.sh > $x.expect  || true

$x.actual : $x.lox src/lox.exe removeCol.sh looseErrorDetails.sh
  ./lox.exe $x.lox 2>&1 | ./removeCol.sh | ./looseErrorDetails.sh > $x.actual  || true

*test-$x: $x.expect $x.actual
  git diff --color $x.expect $x.actual

EOF

done
