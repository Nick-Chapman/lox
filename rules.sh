#!/bin/bash

while read abs; do

d=$(basename $(dirname $abs))
x=$d-$(basename $abs .lox)

cat <<EOF

$x.lox : $abs : ln $abs $x.lox

$x.expect : $x.lox
  ~/other/craftinginterpreters/clox $x.lox 2>&1 | cat > $x.expect  || true

$x.actual : $x.lox src/lox.exe
  ./lox.exe $x.lox 2>&1 | cat > $x.actual || true

*test-$x: $x.expect $x.actual
  git diff --color $x.expect $x.actual

EOF

done
