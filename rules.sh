#!/bin/bash

while read abs; do

d=$(basename $(dirname $abs))
x=$d-$(basename $abs .lox)

cat <<EOF

$x.lox : $abs : ln $abs $x.lox

$x.expect : $x.lox looseSecondaryErrors.sh
  ~/other/craftinginterpreters/clox $x.lox 2>err >$x.expect; cat err | ./looseSecondaryErrors.sh >>$x.expect

$x.actual : $x.lox src/lox.exe looseSecondaryErrors.sh
  ./lox.exe $x.lox 2>err >$x.actual; cat err | ./looseSecondaryErrors.sh >>$x.actual

*test-$x: $x.expect $x.actual
  git diff --color $x.expect $x.actual

EOF

done
