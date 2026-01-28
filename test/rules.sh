#!/bin/bash

while read abs; do

d=$(basename $(dirname $abs))
x=$d-$(basename $abs .lox)

cat <<EOF

$x.lox : $abs : ln $abs $x.lox

# Reference...

$x.expect : reference-lox $x.lox looseSecondaryErrors.sh
  ./reference-lox $x.lox 2>err >$x.expect; cat err | ./looseSecondaryErrors.sh >>$x.expect

$x.T : T.sh @deps-for-T $x.lox looseSecondaryErrors.sh
  ./T.sh $x.lox 2>err >$x.T; cat err | ./looseSecondaryErrors.sh >>$x.T

*test-T-$x: $x.expect $x.T
  git diff --text --color $x.expect $x.T

$x.H : H.sh @deps-for-H $x.lox looseSecondaryErrors.sh
  ./H.sh $x.lox 2>err >$x.H; cat err | ./looseSecondaryErrors.sh >>$x.H

*test-H-$x: $x.expect $x.H
  git diff --text --color $x.expect $x.H

$x.C : C.sh @deps-for-C $x.lox looseSecondaryErrors.sh
  ./C.sh $x.lox 2>err >$x.C; cat err | ./looseSecondaryErrors.sh >>$x.C

*test-C-$x: $x.expect $x.C
  git diff --text --color $x.expect $x.C

EOF

done
