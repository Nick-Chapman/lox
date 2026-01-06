#!/bin/bash

# Drop the column number in error messages.
# to avoid diffs such as:
#
# -[line 2] Error at '=': Invalid assignment target.
# +[line 2.6] Error at '=': Invalid assignment target.

sed 's|\([line [0-9][0-9]*\)\.[0-9][0-9]*]|\1]|'
