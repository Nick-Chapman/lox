#!/bin/bash

awk '{ print } /Error/ { exit }' | sed 's|Error.*|Error details suppressed|'
