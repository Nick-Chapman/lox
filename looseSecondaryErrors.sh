#!/bin/bash


#cat
awk '{ print } /Error/ { exit }'
