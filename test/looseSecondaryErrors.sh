#!/bin/bash

awk '{ print } /Error/ { exit }'
