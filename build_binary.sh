#!/usr/bin/env bash

#############################################
# Uses buildapp utility to create a binary
#
# https://www.xach.com/lisp/buildapp/
#############################################

set -euo pipefail

mkdir -p bin

buildapp --output bin/json2ecl \
         --eval "(load \"~/quicklisp/setup.lisp\")" \
         --asdf-path ~/quicklisp/dists \
         --load-system json2ecl \
         --entry json2ecl:toplevel \
         --compress-core
