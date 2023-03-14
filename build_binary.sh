#!/usr/bin/env bash

set -euo pipefail

mkdir -p bin

buildapp --output bin/json2ecl \
         --eval "(load \"~/quicklisp/setup.lisp\")" \
         --asdf-path ~/quicklisp/dists \
         --load-system json2ecl \
         --entry json2ecl:toplevel \
         --compress-core
