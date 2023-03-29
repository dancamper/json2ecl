#!/usr/bin/env bash

#############################################
# Uses buildapp utility to create a binary
#
# https://www.xach.com/lisp/buildapp/
#############################################

set -euo pipefail

mkdir -p bin

buildapp --output bin/json2ecl \
         --eval "(let* ((home-dir (user-homedir-pathname)) (setup (or (probe-file (merge-pathnames \"quicklisp/setup.lisp\" home-dir)) (probe-file (merge-pathnames \"portacle/all/quicklisp/setup.lisp\" home-dir))))) (load setup))" \
         --eval "(require :asdf)" \
         --eval "(ql:quickload '(:adopt :com.inuoe.jzon :with-user-abort) :silent t)" \
         --asdf-path ~/quicklisp/dists \
         --asdf-path . \
         --load-system json2ecl \
         --entry json2ecl:toplevel \
         --compress-core
