#!/usr/bin/env bash

#############################################
# Uses buildapp utility to create a binary
#
# https://www.xach.com/lisp/buildapp/
#############################################

set -euo pipefail

mkdir -p bin

buildapp --output bin/json2ecl \
         --eval "(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))" \
         --eval "(require :asdf)" \
         --eval "(ql:quickload '(:adopt :com.inuoe.jzon :with-user-abort) :silent t)" \
         --asdf-path ~/quicklisp/dists \
         --asdf-path . \
         --load-system json2ecl \
         --entry json2ecl:toplevel \
         --compress-core
