#!/bin/sh

# Adapted from
# https://github.com/cicakhq/potato/blob/master/tools/build_binary.sh
sbcl --non-interactive \
     --disable-debugger \
     --eval '(require :asdf)' \
     --eval '(ql:quickload :galax)' \
     --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die "galax" :toplevel #'"'"'galax:main :executable t))'
