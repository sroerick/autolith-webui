#!/bin/sh
# Build bin/autolith-webui from source.
# Requires: SBCL + Quicklisp, with the autolith-webui system visible to ASDF
# (make install-links symlinks it into ~/quicklisp/local-projects).
set -e
DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$DIR"
exec sbcl --noinform --disable-debugger --load build.lisp
