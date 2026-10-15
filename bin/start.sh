#!/bin/sh
# Start the console: prefer the built executable, fall back to source.
set -e
DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
if [ -x "$DIR/bin/autolith-webui" ]; then
  exec "$DIR/bin/autolith-webui"
fi
exec sbcl --noinform --non-interactive \
  --eval "(require :asdf)" \
  --eval "(let ((setup (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))) (when (probe-file setup) (load setup)))" \
  --eval "(asdf:load-system :autolith-webui)" \
  --eval "(autolith-webui:start)" \
  --eval "(loop (sleep 60))"
