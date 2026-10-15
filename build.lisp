;;;; build.lisp --- build bin/autolith-webui (run from the repository root).

(require :asdf)
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup)
    (load setup)))
(asdf:load-system :autolith-webui)
(awui:warm-static-cache)
(awui:dump-executable)
