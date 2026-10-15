;;;; package.lisp

(defpackage #:autolith-webui
  (:nicknames #:awui)
  (:use #:cl)
  (:export
   ;; lifecycle
   #:start #:stop #:main #:executable-main #:dump-executable #:self-test
   #:warm-static-cache #:*server*
   ;; configuration
   #:*host* #:*port* #:*token* #:*autolith-binary* #:*maximum-upload-bytes*
   #:data-directory #:uploads-directory #:jobs-directory
   ;; repl sandbox
   #:repl-eval #:repl-reset #:repl-history))
