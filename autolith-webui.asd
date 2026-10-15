;;;; autolith-webui.asd --- a native Common Lisp web console for Autolith.

(defsystem "autolith-webui"
  :description "Native Common Lisp web console for Autolith: htmx frontend, hx multipart uploads, in-image REPL, and headless run-job integration."
  :author "roerick"
  :license "MIT"
  :version "0.1.0"
  :serial t
    :depends-on ("uiop"
                 "usocket"
                 "bordeaux-threads"
                 "flexi-streams"
                 "cl-ppcre"
                 "yason")
  :components ((:file "src/package")
               (:file "src/util")
               (:file "src/config")
               (:file "src/html")
               (:file "src/http")
               (:file "src/state")
                 (:file "src/autolith")
                 (:file "src/chat")
                 (:file "src/repl")
                 (:file "src/routes")
                 (:file "src/main")))
