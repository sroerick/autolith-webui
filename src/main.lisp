;;;; main.lisp --- lifecycle, entry points, and offline self-test.

(in-package #:autolith-webui)

(defun main ()
  "Blocking entry point: start the console and park the thread."
  #+sbcl
  (ignore-errors
   (let ((enable (or (find-symbol "ENABLE-INTERRUPT" :sb-sys)
                     (find-symbol "ENABLE-INTERRUPT" :sb-unix))))
     (when enable
       (funcall enable sb-unix:sigint
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (stop)
                    (sb-ext:exit :code 0))))))
  (start)
  (log-line "ready - press Ctrl-C to stop")
  (loop (sleep 30)))

(defun executable-main ()
  "Toplevel for the dumped executable."
  (main))

(defun dump-executable (&optional path)
  "Save the current image as a standalone executable that starts the console."
  #+sbcl
  (sb-ext:save-lisp-and-die
   (namestring (or path
                   (uiop:merge-pathnames*
                    "bin/autolith-webui"
                    (uiop:pathname-directory-pathname
                     (asdf:system-source-directory :autolith-webui)))))
   :executable t
   :compression t
   :toplevel #'executable-main)
  #-sbcl
  (error "Dumping an executable requires SBCL."))

;;; --- offline self-test ---

(defun self-test ()
  "Run offline assertions (no network, no autolith CLI); return T on success."
  (format t "~&autolith-webui self-test~%")
  (assert (equal (esc "<a href=\"x\">&'")
                 "&lt;a href=&quot;x&quot;&gt;&amp;&#39;"))
  (assert (equal (render (list :div :class "x" "hi" (list :br)))
                 "<div class=\"x\">hi<br></div>"))
  (assert (equal (percent-decode "a+b%20c%2F") "a b c/"))
  (assert (equal (percent-decode "%E2%9C%93") "✓"))
  (let ((params (parse-urlencoded "a=1&b=two+words&flag")))
    (assert (equal (cdr (assoc "a" params :test #'string=)) "1"))
    (assert (equal (cdr (assoc "b" params :test #'string=)) "two words"))
    (assert (equal (cdr (assoc "flag" params :test #'string=)) "")))
  (let* ((boundary "XyZ123")
         (crlf (coerce (list #\Return #\Newline) 'string))
         (payload (coerce (list (code-char 0) (code-char 1) (code-char 200)
                                (code-char 255) #\Return #\Newline)
                          'string))
         (body-text (format nil "--~A~AContent-Disposition: form-data; name=\"note\"~A~A---hello---~A--~A~AContent-Disposition: form-data; name=\"file\"; filename=\"data.bin\"~AContent-Type: application/octet-stream~A~A~A~A--~A--~A"
                             boundary crlf crlf crlf crlf boundary crlf crlf crlf crlf
                             payload crlf boundary crlf)))
    (let ((request (make-request :body (latin1-octets body-text))))
      (parse-multipart request (format nil "multipart/form-data; boundary=~A" boundary))
      (assert (equal (cdr (assoc "note" (req-params request) :test #'string=))
                     "---hello---"))
      (let ((upload (first (req-files request))))
        (assert (equal (upload-filename upload) "data.bin"))
        (assert (equalp (upload-data upload)
                        (make-array 6 :element-type '(unsigned-byte 8)
                                      :initial-contents '(0 1 200 255 13 10)))))))
  (assert (equal (read-state-string "#A((39) BASE-CHAR . \"abc\")") "abc"))
  (assert (equalp (read-state-string "(:A 1 :B #A((39) BASE-CHAR . \"/home/x/\"))")
                  (list :a 1 :b "/home/x/")))
  (let* ((path (uiop:merge-pathnames* "selftest-envelope.sexp" (jobs-directory)))
         (envelope (list :autolith-job
                         :version 1
                         :id "t-1"
                         :role "task"
                         :prompt "line1
line2 \"quoted\""
                         :input (list :task "write" :attachments (list) :source :autolith-webui)
                         :output-contract (list :type :string)
                         :timeout-seconds 60)))
    (write-envelope path envelope)
    (assert (equal (first (read-state-forms path)) envelope))
    (ignore-errors (delete-file path)))
  (let ((rows (parse-sessions "┌──────┐
│SESSION │STATE │
├────────┤
│X-54EfaE│active│2026-10-15T00:15Z│Hi│/home/│
└────────┘")))
    (assert (= 1 (length rows)))
    (assert (equal (session-row-id (first rows)) "X-54EfaE")))
  (assert (constant-time-equal "secret" "secret"))
  (assert (not (constant-time-equal "secret" "Secret")))
  (assert (not (constant-time-equal "secret" "secretX")))
  (format t "~&all self-tests passed.~%")
  t)
