;;;; repl.lisp --- persistent in-image evaluation sandbox.

(in-package #:autolith-webui)

(defparameter *repl-lock* (bt:make-lock "awui-repl"))
(defparameter *repl-package* nil)
(defparameter *repl-history* nil)
(defparameter *repl-history-limit* 100)
(defparameter *repl-eval-timeout-seconds* 20)
(defparameter +repl-eof+ (make-symbol "REPL-EOF"))

(defun ensure-repl-package ()
  "The persistent user package REPL evaluations run in."
  (or (and *repl-package* (find-package (package-name *repl-package*)))
      (setf *repl-package*
            (make-package "WEBUI-USER" :use '("COMMON-LISP" "UIOP")))))

(defun repl-reset ()
  (bt:with-lock-held (*repl-lock*)
    (when *repl-package*
      (ignore-errors (delete-package *repl-package*)))
    (setf *repl-package* nil
          *repl-history* nil)
    "WEBUI-USER package reset."))

(defun print-one (value)
  (handler-case
      (let ((*print-pretty* t)
            (*print-length* 64)
            (*print-level* 8)
            (*print-circle* t))
        (princ-to-string value))
    (error () "#<unprintable>")))

(defun repl-eval (input)
  "Evaluate INPUT in the persistent WEBUI-USER package.
*READ-EVAL* is disabled and evaluation is bounded by a timeout (SBCL).
Returns (values ok-p captured-output)."
  (bt:with-lock-held (*repl-lock*)
    (let* ((ok-p t)
           (started (get-universal-time))
           (out (make-string-output-stream)))
      (let ((*standard-output* out)
            (*error-output* out)
            (*trace-output* out)
            (*query-io* out)
            (*package* (ensure-repl-package))
            (*read-eval* nil)
            (*print-pretty* t)
            (*print-length* 64)
            (*print-level* 8)
            (*print-circle* t))
        (with-input-from-string (stream input)
          (handler-case
              (loop for form = (read stream nil +repl-eof+)
                    until (eq form +repl-eof+)
                    do (let ((values (call-with-timeout
                                      *repl-eval-timeout-seconds*
                                      (lambda ()
                                        (multiple-value-list (eval form))))))
                         (format out "~&;; => ~{~A~^ ; ~}~%"
                                 (mapcar #'print-one values))))
            (t (condition)
               (setf ok-p nil)
               (format out "~&;; ERROR: ~A [~A]~%"
                       condition (type-of condition))))))
      (let ((text (get-output-stream-string out)))
        (push (list :time started :input (or input "") :ok-p ok-p :output text)
              *repl-history*)
        (when (> (length *repl-history*) *repl-history-limit*)
          (setf *repl-history* (subseq *repl-history* 0 *repl-history-limit*)))
        (values ok-p text)))))

(defun repl-history ()
  (bt:with-lock-held (*repl-lock*)
    (copy-list *repl-history*)))
