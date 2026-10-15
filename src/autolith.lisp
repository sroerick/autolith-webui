;;;; autolith.lisp --- wrapping the autolith CLI and headless run-job jobs.

(in-package #:autolith-webui)

;;; --- cached CLI captures ---

(defparameter *cli-lock* (bt:make-lock "awui-cli"))
(defparameter *cli-cache* (make-hash-table :test #'equal))

(defun run-capture (arguments &key (timeout 120))
  "Run the autolith CLI with ARGUMENTS; return (values stdout stderr exit-code)."
  (handler-case
      (call-with-timeout timeout
        (lambda ()
          (uiop:run-program (cons *autolith-binary* arguments)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)))
    (timeout-condition ()
      (values "" (format nil "autolith ~{~A~^ ~} timed out after ~A s."
                         arguments timeout)
              -1))
    (error (condition)
      (values "" (princ-to-string condition) -1))))

(defun cached-capture (key seconds arguments &key (timeout 120))
  (bt:with-lock-held (*cli-lock*)
    (let ((entry (gethash key *cli-cache*)))
      (if (and entry (< (get-universal-time) (+ (cdr entry) seconds)))
          (car entry)
          (multiple-value-bind (out err code)
              (run-capture arguments :timeout timeout)
            (declare (ignore code))
            (let ((text (trim (if (blank-p out)
                                  (format nil "(stderr) ~A" err)
                                  out))))
              (setf (gethash key *cli-cache*) (cons text (get-universal-time)))
              text))))))

(defun autolith-version ()
  (cached-capture "version" 600 (list "--version") :timeout 60))

(defun autolith-models ()
  (cached-capture "models" 600 (list "models") :timeout 180))

(defun localgroup-status-text ()
  (cached-capture "localgroup" 8 (list "localgroup" "status") :timeout 180))

;;; --- detached sessions ---

(defstruct session-row id state started conversation workspace)

(defun parse-sessions (text)
  "Extract session rows from `autolith localgroup status` box output."
  (let ((rows '()))
    (dolist (line (ppcre:split "\\r?\\n" text))
      (when (find #\│ line)
        (let ((cells (mapcar #'trim (ppcre:split "│" line))))
          (when (and (> (length cells) 5)
                     (ppcre:scan "^[0-9A-Za-z]{1,2}-[0-9A-Za-z]{6,}$" (second cells)))
            (push (make-session-row
                   :id (second cells)
                   :state (third cells)
                   :started (fourth cells)
                   :conversation (fifth cells)
                   :workspace (sixth cells))
                  rows)))))
    (nreverse rows)))

(defun localgroup-tell (session message)
  (run-capture (list "localgroup" "tell" session message) :timeout 240))

(defun localgroup-pause (session)
  (run-capture (list "localgroup" "pause" session) :timeout 240))

(defun localgroup-kill (session)
  (run-capture (list "localgroup" "kill" session) :timeout 240))

;;; --- conversations ---

(defun conversations-summary (&optional (limit 8))
  (let* ((directory (conversations-directory))
         (dirs (and (uiop:directory-exists-p directory)
                    (list-subdirectories directory))))
    (values (length dirs)
            (when dirs
              (let ((sorted (sort (copy-list dirs)
                                  (lambda (a b)
                                    (> (or (file-write-date a) 0)
                                       (or (file-write-date b) 0))))))
                (subseq sorted 0 (min limit (length sorted))))))))

;;; --- headless run-job integration ---

(defstruct (job (:conc-name job-))
  id task role permissions timeout-seconds attachments workspace
  status created-at started-at finished-at
  result-text failure-category failure-message trace-id usage
  log-text error-text process)

(defparameter *jobs-lock* (bt:make-lock "awui-jobs"))
(defparameter *jobs* (make-hash-table :test #'equal))

(defun default-workspace ()
  (trim (or (env-value "AUTOLITH_WEBUI_WORKSPACE")
            (namestring (user-homedir-pathname)))))

(defun job-file (id extension)
  (uiop:merge-pathnames* (format nil "~A.~A" id extension) (jobs-directory)))

(defun job-prompt (job)
  (let ((base (format nil
                      "Complete the following task and submit your final answer as a plain string result.~%~%Task:~%~A"
                      (job-task job))))
    (if (job-attachments job)
        (format nil
                "~A~%~%Uploaded attachment files are available at these absolute paths:~{~%~A~}~%Inspect them with your tools as needed."
                base (job-attachments job))
        base)))

(defun job-envelope (job)
  (list :autolith-job
        :version 1
        :id (job-id job)
        :role (job-role job)
        :prompt (job-prompt job)
        :input (list :task (job-task job)
                     :attachments (job-attachments job)
                     :source :autolith-webui)
        :output-contract '(:type :string)
        :timeout-seconds (job-timeout-seconds job)))

(defun write-envelope (path envelope)
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                          :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-readably* nil)
              (*print-escape* t)
              (*print-array* nil)
              (*print-circle* nil)
              (*print-pretty* t)
              (*print-case* :downcase))
          (write envelope :stream stream)))
    (terpri stream)))

(defun prune-jobs ()
  (when (> (hash-table-count *jobs*) 80)
    (let* ((entries (sort (loop for id being the hash-keys of *jobs*
                                collect (cons id (job-created-at (gethash id *jobs*))))
                          (lambda (a b) (> (or (cdr a) 0) (or (cdr b) 0)))))
           (doomed (subseq entries 80)))
      (dolist (entry doomed)
        (remhash (car entry) *jobs*)))))

(defun read-stream-file (path &optional (limit 8000))
  (when (uiop:file-exists-p path)
    (last-chars (trim (uiop:read-file-string path)) limit)))

(defun run-job-worker (job)
  (handler-case
      (let* ((input-path (job-file (job-id job) "input"))
             (result-path (job-file (job-id job) "result"))
             (stdout-path (job-file (job-id job) "log"))
             (stderr-path (job-file (job-id job) "err"))
             (deadline (+ (get-universal-time) (job-timeout-seconds job) 180))
             (process nil))
        (write-envelope input-path (job-envelope job))
        (setf (job-started-at job) (get-universal-time)
              (job-status job) :running)
        (unwind-protect
             (progn
               (setf process
                     (uiop:with-current-directory
                         ((uiop:ensure-directory-pathname (job-workspace job)))
                       (uiop:launch-program
                        (list *autolith-binary* "run-job"
                              "--input" (namestring input-path)
                              "--output" (namestring result-path)
                              "--permissions" (job-permissions job))
                        :output stdout-path
                        :error-output stderr-path)))
               (setf (job-process job) process)
               (loop while (and (uiop:process-alive-p process)
                                (< (get-universal-time) deadline))
                     do (sleep 1))
               (when (uiop:process-alive-p process)
                 (ignore-errors (uiop:terminate-process process))
                 (setf (job-status job) :timeout)))
          (when (and process (uiop:process-alive-p process))
            (ignore-errors (uiop:terminate-process process))))
        (setf (job-log-text job) (read-stream-file stdout-path)
              (job-error-text job) (read-stream-file stderr-path))
        (let ((envelope (first (read-state-forms result-path)))
              (status (job-status job)))
          (setf (job-finished-at job) (get-universal-time))
          (cond
            (envelope
             (setf (job-trace-id job) (state-field envelope :trace-id)
                   (job-usage job) (state-field envelope :usage))
             (if (eq (state-field envelope :status) :succeeded)
                 (let ((result (state-field envelope :result)))
                   (setf (job-status job) :succeeded
                         (job-result-text job)
                         (if (stringp result) result (princ-to-string result))))
                 (let ((failure (state-field envelope :failure)))
                   (setf (job-status job) :failed
                         (job-failure-category job)
                         (and failure (state-field failure :category :unknown))
                         (job-failure-message job)
                         (and failure (state-field failure :message
                                                   "The job failed."))))))
            ((eq status :killed) nil)
            ((eq status :timeout)
             (setf (job-status job) :timeout))
            (t
             (setf (job-status job) :crashed)))))
    (error (condition)
      (setf (job-status job) :failed
            (job-failure-message job) (princ-to-string condition)
            (job-finished-at job) (get-universal-time)))))

(defun submit-job (task &key (role "task") (permissions "sandbox") (timeout-seconds 900)
                             (attachments nil) (workspace (default-workspace)))
  "Queue one headless child-agent job; it runs on its own thread via
`autolith run-job` and its result envelope is parsed when it finishes."
  (let* ((id (concatenate 'string "j" (random-id 10)))
         (job (make-job :id id
                        :task (trim task)
                        :role (or role "task")
                        :permissions (or permissions "sandbox")
                        :timeout-seconds (clamp-int (or timeout-seconds 900) 30 86400)
                        :attachments attachments
                        :workspace (if (blank-p workspace) (default-workspace) workspace)
                        :status :queued
                        :created-at (get-universal-time))))
    (bt:with-lock-held (*jobs-lock*)
      (setf (gethash id *jobs*) job)
      (prune-jobs))
    (ignore-errors
     (bt:make-thread (lambda () (run-job-worker job))
                     :name (format nil "awui-job-~A" id)))
    job))

(defun kill-job (id)
  (let ((job (bt:with-lock-held (*jobs-lock*) (gethash id *jobs*))))
    (when (and job (member (job-status job) '(:queued :running)))
      (setf (job-status job) :killed)
      (when (and (job-process job) (uiop:process-alive-p (job-process job)))
        (ignore-errors (uiop:terminate-process (job-process job))))
      job)))

(defun job-list-newest-first ()
  (bt:with-lock-held (*jobs-lock*)
    (sort (loop for job being the hash-values of *jobs* collect job)
          (lambda (a b) (> (or (job-created-at a) 0) (or (job-created-at b) 0))))))
