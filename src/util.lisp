;;;; util.lisp --- small shared helpers.

(in-package #:autolith-webui)

(defun trim (string)
  "Trim ASCII whitespace from both ends of STRING (NIL-safe)."
  (when string
    (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) string)))

(defun blank-p (string)
  "True when STRING is missing or entirely whitespace."
  (or (null string) (zerop (length (trim string)))))

(defun esc (string)
  "Escape STRING for safe inclusion in HTML text and attribute values."
  (when string
    (with-output-to-string (out)
      (loop for character across string
            do (case character
                 (#\< (write-string "&lt;" out))
                 (#\> (write-string "&gt;" out))
                 (#\& (write-string "&amp;" out))
                 (#\" (write-string "&quot;" out))
                 (#\' (write-string "&#39;" out))
                 (t (write-char character out)))))))

(defparameter +hex-digits+ "0123456789abcdef")

(defun random-id (&optional (characters 12))
  "Return a random lowercase hex identifier of CHARACTERS characters."
  (with-output-to-string (out)
    (dotimes (i characters)
      (write-char (char +hex-digits+ (random 16)) out))))

(defun short-id (id &optional (limit 9))
  (subseq (or id "") 0 (min limit (length (or id "")))))

(defun utf8-octets (string)
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun utf8-string (octets)
  "Decode OCTETS as UTF-8; fall back to latin-1 instead of failing."
  (handler-case
      (flexi-streams:octets-to-string octets :external-format :utf-8)
    (error ()
      (flexi-streams:octets-to-string octets :external-format :latin-1))))

(defun latin1-string (octets)
  (flexi-streams:octets-to-string octets :external-format :latin-1))

(defun latin1-octets (string)
  (flexi-streams:string-to-octets string :external-format :latin-1))

(defun ascii-octets (string)
  (latin1-octets string))

(defun read-file-octets (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun file-size (path)
  (ignore-errors
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (file-length stream))))

(defun ensure-directory (pathspec)
  (ensure-directories-exist pathspec)
  pathspec)

(defun format-utc (universal-time)
  "Format a universal time as an RFC 3339 UTC timestamp."
  (when universal-time
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time universal-time 0)
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
              year month date hour minute second))))

(defun format-ago (universal-time)
  "Render UNIVERSAL-TIME as a coarse relative age."
  (when universal-time
    (let ((delta (- (get-universal-time) universal-time)))
      (cond ((< delta 0) "soon")
            ((< delta 60) "just now")
            ((< delta 3600) (format nil "~Dm ago" (floor delta 60)))
            ((< delta 86400) (format nil "~Dh ago" (floor delta 3600)))
            (t (format nil "~Dd ago" (floor delta 86400)))))))

(defun format-size (bytes)
  (cond ((null bytes) "-")
        ((< bytes 1024) (format nil "~D B" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1F KB" (/ bytes 1024.0)))
        ((< bytes (* 1024 1024 1024)) (format nil "~,1F MB" (/ bytes 1048576.0)))
        (t (format nil "~,2F GB" (/ bytes 1073741824.0)))))

(defun excerpt (string &optional (limit 80))
  "Collapse whitespace and clip STRING to LIMIT characters."
  (let ((clean (ppcre:regex-replace-all "\\s+" (or string "") " ")))
    (if (> (length clean) limit)
        (concatenate 'string (subseq clean 0 limit) "...")
        clean)))

(defun last-chars (string &optional (limit 4000))
  (let ((s (or string "")))
    (if (> (length s) limit)
        (concatenate 'string "... " (subseq s (- (length s) limit)))
        s)))

(defun clamp-int (value minimum maximum)
  (min maximum (max minimum value)))

(defun parse-integer-or (string default)
  (if (blank-p string)
      default
      (handler-case (parse-integer (trim string)) (error () default))))

(defun constant-time-equal (a b)
  "Compare A and B without leaking length or position through early exit."
  (let ((result (if (and (stringp a) (stringp b) (= (length a) (length b))) 0 1)))
    (when (and (stringp a) (stringp b))
      (loop for ca across a
            for cb across b
            do (setf result (logior result (logxor (char-code ca) (char-code cb))))))
    (zerop result)))

(defun safe-filename (name)
  "Reduce NAME to a flat, filesystem-safe basename, or NIL when impossible."
  (when (and (stringp name) (plusp (length name)))
    (let* ((last-slash (position #\/ name :from-end t))
           (after-slash (if last-slash (subseq name (1+ last-slash)) name))
           (last-back (position #\\ after-slash :from-end t))
           (base (if last-back (subseq after-slash (1+ last-back)) after-slash))
           (clean (remove-if-not
                   (lambda (character)
                     (or (alphanumericp character)
                          (find character ".-_" :test #'char=)))
                   base))
           (no-dots (string-left-trim "." clean)))
      (when (plusp (length no-dots))
        (subseq no-dots 0 (min (length no-dots) 120))))))

(define-condition timeout-condition (error)
  ((seconds :initarg :seconds :reader timeout-condition-seconds))
  (:report (lambda (condition stream)
             (format stream "Operation timed out after ~A seconds."
                     (timeout-condition-seconds condition)))))

#+sbcl
(defun call-with-timeout (seconds thunk)
  (handler-case (sb-ext:with-timeout seconds (funcall thunk))
    (sb-ext:timeout () (error 'timeout-condition :seconds seconds))))

#-sbcl
(defun call-with-timeout (seconds thunk)
  ;; Other implementations run unbounded; SBCL is the packaged path.
  (declare (ignore seconds))
  (funcall thunk))

(defun list-subdirectories (directory)
  "List the immediate subdirectories of DIRECTORY (NIL on error)."
  (ignore-errors
    (let ((base (uiop:ensure-directory-pathname directory)))
      (remove-if-not
       #'uiop:directory-pathname-p
       (directory (make-pathname :defaults base :name nil :type nil :version nil
                                 :directory (append (pathname-directory base)
                                                    (list :wild))))))))

(defun list-files (directory)
  "List the immediate files of DIRECTORY (NIL on error)."
  (ignore-errors
    (let ((base (uiop:ensure-directory-pathname directory)))
      (remove-if #'uiop:directory-pathname-p
                 (directory (make-pathname :defaults base :name :wild :type :wild
                                           :version nil))))))
