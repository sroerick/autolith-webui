;;;; state.lisp --- reading Autolith durable state (agendas, memories, papercuts).

(in-package #:autolith-webui)

(defparameter *state-readtable* nil)

(defun ensure-state-readtable ()
  "A readtable that can read Autolith state files. Autolith prints workspace
strings as #A((39) BASE-CHAR . \"/path\"), which the standard array reader
cannot round-trip; this dispatch recovers the string."
  (or *state-readtable*
      (setf *state-readtable*
            (let ((table (copy-readtable nil)))
              (set-dispatch-macro-character
               #\# #\A
               (lambda (stream sub-char infix)
                 (declare (ignore sub-char infix))
                 (let ((form (read stream t nil t)))
                   (if (and (consp form)
                            (stringp (cdr (last form))))
                       (cdr (last form))
                       form)))
               table)
              table))))

(defparameter +state-eof+ (make-symbol "STATE-EOF"))

(defparameter *state-read-package* nil)

(defun ensure-state-read-package ()
  "An isolated intern home for unqualified symbols read from state text.
Unqualified tokens never touch live packages; the parsed form is mapped
onto keywords afterwards by NORMALIZE-STATE-FORM."
  (or *state-read-package*
      (setf *state-read-package*
            (or (find-package "AUTOLITH-WEBUI-STATE-READ")
                (make-package "AUTOLITH-WEBUI-STATE-READ" :use '())))))

(defun normalize-state-symbol (symbol)
  "Map symbols interned in the isolated read package to keywords. NIL and
T are interned there too, so they read back as themselves; every other
symbol, keyword, or object passes through unchanged."
  (if (eq (symbol-package symbol) (ensure-state-read-package))
      (let ((name (symbol-name symbol)))
        (cond ((string-equal name "NIL") nil)
              ((string-equal name "T") 't)
              (t (values (intern name :keyword)))))
      symbol))

(defun normalize-state-form (form)
  (typecase form
    (cons (cons (normalize-state-form (car form))
                (normalize-state-form (cdr form))))
    (symbol (normalize-state-symbol form))
    (t form)))

(defun read-state-string (string)
  (let ((*read-eval* nil)
        (*readtable* (ensure-state-readtable))
        (*package* (ensure-state-read-package)))
    (with-input-from-string (stream string)
      (normalize-state-form (read stream nil +state-eof+)))))

(defun read-state-forms (path)
  "Read every form from an Autolith state file with a safe reader.
Returns the empty list when PATH is missing. Reading never evaluates:
*READ-EVAL* is disabled, and unqualified symbols first intern in an
isolated package, then become keywords (with NIL and T kept as
themselves), so nothing from the file can call home."
  (when (uiop:file-exists-p path)
    (with-open-file (stream path :if-does-not-exist nil)
      (let ((*read-eval* nil)
            (*readtable* (ensure-state-readtable))
            (*package* (ensure-state-read-package)))
        (loop for form = (read stream nil +state-eof+)
              until (eq form +state-eof+)
              collect (normalize-state-form form))))))

(defun state-form (forms header)
  (loop for form in forms
        when (and (consp form) (eq (first form) header))
          return form))

(defun state-field (form key &optional default)
  (when (consp form)
    (let ((value (getf (rest form) key)))
      (if (null value) default value))))

(defun state-string (form key &optional default)
  "Like STATE-FIELD but only returns STRING values; anything else
(e.g. the keyword :NIL Autolith writes for an absent workspace on
global memories) falls back to DEFAULT."
  (let ((value (and (consp form) (getf (rest form) key))))
    (if (stringp value) value default)))

(defun state-records (file header-tag record-tag &optional (container :records))
  "Collect RECORD-TAG records from a state FILE. Handles both the single-form
layout (agendas.sexp keeps items under :RECORDS) and the stream layout (one
header form followed by one record form each)."
  (let ((forms (read-state-forms (state-file file))))
    (remove-if-not (lambda (form)
                     (and (consp form) (eq (first form) record-tag)))
                   (append
                    (let ((header (state-form forms header-tag)))
                      (and header (state-field header container)))
                    forms))))

(defun state-file (name)
  (uiop:merge-pathnames* name (state-directory)))

(defun agenda-records ()
  (state-records "agendas.sexp" :agendas :agenda))

(defun memory-records ()
  (state-records "memories.sexp" :memories :memory))

(defun papercut-records ()
  (state-records "papercuts.sexp" :papercuts :papercut))
