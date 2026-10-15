;;;; config.lisp --- runtime configuration and directories.

(in-package #:autolith-webui)

(defun env-value (name &optional default)
  (let ((value (uiop:getenv name)))
    (if (blank-p value) default value)))

(defun env-integer (name &optional default)
  (parse-integer-or (uiop:getenv name) default))

(defparameter *host* "127.0.0.1")
(defparameter *port* 8078)
(defparameter *token* nil
  "When non-nil, every request must present this shared token via
query parameter, cookie, or Authorization: Bearer header.")
(defparameter *autolith-binary* "autolith")
(defparameter *maximum-upload-bytes* (* 64 1024 1024))
(defparameter *maximum-head-bytes* (* 96 1024))

(defparameter *data-directory* nil)
(defparameter *uploads-directory* nil)
(defparameter *jobs-directory* nil)
(defparameter *static-cache*
  (make-hash-table :test #'equal)
  "Embedded copies of static assets; the dumped executable serves these when
the source tree is not present next to the binary.")

(defun data-directory ()
  (or *data-directory*
      (setf *data-directory*
            (ensure-directory
             (uiop:ensure-directory-pathname
              (or (env-value "AUTOLITH_WEBUI_HOME")
                  (uiop:merge-pathnames* ".local/share/autolith-webui/"
                                         (user-homedir-pathname))))))))

(defun uploads-directory ()
  (or *uploads-directory*
      (setf *uploads-directory*
            (ensure-directory
             (uiop:ensure-directory-pathname
              (or (env-value "AUTOLITH_WEBUI_UPLOADS")
                  (uiop:merge-pathnames* "uploads/" (data-directory))))))))

(defun jobs-directory ()
  (or *jobs-directory*
      (setf *jobs-directory*
            (ensure-directory
             (uiop:ensure-directory-pathname
              (or (env-value "AUTOLITH_WEBUI_JOBS")
                  (uiop:merge-pathnames* "jobs/" (data-directory))))))))

(defun state-directory ()
  "The durable Autolith state directory (agendas, memories, papercuts)."
  (uiop:ensure-directory-pathname
   (or (env-value "AUTOLITH_WEBUI_STATE_DIR")
       (uiop:merge-pathnames* ".local/share/autolith/" (user-homedir-pathname)))))

(defun conversations-directory ()
  (uiop:merge-pathnames* "conversations/" (state-directory)))

(defun static-directory ()
  (or (ignore-errors (uiop:ensure-directory-pathname
                      (env-value "AUTOLITH_WEBUI_STATIC")))
      (ignore-errors (uiop:merge-pathnames*
                      "static/"
                      (asdf:system-source-directory :autolith-webui)))))

(defun ensure-runtime-directories ()
  (ensure-directory (data-directory))
  (ensure-directory (uploads-directory))
  (ensure-directory (jobs-directory)))

(defun static-content-type (name)
  (let ((type (ignore-errors (string-upcase (pathname-type name)))))
    (case type
      (:css "text/css; charset=utf-8")
      (:js "text/javascript; charset=utf-8")
      (:svg "image/svg+xml")
      (:png "image/png")
      (:ico "image/x-icon")
      (:html "text/html; charset=utf-8")
      (:txt "text/plain; charset=utf-8")
      (t "application/octet-stream"))))

(defun warm-static-cache ()
  "Cache static assets into the image so the dumped executable can serve them
without the source tree."
  (let ((directory (static-directory)))
    (when directory
      (dolist (path (ignore-errors (list-files directory)))
        (ignore-errors
          (setf (gethash (pathname-name path) *static-cache*)
                (read-file-octets path)))))))
