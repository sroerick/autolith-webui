;;;; http.lisp --- a small threaded HTTP/1.1 server with native multipart support.

(in-package #:autolith-webui)

;;; --- protocol errors ---

(define-condition http-error (error)
  ((status :initarg :status :reader http-error-status)
   (message :initarg :message :reader http-error-message))
  (:report (lambda (condition stream)
             (format stream "HTTP ~A: ~A"
                     (http-error-status condition)
                     (http-error-message condition)))))

(defun http-error (status control &rest arguments)
  (error 'http-error :status status :message (apply #'format nil control arguments)))

;;; --- logging ---

(defparameter *log-lock* (bt:make-lock "awui-log"))
(defparameter *log-ring* nil)
(defparameter *connection-count* 0)
(defparameter *connection-count-lock* (bt:make-lock "awui-connections"))

(defun log-line (control &rest arguments)
  (let ((line (format nil "[~A] ~A"
                      (format-utc (get-universal-time))
                      (apply #'format nil control arguments))))
    (bt:with-lock-held (*log-lock*)
      (push line *log-ring*)
      (when (> (length *log-ring*) 200)
        (setf *log-ring* (subseq *log-ring* 0 200))))
    (format *error-output* "~A~%" line)
    line))

(defun log-tail (&optional (limit 12))
  (bt:with-lock-held (*log-lock*)
    (subseq *log-ring* 0 (min limit (length *log-ring*)))))

;;; --- request/response model ---

(defstruct (request (:conc-name req-))
  method raw-target path query-params headers body params files)

(defstruct (upload (:conc-name upload-))
  name filename content-type data)

(defstruct (response (:conc-name resp-))
  status content-type headers body)

(defun param (request name &optional default)
  "Return parameter NAME from the posted body or the query string."
  (let ((entry (assoc name (append (req-params request) (req-query-params request))
                      :test #'string=)))
    (if entry (cdr entry) default)))

(defun header-value (request name)
  (cdr (assoc name (req-headers request) :test #'string=)))

;;; --- routing ---

(defstruct route method pattern function)

(defparameter *routes* nil)
(defparameter *routes-installed-p* nil)

(defun register-route (method pattern-string function)
  (setf *routes*
        (remove-if (lambda (route)
                     (and (eq (route-method route) method)
                          (string= (route-pattern route) pattern-string)))
                   *routes*))
  (push (make-route :method method :pattern pattern-string :function function)
        *routes*))

(defmacro defroute (method pattern args &body body)
  (let ((request (first args))
        (captures (rest args)))
    `(register-route ,method ,pattern
                     (lambda (,request &rest route-captures)
                       (destructuring-bind (,@captures) route-captures
                         (declare (ignorable ,@captures))
                         ,@body)))))

;;; --- responses ---

(defun status-reason (code)
  (case code
    (100 "Continue") (200 "OK") (201 "Created") (204 "No Content")
    (302 "Found") (304 "Not Modified")
    (400 "Bad Request") (401 "Unauthorized") (403 "Forbidden")
    (404 "Not Found") (405 "Method Not Allowed") (411 "Length Required")
    (413 "Payload Too Large") (431 "Request Header Fields Too Large")
    (500 "Internal Server Error") (502 "Bad Gateway") (505 "HTTP Version Not Supported")
    (t "Response")))

(defun respond (status content-type body &key (headers nil))
  (make-response :status status :content-type content-type :headers headers :body body))

(defun html-response (body &optional (status 200))
  (respond status "text/html; charset=utf-8" body))

(defun text-response (body &optional (status 200) (type "text/plain; charset=utf-8"))
  (respond status type body))

(defun octets-response (octets content-type &key filename inline-p)
  (respond 200 content-type octets
           :headers (list (list "Content-Disposition"
                                (format nil "~A; filename=\"~A\""
                                        (if inline-p "inline" "attachment")
                                        filename)))))

(defun redirect-response (location)
  (respond 302 "text/plain; charset=utf-8" ""
           :headers (list (list "Location" location))))

(defun error-response (status message)
  (respond status "text/plain; charset=utf-8" message))

;;; --- auth ---

(defun cookie-value (request name)
  (let ((header (header-value request "cookie")))
    (when header
      (loop for pair in (ppcre:split ";\\s*" header)
            for equals = (position #\= pair)
            when (and equals (string= (subseq pair 0 equals) name))
              return (subseq pair (1+ equals))))))

(defun authorization-token (request)
  (let ((header (header-value request "authorization")))
    (when (and header (>= (length header) 7)
                 (string-equal "bearer " header :end1 7 :end2 7))
      (subseq header 7))))

(defun request-token (request)
  (or (param request "token")
      (cookie-value request "token")
      (authorization-token request)))

(defun authorized-p (request)
  (or (null *token*)
      (constant-time-equal (request-token request) *token*)))

(defun dispatch (request)
  (unless (authorized-p request)
    (return-from dispatch (error-response 401 "Unauthorized: missing or invalid token.")))
  (let ((method (if (eq (req-method request) :head) :get (req-method request))))
    (dolist (route *routes*)
      (when (or (eq (route-method route) t) (eq (route-method route) method))
        (multiple-value-bind (match captures)
            (ppcre:scan-to-strings (route-pattern route) (req-path request))
          (when match
            (return-from dispatch
              (apply (route-function route) request
                     (if captures (coerce captures 'list) ()))))))))
  (error-response 404 (format nil "No route for ~A ~A." (req-method request) (req-path request))))

;;; --- wire reading ---

(defun read-head-octets (stream)
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
    (loop
      (let ((byte (read-byte stream nil nil)))
        (unless byte
          (http-error 400 "Connection closed mid-request."))
        (vector-push-extend byte buffer)
        (when (and (>= (fill-pointer buffer) 4)
                   (= (aref buffer (- (fill-pointer buffer) 4)) 13)
                   (= (aref buffer (- (fill-pointer buffer) 3)) 10)
                   (= (aref buffer (- (fill-pointer buffer) 2)) 13)
                   (= (aref buffer (- (fill-pointer buffer) 1)) 10))
          (return-from read-head-octets buffer))
        (when (> (fill-pointer buffer) *maximum-head-bytes*)
          (http-error 431 "Request head too large."))))))

(defun content-length (headers)
  (let ((value (cdr (assoc "content-length" headers :test #'string=))))
    (when value
      (let ((number (ignore-errors (parse-integer (trim value)))))
        (when (and number (>= number 0)) number)))))

(defun read-body-octets (stream length)
  (when (> length *maximum-upload-bytes*)
    (http-error 413 "Request body exceeds the upload limit."))
  (let ((buffer (make-array length :element-type '(unsigned-byte 8)))
        (filled 0))
    (loop while (< filled length)
          do (let ((count (read-sequence buffer stream :start filled)))
               (when (<= count filled)
                 (http-error 400 "Connection closed mid-body."))
               (setf filled count)))
    buffer))

;;; --- decoding helpers ---

(defun percent-decode (string &key (plus-as-space-p t))
  (let* ((length (length string))
         (bytes (make-array length :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)))
    (loop for index from 0 below length
          do (let ((character (char string index)))
               (cond
                 ((char= character #\%)
                  (let ((high (and (< (+ index 2) length)
                                   (digit-char-p (char string (+ index 1)) 16)))
                        (low (and (< (+ index 2) length)
                                  (digit-char-p (char string (+ index 2)) 16))))
                    (if (and high low)
                        (progn (vector-push-extend (+ (* high 16) low) bytes)
                               (incf index 2))
                        (vector-push-extend (char-code character) bytes))))
                 ((and plus-as-space-p (char= character #\+))
                  (vector-push-extend 32 bytes))
                 (t
                  (vector-push-extend (char-code character) bytes)))))
    (utf8-string bytes)))

(defun parse-urlencoded (string)
  (when (blank-p string)
    (return-from parse-urlencoded nil))
  (loop for pair in (ppcre:split "&" string)
        for equals = (position #\= pair)
        collect (if equals
                    (cons (percent-decode (subseq pair 0 equals))
                          (percent-decode (subseq pair (1+ equals))))
                    (cons (percent-decode pair) ""))))

;;; --- multipart/form-data ---

(defun multipart-boundary (content-type)
  (multiple-value-bind (all groups)
      (ppcre:scan-to-strings "boundary=(?:\"([^\"]+)\"|([^;\\s]+))" content-type)
    (declare (ignore all))
    (when groups
      (loop for group across groups
            when group return (trim group)))))

(defun parse-multipart-part (part-text)
  "Split one multipart part into (values name filename content-type octets)."
  (let* ((crlf2 (search (coerce (list #\Return #\Newline #\Return #\Newline) 'string)
                        part-text))
         (lf2 (and (null crlf2)
                   (search (coerce (list #\Newline #\Newline) 'string) part-text)))
         (header-text (cond (crlf2 (subseq part-text 0 crlf2))
                            (lf2 (subseq part-text 0 lf2))
                            (t part-text)))
         (body-start (cond (crlf2 (+ crlf2 4))
                           (lf2 (+ lf2 2))
                           (t nil)))
         (body-text (if body-start (subseq part-text body-start) "")))
    (let ((headers (loop for line in (ppcre:split "\\r?\\n" header-text)
                         for colon = (position #\: line)
                         when colon
                           collect (cons (string-downcase (trim (subseq line 0 colon)))
                                         (trim (subseq line (1+ colon)))))))
      (let* ((disposition (cdr (assoc "content-disposition" headers :test #'string=)))
             (name (and disposition
                        (ppcre:register-groups-bind (value)
                            ("name=\"([^\"]*)\"" disposition)
                          value)))
             (filename (and disposition
                            (ppcre:register-groups-bind (value)
                                ("filename=\"([^\"]*)\"" disposition)
                              (and (plusp (length value)) value))))
             (part-type (cdr (assoc "content-type" headers :test #'string=))))
        (values name filename (or part-type "application/octet-stream")
                (latin1-octets body-text))))))

(defun parse-multipart (request content-type)
  "Parse a multipart/form-data body into params and file uploads.
Part payloads round-trip through latin-1 so binary uploads survive intact."
  (let ((boundary (multipart-boundary content-type)))
    (unless boundary
      (http-error 400 "multipart/form-data body without boundary."))
    (let* ((text (latin1-string (req-body request)))
           (delimiter (concatenate 'string "--" boundary))
           (scanner (ppcre:create-scanner (ppcre:quote-meta-chars delimiter)))
           (matches (ppcre:all-matches scanner text))
           (pairs (loop for rest on matches by #'cddr
                        collect (list (first rest) (second rest))))
           (params '())
           (files '()))
        (loop for index from 0 below (length pairs)
              do (let ((entry (nth index pairs))
                       (next (and (< (1+ index) (length pairs))
                                  (nth (1+ index) pairs))))
                   (let ((match-end (second entry)))
                     (when (and (< match-end (length text))
                                  (string= text "--" :start1 match-end
                                           :end1 (min (length text) (+ match-end 2))))
                       (return)))
                   (when next
                     (multiple-value-bind (name filename part-type data)
                         (multipart-part-at text entry next)
                       (when name
                         (if filename
                             (push (make-upload :name name :filename filename
                                                :content-type part-type :data data)
                                   files)
                             (push (cons name (utf8-string data)) params)))))))
        (setf (req-params request) (nreverse params)
              (req-files request) (nreverse files)))))

(defun multipart-part-at (text entry next)
  "Extract one multipart part between delimiter ENTRY and delimiter NEXT.
Returns (values name filename content-type octets)."
  (let* ((match-end (second entry))
         (next-start (first next))
         (line-end (or (position #\Newline text :start match-end)
                       (1- next-start)))
         (part-start (1+ line-end))
         (part-end next-start)
         (payload-end
           (cond ((and (>= part-end 2)
                       (char= (char text (- part-end 2)) #\Return)
                       (char= (char text (- part-end 1)) #\Newline))
                  (- part-end 2))
                 ((and (>= part-end 1)
                       (char= (char text (- part-end 1)) #\Newline))
                  (1- part-end))
                 (t part-end))))
    (if (< part-start payload-end)
        (parse-multipart-part (subseq text part-start payload-end))
        (values nil nil nil nil))))

(defun parse-request-params (request)
  (let ((content-type (or (header-value request "content-type") "")))
    (cond
      ((zerop (length content-type)) nil)
      ((ppcre:scan "(?i)multipart/form-data" content-type)
       (parse-multipart request content-type))
      ((ppcre:scan "(?i)application/x-www-form-urlencoded" content-type)
       (setf (req-params request)
             (parse-urlencoded (utf8-string (req-body request)))))
      (t nil))))

;;; --- head parsing ---

(defun parse-request-head (head-octets)
  (let ((lines (ppcre:split "\\r?\\n" (latin1-string head-octets))))
    (unless lines
      (http-error 400 "Empty request."))
    (let* ((parts (ppcre:split "\\s+" (first lines))))
      (unless (and (= 3 (length parts))
                   (member (first parts)
                           '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS")
                           :test #'string=))
        (http-error 400 "Malformed request line."))
      (unless (string= (third parts) "HTTP/1.1")
        (http-error 505 "Only HTTP/1.1 is supported."))
      (let* ((method (intern (first parts) :keyword))
             (target (second parts))
             (question (position #\? target))
             (raw-path (if question (subseq target 0 question) target))
             (query-string (if question (subseq target (1+ question)) ""))
             (path (percent-decode raw-path :plus-as-space-p nil))
             (headers (loop for line in (rest lines)
                            for colon = (position #\: line)
                            when colon
                              collect (cons (string-downcase (subseq line 0 colon))
                                            (string-left-trim " " (subseq line (1+ colon)))))))
        (when (assoc "transfer-encoding" headers :test #'string=)
          (http-error 411 "Chunked request bodies are not supported."))
          (list method path (parse-urlencoded query-string) headers)))))

;;; --- wire writing ---

(defun write-response (stream response head-request-p)
  (let* ((body (resp-body response))
         (bytes (cond ((stringp body) (utf8-octets body))
                      ((null body) (make-array 0 :element-type '(unsigned-byte 8)))
                      (t body)))
         (lines (list (format nil "HTTP/1.1 ~D ~A"
                              (resp-status response)
                              (status-reason (resp-status response)))
                      (format nil "Content-Type: ~A"
                              (or (resp-content-type response)
                                  "text/plain; charset=utf-8"))
                      (format nil "Content-Length: ~D" (length bytes))
                      "Connection: close"
                      "X-Content-Type-Options: nosniff"
                      "X-Frame-Options: DENY"
                      "Referrer-Policy: no-referrer")))
    (dolist (header (resp-headers response))
      (setf lines (append lines (list (format nil "~A: ~A" (first header) (second header))))))
      (write-sequence
       (ascii-octets
        (with-output-to-string (head)
          (dolist (line lines)
            (format head "~A~C~C" line #\Return #\Newline))
          (format head "~C~C" #\Return #\Newline)))
       stream)
    (unless head-request-p
      (write-sequence bytes stream))
    (finish-output stream)))

(defun write-plain-response (socket status message)
  (ignore-errors
    (write-response (usocket:socket-stream socket)
                    (error-response status message)
                    nil)))

;;; --- connection handling ---

(defun serve-one-request (socket)
  (let* ((stream (usocket:socket-stream socket))
         (head (parse-request-head (read-head-octets stream)))
         (method (nth 0 head))
         (path (nth 1 head))
         (query (nth 2 head))
         (headers (nth 3 head)))
    (let ((expect (cdr (assoc "expect" headers :test #'string=))))
      (when (and expect (string-equal (trim expect) "100-continue"))
        (write-sequence
         (ascii-octets (format nil "HTTP/1.1 100 Continue~C~C~C~C"
                               #\Return #\Newline #\Return #\Newline))
         stream)
        (finish-output stream)))
    (let* ((length (or (content-length headers) 0))
           (body (read-body-octets stream length))
           (request (make-request :method method :raw-target path :path path
                                  :query-params query :headers headers :body body)))
      (parse-request-params request)
      (let* ((started (get-internal-real-time))
             (response (dispatch request)))
        (write-response stream response (eq method :head))
        (log-line "~A ~A -> ~D (~A ms)" method path (resp-status response)
                  (round (- (get-internal-real-time) started)
                         (/ internal-time-units-per-second 1000)))))))

(defun handle-connection (socket)
  (unwind-protect
       (progn
         (bt:with-lock-held (*connection-count-lock*)
           (incf *connection-count*))
         (ignore-errors
          (setf (usocket:socket-option socket :receive-timeout) 180))
         (handler-case
             (serve-one-request socket)
           (http-error (condition)
             (write-plain-response socket (http-error-status condition)
                                   (http-error-message condition)))
           ((or end-of-file usocket:connection-reset-error
                usocket:connection-aborted-error usocket:timeout-error)
            nil)
           (error (condition)
             (log-line "request error: ~A" condition)
             (write-plain-response socket 500 "Internal server error."))))
    (bt:with-lock-held (*connection-count-lock*)
      (decf *connection-count*))
    (ignore-errors (usocket:socket-close socket))))

;;; --- server lifecycle ---

(defstruct (server (:conc-name server-))
  socket thread host port started-at)

(defparameter *server* nil)
(defparameter *stopping-p* nil)

(defun accept-loop (server)
  (loop
    (handler-case
        (let ((client (usocket:socket-accept (server-socket server)
                                             :element-type '(unsigned-byte 8))))
          (ignore-errors
           (bt:make-thread (lambda () (handle-connection client))
                           :name "awui-connection")))
      (error (condition)
        (cond (*stopping-p* (return-from accept-loop))
              (t (log-line "accept error: ~A" condition)
                 (sleep 0.05)))))))

(defun start (&key (host (env-value "AUTOLITH_WEBUI_HOST" *host*))
                   (port (env-integer "AUTOLITH_WEBUI_PORT" *port*)))
  "Start the console server and return the server object."
  (when *server*
    (error "The console server is already running on port ~A." (server-port *server*)))
  (setf *stopping-p* nil)
  (setf *token* (env-value "AUTOLITH_WEBUI_TOKEN"))
  (ensure-runtime-directories)
  (warm-static-cache)
  (unless *routes-installed-p*
    (install-routes)
    (setf *routes-installed-p* t))
  (let* ((socket (usocket:socket-listen host port :reuseaddress t :backlog 128))
         (server (make-server :socket socket :host host :port port
                              :started-at (get-universal-time))))
    (setf (server-thread server)
          (bt:make-thread (lambda () (accept-loop server)) :name "awui-accept"))
    (setf *server* server)
    (log-line "autolith-webui listening on ~A:~A" host port)
    server))

(defun stop ()
  "Stop the console server; returns the stopped server object."
  (let ((server *server*))
    (when server
      (setf *stopping-p* t
            *server* nil)
      (ignore-errors (usocket:socket-close (server-socket server)))
      server)))

;;; --- static assets ---

(defun serve-static (name)
  (let* ((directory (static-directory))
         (path (and directory (uiop:merge-pathnames* name directory))))
    (cond ((and path (uiop:file-exists-p path))
           (respond 200 (static-content-type name) (read-file-octets path)
                    :headers (list (list "Cache-Control" "no-cache"))))
          ((gethash name *static-cache*)
           (respond 200 (static-content-type name) (gethash name *static-cache*)
                    :headers (list (list "Cache-Control" "no-cache"))))
          (t (error-response 404 "Static asset not found.")))))
