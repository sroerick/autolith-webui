;;;; chat.lisp --- session chat: transcripts from conversation event logs.

(in-package #:autolith-webui)

;;; --- conversation log access ---

(defun valid-session-id-p (id)
  "Detached session ids look like X-54EfaE. Everything chat reads from a
session id goes through here first so it can never reach a pathname."
  (and (stringp id)
       (ppcre:scan "^[0-9A-Za-z]{1,2}-[0-9A-Za-z]{2,}$" id)))

(defun session-conversation-id (session)
  "Conversation directories drop the dash from the session id
(X-54EfaE -> X54EfaE)."
  (let ((dash (position #\- session)))
    (if dash
        (concatenate 'string (subseq session 0 dash)
                     (subseq session (1+ dash)))
        session)))

(defun conversation-events (session)
  "Every event form from the session's conversation log, oldest first.
Returns NIL when the directory or files are missing."
  (let ((directory (uiop:merge-pathnames*
                    (format nil "~A/" (session-conversation-id session))
                    (conversations-directory))))
    (when (uiop:directory-exists-p directory)
      (let ((files (sort (copy-list (list-files directory))
                         #'string< :key #'namestring)))
        (loop for file in files
              append (read-state-forms file))))))

;;; --- wire JSON (the assistant side of the log) ---

(defun json-get (object key)
  (when (hash-table-p object)
    (gethash key object)))

(defun provider-chat-item (seq time wire-json)
  "Turn one :PROVIDER-ITEM wire JSON into a chat item: assistant text
(kind :agent, from output_text blocks) or a tool call (kind :tool).
Reasoning items and anything unparseable are skipped."
  (handler-case
      (let ((object (yason:parse wire-json)))
        (cond ((equal (json-get object "type") "message")
               (let ((text (with-output-to-string (out)
                             (dolist (block (json-get object "content"))
                               (when (equal (json-get block "type")
                                            "output_text")
                                 (let ((piece (json-get block "text")))
                                   (when piece
                                     (write-string piece out))))))))
                 (when (plusp (length text))
                   (list :kind :agent :seq seq :time time :text text))))
              ((equal (json-get object "type") "function_call")
               (list :kind :tool :seq seq :time time
                     :text (format nil "~A~@[ · ~A~]"
                                   (json-get object "name")
                                   (json-get object "namespace"))
                     :detail (or (json-get object "arguments") "")))))
    (error () nil)))

(defun chat-items-from-forms (forms &key (limit 80))
  "Extract chat items from conversation event FORMS, oldest first,
capped to the last LIMIT items. Each branch appends a LIST of items so
a single provider item can never splice into the top-level result."
  (let* ((items (loop for form in forms
                      for head = (first form)
                      for args = (rest form)
                      when (eq head :message)
                        append (let ((content (getf args :content)))
                                 (when (stringp content)
                                   (list (list :kind (if (eq (getf args :role)
                                                                     :user)
                                                         :user :agent)
                                               :seq (getf args :seq)
                                               :time (getf args :time)
                                               :text content))))
                      when (eq head :provider-item)
                        append (let ((item (provider-chat-item (getf args :seq)
                                                               (getf args :time)
                                                               (getf args :wire-json))))
                                 (when item (list item)))))
         (ordered (sort items #'<
                        :key (lambda (item) (or (getf item :seq) 0)))))
    (if (> (length ordered) limit)
        (last ordered limit)
        ordered)))

(defun chat-items (session &key (limit 80))
  (when (valid-session-id-p session)
    (chat-items-from-forms (conversation-events session) :limit limit)))

;;; --- rendering ---

(defun chat-clock (time)
  (when time
    (multiple-value-bind (second minute hour)
        (decode-universal-time time 0)
      (declare (ignore second))
      (format nil "~2,'0D:~2,'0D" hour minute))))

(defun chat-bubble (item)
  (ecase (getf item :kind)
    (:user
     (list :div :class "msg user"
           (list :div :class "msg-meta"
                 (if (getf item :time)
                     (format nil "you · ~A" (chat-clock (getf item :time)))
                     "you · queued"))
           (list :div :class "msg-body" (getf item :text))))
    (:agent
     (list :div :class "msg agent"
           (list :div :class "msg-meta"
                 (format nil "agent · ~A" (chat-clock (getf item :time))))
           (list :div :class "msg-body" (getf item :text))))
    (:tool
       (list :details :class "msg tool"
             :data-seq (getf item :seq)
             (list :summary (getf item :text))
             (list :pre :class "msg-detail" (getf item :detail))))))

(defun thread-bubbles-html (session &optional pending error)
  "Just the transcript content: bubbles, an optional just-sent user
bubble, and an optional error note. Swapped INTO the persistent
#chat-thread container, so its scroll position survives polling."
  (let* ((items (chat-items session))
         (last-user (and items
                         (eq (getf (car (last items)) :kind) :user)
                         (getf (car (last items)) :text)))
         (items (if (and pending (not (equal last-user pending)))
                    (append items
                            (list (list :kind :user :time nil :text pending)))
                    items)))
    (list
     (when error (list :div :class "msg error" error))
     (mapcar #'chat-bubble items)
     (unless items
       (list :p :class "muted"
             "No transcript events yet (or the conversation log is
unavailable). Messages you send still queue.")))))

(defun thread-html (session &optional pending error)
  "The #chat-thread scroll container carrying the polling attributes,
wrapping the transcript content (swapped via innerHTML so the
container - and its scroll position - persists)."
  (list :div :id "chat-thread"
        :hx-get (format nil "/chat/messages?session=~A" session)
        :hx-trigger "every 3s" :hx-swap "innerHTML"
        (thread-bubbles-html session pending error)))

(defun chat-threads-html (&optional selected)
  "The polling session sidebar."
  (let ((rows (parse-sessions (localgroup-status-text))))
    (list :aside :id "chat-threads"
          :hx-get (format nil "/chat/threads?session=~A" (or selected ""))
          :hx-trigger "every 8s" :hx-swap "outerHTML"
          (if (null rows)
              (list :p :class "muted" "No detached sessions.")
              (loop for row in rows
                    for id = (session-row-id row)
                    collect (list :button
                                  :class (if (equal id selected)
                                             "chat-thread-btn active"
                                             "chat-thread-btn")
                                  :hx-get (format nil "/chat/thread?session=~A" id)
                                  :hx-target "#chat-main" :hx-swap "innerHTML"
                                  :hx-push-url (format nil "/panel/chat?session=~A" id)
                                  (list :span :class "mono" id)
                                  (list :span
                                        :class (if (string= (session-row-state row)
                                                            "active")
                                                   "badge st-running"
                                                   "badge st-queued")
                                        (session-row-state row))
                                  (list :div :class "muted"
                                        (excerpt (session-row-conversation row)
                                                 44))))))))

(defun chat-composer-html (session)
  (list :form :class "composer"
        :method "post" :action "/chat/send"
        :enctype "multipart/form-data"
        :hx-post "/chat/send" :hx-encoding "multipart/form-data"
          :hx-target "#chat-thread" :hx-swap "innerHTML"
        (list :input :type "hidden" :name "session" :value session)
        (list :textarea :name "message" :required "required"
              :placeholder "message the session...")
        (list :button :type "submit" "send"
              (list :span :class "htmx-indicator spinner"))))

(defun panel-chat-html (&optional selected)
  (list
   (list :h2 "Chat")
   (list :p :class "muted"
         "Talk to detached sessions (`localgroup tell`). The transcript reads
the session's conversation log and polls every 3s; sends queue a message.")
   (list :div :class "chat-grid"
         (chat-threads-html selected)
         (list :div :id "chat-main"
               (if (and selected (valid-session-id-p selected))
                   (list (thread-html selected)
                         (chat-composer-html selected))
                   (list :p :class "muted"
                         "Select a session from the sidebar."))))))