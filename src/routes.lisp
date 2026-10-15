;;;; routes.lisp --- panels and handlers for the console.

(in-package #:autolith-webui)

;;; --- shared fragments ---

(defun status-badge (status)
  (let ((text (string-downcase (princ-to-string status))))
    (list :span :class (format nil "badge st-~A" text) text)))

(defun stat-card (label value)
  (list :div :class "card stat"
        (list :div :class "num mono" value)
        (list :div :class "label muted" label)))

(defun basename (path)
  (car (last (remove "" (ppcre:split "/" (or path "")) :test #'string=))))

(defun model-count ()
  (length (remove-if #'blank-p (ppcre:split "\\r?\\n" (autolith-models)))))

(defun job-total ()
  (hash-table-count *jobs*))

(defun server-port-now ()
  (if *server* (server-port *server*) (env-integer "AUTOLITH_WEBUI_PORT" *port*)))

(defun server-meta-text ()
  (format nil "~A · :~A · ~A ~A"
          (short-id (autolith-version) 40)
          (server-port-now)
          (lisp-implementation-type)
          (lisp-implementation-version)))

;;; --- uploads ---

(defun unique-path (path)
  (unless (uiop:file-exists-p path)
    (return-from unique-path path))
  (let ((type (pathname-type path))
        (stem (pathname-name path)))
    (loop for i from 1
          for candidate = (make-pathname :defaults path
                                         :name (format nil "~A-~D" stem i)
                                         :type type)
          while (uiop:file-exists-p candidate)
          finally (return candidate))))

(defun save-uploads (uploads)
  "Persist UPLOADS into the uploads directory; return their absolute paths."
  (let ((directory (uploads-directory)))
    (ensure-directory directory)
    (loop for upload in uploads
          for base = (safe-filename (upload-filename upload))
          when (and base (plusp (length (upload-data upload))))
            collect (let* ((candidate (uiop:merge-pathnames* base directory))
                           (final (unique-path candidate)))
                      (with-open-file (stream final :direction :output
                                              :element-type '(unsigned-byte 8)
                                              :if-exists :error)
                        (write-sequence (upload-data upload) stream))
                      (namestring final)))))

;;; --- overview ---

(defun conversation-row (dir)
  (list :tr
        (list :td :class "mono" (basename (namestring dir)))
        (list :td :class "mono muted" (format-ago (file-write-date dir)))
        (list :td :class "mono muted"
              (princ-to-string
               (length (ignore-errors (list-files dir)))))))

(defun conversations-table (recent)
  (if (null recent)
      (list :p :class "muted" "No saved conversations.")
      (list :table
            (list :thead
                  (list :tr (list :th "id") (list :th "last activity")
                        (list :th "events")))
            (cons :tbody (mapcar #'conversation-row recent)))))

(defun panel-overview-html ()
  (multiple-value-bind (conversation-count recent) (conversations-summary 8)
    (let* ((agendas (agenda-records))
           (open-items (loop for agenda in agendas
                             append (remove-if-not
                                     (lambda (item)
                                       (member (state-field item :status)
                                               '(:todo :doing :blocked :note)))
                                     (state-field agenda :items))))
           (memories (memory-records))
           (papercuts (papercut-records))
           (sessions (parse-sessions (localgroup-status-text))))
      (list
       (list :h2 "Overview")
       (list :div :class "grid"
             (stat-card "autolith" (short-id (autolith-version) 24))
             (stat-card "models" (princ-to-string (model-count)))
             (stat-card "detached sessions" (princ-to-string (length sessions)))
             (stat-card "conversations" (princ-to-string conversation-count))
             (stat-card "open agenda items" (princ-to-string (length open-items)))
             (stat-card "memories" (princ-to-string (length memories)))
             (stat-card "papercuts" (princ-to-string (length papercuts)))
             (stat-card "jobs this run" (princ-to-string (job-total))))
       (list :div :class "card"
             (list :h3 "Models")
             (list :pre (autolith-models)))
       (list :div :class "card"
             (list :h3 "Recent conversations")
             (conversations-table recent))
       (list :div :class "card"
             (list :h3 "Server log")
             (list :pre (format nil "~{~A~%~}" (reverse (log-tail 12)))))))))

;;; --- agent jobs ---

(defun usage-text (job)
  (let ((usage (job-usage job)))
    (if usage
        (format nil "~D in · ~D out · ~D req"
                (or (getf usage :input-tokens) 0)
                (or (getf usage :output-tokens) 0)
                (or (getf usage :provider-requests) 0))
        "-")))

(defun job-result-cell (job)
  (case (job-status job)
    (:succeeded
     (list :details (list :summary "view result")
           (list :pre (or (job-result-text job) ""))))
    (:failed
     (list :div
           (list :span :class "badge st-failed"
                 (princ-to-string (or (job-failure-category job) :failed)))
           (list :details (list :summary "why")
                 (list :pre (or (job-failure-message job) "")))))
    (:crashed
     (list :details (list :summary "no result written")
           (list :pre (or (job-error-text job) (job-log-text job) ""))))
    ((:queued :running)
     (list :span :class "muted" "working..."))
    (t (list :span :class "muted"
             (string-downcase (princ-to-string (job-status job)))))))

(defun job-row-html (job)
  (list :tr
        (list :td :class "mono" (short-id (job-id job)))
        (list :td (status-badge (job-status job)))
        (list :td
              (list :details
                    (list :summary (excerpt (job-task job) 70))
                    (list :pre (format nil "task: ~A~%role: ~A · permissions: ~A · timeout: ~As · workspace: ~A~%"
                                       (job-task job) (job-role job)
                                       (job-permissions job)
                                       (job-timeout-seconds job)
                                       (job-workspace job))
                          (when (job-log-text job)
                            (list :div (list :span :class "muted" "process output:"))
                            (list :pre (job-log-text job))))))
        (list :td (job-result-cell job))
        (list :td :class "mono muted" (usage-text job)
              (list :div :class "muted" (format-ago (job-created-at job))))
        (list :td (when (member (job-status job) '(:queued :running))
                    (list :button :class "danger"
                          :hx-post "/agent/kill"
                          :hx-vals (format nil "{\"id\":\"~A\"}" (job-id job))
                          :hx-confirm "Kill this job's process?"
                          :hx-target "#job-list" :hx-swap "innerHTML"
                          "kill")))))

(defun job-rows-html ()
  (let ((jobs (job-list-newest-first)))
    (if (null jobs)
        (list (list :tr (list :td :colspan "6" :class "muted"
                              "No jobs yet. Running one boots a headless autolith child agent.")))
        (loop for job in jobs
              collect (job-row-html job)))))

(defun error-row (message)
  (list (list :tr (list :td :colspan "6" :class "error" message))))

(defun panel-agent-html ()
  (list
   (list :h2 "Agent jobs")
   (list :p :class "muted"
         "Each submission runs one contracted headless child-agent job via `autolith run-job`. The result contract is a plain string.")
   (list :form :class "card stack"
         :hx-post "/agent/run"
         :hx-encoding "multipart/form-data"
         :hx-target "#job-list" :hx-swap "innerHTML"
         (list :label "Task")
         (list :textarea :name "task" :required "required"
               :placeholder "Describe the job for the child agent...")
         (list :div :class "row"
               (list :label "Role"
                     (list :select :name "role"
                           (list :option :value "task" "task")
                           (list :option :value "scout" "scout")
                           (list :option :value "designer" "designer")
                           (list :option :value "reviewer" "reviewer")
                           (list :option :value "librarian" "librarian")))
               (list :label "Permissions"
                     (list :select :name "permissions"
                           (list :option :value "sandbox" :selected "selected" "sandbox")
                           (list :option :value "auto" "auto")
                           (list :option :value "full" "full")))
               (list :label "Timeout (s)"
                     (list :input :type "number" :name "timeout" :value "900"
                           :min "30" :max "86400"))
               (list :label "Attachment (optional)"
                     (list :input :type "file" :name "file")))
         (list :div :class "row"
               (list :label "Workspace"
                     (list :input :type "text" :name "workspace"
                           :value (default-workspace)))
               (list :span :class "grow")
               (list :button :type "submit" "Run job"
                     (list :span :class "htmx-indicator spinner"))))
   (list :p :class "hint muted"
         "Attachments upload via hx-encoding=\"multipart/form-data\", land in the uploads directory, and their absolute paths are passed to the job.")
   (list :table :class "jobs"
         (list :thead (list :tr (list :th "id") (list :th "status") (list :th "job")
                                    (list :th "result") (list :th "usage") (list :th "")))
         (list :tbody :id "job-list"
               :hx-get "/agent/jobs" :hx-trigger "every 3s" :hx-swap "innerHTML"
               (job-rows-html)))))

;;; --- repl ---

(defun repl-out-html ()
  (let ((history (repl-history)))
    (if (null history)
        (list :p :class "muted" "No evaluations yet.")
        (loop for entry in history
              collect (list :div :class "eval"
                            (list :div :class "eval-head"
                                  (list :span :class (if (getf entry :ok-p)
                                                         "badge st-succeeded"
                                                         "badge st-failed")
                                        (if (getf entry :ok-p) "ok" "error"))
                                  (list :span :class "muted mono"
                                        (format-ago (getf entry :time))))
                            (list :pre (getf entry :input))
                            (list :pre :class "out" (getf entry :output)))))))

(defun panel-repl-html ()
  (list
   (list :h2 "REPL")
   (list :p :class "muted"
         "Evaluates in this server's Lisp image, package WEBUI-USER (uses COMMON-LISP and UIOP). *READ-EVAL* disabled, 20s timeout, output captured.")
   (list :form :class "card stack"
         :hx-post "/repl" :hx-target "#repl-out" :hx-swap "innerHTML"
         (list :textarea :name "expr" :required "required" :spellcheck "false"
               :class "mono" :placeholder "(+ 1 2)")
         (list :div :class "row"
               (list :button :type "submit" "Evaluate"
                     (list :span :class "htmx-indicator spinner"))
               (list :button :type "button"
                     :hx-post "/repl/reset" :hx-target "#repl-out" :hx-swap "innerHTML"
                     :hx-confirm "Reset the WEBUI-USER package (drops all definitions)?"
                     "Reset package")))
   (list :div :id "repl-out" (repl-out-html))))

;;; --- files ---

(defun file-rows-html ()
  (let ((directory (uploads-directory)))
    (let ((files (when (uiop:directory-exists-p directory)
                   (sort (list-files directory)
                         (lambda (a b)
                           (> (or (file-write-date a) 0)
                              (or (file-write-date b) 0)))))))
      (if (null files)
          (list (list :tr (list :td :colspan "4" :class "muted" "No uploads yet.")))
          (loop for path in files
                for name = (file-namestring path)
                collect (list :tr
                              (list :td (list :a :href (format nil "/files/download?name=~A" name)
                                              name))
                              (list :td :class "mono muted" (format-size (file-size path)))
                              (list :td :class "mono muted"
                                    (format-ago (file-write-date path)))
                              (list :td
                                    (list :button :class "danger"
                                          :hx-post "/files/delete"
                                          :hx-vals (format nil "{\"name\":\"~A\"}" name)
                                          :hx-confirm (format nil "Delete ~A?" name)
                                          :hx-target "#file-table" :hx-swap "innerHTML"
                                          "delete"))))))))

(defun panel-files-html ()
  (list
   (list :h2 "Files")
   (list :p :class "muted"
         (format nil "Uploads land in ~A (hx-encoding=\"multipart/form-data\")."
                 (uploads-directory)))
   (list :form :class "card row"
         :hx-post "/files/upload"
         :hx-encoding "multipart/form-data"
         :hx-target "#file-table" :hx-swap "innerHTML"
         (list :input :type "file" :name "file" :multiple "multiple")
         (list :button :type "submit" "Upload"
               (list :span :class "htmx-indicator spinner")))
   (list :table
         (list :thead (list :tr (list :th "name") (list :th "size")
                                    (list :th "uploaded") (list :th "")))
         (list :tbody :id "file-table" (file-rows-html)))))

;;; --- autolith state ---

(defun agenda-item-row (item)
  (list :tr
        (list :td (status-badge (state-field item :status :todo)))
        (list :td (or (state-field item :text) ""))
        (list :td :class "mono muted"
              (format-ago (state-field item :updated-at)))))

(defun agenda-items-html (items)
  (if (null items)
      (list :p :class "muted" "empty")
      (list :table
            (list :thead
                  (list :tr (list :th "status") (list :th "item") (list :th "updated")))
            (cons :tbody (mapcar #'agenda-item-row items)))))

(defun agendas-html ()
  (let ((records (agenda-records)))
    (if (null records)
        (list :p :class "muted" "No agenda state found.")
        (loop for record in records
              collect (list :div :class "agenda"
                            (list :h4 :class "mono"
                                    (state-string record :directory "?"))
                            (agenda-items-html (state-field record :items)))))))

(defun memories-html ()
  (let ((records (memory-records)))
    (if (null records)
        (list :p :class "muted" "No memories found.")
        (loop for record in records
              collect (list :details :class "memory"
                            (list :summary
                                  (list :span :class "badge"
                                        (string-downcase
                                         (princ-to-string
                                          (state-field record :scope :workspace))))
                                    (format nil " ~A " (state-string record :title
                                                                      "(untitled)"))
                                    (list :span :class "muted mono"
                                          (format nil "~A · ~A"
                                                  (basename (state-string record :workspace ""))
                                                  (format-ago (state-field record :updated-at)))))
                            (list :div :class "muted mono"
                                  (let ((tags (state-field record :tags (list))))
                                    (if (listp tags)
                                        (format nil "~{#~A~^ ~}" tags)
                                        "")))
                            (list :pre (last-chars (state-string record :content "")
                                                   2000)))))))

(defun papercut-status (record)
  (cond ((state-field record :closed-at) "closed")
        ((state-field record :resolution) "closed")
        ((state-field record :assessments) "assessed")
        (t "open")))

(defun papercuts-html ()
  (let ((records (papercut-records)))
    (if (null records)
        (list :p :class "muted" "No papercuts reported.")
        (loop for record in records
              collect (list :details :class "papercut"
                            (list :summary
                                  (status-badge (papercut-status record))
                                    (format nil " ~A " (state-string record :title
                                                                      "(untitled)"))
                                    (list :span :class "muted mono"
                                          (format nil "~A · reported ~A"
                                                  (basename (state-string record :workspace ""))
                                                  (format-ago (state-field record :reported-at)))))
                            (list :pre (last-chars (state-string record :content "")
                                                   2000))
                            (when (state-field record :resolution)
                              (list :div (list :span :class "muted" "resolution:")
                                    (list :pre (state-field record :resolution)))))))))

(defun state-body-html ()
  (list
   (list :div :class "card"
         (list :h3 "Agendas")
         (agendas-html))
   (list :div :class "card"
         (list :h3 "Memories")
         (memories-html))
   (list :div :class "card"
         (list :h3 "Papercuts")
         (papercuts-html))))

(defun panel-state-html ()
  (list
   (list :h2 "Autolith state")
   (list :p :class "muted"
         "Read-only view of the durable state under the Autolith data directory. Refreshes every 20s.")
   (list :div :id "state-body"
         :hx-get "/state/body" :hx-trigger "every 20s"
         (state-body-html))))

;;; --- detached sessions ---

(defun sessions-body-html ()
  (let* ((text (localgroup-status-text))
         (rows (parse-sessions text)))
    (list
     (if (null rows)
         (list :p :class "muted" "No detached sessions (or status unavailable).")
         (cons :table
               (list (list :thead (list :tr (list :th "session") (list :th "state")
                                            (list :th "started") (list :th "conversation")
                                            (list :th "workspace") (list :th "actions")))
                     (cons :tbody
                           (loop for row in rows
                                 collect (list :tr
                                               (list :td :class "mono" (session-row-id row))
                                               (list :td (list :span
                                                               :class (if (string= (session-row-state row) "active")
                                                                          "badge st-running"
                                                                          "badge st-queued")
                                                               (session-row-state row)))
                                               (list :td :class "mono muted" (session-row-started row))
                                               (list :td (excerpt (session-row-conversation row) 60))
                                               (list :td :class "muted" (excerpt (session-row-workspace row) 40))
                                               (list :td :class "actions"
                                                     (list :button
                                                           :hx-post "/sessions/action"
                                                           :hx-vals (format nil "{\"session\":\"~A\",\"action\":\"pause\"}"
                                                                            (session-row-id row))
                                                           :hx-target "#sessions-body" :hx-swap "innerHTML"
                                                           "pause")
                                                     (list :button :class "danger"
                                                           :hx-post "/sessions/action"
                                                           :hx-vals (format nil "{\"session\":\"~A\",\"action\":\"kill\"}"
                                                                            (session-row-id row))
                                                           :hx-confirm (format nil "Kill detached session ~A?" (session-row-id row))
                                                           :hx-target "#sessions-body" :hx-swap "innerHTML"
                                                           "kill"))))))))
     (list :details (list :summary "raw status output")
           (list :pre text)))))

(defun panel-sessions-html ()
  (list
   (list :h2 "Detached sessions")
   (list :p :class "muted"
         "Inspect and control detached Autolith sessions (`autolith localgroup`). Refreshes every 12s.")
   (list :div :id "sessions-body"
         :hx-get "/sessions/body" :hx-trigger "every 12s"
         (sessions-body-html))
   (list :form :class "card stack"
         :hx-post "/sessions/tell" :hx-target "#sessions-note" :hx-swap "innerHTML"
         (list :div :class "row"
               (list :label "Session"
                     (list :input :name "session" :required "required"
                           :placeholder "e.g. X-54EfaE"))
               (list :label "Message"
                     (list :input :name "message" :required "required"
                           :placeholder "one message to queue")))
         (list :button :type "submit" "Queue message"
               (list :span :class "htmx-indicator spinner")))
   (list :div :id "sessions-note" :class "note")))

;;; --- page shell ---

(defparameter +shell-script+
  "document.body.addEventListener('htmx:afterOnLoad', function (evt) {
  var path = evt.detail && evt.detail.pathInfo && evt.detail.pathInfo.requestPath;
  if (!path) return;
  document.querySelectorAll('nav button').forEach(function (b) {
    b.classList.toggle('active', b.getAttribute('hx-get') === path);
  });
});
document.body.addEventListener('htmx:afterRequest', function (evt) {
  var elt = evt.detail && evt.detail.elt;
  if (evt.detail.successful && elt && elt.tagName === 'FORM' && elt.reset) elt.reset();
});")

(defun nav-tabs (active)
  (loop for (key label) in
        '((:overview "overview") (:agent "agent") (:repl "repl")
          (:files "files") (:state "state") (:sessions "sessions"))
        collect (list :button
                      :class (if (eq key active) "tab active" "tab")
                      :hx-get (format nil "/panel/~(~A~)" key)
                      :hx-target "#panel"
                      :hx-push-url "true"
                      label)))

(defun page-shell (&key (initial :overview) (initial-html (panel-overview-html)))
  (render
   (list :raw "<!doctype html>"
         (list :html
               (list :head
                     (list :meta :charset "utf-8")
                     (list :meta :name "viewport" :content "width=device-width, initial-scale=1")
                     (list :title "Autolith Console")
                     (list :link :rel "icon" :href "/static/favicon.svg")
                     (list :link :rel "stylesheet" :href "/static/style.css")
                     (list :script :src "/static/htmx.min.js" :defer "defer"))
               (list :body
                     (list :header
                           (list :div :class "brand" "autolith"
                                 (list :span :class "accent" "::") " console")
                           (list :nav (nav-tabs initial))
                           (list :div :class "meta muted mono" (server-meta-text)))
                     (list :main :id "panel" initial-html)
                     (list :footer
                           (list :span :class "muted"
                                 "native Common Lisp console for Autolith · htmx + hx multipart · no web frameworks"))
                     (list :script (list :raw +shell-script+)))))))

(defun panel-key (name)
  (intern (string-upcase name) :keyword))

(defun panel-html (key)
  (case key
    (:overview (panel-overview-html))
    (:agent (panel-agent-html))
    (:repl (panel-repl-html))
    (:files (panel-files-html))
    (:state (panel-state-html))
    (:sessions (panel-sessions-html))
    (t (list :p :class "muted" "Unknown panel."))))

;;; --- route installation ---

(defun install-routes ()
  (defroute :get "^/$" (request)
    (declare (ignore request))
    (html-response (page-shell :initial :overview)))

  (defroute :get "^/panel/(overview|agent|repl|files|state|sessions)$" (request panel)
    (let* ((key (panel-key panel))
           (html (panel-html key)))
      (if (header-value request "hx-request")
          (html-response (render html))
          (html-response (page-shell :initial key :initial-html html)))))

  (defroute :post "^/agent/run$" (request)
    (let ((task (param request "task"))
          (role (param request "role"))
          (permissions (param request "permissions"))
          (timeout (parse-integer-or (param request "timeout") 900))
          (workspace (param request "workspace")))
      (cond
        ((blank-p task)
         (html-response (render (error-row "Task text is required."))))
        ((not (member permissions '("sandbox" "auto" "full") :test #'string=))
         (html-response (render (error-row "Unknown permissions mode."))))
        ((and workspace (not (uiop:directory-exists-p workspace)))
         (html-response (render (error-row
                                 (format nil "Workspace directory does not exist: ~A" workspace)))))
        (t
         (let ((paths (save-uploads (req-files request))))
           (submit-job task
                       :role (or role "task")
                       :permissions (or permissions "sandbox")
                       :timeout-seconds timeout
                       :attachments paths
                       :workspace workspace)
           (html-response (render (job-rows-html))))))))

  (defroute :get "^/agent/jobs$" (request)
    (declare (ignore request))
    (html-response (render (job-rows-html))))

  (defroute :post "^/agent/kill$" (request)
    (kill-job (param request "id"))
    (html-response (render (job-rows-html))))

  (defroute :post "^/repl$" (request)
    (let ((expr (param request "expr")))
      (repl-eval (if (blank-p expr) "nil" expr))
      (html-response (render (repl-out-html)))))

  (defroute :post "^/repl/reset$" (request)
    (declare (ignore request))
    (repl-reset)
    (html-response (render (repl-out-html))))

  (defroute :post "^/files/upload$" (request)
    (if (null (req-files request))
        (html-response (render (list (list :tr (list :td :colspan "4" :class "error"
                                                      "No file part in request.")))))
        (progn
          (save-uploads (req-files request))
          (html-response (render (file-rows-html))))))

  (defroute :get "^/files/download$" (request)
    (let* ((name (safe-filename (param request "name")))
           (path (and name (uiop:merge-pathnames* name (uploads-directory)))))
      (cond
        ((not name) (error-response 400 "Missing file name."))
        ((not (uiop:file-exists-p path)) (error-response 404 "No such file."))
        (t (let ((content-type (static-content-type name)))
               (octets-response (read-file-octets path) content-type
                                :filename name
                                :inline-p (or (ppcre:scan "^text/" content-type)
                                              (search "svg" content-type))))))))

  (defroute :post "^/files/delete$" (request)
    (let ((name (safe-filename (param request "name"))))
      (when name
        (ignore-errors
         (delete-file (uiop:merge-pathnames* name (uploads-directory)))))
      (html-response (render (file-rows-html)))))

  (defroute :get "^/state/body$" (request)
    (declare (ignore request))
    (html-response (render (state-body-html))))

  (defroute :get "^/sessions/body$" (request)
    (declare (ignore request))
    (html-response (render (sessions-body-html))))

  (defroute :post "^/sessions/tell$" (request)
    (let* ((session (param request "session"))
           (message (param request "message")))
      (if (or (blank-p session) (blank-p message))
          (html-response (render (list :span :class "note"
                                       "Session id and message are required.")))
          (multiple-value-bind (out err code)
              (localgroup-tell (trim session) (trim message))
            (declare (ignore out))
            (html-response (render
                            (list :span :class (if (eql code 0) "note" "error")
                                  (if (eql code 0)
                                      (format nil "Message queued for ~A." (trim session))
                                      (format nil "tell failed: ~A" (trim err))))))))))

  (defroute :post "^/sessions/action$" (request)
    (let* ((session (param request "session"))
           (action (param request "action")))
      (when (and session action (not (blank-p session)))
        (cond ((string= action "pause") (localgroup-pause (trim session)))
              ((string= action "kill") (localgroup-kill (trim session)))))
      (html-response (render (sessions-body-html)))))

  (defroute :get "^/static/([A-Za-z0-9._-]+)$" (request name)
    (declare (ignore request))
    (serve-static name))

  (defroute :get "^/favicon\\.ico$" (request)
    (declare (ignore request))
    (serve-static "favicon.svg"))

  (values))
