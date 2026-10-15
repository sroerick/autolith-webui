;;;; html.lisp --- a tiny S-expression to HTML emitter with default escaping.

(in-package #:autolith-webui)

(defparameter +void-elements+
  '(:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr))

(defun tag-name (tag)
  (string-downcase (symbol-name tag)))

(defun split-attrs (items)
  "Split a mixed attribute/child list into (values attrs children).
Attributes are keyword/value pairs at the front of ITEMS."
  (let ((attrs '())
        (rest items))
    (loop while (and (consp rest)
                     (keywordp (first rest))
                     (consp (rest rest)))
          do (push (cons (first rest) (second rest)) attrs)
             (setf rest (cddr rest)))
    (values (nreverse attrs) rest)))

(defun render-attr (stream name value)
  (cond ((null value))
        ((eq value t)
         (format stream " ~A" (tag-name name)))
        (t
         (format stream " ~A=\"~A\"" (tag-name name) (esc (princ-to-string value))))))

(defun render-node (stream node)
  (typecase node
    (null nil)
    (string (write-string (esc node) stream))
    (number (princ node stream))
    (symbol nil)
    (list
     (let ((head (first node)))
       (cond
          ((eq head :raw)
           (write-string (or (second node) "") stream)
           (dolist (child (cddr node))
             (render-node stream child)))
         ((keywordp head)
          (multiple-value-bind (attrs children) (split-attrs (rest node))
            (format stream "<~A" (tag-name head))
            (dolist (attr attrs)
              (render-attr stream (car attr) (cdr attr)))
            (if (member head +void-elements+ :test #'eq)
                (write-char #\> stream)
                (progn
                  (write-char #\> stream)
                  (dolist (child children)
                    (render-node stream child))
                  (format stream "</~A>" (tag-name head))))))
         (t
          (dolist (child node)
            (render-node stream child))))))))

(defun render (node)
  "Render a nested S-expression into an HTML string.

Strings and numbers are escaped. Tags are keyword heads followed by
optional keyword/value attribute pairs and then children. (:raw STRING)
splices pre-rendered HTML. A plain list of nodes renders each node."
  (with-output-to-string (stream)
    (render-node stream node)))
