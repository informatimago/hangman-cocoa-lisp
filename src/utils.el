
(defun pjb-camel-to-lisp (name &optional capitalize)
  (mapconcat (let ((capitalize capitalize))
               (lambda (token)
                 (if capitalize
                     (progn
                       (capitalize token)
                       (setf capitalize nil))
                     (downcase token))))
             (jde-split-by-camel-notation
              (etypecase name
                (string name)
                (symbol (symbol-name name))))
             "-"))

(defun gen-key-arg (name)
  (let* ((name (etypecase name
                 (string name)
                 (symbol (symbol-name name))))
         (parname  (intern (pjb-camel-to-lisp name)))
         (parnamep (intern (format "%s-p" parname))))
    `(,parname nil ,parnamep)))

(defun gen-set-arg (name)
  (let* ((name (etypecase name
                 (string name)
                 (symbol (symbol-name name))))
         (parname  (intern (pjb-camel-to-lisp name)))
         (parnamep (intern (format "%s-p" parname))))
    `(when ,parnamep
       [self ,(intern (format "set%c%s:"
                              (capitalize (aref name 0))
                              (subseq name 1)))
             ,parname])))

(defun gen-key-args-and-set-msgs (names)
  (let* ((args (mapcar (function gen-key-arg) names))
         (sets (mapcar (function gen-set-arg) names)))
    (insert "(&key\n")
    (dolist (arg args)
      (insert (format "%S\n" arg)))
    (insert "&allow-other-keys)\n")
    (insert "(when (next-method-p) (call-next-method))\n")
    (dolist (set sets)
      (insert (format "%S\n" set)))
    (insert "self")))

