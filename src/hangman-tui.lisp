(in-package "COM.INFORMATIMAGO.HANGMAN.COCOA")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun configure ()
    (setf (logical-pathname-translations "RESOURCES")
          (list (list "RESOURCES:WORDS"        #P"/usr/share/dict/words")
                (list "RESOURCES:STATES"
                      #+DEBUGGING #P"~/src/public/games/hangman-cocoa-lisp/src/hangman.states"
                      #-DEBUGGING #P"/usr/local/games/hangman/hangman.sexp")
                (list "RESOURCES:**;*.*"       #P"/usr/local/games/hangman/**/*.*"))))
  (configure))



(defparameter *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun satisfactory-word-p (word &optional (alphabet *alphabet*))
  (and (< 2 (length word))
       (every (lambda (letter) (find letter alphabet :test (function char-equal)))
              word)))

(defun load-words ()
  (remove-if-not (function satisfactory-word-p)
                 (string-list-text-file-contents #P"RESOURCES:WORDS")))
(defun choose-random-word (words)
  (elt words (random (length words))))

(defun play ()
  (configure)
  (let* ((words           (load-words))
         (states          (sexp-file-contents #P"RESOURCES:STATES"))
         (max-error-count (1- (length states))))
    (loop
      :for game := (make-hangman (choose-random-word words) max-error-count)
      :do (loop
            :do (format t "~A~%~A~%"
                        (elt states (hangman-error-count game))
                        (hangman-found-word game))
                (finish-output)
                (when (<= max-error-count (hangman-error-count game))
                  (format t "~%You're hung!~%")
                  (finish-output)
                  (loop-finish))
                (format *query-io* "Choose a letter: ")
                (let ((letter (ignore-errors (aref (string-trim " " (read-line *query-io*)) 0))))
                  (when letter
                    (let ((try (hangman-try-letter game letter)))
                      (format t "~&~A~%"
                              (case try
                                (:good-guess "Yay!")
                                (:already-tried "You already tried this letter!")
                                (:bad-guess "Aiaiai!")
                                (:wins "Pffuit!")))
                      (when (eq try :wins)
                        (format t "~%~A~%You're saved!~%" (hangman-found-word game))
                        (finish-output)
                        (loop-finish))))))
      :while (y-or-n-p "Again? "))))



;;;; THE END ;;;;
