;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-cli.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This script generates the Hangman CLI with CCL.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-08-06 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2018
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "COMMON-LISP-USER")

;;; --------------------------------------------------------------------
;;; Load quicklisp
(format t "~%;;; Loading quicklisp.~%")
(finish-output)
(load #P"~/quicklisp/setup.lisp")
(setf quicklisp-client:*quickload-verbose* t)

;;; --------------------------------------------------------------------
;;; Load the application:

(defparameter *program-name*      "hangman")
(defparameter *release-directory* #P"/usr/local/bin/")
(defparameter *program-pathname*  (merge-pathnames *program-name* *release-directory*))
(defparameter *version*           "1.0.1")
(defparameter *copyright*
  "Copyright 2018 Pascal Bourguignon
License: AGPL3")

(push (or *load-pathname* #P"./") asdf:*central-registry*)
(ql:quickload :com.informatimago.common-lisp.cesarum)
(ql:quickload :com.informatimago.hangman.tui)
;; (push (function com.informatimago.hangman.cocoa:play) ccl:*lisp-startup-functions*)

;;; --------------------------------------------------------------------
;;; Save the application package.

(format t "Generating ~A~%" *program-pathname*)

(shadow 'copy-file)
(defun copy-file (source destination)
  (ensure-directories-exist destination)
  (format t "Copying ~A~%" destination)
  (com.informatimago.common-lisp.cesarum.file:copy-file source destination
                                                        :element-type '(unsigned-byte 8)
                                                        :if-exists :supersede))

(copy-file #P"~/src/public/games/hangman-cocoa-lisp/src/hangman.states"
           #P"/usr/local/games/hangman/hangman.sexp")


(defun save-hangman-program ()
  ;; ccl::save-application
  ;;  calls ccl::%save-application-interal
  ;;  calls ccl::save-image
  #+ccl
  (ccl::save-application               ; This doesn't return.
   *program-pathname*
   :toplevel-function (function com.informatimago.hangman.cocoa::play)
   :init-file nil
   :error-handler :quit
   :purify t
   :mode #o755
   :prepend-kernel t))


(save-hangman-program)


;;;; THE END ;;;;
