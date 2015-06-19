;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This script generates the Hangman application on CCL on MacOSX.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-19 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(defparameter *program-name*      "Hangman")
(defparameter *release-directory* #P"~/Desktop/")
(defparameter *version*           "1.0.0")
(defparameter *copyright*
  "Copyright 2015 Pascal Bourguignon
License: AGPL3")

(push (or *load-pathname* #P"./") asdf:*central-registry*)
(ql:quickload :com.informatimago.common-lisp.cesarum)
(ql:quickload :com.informatimago.hangman)
(push (function com.informatimago.hangman.cocoa:main) ccl:*lisp-startup-functions*)

;;; --------------------------------------------------------------------
;;; Save the application package.

#+ccl
(require :build-application)

(format t "Generating ~A~%" (merge-pathnames (make-pathname :name *program-name*
                                                            :type "app")
                                             *release-directory*))


(let ((dest-dir (merge-pathnames (make-pathname :directory (list :relative
                                                                 (format nil "~A.app" *program-name*)
                                                                 "Contents" "Resources" "images")
                                                :name "test" :type "png")
                                 *release-directory*))
      (images   (make-pathname :directory (list :relative :up "images") :name :wild :type "png")))
  (ensure-directories-exist dest-dir)
  (dolist (image (directory images))
    (let ((dest-file (make-pathname :name (pathname-name image) :type (pathname-type image)
                                    :defaults dest-dir)))
      (format t "Copying ~A~%" dest-file)
      (com.informatimago.common-lisp.cesarum.file:copy-file image dest-file
                                                            :element-type '(unsigned-byte 8)
                                                            :if-exists :supersede))))



(defun save-simple-application (&key (name *program-name*) (directory *release-directory*) (creator-string "????"))
  ;; ccl::build-application
  ;;  calls ccl::save-application
  ;;  calls ccl::%save-application-interal
  ;;  calls ccl::save-image
  #+ccl
  (ccl::build-application               ; This doesn't return.
   :name name
   :directory directory
   :type-string "APPL"
   :creator-string creator-string
   :copy-ide-resources nil              ; whether to copy the IDE's resources
   :info-plist nil                      ; optional user-defined info-plist
   :nibfiles '()
                                        ; a list of user-specified nibfiles
                                        ; to be copied into the app bundle
   :main-nib-name "MainMenu"
                                        ; the name of the nib that is to be loaded
                                        ; as the app's main. this name gets written
                                        ; into the Info.plist on the "NSMainNibFile" key
   :private-frameworks '()
   :toplevel-function nil
   :altconsole nil))                    ; use t for a console for *standard-output*, *error-output*.


(save-simple-application :name *program-name*
                         :directory *release-directory*
                         :creator-string "SOSH")


;;;; THE END ;;;;
