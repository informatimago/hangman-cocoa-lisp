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
(defparameter *version*           "1.0.1")
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


(defun copy-files (files dest-dir)
  (ensure-directories-exist (make-pathname :name "test" :type "test" :defaults dest-dir))
  (dolist (file (directory files))
    (let ((dest-file (make-pathname :name (pathname-name file) :type (pathname-type file)
                                    :defaults dest-dir)))
      (format t "Copying ~A~%" dest-file)
      (com.informatimago.common-lisp.cesarum.file:copy-file file dest-file
                                                            :element-type '(unsigned-byte 8)
                                                            :if-exists :supersede))))


(let ((resources (merge-pathnames (make-pathname :directory (list :relative
                                                                  (format nil "~A.app" *program-name*)
                                                                  "Contents" "Resources"))
                                  *release-directory*)))
  
  (copy-files (make-pathname :directory (list :relative :up "Resources" "en.lproj") :name :wild :type :wild)
              (merge-pathnames #P"en.lproj/" resources))

  (copy-files (make-pathname :directory (list :relative :up "Resources" "images") :name :wild :type "png")
              (merge-pathnames #P"images/" resources))

  (copy-files (make-pathname :directory (list :relative :up "Resources") :name "AppIcon" :type "icns")
              resources))




(defun save-hangman-application ()
  ;; ccl::build-application
  ;;  calls ccl::save-application
  ;;  calls ccl::%save-application-interal
  ;;  calls ccl::save-image
  #+ccl
  (ccl::build-application               ; This doesn't return.
   :name *program-name*
   :directory *release-directory*
   :type-string "APPL"
   :creator-string "SOSH"
   :copy-ide-resources nil              ; whether to copy the IDE's resources
   :info-plist (com.informatimago.hangman.cocoa::dictionary
                :|LSApplicationCategoryType| "public.app-category.word-games"
                :|CFBundleIconFile| "AppIcon.icns"
                :|CFBundleIdentifier| "com.informatimago.hangman.lisp"
                :|CFBundleShortVersionString|  (format nil "~A" *version*)
                :|CFBundleVersion| (format nil "~A ~A" "ccl" (lisp-implementation-version))
                :|LSMinimumSystemVersion| "10.7"
                :|CFBundleDevelopmentRegion| "English"
                :|NSHumanReadableCopyright| (format nil "Copyright 2015 Pascal Bourguignon~%License: AGPL3")
                ;; :|CFBundleHelpBookFolder| "Resources"
                ;; :|CFBundleHelpBookName| "HangmanHelp"
                ;; :|NSAppleScriptEnabled| nil ; not yet.
                ;; :|CFBundleDocumentTypes| (cf-bundle-document-types)
                ;; :|UTExportedTypeDeclarations| (exported-type-utis)
                ;; (dictionary-version $default-info-dictionary-version)
                ;; (development-region $default-info-plist-development-region)
                ;; (executable $default-info-plist-executable)
                ;; (has-localized-display-name $default-info-plist-has-localized-display-name)
                ;; overriden by write-info-plist, I assume. :|NSMainNibFile| "MainMenu"
                ;; overriden by write-info-plist (bundle-name $default-info-plist-bundle-name)
                ;; overriden by write-info-plist (bundle-package-type $default-info-plist-bundle-package-type)
                ;; overriden by write-info-plist (bundle-signature $default-info-plist-bundle-signature)
                :|NSPrincipalClass| "LispApplication")
   :nibfiles '()                        ; a list of user-specified nibfiles
                                        ; to be copied into the app bundle
   ;; :main-nib-name "MainMenu"
   ;;                                      ; the name of the nib that is to be loaded
   ;;                                      ; as the app's main. this name gets written
   ;;                                      ; into the Info.plist on the "NSMainNibFile" key
   :private-frameworks '()
   :toplevel-function nil
   :altconsole t))                    ; use t for a console for *standard-output*, *error-output*.


(save-hangman-application)


;;;; THE END ;;;;
