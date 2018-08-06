;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.hangman.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Hangman Cocoa Application
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-17 <PJB> Created.
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

(asdf:defsystem "com.informatimago.hangman.tui"
  :description "A Hangman game with a Text User Interface."
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum")
  :components (      (:file "packages"             :depends-on ())
                     (:file "hangman"              :depends-on ("packages"))
                     (:file "hangman-tui"          :depends-on ("packages" "hangman")))

  ;; :in-order-to ((asdf:test-op (asdf:test-op "com.informatimago.editor.test")))
  )

;;;; THE END ;;;;

