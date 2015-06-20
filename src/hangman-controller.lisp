;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               hangman-controller.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Hangman Controller.
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
(in-package "COM.INFORMATIMAGO.HANGMAN.COCOA")
(objcl:set-objective-cl-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  @[NSResponder subClass:HangmanController
                slots: ((window       :initarg :window       :accessor controller-window)
                        (image        :initarg :image        :accessor controller-image
                                      :documentation "A NSImageView containing the gallows.")
                        (letters      :initarg :letters      :accessor controller-letters
                                      :documentation "A NSView (or NSMatrix) containing the letter buttons.")
                        (message      :initarg :message      :accessor controller-message
                                      :documentation "A NSTextField")
                        (guessed      :initarg :message      :accessor controller-guessed
                                      :documentation "A NSTextField")
                        (texts        :initarg :texts        :accessor controller-texts
                                      :initform '())
                        (buttons      :initarg :buttons      :accessor controller-buttons
                                      :initform '())
                        (hangman      :initarg :hangman      :accessor controller-hangman)
                        (words        :initarg :words        :accessor controller-words)
                        (loaded-words :initarg :loaded-words :accessor controller-loaded-words)
                        (finished     :initarg :finished     :accessor controller-finished)
                        (game-counter :initform 0 :accessor controller-game-counter)
                        (wins-counter :initform 0 :accessor controller-wins-counter))])




@[HangmanController
  method:(init)
  resultType:(:id)
  body:
  (unless (oclo:nullp (setf self [super init]))
    (setf (controller-words self)
          #("along" "and" "andor" "any" "buffer" "but" "can"
            "copy" "create" "details" "distributed" "either" "enter"
            "evaluation" "even" "file" "fitness" "for" "foundation"
            "free" "general" "gnu" "have" "hope" "implied" "later"
            "license" "lisp" "merchantability" "modify" "more" "not"
            "notes" "option" "own" "particular" "program" "public"
            "published" "purpose" "received" "redistribute" "save"
            "see" "should" "software" "terms" "text" "that" "the"
            "then" "this" "under" "useful" "version" "visit" "want"
            "warranty" "will" "with" "without" "you" "your")
          (controller-loaded-words self) (load-words)))
  self]

;; Create the UI:

(defun make-letter-button (frame letter target &rest keys &key &allow-other-keys)
  (apply (function make-button) frame :title letter
                                      :target target
                                      :action (oclo:@selector "selectLetter:")
                                      :button-type :momentary-push-in
                                      :bezel-style :small-square
                                      keys))

(defvar *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun make-letter-buttons (frame target &key (rows 2) (alphabet *alphabet*))
  (let* ((columns (ceiling (length alphabet) rows))
         (butw    (truncate (- (rect-width frame)  (* 3 (1- columns))) columns))
         (buth    (truncate (- (rect-height frame) (* 3 (1- rows)))    rows))
         (view    (make-view frame)))
    (loop
      :with a = 0
      :for r :below rows
      :for y = (- (rect-height frame) buth) :then (- y buth 3)
      :while (< a (length alphabet))
      :do (loop :for c :below columns
                :for x = 0 :then (+ x butw 3)
                :for l = (aref alphabet a)
                :do (incf a)
                    (make-letter-button (list x y butw buth) (string l) target
                                        :superview view)))
    view))


@[HangmanController
  method:(createUI)
  resultType:(:void)
  body:
  (setf (controller-window self)  (make-window '(20 20 472 246) '(:titled :miniaturizable)
                                               :buffered t
                                               ;; &keys:
                                               :one-shot t
                                               :retained no
                                               :title "hangman"
                                               :movable t
                                               :movable-by-window-background t
                                               :has-shadow t
                                               :delegate self
                                               :initial-first-responder self
                                               :next-responder [NSApplication sharedApplication]
                                               :autodisplay t))
  [(controller-window self) makeFirstResponder: self]
  (let ((win [(controller-window self) contentView]))
    (setf (controller-image self)   (make-image-view '(180 158 112 71)
                                                     :image-scaling :proportionally-down
                                                     :image-alignment :center
                                                     :image-frame-style :gray-bezel
                                                     :animages nil :editable nil
                                                     :refuses-first-responder t
                                                     :superview win)
          (controller-message self) (make-text-field '(309 161 135 65)
                                                     :string-value "Play hang man!"
                                                     :editable nil :selectable nil :bordered nil :bezeled nil
                                                     :draws-background nil :background-color [NSColor clearColor]
                                                     :superview win)
          (controller-guessed self) (make-text-field '(21 112 430 41)
                                                     :string-value "word"
                                                     :editable nil :selectable nil :bordered nil :bezeled nil
                                                     :draws-background nil :background-color [NSColor clearColor]
                                                     :alignment :center
                                                     :font [NSFont systemFontOfSize: (cgfloat 19)]
                                                     :superview win)
          (controller-letters self) (make-letter-buttons '(129 59 312 52) self))
    [win addSubview:(controller-letters self)]
    (push (make-text-field '(21 87 94 20)
                           :string-value "Select a letter:"
                           :editable nil :selectable nil :bordered nil :bezeled nil
                           :draws-background nil :background-color [NSColor clearColor]
                           :superview win)
          (controller-texts self))
    (push (make-button '(188 11 107 32)
                       :title "New Game ⏎"
                       :action (oclo:@selector "newGame:")
                       :target self
                       :bezel-style :rounded
                       :type :momentary-push
                       :superview win)
          (controller-buttons self)))
  [(controller-window self) makeKeyAndOrderFront:self]
  [[NSApplication sharedApplication] setDelegate:self]]


@[HangmanController
  method: (dropUI)
  resultType: (:void)
  body:
  [[NSApplication sharedApplication] setDelegate:nil]
  (when (controller-window self)
    [(controller-window self) close]
    (setf (controller-window self) nil
          (controller-image self) nil
          (controller-message self) nil
          (controller-guessed self) nil
          (controller-texts self) '()
          (controller-buttons self) '()))]



(defun make-menu-bar ()
  (menu "MenuBar"
    (menu "Hangman"
      (item "About Hangman" "orderFrontStandardAboutPanel:")
      -
      (menu "Services")
      -
      (item "Hide Hangman" "hide:" "h")
      (item "Hide Others" "hideOtherApplications:" (:command :option "h"))
      (item "Show All" "unhideAllApplications:")
      -
      (item "Quit Hangman" "terminate:" "q"))
    (menu "Window"
      (item "Minimize" "performMiniaturize:" "m")
      (item "Zoom"     "performZoom:")
      -
      (item "Bring All to Front" "arrangeInFront:"))
    (menu "Help"
      (item "Hangman Help" "showHelp:" "?"))))


;; application delegate methods:

@[HangmanController
  method: (applicationDidFinishLaunching:(:id)aNotification) 
  resultType: (:void)
  body:
  (declare (ignore aNotification))
  (let* ((main     (make-menu-bar))
         (services [[[[main itemAtIndex:0] submenu] itemAtIndex:2] submenu]))
    [[NSApplication sharedApplication] setMainMenu: main]
    [[NSApplication sharedApplication] setServicesMenu:services])
  [self createUI]
  [(controller-window self) center]
  [(controller-window self) makeKeyAndOrderFront:nil]
  (setf *random-state* (make-random-state t))
  (initialize-game self)]

@[HangmanController
  method: (applicationWillTerminate:(:id)aNotification) 
  resultType: (:void)
  body:
  (declare (ignore aNotification))
  (finalize-game self)]

@[HangmanController
  method: (applicationShouldTerminate:(:id)sender) 
  resultType: (:int)
  body:
  (declare (ignore sender))
  yes]



;; NSResponder methods:
;; HangmanController is the initialFirstResponder of its window.

@[HangmanController
  method: (acceptsFirstResponder)
  resultType: (:char)
  body: YES]

@[HangmanController
  method: (resignFirstResponder)
  resultType: (:char)
  body: NO]

@[HangmanController
  method: (keyDown:(:id)event)
  resultType: (:void)
  body:
  (let ((letter (objcl:lisp-string [event characters])))
    (cond ((and (= 1 (length letter))
                (find (character letter) (hangman-alphabet (controller-hangman self))
                      :test (function char-equal)))
           (process-letter self letter))
          ((find (character letter) #(#\newline #\return #\linefeed))
           (initialize-game self))
          (t
           [super keyDown:event])))]


;; Game control:

@[HangmanController
  method: (newGame: (:id)sender)
  resultType: (:void)
  body: (declare (ignorable sender))
  (initialize-game self)]

@[HangmanController
  method: (selectLetter:(:id)letter)
  resultType: (:void)
  body: (process-letter self (get-letter-from-sender letter))]

(defun get-letter-from-sender (button)
  (objcl:lisp-string [button title]))

;; Implementation of game; we reuse a previously written lisp hangman game.

(defun satisfactory-word-p (word &optional (alphabet *alphabet*))
  (and (< 2 (length word))
       (every (lambda (letter) (find letter alphabet :test (function char-equal)))
              word)))

(defun load-words ()
  (remove-if-not (function satisfactory-word-p)
                 (string-list-text-file-contents "/usr/share/dict/words")))

(defun words (hc)
  (when (controller-loaded-words hc)
    (setf (controller-words hc) (controller-loaded-words hc)
          (controller-loaded-words hc) nil))
  (controller-words hc))

(defun choose-random-word (words)
  (elt words (random (length words))))

(defun maximum-error-count ()
  ;; We should count the images. cf. -[NSBundle pathsForResourcesOfType:inDirectory:]
  11)

(defun image-at-index (index)
  (let ((image [[NSImage alloc] initWithContentsOfFile: [[NSBundle mainBundle]
                                                         pathForResource: (objcl:objc-string (format nil "hung-~D" index))
                                                         ofType: (objcl:objc-string "png")
                                                         inDirectory: (objcl:objc-string "images")]]))
    (if (oclo:nullp image)
        ;; While developping, we refer the resources from the src/ directory:
        (let ((pathname  (merge-pathnames (make-pathname :directory '(:relative :up "Resources" "images")
                                                         :name (format nil "hung-~D" index)
                                                         :type "png")
                                          (or *compile-file-pathname* *load-pathname* #P"./"))))
          [[NSImage alloc] initWithContentsOfFile: (objcl:objc-string (namestring (truename pathname)))])
        image)))

(defun set-hang-image (hc index)
  (when (<= 0 index (maximum-error-count))
    [(controller-image hc) setImage: (image-at-index index)]))

(defun initialize-game (hc)
  (let ((game  (make-hangman (choose-random-word (words hc)) (maximum-error-count))))
    (setf (controller-hangman hc) game)
    (set-hang-image hc 0)
    [(controller-guessed hc) setStringValue: (objcl:objc-string (hangman-found-word game))]
    [(controller-message hc) setStringValue: (objcl:objc-string "")]
    (setf (controller-finished hc) nil)))

(defun finalize-game (hc)
  [(controller-message hc) setStringValue: (objcl:objc-string "Good bye!")]
  (setf (controller-finished hc) t))


(defun process-letter (hc letter)
  (when (controller-finished hc)
    (return-from process-letter))
  (let ((game (controller-hangman hc)))
    (case (prog1 (hangman-try-letter game (character letter))
            (set-hang-image hc (hangman-error-count game)))
      (:wins
       (incf (controller-wins-counter hc))
       (incf (controller-game-counter hc))
       [(controller-guessed hc) setStringValue: (objcl:objc-string (hangman-word game))]
       [(controller-message hc) setStringValue: (objcl:objc-string (format nil "You win!~%~D/~D"
                                                                           (controller-wins-counter hc)
                                                                           (controller-game-counter hc)))]
       (provide-word-for-services (hangman-word game))
       (setf (controller-finished hc) t))
      (:loses
       (incf (controller-game-counter hc))
       [(controller-guessed hc) setStringValue: (objcl:objc-string (hangman-word game))]
       [(controller-message hc) setStringValue: (objcl:objc-string (format nil "You lose!~%~D/~D"
                                                                           (controller-wins-counter hc)
                                                                           (controller-game-counter hc)))]
       (provide-word-for-services (hangman-word game))
       (setf (controller-finished hc) t))
      (:alreadyTried
       [(controller-guessed hc) setStringValue: (objcl:objc-string (hangman-found-word game))]
       [(controller-message hc) setStringValue: (objcl:objc-string "You already tried this letter!")])
      (:bad-guess
       [(controller-guessed hc) setStringValue: (objcl:objc-string (hangman-found-word game))]
       [(controller-message hc) setStringValue: (objcl:objc-string "Bad guess!")])
      (:good-guess
       [(controller-guessed hc) setStringValue: (objcl:objc-string (hangman-found-word game))]
       [(controller-message hc) setStringValue: (objcl:objc-string "Good guess!")]))))

(defun provide-word-for-services (word)
  ;; This doesn't work. :-(
  #-(and)
  (let ((pboard [NSPasteboard pasteboardWithName:#$NSFindPboard]))
    [pboard clearContents]
    [pboard setString: (objcl:objc-string word) forType: #$NSPasteboardTypeString]))


(defvar *controller*)

(defun main ()
  ;; This function is put on the ccl:*lisp-startup-functions* list by
  ;; then generate-application.lisp script.  Therefore it will be
  ;; called on startup, before the NSApplication is launched.
  (setf *controller* [[HangmanController alloc] init])

  ;; Setting the controller as NSApplication delegate, will let it
  ;; receive the applicationDidFinishLaunching: message so it may
  ;; complete the application initialization (menus, windows).
  [[NSApplication sharedApplication] setDelegate:*controller*]
  #-(and)
  (progn ;; to debug the application:
    (ql:quickload :swank)
    (funcall (intern "CREATE-SERVER" "SWANK") :port 4099)))


;;;; THE END ;;;;


