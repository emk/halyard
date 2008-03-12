;; halyard.el --- Edit Halyard source code using Emacs.
;; Copyright 2002 Trustees of Dartmouth College

;; Author: Eric Kidd <eric.kidd@pobox.com>
;; Version: Unreleased

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published 
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions
;;;
;;; To install halyard.el, place this file somewhere (your home directory is
;;; good), and add the following lines to your .emacs file:
;;;
;;;   (autoload 'halyard-mode "/path/to/halyard.el"
;;;             "Mode for editing Halyard source code" t)
;;;
;;; Next, set up your auto-mode-alist to map the right file extensions
;;; to halyard-mode:
;;;
;;;     ;; associate "*.ss" files with the Halyard-mode bindings.
;;;     (setq auto-mode-alist
;;;           (append '(("\\.ss\\'" . halyard-mode))
;;;                   auto-mode-alist))
;;;
;;;
;;; You're set!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Required Packages

(require 'cl)
(require 'derived)
(require 'easymenu)
(require 'scheme)
(require 'tempo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customization Support

(defgroup halyard nil
  "Mode for editing Halyard source code.
Halyard is a language for card-based interactive multimedia programming."
  :group 'languages)

(defface halyard-tab-face
  ;; Apapted from make-mode.el.
  '((((class color))
     (:background  "hotpink")
     (:foreground "hotpink"))
    (t
     (:reverse-video t)))
  "Face to use for highlighting tabs in Halyard mode."
  :group 'faces
  :group 'halyard)

(defface halyard-relative-path-face
  '((t
     (:foreground "blue")))
  "Face to use for highlighting relative node paths in Halyard mode."
  :group 'faces
  :group 'halyard)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Imenu Support
;;;
;;; Specify how to build an index menu for a Halyard script.  This code is
;;; adapted from scheme.ss.

(defconst halyard-imenu-generic-expression
  '((nil "^\\s-*(\\(test-\\)?card\\s-+\\(\\sw+\\)" 2)
    ("Structures" "^\\s-*(define-struct\\s-+\\(\\sw+\\)" 1)
    ("Stylesheets" "^\\s-*(define-stylesheet\\s-+\\(\\sw+\\)" 1)
    ("Variables" "^\\s-*(define\\s-+\\(\\sw+\\)" 1)
    ("Persistent Variables" "^\\s-*(define/p\\s-+\\(\\sw+\\)" 1)
    ("Functions" "^\\s-*(define\\s-+(\\(\\sw+\\)" 1)
    ("Macros" "^\\s-*(define-syntax\\s-+(?\\(\\sw+\\)" 1)
    ("Templates" "^\\s-*(define-[^-]*-template\\s-+\\(\\sw+\\)" 1)
    )
  "Imenu generic expression for Halyard mode.  See `imenu-generic-expression'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interactive Commands


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax Highlighting

(defconst halyard-font-lock-keywords
  (append scheme-font-lock-keywords-2
   (eval-when-compile
     (list

      ;; Control structures.
      (cons
       (concat
        "(" (regexp-opt
             '("fn" "callback" "deferred-callback" "while" "when" "unless"
               "with-dc" "with-saved-text-position"
               "with-saved-graphic-position" "with-offset-origin"
               "with-default-element-parent"
               "with-timeout-override"
               "require" "set!" "and" "or" "module" "on" "send" "prop"
               "state-db-fn" "state-db-fn/rt" "match-let" "with-syntax"
               "define-goal" "define-goal*"
               "with-errors-blocked" "test" "test-elements"
               "begin-for-syntax" "def"
               "with-instance" "with-handlers" "parameterize"
               "animate" "interpolate" "after" "method" "with-handlers"
               "method" "on" "advise" "attr" "value" "default"
               "syntax-case" "syntax/loc" "quasisyntax/loc" "foreach"
               "provide" "lib" "all-from" "after-updating" "defclass"
               "defmethod" "defgeneric" "setup" "run") t)
        "\\>") 1)

      ;; Magic variables.
      (cons "\\<\\(self\\|super\\|after\\|before\\|around\\)\\>" 1)

      ;; Method invocation.
      (list "\\<\\(\\.\\sw+\\)\\>"
            '(1 font-lock-function-name-face))

      ;; Class names.
      (list "\\<\\(%[^% ()]+%\\)\\>"
            '(1 font-lock-function-name-face))

      ;; Relative pathnames.
      (list "\\<\\(@\\sw+\\)\\>"
            ;; (The extra ' in front of halyard-relative-path-face is needed
            ;; in Emacs 21 for some horrible, evil, and inexplicable
            ;; reason.)
            '(1 'halyard-relative-path-face))

      ;; Non-standard definitions.  Make sure these get processed *after*
      ;; the rules we inherit from scheme-mode.  (This is loosely adapted
      ;; from one of the scheme.el patterns for DSSSL.)
      (list "(\\(define\\(\\sw+\\)\\)\\> *\\([[(]?\\)\\(\\sw+\\)\\>"
            '(1 font-lock-keyword-face)
            '(4 (cond
                 ((match-beginning 3) 'font-lock-function-name-face)
                 ((equal (match-beginning 2) "define-struct")
                  'font-lock-type-face)
                 (t 'font-lock-variable-name-face))))
      
      ;; Cards.
      (list "(\\(\\(test-\\)?\\(group\\|sequence\\|box\\|clickable-zone\\|text-box\\|text\\|graphic\\|rectangle\\|rectangle-outline\\|sprite\\|browser\\|edit-box\\|geiger-audio\\|sine-wave\\|vorbis-audio\\|movie\\|card\\|elem\\)\\)\\> *\\(\\sw+\\)\\>"
            '(1 font-lock-keyword-face)
            '(4 font-lock-function-name-face))
      
      ;; Loops.
      (list "(\\(for\\(each\\)?\\)\\> *[[(] *\\(\\sw+\\)\\>"
            '(1 font-lock-keyword-face)
            '(3 font-lock-variable-name-face))

      ;; Literal tabs, which make a non-portable mess of the source.
      ;; (The extra ' in front of halyard-tab-face is needed in Emacs 21
      ;; for some horrible, evil, and inexplicable reason.)
      (list "\\(\t\\)" '(1 'halyard-tab-face))
      
      ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indentation
;;;
;;; Note that these keywords get indented in all Scheme-derived modes,
;;; not just this one.  I hope this isn't a problem.
;;;
;;; The number in each declaration specified the number of expressions
;;; which go on the same line as the the name of the form.

(put 'module 'scheme-indent-function 2)
(put 'group 'scheme-indent-function 2)
(put 'sequence 'scheme-indent-function 2)
(put 'card 'scheme-indent-function 2)
(put 'elem 'scheme-indent-function 2)
(put 'on 'scheme-indent-function 1)
(put 'state-db-fn 'scheme-indent-function 1)
(put 'state-db-fn/rt 'scheme-indent-function 2)
(put 'define-state-db-listener 'scheme-indent-function 1)
(put 'define-state-db-listener/rt 'scheme-indent-function 2)
(put 'define-group-template 'scheme-indent-function 3)
(put 'define-card-template 'scheme-indent-function 3)
(put 'define-element-template 'scheme-indent-function 3)
(put 'test-card 'scheme-indent-function 2) ; for 5Ltest_scheme only.
(put 'fn 'scheme-indent-function 1)
(put 'callback 'scheme-indent-function 0)
(put 'deferred-callback 'scheme-indent-function 0)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'for 'scheme-indent-function 1)
(put 'foreach 'scheme-indent-function 1)
(put 'with-dc 'scheme-indent-function 1)
(put 'with-saved-text-position 'scheme-indent-function 0)
(put 'with-saved-graphic-position 'scheme-indent-function 0)
(put 'with-offset-origin 'scheme-indent-function 1)
(put 'with-default-element-parent 'scheme-indent-function 1)
(put 'with-timeout-override 'scheme-indent-function 1)
(put 'define-stylesheet 'scheme-indent-function 1)
(put 'syntax-case 'scheme-indent-function 2)
(put 'match-let 'scheme-indent-function 1)
(put 'with-syntax 'scheme-indent-function 1)
(put 'define-goal 'scheme-indent-function 3)
(put 'define-goal* 'scheme-indent-function 4)
(put 'with-errors-blocked 'scheme-indent-function 1)
(put 'define-goal-condition 'scheme-indent-function 1)
(put 'define-goal-condition 'scheme-indent-function 2)
(put 'test 'scheme-indent-function 1)
(put 'test-elements 'scheme-indent-function 1)
(put 'begin-for-syntax 'scheme-indent-function 0)
(put 'def 'scheme-indent-function 1)
(put 'with-instance 'scheme-indent-function 1)
(put 'with-handlers 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'animate 'scheme-indent-function 1)
(put 'simultaneously 'scheme-indent-function 0)
(put 'ease-in 'scheme-indent-function 0)
(put 'ease-out 'scheme-indent-function 0)
(put 'ease-in/out 'scheme-indent-function 0)
(put 'after 'scheme-indent-function 0)
(put 'immediately 'scheme-indent-function 0)
(put 'finally 'scheme-indent-function 0)
(put 'quantize 'scheme-indent-function 1)
(put 'method 'scheme-indent-function 1)
(put 'with-handlers 'scheme-indent-function 1)
(put 'advise 'scheme-indent-function 2)
(put 'syntax/loc 'scheme-indent-function 1)
(put 'quasisyntax/loc 'scheme-indent-function 1)
(put 'after-updating 'scheme-indent-function 1)
(put 'setup 'scheme-indent-function 0)
(put 'run 'scheme-indent-function 0)
(put 'box 'scheme-indent-function 2)
(put 'clickable-zone 'scheme-indent-function 2)
(put 'text-box 'scheme-indent-function 2)
(put 'text 'scheme-indent-function 2)
(put 'graphic 'scheme-indent-function 2)
(put 'rectangle 'scheme-indent-function 2)
(put 'rectangle-outline 'scheme-indent-function 2)
(put 'sprite 'scheme-indent-function 2)
(put 'browser 'scheme-indent-function 2)
(put 'edit-box 'scheme-indent-function 2)
(put 'geiger-audio 'scheme-indent-function 2)
(put 'sine-wave 'scheme-indent-function 2)
(put 'vorbis-audio 'scheme-indent-function 2)
(put 'movie 'scheme-indent-function 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Halyard API Information
;;;
;;; This code is inspired by and based on dylan-params.el.  It provides
;;; a bunch of ways for programmers to look up information about the halyard
;;; API while editing.

(defvar halyard-id-chars "-a-zA-Z0-9!&*<>=|^$%@_+~?/")

(defun halyard-struct-commands (struct-name &rest members)
  `((,(intern (concat struct-name "?")) obj => bool)
     ,@(mapcar #'(lambda (m)
                   `(,(intern (concat struct-name "-" m))
                     ,(intern struct-name) => ,(intern m)))
               members)
     ,@(mapcar #'(lambda (m)
                   `(,(intern (concat "set-" struct-name "-" m "!"))
                     ,(intern struct-name) ,(intern m)))
               members)))

(defconst halyard-functions
  `(;; From kernel.ss.
    (point x y => point)
    ,@(halyard-struct-commands "point" "x" "y")
    (rect left top right bottom => rect)
    ,@(halyard-struct-commands "rect" "left" "top" "right" "bottom")
    (color red green blue &opt alpha => color)
    ,@(halyard-struct-commands "color" "red" "green" "blue" "alpha")
    (percent value => percent)
    ,@(halyard-struct-commands "percent" "value")
    (assert cond)
    (member? item list => bool)
    (value->string value => string)
    (cat &rest values => string)
    (label name &body body)
    (with-errors-blocked (report-func) &body body)
    (call-prim name &rest args => result)
    (have-prim? name => bool)
    (idle)
    (5l-log msg)
    (debug-log msg)
    (caution msg)
    (debug-caution msg)
    (non-fatal-error msg)
    (fatal-error msg)
    (engine-var name => value)
    (set-engine-var! name value)
    (engine-var-exists? name => bool)
    (throw msg => no-return)
    (exit-script)
    (jump card-or-name => no-return)
    (refresh)
    (card-exists? name => bool)
    (current-card -> card)
    (card-name card => string)
    (card name &body body)
    ;; From api.ss
    (with-tracing &body body)
    (fn (&rest args) &body body)
    (callback &body body)
    (deferred-callback &body body)
    (while test &body body)
    (for (name init-value cond next-value) &body body)
    (foreach (name list) &body body)
    (define-engine-variable name 5l-name init-value)
    (define/p name init-value)
    (text-position => point)
    (set-text-position! point)
    (with-saved-text-position &body body)
    (graphic-position => point)
    (set-graphic-position! point)
    (with-saved-graphic-position &body body)
    (point-offset point by-point => new-point)
    (point-different point1 point2 => diff-point)
    (rect-offset rect by-point => new-rect)
    (rect-left-top rect => point)
    (rect-left-bottom rect => point)
    (rect-right-top rect => point)
    (rect-right-bottom rect => point)
    (origin => point)
    (set-origin! point)
    (with-offset-origin to-point &body body)
    (define-stylesheet name
      &key base family size flags justification color highlight-color
      height-adjustment shadow-offset shadow-color highlight-shadow-color)
    (draw-text style rect text)
    (measure-text style text &key max-width)
    ;; From halyard.ss
    (load-picture name point &key rect)
    (modal-input rect size forecolor backcolor => string)
    (zone name rect action &key cursor)
    (delete-element name)
    (delete-elements &opt names)
    (clear-screen color)
    (rect-center rect => point)
    (center-text stylesheet rect text &key axis)
    (html name rect path)
    (edit-box name rect text)
    (movie name r location &key controller? audio-only? loop? interaction?)
    (wait name &key frame)
    (tc arg1 &opt arg2 arg3)
    (draw-line from to color width)
    (draw-box rect color)
    (draw-box-outline rect color width)
    (insert-rect rect pixels => rect)
    (timeout seconds card)
    (current-card-name => string)
    (fade)
    (unfade)
    )
  "Information about Halyard commands known to Halyard mode.")

(defun halyard-command-find (name)
  "Return information about the Halyard command NAME, or nil."
  (assoc name halyard-functions))

(defun halyard-command-group (cmd start)
  ;; Return the entries in CMD between START and the next special symbol.
  (let* ((specials '(&opt &key &rest &body =>))
         (result '())
         (scan-start #'(lambda (l)
                         (if l
                             (if (eq (car l) start)
                                 (cdr l)
                               (funcall scan-start (cdr l)))
                           '())))
         (extract-result #'(lambda (l)
                             (when (and l (not (memq (car l) specials)))
                               (push (car l) result)
                               (funcall extract-result (cdr l))))))
    (funcall extract-result (funcall scan-start cmd))
    (reverse result)))

(defun halyard-command-name (cmd)
  "Get the name of the specified Halyard command."
  (car cmd))

(defun halyard-command-macro-p (cmd)
  "Does CMD describe a macro?"
  (memq '&body cmd))

(defun halyard-command-macro-pattern (cmd)
  "Get the macro pattern associated with CMD."
  (cdr cmd))

(defun halyard-command-args (cmd)
  "Return the mandatory arguments of CMD."
  (halyard-command-group cmd (car cmd)))

(defun halyard-command-opt-args (cmd)
  "Return the optional arguments of CMD."
  (halyard-command-group cmd '&opt))

(defun halyard-command-rest-args (cmd)
  "Return the &rest argument of CMD."
  (halyard-command-group cmd '&rest))

(defun halyard-command-key-args (cmd)
  "Return the keyword arguments of CMD."
  (halyard-command-group cmd '&key))

(defun halyard-command-results (cmd)
  "Return the return value(s) of CMD."
  (halyard-command-group cmd '=>))

(defun halyard-command-help-message (cmd)
  "Return a help string for CMD."
  (let ((pretty #'(lambda (sep items)
                    (if (not items)
                        ""
                      (apply #'concat sep
                             (mapcar
                              #'(lambda (i)
                                  (let ((name (symbol-name i)))
                                    (if (equal (elt name 0) ?&)
                                        (concat " " name)
                                        (concat " " (upcase name)))))
                              items))))))
    (if (halyard-command-macro-p cmd)
        (concat "(" (symbol-name (halyard-command-name cmd))
                (funcall pretty "" (halyard-command-macro-pattern cmd)) ")")
      (concat "(" (symbol-name (halyard-command-name cmd))
              (funcall pretty "" (halyard-command-args cmd))
              (funcall pretty " &opt" (halyard-command-opt-args cmd))
              (funcall pretty " &rest" (halyard-command-rest-args cmd))
              (funcall pretty " &key" (halyard-command-key-args cmd)) ")"
              (funcall pretty " =>" (halyard-command-results cmd))))))

(defun halyard-current-function-name ()
  "Get the name of the current function."
  (save-excursion
    ;; Search back to start of the current function.
    (condition-case nil
        (while t
          (backward-sexp))
      (error nil)) 
    (let* ((start (point))
           (execute-this (skip-chars-forward halyard-id-chars))
           (end (point))
           (name (buffer-substring-no-properties start end)))
      name)))

(defun halyard-display-function-params ()
  "Displays the parameters of the current function, if known."
  (interactive)
  (save-excursion
    (skip-chars-backward halyard-id-chars)
    (let* ((name (intern (halyard-current-function-name)))
           (cmd (halyard-command-find name)))
      (if cmd
          (message "%s" (halyard-command-help-message cmd))
        (message "Unknown Halyard command: %s" name)))))

(defvar halyard-function-name-history-list '())

(defun halyard-read-function-name (prompt)
  (completing-read prompt
                   (mapcar #'(lambda (cmd)
                               (cons (symbol-name (halyard-command-name cmd))
                                     nil))
                           halyard-functions)
                   nil t nil 'halyard-function-name-history-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Templates & Abbreviations

(define-abbrev-table 'halyard-mode-abbrev-table '())

(defun define-halyard-abbrev (name expansion &optional hook count)
  "Define an abbreviation for use with Halyard mode.
See define-abbrev for more information."
  (define-abbrev halyard-mode-abbrev-table name expansion hook count))

(tempo-define-template "draw-box-rect"
                       '("draw-box (rect " (p . "Left: ") " "
                         (p . "Top: ") " " (p . "Right: ") " "
                         (p . "Bottom: ") ") " (p . "Color: ") ")"))

(define-halyard-abbrev "dbr" "" 'tempo-template-draw-box-rect)

(defun halyard-insert-function ()
  "Choose and insert a function, prompting for arguments."
  (interactive)
  (let ((name (halyard-read-function-name "Insert function: ")))
    (unless (equal name "")
      (let ((cmd (halyard-command-find (intern name))))
        (if (halyard-command-macro-p cmd)
            (error "Insertion of Halyard macros isn't implemented")
          ;; Generate a template on the fly and insert it.  Why not?
          (tempo-define-template
           (concat "halyard-" name)
           `("(" ,name
             ,@(mapcar #'(lambda (a)
                           (let ((name (upcase-initials (symbol-name a))))
                             `(l " " (p . ,(concat name ": ")))))
                       (halyard-command-args cmd))
             ")" >) nil)
          (tempo-insert-template (intern
                                  (concat "tempo-template-halyard-" name))
                                 nil)
          (message "%s" (halyard-command-help-message cmd))
          )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keybindings

(defconst halyard-mode-map nil
  "Keybindings for Halyard mode")

;; Do our basic initialization the first time around.
(unless halyard-mode-map
  (setq halyard-mode-map (copy-keymap scheme-mode-map))
  (define-key halyard-mode-map "\C-c\C-h" 'halyard-display-function-params)
  (define-key halyard-mode-map "\C-c\C-f" 'halyard-insert-function)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Yummy Filling

(define-derived-mode halyard-mode scheme-mode "Halyard"
  "Major mode for editing Halyard source code.

\\{halyard-mode-map}"

  ;; Code to set up font-lock mode (borrowed from scheme.el).
  (setq font-lock-defaults
        '(halyard-font-lock-keywords
          nil t (("+-*/.<>=!?$@%_&~^:" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))

  ;; Brutally discourage tabs, especially 8-space tabs.
  (setq indent-tabs-mode nil)
  (setq tab-width 4)

  ;; Install our keymap.
  (use-local-map halyard-mode-map)

  ;; Make tempo.el templates interactive (EXPERIMENTAL).
  (set (make-local-variable 'tempo-interactive) t)

  ;; Install our abbreviation table.
  (abbrev-mode t)
  (setq local-abbrev-table halyard-mode-abbrev-table)

  ;; Set up our imenu support (borrowed from scheme.el).
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression halyard-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
        '(("+-*/.<>=?!$%_&~^:" . "w")))

  ;; Install an "Index" menu.
  (imenu-add-menubar-index)
  )

(provide 'halyard)

;;; halyard.el ends here
