;; tamale.el --- Edit Tamale source code using Emacs.
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
;;; To install tamale.el, place this file somewhere (your home directory is
;;; good), and add the following lines to your .emacs file:
;;;
;;;   (autoload 'tamale-mode "/path/to/tamale.el"
;;;             "Mode for editing Tamale source code" t)
;;;
;;; Next, set up your auto-mode-alist to map the right file extensions
;;; to tamale-mode.  You're set!


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

(defgroup tamale nil
  "Mode for editing Tamale source code.
Tamale is a language for card-based interactive multimedia programming."
  :group 'languages)

(defface tamale-tab-face
  ;; Apapted from make-mode.el.
  '((((class color))
     (:background  "hotpink")
     (:foreground "hotpink"))
    (t
     (:reverse-video t)))
  "Face to use for highlighting tabs in Tamale mode."
  :group 'faces
  :group 'tamale)

(defface tamale-relative-path-face
  '((t
     (:foreground "blue")))
  "Face to use for highlighting relative node paths in Tamale mode."
  :group 'faces
  :group 'tamale)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Imenu Support
;;;
;;; Specify how to build an index menu for a Tamale script.  This code is
;;; adapted from scheme.ss.

(defconst tamale-imenu-generic-expression
  '((nil "^\\s-*(\\(test-\\)?card\\s-+\\(\\sw+\\)" 2)
    ("Structures" "^\\s-*(define-struct\\s-+\\(\\sw+\\)" 1)
    ("Stylesheets" "^\\s-*(define-stylesheet\\s-+\\(\\sw+\\)" 1)
    ("Variables" "^\\s-*(define\\s-+\\(\\sw+\\)" 1)
    ("Persistent Variables" "^\\s-*(define/p\\s-+\\(\\sw+\\)" 1)
    ("Functions" "^\\s-*(define\\s-+(\\(\\sw+\\)" 1)
    ("Macros" "^\\s-*(define-syntax\\s-+(?\\(\\sw+\\)" 1)
    ("Templates" "^\\s-*(define-[^-]*-template\\s-+\\(\\sw+\\)" 1)
    )
  "Imenu generic expression for Tamale mode.  See `imenu-generic-expression'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interactive Commands


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax Highlighting

(defconst tamale-font-lock-keywords
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
	       "require" "set!" "and" "or" "module" "on" "send" "prop"
               "state-db-fn" "state-db-fn/rt" "match-let" "with-syntax"
               "define-goal" "define-goal*"
               "with-errors-blocked") t)
	"\\>") 1)

      ;; Magic variables.
      (cons "\\<\\(self\\)\\>" 1)

      ;; Relative pathnames.
      (list "\\s-\\(@\\sw+\\)"
            ;; (The extra ' in front of tamale-relative-path-face is needed
            ;; in Emacs 21 for some horrible, evil, and inexplicable
            ;; reason.)
            '(1 'tamale-relative-path-face))

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
      (list "(\\(\\(test-\\)?\\(group\\|sequence\\|card\\|element\\)\\)\\> *\\(\\sw+\\)\\>"
	    '(1 font-lock-keyword-face)
            '(4 font-lock-function-name-face))
      
      ;; Loops.
      (list "(\\(for\\(each\\)?\\)\\> *[[(] *\\(\\sw+\\)\\>"
	    '(1 font-lock-keyword-face)
            '(3 font-lock-variable-name-face))

      ;; Literal tabs, which make a non-portable mess of the source.
      ;; (The extra ' in front of tamale-tab-face is needed in Emacs 21
      ;; for some horrible, evil, and inexplicable reason.)
      (list "\\(\t\\)" '(1 'tamale-tab-face))
      
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
(put 'element 'scheme-indent-function 2)
(put 'on 'scheme-indent-function 2)
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
(put 'define-stylesheet 'scheme-indent-function 1)
(put 'syntax-case 'scheme-indent-function 2)
(put 'match-let 'scheme-indent-function 1)
(put 'with-syntax 'scheme-indent-function 1)
(put 'define-goal 'scheme-indent-function 2)
(put 'define-goal* 'scheme-indent-function 4)
(put 'with-errors-blocked 'scheme-indent-function 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tamale API Information
;;;
;;; This code is inspired by and based on dylan-params.el.  It provides
;;; a bunch of ways for programmers to look up information about the tamale
;;; API while editing.

(defvar tamale-id-chars "-a-zA-Z0-9!&*<>=|^$%@_+~?/")

(defun tamale-struct-commands (struct-name &rest members)
  `((,(intern (concat struct-name "?")) obj => bool)
     ,@(mapcar #'(lambda (m)
		   `(,(intern (concat struct-name "-" m))
		     ,(intern struct-name) => ,(intern m)))
	       members)
     ,@(mapcar #'(lambda (m)
		   `(,(intern (concat "set-" struct-name "-" m "!"))
		     ,(intern struct-name) ,(intern m)))
	       members)))

(defconst tamale-functions
  `(;; From kernel.ss.
    (point x y => point)
    ,@(tamale-struct-commands "point" "x" "y")
    (rect left top right bottom => rect)
    ,@(tamale-struct-commands "rect" "left" "top" "right" "bottom")
    (color red green blue &opt alpha => color)
    ,@(tamale-struct-commands "color" "red" "green" "blue" "alpha")
    (percent value => percent)
    ,@(tamale-struct-commands "percent" "value")
    (assert cond)
    (member? item list => bool)
    (value->string value => string)
    (cat &rest values => string)
    (label name &body body)
    (with-errors-blocked (report-func) &body body)
    (call-5l-prim name &rest args => result)
    (have-5l-prim? name => bool)
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
    ;; From tamale.ss
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
  "Information about Tamale commands known to Tamale mode.")

(defun tamale-command-find (name)
  "Return information about the Tamale command NAME, or nil."
  (assoc name tamale-functions))

(defun tamale-command-group (cmd start)
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

(defun tamale-command-name (cmd)
  "Get the name of the specified Tamale command."
  (car cmd))

(defun tamale-command-macro-p (cmd)
  "Does CMD describe a macro?"
  (memq '&body cmd))

(defun tamale-command-macro-pattern (cmd)
  "Get the macro pattern associated with CMD."
  (cdr cmd))

(defun tamale-command-args (cmd)
  "Return the mandatory arguments of CMD."
  (tamale-command-group cmd (car cmd)))

(defun tamale-command-opt-args (cmd)
  "Return the optional arguments of CMD."
  (tamale-command-group cmd '&opt))

(defun tamale-command-rest-args (cmd)
  "Return the &rest argument of CMD."
  (tamale-command-group cmd '&rest))

(defun tamale-command-key-args (cmd)
  "Return the keyword arguments of CMD."
  (tamale-command-group cmd '&key))

(defun tamale-command-results (cmd)
  "Return the return value(s) of CMD."
  (tamale-command-group cmd '=>))

(defun tamale-command-help-message (cmd)
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
    (if (tamale-command-macro-p cmd)
	(concat "(" (symbol-name (tamale-command-name cmd))
		(funcall pretty "" (tamale-command-macro-pattern cmd)) ")")
      (concat "(" (symbol-name (tamale-command-name cmd))
	      (funcall pretty "" (tamale-command-args cmd))
	      (funcall pretty " &opt" (tamale-command-opt-args cmd))
	      (funcall pretty " &rest" (tamale-command-rest-args cmd))
	      (funcall pretty " &key" (tamale-command-key-args cmd)) ")"
	      (funcall pretty " =>" (tamale-command-results cmd))))))

(defun tamale-current-function-name ()
  "Get the name of the current function."
  (save-excursion
    ;; Search back to start of the current function.
    (condition-case nil
	(while t
	  (backward-sexp))
      (error nil)) 
    (let* ((start (point))
           (execute-this (skip-chars-forward tamale-id-chars))
           (end (point))
           (name (buffer-substring-no-properties start end)))
      name)))

(defun tamale-display-function-params ()
  "Displays the parameters of the current function, if known."
  (interactive)
  (save-excursion
    (skip-chars-backward tamale-id-chars)
    (let* ((name (intern (tamale-current-function-name)))
	   (cmd (tamale-command-find name)))
      (if cmd
	  (message "%s" (tamale-command-help-message cmd))
	(message "Unknown Tamale command: %s" name)))))

(defvar tamale-function-name-history-list '())

(defun tamale-read-function-name (prompt)
  (completing-read prompt
		   (mapcar #'(lambda (cmd)
			       (cons (symbol-name (tamale-command-name cmd))
				     nil))
			   tamale-functions)
		   nil t nil 'tamale-function-name-history-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Templates & Abbreviations

(define-abbrev-table 'tamale-mode-abbrev-table '())

(defun define-tamale-abbrev (name expansion &optional hook count)
  "Define an abbreviation for use with Tamale mode.
See define-abbrev for more information."
  (define-abbrev tamale-mode-abbrev-table name expansion hook count))

(tempo-define-template "draw-box-rect"
		       '("draw-box (rect " (p . "Left: ") " "
			 (p . "Top: ") " " (p . "Right: ") " "
			 (p . "Bottom: ") ") " (p . "Color: ") ")"))

(define-tamale-abbrev "dbr" "" 'tempo-template-draw-box-rect)

(defun tamale-insert-function ()
  "Choose and insert a function, prompting for arguments."
  (interactive)
  (let ((name (tamale-read-function-name "Insert function: ")))
    (unless (equal name "")
      (let ((cmd (tamale-command-find (intern name))))
	(if (tamale-command-macro-p cmd)
	    (error "Insertion of Tamale macros isn't implemented")
	  ;; Generate a template on the fly and insert it.  Why not?
	  (tempo-define-template
	   (concat "tamale-" name)
	   `("(" ,name
	     ,@(mapcar #'(lambda (a)
			   (let ((name (upcase-initials (symbol-name a))))
			     `(l " " (p . ,(concat name ": ")))))
		       (tamale-command-args cmd))
	     ")" >) nil)
	  (tempo-insert-template (intern
				  (concat "tempo-template-tamale-" name))
				 nil)
	  (message "%s" (tamale-command-help-message cmd))
	  )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keybindings

(defconst tamale-mode-map nil
  "Keybindings for Tamale mode")

;; Do our basic initialization the first time around.
(unless tamale-mode-map
  (setq tamale-mode-map (copy-keymap scheme-mode-map))
  (define-key tamale-mode-map "\C-c\C-h" 'tamale-display-function-params)
  (define-key tamale-mode-map "\C-c\C-f" 'tamale-insert-function)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Yummy Filling

(define-derived-mode tamale-mode scheme-mode "Tamale"
  "Major mode for editing Tamale source code.

\\{tamale-mode-map}"

  ;; Code to set up font-lock mode (borrowed from scheme.el).
  (setq font-lock-defaults
        '(tamale-font-lock-keywords
          nil t (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))

  ;; Brutally discourage tabs, especially 8-space tabs.
  (setq indent-tabs-mode nil)
  (setq tab-width 4)

  ;; Install our keymap.
  (use-local-map tamale-mode-map)

  ;; Make tempo.el templates interactive (EXPERIMENTAL).
  (set (make-local-variable 'tempo-interactive) t)

  ;; Install our abbreviation table.
  (abbrev-mode t)
  (setq local-abbrev-table tamale-mode-abbrev-table)

  ;; Set up our imenu support (borrowed from scheme.el).
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression tamale-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
	'(("+-*/.<>=?!$%_&~^:" . "w")))

  ;; Install an "Index" menu.
  (imenu-add-menubar-index)
  )

(provide 'tamale)

;;; tamale.el ends here
