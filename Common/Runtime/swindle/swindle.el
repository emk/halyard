;;; swindle.el --- Swindle editing mode.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;; Commentary:
;; Hack of scheme.el for Swindle.

;;; Code:

(require 'lisp-mode)

(defvar swindle-mode-syntax-table nil)
(if (not swindle-mode-syntax-table)
  (let ((i 0))
    (setq swindle-mode-syntax-table (make-syntax-table))
    (set-syntax-table swindle-mode-syntax-table)
    ;; Default is atom-constituent.
    (while (< i 256)
      (modify-syntax-entry i "_   ")
      (setq i (1+ i)))
    ;; Word components.
    (setq i ?0)
    (while (<= i ?9)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    ;; Whitespace
    (modify-syntax-entry ?\t "    ")
    (modify-syntax-entry ?\n ">   ")
    (modify-syntax-entry ?\f "    ")
    (modify-syntax-entry ?\r "    ")
    (modify-syntax-entry ?   "    ")
    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  ")
    (modify-syntax-entry ?\] ")[  ")
    (modify-syntax-entry ?{  "(}  ")
    (modify-syntax-entry ?}  "){  ")
    (modify-syntax-entry ?\| "  23")
    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  ")
    (modify-syntax-entry ?\) ")(  ")
    (modify-syntax-entry ?\; "<   ")
    (modify-syntax-entry ?\" "\"    ")
    (modify-syntax-entry ?'  "  p")
    (modify-syntax-entry ?`  "  p")
    ;; Special characters
    (modify-syntax-entry ?,  "_ p")
    (modify-syntax-entry ?@  "_ p")
    (modify-syntax-entry ?#  "_ p14")
    (modify-syntax-entry ?\\ "\\   ")))

(defvar swindle-mode-abbrev-table nil)
(define-abbrev-table 'swindle-mode-abbrev-table ())

(defvar swindle-imenu-generic-expression
  '((nil
     "^(def\\(ine\\)?\\(\\|-?\\(generic\\(\\|-procedure\\)\\|method\\|matcher\\)\\)*\\s-+(?\\(\\sw+\\)" 5)
    ("Types"
     "^(def\\(ine-\\)class\\s-+(?\\(\\sw+\\)" 2)
    ("Macros"
     "^(\\(def\\(macro\\|subst\\)\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 3))
  "Imenu generic expression for Swindle mode.
See `imenu-generic-expression'.")

(defun swindle-mode-variables ()
  (set-syntax-table swindle-mode-syntax-table)
  (setq local-abbrev-table swindle-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'swindle-indent-function)
  (setq mode-line-process '("" swindle-mode-line-process))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression swindle-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
       '(("+-*/.<>=?!$%_&~^:" . "w")))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((swindle-font-lock-keywords
           swindle-font-lock-keywords-1 swindle-font-lock-keywords-2)
          nil t (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun))))

(defvar swindle-mode-line-process "")

(defvar swindle-mode-map nil
  "Keymap for Swindle mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(unless swindle-mode-map
  (let ((map (make-sparse-keymap "Swindle")))
    (setq swindle-mode-map (make-sparse-keymap))
    (set-keymap-parent swindle-mode-map lisp-mode-shared-map)
    (define-key swindle-mode-map [menu-bar] (make-sparse-keymap))
    (define-key swindle-mode-map [menu-bar swindle]
      (cons "Swindle" map))
    (define-key map [run-swindle] '("Run Inferior Swindle" . run-swindle))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)))

(defun run-swindle ()
  (interactive)
  (run-scheme swindle-program-name))

;; Used by cmuscheme
(defun swindle-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'indent-sexp))


;;;###autoload
(defun swindle-mode ()
  "Major mode for editing Swindle code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior Swindle process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Swindle buffers.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{swindle-mode-map}
Entry to this mode calls the value of `swindle-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (swindle-mode-initialize)
  (swindle-mode-variables)
  (run-hooks 'swindle-mode-hook))

(defun swindle-mode-initialize ()
  (use-local-map swindle-mode-map)
  (setq major-mode 'swindle-mode)
  (setq mode-name "Swindle"))

(defgroup swindle nil
  "Editing Swindle code"
  :group 'lisp)

(defcustom swindle-mode-hook nil
  "Normal hook run when entering `swindle-mode'.
See `run-hooks'."
  :type 'hook
  :group 'swindle)

(defcustom swindle-program-name "swindle"
  "*Program invoked by the `run-swindle' command."
  :type 'string
  :group 'swindle)

(defface font-lock-swindle-var-face
  '((((class color)) (:foreground "Pink"))
    (t (:inverse-video t :bold t)))
  nil)
(setq font-lock-swindle-var-face 'font-lock-swindle-var-face)

(defconst swindle-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(def\\(ine\\)?\\*?\\("
                   ;; Function names.
                   "\\(\\|-values\\|"
                   "-?\\(\\|before\\|after\\|around\\)method"
                   "\\|-?generic\\)\\|"
                   ;; Macro names, as variable names.  A bit dubious, this.
                   "\\(-?macro\\|-id-macro\\|-?syntax\\|subst\\)\\|"
                   ;; Class names.
                   "-?\\(entity\\)?class\\|struct\\|matcher0?"
                   "\\)\\)\\*?\\>"
                   ;; Any whitespace and declared object.
                   "[ \t]*\\((\\)?"
                   "\\(\\sw+\\)?")
           '(1 font-lock-keyword-face)
           '(9 (cond ((match-beginning 6) font-lock-constant-face)
                     ((match-beginning 8) font-lock-function-name-face)
                     (t
                      (if (save-match-data
                            (string-match "^\\*.*\\*$" (match-string 9)))
                        font-lock-swindle-var-face
                        font-lock-variable-name-face)))
               nil t))
     ))
  "Subdued expressions to highlight in Swindle modes.")

(defconst swindle-font-lock-keywords-2
  (append swindle-font-lock-keywords-1
   (eval-when-compile
     (list
      ;; Control structures.
      (cons
       (concat
        "(" (regexp-opt
             '("begin" "call-with-current-continuation" "call/cc"
               "call-with-input-file" "call-with-output-file" "case" "cond"
               "do" "else" "for-each" "if" "lambda"
               "let" "let*" "let-syntax" "letrec" "letrec-syntax"
               "let-id-macro" "let-macro" "letmacro" "letsubst" "letsyntax"
               "let/cc" "let/ec"
               ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
               "and" "or" "delay"
               ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
               "quasiquote" "quote" "unquote" "unquote-splicing"
               "map" "syntax" "syntax-rules"
               ;; New MzScheme stuff
               "syntax/loc" "quote-syntax" "quasisyntax" "quasisyntax/loc"
               "syntax-case" "with-syntax"
               "module" "provide" "require"
               ;; Some more
               "when" "unless" "begin0"
               "thunk" "while" "until"
               "generic" "method" "beforemethod" "aftermethod" "aroundmethod"
               "matcher" "match"
               "class" "entityclass"
               "setf!" "psetf!" "pset!" "error" "with-slots" "with-accessors"
               "dynamic-wind" "let-values" "let*-values" "fluid-let"
               "parameterize" "with-handlers" "ignore-errors" "no-errors"
               ) t)
        "\\>") 1)
      ;; Builtin variables.
      '("\\<\\*\\sw+\\*\\>" . font-lock-swindle-var-face)
      ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
      '("\\(\\<<\\sw+>\\>\\|([ \t]*singleton[ \t]*[^ \t\n\r)]*[ \t]*)\\)" .
        font-lock-type-face)
      (list (concat "([ \t]*"
                    "\\(\\sw+\\)[ \t]*"
                    "\\(\\<<\\sw+>\\>\\|"
                    "([ \t]*singleton[ \t]*'[^ \t\n\r)]*[ \t]*)\\)[ \t]*"
                    "\\([^ \n\r()]*([^ \n\r()]*)[^ \n\r()]*\\|[^ \n\r()]+\\)?"
                    "[ \t]*)")
            '(1 font-lock-variable-name-face nil t))
      ;; Swindle `:' keywords as builtins (also MzScheme's).
      '("\\<:\\sw+\\>" . font-lock-builtin-face)
      '("#\\<<\\sw+>>\\>" . font-lock-builtin-face)
      )))
  "Gaudy expressions to highlight in Swindle modes.")

(defvar swindle-font-lock-keywords swindle-font-lock-keywords-2
  "Default expressions to highlight in Swindle modes.")


(defvar calculate-lisp-indent-last-sexp)

;; Copied from lisp-indent-function, but with gets of
;; swindle-indent-{function,hook}.
(defun swindle-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
      ;; car of form doesn't seem to be a a symbol
      (progn
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'swindle-indent-function)
                         (get (intern-soft function) 'swindle-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))


;;; Let is different in Swindle

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun swindle-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (swindle-indent-specform 2 state indent-point)
;;      (swindle-indent-specform 1 state indent-point)))

(defun swindle-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'swindle-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin                 'swindle-indent-function 0)
(put 'case                  'swindle-indent-function 1)
(put 'delay                 'swindle-indent-function 0)
(put 'do                    'swindle-indent-function 2)
(put 'lambda                'swindle-indent-function 1)
(put 'let                   'swindle-indent-function 'defun)
(put 'let*                  'swindle-indent-function 1)
(put 'letrec                'swindle-indent-function 1)
(put 'let-values            'swindle-indent-function 1)
(put 'let*-values           'swindle-indent-function 1)
(put 'fluid-let             'swindle-indent-function 1)
(put 'let/cc                'swindle-indent-function 1)
(put 'let/ec                'swindle-indent-function 1)
(put 'let-id-macro          'swindle-indent-function 2)
(put 'let-macro             'swindle-indent-function 2)
(put 'letmacro              'swindle-indent-function 1)
(put 'letsubst              'swindle-indent-function 1)
(put 'sequence              'swindle-indent-function 0) ; SICP, not r4rs
(put 'letsyntax             'swindle-indent-function 1)
(put 'let-syntax            'swindle-indent-function 1)
(put 'letrec-syntax         'swindle-indent-function 1)
(put 'syntax-rules          'swindle-indent-function 1)

(put 'call-with-input-file  'swindle-indent-function 1)
(put 'with-input-from-file  'swindle-indent-function 1)
(put 'with-input-from-port  'swindle-indent-function 1)
(put 'call-with-output-file 'swindle-indent-function 1)
(put 'with-output-to-file   'swindle-indent-function 1)
(put 'with-output-to-port   'swindle-indent-function 1)
(put 'with-slots            'swindle-indent-function 2)
(put 'with-accessors        'swindle-indent-function 2)
(put 'call-with-values      'swindle-indent-function 1) ; r5rs?
(put 'dynamic-wind          'swindle-indent-function 'defun) ; r5rs?

(put 'if                    'swindle-indent-function 1)
(put 'method                'swindle-indent-function 1)
(put 'beforemethod          'swindle-indent-function 1)
(put 'aftermethod           'swindle-indent-function 1)
(put 'aroundmethod          'swindle-indent-function 1)
(put 'when                  'swindle-indent-function 1)
(put 'unless                'swindle-indent-function 1)
(put 'thunk                 'swindle-indent-function 0)
(put 'while                 'swindle-indent-function 1)
(put 'until                 'swindle-indent-function 1)
(put 'parameterize          'swindle-indent-function 1)
(put 'with-handlers         'swindle-indent-function 1)
(put 'begin0                'swindle-indent-function 1)
(put 'with-output-to-string 'swindle-indent-function 0)
(put 'ignore-errors         'swindle-indent-function 0)
(put 'no-errors             'swindle-indent-function 0)
(put 'matcher               'swindle-indent-function 1)
(put 'match                 'swindle-indent-function 1)
(put 'regexper              'swindle-indent-function 1)
(put 'dotimes               'swindle-indent-function 1)
(put 'dolist                'swindle-indent-function 1)

(put 'with-syntax           'swindle-indent-function 1)
(put 'syntax-case           'swindle-indent-function 2)
(put 'module                'swindle-indent-function 2)

(put 'syntax                'swindle-indent-function 0)
(put 'quasisyntax           'swindle-indent-function 0)
(put 'syntax/loc            'swindle-indent-function 1)
(put 'quasisyntax/loc       'swindle-indent-function 1)



(provide 'swindle)

;;; swindle.el ends here
