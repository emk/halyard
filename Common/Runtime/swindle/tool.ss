;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;; This allows adding a Swindle icon on startup, and also a GCalc button
(module tool mzscheme
  (require (lib "unitsig.ss")
           (lib "tool.ss" "drscheme")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (rename (lib "browser.ss" "net") send-url send-url)
           (lib "string-constant.ss" "string-constants"))
  (provide tool@)
  (define tool@
    (unit/sig drscheme:tool-exports^ (import drscheme:tool^)
      (define swindle-url "http://www.barzilay.org/Swindle/")
      ;; Swindle languages
      (define (swindle-language
               l-name l-module l-entry-name l-one-line l-sensitive? l-url)
        (class (drscheme:language:module-based-language->language-mixin
                (drscheme:language:simple-module-based-language->module-based-language-mixin
                 (class* object%
                         (drscheme:language:simple-module-based-language<%>)
                   (define/public (get-language-numbers) '(0 0))
                   (define/public (get-language-position)
                     (list "Swindle" l-entry-name))
                   (define/public (get-module) l-module)
                   (define/public (get-one-line-summary) l-one-line)
                   (define/public (get-language-url) l-url)
                   (define/public (get-reader) read-syntax)
                   (super-instantiate ()))))
          (define/override (default-settings)
            (drscheme:language:make-simple-settings
             l-sensitive? 'current-print 'mixed-fraction-e #f #t 'debug))
          (define/override (get-language-name) l-name)
          (define swindle-delta (make-object style-delta%))
          (send swindle-delta
                set-delta-background (make-object color% 255 255 128))
          (define/override (get-style-delta) swindle-delta)
          (define/override (config-panel parent)
            (let* ((make-panel
                    (lambda (msg contents)
                      (make-object message% msg parent)
                      (let ((p (instantiate vertical-panel% ()
                                 (parent parent)
                                 (style '(border))
                                 (alignment '(left center)))))
                        (if (string? contents)
                          (make-object message% contents p)
                          (contents p)))))
                   (title-panel
                    (instantiate horizontal-panel% ()
                      (parent parent)
                      (alignment '(center top))))
                   (title (make-object message% "Swindle Setup" title-panel))
                   (title-pic
                    (make-object message%
                      (make-object bitmap%
                        (build-path (collection-path "swindle")
                                    "icons" "swindle-logo.gif"))
                      title-panel))
                   (input
                    (make-panel (string-constant input-syntax)
                                (if l-sensitive?
                                  "always case-sensitive"
                                  (lambda (p)
                                    (make-object check-box%
                                      (string-constant case-sensitive-label)
                                      p void)))))
                   (debugging
                    (make-panel
                     (string-constant dynamic-properties)
                     (lambda (p)
                       (instantiate radio-box% ()
                         (label #f)
                         (choices
                          `(,(string-constant no-debugging-or-profiling)
                            ,(string-constant debugging)
                            ,(string-constant debugging-and-profiling)))
                         (parent p)
                         (callback void)))))
                   (output
                    (make-panel (string-constant output-style-label)
                                "always current-print")))
              (case-lambda
               (()
                (drscheme:language:make-simple-settings
                 (or l-sensitive? (send input get-value))
                 'current-print 'mixed-fraction-e #f #t
                 (case (send debugging get-selection)
                   [(0) 'none]
                   [(1) 'debug]
                   [(2) 'debug/profile])))
               ((settings)
                (unless l-sensitive?
                  (send input set-value
                        (drscheme:language:simple-settings-case-sensitive
                         settings)))
                (send debugging set-selection
                      (case (drscheme:language:simple-settings-annotations
                             settings)
                        [(none) 0]
                        [(debug) 1]
                        [(debug/profile) 2]))))))
               (super-instantiate ())))
      (define (add-swindle-language
               name module entry-name one-line sensitive? url)
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin)
                       (swindle-language
                        name
                        `(lib ,(string-append module ".ss") "swindle")
                        entry-name one-line sensitive? url)))))
      (define phase1 void)
      (define (phase2)
        (for-each (lambda (args)
                    (apply add-swindle-language `(,@args ,swindle-url)))
                  '(("Swindle" "swindle" "Full Swindle"
                     "Full Swindle extensions" #f)
                    ("Swindle w/o CLOS" "turbo" "Swindle without CLOS"
                     "Basic Swindle: no object system" #f)
                    ("HTML Swindle" "html" "HTML Swindle"
                     "Basic Swindle and HTML extensions" #t)))
        (parameterize ((current-directory (collection-path "swindle")))
          (define (do-customize file)
            (when (regexp-match "\\.ss$" file)
              (with-input-from-file file
                (lambda ()
                  (let ((l (read-line)))
                    (when (regexp-match "^;+ *CustomSwindle *$" l)
                      (let ((file (regexp-replace "\\.ss$" file ""))
                            (name #f) (dname #f) (one-line #f) (url #f))
                        (let loop ((l (read-line)))
                          (cond
                           ((regexp-match "^;+ *([A-Z][A-Za-z]*): *(.*)$" l) =>
                            (lambda (m)
                              (let ((sym (string->symbol (cadr m)))
                                    (val (caddr m)))
                                (case sym
                                  ((|Name|)       (set! name     val))
                                  ((|DialogName|) (set! dname    val))
                                  ((|OneLine|)    (set! one-line val))
                                  ((|URL|)        (set! url      val)))
                                (loop (read-line)))))))
                        (unless name (set! name file))
                        (unless dname (set! dname name))
                        (unless one-line
                          (set! one-line
                                (string-append "Customized Swindle: " name)))
                        (unless url (set! url swindle-url))
                        (add-swindle-language
                         name file dname one-line #f url))))))))
          (for-each do-customize (directory-list))))
      ;; GCalc button and menu entry
      (define show-gcalc?
        (directory-exists? (build-path (collection-path "swindle") "gcalc")))
      (define gcalc-bitmap
        (and show-gcalc? (drscheme:unit:make-bitmap
                          "GCalc" (build-path (collection-path "swindle")
                                              "gcalc" "gcalc.gif"))))
      (define (gcalc-unit-frame% super%)
        (class* super% ()
          (super-instantiate ())
          (rename (super-help-menu:after-about help-menu:after-about))
          (define/override (help-menu:after-about menu)
            (instantiate menu-item% ()
              (label "Swindle Home")
              (parent menu)
              (callback (lambda (item evt)
                          (send-url "http://www.barzilay.org/Swindle/"))))
            (super-help-menu:after-about menu))
          (let* ((p (send this get-button-panel))
                 (b (make-object button% (gcalc-bitmap this) p
                                 (lambda (b e)
                                   (send (dynamic-require
                                          '(lib "gcalc.ss" "swindle" "gcalc")
                                          'gcalc-frame)
                                         show #t)))))
            (send p change-children (lambda (cs) (cons b (remq b cs)))))))
      (when show-gcalc?
        (drscheme:get/extend:extend-unit-frame gcalc-unit-frame%)))))
