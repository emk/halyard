;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2008 Trustees of Dartmouth College
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.
;;
;; @END_LICENSE

(module api (lib "mizzen.ss" "mizzen")


  ;;=======================================================================
  ;;  Selective exports
  ;;=======================================================================
  ;;  We only want to export the truly public parts of these files, not the
  ;;  internal implementation details.

  (require (lib "nodes.ss" "halyard/private"))
  (provide jump delete-element current-group-member current-card
           static-root-node running-root-node
           define-def-and-super-abbrev setup run *node-defined-hook*
           %node% node? static-node? node-name node-full-name node-parent
           node-elements find-child-node %group-member%
           group %card-group% group-members card-group? %root-node%
           %card% card? card jump-current element? %element% elem

           ;; The following APIs are either ancient, or a little bit
           ;; dubious.  Could we remove some of these or make them private?
           last-card-visited
           find-node find-running-node find-static-node
           default-element-parent call-with-default-element-parent
           with-default-element-parent)

  (require (lib "kernel.ss" "halyard/private"))
  (provide *enter-card-hook* *exit-card-hook*
           *card-body-finished-hook* *before-draw-hook*
           *dangerous-exit-script-hook*
           runtime-directory value->boolean idle
           engine-var set-engine-var! engine-var-exists? define/p
           exit-script refresh
           run-deferred executing-deferred-safe-time-callbacks?

           ;; More ancient APIs that we may want to do something about.
           find-card card-exists? card-name)


  ;;=======================================================================
  ;;  Re-export Low-level APIs
  ;;=======================================================================
  ;;  We want to include all these files in our public API.

  (require (lib "begin-var.ss" "mizzen"))
  (provide (all-from (lib "begin-var.ss" "mizzen")))
  
  (require (lib "hook.ss" "halyard/private"))
  (provide (all-from (lib "hook.ss" "halyard/private")))

  (require (lib "indent.ss" "halyard/private"))
  (provide (all-from (lib "indent.ss" "halyard/private")))

  (require (lib "types.ss" "halyard/private"))
  (provide (all-from (lib "types.ss" "halyard/private")))

  (require (lib "util.ss" "mizzen"))
  (provide (all-from (lib "util.ss" "mizzen")))

  (require (lib "util.ss" "halyard/private"))
  (provide (all-from (lib "util.ss" "halyard/private")))

  (require (lib "events.ss" "halyard/private"))
  (provide (all-from (lib "events.ss" "halyard/private")))

  (require (lib "shapes.ss" "halyard/private"))
  (provide (all-from (lib "shapes.ss" "halyard/private")))

  (require (lib "interpolate.ss" "halyard/private"))
  (provide (all-from (lib "interpolate.ss" "halyard/private")))

  (require (lib "content-paths.ss" "halyard/private"))
  (provide (all-from (lib "content-paths.ss" "halyard/private")))

  (require (lib "external-nodes.ss" "halyard/private"))
  (provide (all-from (lib "external-nodes.ss" "halyard/private")))


  ;;=======================================================================
  ;;  Support Modules
  ;;=======================================================================
  
  (require (lib "trace.ss" "halyard/private"))
  (provide with-tracing)
  (set-trace-output-printer! debug-log)

  (require (rename (lib "match.ss") match-let match-let))
  (provide match-let)

  ;; Make sure this gets loaded.  It will register itself with the kernel.
  (require (lib "tags.ss" "halyard/private"))
  (provide (all-from (lib "tags.ss" "halyard/private")))

  
  ;;;======================================================================
  ;;;  Useful Syntax
  ;;;======================================================================

  (provide callback deferred-callback for)

  ;;; Create an anonymous function object with no parameters.
  ;;;
  ;;; @syntax (callback body ...)
  ;;; @param BODY body The body of the function.
  (define-syntax callback
    (syntax-rules ()
      [(callback code ...)
       (lambda () (begin/var code ...))]))
  (define-syntax-indent callback 0)

  ;;; Create an anonymous function object with no parameters.  When
  ;;; invoked, this function object may run immediately, or it may run at
  ;;; a later time.  You should use deferred callbacks for code
  ;;; which needs to be run in response to a mouse event or key press, and
  ;;; which needs to play video, request user input or sleep.
  ;;;
  ;;; The callback will always return false.
  ;;;
  ;;; @syntax (callback body ...)
  ;;; @param BODY body The body of the function.
  (define-syntax deferred-callback
    (syntax-rules ()
      [(deferred-callback code ...)
       (callback (run-deferred (callback code ...)))]))
  (define-syntax-indent deferred-callback 0)

  ;;; Run a body of code until a condition is met, updating a loop variable
  ;;; as specified.  This works in a fashion similar to C's 'for' loop.
  ;;;
  ;;; @syntax (for [name init-value cond next-value] body ...)
  ;;; @param NAME name The name of the loop variable.
  ;;; @param EXPRESSION init-value The initial value of 'name' before starting
  ;;;   the loop.
  ;;; @param EXPRESSION cond This expression is tested before each pass
  ;;;   through the loop.  If it returns false, the loop will end.
  ;;; @param EXPRESSION next-value An expression to calculate the value of
  ;;;   'name' for the next trip through the loop.
  (define-syntax for
    (syntax-rules ()
      [(for [name init-value cond next-value] body ...)
       (let loop [[name init-value]]
         (when cond
           (begin/var body ...)
           (loop next-value)))]))
  (define-syntax-indent for 1)


  ;;;======================================================================
  ;;;  Useful Constants
  ;;;======================================================================

  (provide $screen-rect)

  ;;; @type RECT The screen rectangle, in global co-ordinates.
  (define $screen-rect (rect 0 0 800 600))


  ;;;======================================================================
  ;;;  Mathematical Primitives
  ;;;======================================================================

  (provide real->string)

  ;;; Format a real number as "x.xxx".
  ;;;
  ;;; @param REAL n The number to format.
  ;;; @param INTEGER places The number of places past the decimal point.
  (define (real->string n places)
    (define (pad-with-zeros str wanted-length)
      (if (>= (string-length str) wanted-length)
          str
          (pad-with-zeros (string-append "0" str) wanted-length)))
    (define sign (if (< n 0) "-" ""))
    ;; PORTING - This next line isn't completely portable Scheme--too many
    ;; behaviors of the number hierarchy are undefined.
    (define scaled (inexact->exact (round (* (abs n) (expt 10 places)))))
    (define str (pad-with-zeros (number->string scaled) (+ places 1)))
    (define split-pos (- (string-length str) places))
    (cat sign
         (substring str 0 split-pos)
         "."
         (substring str split-pos (string-length str))))


  ;;;======================================================================
  ;;;  Text Drawing
  ;;;======================================================================
  ;;;  Halyard text drawing uses stylesheets.
  
  (provide *stylesheet-list* (rename register-style stylesheet)
           stylesheet? stylesheet-name stylesheet-long-name stylesheet-family
           stylesheet-size stylesheet-flags
           stylesheet-justification stylesheet-color
           stylesheet-highlight-color stylesheet-height-adjustment
           stylesheet-shadow-offset stylesheet-shadow-color
           stylesheet-highlight-shadow-color
           define-stylesheet measure-text draw-text)

  ;;; Holds a list of all registered stylesheets.  
  (define *stylesheet-list* (list))

  (define-struct stylesheet
    (name long-name family size flags justification
     color highlight-color height-adjustment
     shadow-offset shadow-color
     highlight-shadow-color)
    (make-inspector))
  
  ;; Helper: Convert a list of flags to a defstyle flag value.
  (define (flags->defstyle-flags flags)
    (cond
     [(null? flags) 'r]
     [(equal? flags '(bold)) 'b]
     [(equal? flags '(italic)) 'i]
     [(equal? flags '(bold italic)) 'bi]
     [(equal? flags '(italic bold)) 'bi]
     [else
      (error (cat "Unknown stylesheet flags: " flags))]))

  ;; Helper: Given a stylesheet, register a corresponding defstyle.
  (define (register-defstyle sheet)
    (call-prim 'DefStyle
                  (stylesheet-long-name sheet)
                  (stylesheet-family sheet)
                  (stylesheet-size sheet)
                  (flags->defstyle-flags (stylesheet-flags sheet))
                  (stylesheet-justification sheet)
                  (stylesheet-color sheet)
                  (stylesheet-highlight-color sheet)
                  (stylesheet-height-adjustment sheet)
                  (stylesheet-shadow-offset sheet)
                  (stylesheet-shadow-color sheet)
                  (stylesheet-highlight-shadow-color sheet))
    ;; XXX - Ugly hack for stylesheet display utilities in VTRA program.
    ;; (We don't want to display anonymous stylesheets, though.)
    (when (stylesheet-name sheet)
      (set! *stylesheet-list* (cons sheet *stylesheet-list*))))

  ;; Internal hash table containing the STYLESHEET-LONG-NAME of all
  ;; registered stylesheets.
  (define *registered-styles* (make-hash-table))

  ;; Helper: If we haven't already registered an equivalent style sheet,
  ;; register this one.
  (define (maybe-register-defstyle sheet)
    (define name (stylesheet-long-name sheet))
    (unless (hash-table-get *registered-styles* name (lambda () #f))
      (hash-table-put! *registered-styles* name #t)
      (register-defstyle sheet)))

  ;; Helper: Creates nice, long, X11-inspired style names so we never
  ;; register any duplicates.
  (define (style->long-name name family size flags justification
                            text-color highlight-color height-adjustment
                            shadow-offset shadow-color
                            highlight-shadow-color)
    (define c color->hex-string)
    (define (v-or-p value)
      (if (percent? value)
          (cat (percent-value value) "%")
          value))
    (symcat family "-" size "-" (flags->defstyle-flags flags) "-"
            justification "-" (c text-color) "-"
            (c highlight-color) "-" (v-or-p height-adjustment) "-"
            shadow-offset "-" (c shadow-color) "-"
            (c highlight-shadow-color)))

  ;; An internal helper function which does all the heavy lifting.
  (define (register-style &key
                          [name #f]
                          [base #f]
                          [family (if base (stylesheet-family base) "Times")]
                          [size (if base (stylesheet-size base) 12)]
                          [flags (if base (stylesheet-flags base) '())]
                          [justification
                           (if base (stylesheet-justification base) 'left)]
                          [text-color :color
                           (if base
                               (stylesheet-color base)
                               (color #xFF #xFF #xFF))]
                          [highlight-color
                           (if base
                               (stylesheet-highlight-color base)
                               (color #xFF #xFF #xFF))]
                          [height-adjustment
                           (if base (stylesheet-height-adjustment base) 0)]
                          [shadow-offset
                           (if base (stylesheet-shadow-offset base) 0)]
                          [shadow-color
                           (if base
                               (stylesheet-shadow-color base)
                               (color #x00 #x00 #x00))]
                          [highlight-shadow-color
                           (if base
                               (stylesheet-highlight-shadow-color base)
                               shadow-color)])
    (let* [[long-name (style->long-name name family size flags justification
                                        text-color highlight-color
                                        height-adjustment shadow-offset
                                        shadow-color highlight-shadow-color)]
           [sheet (make-stylesheet name long-name family size flags
                                   justification text-color highlight-color
                                   height-adjustment shadow-offset shadow-color
                                   highlight-shadow-color)]]
      (maybe-register-defstyle sheet)
      sheet))

  ;;; Define a new stylesheet for drawing text.
  ;;;
  ;;; @syntax (DEFINE-STYLESHEET name &key base family size flags justification
  ;;;                            color highlight-color height-adjustment
  ;;;                            shadow-offset shadow-color 
  ;;;                            highlight-shadow-color windows-adjustment)
  ;;; @param SYMBOL name The name of this stylesheet.
  ;;; @key STYLESHEET base An optional stylesheet from which to default
  ;;;   other keyword arguments.
  ;;; @key STRING family The font family to use.
  ;;; @key STRING size The font size.
  ;;; @key LIST:SYMBOL flags One of '(), '(bold), '(italic) or '(bold italic).
  ;;; @key SYMBOL justification One of 'left, 'center or 'right.
  ;;; @key COLOR color The text color.
  ;;; @key COLOR highlight-color The text highlight color.
  ;;; @key PERCENT leading Change in the default leading.
  ;;; @key INTEGER shadow-offset The offset for the drop, or 0, for no drop
  ;;;   shadow.  Positive offsets are down and to the right.
  ;;; @key COLOR shadow-color The color to use for the shadow.
  ;;; @key COLOR highlight-shadow-color The color to use the shadow of
  ;;;   highlighted text.
  ;;; @key INTEGER windows-adjustment The amount to adjust the first
  ;;;   baseline by when using the legacy Windows text subsystem.
  ;;; @xref draw-text input
  ;;; @legacy defstyle header
  (define-syntax define-stylesheet
    (syntax-rules ()
      [(define-stylesheet name args ...)
       (define name (register-style :name 'name args ...))]))
  (define-syntax-indent define-stylesheet 1)

  ;;; Draw a string of text.
  ;;;
  ;;; @param RECT r The recentangle in which to draw the text.
  ;;;   The text may extend below the bottom of this box if necessary.
  ;;; @param STYLESHEET style The stylesheet to use.
  ;;; @param STRING text The text to draw.
  ;;; @xref measure-text text-position *text-x* *text-y*
  ;;; @legacy text textaa
  (define (draw-text r style text)
    ;; XXX - TextAA uses an idiosyncratic formating language.
    (call-prim 'TextAA (stylesheet-long-name style) r text))
  
  ;;; Measure a string of text.
  ;;;
  ;;; @param STYLESHEET style The stylesheet to use.
  ;;; @param STRING text The text to measure.
  ;;; @key STRING max-width The maximum horizontal space which may be used
  ;;;   for laying out the text.  Defaults to the screen width.
  ;;; @xref draw-text
  (define (measure-text style text
                        &key (max-width (rect-width $screen-rect)))
    ;;; XXX - Note that we stomp the "saved-text-position" used by
    ;;; deprecated.ss.
    (call-prim 'MeasureTextAA (stylesheet-long-name style) text max-width))
  
  ) ; end module
