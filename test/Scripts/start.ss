;; PORTED
;;=========================================================================
;;  Tamale Demo and Test Script
;;=========================================================================
;;  The goal of this script is to demonstrate good Tamale coding style,
;;  show how all of Tamale's features are used, and provide a way for
;;  Tamale programmers to test whether a given feature is working.
;;
;;  Tamale is still very much in flux, and we won't freeze any APIs until
;;  version 1.0 is released.  And quite frankly, we *do* need to clean up
;;  a bunch of stuff before it's ready for use by the general public.
;;  We apologize for any temporary inconveniences.


;;=========================================================================
;;  Global Setup and Initialization
;;=========================================================================
;;  At the top of each script, we can include any modules we'll need,
;;  define global variables, and set a few internal parameters.

;; Our local library of templates, functions, etc.
(require "base.ss")

;; This file is only included here so that the syntax highlighter knows
;; about it.

;; We want this for a version number on the index card.
;;(require (lib "updater.ss" "5L"))

;; XXX - These modules are used in various cards below, but they're
;; probably obsolete and are expected to go away or at least be
;; removed from the core.
(require (lib "data-file.ss" "5L"))


;; Other non-Runtime files we'll be using appear further down in the script.

;; Tamale has an internal graphics cache.  If you make this cache
;; larger, Tamale uses more memory but displays graphics faster.
;; The default size is probably reasonable.
(set-image-cache-size! (* 4 1024 1024))


;;=========================================================================
;;  Global Event Handlers
;;=========================================================================
;;  Tamale uses a HyperCard-like event-handling model: Events are first
;;  passed to individual elements on the screen (in the case of mouse
;;  events), or to the card itself (in the case of key events).  If an
;;  event isn't handled, it travels up the containment hierarchy:
;;
;;    Elements... -> Card -> Groups/Sequences... -> Program
;;
;;  Here are our global, program-level event handlers.

(with-instance %root-node%
  (def (char event)
    (cond
     [(and (equal? (event-character event) #\i)
           (equal? (event-modifiers event) '(alt)))
      (jump index)]
     [else (super)])))


;;=========================================================================
;;  Cards
;;=========================================================================
;;  Our actual script.

;; Scripts begin running at the start card.  In our case, we'll just
;; immediately JUMP to our index card, below, without bothering to display
;; anything.  The JUMP function changes the current card.
(card start (%card%)
  (run
    (debug-log "Hello, world!  IT'S ALIIIIVE!")
    (jump index)))

(define-class %menu-item% (%box%)
  (attr y :type <integer>)
  (attr text :type <string>)
  (attr jump-to) ; TODO - Add :type specifier.  See case 1394.
  
  (value at (point 0 (.y)))
  (value shape (shape 800 100))
  (value clickable-where-transparent? #t)
  
  (centered-text label ($menu-style (.text) :max-width (rect-width (.shape))))

  (def (mouse-down event)
    (jump (.jump-to))))

(define-node-helper menu-item (y text jump-to) %menu-item%)

;; The index card is based on our %simple-card% template.
(card index (%black-test-card% :title "Tamale Features (updated)")
  (menu-item controls ( 80 "Controls"    @features/controls))
  (menu-item movies   (180 "More Movies" @media/qt/movies))
  (text release-id
      ((point 10 580) $title-style (or #| (program-release-id) |# "")))
  )


;;=========================================================================
;;  Other Files in This Script
;;=========================================================================

(require (file "media.ss"))
(require (file "features.ss"))
(require (file "bugs.ss"))
(require (file "experiments.ss"))
(require (file "test-cases.ss"))
;;(require (file "updater-example.ss"))
;;(require (file "quake2-example.ss"))

;; Test cases from the standard library.
(require (file "ruby-objects-test.ss"))
(require (lib "tamale-unit-test.ss" "5L"))
(require (lib "updater-test.ss" "5L"))

;; These files are included in start.ss only to make sure we get
;; appropriate highlighting and indentation for the identifiers they
;; export.
(require (lib "animate.ss" "5L"))
(require (lib "tamale-unit.ss" "5L"))
