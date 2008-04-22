;; PORTED
(module media (lib "halyard.ss" "halyard")
  (require (lib "proxy-initialize-and-methods.ss" "halyard"))
  (require (file "base.ss"))

  (provide start-ambient kill-ambient)
  
  ;;; Live demo toy: Play some audio in the background.
  (define (start-ambient)
    (new-movie (rect 0 0 0 0) "quackery_vp3_captioned.mov"
               :name 'ambient :audio-only? #t :loop? #t))
  
  ;;; Live demo toy: Kill our background audio.
  (define (kill-ambient)
    (delete-element-if-exists 'ambient))

  ;;; The size of our QuickTime movies.
  (define $default-movie-shape (shape 384 288))

  ;;; The size of our QuickTime movies, with enough space for a controller.
  (define $default-movie-and-controller-shape (shape 384 (+ 288 16)))
  
  ;;; The number of pixels to offset a caption from a movie.
  (define $default-caption-offset 10)

  ;;; The default height to use for caption text.
  (define $default-caption-height
    (* 2 (rect-height (measure-text $caption-style "A"))))

  ;;; Displays a movie or audio caption against a black background.
  (define-class %captioned-card% (%black-test-card%)
    (text-box caption ((rect 100 475 700 590) $caption-style ""))
    
    ;; Caption events are passed from media elements to their parents (like
    ;; all regular events), so we can intercept them at the card level and
    ;; redirect them to a location of our choice.
    (def (media-caption event)
      (set! ((.caption) .text) (event-caption event))))

  ;;; A card which displays a single movie.
  (define-class %movie-card% (%captioned-card%)
    (attr path :type <string>)
    (attr movie-shape $default-movie-shape)
    (attr controller? #f)
      
    ;;; Note that if we create movies with ELEM, they're going to start
    ;;; playing almost immediately.  This only works if you want the movie
    ;;; to start as soon as you display the card.  Otherwise, you need to
    ;;; create the movie in RUN.
    (movie movie ((move-rect-center-to (.movie-shape)
                                       (rect-center $screen-rect))
                  (.path) :controller? (.controller?)))
    )

  ;;; A sample "proxy" class which contains a caption and a movie, and which
  ;;; acts as though it were a regular movie.
  (define-class %captioned-movie% (%box%)
    (proxy-initialize-and-methods movie %movie%)

    ;; Convert a %widget%-style :rect into a %custom-element%-style :at and
    ;; :shape.
    ;; TODO - See case 2644 for cleaning up rect/at silliness.
    (attr movie-rect $default-movie-shape)
    (value at (rect-left-top (.movie-rect)))
    (value shape (shape (rect-width (.movie-rect))
                        (+ (rect-height (.movie-rect))
                           $default-caption-offset $default-caption-height)))

    (setup
      (.add-child-initializer! :rect (rect-shape (.movie-rect)))
      (.create-child-element))

    (text-box caption ((rect 0 (+ (rect-height (.movie-rect))
                                  $default-caption-offset)
                             (rect-width (.shape)) (rect-height (.shape)))
                       $caption-style ""))
    
    (def (media-caption event)
      (set! ((.caption) .text) (event-caption event)))
    )

  (group /media)


  ;;=======================================================================
  ;;  QuickTime
  ;;=======================================================================

  (group /media/qt)
  
  (card /media/qt/video-captions (%movie-card%)
    (value title "Video Captions")
    (value path "duck_and_cover_intro_vp3_captioned.mov"))

  (card /media/qt/external-video-captions (%movie-card%)
    (value title "External Video Captions")
    (value path "quackery_vp3.mov")
    (value controller? #t)
    (value movie-shape $default-movie-and-controller-shape))

  (card /media/qt/wait-test (%movie-card%) 
    (value title "Wait Test") 
    (value path "quackery_vp3.mov")

    (run
      (define (caption msg)
        (set! ((.caption) .text) msg))
      
      ((.movie) .wait (tc 3 00))
      (caption "Hi, Raymond!")
      ((.movie) .wait (tc 5 0))
      (caption "We're listening...")
      ((.movie) .wait (tc 8 0))
      (caption "")
      ((.movie) .wait (tc 23 0))
      (caption "No Z-rays?  Gosh!")
      ((.movie) .wait (tc 26 0))
      (caption "")
      ((.movie) .wait)
      (delete-element (.movie))
      (caption "The End.")))
  
  (define-class %self-deleting-movie% (%movie%)
    ;; We need something to actually test media-finished and self deletion,
    ;; so let's do this.  You probably wouldn't want to do this in regular
    ;; code unless you were up to something clever.
    (def (media-finished event)
      (delete-element self)))

  
  (card /media/qt/movies (%captioned-card%) 
    (value title "Multiple Movies")

    (define $rect1 (move-rect-top-to $default-movie-and-controller-shape 50))
    (define $rect2 (move-rect-right-to $rect1 (rect-right $screen-rect)))

    (movie movie1 ($rect1 "duck_and_cover_intro_vp3_captioned.mov"
                   :controller? #t)
      (setup
        ;; Because we haven't fixed case 2359, we need to pause a movie
        ;; manually right after we create it.
        (.pause)))

    (text-button pause ((below (.movie1) 20) "Pause" :command 'pause-movie1))
    (def (pause-movie1)
      ((.movie1) .pause))
    
    (text-button resume ((to-the-right-of (.pause) 10) "Resume"
                         :command 'resume-movie1))
    (def (resume-movie1)
      ((.movie1) .resume))

    (text-button show-2nd ((to-the-right-of (.resume) 10) "Show Second Movie"
                           :shown? #f)
      ;; Let's override CLICK here, just to be different.
      (def (click)
        (set! (.enabled?) #f)
        (%self-deleting-movie% .new :rect $rect2
                                    :path "quackery_vp3.mov")))
    
    (run      
      ((.movie1) .wait (tc 10 00))
      (set! ((.show-2nd) .shown?) #t)))  

  (card /media/qt/attached-captions (%black-test-card%)
    (value title "Attached captions")
    (elem movie (%captioned-movie% :movie-rect (move-rect-center-to
                                                $default-movie-shape
                                                (rect-center $screen-rect))
                                   :path "quackery_vp3.mov"))
    (run
      ;; Make sure we proxy WAIT correctly.
      ((.movie) .wait)
      (jump @index))
    )

  ;; Under Vista, I have a number of problems with this card.  In
  ;; particular, it gives a video playback error when run as an
  ;; Administrator, and will generally freeze on the first frame of video
  ;; when run with Aero.  Not ready for prime time!
  (card /media/qt/xiph-qt-codec (%black-test-card%)
    (value title "XiphQT Ogg Theora video (experimental, codec required)")

    (text-button get-codec ((below @title-elem 10) "Get Codec")
      (def (click)
        (open-in-browser "http://www.xiph.org/quicktime/")))

    (elem movie (%captioned-movie%
                 :movie-rect (move-rect-center-to
                              $default-movie-and-controller-shape
                              (rect-center $screen-rect))
                 :path "duck_and_cover_intro_theora.ogg"
                 :controller? #t)))

  (group /media/qt/missing)
  
  (define-class %qtvr-card% (%white-test-card%)
    (attr movie-shape :type <rect>)
    (attr path :type <string>)
    (movie qtvr ((move-rect-center-to (.movie-shape)
                                      (rect-center $screen-rect))
                 (.path) :interaction? #t))
    )
                             

  (card /media/qt/missing/qtvr1 (%qtvr-card%
                                :title "QTVR Demo #1 (media missing)"
                                :movie-shape (shape 320 320)
                                :path "powerbook17_jan2003_qtvr320.mov"))

  (card /media/qt/missing/qtvr2 (%qtvr-card%
                                :title "QTVR Demo #2 (media missing)"
                                :movie-shape (shape 480 480)
                                :path "powerbook17_jan2003_qtvr480.mov"))
  
  
  ;;=======================================================================
  ;;  Audio Streams
  ;;=======================================================================

  ;; TODO - Is there a better place to put these?  We can technically
  ;; lexically scope them within the class, but that is confusing.
  ;; Should they be methods or something?
  (define box-count 4)
  (define (box-count->volume count)
    (/ count (- box-count 1)))
  (define (box-rect n)
    (define left 1)
    (define top  (+ (* (- box-count n 1) 11) 1))
    (rect left top (+ left 10) (+ top 10)))
    
  (define-class %volume-control% (%custom-element%) 
    (attr channel) 
    (value alpha? #t)
    (value shape (shape 12 45))

    (def (stream)
      ((.parent) .stream))

    ;; TODO - This is an internal slot. We should be using (slot
    ;; 'box-fill-limit) directly, but that's a pain.  We need to
    ;; actually create an appropriate wrapper for defining private
    ;; slots.
    (attr box-fill-limit (1- box-count) :writable? #t) ; Redesign me.

    (def (draw)
      (for [n 0 (< n box-count) (+ n 1)]
        (draw-rectangle (box-rect n)
                        (if (<= n (.box-fill-limit))
                          (color #x00 #x00 #xFF)
                          (color #xFF #xFF #xFF)))
        (draw-rectangle-outline (box-rect n) (color #x00 #x00 #x00) 1)))

    ;; TODO - Private method
    (def (do-set-volume n)
      (set! (.box-fill-limit) n)
      (.invalidate)
      ((.stream) .set-channel-volume! (.channel) (box-count->volume n)))

    (def (mouse-down event)
      (define p (event-position event))
      (for [n 0 (< n box-count) (+ n 1)]
        (when (point-in-shape? p (offset-by-point (box-rect n) (.at)))
          (.do-set-volume n))))
    )

  ;; TODO - We used to format this like one of the transition test cards.
  ;; Maybe we can factor out a common "big black text centered one a white
  ;; screen" template and use it for both?
  (define-class %audio-stream-card% (%fancy-white-test-card%)
    (elem left (%volume-control%
                :at (point (- (rect-right $screen-rect) 26)
                           (- (rect-bottom $screen-rect) 46))
                :channel 'left))
    (elem right (%volume-control% :at (to-the-right-of (.left) 1)
                                  :channel 'right))

    (elem caption
        (%text-box%
         :bounds (rect (rect-left $screen-rect)
                       (- (rect-bottom $screen-rect) 100)
                       (rect-right $screen-rect)
                       (rect-width $screen-rect))
         :style (stylesheet :base $caption-style :color $color-black)
         :text ""))
    
    (def (media-caption event)
      (set! ((.caption) .text) (event-caption event)))
    )

  (group /media/audiostream)
  (group /media/audiostream/vorbis)

  (card /media/audiostream/vorbis/stereo
      (%audio-stream-card% :title "Vorbis Stereo\n(Looping)")
    (vorbis-audio stream ("oggtest-stereo.ogg" :loop? #t)))

  (card /media/audiostream/vorbis/mono 
      (%audio-stream-card% :title "Vorbis Mono")
    (vorbis-audio stream ("oggtest-mono.ogg")))

  (card /media/audiostream/vorbis/twobeeps 
      (%audio-stream-card% :title "Vorbis Two Beeps\n(Broken)")
    (vorbis-audio stream ("oggtest-twobeeps.ogg")))

  (card /media/audiostream/vorbis/long 
      (%audio-stream-card% :title "Long Vorbis\n(FDA Advisory)")
    (vorbis-audio stream ("quackery.ogg")))

  (group /media/audiostream/geiger)

  (card /media/audiostream/geiger/synth 
      (%audio-stream-card% :title "Geiger Counter")
    (geiger-audio stream ("oggtest-geiger-chirp.ogg")
      (setup
        (.set-counts-per-second! 10.0))))

  (group /media/audiostream/geiger/loop)

  (card /media/audiostream/geiger/loop/rate-point8mrph
      (%audio-stream-card% :title "Ludlum 0.8 mRph")
    (vorbis-audio stream ("ludlum/lud-mod14c-00_8mRph.ogg" :loop? #t)))

  (card /media/audiostream/geiger/loop/rate-2mrph
      (%audio-stream-card% :title "Ludlum 2 mRph")
    (vorbis-audio stream ("ludlum/lud-mod14c-02_0mRph.ogg" :loop? #t)))

  (card /media/audiostream/geiger/loop/rate-5mrph
      (%audio-stream-card% :title "Ludlum 5 mRph")
    (vorbis-audio stream ("ludlum/lud-mod14c-05_0mRph.ogg" :loop? #t)))

  (card /media/audiostream/geiger/loop/rate-10mrph
      (%audio-stream-card% :title "Ludlum 10 mRph") 
    (vorbis-audio stream ("ludlum/lud-mod14c-10_0mRph.ogg" :loop? #t)))

  (card /media/audiostream/geiger/loop/rate-50mrph
      (%audio-stream-card% :title "Ludlum 50 mRph")
    (vorbis-audio stream ("ludlum/lud-mod14c-50_0mRph.ogg" :loop? #t)))

  (card /media/audiostream/sine
      (%audio-stream-card% :title "440Hz Sine Wave\n(Synthesized)")
    (sine-wave stream (440)))
  
  
  ;;=======================================================================
  ;;  Complete Geiger Counter Synth
  ;;=======================================================================

  (card /media/geiger-synth (%audio-stream-card%)
    (value title "Geiger Synth")
    (def (stream) @stream)
    (run
      (set! (state-db '/synth/cps) 0.0)
      (%geiger-synth% .new
        :name 'stream
        :state-path '/synth/cps
        :chirp "oggtest-geiger-chirp.ogg"
        :loops '(0.8 "ludlum/lud-mod14c-00_8mRph.ogg"
                 2.0 "ludlum/lud-mod14c-02_0mRph.ogg"
                 5.0 "ludlum/lud-mod14c-05_0mRph.ogg"
                 10.0 "ludlum/lud-mod14c-10_0mRph.ogg"
                 50.0 "ludlum/lud-mod14c-50_0mRph.ogg")))
    
    (def (idle)
      ;;(define loc (event-position event))
      (define loc (mouse-position))
      ;; Manually cancelling out the sqrt with the (* dist dist)
      (define dist-squared (+ (* (point-x loc) (point-x loc))
                              (* (point-y loc) (point-y loc))))
      (define denominator (if (= dist-squared 0) 
                              1/2
                              dist-squared))
      (define cps (* 100000 (/ 1.0 denominator)))
      (set! (state-db '/synth/cps) cps)))
  
  )