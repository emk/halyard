(module media (lib "5l.ss" "5L")
  (require (file "base.ss"))

  ;; Needed for CENTER-TEXT.
  (require (lib "deprecated.ss" "5L"))

  (provide start-ambient kill-ambient)
  
  ;;; Live demo toy: Play some audio in the background.
  (define (start-ambient)
    (movie (rect 0 0 0 0) "quackery_vp3_captioned.mov"
           :name 'ambient :audio-only? #t :loop? #t))
  
  ;;; Live demo toy: Kill our background audio.
  (define (kill-ambient)
    (delete-element-if-exists 'ambient))

  ;;; The size of our QuickTime movies.
  (define $default-movie-size (rect 0 0 384 288))

  ;;; The size of our QuickTime movies, with enough space for a controller.
  (define $default-movie-and-controller-size (rect 0 0 384 (+ 288 16)))
  
  ;;; Displays a movie or audio caption against a black background.
  (define-class %captioned-card% (%black-test-card%)
    (setup
      (text-box (rect 100 475 700 590) $caption-style "" :name 'caption))
    
    ;; Caption events are passed from media elements to their parents (like
    ;; all regular events), so we can intercept them at the card level and
    ;; redirect them to a location of our choice.
    (def (media-caption event)
      (set! (@caption .text) (event-caption event))))

  ;;; A card which displays a single movie.
  (define-class %movie-card% (%captioned-card%)
    (attr path :type <string>)
    (attr movie-size $default-movie-size)
    (attr controller? #f)
      
    (run
      ;; Figure out where we'll put our movie.
      (define center (rect-center $screen-rect))
      (define bounds (move-rect-center-to (.movie-size) center))
    
      ;; Create and run our movie.
      (movie bounds (.path) :name 'movie :controller? (.controller?))))

  (sequence media)


  ;;=======================================================================
  ;;  QuickTime
  ;;=======================================================================

  (sequence media/qt)
  
  (card media/qt/video-captions (%movie-card%)
    (value title "Video Captions")
    (value path "duck_and_cover_intro_vp3_captioned.mov"))

  (card media/qt/external-video-captions (%movie-card%)
    (value title "External Video Captions")
    (value path "quackery_vp3.mov")
    (value controller? #t)
    (value movie-size $default-movie-and-controller-size))

  (card media/qt/wait-test (%movie-card%) 
    (value title "Wait Test") 
    (value path "quackery_vp3.mov")

    (run
      (define (caption msg)
        (set! (@caption .text) msg))
      
      (wait @movie :frame (tc 3 00))
      (caption "Hi, Raymond!")
      (wait @movie :frame (tc 5 0))
      (caption "We're listening...")
      (wait @movie :frame (tc 8 0))
      (caption "")
      (wait @movie :frame (tc 23 0))
      (caption "No Z-rays?  Gosh!")
      (wait @movie :frame (tc 26 0))
      (caption "")
      (wait @movie)
      (delete-element @movie)
      (caption "The End.")))
  
  (define-class %self-deleting-movie% (%movie%)
    (def (media-finished event)
      (delete-element self)))
  
  (card media/qt/movies (%captioned-card%) 
    (value title "Multiple Movies")
    
    (run
      (define rect1 (move-rect-top-to $default-movie-and-controller-size 50))
      (define rect2 (move-rect-right-to rect1 (rect-right $screen-rect)))
      
      ;; Create our first movie and pause it.
      (refresh)
      (movie rect1 "duck_and_cover_intro_vp3_captioned.mov"
             :name 'movie1 :controller? #t)
      ;; TODO - Make this work.
      (@movie1 .pause)
      
      ;; Create our buttons.

      (text-button (below @movie1 20) "Pause"
                   (callback (@movie1 .pause))
                   :name 'pause)
      (text-button (to-the-right-of @pause 10) "Resume"
                   (callback (@movie1 .resume))
                   :name 'resume)
      (text-button (to-the-right-of @resume 10) "Show Second Movie"
                   (callback
                     (set! (@show-2nd .enabled?) #f)
                     (refresh)
                     (%self-deleting-movie% .new
                       :rect rect2
                       :path "quackery_vp3.mov"))
                   :name 'show-2nd :shown? #f)

      (wait @movie1 :frame (tc 10 00))
      (set! (@show-2nd .shown?) #t)))

  (sequence media/qt/missing)
  
  (card media/qt/missing/qtvr1 ()
    (setup
      (draw-white-background))
    
    (run
      (movie (move-rect-center-to (rect 0 0 320 320)
                                  (rect-center $screen-rect))
             "powerbook17_jan2003_qtvr320.mov"
             :interaction? #t)))
  
  (card media/qt/missing/qtvr2 ()
    (setup
      (draw-white-background))
    (run
      (movie (move-rect-center-to (rect 0 0 480 480)
                                  (rect-center $screen-rect))
             "powerbook17_jan2003_qtvr480.mov"
             :interaction? #t)))
  
  ;;=======================================================================
  ;;  Ogg Theora
  ;;=======================================================================
  
  (sequence media/ogg-theora)

  ;; ----- INSTALLING the XIPH QUICKTIME COMPONENTS FOR OGG PLAYBACK -----
  ;;
  ;; The current method of playing Ogg Theora video in Tamale is to install
  ;; QuickTime and the Xiph QuickTime Components.
  ;;
  ;; Installation:
  ;;
  ;; - Download the QuickTime Components from http://xiph.org/quicktime/
  ;; - Run the EXE
  ;; - Click "Ok" a dozen times.
  ;; - Restart tamale_test and the card should work.
  ;;
  ;; ---------------------------------------------------------------------
  
  ;; A first shot at a test card demonstrating ogg theora playback in Tamale.
  ;;
  ;; Hooray, it works!
  ;;
  ;; NOTE: if this card DOES NOT work for you, then you might need to install
  ;;   the Xiph QuickTime plugin for Ogg Theora (see above).
  (card media/ogg-theora/video-test ()
    (run
      (define video-shape (rect 0 0 640 480))
      (movie (offset-rect video-shape (point 100 100))
             "CougarAceListing24july2006.ogg")))
  
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
    (attr stream) 
    (attr channel) 
    (value alpha? #t)

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

  (define (volume-control p stream channel)
    (define x (point-x p))
    (define y (point-y p))
    (%volume-control% .new :bounds (rect x y (+ x 12) (+ y 45))
                           :stream stream
                           :channel channel))

  (define-class %audio-stream-card% (%card%) 
    (attr title)
    
    (setup
      (draw-white-background)
      (center-text $audio-stream-style $screen-rect (.title))
      (define volume-location (point (- (rect-right $screen-rect) 26)
                                     (- (rect-bottom $screen-rect) 46)))
      (volume-control volume-location @stream 'left)
      (volume-control (point (+ 13 (point-x volume-location))
                             (point-y volume-location))
                      @stream 'right)
      
      ;; We have captions for one of our Vorbis tracks.
      (text-box (rect (rect-left $screen-rect) (- (rect-bottom $screen-rect) 100)
                      (rect-right $screen-rect) (rect-width $screen-rect))
                (stylesheet :base $caption-style :color $color-black)
                "" :name 'caption))
    
    (def (media-caption event)
      (set! (@caption .text) (event-caption event)))
    )

  (sequence media/audiostream)
  (sequence media/audiostream/vorbis)

  (card media/audiostream/vorbis/stereo (%audio-stream-card%)
    (value title "Vorbis Stereo\n(Looping)")
    (run
      (vorbis-audio "oggtest-stereo.ogg" :name 'stream :loop? #t)))

  (card media/audiostream/vorbis/mono (%audio-stream-card%)
    (value title "Vorbis Mono")
    (run
      (vorbis-audio "oggtest-mono.ogg" :name 'stream)))

  (card media/audiostream/vorbis/twobeeps (%audio-stream-card%)
    (value title "Vorbis Two Beeps\n(Broken)")
    (run
      (vorbis-audio "oggtest-twobeeps.ogg" :name 'stream)))

  (card media/audiostream/vorbis/long (%audio-stream-card%)
    (value title "Long Vorbis\n(FDA Advisory)")
    (run
      (vorbis-audio "quackery.ogg" :name 'stream)))

  (sequence media/audiostream/geiger)

  (card media/audiostream/geiger/synth (%audio-stream-card%)
    (value title "Geiger Counter")
    (run
      (geiger-audio "oggtest-geiger-chirp.ogg" :name 'stream)
      (@stream .set-counts-per-second! 10.0)))

  (sequence media/audiostream/geiger/loop)

  (card media/audiostream/geiger/loop/rate-point8mrph (%audio-stream-card%)
    (value title "Ludlum 0.8 mRph")
    (run
      (vorbis-audio "ludlum/lud-mod14c-00_8mRph.ogg" :name 'stream :loop? #t)))

  (card media/audiostream/geiger/loop/rate-2mrph (%audio-stream-card%)
    (value title "Ludlum 2 mRph")
    (run
      (vorbis-audio "ludlum/lud-mod14c-02_0mRph.ogg" :name 'stream :loop? #t)))

  (card media/audiostream/geiger/loop/rate-5mrph (%audio-stream-card%)
    (value title "Ludlum 5 mRph")
    (run
      (vorbis-audio "ludlum/lud-mod14c-05_0mRph.ogg" :name 'stream :loop? #t)))

  (card media/audiostream/geiger/loop/rate-10mrph (%audio-stream-card%) 
    (value title "Ludlum 10 mRph")
    (run
      (vorbis-audio "ludlum/lud-mod14c-10_0mRph.ogg" :name 'stream :loop? #t)))

  (card media/audiostream/geiger/loop/rate-50mrph (%audio-stream-card%)
    (value title "Ludlum 50 mRph")
    (run
      (vorbis-audio "ludlum/lud-mod14c-50_0mRph.ogg" :name 'stream :loop? #t)))

  (card media/audiostream/sine (%audio-stream-card%)
    (value title "440Hz Sine Wave\n(Synthesized)")
    (run
      (sine-wave 440 :name 'stream)))
  
  ;;=======================================================================
  ;;  Complete Geiger Counter Synth
  ;;=======================================================================

  (card media/geiger-synth (%audio-stream-card%)
    (value title "Geiger Synth")
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