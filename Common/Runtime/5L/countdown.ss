;;========================================================================
;; Template definitions and helper functions for timer template  
;;========================================================================
(module countdown (lib "5L.ss" "5L")
  (require (lib "tamale.ss" "5L"))

  (provide %countdown% countdown)
  
  (define (update-time time)
    (define (format-time time-in-seconds)
      (list (quotient time-in-seconds 3600)
            (quotient (modulo time-in-seconds 3600) 60)
            (modulo (modulo time-in-seconds 3600) 60)))
    (let* [[initial-time-in-seconds (+ (* 3600 (car time))
                                       (* 60 (cadr time))
                                       (caddr time))]
           [elapsed-time-in-seconds (- (current-seconds) (cadddr time))]
           [current-time-in-seconds (- initial-time-in-seconds
                                       elapsed-time-in-seconds)]]
      (if (<= current-time-in-seconds 0)
          (list 0 0 0 (current-seconds))
          (append (format-time current-time-in-seconds)
                  (list (current-seconds))))))
  
  (define-element-template %countdown%
      [[value :type <list> 
              :default (list 0 0 0 (current-seconds)) 
              :label "value"]
       [style :label "stylesheet"]
       [action :label "function"]]
      (%zone%
       :overlay? #t
       :alpha? #t)
    (define val value)
    (on idle (event)
      (let [[new-val (update-time val)]]
        (when (not (equal? val new-val))
          (set! val new-val)
          (with-dc self
            (clear-screen (color 0 0 0 0))
            (center-text style (dc-rect)
                         (cat (car val) ":" 
                              (cadr val) ":" 
                              (caddr val)))
            (if (and (zero? (car val))
                     (zero? (cadr val))
                     (zero? (caddr val)))
                (action)))))))
  
  (define (countdown name shape value style action)
    (define (correct-format? value)
      (and (instance-of? value <list>)
           (not (member #f
                        (map (lambda (n) 
                               (instance-of? n <number>))
                             value)))))
    (if (not (correct-format? value))
        (error (cat "time format expects: (h m s). given:" value)))
    (create %countdown%
            :name name
            :shape shape
            :value (append value (list (current-seconds)))
            :style style
            :action action))
  )