(module events (lib "5l.ss" "5L")
  
  (provide on pass)

  (define pass
    (lambda ()
      (error "Can only call 'pass' from an event handler"))) 

  (define-struct event-table
    (handlers next want-expensive?)
    (make-inspector))

  (define *current-event-table* (make-event-table (make-hash-table) #f #f))

  (define (update-expensive-event-status)
    (call-5l-prim 'EnableExpensiveEvents
                  (event-table-want-expensive? *current-event-table*)))


  (define (set-event-handler! table name handler)
    (let [[handlers (event-table-handlers table)]]
      (when (hash-table-get handlers name (lambda () #f))
        (debug-caution (cat "Replacing handler for: " name)))
      (when (member? name '(idle mousemoved))
        (set! (event-table-want-expensive? table) #t)
        (update-expensive-event-status))
      (hash-table-put! (event-table-handlers table) name handler)))

  (define-syntax on
    (syntax-rules []
      [(on name (args ...) body ...)
       (set-event-handler! *current-event-table* 'name
                           (lambda (args ...) body ...))]))

  (define (call-event-handler table name args)
    (let* [[handlers (event-table-handlers table)]
           [handler (hash-table-get handlers name (lambda () #f))]]
      (if handler
          ;; Temporarily rebind 'pass' to a non-local exit function.
          (label return
            (fluid-let [[pass (lambda () (return #f))]]
              (apply handler args)
              #t))
          #f)))
  
  (define (push-event-table card)
    (set! *current-event-table*
          (make-event-table (make-hash-table) *current-event-table*
                            (event-table-want-expensive?
                             *current-event-table*))))

  (define (pop-event-table card)
    (let [[next (event-table-next *current-event-table*)]]
      (assert next)
      (set! *current-event-table* next)
      (update-expensive-event-status)))

  (hook-add-function! *enter-card-hook* '_events push-event-table)
  (hook-add-function! *exit-card-hook* '_events pop-event-table)  

  (define (dispatch-event name . args)
    (unless (member? name '(idle mousemoved))
      ;; Only log things which are infrequent, for the sake of sanity.
      (debug-log (cat "Dispatching event: " name " " args)))
    ;; Walk our event tables recursively, looking for a handler.
    (let loop [[table *current-event-table*]]
      (if table
          (if (call-event-handler table name args)
              (set! (engine-var "_pass") #f)
              (loop (event-table-next table)))
          (set! (engine-var "_pass") #t))))

  (call-5l-prim 'RegisterEventDispatcher dispatch-event)
  
  )