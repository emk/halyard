(module hook mzscheme
  
  (provide (rename my-make-hook make-hook)
	   hook? hook-name
	   hook-add-function! hook-remove-function!
	   hook-functions
	   call-hook-functions
	   )

  (define-struct hook (name function-table) (make-inspector))

  (define (my-make-hook name)
    (make-hook name (make-hash-table)))

  (define (hook-add-function! hk tag function)
    ;; The function should have the parameters specified by the hook
    ;; creator.
    (hash-table-put! (hook-function-table hk) tag function))

  (define (hook-remove-function! hk tag)
    (hash-table-remove! (hook-function-table hk) tag))

  (define (hook-functions hk)
    ;; Return the hook functions in any old order.
    (hash-table-map (hook-function-table hk) (lambda (key value) value)))

  (define (call-hook-functions hk . args)
    ;; This is just a handy wrapper around hook-functions for callers
    ;; who don't care about return values.
    (let loop [[functions (hook-functions hk)]]
      (unless (null? functions)
	(apply (car functions) args)
        (loop (cdr functions))))
    #f)

  )