;;=========================================================================
;;  Fake #%fivel-engine Module for DrScheme
;;=========================================================================
;;  When running under DrScheme, we need to provide a low-budget immitation
;;  of our standalone engine so users can test simple code.

(module #%fivel-engine mzscheme
  (provide %call-5l-prim)
  
  (define (sym-or-str-arg string-or-symbol)
    (cond
      [(symbol? string-or-symbol) string-or-symbol]
      [(string? string-or-symbol) (string->symbol string-or-symbol)]
      [else
       (error "Expected string or symbol")]))
  
  (define (value->string value)
    (if (string? value)
	value
	(let ((str-port (open-output-string)))
	  (write value str-port)
	  (get-output-string str-port))))

  (define *variable-table* (make-hash-table))
  
  (define (prim-get variable)
    ;; This behavior is silly, but it mimicks the engine.
    (let [[val (hash-table-get *variable-table*
                               (sym-or-str-arg variable)
                               (lambda () #f))]]
      (if val
          val
          (begin
            (prim-log 'Debug
                      (string-append "Getting "
                                     (symbol->string (sym-or-str-arg variable))
                                     " before it is set.")
                      'log)
            (prim-set variable "0")
            "0"))))

  (define (prim-set variable value)
    ;; This behavior is silly, but it mimicks the engine.
    (hash-table-put! *variable-table*
                     (sym-or-str-arg variable)
                     (value->string value))
    #f)
    
  (define (prim-log facility msg level)
    (display facility)
    (display ": ")
    (display msg)
    (newline)
    (when (eq? (sym-or-str-arg level) 'fatalerror)
      ;; Try to do something semi-useful with fatal errors.  These should
      ;; really abort all further execution, but this will do for now.
      (error msg)))
    
  (define (%call-5l-prim name . args)
    (case name

      ;; Primitives with actual implementations.
      [[get] (apply prim-get args)]
      [[set] (apply prim-set args)]
      [[log] (apply prim-log args)]
      
      ;; The do-nothing primitives.
      [[setwindowtitle defstyle header keybind loadpal]
       (void)]

      ;; 
      [else
       (error (string-append "The 5L primitive "
                             (symbol->string name)
                             " is not emulated by DrScheme."))])))