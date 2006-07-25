(module contract-helpers mzscheme

  (provide module-source-as-symbol build-src-loc-string)

  ;; build-src-loc-string : syntax -> string
  (define (build-src-loc-string stx)
    (let ([source (syntax-source stx)]
          [line (syntax-line stx)]
          [col (syntax-column stx)]
          [pos (syntax-position stx)])
      (cond
        [(and (string? source) line col)
         (format "~a: ~a.~a" source line col)]
        [(and line col)
         (format "~a.~a" line col)]
        [(and (string? source) pos)
         (format "~a: ~a" source pos)]
        [pos
         (format "~a" pos)]
        [else ""])))
  
  (define o (current-output-port))
  
  ;; module-source-as-symbol : syntax -> symbol
  ;; constructs a symbol for use in the blame error messages
  ;; when blaming the module where stx's occurs.
  (define (module-source-as-symbol stx)
    (let ([src-module (syntax-source-module stx)])
      (cond
        [(symbol? src-module) src-module]
        [(module-path-index? src-module) 
         (let-values ([(path base) (module-path-index-split src-module)])
           ;; we dont' normalize here, because we don't
           ;; want to assume that the collection paths
           ;; are set or the file system can be accessed.
           (if path
               (string->symbol (format "~s" path))
               'top-level))]
        [else 'top-level]))))
