(module define-node-helper (lib "language.ss" "5L")
  (require (lib "nodes.ss" "5L"))
  (require-for-syntax (lib "util.ss" "5L"))

  (provide define-node-helper)

  ;;; If you write:
  ;;;
  ;;;   (define-node-helper menu-item (y text jump-to) elem %menu-item%)
  ;;;
  ;;; ..then the following:
  ;;;
  ;;;   (menu-item name (80 "Hello" @something :shown? #f)
  ;;;     (def (click) ...))
  ;;;
  ;;; ...will expand to:
  ;;;
  ;;;   (elem name (%menu-item% :y 80 :text "Hello" :jump-to @something
  ;;;                           :shown? #f)
  ;;;     (def (click) ...))
  ;;;
  ;;; TODO - This would all be much nicer if we had .ELEM, which we should
  ;;; probably think about implementing someday (assuming we decide to
  ;;; ever allow anonymous class declarations).
  (define-syntax (define-node-helper stx)
    (define (names->keys+names names-stx)
      (datum->syntax-object stx
        (let recurse [[names (syntax->list names-stx)]]
          (if (null? names)
              '()
              (let [[symbol (syntax-object->datum (car names))]]
                (unless (symbol? symbol)
                  (raise-syntax-error 'define-node-helper
                                      "Malformed argument list" names-stx))
                (list* (symcat ":" symbol) (car names)
                       (recurse (cdr names))))))))
    
    (syntax-case stx []
      [(_ helper-name (args ...) type class)
       (quasisyntax/loc stx
         (define-syntax helper-name
           (syntax-rules ()
             [(_ name (args ... . keys) . body)
              (type name (class #,@(names->keys+names #'(args ...)) . keys) 
                . body)])))]))

  )