(module define-node-helper (lib "language.ss" "5L")
  (require (lib "nodes.ss" "5L"))
  (require-for-syntax (lib "util.ss" "5L"))
  (require (lib "tags.ss" "5L"))
  (require (lib "indent.ss" "5L"))

  (provide define-node-helper)

  ;;; If you write:
  ;;;
  ;;;   (define-node-helper menu-item (y text jump-to) %menu-item%)
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
  ;;; (Well, it actually expands to DEFINE-NODE instead of ELEM, which
  ;;; means use can use it for cards and other types of nodes as well.)
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
      [(_ helper-name (args ...) class)
       (quasisyntax/loc stx
         (begin
           (define-syntax helper-name
             (syntax-rules ()
               [(_ name (args ... . keys) . body)
                (define-node name (class #,@(names->keys+names #'(args ...)) 
                                    . keys) 
                  . body)]))
           (define-syntax-indent helper-name 2)))]))
  
  (define-syntax-help define-node-helper 
    (define-node-helper helper-name (args ...) class))
  
  (define-syntax-tagger define-node-helper
    [(_ helper-name (args ...) class)
     'syntax helper-name (helper-name name (args ... &rest keys) 
                                      body (... ...))])
  )