
(module toplevel mzscheme
  (require "kerncase.ss")

  (provide eval-compile-time-part-of-top-level
	   expand-top-level-with-compile-time-evals)

  (define (eval-compile-time-part-of-top-level stx)
    (kernel-syntax-case stx #f
      [(begin e ...)
       (for-each eval-compile-time-part-of-top-level (cdr (syntax->list stx)))]
      [(require req ...)
       (for-each (lambda (req)
		   (namespace-require/expansion-time (syntax-object->datum req)))
		 (syntax->list (syntax (req ...))))]
      [(module . _)
       (eval stx)]
      [(define-syntaxes . _)
       (eval stx)]
      [(require-for-syntax . _)
       (eval stx)]
      [(define-values (id ...) . _)
       (for-each (lambda (id)
		   (with-syntax ([id id]
				 [undefined (letrec ([x x]) x)])
		     (eval (syntax (define (id) undefined)))))
		 (syntax->list (syntax (id ...))))]
      [_else (void)]))

  (define (expand-top-level-with-compile-time-evals expr)
    (let ([e (expand-to-top-form expr)])
      (syntax-case e (begin)
	[(begin expr ...)
	 (with-syntax ([(expr ...) 
			;;left-to-right part of this map isimportant:
			(map expand-top-level-with-compile-time-evals
			     (syntax->list (syntax (expr ...))))]
		       [(beg . _) e])
	   (datum->syntax-object e (syntax-e (syntax (beg expr ...))) e e))]
	[else 
	 (let ([e (expand e)])
	   (eval-compile-time-part-of-top-level e)
	   e)]))))
