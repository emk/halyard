
(module kerncase mzscheme

  (define-syntax kernel-syntax-case
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? clause ...)
	 (quasisyntax/loc
	  stx
	  (syntax-case* stxv #,(datum->syntax-object
				stx
				'(quote 
				  quote-syntax #%datum #%top
				  lambda case-lambda
				  let-values letrec-values
				  begin begin0 set!
				  with-continuation-mark
				  if #%app
				  define-values define-syntaxes
				  module #%plain-module-begin require provide require-for-syntax))
			(if trans? module-transformer-identifier=? module-identifier=?)
	    clause ...))])))

  (define (kernel-form-identifier-list stx)
    (map (lambda (s)
	   (datum->syntax-object stx s #f))
	 '(begin
	    define-values
	    define-syntaxes
	    set!
	    let-values
	    let*-values
	    letrec-values
	    lambda
	    case-lambda
	    if
	    quote
	    letrec-syntaxes+values
	    fluid-let-syntax
	    with-continuation-mark
	    #%app
	    #%top
	    #%datum)))
  
  (provide kernel-syntax-case
	  kernel-form-identifier-list))
