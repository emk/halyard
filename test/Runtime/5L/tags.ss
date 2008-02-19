(module tags (lib "language.ss" "5L")
        
  (require (lib "kernel.ss" "5L"))
        
  (provide maybe-insert-def maybe-insert-help 
           define-syntax-tagger define-syntax-tagger*
           define-syntax-taggers define-syntax-taggers*
           define-syntax-help
           (rename process-definition tagger-process-definition)
           form-name)
        
  (define *syntax-taggers* (make-hash-table))
        
  (define (insert-syntax-tagger! name fn)
    (hash-table-put! *syntax-taggers* name fn))
      
  (define (find-syntax-tagger name)
    (hash-table-get *syntax-taggers* name (lambda () #f)))
  
  ;; Syntax taggers are small syntax expanders that are run on the
  ;; code not for their expansion, but to add help information and
  ;; source location information linked to a name to a database that
  ;; the editor can access. There are two ways to define syntax
  ;; taggers; a high-level API, and a low-level API. The high-level
  ;; API works a lot like syntax rules; you specify patterns that
  ;; match the syntax being tagged, and then specify which parts of
  ;; those patterns correspond to the name being tagged and the
  ;; help. The syntax is as follows:
  ;;
  ;; (define-syntax-tagger definer
  ;;   [(pattern ...) 
  ;;    type name help]
  ;;   ...)
  ;; 
  ;; This will match forms that begin with definer against each of the
  ;; patterns, and depending on which pattern matches, will insert
  ;; tags based on the type, name, and help given. 'type' is a symbol
  ;; that determines what kind of definition this is, so the editor
  ;; can display the correct icon in the definition browser.  It can
  ;; be on of the following values: 'syntax, 'function, 'variable,
  ;; 'constant, 'class, 'template, 'group, 'sequence, 'card, 'element,
  ;; or any other symbol; each of the listed symbols will cause the
  ;; editor to display the corresponding icon, while for all of the
  ;; other symbols the editor will just use a default symbol. 'name'
  ;; should be any of the pattern variables listed in the pattern, and
  ;; will attach the tagging information to the name that matched that
  ;; variable at the source location where that name occurs. Help is
  ;; optional; it can be either #f, in which case no help information
  ;; is inserted into the database, or it can be an expression based
  ;; on pattern variables from the pattern, in which case the help
  ;; will be based on substituting the values that matched the pattern
  ;; into the help expression. 
  ;; 
  ;; An example:
  ;; 
  ;; (define-syntax define-foo
  ;;   (syntax-rules ()
  ;;     [(define-foo (name arg ...) body)
  ;;      <do something to define a foo as a function>]
  ;;     [(define-foo super (name arg ...) body)
  ;;      <do something to define a foo as syntax with a superclass>]))
  ;; (define-syntax-tagger define-foo
  ;;   [(define-foo (name arg ...) body)
  ;;    'function name (name arg ...)]
  ;;   [(define-foo super (name arg ...) body)
  ;;    'syntax name (name arg ...)])
  ;; (define-foo (test a b) (+ a b))
  ;; (define-foo syntax (frob foo bar) (cat foo bar))
  ;;
  ;; This example will insert into the databae information defining
  ;; 'test' to be a function defined on the line that it is defined
  ;; on, with help string "(test a b)", and will insert information
  ;; defining 'frob' to be syntax defined on the line it was defined 
  ;; on with help string "(frob foo bar)". 
  ;;
  ;; This high-level interface is fine for most purposes, but
  ;; sometimes you need to be able to programmatically manipulate a
  ;; definition to determine what is defined where and what help
  ;; strings to add to the database. For these purposes we have 
  ;; define-syntax-tagger*, which has the following forms:
  ;; 
  ;; (define-syntax-tagger* name function)
  ;; (define-syntax-tagger* (name stx) body)
  ;;
  ;; These cases are analogous to define-syntax when used with a
  ;; syntax-case based expander; in the first case, function is a
  ;; function that takes one argument, the syntax object for the
  ;; definition being processed, while in the second case, that
  ;; function is defined implicitly with the argument name given.
  ;; Within these taggers, you need to call maybe-insert-def and
  ;; maybe-insert-help to insert the definition information and help
  ;; information for the definitions that you're dealing with.  These
  ;; functions are used as follows:
  ;;
  ;; (maybe-insert-def name-syntax type)
  ;; (maybe-insert-def name-syntax help)
  ;; 
  ;; The 'name-syntax' in these calls should be a syntax object that 
  ;; corresponds to the name being defined. 'type' is a symbol, and 
  ;; works as described above. 'help' should be any scheme object that 
  ;; when printed will produce the desired help text; usually it is a 
  ;; list produced by syntax-object->datum. 
  ;;
  ;; There are also variants of both of these forms for defining
  ;; multiple taggers at once, define-syntax-taggers and
  ;; define-syntax-taggers*. You can find definitions of each of
  ;; these, and examples of their use, in the rest of this file.
  
  (define-syntax define-syntax-taggers*
    (syntax-rules ()
      [(_ (name ...) func)
       (let ((fn func))
         (insert-syntax-tagger! 'name fn)
         ...)]))
  
  (define-syntax define-syntax-tagger*
    (syntax-rules ()
      [(_ (name arg) body ...)
       (define-syntax-tagger* name (lambda (arg) body ...))]
      [(_ name func)
       (define-syntax-taggers* (name) func)]))
        
  (define-syntax-indent define-syntax-taggers* 1)
  (define-syntax-indent define-syntax-tagger* 1)
  
  (define-syntax define-syntax-taggers
    (syntax-rules ()
      [(_ (tagger-name ...)
          [pattern tag-type tag-name help] ...)
       (define-syntax-taggers* (tagger-name ...) 
         (lambda (stx)
           (syntax-case stx ()
             [pattern
              (begin 
                (maybe-insert-def #'tag-name tag-type)
                (let ((help-expr (syntax-object->datum #'help)))
                  (when help-expr
                    (maybe-insert-help #'tag-name help-expr))))]
             ...)))]))
        
  (define-syntax define-syntax-tagger
    (syntax-rules ()
      [(_ tagger-name body ...)
       (define-syntax-taggers (tagger-name) body ...)]))
        
  (define-syntax-indent define-syntax-taggers 1)
  (define-syntax-indent define-syntax-tagger 1)
        
  ;;; DEFINE-SYNTAX help can be used to provide a help string for a top-level
  ;;; macros that expands using SYNTAX-CASE.  But it's useless to try and
  ;;; generate this form in a macro expansion--use DEFINE-SYNTAX-TAGGER
  ;;; directly instead.
  (define-syntax define-syntax-help 
    (syntax-rules ()
      [(_ name help) (void)]))
  (define-syntax-tagger* (define-syntax-help stx)
    (syntax-case stx ()
      [(_ name help) 
       (maybe-insert-help #'name (syntax-object->datum #'help))]))
  (define-syntax-indent define-syntax-help 1)
  
  (define (insert-def name type line)
    (call-5l-prim 'ScriptEditorDBInsertDef name type line))

  (define (insert-help name help)
    (call-5l-prim 'ScriptEditorDBInsertHelp name (value->string help)))

  (define (maybe-insert-def name type)
    (let [[sym (syntax-object->datum name)]]
      (when (symbol? sym)
        (insert-def sym type (syntax-line name)))))

  (define (maybe-insert-help name help)
    (let [[sym (syntax-object->datum name)]]
      (when (symbol? sym)
        (insert-help sym help))))

  (define (form-name stx)
    (syntax-case stx ()
      [(name . body)
       (let [[datum (syntax-object->datum #'name)]]
         (if (symbol? datum)
             datum
             #f))]
      [anything-else
       #f]))
  
  (define (variable-type stx)
    (let [[name (syntax-object->datum stx)]]
      (if (symbol? name)
          (let [[str (symbol->string name)]]
            (if (> (string-length str) 0)
                (let [[letter (string-ref str 0)]]
                  (if (equal? letter #\$)
                      'constant
                      'variable))
                'variable))
          'variable)))
  
  (define (process-definition stx)
    (let ((tagger (find-syntax-tagger (form-name stx))))
      (when tagger
        (tagger stx))))
  
  (define-syntax-tagger* (module stx)
    (syntax-case stx ()
      [(module name language . body)
       (for-each process-definition (syntax->list #'body))]
      [anything-else #f]))
        
  (define-syntax-tagger* (begin stx)
    (syntax-case stx ()
      [(begin . body)
       (for-each process-definition (syntax->list #'body))]
      [anything-else #f]))
        
  (define-syntax-taggers* (define define* defgeneric defgeneric* defmethod)
    (lambda (stx)
      (syntax-case stx ()
         [(_ (name . args) . body)
          (begin 
            (maybe-insert-def #'name 'function)
            (maybe-insert-help #'name (syntax-object->datum #'(name . args))))]
         [(_ name . body)
          (maybe-insert-def #'name (variable-type #'name))]
         [anything-else #f])))
                       
  (define-syntax-taggers* (define-syntax define-syntax* defsyntax defsyntax*)
    (lambda (stx)
      (syntax-case stx ()
        [(_ name (syntax-rules literals [(pat-name . pat-body) template] ...))
         ;; This is a guard expression, to determine if we should match this 
         ;; rule. This is basically a substitute for adding syntax-rules to the
         ;; literals list of the syntax-case, because our syntax object comes 
         ;; from read-syntax, which doesn't have any lexical informations, so
         ;; literal matching doesn't work (see 12.2 in the MzSchem manual and 
         ;; 4.3.2 in R5RS for details).
         (eq? 'syntax-rules (syntax-object->datum #'syntax-rules))
         (begin 
           (maybe-insert-def #'name 'syntax)
           (foreach (pattern (syntax->list #'((name . pat-body) ...)))
             (maybe-insert-help #'name 
                                (syntax-object->datum pattern))))]
        [(_ (name stx) . body)
         (maybe-insert-def #'name 'syntax)]
        [(_ name . body) 
         (maybe-insert-def #'name 'syntax)])))
  
  (define-syntax-tagger* (define-syntaxes stx)
    (syntax-case stx ()
      [(_ (name ...) . body)
       ;; Most of the time these are all on the same line. In that case we
       ;; don't know what order they'll show up in on the sidebar, but it 
       ;; seems that it's the reverse of the order we put them into the 
       ;; database, so we'll put them in in backwards order to get it to 
       ;; come out right. 
       (foreach [def (reverse (syntax->list #'(name ...)))]
         (maybe-insert-def def 'function))]
      [anything-else #f]))
        
  (define-syntax-tagger make-provide-syntax 
    [(_ base provider) 
     'syntax provider #f])
  
  (define-syntax-taggers (defsubst defsubst*)
    [(_ (name . args) rewrite)
     'syntax name (name . args)])
        
  (define-syntax-taggers* (card sequence group element)
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (maybe-insert-def #'name (form-name stx))]
        [(_ name args . body)
         (maybe-insert-def #'name (form-name stx))]
        [anything-else #f])))
  
  (define-syntax-taggers (define-group-template 
                          define-card-template 
                          define-element-template)
    [(_ name params args . body)
     'template name #f])
      
  (define-syntax-tagger define-stylesheet
    [(define-stylesheet name . body)
     'constant name (define-stylesheet name . body)])
  
  (define-syntax-taggers* (defclass defclass* defstruct)
    (lambda (stx)
      (syntax-case stx ()
         [(_ name args . body)
          (let* ((class-name (syntax-object->datum #'name))
                 (slots (map 
                         (lambda (syn) 
                           (process-slot-definition syn class-name))
                         (syntax->list #'body))))
            ;; We're adding a bunch of definitions that map to the same line. 
            ;; We don't actually have any guarantee as to the order in which 
            ;; they will show up in the navigation sidebar, but it seems like 
            ;; they always show up in the reverse order that I put them into 
            ;; the database, so I'll put them in in the reverse of the way I 
            ;; want them to display. 
            (maybe-insert-def (predicate-syntax #'name class-name) 'function)
            (maybe-insert-help (predicate-syntax #'name class-name)
                               (predicate-help class-name))
            (maybe-insert-def (make-fn-syntax #'name class-name) 'function)
            (maybe-insert-help (make-fn-syntax #'name class-name)
                               (make-fn-help class-name slots))
            (maybe-insert-def #'name 'class))]
         [anything-else #f])))
  
  (define (process-slot-definition slot-stx class-name)
    (syntax-case slot-stx ()
      [(slot . args) 
       (insert-defs-for-slot #'slot (syntax-object->datum #'args) class-name)]
      [slot 
       (insert-defs-for-slot #'slot '() class-name)]))
  
  ;; This function will insert definitions and help for the setters and 
  ;; getters of all of the slots. It doesn't follow the full algorithm that
  ;; swindle does for creating the names of the setters and getters, so it 
  ;; could in some cases generate setter and getter names that are wrong. 
  ;; What we do is check to see if there is an :accessor keyword in the slot 
  ;; definition. If there is, then the accessor is the value passed to the 
  ;; :accessor keyword, otherwise, the accessor is just the slot name. The 
  ;; getter is then the accessor, and the setter is set-<accessor>!. This 
  ;; ignores the :reader and :writer keywords, but they are never used, in
  ;; either our code or swindle, so ignoring those should be OK. 
  (define (insert-defs-for-slot slot-stx args class-name)
    (let* ((slot (syntax-object->datum slot-stx))
           (accessor (accessor-name slot args class-name)))
      (maybe-insert-def (setter-syntax slot-stx accessor) 
                        'function)
      (maybe-insert-help (setter-syntax slot-stx accessor)
                         (setter-help accessor class-name))
      (maybe-insert-def (getter-syntax slot-stx accessor) 
                        'function)
      (maybe-insert-help (getter-syntax slot-stx accessor)
                         (getter-help accessor class-name))
      slot))
  
  (define (strip-class-brackets class)
    (regexp-replace
     "^<(.*)>$" (symbol->string class) "\\1"))
  
  (define (build-accessor-name slot class)
    (string-append (strip-class-brackets class)
                   "-"
                   (symbol->string slot)))
  
  (define (accessor-name slot args class)
    (let ((accessor (getarg args :accessor)))
      (if accessor
        (symbol->string accessor)
        (build-accessor-name slot class))))
        
  (define (getter-help accessor class)
    (list
     (string->symbol accessor)
     (string->symbol (strip-class-brackets class))))
  
  (define (setter-name accessor)
    (string-append "set-"
     accessor
     "!"))
  
  (define (setter-help accessor class)
    (list 
     (string->symbol (setter-name accessor))
     (string->symbol (strip-class-brackets class))
     'value))
  
  (define (wrap-syntax stx datum)
    (datum->syntax-object stx datum stx))
       
  (define (getter-syntax stx accessor)
    (wrap-syntax stx (string->symbol accessor)))
  
  (define (setter-syntax stx accessor)
    (wrap-syntax stx (string->symbol (setter-name accessor))))
        
  (define (make-fn-name class)
    (string-append "make-" (strip-class-brackets class)))
        
  (define (make-fn-syntax stx class)
    (wrap-syntax 
     stx 
     (string->symbol 
      (make-fn-name class))))
        
  (define (make-fn-help class slots)
    (cons 
     (string->symbol (make-fn-name class))
     slots))
        
  (define (predicate-name class)
    (string-append (strip-class-brackets class) "?"))
        
  (define (predicate-syntax stx class)
    (wrap-syntax 
     stx
     (string->symbol 
      (predicate-name class))))
        
  (define (predicate-help class)
    (list 
     (string->symbol (predicate-name class))
     'obj))
  
  (define (extract-definitions file-path)
    (call-with-input-file file-path
      (lambda (port)
        (define (next)
          (read-syntax file-path port))
        (port-count-lines! port)
        (let recurse [[stx (next)]]
          (unless (eof-object? stx)
            (process-definition stx)
            (recurse (next)))))))

  ;; This will allow meta-. to work on ruby-style classes.  This only
  ;; extracts the most basic information to allow meta-. on the class
  ;; name itself; methods, attrs, help strings, etc will need more
  ;; work later. See the bugs filed against the Editor in FogBugz.
  ;;
  ;; This is here rather than in ruby-objects.ss, because requiring
  ;; tags.ss from ruby-objects.ss would cause circular dependencies.
  (define-syntax-tagger define-class
    [(_ name super . body) 
     'class name #f])

  (set-extract-definitions-fn! extract-definitions))