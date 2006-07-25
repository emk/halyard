(module cm mzscheme
  (require (lib "moddep.ss" "syntax"))

  (provide make-compilation-manager-load/use-compiled-handler
	   managed-compile-zo
	   trust-existing-zos
	   (rename trace manager-trace-handler))
  
  (define trace (make-parameter void))
  (define indent (make-parameter ""))
  (define trust-existing-zos (make-parameter #f))
  
  (define my-max
    (case-lambda
      (() 0)
      (x (apply max x))))
  
  (define (get-deps code path)
    (let-values ([(imports fs-imports) (module-compiled-imports code)])
      (map (lambda (x) 
	     (resolve-module-path-index x path))
	   ;; Filter symbols:
	   (let loop ([l (append imports fs-imports)])
	     (cond
	      [(null? l) null]
	      [(symbol? (car l))(loop (cdr l))]
	      [else (cons (car l) (loop (cdr l)))])))))
  
  (define (get-compilation-path path)
    (let-values (((base name-suffix must-be-dir?) (split-path path)))
      (let ((name (regexp-replace "\\..?.?.?$" name-suffix "")))
        (cond
          ((eq? 'relative base) (build-path "compiled" name))
          (else (build-path base "compiled" name))))))
  
  (define (get-code-dir path)
    (let-values (((base name-suffix must-be-dir?) (split-path path)))
      (cond
        ((eq? 'relative base) (build-path "compiled"))
        (else (build-path base "compiled")))))
  
  (define (write-deps code path)
    (let ((dep-path (string-append (get-compilation-path path) ".dep"))
          (deps (get-deps code path)))
      (let ((op (open-output-file dep-path 'replace)))
        (write (cons (version) deps) op)
        (close-output-port op))))
  
  (define (touch path)
    (close-output-port (open-output-file path 'append)))

  (define (compilation-failure path zo-name)
    (with-handlers ((not-break-exn? void))
      (delete-file zo-name))
    (let ((out (open-output-file (string-append (get-compilation-path path) ".fail")
                                 'replace)))
      (close-output-port out))
    ((trace) (format "~afailure" (indent))))
    
  (define (compile-zo path)
    ((trace) (format "~acompiling: ~a" (indent) path))
    (indent (format "  ~a" (indent)))
    (let ((zo-name (string-append (get-compilation-path path) ".zo")))
      (if (and (file-exists? zo-name)
	       (trust-existing-zos))
	  (touch zo-name)
	  (begin
	    (with-handlers ((not-break-exn? void))
              (delete-file zo-name))
            (with-handlers ((exn:get-module-code? (lambda (ex) (compilation-failure path zo-name))))
              (let ((code (get-module-code path))
                    (code-dir (get-code-dir path)))
                (if (not (directory-exists? code-dir))
                    (make-directory code-dir))
                (let ((out (open-output-file zo-name 'replace)))
                  (with-handlers ((exn:application:type?
                                   (lambda (ex) (compilation-failure path zo-name))))
                    (dynamic-wind void
                                  (lambda () (write code out))
                                  (lambda () (close-output-port out)))))
                (write-deps code path))))))
    (indent (substring (indent) 2 (string-length (indent))))
    ((trace) (format "~aend compile: ~a" (indent) path)))
  
  (define (get-compiled-time path)
    (with-handlers ((exn:i/o:filesystem?
                     (lambda (ex)
                       (with-handlers ((exn:i/o:filesystem?
                                        (lambda (ex) -inf.0)))
                         (file-or-directory-modify-seconds (string-append (get-compilation-path path)
                                                                          ".fail"))))))
      (file-or-directory-modify-seconds (string-append (get-compilation-path path) ".zo"))))
  
  (define (compile-root path up-to-date)
    (let ([path (normal-case-path (simplify-path (expand-path path)))])
      (let ((stamp (and up-to-date
			(hash-table-get up-to-date path (lambda () #f)))))
	(cond
          (stamp stamp)
          (else
           ((trace) (format "~achecking: ~a" (indent) path))
           (let ((path-zo-time (get-compiled-time path))
                 (path-time 
                  (with-handlers ((exn:i/o:filesystem? 
                                   (lambda (ex)
                                     ((trace) (format "~a~a does not exist" (indent) path))
                                     #f)))
                    (file-or-directory-modify-seconds path))))
             (cond
               ((not path-time) +inf.0)
               (else
                (cond
                  ((> path-time path-zo-time) (compile-zo path))
                  (else
                   (let ((deps (with-handlers ((exn:i/o:filesystem? (lambda (ex) #f)))
                                 (call-with-input-file (string-append (get-compilation-path path) ".dep")
                                   read))))
                     (cond
                       ((or (not (pair? deps))
                            (not (equal? (version) (car deps))))
                        (compile-zo path))
                       ((> (apply my-max (map (lambda (d) (compile-root d up-to-date)) (cdr deps)))
                           path-zo-time)
                        (compile-zo path))))))
                (let ((stamp (get-compiled-time path)))
                  (hash-table-put! up-to-date path stamp)
                  stamp)))))))))
  
  (define (managed-compile-zo zo)
    (parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)])
       (compile-root (path->complete-path zo) (make-hash-table 'equal))))
  
  (define (make-compilation-manager-load/use-compiled-handler)
    (let ([orig-eval (current-eval)]
	  [orig-load (current-load)]
	  [orig-namespace (current-namespace)]
	  [cache (make-hash-table 'equal)]
	  [default-handler (current-load/use-compiled)])
      (let ([compilation-manager-load-handler
	     (lambda (path mod-name)
	       ((trace) (format "~aloading: ~a ~a" (indent) path mod-name))
	       (cond
		((not mod-name) (default-handler path mod-name))
		(else 
		 (unless (or (eq? 'none (use-compiled-file-kinds))
			     (not (and (eq? orig-eval (current-eval))
				       (eq? orig-load (current-load))
				       (eq? orig-namespace (current-namespace)))))
	            (compile-root path cache))
		 (default-handler path mod-name))))])
	compilation-manager-load-handler))))
