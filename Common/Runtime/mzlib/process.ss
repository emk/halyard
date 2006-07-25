
(module process mzscheme
  (provide process
	   process*
	   process/ports
	   process*/ports
	   system
	   system*)

  (require (lib "thread.ss"))

  ;; Helpers: ----------------------------------------

  (define (shell-path/args who argstr)
    (case (system-type)
      ((unix macosx) (append '("/bin/sh" "-c") (list argstr)))
      ((windows) (let ([cmd 		  
			(let ([d (find-system-path 'sys-dir)])
			  (let ([cmd (build-path d "cmd.exe")])
			    (if (file-exists? cmd)
				cmd
				(let ([cmd (build-path d "command.com")])
				  (if (file-exists? cmd)
				      cmd
				      ;; One last try: up a dir
				      (build-path d 'up "command.com"))))))])
		   (list cmd
			 'exact
			 (format "~a /c ~a" cmd argstr))))
      (else (raise-mismatch-error 
	     (format "~a: don't know what shell to use for platform: " who)
	     (system-type)))))

  (define (if-stream-out p)
    (if (or (not p) (file-stream-port? p))
	p
	(if (output-port? p)
	    #f
	    (raise-type-error
	     'subprocess
	     "output port"
	     p))))
	
  (define (if-stream-in p)
    (if (or (not p) (file-stream-port? p))
	p
	(if (input-port? p)
	    #f
	    (raise-type-error
	     'subprocess
	     "input port"
	     p))))

  (define (streamify-in cin in get-thread?)
    (if (and cin (not (file-stream-port? cin)))
	(let ([t (thread (lambda () 
			   (dynamic-wind
			    void
			    (lambda () 
			      (with-handlers ([exn:break? void])
				(copy-port cin in)))
			    (lambda () (close-output-port in)))))])
	  (and get-thread? t))
	in))

  (define (streamify-out cout out get-thread?)
    (if (and cout (not (file-stream-port? cout)))
	(let ([t (thread (lambda () (copy-port out cout)))])
	  (and get-thread? t))
	out))

  ;; Old-style functions: ----------------------------------------

  (define (process*/ports cout cin cerr exe . args)
    (let-values ([(subp out in err) (apply subprocess 
					   (if-stream-out cout)
					   (if-stream-in cin)
					   (if-stream-out cerr)
					   exe args)])      
      (list (streamify-out cout out #f)
	    (streamify-in cin in #f)
	    (subprocess-pid subp)
	    (streamify-out cerr err #f)
	    (letrec ((control
		      (lambda (m)
			(case m
			  ((status) (let ((s (subprocess-status subp)))
				      (cond ((not (integer? s)) s)
					    ((zero? s) 'done-ok)
					    (else 'done-error))))
			  ((wait) (subprocess-wait subp))
			  ((interrupt) (subprocess-kill subp #f))
			  ((kill) (subprocess-kill subp #t))
			  (else
			   (raise-type-error 'control-process "'status, 'wait, 'interrupt, or 'kill" m))))))
	      control))))

  (define (process/ports out in err str)
    (apply process*/ports out in err (shell-path/args "process/ports" str)))

  (define (process* exe . args)
    (apply process*/ports #f #f #f exe args))

  (define (process str)
    (apply process* (shell-path/args "process" str)))

  ;; Note: these always use current ports
  (define (system* exe . args)
    (if (eq? (system-type) 'macos)
	(begin
	  (unless (null? args)
	    (raise-mismatch-error 'system* "command-line arguments not supported for MacOS" args))
	  (subprocess #f #f #f exe))
	(let ([cout (current-output-port)]
	      [cin (current-input-port)]
	      [cerr (current-error-port)])
	  (let-values ([(subp out in err)
			(apply
			 subprocess
			 (if-stream-out cout)
			 (if-stream-in cin)
			 (if-stream-out cerr)
			 exe args)])
	    (let ([ot (streamify-out cout out #t)]
		  [it (streamify-in cin in #t)]
		  [et (streamify-out cerr err #t)])
	      (subprocess-wait subp)
	      (when it
		;; stop piping output to subprocess
		(break-thread it))
	      ;; wait for other pipes to run dry:
	      (when (thread? ot)
		(thread-wait ot))
	      (when (thread? et)
		(thread-wait et)))
	    (zero? (subprocess-status subp))))))

  (define (system str)
    (if (eq? (system-type) 'macos)
	(subprocess #f #f #f "by-id" str)
	(apply system* (shell-path/args "system" str)))))
