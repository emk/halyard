(module data-file (lib "5L.ss" "5L")
  (require (lib "tamale.ss" "5L"))
  (provide read-data write-data)

  (define tables (make-hash-table 'equal))
  
  (define (get-table table 
            &opt (thunk (lambda () (error "Table doesn't exist" table))))
    (hash-table-get tables table thunk))

  (define (make-table table)
    (hash-table-put! tables table (make-hash-table 'equal)))
  
  (define (read-data table key &key (default #f))
    (cond
     ((hash-table-get tables table (lambda() #f)) =>
      (lambda (table) (hash-table-get table key (lambda () default))))
     (else (hash-table-get (read-data-from-file table)
                           key (lambda () default)))))

  (define (read-data-from-file table)
    (define file-with-path (build-path "Data" (cat table ".dat")))
    (define the-table (get-table table 
                                 (lambda () 
                                   (begin (make-table table) 
                                          (get-table table)))))    
    (when (file-exists? file-with-path)
      (let ((file-port (open-input-file file-with-path)))
        (letrec ((read-pair
                  (lambda ()
                    (let ((key (read file-port))
                          (value (read file-port)))
                      (if (not (eof-object? key))
                          (begin
                            (hash-table-put! the-table key value)
                            (read-pair))
                          (close-input-port file-port))))))
          (read-pair))))
    the-table)

  (define (write-data table key datum)
    (define path "Data")
    (define dir (ensure-dir-exists path))
    (define filename (cat table ".dat"))
    (define file-with-path (build-path dir filename))
    (define file-port (open-output-file file-with-path 'replace))
    (define the-table (get-table table))
    (hash-table-put! the-table key datum)
    (hash-table-for-each the-table (lambda (key value)
                                     (begin
                                       (write key file-port)
                                       (display "\t" file-port)
                                       (write value file-port)
                                       (newline file-port))))
    (close-output-port file-port))

)
  


      