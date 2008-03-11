(module data-file (lib "5l.ss" "halyard")
  (require (lib "tamale.ss" "halyard"))
  (provide ;; TODO - Remove these two functions from API.  They are obsoleted
           ;; by pref and set-pref!.
           read-data
           write-data
           user-has-saved-data?  ;determine if a given user-id has stored data

           pref
           set-pref!
           clear-prefs!

           user-id
           set-user-id!
           ;; set-user-id! and register the datafile for the crash reporter
           set-user-id-and-register-for-debug!
           
           global-pref
           set-global-pref!
           clear-global-prefs!
           user-pref
           set-user-pref!
           clear-user-prefs!

           define-global-pref
           define-user-pref
           )

  (define *tables* (make-hash-table 'equal))
  
  ;; given a user-id, returns the full file path of the corresponding data file
  (define (datafile-path id)
    (build-path (script-user-data-directory) (cat id ".dat")))
  
  ;; Determine if a "user-id.dat" file exists for a given user-id.
  (define (user-has-saved-data? user-id)
    (file-exists? (datafile-path user-id)))
  
  (define (find-table table-name)
    (unless (hash-table-has-key? *tables* table-name)
      (hash-table-put! *tables* table-name
                       (maybe-read-data-from-file table-name)))
    (hash-table-get *tables* table-name))

  (define (read-data table key &key (default #f))
    (hash-table-get (find-table table) key (lambda () default)))

  (define (read-pair-from-line line the-table)
    (let* ((str-input (open-input-string line))
           (key (read str-input))
           (value (read str-input)))
      (when (not (or (eof-object? key) 
                     (eof-object? value)))
        (hash-table-put! the-table key value))))

  (define (maybe-read-data-from-file table)
    (define file-with-path (datafile-path table))
    (define the-table (make-hash-table 'equal))
    (when (file-exists? file-with-path)
      (let ((file-port (open-input-file file-with-path)))
        (letrec ((read-pair
                  (lambda ()
                    (with-handlers 
                      [[exn:fail? 
                        (lambda (ex) 
                          (debug-log (cat "maybe-read-data-from-file: "
                                          (exn-message ex)))
                          (read-pair))]]
                      (let ((next-line (read-line file-port)))
                        (if (not (eof-object? next-line))
                          (begin
                            (read-pair-from-line next-line the-table)
                            (read-pair))
                          (close-input-port file-port)))))))
          (read-pair))))
    the-table)

  (define (write-data table key datum)
    (define the-table (find-table table))
    (hash-table-put! the-table key datum)
    (flush-data table))

  (define (flush-data table)
    (define file-with-path (datafile-path table))
    (define file-port (open-output-file file-with-path 'replace))
    (define the-table (find-table table))
    (hash-table-for-each the-table (lambda (key value)
                                     (write key file-port)
                                     (display "\t" file-port)
                                     (write value file-port)
                                     (newline file-port)))
    (close-output-port file-port))
    
  (define (clear-data table)
    (hash-table-put! *tables* table (make-hash-table 'equal))
    (flush-data table))

  (define pref read-data)
  (define set-pref! write-data)
  (define clear-prefs! clear-data)

  (define *user-id* #f)
  (define (user-id)
    (if *user-id*
        *user-id*
        (error "Cannot access per-user prefs before setting user-id")))
  (define (set-user-id! value)
    (set! *user-id* value))
  
  ;; set-user-id! and also register the user's datafile to be included when
  ;; crash reports are generated.
  (define (set-user-id-and-register-for-debug! value)
    (set-user-id! value)
    (register-debug-report-file! (datafile-path value) "Userpref data file"))

  (define (user-pref key &key (default #f))
    (pref (user-id) key :default default))
  (define (set-user-pref! key value)
    (set-pref! (user-id) key value))
  (define (clear-user-prefs!)
    (clear-prefs! (user-id)))

  (define (global-pref key &key (default #f))
    (pref 'global key :default default))
  (define (set-global-pref! key value)
    (set-pref! 'global key value))
  (define (clear-global-prefs!)
    (clear-prefs! 'global))

  ;; Internal helper functions.
  (define (%pref table key default)
    (pref table key :default default))
  (define (set-%pref! table key junk-default value)
    (set-pref! table key value))

  (define-syntax define-pref
    (syntax-rules ()
      [(define-pref var default category)
       (define-symbol-macro var (%pref category 'var default))])) 

  (define-syntax define-user-pref
    (syntax-rules ()
      [(define-user-pref var default)
       (define-pref var default (user-id))]))

  (define-syntax define-global-pref
    (syntax-rules ()
      [(define-global-pref var default)
       (define-pref var default 'global)]))
  
  )
