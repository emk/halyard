;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2009 Trustees of Dartmouth College
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.
;;
;; @END_LICENSE

(module data-file (lib "halyard.ss" "halyard")
  (require (lib "elements.ss" "halyard/private"))
  (provide ;; TODO - Remove these two functions from API.  They are obsoleted
           ;; by pref and set-pref!.
           read-data
           data-file->hash-table
           write-data
           user-has-saved-data?  ;determine if a given user-id has stored data

           pref
           set-pref!
           clear-prefs!
           
           ;; XXX - these shouldn't be exported, but there seems to be a bug 
           ;; in PLTs module system that makes it so the define-pref macros
           ;; can't expand to them properly without exporting them.
           %pref
           set-%pref!

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
           
           with-temporary-user-data)

  (define *tables* (make-hash-table 'equal))
  
  ;; If we are migrating from an old program, we may have user pref files
  ;; stored in ...\Tamale\Program Name\, rather than ...\Halyard\Program Name\.
  ;; In order to indicate that we would like to use the user's old pref files,
  ;; a program can add a file named config/USE-TAMALE-DIRECTORY-FOR-USER-PREFS.
  ;; We will then check for any Tamale\Program Name\ user data directory, and
  ;; if that exists, use it instead of the default (script-user-data-directory)
  (define *user-data-directory*
    (if (file-exists? "config/USE-TAMALE-DIRECTORY-FOR-USER-PREFS")
      (let-values [[[halyard-dir program-name must-be-dir?] 
                    (split-path (build-path (script-user-data-directory)))]]
        (define tamale-user-data-directory
          (build-path halyard-dir 'up "Tamale" program-name))
        (if (directory-exists? tamale-user-data-directory)
          (simplify-path tamale-user-data-directory)
          (script-user-data-directory)))
      (script-user-data-directory)))
  
  ;; given a user-id, returns the full file path of the corresponding data file
  (define (datafile-path id)
    (build-path *user-data-directory* (cat id ".dat")))
  
  ;; Determine if a "user-id.dat" file exists for a given user-id.
  (define (user-has-saved-data? user-id)
    (file-exists? (datafile-path user-id)))
  
  (define (find-table table-name)
    (unless (hash-table-has-key? *tables* table-name)
      (hash-table-put! *tables* table-name
                       (data-file->hash-table (datafile-path table-name))))
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

  ;;; Open 'path' and read a hash table from it.  If 'path' does not exist,
  ;;; return an empty hash table.
  (define (data-file->hash-table path)
    (define the-table (make-hash-table 'equal))
    (when (file-exists? path)
      (let ((file-port (open-input-file path)))
        (letrec ((read-pair
                  (lambda ()
                    (with-handlers 
                      [[exn:fail? 
                        (lambda (ex) 
                          (debug 'halyard "data-file->hash-table: "
                                 (exn-message ex))
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
  
  (define (call-with-temporary-user-data user-id thunk)
    (fluid-let [[*user-id* user-id]]
      (clear-user-prefs!)
      (thunk)))
  
  ;;; Call BODY with a new-initialized set of user prefs in place,
  ;;; and restore the old ones when we're done.
  (define-syntax with-temporary-user-data
    (syntax-rules ()
      [(_ () body ...)
       (with-temporary-user-data ('fake-user-for-test-cases) body ...)]
      [(_ (user-id) body ...)
       (call-with-temporary-user-data user-id (fn () body ...))]))
  (define-syntax-indent with-temporary-user-data 1)
  
  )
