(module updater-test (lib "5L.ss" "5L")
  (require (lib "tamale-unit.ss" "5L"))
  (require (lib "updater.ss" "5L"))
  
  (define (create-test-file dir name)
    (with-output-to-file (build-path dir name)
      (thunk (display name) (newline))))
  
  (define-test-case <filesystem-test> () 
      [[test-directory #f]]
    (setup
      (set! (test-directory self) (ensure-dir-exists "UpdateTest")))
    (teardown 
      (delete-directory-recursive (test-directory self)))
    (test "New directory should be writeable."
      (assert (root-directory-writeable?))
      (assert (dir-writeable? (test-directory self))))
    (test "delete-directory-recursive should delete only the correct files."
      (let* ((deleted-dir (build-path (test-directory self) "DeleteDir"))
             (inner-dir (build-path deleted-dir "InnerDir")))
        (make-directory deleted-dir)
        (make-directory inner-dir)
        (create-test-file deleted-dir "Deleted")
        (create-test-file inner-dir "File1")
        (create-test-file inner-dir "File2")
        (create-test-file (test-directory self) "NotDeleted")
        
        (assert (directory-exists? deleted-dir))
        (assert (directory-exists? inner-dir))
        (assert (file-exists? (build-path deleted-dir "Deleted")))
        (assert (file-exists? (build-path inner-dir "File1")))
        (assert (file-exists? (build-path inner-dir "File2")))
        (assert (file-exists? 
                 (build-path (test-directory self) "NotDeleted")))
        
        (delete-directory-recursive deleted-dir)
        
        (assert (not (directory-exists? deleted-dir)))
        (assert (not (directory-exists? inner-dir)))
        (assert (not (file-exists? (build-path deleted-dir "Deleted"))))
        (assert (not (file-exists? (build-path inner-dir "File1"))))
        (assert (not (file-exists? (build-path inner-dir "File2"))))
        (assert (file-exists? 
                 (build-path (test-directory self) "NotDeleted"))))))
  
  (card updater-test
      (%test-suite%
       :tests (list <filesystem-test>)))
  )