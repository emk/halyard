(module updater-test (lib "5L.ss" "5L")
  (require (lib "tamale-unit.ss" "5L"))
  (require (lib "updater.ss" "5L"))
  
  (define (create-test-file dir name)
    (with-output-to-file (build-path dir name)
      (thunk (display name) (newline))))
  
  (define-test-case <filesystem-test> () 
      [[test-directory #f]
       [outer-directory #f]
       [inner-directory #f]]
    (setup
      (set! (test-directory self) (ensure-dir-exists "UpdateTest"))
      (set! (outer-directory self) 
            (ensure-dir-exists (build-path "UpdateTest" "Outer")))
      (set! (inner-directory self) 
            (ensure-dir-exists (build-path "UpdateTest" "Outer" "Inner")))
      (create-test-file (outer-directory self) "Deleted")
      (create-test-file (inner-directory self) "File1")
      (create-test-file (inner-directory self) "File2"))
    (teardown 
      (delete-directory-recursive (test-directory self)))
    (test "New directory should be writeable."
      (assert (root-directory-writeable?))
      (assert (dir-writeable? (test-directory self))))
    (test "delete-directory-recursive should delete only the correct files."
      (let ((deleted-dir (outer-directory self))
            (inner-dir (inner-directory self)))
        (create-test-file (test-directory self) "NotDeleted")
        
        ;; Make sure our files exist before the delete.
        (assert (directory-exists? deleted-dir))
        (assert (directory-exists? inner-dir))
        (assert (file-exists? (build-path deleted-dir "Deleted")))
        (assert (file-exists? (build-path inner-dir "File1")))
        (assert (file-exists? (build-path inner-dir "File2")))
        (assert (file-exists? 
                 (build-path (test-directory self) "NotDeleted")))
        
        ;; Delete the files.
        (delete-directory-recursive deleted-dir)
        
        ;; Make sure the files don't exist, except for the file outside
        ;; the deleted directory. 
        (assert (not (directory-exists? deleted-dir)))
        (assert (not (directory-exists? inner-dir)))
        (assert (not (file-exists? (build-path deleted-dir "Deleted"))))
        (assert (not (file-exists? (build-path inner-dir "File1"))))
        (assert (not (file-exists? (build-path inner-dir "File2"))))
        (assert (file-exists? 
                 (build-path (test-directory self) "NotDeleted")))))
    (test "copy-recursive should copy correct files."
      (define copy-dir (build-path (test-directory self) "Copy"))
      (define inner-copy-dir (build-path copy-dir "Inner"))
      (copy-recursive (outer-directory self) copy-dir)
      (assert (directory-exists? copy-dir))
      (assert (directory-exists? inner-copy-dir))
      (assert (file-exists? (build-path copy-dir "Deleted")))
      (assert (file-exists? (build-path inner-copy-dir "File1")))
      (assert (file-exists? (build-path inner-copy-dir "File2")))))
  
  ;; TODO - possibly refactor these into some sort of file-utils and 
  ;; tamale-unit. 

  (define (assert-file-equals str file)
    (if (file-exists? file)
      (let ((contents (read-string-from-file file)))
        (unless (equal? str (read-string-from-file file))
          (error 
           (cat "Expected <" str "> in file <" file ">, got <" contents ">"))))
      (error (cat "File <" file "> does not exist"))))
  
  (define-test-case <mock-downloader-test> () 
      [[test-directory #f]
       [downloader #f]]
    (setup 
      (set! (test-directory self) (ensure-dir-exists "MockTest"))
      (set! (downloader self) 
            (make <mock-downloader> :directory (test-directory self)))
      (add-mock-url (downloader self) "test://foo.com/bar" "foo\nbar\n")
      (add-mock-url (downloader self) "test://example.com/example" "example")
      (add-mock-url (downloader self) "test://evil.com/..\\BADFILE" "bad!")
      (add-mock-url (downloader self) "test://nofilename.com/" "noname"))
    (teardown 
      (delete-directory-recursive (test-directory self)))
    (test "mock-downloader should write files."
      (download (downloader self) "test://example.com/example")
      (download (downloader self) "test://foo.com/bar" :file "foobar")
      (assert-file-equals "example" 
                          (build-path (test-directory self) "example"))
      (assert-file-equals "foo\nbar\n" 
                          (build-path (test-directory self) "foobar")))
    (test "mock-downloader should do basic sanitization"
      (download (downloader self) "test://evil.com/..\\BADFILE")
      (download (downloader self) "test://nofilename.com/")
      (assert (not (file-exists? (build-path (current-directory) "BADFILE"))))
      (assert-file-equals "bad!" (build-path (test-directory self) "temp1"))
      (assert-file-equals "noname" 
                          (build-path (test-directory self) "temp2"))))

  (define null-digest "da39a3ee5e6b4b0d3255bfef95601890afd80709")
  (define foo-digest "855426068ee8939df6bce2c2c4b1e7346532a133")
  
  ;; TODO - macroize, move to tamale-unit, maybe make more efficient. 
  (define (assert-set-equal a b)
    (foreach (item a)
      (assert (member? item b)))
    (foreach (item b)
      (assert (member? item a))))
  
  (define (setup-mock-root-directory dir update-dir)
    (define mock-downloader
      (mock-downloader-from-dir 
       update-dir
       :prefix "test://update.com/" 
       :download-dir (build-path dir "Temp")))
    (add-urls-from-manifests mock-downloader "test://update.com/" update-dir)
    (set-update-downloader! mock-downloader)
    (set-update-root-directory! dir))
  
  (define-test-case <updater-test> () 
      [[test-directory #f]
       [base-directory #f]
       [update-directory #f]]
    (setup 
      (set! (test-directory self) (ensure-dir-exists "UpdaterTest"))
      (set! (base-directory self)
            (copy-recursive 
             (build-path (fixture-dir "updater") "base") 
             (test-directory self)))
      (set! (update-directory self)
            (copy-recursive 
             (build-path (fixture-dir "updater") "update")
             (test-directory self))))
    (teardown 
      (delete-directory-recursive (test-directory self))
      (set-update-downloader! #f)
      (set-update-root-directory! #f))
    (test "Parsing manifests."
      (assert-equal 
       `((,null-digest "bar.txt")
         (,null-digest "foo.txt"))
       (parse-manifest (build-path (base-directory self) "MANIFEST.base")))
      (assert-equal 
       `((,null-digest "sub/baz.txt")
         (,null-digest "sub/foo.txt"))
       (parse-manifest (build-path (base-directory self) "MANIFEST.sub"))))
    (test "diff-manifests should work."
      (define manifest-a '(("123" "foo.txt")
                           ("456" "bar.txt")
                           ("ABC" "sub/thing.txt")))
      (define manifest-b '(("125" "foo.txt")
                           ("456" "bar.txt")
                           ("ABC" "sub/thing.txt")
                           ("DEF" "sub/zot.txt")))
      (assert-set-equal '() (diff-manifests manifest-a manifest-a))
      (assert-set-equal '(("125" "foo.txt") ("DEF" "sub/zot.txt")) 
                        (diff-manifests manifest-a manifest-b)))
    (test "Checking for updates, update should be available."
      (setup-mock-root-directory (base-directory self) (update-directory self))
      (assert (check-for-update "test://update.com/")))
    (test "Checking for update, update should not be available."
      (setup-mock-root-directory 
       (update-directory self) (update-directory self))
      (assert (not (check-for-update "test://update.com/"))))
    (test "Downloading files for update."
      (setup-mock-root-directory (base-directory self) (update-directory self))
      (define download-dir (build-path (base-directory self) "Temp"))
      (download-update "test://update.com/" (fn (a b) #f))
      (assert-set-equal `("MANIFEST.base" "MANIFEST.sub" 
                          ,foo-digest ,null-digest)
                        (directory-list download-dir))
      (assert-file-equals "foo\r\n" (build-path download-dir foo-digest))
      (assert-file-equals "" (build-path download-dir null-digest)))
    )
  
  (card updater-test
      (%test-suite%
       :tests (list <filesystem-test> <mock-downloader-test> <updater-test>)))
  )