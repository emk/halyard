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

(module updater-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "updater.ss" "halyard"))
  
  (define vc-exclude (list (regexp "\\.svn$")))
  
  (define (create-test-file dir name)
    (with-output-to-file (build-path dir name)
      (thunk (display name) (newline))))
  
  (define-class %filesystem-test% (%test-case%) 
    (attr test-directory #f :writable? #t)
    (attr outer-directory #f :writable? #t)
    (attr inner-directory #f :writable? #t)
    (setup-test
      (set! (.test-directory) (ensure-directory-exists "UpdateTest"))
      (set! (.outer-directory) 
            (ensure-directory-exists (build-path "UpdateTest" "Outer")))
      (set! (.inner-directory) 
            (ensure-directory-exists (build-path "UpdateTest" "Outer" "Inner")))
      (create-test-file (.outer-directory) "Deleted")
      (create-test-file (.inner-directory) "File1")
      (create-test-file (.inner-directory) "File2"))
    (teardown-test 
      (delete-directory-recursive (.test-directory)))
    ;; Note that we can get away with calling UNSAFE-DIRECTORY-WRITEABLE?
    ;; under Vista in this test case ONLY because we expect to be running
    ;; in a Subversion checkout, not in an installed copy.
    (test "New directory should be writeable."
      (assert (unsafe-directory-writeable? (.test-directory))))
    (test "delete-directory-recursive should delete only the correct files."
      (let ((deleted-dir (.outer-directory))
            (inner-dir (.inner-directory)))
        (create-test-file (.test-directory) "NotDeleted")
        
        ;; Make sure our files exist before the delete.
        (assert (directory-exists? deleted-dir))
        (assert (directory-exists? inner-dir))
        (assert (file-exists? (build-path deleted-dir "Deleted")))
        (assert (file-exists? (build-path inner-dir "File1")))
        (assert (file-exists? (build-path inner-dir "File2")))
        (assert (file-exists? 
                 (build-path (.test-directory) "NotDeleted")))
        
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
                 (build-path (.test-directory) "NotDeleted")))))
    (test "copy-recursive should copy correct files."
      (define copy-dir (build-path (.test-directory) "Copy"))
      (define inner-copy-dir (build-path copy-dir "Inner"))
      (copy-recursive-excluding (list (regexp "Deleted")) 
                                (.outer-directory) copy-dir)
      (assert (directory-exists? copy-dir))
      (assert (directory-exists? inner-copy-dir))
      (assert (not (file-exists? (build-path copy-dir "Deleted"))))
      (assert (file-exists? (build-path inner-copy-dir "File1")))
      (assert (file-exists? (build-path inner-copy-dir "File2")))))
  
  ;; TODO - possibly refactor these into some sort of file-utils and 
  ;; halyard-unit. 

  (define (assert-file-equals str file)
    (if (file-exists? file)
      (let ((contents (read-string-from-file file)))
        (unless (equal? str (read-string-from-file file))
          (error 
           (cat "Expected <" str "> in file <" file ">, got <" contents ">"))))
      (error (cat "File <" file "> does not exist"))))
  
  ;; NOTE - this is dead code. It's just here until I refactor the 
  ;; sanitization tests out. 
  (define-class %mock-downloader-test% (%test-case%) 
    (attr test-directory #f :writable? #t)
    (attr downloader #f :writable? #t)
    (setup-test 
      (set! (.test-directory) (ensure-directory-exists "MockTest"))
      (set! (.downloader) 
            (make <mock-downloader> :directory (.test-directory)))
      (add-mock-url (.downloader) "test://foo.com/bar" "foo\nbar\n")
      (add-mock-url (.downloader) "test://example.com/example" "example")
      (add-mock-url (.downloader) "test://evil.com/..\\BADFILE" "bad!")
      (add-mock-url (.downloader) "test://nofilename.com/" "noname"))
    (teardown-test 
      (delete-directory-recursive (.test-directory)))
    (test "mock-downloader should write files."
      (download (.downloader) "test://example.com/example")
      (download (.downloader) "test://foo.com/bar" :file "foobar")
      (assert-file-equals "example" 
                          (build-path (.test-directory) "example"))
      (assert-file-equals "foo\nbar\n" 
                          (build-path (.test-directory) "foobar")))
    (test "mock-downloader should do basic sanitization"
      (download (.downloader) "test://evil.com/..\\BADFILE")
      (download (.downloader) "test://nofilename.com/")
      (assert (not (file-exists? (build-path (current-directory) "BADFILE"))))
      (assert-file-equals "bad!" (build-path (.test-directory) "temp1"))
      (assert-file-equals "noname" 
                          (build-path (.test-directory) "temp2"))))
  
  (define-class %downloader-test% (%test-case%)
    (attr test-directory #f :writable? #t)
    (attr downloader #f :writable? #t)
    (attr url-prefix #f :writable? #t)
    (setup-test
      (set! (.test-directory) (ensure-directory-exists "DownloadTest"))
      (set! (.url-prefix) 
            (cat "file:///" (path->string (halyard-fixture-dir "updater")) 
                 "/downloader/")))
    (teardown-test
      (delete-directory-recursive (.test-directory)))
    (test "downloader should write files."
      (download (cat (.url-prefix) "test") (.test-directory))
      (download (cat (.url-prefix) "bar") (.test-directory) 
                :name "foobar")
      (assert-file-equals "test\n"
                          (build-path (.test-directory) "test"))
      (assert-file-equals "foo\nbar\n"
                          (build-path (.test-directory) "foobar"))))
  
  (define-class %parsing-test% (%test-case%)
    (attr base-directory (build-path (halyard-fixture-dir "updater") "base")
          :writable? #t)
    (test "Parsing manifests."
      (assert-equals 
       `((,null-digest 0 "bar.txt")
         (,null-digest 0 "foo.txt"))
       (parse-manifest (build-path (.base-directory) "MANIFEST.base")))
      (assert-equals 
       `((,null-digest 0 "sub/baz.txt")
         (,null-digest 0 "sub/foo.txt"))
       (parse-manifest (build-path (.base-directory) "MANIFEST.sub"))))
    (test "Parsing .spec files."
      (assert-set-equal 
       '(("Update-URL" "http://www.example.com/updates/")
         ("Build" "base")
         ("MANIFEST" (("142b5a7005ee1b9dc5f1cc2ec329acd0ad3cc9f6"
                       110 "MANIFEST.sub")
                      ("82b90fb155029800cd45f08d32df240d672dfd5b"
                       102 "MANIFEST.base"))))
       (parse-spec-file (build-path (.base-directory) "release.spec")))))
  
  (define null-digest "da39a3ee5e6b4b0d3255bfef95601890afd80709")
  (define foo-digest "855426068ee8939df6bce2c2c4b1e7346532a133")
  
  ;; TODO - macroize?, move to halyard-unit
  (define (assert-set-equal a b)
    (foreach (item a)
      (unless (member? item b)
        (error (cat "Couldn't find " item " in " b))))
    (foreach (item b)
      (unless (member? item a)
        (error (cat "Couldn't find " item " in " a)))))

  ;; Construct a URL to one of our fake update servers.
  (define (update-server-url name)
    (cat "file:///" (path->string (halyard-fixture-dir "updater"))
         "/" name "/"))

  ;; TODO - add test case for update spec file having new URL. 
  (define-class %updater-test% (%test-case%) 
    (attr test-directory #f :writable? #t)
    (attr base-directory #f :writable? #t)
    (attr update-directory #f :writable? #t)
    (attr url-prefix #f :writable? #t)
    (setup-test 
      (set! (.test-directory) (ensure-directory-exists "UpdaterTest"))
      (set! (.base-directory)
            (copy-recursive-excluding vc-exclude 
             (build-path (halyard-fixture-dir "updater") "base") 
             (.test-directory)))
      (set! (.update-directory)
            (copy-recursive-excluding vc-exclude 
             (build-path (halyard-fixture-dir "updater") "update")
             (.test-directory)))
      (set! (.url-prefix) (update-server-url "update-server")))
    (teardown-test 
      (delete-directory-recursive (.test-directory))
      (clear-updater!))
    (test "diff-manifests should list everything the update installer must change."
      (define manifest-a '(("123" 0 "foo.txt")
                           ("456" 1 "bar.txt")
                           ("ABC" 2 "sub/thing.txt")))
      (define manifest-b '(("125" 2 "foo.txt")
                           ("456" 1 "bar.txt")
                           ("ABC" 2 "SUB/thing.txt")
                           ("DEF" 3 "SUB/zot.txt")
                           ("ABC" 2 "SUB/thing-dup.txt")))
      (assert-set-equal '() (diff-manifests manifest-a manifest-a))
      (assert-set-equal '(("125" 2 "foo.txt")
                          ("ABC" 2 "SUB/thing.txt")
                          ("DEF" 3 "SUB/zot.txt")
                          ("ABC" 2 "SUB/thing-dup.txt")) 
                        (diff-manifests manifest-a manifest-b)))
    (test "Automatic update should be possible." 
      (assert (auto-update-possible? (.base-directory)))
      (init-updater! :root-directory (.base-directory)))
    (test "Checking for staging update, update should be available."
      (assert (auto-update-possible? (.base-directory)))
      (init-updater! :root-directory (.base-directory) :staging? #t)
      (set-updater-url! (.url-prefix))
      (assert (check-for-update))
      (assert (not (null? (get-manifest-diffs))))
      (assert-equals 5 (update-size)))
    (test "Checking for staging update, update should not be available."
      (assert (auto-update-possible? (.update-directory)))
      (init-updater! :root-directory (.update-directory) :staging? #t)
      (set-updater-url! (.url-prefix))
      (assert (not (check-for-update)))
      (assert-equals '() (get-manifest-diffs)))
    (test "Checking for regular update, update should not be available."
      (assert (auto-update-possible? (.base-directory)))
      (init-updater! :root-directory (.base-directory))
      (set-updater-url! (.url-prefix))
      (assert (not (check-for-update)))
      (assert-equals '() (get-manifest-diffs)))
    (test "Checking for downgrade, update should be available."
      (assert (auto-update-possible? (.update-directory)))
      (init-updater! :root-directory (.update-directory))
      (set-updater-url! (.url-prefix))
      (assert (check-for-update))
      (assert (not (null? (get-manifest-diffs))))
      (assert-equals 0 (update-size)))
    (test "Downloading files for update."
      (assert (auto-update-possible? (.base-directory)))
      (init-updater! :root-directory (.base-directory) :staging? #t)
      (set-updater-url! (.url-prefix))
      (assert (check-for-update))
      (define download-dir (build-path (.base-directory) "Updates"))
      (define callback-args '())
          
      (download-update (fn (a b) (push! (list a b) callback-args)))
          
      (assert-set-equal '("release.spec" "release.spec.sig"
                          "manifests" "pool" "temp")
                        (map path->string (directory-list download-dir)))
      ;; Note that we have new files which hash to null-digest; but we
      ;; should not be downloading those, since we already have files
      ;; which have that hash on disk; instead, the update installer
      ;; is expected to copy those files from wherever they already
      ;; are.
      (assert-set-equal 
       (list foo-digest)
       (map path->string 
            (directory-list (build-path download-dir "pool"))))
      (assert-set-equal 
       '("update") 
       (map path->string 
            (directory-list (build-path download-dir "manifests"))))
      (assert-set-equal '("MANIFEST.base" "MANIFEST.sub")
                        (map path->string
                             (directory-list 
                              (build-path download-dir "manifests" "Update"))))
      (assert-file-equals "foo\r\n" 
                          (build-path download-dir "pool" foo-digest))

      ;; XXX - this is an unstable test. Because there are two files that 
      ;; are being updated to "foo\r\n", we might be reporting either one
      ;; of them as the one we download.
      (assert-set-equal '((0 "foo.txt") 
                          (1 "foo.txt")) callback-args))

    (test "Updater should check signature on *.spec file"
      (assert (auto-update-possible? (.base-directory)))
      (init-updater! :root-directory (.base-directory) :staging? #t)
      (set-updater-url! (update-server-url "update-server-bad-sig"))
      (assert-raises updater-security-error?
                     (check-for-update)))

    (test "Update should fail if manifest has been modified"
      (assert (auto-update-possible? (.base-directory)))
      (init-updater! :root-directory (.base-directory) :staging? #t)
      (set-updater-url! (update-server-url "update-server-bad-manifest"))
      (assert (check-for-update))
      (assert-raises updater-security-error?
                     (download-update (fn (a b) (void)))))

    (test "Update should fail if file has been modified"
      (assert (auto-update-possible? (.base-directory)))
      (init-updater! :root-directory (.base-directory) :staging? #t)
      (set-updater-url! (update-server-url "update-server-bad-file"))
      (assert (check-for-update))
      (assert-raises updater-security-error?
                     (download-update (fn (a b) (void)))))

    )
  
  (define (sig-dir)
    (build-path (halyard-fixture-dir "updater") "signatures"))

  (define (sig-key-dir)
    (build-path (sig-dir) "config"))

  (define (sig-file name)
    (build-path (sig-dir) name))

  (define-class %crypto-test% (%test-case%) 
    (test "Good signatures pass validation"
      (assert (gpg-signature-valid? (sig-key-dir)
                                    (sig-file "good.txt.sig")
                                    (sig-file "good.txt"))))
    (test "Invalid signatures fail validation"
      (assert (not (gpg-signature-valid? (sig-key-dir)
                                         (sig-file "bad1.txt.sig")
                                         (sig-file "bad1.txt")))))
    (test "Signatures from unknown keys fail validation"
      (assert (not (gpg-signature-valid? (sig-key-dir)
                                         (sig-file "bad2.txt.sig")
                                         (sig-file "bad2.txt")))))
    (test "SHA 1 hashes for files should be correct"
      (assert-equals "7a499dc7e5c2237b6e85de0f9cada0aa1af0060a"
                     (sha1-file (sig-file "good.txt"))))
    )

  ;; For testing the installer:
  (provide create-installer-fixture)
  (define (create-installer-fixture)
    (define fix-dir (ensure-directory-exists "InstallerFixture"))
    (define base-dir (copy-recursive-excluding vc-exclude 
                      (build-path (halyard-fixture-dir "updater") "base") 
                      fix-dir))
    (define url-prefix 
      (cat "file:///" (halyard-fixture-dir "updater") "/update-server/"))
    (init-updater! :root-directory base-dir :staging? #t)
    (set-updater-url! url-prefix)
    (assert (auto-update-possible?))
    (assert (check-for-update))
    (download-update (fn (a b) #f)))
  
  (card /tests/updater
      (%test-suite%
       :tests (list %filesystem-test% %downloader-test% %parsing-test% 
                    %updater-test% %crypto-test%)))
  )