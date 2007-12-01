(module updater (lib "5l.ss" "5L")
  (require (lib "tamale.ss" "5L"))

  ;; TODO - these should probably be factored out into some sort of file-utils
  ;; library.
  (provide delete-directory-recursive copy-recursive copy-recursive-excluding
           read-string-from-file unsafe-directory-writeable?)
  
  ;;; On Vista, nobody (not even administrators) can directly write to
  ;;; "C:\Program Files".  But if we use UAC, we can ask the user to
  ;;; approve the changes and grant us write access (perhaps by typing an
  ;;; administrator password).
  (define (updater-can-use-uac-to-get-administrator-privileges?)
    (call-5l-prim 'IsVistaOrNewer))

  ;;; Test to see whether we can write to a directory.  DO NOT USE THIS
  ;;; FUNCTION ON VISTA!  If you test whether "C:\Program Files\Blah" is
  ;;; writable under Vista, this function will be fooled by the UAC
  ;;; backwards compatibility glue, and incorrectly return #t.
  (define (unsafe-directory-writeable? dir)
    (define path
      (build-path dir (cat "TEMP_PERMISSION_TEST_" (random 1000000000))))
    (with-handlers [[exn:fail:filesystem? (lambda (exn) #f)]]
      (define test (open-output-file path))
      (close-output-port test)
      (delete-file path)
      #t))

  (define (delete-directory-recursive path)
    (cond 
      [(link-exists? path) (delete-file path)]
      [(directory-exists? path) 
       (foreach [inner (directory-list path)]
         (delete-directory-recursive (build-path path inner)))
       (delete-directory path)]
      [(file-exists? path) (delete-file path)]))
  
  (define (strip-base path)
    (let-values [[[base file must-be-dir] (split-path path)]]
      file))
  
  ;; Copy a file, or recursively copy a directory and all contained files and 
  ;; directories. Is designed to emulate cp -r, so it copies links rather than
  ;; indirecting through them, and will not overwrite an existing directory 
  ;; but instead will copy into it. 
  (define (copy-recursive src dest)
    (copy-recursive-excluding '() src dest))
  
  (define (extract-filename path)
    (let-values [[(base name must-be-dir?) (split-path path)]]
      name))
  
  (define (copy-recursive-excluding exclude src dest)
    (if (directory-exists? dest)
      (copy-recursive-excluding exclude src (build-path dest (strip-base src)))
      (begin
        (cond 
          [(ormap (fn (re) (regexp-match re (path->string src))) exclude) 
           #t]
          [(link-exists? src) (copy-file src dest)]
          [(directory-exists? src)
           (make-directory dest)
           (foreach [file (directory-list src)]
             (copy-recursive-excluding exclude 
                                       (build-path src file) 
                                       (build-path dest file)))]
          [(file-exists? src) (copy-file src dest)])
        dest)))
  
  (define (read-string-from-file file)
    (with-input-from-file 
     file 
     (thunk (let loop [[str ""] [last ""]]
              (if (eof-object? last)
                str
                (loop (string-append str last) (read-string 100)))))))
  
  ;;===========================================================================

  (provide gpg-signature-valid? sha1-file)

  ;;; Verify the GPG signature of a file.  TRUSTED-KEYS-DIR should contain
  ;;; a keyring named "trustedkeys.gpg", specifying what public keys should
  ;;; be used to verify the signature.  SIGNATURE should be the path to the
  ;;; detached digital signature, and FILE should be the path of the signed
  ;;; file.
  ;;;
  ;;; Note that this function expects to find a copy of gpgv in
  ;;; (CURRENT-DIRECTORY).
  (define (gpg-signature-valid? trusted-keys-dir signature file)
    (with-values
        [[proc stdout stdin stderr]
         ;; PORTABILITY - Windows requires the full name gpgv.exe here.
         (subprocess #f #f #f (build-path (current-directory) "gpgv.exe")
                     "--homedir" (path->string trusted-keys-dir)
                     (path->string signature)
                     (path->string file))]
      (dynamic-wind
       (fn () (void))
       (fn ()
         (subprocess-wait proc)
         (= (subprocess-status proc) 0))
       (fn ()
         (close-input-port stdout)
         (close-output-port stdin)
         (close-input-port stderr)))))

  ;;; Calculate the SHA1 sum of a file, and return it as a string.
  (define (sha1-file file)
    (if (file-exists? file)
      (call-5l-prim 'Sha1File (path->string file))
      (error (cat "File does not exist: " file))))

  ;;===========================================================================
  
  (provide updater-security-error?
           <downloader> <mock-downloader> add-mock-url download cancel-download
           parse-manifest parse-spec-file program-release-id)
  
  ;;; Represents an updater security exception, which occurs whenever we
  ;;; can't verify the integrity of a file.  We have a special struct for
  ;;; security exceptions mostly so that we can write (ASSERT-RAISES
  ;;; UPDATER-SECURITY-ERROR? ...) in unit tests, and have the test fail if
  ;;; any *non*-security error occurs.
  (define-struct (updater-security-error exn:fail) (url))

  ;;; Raise a security exception for FILE.
  (define (security-error url)
    (raise (make-updater-security-error
            (string->immutable-string
             (cat "Could not verify integrity of " url))
            (current-continuation-marks)
            url)))

  (defclass <downloader> ()
    directory)
  
  (defclass <mock-downloader> (<downloader>)
    [files :initvalue '()])
  
  (define (add-mock-url mock-downloader url file)
    (push! (cons url file) (mock-downloader-files mock-downloader)))
  
  (define manifest-digest first)
  (define manifest-size second)
  (define manifest-file third)
  (define build-url cat)

  ;; SECURITY WARNING - URLs parsed by last-component are not fully sanitized, 
  ;; and so could be used to overwrite arbitrary files. Basic sanitation is 
  ;; done to prevent basic directory traversal attacks, but there may be other 
  ;; attacks possible.
  ;; TODO - should change this to a positive match (like [A-Za-z0-9._-]) 
  ;; instead of a negative match (anything but / or \). 
  (define (last-component url)
    (let [[results (regexp-match (regexp "/([^/\\\\]+)(#|$)") url)]]
      (if results
        (second results)
        #f)))
  
  (define (next-temp-file-in-dir dir)
    (define max-num
      (foldl (fn (file cur-max) 
               (let ((match (regexp-match (regexp "^temp([0-9]+)$") file)))
                 (if match
                   (max cur-max (read-from-string (second match)))
                   cur-max)))
             0
             (directory-list dir)))
    (cat "temp" (1+ max-num)))
  
  (define (download-file url file)
    (call-5l-prim 'Download url file))
  
  (define (cancel-download) 
    (call-5l-prim 'CancelDownload))
  
  ;;; Given a URL, a destination DIR, and an optional NAME (pass #f if you
  ;;; want to default it), calculate the name of our download target file.
  (define (find-download-target url dir name)
    (build-path dir 
                (or name
                    (last-component url)
                    (next-temp-file-in-dir dir))))
  
  ;;; Download URL into DIR, using the optional NAME as a filename.  If
  ;;; NAME is not supplied, default to either the URL's filename or a
  ;;; temporary name. Return the destination path of the downloaded file.
  (define (download url dir &key (name #f))
    (define path (find-download-target url dir name))
    (unless (download-file url path)
      (error (cat "Couldn't download " url " to " path)))
    path)

  ;;; Download to a temporary file, and check the SHA-1 hash before moving
  ;;; to the final location.  We don't _ever_ want to save an unverified
  ;;; file to its real name, because of the way an incomplete download will
  ;;; be resumed.
  ;;;
  ;;; This is a key part of checking download integrity--the
  ;;; release.spec.sig file provides a trusted base to start from, and the
  ;;; we need to walk down through the MANIFEST.* files to the actual
  ;;; update files, using the SHA1 hashes discovered in each step to verify
  ;;; the integrity of the next step.
  (define (download-verified sha1 url dir &key (name #f))
    ;; Download the file to a temporary location.
    (define temp-path (download url (temp-dir) :name "download.tmp"))

    ;; Check the SHA1 hash of the downloaded file.
    (define path (find-download-target url dir name))
    (define temp-sha1 (sha1-file temp-path))
    (unless (equals? sha1 temp-sha1)
      (security-error url))

    ;; OK, the file is good, so move it to its final location.
    (rename-file-or-directory temp-path path))

  (define (parse-manifest path)
    (with-input-from-file 
     path
     (thunk
       (parse-opened-manifest))))
  
  (define (parse-opened-manifest)
    (define r (regexp "^([0-9a-fA-F]*) ([0-9]+) (.+)$"))
    (let loop [[l '()]]
      (let [[line (read-line)]]
        (if (eof-object? line)
          (reverse l)
          (let [[m (regexp-match r line)]]
            (if m
              (loop (cons (list (second m) ; The hash
                                (string->number (third m)) ; The size
                                (fourth m)) ; The filename
                          l))))))))
  
  (define (parse-spec-file path)
    (with-input-from-file
     path
     (thunk
       (parse-opened-spec-file))))
  
  (define (parse-opened-spec-file)
    (define r (regexp "^([a-zA-Z0-9-]*): *(.*)$"))
    (let loop [[alist '()]]
      (let [[line (read-line)]]
        (cond
          ((eof-object? line) alist) ; TODO - should be an error
          ((= (string-length line) 0)
           (cons (list "MANIFEST" (parse-opened-manifest)) alist))
          (else (let [[m (regexp-match r line)]]
                  (if m
                    (loop (cons (cdr m) alist))
                    (error "Couldn't parse release specification."))))))))

  ;;; Return the ID of this program release as a string, or #f if this
  ;;; isn't an official release.
  (define (program-release-id)
    (define spec-file (build-path (current-directory) "release.spec"))
    (if (file-exists? spec-file)
        (second (assoc "Build" (parse-spec-file spec-file)))
        #f))

  
  ;;========================================================================
  
  (provide auto-update-possible?
           check-for-update
           download-update
           apply-update
           init-updater! 
           clear-updater!
           update-size)
  
  (define (spec-file-get alist key)
    (define entry (assoc key alist))
    (unless entry (error "Corrupt .spec file" key alist))
    (second entry))
  
  (define (auto-update-possible? dir)
    (and (file-exists? (build-path dir "release.spec"))
         (or (updater-can-use-uac-to-get-administrator-privileges?)
             ;; PORTABILITY - We're not on Vista, so we can get away with
             ;; using UNSAFE-DIRECTORY-WRITEABLE?.  But this code is still
             ;; Windows-specific.
             (unsafe-directory-writeable? (current-directory)))))
  
  (defclass <spec> ()
    url build meta-manifest)
  
  (define (read-spec file)
    (define raw (parse-spec-file file))
    (make <spec> 
          :url (spec-file-get raw "Update-URL")
          :build (spec-file-get raw "Build")
          :meta-manifest (spec-file-get raw "MANIFEST")))
  
  (defclass <updater> () 
    root-directory update-dir staging? base-spec update-spec url-prefix)
  
  (define *updater* #f)
  
  (define (ensure-dir-exists-absolute dir)
    (when (not (directory-exists? dir))
      (make-directory dir))
    dir)
  
  ;; TODO - work out dependencies between auto-update-possible and 
  ;; init-updater!, so I can make sure that updates are possible when 
  ;; I do init. 
  ;;
  ;; The :ROOT-DIRECTORY argument is really only useful for running test
  ;; suites.  Note that we put Updates/ in the SCRIPT-USER-DATA-DIRECTORY,
  ;; because that's one of the few places we're guaranteed to have write
  ;; privileges under Windows Vista.
  (define (init-updater! &key (root-directory #f) (staging? #f))
    (define root-dir (or root-directory (current-directory)))
    (define user-data-dir (or root-directory (script-user-data-directory)))
    (define update-dir 
      (ensure-dir-exists-absolute (build-path user-data-dir "Updates")))
    (define spec (read-spec (build-path root-dir "release.spec")))
    (set! *updater* 
          (make <updater> 
                :root-directory root-dir
                :staging? staging?
                :update-dir update-dir
                :base-spec spec
                :url-prefix (spec-url spec)))
    (ensure-dir-exists-absolute (temp-dir))
    (if (file-exists? (temp-dir "MANIFEST-DIFF"))
        (delete-file (temp-dir "MANIFEST-DIFF")))
    (ensure-dir-exists-absolute (pool-dir))
    (ensure-dir-exists-absolute (special-dir "manifests")))
  
  (define (special-dir &rest components)
    (apply build-path (updater-update-dir *updater*) components))
  (define (update-dir &rest components)
    (apply special-dir components))
  (define (temp-dir &rest components)
    (apply special-dir "temp" components))
  (define (manifest-dir build &rest components)
    (apply special-dir "manifests" build components))
  (define (pool-dir &rest components)
    (apply special-dir "pool" components))
  
  (define (update-build)
    (spec-build (updater-update-spec *updater*)))

  (define (register-update-spec file)        
    (define spec (read-spec file))
    ;; We change our URL to the one from the spec file we download, so we can
    ;; do simple redirects. 
    ;; TODO - reenable, once I find a better way to force a URL for unit
    ;; testing.
    ; (set! (updater-url-prefix *updater*) (spec-url spec))
    (set! (updater-update-spec *updater*) spec))
  
  (define (clear-updater!) (set! *updater* #f))
  
  (provide set-updater-url!)
  (define (set-updater-url! url) (set! (updater-url-prefix *updater*) url))
  
  (define (get-manifest-names dir)
    (define r (regexp "^MANIFEST\\..*$"))
    (filter (fn (file) (regexp-match r file))
            (map path->string (directory-list dir))))
  
  (define (file-hash-from-manifest manifest)
    (define hash (make-hash-table 'equal))
    (foreach (line manifest)
      (hash-table-put! hash (manifest-file line) line))
    hash)
  
  (provide diff-manifests)
  
  ;; TODO - complain if hashes match but sizes differ
  (define (diff-manifests base-manifest update-manifest)
    (define base-files (file-hash-from-manifest base-manifest))
    (define update-files (file-hash-from-manifest update-manifest))
    (define diff '())
    (foreach ((name info) update-files)
      (unless (equal? (hash-table-get base-files name (thunk #f)) info)
        (push! info diff)))
    diff)
  
  (define (parse-manifests-in-dir dir)
    (define manifests (get-manifest-names dir))
    (foldl append '() 
           (map (fn (file) (parse-manifest (build-path dir file))) manifests)))

  (define (manifest-url-prefix)
    (build-url (updater-url-prefix *updater*) 
               "manifests/" 
               (update-build)
               "/"))
  
  ;; TODO - should only get manifests that differ in the spec file.
  (provide get-manifest-diffs)
  (define (get-manifest-diffs)
    (if (file-exists? (temp-dir "MANIFEST-DIFF"))
      (parse-manifest (temp-dir "MANIFEST-DIFF"))
      (begin/var 
        (define root-dir (updater-root-directory *updater*))
        (define download-dir (manifest-dir (update-build)))
        (ensure-dir-exists-absolute download-dir)
        (define base-manifests (get-manifest-names root-dir))
        ;; We need to get the expected SHA-1 sums of the manifests.
        (define manifest-info
          (file-hash-from-manifest
            (spec-meta-manifest (updater-update-spec *updater*))))
        ;; Loop over our manifests, downloading them and verifying their
        ;; digests.
        (foreach [manifest base-manifests]
          ;; TODO - Brian said, "Deal with time issues."  I don't know what
          ;; this meant.
          (define expected-digest
            (manifest-digest
             (hash-table-get manifest-info manifest
                             (fn () (error (cat manifest
                                                " no longer in program"))))))
          (download-verified expected-digest
                             (build-url (manifest-url-prefix) manifest)
                             download-dir))
        (define diffs (diff-manifests (parse-manifests-in-dir root-dir)
                                      (parse-manifests-in-dir download-dir)))
        (write-diffs-to-file (temp-dir "MANIFEST-DIFF") diffs)
        diffs)))
  
  (define (write-diffs-to-file file diffs)
    (with-output-to-file 
     file
     (thunk 
       (foreach (entry diffs)
         (display (cat (manifest-digest entry) " " (manifest-size entry) " " 
                       (manifest-file entry) "\n"))))))
  
  (define (download-update-spec)
    (define url (cat (updater-url-prefix *updater*)
                     (if (updater-staging? *updater*)
                       "staging.spec"
                       "release.spec")))
    ;; Grab both the *.spec file and the *.spec.sig file *before* looking
    ;; into the *.spec file.  After all, we don't want to rely on any spec
    ;; inforamation (including the URL contained in the spec file) until
    ;; we've checked the signature.
    (download url (update-dir) :name "release.spec")
    (assert (file-exists? (update-dir "release.spec")))
    (download (cat url ".sig") (update-dir) :name "release.spec.sig")
    (assert (file-exists? (update-dir "release.spec.sig")))

    ;; If our signature is bad, fail *immediately*, and don't set anything
    ;; else up.
    (unless (gpg-signature-valid? (updater-root-directory *updater*)
                                  (update-dir "release.spec.sig")
                                  (update-dir "release.spec"))
      (security-error url))

    (register-update-spec (update-dir "release.spec")))
  
  ;; Check to see if an update is available. Returns #t if one is.
  ;; XXX - should only do diffs of manifests that we actually have present 
  ;; on this computer. If media has changed, and we have a web install,
  ;; we don't need to do an update. 
  (define (check-for-update)
    (download-update-spec)
    (not (eq? '() (diff-manifests 
                   (spec-meta-manifest (updater-base-spec *updater*))
                   (spec-meta-manifest (updater-update-spec *updater*))))))
  
  ;; Check the size of the update. This requires downloading the manifests, 
  ;; which are large. This should only be done once we have verified that 
  ;; updates are, indeed, available.
  ;; XXX - this isn't actually used, and is wrong. The size calculated by 
  ;;       download-update is more accurate.
  (define (update-size)
    (define diffs (get-manifest-diffs))
    (define counted '())
    (foldl (fn (entry total) 
             (if (member? (manifest-digest entry) counted)
               total
               (begin 
                 (set! counted (cons (manifest-digest entry) counted))
                 (+ total (manifest-size entry)))))
             0 diffs))
  
  ;; Downloads a particular update. Takes a progress indicator callback. The 
  ;; progress indicator callback will take two arguments, a percentage and a 
  ;; string that indicates what is currently happening. 
  ;; TODO - implement progress indicator
  ;; TODO - unit test errors
  (define (download-update progress)
    (define diffs (get-manifest-diffs))
    (define seen (make-hash-table 'equal))
    (define download-files 
      (filter (fn (file) 
                (define not-seen? 
                  (not (or (file-exists? (pool-dir (manifest-digest file)))
                           (hash-table-get 
                            seen (manifest-digest file) (fn () #f)))))
                (hash-table-put! seen (manifest-digest file) #t)
                not-seen?)
              diffs))
    (define total-size (foldl + 0 (map manifest-size download-files)))
    (define current-size 0)
    (define (report-progress file) 
      (progress (if (= total-size 0) 1 (/ current-size total-size)) 
                (manifest-file file)))
    (foreach (file download-files)
      (report-progress file)
      (download-verified (manifest-digest file)
                         (build-url (updater-url-prefix *updater*)
                                    "pool/"
                                    (manifest-digest file))
                         (pool-dir))
      (inc! current-size (manifest-size file))
      (report-progress file)))
  
  (define (copy-file-force src dest)
    (if (file-exists? dest)
        (delete-file dest))
    (copy-file src dest))

  ;; Applies a given update. Copies the update installer into place, launches 
  ;; it, passes it the information needed to apply the update, and quits the 
  ;; program so the installer can do its work. 
  ;; PORTABILITY - should work on non-Windows systems
  (define (apply-update)
    (define updater-exe-path
      (build-path (updater-update-dir *updater*) "UpdateInstaller.exe"))

    ;; If we have a new update installer, make a copy with the correct name
    ;; in UPDATER-UPDATE-DIR.  We used to just go ahead and install it into
    ;; its final location, but that won't work under Vista, because we can't
    ;; escalate to administrator privileges until we spawn a new process.
    (define found-updater? #f)
    (define diffs (get-manifest-diffs))
    (foreach (entry diffs)
      (when (equal? (manifest-file entry) "UpdateInstaller.exe")
        (copy-file-force (build-path (pool-dir) (manifest-digest entry)) 
                         updater-exe-path)
        (set! found-updater? #t)))

    ;; If we don't have a new update installer, copy the old one into our
    ;; Updates directory.  This way, it's always in a constant location.
    (unless found-updater?
      (copy-file-force (build-path (updater-root-directory *updater*) 
                                   "UpdateInstaller.exe")
                       updater-exe-path))

    (call-5l-prim 'LaunchUpdateInstallerBeforeExiting)
    (exit-script))
  )