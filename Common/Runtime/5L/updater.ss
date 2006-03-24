(module updater (lib "5L.ss" "5L")
  (require (lib "tamale.ss" "5L"))

  ;; TODO - these should probably be factored out into some sort of file-utils
  ;; library.
  (provide dir-writeable? root-directory-writeable? delete-directory-recursive 
           copy-recursive read-string-from-file)
  
  ;; TODO - should probably be moved to tamale.ss, and used in 
  ;; ensure-dir-exists. Also, is current-directory really the right way
  ;; to get the root directory? 
  (define (root-directory-writeable?)
    (dir-writeable? (current-directory)))
  
  (define (dir-writeable? dir)
    (memq 'write (file-or-directory-permissions dir)))
  
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
    (if (directory-exists? dest)
      (copy-recursive src (build-path dest (strip-base src)))
      (begin
        (cond 
          [(link-exists? src) (copy-file src dest)]
          [(directory-exists? src)
           (make-directory dest)
           (foreach [file (directory-list src)]
             (copy-recursive (build-path src file) (build-path dest file)))]
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
  
  (provide <mock-downloader> add-mock-url download mock-downloader-from-dir
           parse-manifest add-urls-from-manifests)
  
  (defclass <downloader> ()
    directory)
  
  (defclass <mock-downloader> (<downloader>)
    [files :initvalue '()])
  
  (define (add-mock-url mock-downloader url file)
    (push! (cons url file) (mock-downloader-files mock-downloader)))
  
  (define (mock-downloader-from-dir dir &key prefix download-dir)
    (define downloader (make <mock-downloader> :directory download-dir))
    (add-mock-urls-recursive downloader prefix dir)
    downloader)
  
  (define manifest-digest first)
  (define manifest-file second)
  (define build-url cat)
  
  (define (add-urls-from-manifests downloader prefix dir)
    (foreach (file (parse-manifests-in-dir dir))
      (add-mock-url 
       downloader 
       (build-url prefix (manifest-digest file))
       (read-string-from-file (build-path dir (manifest-file file))))))
  
  read-string-avail!
  
  (define (add-mock-urls-recursive downloader url path)
    (cond 
      [(link-exists? path) #f] 
      [(directory-exists? path) 
       (foreach [file (directory-list path)]
         (add-mock-urls-recursive
          downloader (cat url file) (build-path path file)))]
      [(file-exists? path) (add-mock-url downloader url
                                         (read-string-from-file path))]))

  ;; SECURITY WARNING - URLs parsed by last-component are not fully sanitized, 
  ;; and so could be used to overwrite arbitrary files. Basic sanitation is 
  ;; done to prevent basic directory traversal attacks, but there may be other 
  ;; attacks possible.
  (define (last-component url)
    (let [[results (regexp-match (regexp "/([^/\\\\]+)(#|$)") url)]]
      (if results
        (second results)
        results)))
  
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
           
  
  (define (download mock-downloader url &key (file #f))
    (define path 
      (build-path 
       (downloader-directory mock-downloader)  
       (or file (last-component url)
           (next-temp-file-in-dir (downloader-directory mock-downloader)))))
    (with-output-to-file 
     path
     (thunk 
       (define content (assoc url (mock-downloader-files mock-downloader)))
       (if content
         (display (cdr content))
         (error (cat "URL not found: " url))))))
  
  (define (parse-manifest path)
    (define r (regexp "^([0-9a-fA-F]*) +(.*)$"))
    (with-input-from-file 
     path
     (thunk 
       (let loop [[l '()]]
         (let [[line (read-line)]]
           (if (eof-object? line)
             (reverse l)
             (loop (cons (cdr (regexp-match r line)) l))))))))
         
  ;;========================================================================
  
  (provide auto-update-possible?
           check-for-update
           download-update
           apply-update
           set-update-downloader!
           set-update-root-directory!)
  
  ;; For now, this just checks if we can write to the root directory. In the 
  ;; future, we might want to check if the network is up, too, or check 
  ;; recursively that all files and directories are writeable. 
  (define auto-update-possible? root-directory-writeable?)
  
  (define *update-downloader* #f)
  (define *update-root-directory* #f)
  
  (define (set-update-downloader! val) (set! *update-downloader* val))
  (define (set-update-root-directory! val) (set! *update-root-directory* val))
  
  (define (create-and-set-download-dir! downloader)
    (define dir (build-path *update-root-directory* "Temp"))
    (unless (directory-exists? dir)
      (make-directory dir))
    (set! (downloader-directory downloader) dir)
    dir)
  
  (defclass <update> ()
    manifest-hash size user-info)
  
  (define (get-manifest-names dir)
    (define r (regexp "^MANIFEST\\..*$"))
    (filter (fn (file) (regexp-match r file)) (directory-list dir)))
  
  (define (file-hash-from-manifests manifest)
    (define hash (make-hash-table 'equal))
    (add-files-from-manifest hash manifest)
    hash)
  
  (define (add-files-from-manifest hash manifest)
    (foreach (line manifest)
      (hash-table-put! hash (second line) (first line))))
  
  (provide diff-manifests)
  
  (define (diff-manifests base-manifests update-manifests)
    (define base-files (file-hash-from-manifests base-manifests))
    (define update-files (file-hash-from-manifests update-manifests))
    (define diff '())
    (foreach ((file hash) update-files)
      (unless (equal? (hash-table-get base-files file (thunk #f)) hash)
        (push! (list hash file) diff)))
    diff)
  
  (define (parse-manifests-in-dir dir)
    (define manifests (get-manifest-names dir))
    (foldl append '() 
           (map (fn (file) (parse-manifest (build-path dir file))) manifests)))
  
  ;; TODO - check if diffs already exist. Should be optional, so we can force
  ;; a new download if we're doing check-for-update.
  (define (get-update-diffs url)
    (define download-dir (create-and-set-download-dir! *update-downloader*))
    (define base-manifests (get-manifest-names *update-root-directory*))
    (foreach (manifest base-manifests)
      ;; TODO - Deal with failures, deal with time issues.
      (download *update-downloader* (cat url manifest)))
    (diff-manifests (parse-manifests-in-dir *update-root-directory*)
                    (parse-manifests-in-dir download-dir)))
  
  ;; Check to see if an update is available. Returns #t if one is.
  (define (check-for-update url)
    (not (eq? (get-update-diffs url) '())))
  (require (lib "tamale-unit.ss" "5L"))
  ;; Downloads a particular update. Takes a URL to download from, and 
  ;; a progress indicator callback. The progress indicator callback will take 
  ;; two arguments, a percentage and a string that indicates what is 
  ;; currently happening. 
  (define (download-update url progress)
    (define diffs (get-update-diffs url))
    (foreach (file diffs)
      (unless (file-exists? 
               (build-path 
                *update-root-directory* "Temp" (manifest-digest file)))
        (download *update-downloader* 
                  (build-url url (manifest-digest file))))))
  
  ;; Applies a given update. Will launch an updater, pass it the information 
  ;; needed to apply the update, and quit the program so the updater can do 
  ;; its work. 
  (define (apply-update update)
    #f)
  
  )