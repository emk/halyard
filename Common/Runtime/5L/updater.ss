(module updater (lib "5L.ss" "5L")
  (require (lib "tamale.ss" "5L"))

  (provide dir-writeable? root-directory-writeable? delete-directory-recursive)
  
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
  
  (provide auto-update-possible?
           check-for-update
           download-update
           apply-update)
  
  ;; For now, this just checks if we can write to the root directory. In the 
  ;; future, we might want to check if the network is up, too, or check 
  ;; recursively that all files and directories are writeable. 
  (define auto-update-possible? root-directory-writeable?)
  
  (defclass <update> ()
    manifest-hash size user-info)
  
  ;; Check to see if an update is available. Returns the update information.
  (define (check-for-update)
    #f
    )
  
  ;; Downloads a particular update. Takes the update information object, 
  ;; and a function that will update a progress indicator. The 
  ;; progress indicator callback will take two arguments, a percentage and 
  ;; a string that indicates what is currently happening. 
  (define (download-update update progress)
    #f)
  
  ;; Applies a given update. Will launch an updater, pass it the information 
  ;; needed to apply the update, and quit the program so the updater can do 
  ;; its work. 
  (define (apply-update update)
    #f)
  
  (define (fetch-new-manifest) 
    ;; XXX - this is a fake stub that will just read a file named MANIFEST 
    ;; from the auto-update directory. 
    (define dir (ensure-dir-exists "Auto-update"))
    (build-path dir "MANIFEST"))
  
  (define (old-manifest)
    (build-path (current-directory) "MANIFEST"))
  
  )