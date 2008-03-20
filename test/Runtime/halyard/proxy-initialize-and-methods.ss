;;; Support for elements which forward unknown methods to a specific child
;;; element.  We also provide support for forwarding initializer keywords.
;;;
;;;   (define-class %foo-wrapper% (%box%)
;;;     (proxy-initialize-and-methods foo %foo%)
;;;     (setup
;;;       (.add-child-initializer! :message "Hello, world!")
;;;       (.create-child-element)))
;;;
;;; In this example, you can refer to the child element as (.foo).
(module proxy-initialize-and-methods (lib "halyard.ss" "halyard")

  (provide proxy-initialize-and-methods)

  (define (build-wrapper! klass child-name child-class)
    (with-instance klass
      ;;; When creating our child element, initialize KEYWORD using VALUE.
      (def (add-child-initializer! keyword value)
        (unless (has-slot? 'child-initializers)
          (set! (slot 'child-initializers)
                (list :parent self :name child-name)))
        (set! (slot 'child-initializers)
              (list* keyword value (slot 'child-initializers))))

      ;;; Create our child element with the specified class and
      ;;; initializers.  This function must be called from SETUP.
      (def (create-child-element)
        (apply send child-class 'new (slot 'child-initializers)))
      
      ;;; Proxy all unknown methods and initializer keywords to our child
      ;;; element.
      (def (method-missing name &rest args)
        (cond
         [(.initialized?)
          (apply send (send self child-name) name args)]
         [(setter-name? name)
          (assert (= (length args) 1))
          (.add-child-initializer! (setter-name->keyword name) (car args))]
         [else
          (error (cat "Tried to call ." name " on " self " before " child-name
                      " was initialized"))]))

      ;; Install a wrapper function to look up the child.
      (.define-method child-name
        (method () (.find-elem child-name)))
      
      ))

  (with-instance (%node% .class)
    ;;; Method-call version of INITIALIZE-AND-PROXY-FOR-CHILD-ELEMENT.
    (def (proxy-initialize-and-methods child-name child-class)
      (build-wrapper! self child-name child-class)))
  
  ;;; Specify that any unknown initializer keywords and method calls should
  ;;; be automatically forwarded to a child element named CHILD-NAME of
  ;;; type CHILD-CLASS.
  (define-syntax proxy-initialize-and-methods
    (syntax-rules ()
      [(_ child-name child-class)
       (.proxy-initialize-and-methods 'child-name child-class)]))
  
  )