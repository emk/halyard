(module url-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide net:url^)
  
  (define-signature net:url^
    ((struct url (scheme host port path params query fragment))
     get-pure-port			;; url [x list (str)] -> in-port
     get-impure-port			;; url [x list (str)] -> in-port
     post-pure-port			;; url [x list (str)] -> in-port
     post-impure-port			;; url [x list (str)] -> in-port
     display-pure-port			;; in-port -> ()
     purify-port			;; in-port -> list (mime-header)
     netscape/string->url		;; (string -> url)
     string->url			;; str -> url
     url->string
     call/input-url			;; url x (url -> in-port) x
					;; (in-port -> T)
					;; [x list (str)] -> T
     combine-url/relative		;; url x str -> url
     url-exception?                     ;; T -> boolean

     current-proxy-servers)))           ;; (U ((U #f (list string num)) -> void) (-> (U #f (list string num))))

