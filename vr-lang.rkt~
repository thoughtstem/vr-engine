(module vr-lang racket
  (provide
          send-to-browser
          (all-from-out racket)
          (all-from-out web-server/servlet)
          (all-from-out web-server/servlet-env)
          (all-from-out "./my-ip-qr.rkt")
          (all-from-out "./vr.rkt")
          (all-from-out "./component-definer.rkt")
          (all-from-out "./attribute-definer.rkt")
           #%module-begin)


(require web-server/servlet
         web-server/servlet-env
         
         "./my-ip-qr.rkt"
         "./component-definer.rkt"
         "./attribute-definer.rkt"
         "./vr.rkt")

  (define (component-imports)
    (define import (λ(c) `(script ((src ,(string-append c ".js"))))))
    (map import (hash-keys all-components)))

  (define (send-to-browser s)
    (define (my-app req)
      (response/xexpr
       `(html (head (title "Hello world!")
                    (script ((src "https://aframe.io/releases/0.7.0/aframe.min.js")))
                    ,@(component-imports))
              (body ,(my-ip-qr-img "/main")
                    ,(scene->html s)))))
 
    (serve/servlet my-app
                   #:port 8000
                   #:listen-ip #f
                   #:servlet-path "/main"
                   #:extra-files-paths
                   (list
                    (build-path "./"))))

  )

