(module vr-lang racket
  (provide
          send-to-browser
          (all-from-out racket)
          (all-from-out web-server/servlet)
          (all-from-out web-server/servlet-env)
          (all-from-out "./my-ip-qr.rkt")
          (except-out (all-from-out "./vr.rkt")
                      color
                      position)
          (rename-out [make-color color]
                      [safe-position position])
          (all-from-out "./component-definer.rkt")
          (all-from-out "./attribute-definer.rkt")
          (all-from-out 2htdp/image)
           #%module-begin)


  (require web-server/servlet
           web-server/servlet-env
           (prefix-in h: 2htdp/image)
           "./my-ip-qr.rkt"
           "./component-definer.rkt"
           "./attribute-definer.rkt"
           "./vr.rkt")

  ;Apparently this needs to be a macro so that scene->html executes in the context of the
  ;  User's script, so that the current-directory points to where they saved their rkt file

  (define-syntax (send-to-browser stx)
    (syntax-case stx ()
      [(_ s)
       #'(send-html-to-browser (scene->html s))]))

  (define (send-html-to-browser s)
    (define (my-app req)
      (response/xexpr
       `(html (head (title "Hello world!")
                    (script ((src "https://aframe.io/aframe/dist/aframe-master.min.js")))
                    ,@(component-imports))
              (body ,(my-ip-qr-img "/main")
                    ,s))))
 
    (serve/servlet my-app
                   #:port 8000
                   #:listen-ip #f
                   #:servlet-path "/main"
                   #:extra-files-paths
                   (list
                    (build-path "./"))))

  )

