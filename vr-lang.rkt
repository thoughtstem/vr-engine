(module vr-lang racket
  (provide
          send-to-browser
          ;--------------
          vr-scene
          basic-scene
          basic-entity
          sky
          box
          animation
          cylinder
          sphere
          plane
          tetrahedron
          cone
          basic
          light
          camera
          cursor
          assets
          assets-item
          obj-model
          ;--------------
          (all-from-out racket)
          (all-from-out web-server/servlet)
          (all-from-out web-server/servlet-env)
          (all-from-out "./my-ip-qr.rkt"
                        "./assets.rkt")
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
           "./vr.rkt"
           "./assets.rkt")

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

  ;--------------------------
  
  (define (vr-scene #:scene [s (scene)])
    (send-to-browser s))

  (define (basic-entity #:components-list [c '()])
    (entity "entity" c))

  (define (sky #:components-list [c (list (random-color))])
    (entity "sky" c))

  (define (box #:components-list [c (list (random-color))])
    (entity "box" c))

  (define (animation #:components-list [c '()])
    (entity "animation" c))

  (define (cylinder #:components-list [c (list (random-color))])
    (entity "cylinder" c))

  (define (sphere #:components-list [c (list (random-color))])
    (entity "sphere" c))

  (define (plane #:components-list [c (list (random-color))])
    (entity "plane" c))

  (define (tetrahedron #:components-list [c (list (random-color))])
    (entity "tetrahedron" c))

  (define (cone #:components-list [c (list (random-color))])
    (entity "cone" c))

  (define (light #:components-list [c '()])
    (entity "light" c))

  (define (camera #:components-list [c '()])
    (entity "camera" c))

  (define (cursor #:components-list [c '()])
    (entity "cursor" c))
  
  (define (assets #:components-list [c '()])
    (entity "assets" c))
  
  (define (assets-item #:components-list [c '()])
    (entity "assets-item" c))
  
  (define (obj-model #:components-list [c '()])
    (entity "obj-model" c))

  (define (basic-scene #:entities-list [e-list (list (sky))])
    (scene e-list))

  )

