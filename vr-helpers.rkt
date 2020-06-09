#lang at-exp racket

(provide
 send-to-browser
 ;--------------
 vr-scene

 basic-entity
 basic-sky
 basic-camera
 basic-cursor
           
 ;animation
 light
 assets
 assets-item
 gltf-model

 (rename-out [make-color color]
             [make-scale scale]
             [src texture]
             ;[make-animation animate]
             [safe-position position]
             [list do-many])

 posn-spread
 animate-rotation
 animate-position
 animate-scale
 )

(require racket/runtime-path
         web-server/servlet
         web-server/servlet-env
         (prefix-in h: 2htdp/image)
         "./my-ip-qr.rkt"
         "./component-definer.rkt"
         "./attribute-definer.rkt"
         "./vr.rkt"
         ;"./assets.rkt"
         image-coloring
         ts-kata-util
         )

(require scribble/srcdoc)
(require (for-doc racket/base scribble/manual ))


;Apparently this needs to be a macro so that scene->html executes in the context of the
;  User's script, so that the current-directory points to where they saved their rkt file

(define-syntax (send-to-browser stx)
  (syntax-case stx ()
    [(_ s)
     #'(send-html-to-browser (scene->html s))]))

(define-runtime-path pkg-js-path "js")

  
(define (send-html-to-browser s)
  (displayln (~a (current-directory)))
  (define current-js-path (build-path (current-directory) "js"))
  (delete-directory/files current-js-path #:must-exist? #f)
  (copy-directory/files pkg-js-path current-js-path)
  (define (my-app req)
    (response/xexpr
     `(html (head (title "Hello world!")
                  (script ((src ,(remote-url->local-url "https://aframe.io/aframe/dist/aframe-master.min.js"))))
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

;-------------------------- SCENE
(define (vr-scene ;#:environment         [env '()]
         ;#:sky                 [sky '()]
         ;#:camera              [cams '()]
         ;#:objects-list        [ents '()]
         ;#:custom-objects-list [remotes '()]
         #:entities      [ent #f]
         . custom-entities)
    
  (define s ;(append (list env sky cams) ents remotes
    (filter identity (flatten (cons ent custom-entities))))
  ;)

  (send-to-browser (scene s))
  )

;-------------------------- SOME FUNCTIONS
(define preset?
  (or/c 'default 'contact 'egypt 'checkerboard 'forest
        'goaland 'yavapai 'goldmine 'threetowers 'poison
        'arches 'tron 'japan 'dream 'volcano 'starry 'osiris))

(define dressing?
  (or/c 'cubes 'pyramids 'cylinders 'towers 'mushrooms
        'trees 'apparatus 'torii 'none))

(define ground?
  (or/c 'flat 'hills 'canyon 'spikes 'noise))

(define texture?
  (or/c 'checkerboard 'squares 'walkernoise))

(define (between-0-1-inclusive? x)
  (and (>= x 0) (<= x 1)))

;color/string helpers have been moved to image-coloring

(define (any-color-stx->rgb-list x)
  (cond
    [(object? x) (color-object->rgb-list x)]
    [(string? x) (if (char=? #\# (string-ref x 0))
                     (hex->rgb-list x)
                     (color-name->rgb-list
                      (string-replace (string-downcase x) "-" "")))]
    [(symbol? x)(color-name->rgb-list (string-replace (string-downcase (symbol->string x)) "-" ""))]
    [else x]))

(define (color-object->rgb-list color)
  (define c (send color render))
  (define l (string-split (string-trim (string-trim c "rgba(") ")") ","))
  (list (string->number (string-trim (first l) " "))
        (string->number (string-trim (second l) " "))
        (string->number (string-trim (third l) " "))))

(define (hex->rgb-list x)
  (define l (string->list (string-trim x "#")))
  (define r (string (first l) (second l)))
  (define g (string (third l) (fourth l)))
  (define b (string (fifth l) (sixth l)))
  (list (string->number (~a "#x" r))
        (string->number (~a "#x" g))
        (string->number (~a "#x" b))))

(define (color-name->rgb-list c)
  (define new-c (name->color c))
  (define r (h:color-red new-c))
  (define g (h:color-green new-c))
  (define b (h:color-blue new-c))
  (list r g b))

(define (any-color-stx->hex color)
  (define c (any-color-stx->rgb-list color))
  (define r (decimal->hex (first c)))
  (define g (decimal->hex (second c)))
  (define b (decimal->hex (third c)))
  (~a "#"
      (if (eq? r "") "00" r)
      (if (eq? g "") "00" g)
      (if (eq? b "") "00" b)))
  
(define (any-color-stx->color-obj color)
  (define c (any-color-stx->rgb-list color))
  (if (object? c)
      c
      (make-color (first c) (second c) (third c))))

(define (any-color-stx->rgba-string color)
  (define c (any-color-stx->rgb-list color))
  (if (object? c)
      (send c render)
      (if (false? c)
          c
          (~a "rgba(" (first c) "," (second c) "," (third c) ",255)"))))

(define (decimal->hex x (s ""))
  (define q 0)
  (define r 0)
  (define final s)
  (if (> x 0)
      (begin (set! q (quotient x 16))
             (set! r (remainder x 16))
  
             (set! r (cond
                       [(= r 10) "a"]
                       [(= r 11) "b"]
                       [(= r 12) "c"]
                       [(= r 13) "d"]
                       [(= r 14) "e"]
                       [(= r 15) "f"]
                       [else r]))
             (set! final (~a r final))
             (decimal->hex q final))
      final))

;-----------------  

(define (object->pair obj)
  (cons (string-trim
         (string-trim
          (~a (object-name obj)) "object:") "%") (render obj)))

(define (list-objects->hash l)
  (if (false? l)
      ""
      (make-hash (map object->pair l))))


(define (posn-spread x y z)
  (~a x " " y " " z))

(define (animate-rotation
         #:property [p "rotation"]
         #:from     [f ""]
         #:to       [t (position 0 360 0)]
         #:loops    [l "true"]
         #:duration [d 5000]
         #:direction [dir "normal"]
         )
  (animation__rotation p f (render t) l d dir)) 

(define (animate-position
         #:property [p "position"]
         #:from     [f ""]
         #:to       [t (position 0 20 0)]
         #:loops    [l "true"]
         #:duration [d 5000]
         #:direction [dir "alternate"])
  (animation__position p f (render t) l d dir)) 

(define (animate-scale
         #:property [p "scale"]
         #:from     [f ""]
         #:to       [t  2]
         #:loops    [l "true"]
         #:duration [d 5000]
         #:direction [dir "alternate"])
  (animation__scale p f (~a t " " t " " t) l d dir))




;-------------------------- ENVIRONMENTS
(define/contract/doc (basic-environment #:basic?                [basic? #t]
                                        #:preset                [preset 'default]
                                        #:dressing              [dressing #f]
                                        #:dressing-amount       [amount #f]
                                        #:dressing-color        [color #f]
                                        #:dressing-scale        [scale #f]
                                        #:dressing-variance     [variance #f]
                                        #:fog                   [fog 0]
                                        #:ground                [ground #f]
                                        #:ground-color-1        [color-1 #f]
                                        #:ground-color-2        [color-2 #f]
                                        #:ground-texture        [texture #f]
                                        #:horizon-color         [horizon #f]
                                        #:sky-color             [sky #f])
  (->i ()
       ( #:basic?                [basic? boolean?]
         #:preset                [preset (or/c preset?)]
         #:dressing              [dressing (or/c #f dressing?)]
         #:dressing-amount       [amount (or/c #f real?)]
         #:dressing-color        [color (or/c string? symbol? object?)] 
         #:dressing-scale        [scale (or/c #f real?)]
         #:dressing-variance     [variance (or/c #f object?)] 
         #:fog                   [fog (or/c between-0-1-inclusive?)] 
         #:ground                [ground (or/c #f ground?)]
         #:ground-color-1        [color-1 (or/c string? symbol? object?)]
         #:ground-color-2        [color-2 (or/c string? symbol? object?)]
         #:ground-texture        [texture (or/c #f texture?)]
         #:horizon-color         [horizon (or/c string? symbol? object?)]
         #:sky-color             [sky (or/c string? symbol? object?)] )
       [returns entity?])

  @{Basic Environment.}
  
  (define env-hash (hash
                    "preset"         (~a preset)
                    "dressing"       (~a dressing)
                    "dressingAmount" amount
                    "dressingColor"  (any-color-stx->rgba-string color)
                    "dressingScale"  scale
                    "dressingVariance" (and variance (send variance render))
                    "fog"            fog
                    "ground"         (if (and (eq? preset 'default) basic? (false? ground))
                                         "hills"
                                         (~a ground))
                    "groundColor"    (if (and (eq? preset 'default) basic? (false? color-1))
                                         "#004000"
                                         (any-color-stx->rgba-string color-1))
                    "groundColor2"   (if (and (eq? preset 'default) basic? (false? color-2))
                                         "#005300"
                                         (any-color-stx->rgba-string color-2))
                    "groundTexture"  (if (and (eq? preset 'default) basic? (false? texture))
                                         "walkernoise"
                                         (~a texture))
                    "horizonColor"   (any-color-stx->rgba-string horizon)
                    "skyColor"   (any-color-stx->rgba-string sky)))
  (define env (environment (make-hash (filter-not (λ(p) (or (equal? (cdr p) #f)
                                                            (equal? (cdr p) "#f")))
                                                  (hash->list env-hash)))))
  (basic-entity
   #:components-list (list env)))

;-------------------------- SKY - CAMERA - CURSOR
(define (basic-cursor #:color   [col (make-color 0 0 0)]
                      #:opacity [opac 0.8]
                      #:visible [vis "true"]
                      #:components-list [c '()])
  (entity "cursor" (append (list (any-color-stx->color-obj col)
                                 (opacity opac)
                                 (visible vis))
                           c)))

(define (basic-camera #:position [pos (position 0 1.6 0)]
                      #:fly? [fly? #f]
                      #:acceleration [accel 65]
                      #:cursor [cursor (basic-cursor #:visible "false")]
                      #:components-list [comps '()])
  (entity "camera" (append (list (wasd-controls (hash "fly" (if fly? "true" "false")
                                                      "acceleration" accel))
                                 pos
                                 cursor)
                           comps)))

(define (basic-sky #:radius [r 500]
                   #:color [col (make-color 255 255 255)]
                   #:opacity [opac 0.9]
                   #:components-list [c '()])
  (entity "sky" (append (list (any-color-stx->color-obj col)
                              (radius r)
                              (opacity opac))
                        c)))
  
;-------------------------- 3D OBJECTS
(define/contract/doc (basic-box  #:position [posn (position 0.0 0.0 0.0)]
                                 #:rotation [rota (rotation 0.0 0.0 0.0)] 
                                 #:scale [sca (make-scale 1.0 1.0 1.0)] 
                                 #:depth [dep 1.0] 
                                 #:height [hei 1.0] 
                                 #:width [wid 1.0] 
                                 #:color [col (make-color 128 128 128)] 
                                 #:opacity [opac 1.0] 
                                 #:texture [tex ""] 
                                 #:on-mouse-enter [mouse-enter #f] 
                                 #:on-mouse-leave [mouse-leave #f] 
                                 #:on-mouse-click [mouse-click #f] 
                                 #:animations-list [a-list '()] 
                                 #:components-list [c '()]) 
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c number? object?)]
        #:depth             [dep real?] 
        #:height            [hei real?] 
        #:width             [wid real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Box.}
  
  (entity "box" (append (list posn rota (if (number? sca)
                                            (make-scale sca sca sca)
                                            sca)
                              (any-color-stx->color-obj col)
                              (depth dep)
                              (height hei)
                              (width wid)
                              (opacity opac)
                              (src tex)
                              (mouseenter (list-objects->hash mouse-enter))
                              (mouseleave (list-objects->hash mouse-leave))
                              (on-click (list-objects->hash mouse-click)))
                        (append a-list c))))

(define/contract/doc (basic-cone #:position [posn (position 0.0 0.0 0.0)]
                                 #:rotation [rota (rotation 0.0 0.0 0.0)]
                                 #:scale [sca (make-scale 1.0 1.0 1.0)]                     
                                 #:radius-bottom [radb 1.0]
                                 #:radius-top [radt 0.01]
                                 #:height [hei 1.0]
                                 #:color [col (make-color 128 128 128)]
                                 #:opacity [opac 1.0]
                                 #:texture [tex ""]
                                 #:on-mouse-enter [mouse-enter #f]
                                 #:on-mouse-leave [mouse-leave #f]
                                 #:on-mouse-click [mouse-click #f]
                                 #:animations-list [a-list '()]
                                 #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius-bottom     [radb real?]
        #:radius-top        [radt real?]
        #:height            [hei real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Cone.}
  
  (entity "cone" (append (list posn rota (if (number? sca)
                                             (make-scale sca sca sca)
                                             sca)
                               (any-color-stx->color-obj col)
                               (radius-bottom radb)
                               (radius-top radt)
                               (height hei)
                               (opacity opac)
                               (src tex)
                               (mouseenter (list-objects->hash mouse-enter))
                               (mouseleave (list-objects->hash mouse-leave))
                               (on-click (list-objects->hash mouse-click)))
                         (append a-list c))))
  
(define/contract/doc (basic-cylinder #:position [posn (position 0.0 0.0 0.0)]
                                     #:rotation [rota (rotation 0.0 0.0 0.0)]
                                     #:scale [sca (make-scale 1.0 1.0 1.0)]
                                     #:height [hei 1.0]
                                     #:radius [r 0.5]
                                     #:color [col (make-color 128 128 128)]
                                     #:opacity [opac 1.0]
                                     #:texture [tex ""]
                                     #:on-mouse-enter [mouse-enter #f]
                                     #:on-mouse-leave [mouse-leave #f]
                                     #:on-mouse-click [mouse-click #f]
                                     #:animations-list [a-list '()]
                                     #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:height            [hei real?] 
        #:radius            [r real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Cylinder.}
  
  (entity "cylinder" (append (list posn rota (if (number? sca)
                                                 (make-scale sca sca sca)
                                                 sca)
                                   (any-color-stx->color-obj col)
                                   (radius r)
                                   (height hei)
                                   (opacity opac)
                                   (src tex)
                                   (mouseenter (list-objects->hash mouse-enter))
                                   (mouseleave (list-objects->hash mouse-leave))
                                   (on-click (list-objects->hash mouse-click)))
                             (append a-list c))))

(define/contract/doc (basic-dodecahedron #:position [posn (position 0.0 0.0 0.0)]
                                         #:rotation [rota (rotation 0.0 0.0 0.0)]
                                         #:scale [sca (make-scale 1.0 1.0 1.0)]
                                         #:radius [r 0.5]
                                         #:color [col (make-color 128 128 128)]
                                         #:opacity [opac 1.0]
                                         #:texture [tex ""]
                                         #:on-mouse-enter [mouse-enter #f]
                                         #:on-mouse-leave [mouse-leave #f]
                                         #:on-mouse-click [mouse-click #f]
                                         #:animations-list [a-list '()]
                                         #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius            [r real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Dodecahedron.}
  
  (entity "dodecahedron" (append (list posn rota (if (number? sca)
                                                     (make-scale sca sca sca)
                                                     sca)
                                       (any-color-stx->color-obj col)
                                       (radius r)
                                       (opacity opac)
                                       (src tex)
                                       (mouseenter (list-objects->hash mouse-enter))
                                       (mouseleave (list-objects->hash mouse-leave))
                                       (on-click (list-objects->hash mouse-click)))
                                 (append a-list c))))

(define/contract/doc (basic-icosahedron #:position [posn (position 0.0 0.0 0.0)]
                                        #:rotation [rota (rotation 0.0 0.0 0.0)]
                                        #:scale [sca (make-scale 1.0 1.0 1.0)]
                                        #:radius [r 0.5]
                                        #:color [col (make-color 128 128 128)]
                                        #:opacity [opac 1.0]
                                        #:texture [tex ""]
                                        #:on-mouse-enter [mouse-enter #f]
                                        #:on-mouse-leave [mouse-leave #f]
                                        #:on-mouse-click [mouse-click #f]
                                        #:animations-list [a-list '()]
                                        #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius            [r real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Icosahedron.}
  
  (entity "icosahedron" (append (list posn rota (if (number? sca)
                                                    (make-scale sca sca sca)
                                                    sca)
                                      (any-color-stx->color-obj col)
                                      (radius r)
                                      (opacity opac)
                                      (src tex)
                                      (mouseenter (list-objects->hash mouse-enter))
                                      (mouseleave (list-objects->hash mouse-leave))
                                      (on-click (list-objects->hash mouse-click)))
                                (append a-list c))))
  
(define/contract/doc (basic-octahedron #:position [posn (position 0.0 0.0 0.0)]
                                       #:rotation [rota (rotation 0.0 0.0 0.0)]
                                       #:scale [sca (make-scale 1.0 1.0 1.0)]
                                       #:radius [r 0.5]
                                       #:color [col (make-color 128 128 128)]
                                       #:opacity [opac 1.0]
                                       #:texture [tex ""]
                                       #:on-mouse-enter [mouse-enter #f]
                                       #:on-mouse-leave [mouse-leave #f]
                                       #:on-mouse-click [mouse-click #f]
                                       #:animations-list [a-list '()]
                                       #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius            [r real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Octahedron.}
  
  (entity "octahedron" (append (list posn rota (if (number? sca)
                                                   (make-scale sca sca sca)
                                                   sca)
                                     (any-color-stx->color-obj col)
                                     (radius r)
                                     (opacity opac)
                                     (src tex)
                                     (mouseenter (list-objects->hash mouse-enter))
                                     (mouseleave (list-objects->hash mouse-leave))
                                     (on-click (list-objects->hash mouse-click)))
                               (append a-list c))))
  
(define/contract/doc (basic-sphere #:position [posn (position 0.0 0.0 0.0)]
                                   #:rotation [rota (rotation 0.0 0.0 0.0)]
                                   #:scale [sca (make-scale 1.0 1.0 1.0)]
                                   #:radius [r 1.0]
                                   #:color [col (make-color 128 128 128)]
                                   #:opacity [opac 1.0]
                                   #:texture [tex ""]
                                   #:shader  [sha "standard"]
                                   #:on-mouse-enter [mouse-enter #f]
                                   #:on-mouse-leave [mouse-leave #f]
                                   #:on-mouse-click [mouse-click #f]
                                   #:animations-list [a-list '()]
                                   #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius            [r real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:shader            [sha (or/c string? symbol?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Sphere.}
  
  (entity "sphere" (append (list posn rota (if (number? sca)
                                               (make-scale sca sca sca)
                                               sca)
                                 (any-color-stx->color-obj col)
                                 (radius r)
                                 (opacity opac)
                                 (src tex)
                                 (material (hash "shader" sha))
                                 (mouseenter (list-objects->hash mouse-enter))
                                 (mouseleave (list-objects->hash mouse-leave))
                                 (on-click (list-objects->hash mouse-click))
                                 )
                           (append a-list c))))


(define/contract/doc (basic-tetrahedron #:position [posn (position 0.0 0.0 0.0)]
                                        #:rotation [rota (rotation 0.0 0.0 0.0)]
                                        #:scale [sca (make-scale 1.0 1.0 1.0)]
                                        #:radius [r 0.5]
                                        #:color [col (make-color 128 128 128)]
                                        #:opacity [opac 1.0]
                                        #:texture [tex ""]
                                        #:on-mouse-enter [mouse-enter #f]
                                        #:on-mouse-leave [mouse-leave #f]
                                        #:on-mouse-click [mouse-click #f]
                                        #:animations-list [a-list '()]
                                        #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius            [r real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Tetrahedron.}
  
  (entity "tetrahedron" (append (list posn rota (if (number? sca)
                                                    (make-scale sca sca sca)
                                                    sca)
                                      (any-color-stx->color-obj col)
                                      (radius r)
                                      (opacity opac)
                                      (src tex)
                                      (mouseenter (list-objects->hash mouse-enter))
                                      (mouseleave (list-objects->hash mouse-leave))
                                      (on-click (list-objects->hash mouse-click)))
                                (append a-list c))))

(define/contract/doc (basic-torus #:position [posn (position 0.0 0.0 0.0)]
                                  #:rotation [rota (rotation 0.0 0.0 0.0)]
                                  #:scale [sca (make-scale 1.0 1.0 1.0)]
                                  #:radius [r 0.5]
                                  #:radius-tubular [rt 0.3]
                                  #:color [col (make-color 128 128 128)]
                                  #:opacity [opac 1.0]
                                  #:texture [tex ""]
                                  #:on-mouse-enter [mouse-enter #f]
                                  #:on-mouse-leave [mouse-leave #f]
                                  #:on-mouse-click [mouse-click #f]
                                  #:animations-list [a-list '()]
                                  #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius            [r real?]
        #:radius-tubular    [rt real?]
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Torus.}
  
  (entity "torus" (append (list posn rota (if (number? sca)
                                              (make-scale sca sca sca)
                                              sca)
                                (any-color-stx->color-obj col)
                                (radius r)
                                (radius-tubular rt)
                                (opacity opac)
                                (src tex)
                                (mouseenter (list-objects->hash mouse-enter))
                                (mouseleave (list-objects->hash mouse-leave))
                                (on-click (list-objects->hash mouse-click)))
                          (append a-list c))))

(define (torus-knot #:components-list [c '()])
  (entity "torusKnot" c))

;-------------------------- 2D OBJECTS
(define/contract/doc (basic-text #:position [posn (position 0.0 0.0 0.0)]
                                 #:rotation [rota (rotation 0.0 0.0 0.0)]
                                 #:scale [sca (make-scale 1.0 1.0 1.0)]
                                 #:value [v "Hello, World!"]
                                 #:align [a 'center]
                                 #:baseline [b 'center]
                                 #:font [f 'roboto]
                                 #:letter-spacing [space 1]
                                 #:color [col (make-color 255 255 255)]
                                 #:opacity [opac 1.0]
                                 #:side [s 'double]
                                 #:on-mouse-enter [mouse-enter #f]
                                 #:on-mouse-leave [mouse-leave #f]
                                 #:on-mouse-click [mouse-click #f]
                                 #:animations-list [a-list '()]
                                 #:components-list [c '()]
                                 )
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:value             [val string?]
        #:align             [ali (or/c 'center 'left 'right)]
        #:baseline          [bas (or/c 'top 'center 'bottom)]
        #:font              [font symbol?]
        #:letter-spacing    [space real?] 
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:side              [side (or/c 'front 'back 'double)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Text.}
  
  (entity "text" (append (list posn rota (if (number? sca)
                                             (make-scale sca sca sca)
                                             sca)
                               (value v)
                               (align a)
                               (baseline b)
                               (font f)
                               (letter-spacing space)
                               (any-color-stx->color-obj col)
                               (opacity opac)
                               (side s)
                               (mouseenter (list-objects->hash mouse-enter))
                               (mouseleave (list-objects->hash mouse-leave))
                               (on-click (list-objects->hash mouse-click)))
                         (append a-list c))))

(define/contract/doc (basic-circle #:position [posn (position 0.0 0.0 0.0)]
                                   #:rotation [rota (rotation 0.0 0.0 0.0)]
                                   #:scale [sca (make-scale 1.0 1.0 1.0)]
                                   #:radius [r 0.5]
                                   #:color [col (make-color 128 128 128)]
                                   #:opacity [opac 1.0]
                                   #:texture [tex ""]
                                   #:on-mouse-enter [mouse-enter #f]
                                   #:on-mouse-leave [mouse-leave #f]
                                   #:on-mouse-click [mouse-click #f]
                                   #:animations-list [a-list '()]
                                   #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:radius            [r real?]
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Circle.}
  
  (entity "circle" (append (list posn rota (if (number? sca)
                                               (make-scale sca sca sca)
                                               sca)
                                 (any-color-stx->color-obj col)
                                 (radius r)
                                 (opacity opac)
                                 (src tex)
                                 (mouseenter (list-objects->hash mouse-enter))
                                 (mouseleave (list-objects->hash mouse-leave))
                                 (on-click (list-objects->hash mouse-click)))
                           (append a-list c))))

(define/contract/doc (basic-plane #:position [posn (position 0.0 0.0 0.0)]
                                  #:rotation [rota (rotation 0.0 0.0 0.0)]
                                  #:scale [sca (make-scale 1.0 1.0 1.0)]
                                  #:color [col (make-color 128 128 128)]
                                  #:opacity [opac 1.0]
                                  #:texture [tex ""]
                                  #:on-mouse-enter [mouse-enter #f]
                                  #:on-mouse-leave [mouse-leave #f]
                                  #:on-mouse-click [mouse-click #f]
                                  #:animations-list [a-list '()]
                                  #:height [hei 1.0]
                                  #:width [wid 1.0]
                                  #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:color             [col (or/c string? symbol? object?)] 
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))]
        #:height            [hei real?]
        #:width             [wid real?]
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Plane.}
  
  (entity "plane" (append (list posn rota (if (number? sca)
                                              (make-scale sca sca sca)
                                              sca)
                                (any-color-stx->color-obj col)
                                (height hei)
                                (width wid)
                                (opacity opac)
                                (src tex)
                                (mouseenter (list-objects->hash mouse-enter))
                                (mouseleave (list-objects->hash mouse-leave))
                                (on-click (list-objects->hash mouse-click)))
                          (append a-list c))))

(define/contract/doc (basic-ring #:position [posn (position 0.0 0.0 0.0)]
                                 #:rotation [rota (rotation 0.0 0.0 0.0)]
                                 #:scale [sca (make-scale 1.0 1.0 1.0)]
                                 #:color [col (make-color 128 128 128)]
                                 #:radius-inner [radi 0.8]
                                 #:radius-outer [rado 1.2]
                                 #:opacity [opac 1.0]
                                 #:texture [tex ""]
                                 #:shader  [sha "standard"]
                                 #:on-mouse-enter [mouse-enter #f]
                                 #:on-mouse-leave [mouse-leave #f]
                                 #:on-mouse-click [mouse-click #f]
                                 #:animations-list [a-list '()]
                                 #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:color             [col (or/c string? symbol? object?)]
        #:radius-inner      [radi real?]
        #:radius-outer      [rado real?]
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:shader            [sha (or/c string? symbol?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Ring.}
  
  (entity "ring" (append (list posn rota (if (number? sca)
                                             (make-scale sca sca sca)
                                             sca)
                               (any-color-stx->color-obj col)
                               (radius-inner radi)
                               (radius-outer rado)
                               (segments-theta 64)
                               (opacity opac)
                               (src tex)
                               (material (hash "side" "double"
                                               "shader" sha))
                               (mouseenter (list-objects->hash mouse-enter))
                               (mouseleave (list-objects->hash mouse-leave))
                               (on-click (list-objects->hash mouse-click)))
                         (append a-list c))))

(define/contract/doc (basic-triangle #:position [posn (position 0.0 0.0 0.0)]
                                     #:rotation [rota (rotation 0.0 0.0 0.0)]
                                     #:scale [sca (make-scale 1.0 1.0 1.0)]
                                     #:color [col (make-color 128 128 128)]
                                     ;#:vertex-a [a (vertex  0.0  0.5 0.0)]
                                     ;#:vertex-b [b (vertex -0.5 -0.5 0.0)]
                                     ;#:vertex-c [c (vertex  0.5 -0.5 0.0)]
                                     #:opacity [opac 1.0]
                                     #:texture [tex ""]
                                     #:on-mouse-enter [mouse-enter #f]
                                     #:on-mouse-leave [mouse-leave #f]
                                     #:on-mouse-click [mouse-click #f]
                                     #:animations-list [a-list '()]
                                     #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:color             [col (or/c string? symbol? object?)]
        #:opacity           [opac between-0-1-inclusive?]
        #:texture           [tex (or/c string? h:image?)]
        #:on-mouse-enter    [mouse-enter (or/c #f (listof object?))] 
        #:on-mouse-leave    [mouse-leave (or/c #f (listof object?))]
        #:on-mouse-click    [mouse-click (or/c #f (listof object?))]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic Triangle.}
  
  (entity "triangle" (append (list posn rota (if (number? sca)
                                                 (make-scale sca sca sca)
                                                 sca)
                                   (any-color-stx->color-obj col)
                                   ;(vertex-a a)
                                   ;(vertex-b b)
                                   ;(vertex-c c)
                                   (opacity opac)
                                   (src tex)
                                   (mouseenter (list-objects->hash mouse-enter))
                                   (mouseleave (list-objects->hash mouse-leave))
                                   (on-click (list-objects->hash mouse-click)))
                             (append a-list c))))

;-------------------------- CUSTOM OBJECTS
(define/contract/doc (3d-model #:position        [posn (position 0.0 0.0 0.0)]
                               #:rotation        [rota (rotation 0.0 0.0 0.0)]
                               #:scale           [sca (make-scale 1.0 1.0 1.0)]
                               #:model           [model ""]
                               #:animations-list  [a-list '()]
                               #:components-list [c '()])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:model             [model (or/c string? object?)]
        #:animations-list   [a-list (or/c empty? (listof object?))] 
        #:components-list   [c (or/c empty? (listof entity?))])
       (returns entity?))

  @{Basic 3D Model.}
  
  (entity "entity" (append (list posn rota (if (number? sca)
                                               (make-scale sca sca sca)
                                               sca) model)
                           (append a-list
                                   c))))

(define/contract/doc (basic-stars #:position [posn (position 0.0 0.0 0.0)]
                                  #:rotation [rota (rotation 0.0 0.0 0.0)]
                                  #:scale [sca (make-scale 1.0 1.0 1.0)]
                                  #:color [col "white"]
                                  #:count [count 10000]
                                  #:depth [dep 180]
                                  #:radius [rad 180]
                                  #:star-size [size 1.0]
                                  #:texture [texture ""])
  (->i ()
       (#:position          [posn object?] 
        #:rotation          [rota object?] 
        #:scale             [sca (or/c object? number?)]
        #:color             [col (or/c string? symbol? object?)]
        #:count             [count real?]
        #:depth             [depth real?]
        #:radius            [rad real?]
        #:star-size         [size real?]
        #:texture           [tex (or/c string? h:image?)])
       (returns entity?))

  @{Basic Stars.}
  
  (basic-entity
   #:components-list (list (star-system (hash "color" (any-color-stx->hex col)
                                              "depth" dep
                                              "radius" rad
                                              "starSize" size
                                              "texture" texture))
                           posn rota (if (number? sca)
                                         (make-scale sca sca sca)
                                         sca))))

(define/contract/doc (basic-ocean #:position [posn (position 0.0 0.0 0.0)]
                                  #:rotation [rota (rotation -90.0 0.0 0.0)]
                                  #:scale [sca (make-scale 1.0 1.0 1.0)]
                                  #:amplitude [amp 0.1]
                                  #:amplitude-variance [amp-var 0.3]
                                  #:color [col (make-color (random 256) (random 256) (random 256))]
                                  #:density [den 10]
                                  #:depth [dep 10]
                                  #:opacity [opa 0.8]
                                  #:speed [spe 1.0]
                                  #:speed-variance [spe-var 2.0]
                                  #:width [wid 10])
  (->i ()
       (#:position           [posn object?] 
        #:rotation           [rota object?] 
        #:scale              [sca (or/c object? number?)]
        #:amplitude          [amp real?]
        #:amplitude-variance [amp-var real?]
        #:color              [col (or/c string? symbol? object?)]
        #:density            [den real?]
        #:depth              [dep real?]
        #:opacity            [opac between-0-1-inclusive?]
        #:speed              [spe real?]
        #:speed-variance     [spe-var real?] 
        #:width              [wid real?])
       (returns entity?))

  @{Basic Ocean.}
  
  (basic-entity
   #:components-list (list (ocean (hash "amplitude" amp
                                        "amplitudeVariance" amp-var
                                        "color" (any-color-stx->rgba-string col)
                                        "density" den
                                        "depth" dep
                                        "opacity" opa
                                        "speed" spe
                                        "speedVariance" spe-var
                                        "width" wid))
                           posn rota (if (number? sca)
                                         (make-scale sca sca sca)
                                         sca))))

(define/contract/doc (basic-particles #:position    [posn (position 0.0 0.0 0.0)]
                                      #:rotation    [rota (rotation 0.0 0.0 0.0)]
                                      #:scale       [scale (make-scale 1.0 1.0 1.0)]
                                      #:preset      [preset 'default]
                                      #:image       [texture #f]
                                      #:size        [size    #f]
                                      #:speed       [speed #f]
                                      #:age         [age #f]
                                      #:color       [col #f]
                                      #:count       [count #f]
                                      #:posn-spread [spr #f]
                                      )
  (->i ()
       (#:position           [posn object?] 
        #:rotation           [rota object?] 
        #:scale              [scale object?]
        #:preset             [preset (or/c 'default 'dust 'snow 'rain)]
        #:image              [img (or/c #f h:image?)]
        #:size               [size (or/c #f real?)]
        #:speed              [spe (or/c #f real?)]
        #:age                [age (or/c #f real?)]
        #:color              [col (or/c #f string? symbol? object?)]
        #:count              [count (or/c #f real?)]
        #:posn-spread        [spr (or/c #f object?)])
       (returns entity?))

  @{Basic Particles.}
  
  (define p-hash (hash
                  "preset"  (~a preset)
                  "texture" texture
                  "size"    size
                  "velocityValue"       (and speed "0 5 0")
                  "accelerationValue"   (and speed (~a 0 (- speed) 0 #:separator " "))
                  "accelerationSpread"  (and speed (~a speed 0 speed #:separator " "))
                  "maxAge" age
                  "color"   (if (false? col) col (any-color-stx->hex col))
                  "positionSpread" spr
                  "particleCount" count))
  (define p-system (particle-system (make-hash (filter-not (λ(p) (or (equal? (cdr p) #f)
                                                                     (equal? (cdr p) "#f")))
                                                           (hash->list p-hash)))))
  (basic-entity
   #:components-list (list p-system
                           posn rota scale)))
    

;-------------------------- OTHER STUFF??
;(define (animation #:components-list [c '()])
;  (entity "animation" c))

(define (light #:components-list [c '()])
  (entity "light" c))
  
(define (assets #:components-list [c '()])
  (entity "assets" c))
  
(define (assets-item #:components-list [c '()])
  (entity "assets-item" c))
 

(define (basic-entity #:components-list [c '()])
  (entity "entity" c))

(define (gltf-model #:components-list [c '()])
  (entity "gltf-model" c))
