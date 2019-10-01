#lang racket

(provide
   send-to-browser
   ;--------------
   vr-scene
   basic-environment
   basic-forest
   basic-volcano
   basic-entity
   basic-sky
   basic-camera
   basic-cursor
          
   basic-box
   basic-cone
   basic-cylinder
   basic-dodecahedron
   basic-icosahedron
   basic-octahedron
   basic-sphere
   basic-tetrahedron
          
   basic-circle
   basic-plane
   basic-ring
   basic-triangle

   add-stars
   add-ocean
   add-particles
   
   animation
   light
   assets
   assets-item
   obj-model
   gltf-model

   (rename-out [make-color color]
               [safe-position position])
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
           image-colors
           )


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
    
    (send-to-browser (scene s)))

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

(define (any-color-stx->rgb-list x)
  (cond
    [(string? x)(if (char=? #\# (string-ref x 0))
                    (hex->rgb-list x)
                    (color-name->rgb-list
                     (string-replace (string-downcase x) "-" "")))]
    [(symbol? x)(color-name->rgb-list x)]
    [else x]))

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

(define (any-color-stx->color-obj color)
  (define c (any-color-stx->rgb-list color))
  (if (object? c)
      c
      (make-color (first c) (second c) (third c))))

(define (any-color-stx->rgba-string color)
  (define c (any-color-stx->rgb-list color))
  (if (object? c)
      (render c)
      (if (false? c)
          c
          (~a "rgba(" (first c) "," (second c) "," (third c) ",255)"))))

(define (object->pair obj)
  (cons (string-trim
   (string-trim
    (~a (object-name obj)) "object:") "%") (render obj)))

(define (list-objects->hash l)
  (define pair-list (map object->pair l))
  (make-hash pair-list))

  ;-------------------------- ENVIRONMENTS
  (define (basic-environment #:preset                [preset 'default]
                             #:dressing              [dressing #f]
                             #:dressing-amount       [amount #f]
                             #:dressing-color        [color #f]
                             #:dressing-scale        [scale #f]
                             #:dressing-variance     [variance #f]
                             #:dressing-on-play-area [play-area #f]
                             #:fog                   [fog #f]
                             #:ground                [ground #f]
                             #:ground-color-1        [color-1 #f]
                             #:ground-color-2        [color-2 #f]
                             #:ground-texture        [texture #f]
                             #:horizon-color         [horizon #f]
                             #:other-components-list [comps '()])
    (define env-hash (hash
                      "preset"         (~a preset)
                      "dressing"       (~a dressing)
                      "dressingAmount" amount
                      "dressingColor"  (any-color-stx->rgba-string color)
                      "dressingScale"  scale
                      "dressingVariance" (and variance (send variance render))
                      "fog"            fog
                      "ground"         (~a ground)
                      "groundColor"    (any-color-stx->rgba-string color-1)
                      "groundColor2"   (any-color-stx->rgba-string color-2) 
                      "groundTexture"  (~a texture)
                      "horizonColor"   (any-color-stx->rgba-string horizon)))
    (define env (environment (make-hash (filter-not (λ(p) (or (equal? (cdr p) #f)
                                                              (equal? (cdr p) "#f")))
                                                    (hash->list env-hash)))))
    (basic-entity
     #:components-list (list env)))

  (define (basic-forest #:preset          [preset 'forest]
                        #:dressing        [dressing 'trees]
                        #:dressing-amount [amount 500]
                        #:dressing-color  [color "#888b1d"]
                        #:dressing-scale  [scale 1]
                        #:fog             [fog 0.800]
                        #:ground          [ground 'noise]
                        #:ground-color-1  [color-1 "#937a24"]
                        #:ground-color-2  [color-2 "#987d2e"]
                        #:ground-texture  [texture 'squares]
                        #:horizon-color   [horizon "#eff9b7"]
                        #:other-components-list [comps '()])

    (basic-environment #:preset          preset
                       #:dressing        dressing
                       #:dressing-amount amount
                       #:dressing-color  color
                       #:dressing-scale  scale
                       #:fog             fog
                       #:ground          ground
                       #:ground-color-1  color-1
                       #:ground-color-2  color-2
                       #:ground-texture  texture
                       #:horizon-color   horizon
                       #:other-components-list comps))
  
  (define (basic-volcano #:preset          [preset 'volcano]
                         #:dressing        [dressing 'arches]
                         #:dressing-amount [amount 15]
                         #:dressing-color  [color "#fb0803"]
                         #:dressing-scale  [scale 3]
                         #:fog             [fog 0.870]
                         #:ground          [ground 'canyon]
                         #:ground-color-1  [color-1 "#fb0803"]
                         #:ground-color-2  [color-2 "#510000"]
                         #:ground-texture  [texture 'walkernoise]
                         #:horizon-color   [horizon "#f62300"])

    (basic-environment #:preset          preset
                       #:dressing        dressing
                       #:dressing-amount amount
                       #:dressing-color  color
                       #:dressing-scale  scale
                       #:fog             fog
                       #:ground          ground
                       #:ground-color-1  color-1
                       #:ground-color-2  color-2
                       #:ground-texture  texture
                       #:horizon-color   horizon)
    )

  ;-------------------------- SKY - CAMERA - CURSOR
  (define (basic-cursor #:color   [col (color 0 0 0)]
                        #:opacity [opac 0.8]
                        #:visible [vis "true"]
                        #:components-list [c '()])
    (entity "cursor" (append (list (any-color-stx->color-obj col)
                                   (opacity opac)
                                   (visible vis))
                             c)))

  (define (basic-camera #:fly? [fly? #f]
                        #:acceleration [accel 65]
                        #:cursor [cursor (basic-cursor #:visible "false")]
                        #:components-list [comps '()])
    (entity "camera" (append (list (wasd-controls (hash "fly" (if fly? "true" "false")
                                                        "acceleration" accel))
                                   cursor)
                             comps)))

  (define (basic-sky #:color [col (color 255 255 255)]
                     #:opacity [opac 0.9]
                     #:components-list [c '()])
    (entity "sky" (append (list (any-color-stx->color-obj col)
                                (opacity opac))
                          c)))
  
  ;-------------------------- 3D OBJECTS
  (define (basic-box  #:position [posn (position 0.0 0.0 0.0)]
                      #:rotation [rota (rotation 0.0 0.0 0.0)]
                      #:scale [sca (scale 1.0 1.0 1.0)]
                      #:depth [dep 1.0]
                      #:height [hei 1.0]
                      #:width [wid 1.0]
                      #:color [col (color 128 128 128)]
                      #:opacity [opac 1.0]
                      #:texture [tex ""]
                      #:components-list [c '()])
    (entity "box" (append (list posn rota sca (any-color-stx->color-obj col)
                                (depth dep)
                                (height hei)
                                (width wid)
                                (opacity opac)
                                (src tex))
                          c)))

  (define (basic-cone #:position [posn (position 0.0 0.0 0.0)]
                      #:rotation [rota (rotation 0.0 0.0 0.0)]
                      #:scale [sca (scale 1.0 1.0 1.0)]                     
                      #:radius-bottom [radb 1.0]
                      #:radius-top [radt 0.01]
                      #:height [hei 1.0]
                      #:color [col (color 128 128 128)]
                      #:opacity [opac 1.0]
                      #:texture [tex ""]
                      #:components-list [c '()])
    (entity "cone" (append (list posn rota sca (any-color-stx->color-obj col)
                                 (radius-bottom radb)
                                 (radius-top radt)
                                 (height hei)
                                 (opacity opac)
                                 (src tex))
                           c)))
  
  (define (basic-cylinder #:position [posn (position 0.0 0.0 0.0)]
                          #:rotation [rota (rotation 0.0 0.0 0.0)]
                          #:scale [sca (scale 1.0 1.0 1.0)]
                          #:height [hei 1.0]
                          #:radius [r 0.5]
                          #:color [col (color 128 128 128)]
                          #:opacity [opac 1.0]
                          #:texture [tex ""]
                          #:components-list [c '()])
    (entity "cylinder" (append (list posn rota sca (any-color-stx->color-obj col)
                                     (radius r)
                                     (height hei)
                                     (opacity opac)
                                     (src tex))
                               c)))

  (define (basic-dodecahedron #:position [posn (position 0.0 0.0 0.0)]
                              #:rotation [rota (rotation 0.0 0.0 0.0)]
                              #:scale [sca (scale 1.0 1.0 1.0)]
                              #:radius [r 0.5]
                              #:color [col (color 128 128 128)]
                              #:opacity [opac 1.0]
                              #:texture [tex ""]
                              #:components-list [c '()])
    (entity "dodecahedron" (append (list posn rota sca (any-color-stx->color-obj col)
                                         (radius r)
                                         (opacity opac)
                                         (src tex))
                                   c)))

  (define (basic-icosahedron #:position [posn (position 0.0 0.0 0.0)]
                             #:rotation [rota (rotation 0.0 0.0 0.0)]
                             #:scale [sca (scale 1.0 1.0 1.0)]
                             #:radius [r 0.5]
                             #:color [col (color 128 128 128)]
                             #:opacity [opac 1.0]
                             #:texture [tex ""]
                             #:components-list [c '()])
    (entity "icosahedron" (append (list posn rota sca (any-color-stx->color-obj col)
                                        (radius r)
                                        (opacity opac)
                                        (src tex))
                                  c)))
  
  (define (basic-octahedron #:position [posn (position 0.0 0.0 0.0)]
                            #:rotation [rota (rotation 0.0 0.0 0.0)]
                            #:scale [sca (scale 1.0 1.0 1.0)]
                            #:radius [r 0.5]
                            #:color [col (color 128 128 128)]
                            #:opacity [opac 1.0]
                            #:texture [tex ""]
                            #:components-list [c '()])
    (entity "octahedron" (append (list posn rota sca (any-color-stx->color-obj col)
                                       (radius r)
                                       (opacity opac)
                                       (src tex))
                                 c)))
  
  (define (basic-sphere #:position [posn (position 0.0 0.0 0.0)]
                        #:rotation [rota (rotation 0.0 0.0 0.0)]
                        #:scale [sca (scale 1.0 1.0 1.0)]
                        #:radius [r 1.0]
                        #:color [col (color 128 128 128)]
                        #:opacity [opac 1.0]
                        #:on-mouse-enter [mouse-enter #f]
                        #:on-mouse-leave [mouse-leave #f]
                        #:on-mouse-click [mouse-click #f]
                        #:components-list [c '()])
    (entity "sphere" (append (list posn rota sca
                                   (any-color-stx->color-obj col)
                                   (radius r)
                                   (opacity opac)
                                   (mouseenter (list-objects->hash mouse-enter))
                                   (mouseleave (list-objects->hash mouse-leave))
                                   (on-click (list-objects->hash mouse-click))
                                   )
                             c)))


  (define (basic-tetrahedron #:position [posn (position 0.0 0.0 0.0)]
                             #:rotation [rota (rotation 0.0 0.0 0.0)]
                             #:scale [sca (scale 1.0 1.0 1.0)]
                             #:radius [r 0.5]
                             #:color [col (color 128 128 128)]
                             #:opacity [opac 1.0]
                             #:texture [tex ""]
                             #:components-list [c '()])
    (entity "tetrahedron" (append (list posn rota sca (any-color-stx->color-obj col)
                                        (radius r)
                                        (opacity opac)
                                        (src tex))
                                  c)))

  (define (torus #:components-list [c '()])
    (entity "torus" c))

  (define (torus-knot #:components-list [c '()])
    (entity "torusKnot" c))

  ;-------------------------- 2D OBJECTS  
  (define (basic-circle #:position [posn (position 0.0 0.0 0.0)]
                        #:rotation [rota (rotation 0.0 0.0 0.0)]
                        #:scale [sca (scale 1.0 1.0 1.0)]
                        #:radius [r 0.5]
                        #:color [col (color 128 128 128)]
                        #:opacity [opac 1.0]
                        #:texture [tex ""]
                        #:components-list [c '()])
    (entity "circle" (append (list posn rota sca (any-color-stx->color-obj col)
                                        (radius r)
                                        (opacity opac)
                                        (src tex))
                                  c)))

  (define (basic-plane #:position [posn (position 0.0 0.0 0.0)]
                       #:rotation [rota (rotation 0.0 0.0 0.0)]
                       #:scale [sca (scale 1.0 1.0 1.0)]
                       #:color [col (color 128 128 128)]
                       #:opacity [opac 1.0]
                       #:texture [tex ""]
                       #:height [hei 1.0]
                       #:width [wid 1.0]
                       #:components-list [c '()])
    (entity "plane" (append (list posn rota sca (any-color-stx->color-obj col)
                                  (height hei)
                                  (width wid)
                                  (opacity opac)
                                  (src tex))
                            c)))

  (define (basic-ring #:position [posn (position 0.0 0.0 0.0)]
                      #:rotation [rota (rotation 0.0 0.0 0.0)]
                      #:scale [sca (scale 1.0 1.0 1.0)]
                      #:color [col (color 128 128 128)]
                      #:radius-inner [radi 0.8]
                      #:radius-outer [rado 1.2]
                      #:opacity [opac 1.0]
                      #:texture [tex ""]
                      #:components-list [c '()])
    (entity "ring" (append (list posn rota sca (any-color-stx->color-obj col)
                                 (radius-inner radi)
                                 (radius-outer rado)
                                 (opacity opac)
                                 (src tex))
                           c)))

  (define (basic-triangle #:position [posn (position 0.0 0.0 0.0)]
                          #:rotation [rota (rotation 0.0 0.0 0.0)]
                          #:scale [sca (scale 1.0 1.0 1.0)]
                          #:color [col (color 128 128 128)]
                          ;#:vertex-a [a (vertex  0.0  0.5 0.0)]
                          ;#:vertex-b [b (vertex -0.5 -0.5 0.0)]
                          ;#:vertex-c [c (vertex  0.5 -0.5 0.0)]
                          #:opacity [opac 1.0]
                          #:texture [tex ""]
                          #:components-list [co '()])
    (entity "triangle" (append (list posn rota sca (any-color-stx->color-obj col)
                                 ;(vertex-a a)
                                 ;(vertex-b b)
                                 ;(vertex-c c)
                                 (opacity opac)
                                 (src tex))
                           co)))

  ;-------------------------- CUSTOM OBJECTS
  (define (add-stars #:position [posn (position 0.0 0.0 0.0)]
                     #:rotation [rota (rotation 0.0 0.0 0.0)]
                     #:scale [scale (scale 1.0 1.0 1.0)]
                     #:hex-color [col "#ffffff"]
                     #:count [count 10000]
                     #:depth [dep 180]
                     #:radius [rad 180]
                     #:star-size [size 1.0]
                     #:texture [texture ""])
    (basic-entity
     #:components-list (list (star-system (hash "color" col
                                                "depth" dep
                                                "radius" rad
                                                "starSize" size
                                                "texture" texture))
                             posn rota scale)))

  (define (add-ocean #:position [posn (position 0.0 0.0 0.0)]
                     #:rotation [rota (rotation -90.0 0.0 0.0)]
                     #:scale [scale (scale 1.0 1.0 1.0)]
                     #:amplitude [amp 0.1]
                     #:amplitude-variance [amp-var 0.3]
                     #:color [col "#7AD2F7"]
                     #:density [den 10]
                     #:depth [dep 10]
                     #:opacity [opa 0.8]
                     #:speed [spe 1.0]
                     #:speed-variance [spe-var 2.0]
                     #:width [wid 10])
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
                             posn rota scale)))

  (define (add-particles #:position [posn (position 0.0 0.0 0.0)]
                         #:rotation [rota (rotation 0.0 0.0 0.0)]
                         #:scale    [scale (scale 1.0 1.0 1.0)]
                         #:preset   [preset 'default]
                         #:texture  [texture #f]
                         #:size     [size    #f]
                         #:speed    [speed #f]
                         #:hex-color    [col #f]
                         )
    (define p-hash (hash
                    "preset"  (~a preset)
                    "texture" texture
                    "size"    size
                    "velocityValue"       (and speed "0 5 0")
                    "accelerationValue"   (and speed (~a 0 (- speed) 0 #:separator " "))
                    "accelerationSpread"  (and speed (~a speed 0 speed #:separator " "))
                    "color"   col))
    (define p-system (particle-system (make-hash (filter-not (λ(p) (or (equal? (cdr p) #f)
                                                                       (equal? (cdr p) "#f")))
                                                             (hash->list p-hash)))))
    (basic-entity
     #:components-list (list p-system
                             posn rota scale)))
    

  ;-------------------------- OTHER STUFF??
  (define (animation #:components-list [c '()])
    (entity "animation" c))

  (define (light #:components-list [c '()])
    (entity "light" c))
  
  (define (assets #:components-list [c '()])
    (entity "assets" c))
  
  (define (assets-item #:components-list [c '()])
    (entity "assets-item" c))
  
  (define (obj-model #:components-list [c '()])
    (entity "obj-model" c))

  (define (basic-entity #:components-list [c '()])
    (entity "entity" c))

  (define (gltf-model #:components-list [c '()])
    (entity "gltf-model" c))