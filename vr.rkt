#lang racket

(require "./attribute-definer.rkt")
(require "./component-definer.rkt"
         image-coloring
         (prefix-in h: 2htdp/image))


(provide (rename-out [make-scene  scene])
         (rename-out [make-entity entity])
         (except-out (struct-out entity)
                     entity)
         update-attributes
         scene->html
         make-color
         wasd-controls
         safe-position
         ;make-animation
         )

(define-namespace-anchor a)
(define compile-ns (namespace-anchor->namespace a))

(struct entity (name attrs children))
(struct scene (entities components))

;(define (make-entity name . stuff)
(define (make-entity name stuff)
  (define-values
    (children attrs)
    (partition entity? stuff))

  (entity name attrs children))

(define (update-attributes e new-attrs)
  (struct-copy entity e [attrs new-attrs]))

(define (basic . components)
  (apply (curry make-entity "entity") components))

(define (make-scene . entities-and-components)
  (define (not-entity? e)
    (not (entity? e)))
  
  (define components (filter not-entity? (flatten entities-and-components)))
  (define entities   (filter entity? (flatten entities-and-components)))
  (scene entities
         components))

(define (add-attr e a)
  (struct-copy entity e
               [attrs (cons a (entity-attrs e))]))

(define-attribute id  (s) "~a")
(define-attribute src (s) "~a")
(define-attribute mtl (s) "~a")

(define-attribute fog (d c) "type: exponential; density: ~a; color: ~a")
(define-attribute preset (s) "~a")
(define-attribute no-click () "")
(define-attribute material.color (s) "~a")
(define-attribute raycaster () "objects: :not([no-click=\"\"])")

(define-attribute maxAge (n) "~a")
(define-attribute positionSpread (x y z) "~a ~a ~a")

(define-attribute rotationAxis (s) "~a")
(define-attribute rotationAngle (n) "~a")
(define-attribute accelerationValue (n) "~a")
(define-attribute accelerationSpread (x y z) "~a ~a ~a")
(define-attribute velocityValue (n) "~a")
(define-attribute velocitySpread (x y z) "~a ~a ~a")
(define-attribute size (n) "~a")
(define-attribute duration (n) "~a")
(define-attribute particleCount (n) "~a")
(define-attribute randomize (b) "~a")
(define-attribute opacity (n) "~a")
(define-attribute blending (n) "~a")
(define-attribute maxParticleCount (n) "~a")

(define-attribute height (n) "~a")
(define-attribute width  (n) "~a")
(define-attribute radius (n) "~a")
(define-attribute radius-bottom (n) "~a")
(define-attribute radius-top (n) "~a")

(define-attribute type (s) "~a")
(define-attribute intensity (n) "~a")
(define-attribute angle (n) "~a")
(define-attribute decay (n) "~a")
(define-attribute ground-color (r g b) "rgb(~a, ~a, ~a)")

(define-attribute position (x y z) "~a ~a ~a")

(define/contract (safe-position x y z)
  (-> number? number? number? any/c)
  (position x y z))

(define-attribute rotation (x y z) "~a ~a ~a")
(define-attribute scale    (x y z) "~a ~a ~a")
(define-attribute variance (x y z) "~a ~a ~a")

(define-attribute color    (r g b a) "rgba(~a, ~a, ~a, ~a)")

(define (make-color r (g 0) (b 0) (a 255))
  (cond
    [(symbol? r)(color-name->color r)]
    [(string? r)(if (char=? #\# (string-ref r 0))
                    (hex->color r)
                    (color-name->color
                     (string-replace (string-downcase r) "-" "")))]
    [(number? r) (color r g b a)]
    [else #f]))

; ------------
(define (color-name->color c)
  (define new-c (name->color c))
  (define r (h:color-red new-c))
  (define g (h:color-green new-c))
  (define b (h:color-blue new-c))
  (color r g b 255))

(define (hex->color x)
  (define l (string->list (string-trim x "#")))
  (define r (string (first l) (second l)))
  (define g (string (third l) (fourth l)))
  (define b (string (fifth l) (sixth l)))
  (color(string->number (~a "#x" r))
        (string->number (~a "#x" g))
        (string->number (~a "#x" b))
        255))
;----------------

(define-attribute attribute (s) "~a")
(define-attribute to        (x y z) "~a ~a ~a")
(define-attribute direction (s) "~a")
(define-attribute dur       (n) "~a")
(define-attribute repeat    (s) "~a")
(define-attribute fill      (s) "~a")
(define-attribute shadow    () "")

(define-attribute material (s) "~a")

;-----------
(define-attribute transparent (b) "~a")
(define-attribute wasd-controls (s)"~a")
(define-attribute fly (b) "~a")
(define-attribute acceleration (n) "~a")
(define-attribute visible (b) "~a")
(define-attribute depth  (n) "~a")
(define-attribute radius-inner (n) "~a")
(define-attribute radius-outer (n) "~a")
(define-attribute vertex (x y z) "~a ~a ~a")
(define-attribute vertex-a (x y z) "~a ~a ~a")
(define-attribute vertex-b (x y z) "~a ~a ~a")
(define-attribute vertex-c (x y z) "~a ~a ~a")
(define-attribute animation (p f t l d) "property: ~a; from: ~a; to: ~a; loop: ~a; dur: ~a")
(define-attribute animation__rotation (p f t l d) "property: ~a; from: ~a; to: ~a; loop: ~a; dur: ~a")
(define-attribute animation__position (p f t l d) "property: ~a; from: ~a; to: ~a; loop: ~a; dur: ~a")
(define-attribute animation__scale (p f t l d) "property: ~a; from: ~a; to: ~a; loop: ~a; dur: ~a")

#|
(define (make-animation
         #:property [p ""]
         #:from     [f ""]
         #:to       [t ""]
         #:loops    [l 1]
         #:duration [d 1000])
  (animation p f t l d))
|#
;-----------


(define (attr->html a)
  (list (send a my-name)
        (render a)))

(define (name->html n)
  (string->symbol (string-append "a-" n)))

(define (entity->html e)
  `( ,(name->html (entity-name e))
     ,(map attr->html (entity-attrs e))
     ,@(map entity->html (entity-children e))))

(define (list->entity e)
  (if (list? e)
      (apply basic e)
      e))
 
(define (scene->html s)
  `(a-scene
    (,@(map attr->html (scene-components s)))
    ,@(map entity->html  (map list->entity (scene-entities s)))))





