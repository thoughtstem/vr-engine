#lang racket

(require "./attribute-definer.rkt")
(require "./component-definer.rkt")


(provide (rename-out [make-scene  scene])
         (rename-out [make-entity entity])
         sky
         box
         animation
         cylinder
         sphere
         plane
         tetrahedron
         cone
         basic
         scene->html
         make-color
         light
         camera
         cursor
         safe-position)

(define-namespace-anchor a)
(define compile-ns (namespace-anchor->namespace a))

(struct entity (name attrs children))
(struct scene (entities components))

(define (make-entity name . stuff)
  (define-values
    (children attrs)
    (partition entity? stuff))

  (entity name attrs children))

(define (sky . components)
  (apply (curry make-entity "sky") components))

(define (box . components)
  (apply (curry make-entity "box") components))

(define (animation . components)
  (apply (curry make-entity "animation") components))

(define (cylinder . components)
  (apply (curry make-entity "cylinder") components))

(define (sphere . components)
  (apply (curry make-entity "sphere") components))

(define (plane . components)
  (apply (curry make-entity "plane") components))

(define (tetrahedron . components)
  (apply (curry make-entity "tetrahedron") components))

(define (cone . components)
  (apply (curry make-entity "cone") components))

(define (light . components)
  (apply (curry make-entity "light") components))

(define (camera . components)
  (apply (curry make-entity "camera") components))

(define (cursor . components)
  (apply (curry make-entity "cursor") components))

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

(define-attribute color    (r g b a) "rgba(~a, ~a, ~a, ~a)")

(define (make-color r g b (a 255))
  (color r g b a))

(define-attribute attribute (s) "~a")
(define-attribute to        (x y z) "~a ~a ~a")
(define-attribute direction (s) "~a")
(define-attribute dur       (n) "~a")
(define-attribute repeat    (s) "~a")
(define-attribute fill      (s) "~a")
(define-attribute shadow    () "")

(define-attribute material (s) "~a")


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

