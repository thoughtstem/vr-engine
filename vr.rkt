#lang racket

(require "./attribute-definer.rkt")
(require "./component-definer.rkt")


(provide (rename-out [make-scene  scene])
         (rename-out [make-entity entity])
         scene->html)

(define-namespace-anchor a)
(define compile-ns (namespace-anchor->namespace a))

(struct entity (name attrs children))
(struct scene (entities))

(define (make-entity name . stuff)
  (define-values
    (children attrs)
    (partition entity? stuff))

  (entity name attrs children))

(define (make-scene . entities)
  (scene entities))

(define (add-attr e a)
  (struct-copy entity e
               [attrs (cons a (entity-attrs e))]))

(define-attribute id  (s) "~a")
(define-attribute src (s) "~a")

(define-attribute height (n) "~a")
(define-attribute width  (n) "~a")
(define-attribute radius (n) "~a")

(define-attribute position (x y z) "~a ~a ~a")
(define-attribute rotation (x y z) "~a ~a ~a")
(define-attribute scale    (x y z) "~a ~a ~a")

(define-attribute color    (r g b a) "rgba(~a, ~a, ~a, ~a)")

(define-attribute attribute (s) "~a")
(define-attribute to        (x y z) "~a ~a ~a")
(define-attribute direction (s) "~a")
(define-attribute dur       (n) "~a")
(define-attribute repeat    (s) "~a")


(define (attr->html a)
  (list (send a my-name)
        (render a)))

(define (name->html n)
  (string->symbol (string-append "a-" n)))

(define (entity->html e)
  `( ,(name->html (entity-name e))
     ,(map attr->html (entity-attrs e))
     ,@(map entity->html (entity-children e))))
 
(define (scene->html s)
  `(a-scene ,@(map entity->html (scene-entities s))))

