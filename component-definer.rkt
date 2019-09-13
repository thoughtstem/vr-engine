#lang racket

(provide register-component
         register-remote-component
         component-imports
         all-components
         (all-from-out urlang)
         tester-define
         remote-url->local-url)

(require "./attribute-definer.rkt")

(require urlang)

(require (for-syntax racket/syntax))

(define all-components (make-hash ))

(define-syntax (tester-define stx)
    (syntax-case stx ()
      [(_ name )
       #'(define-attribute name (s) "~a")]))

(define-syntax (register-component stx)
    (syntax-case stx (init: schema:)
      [(_ name init: lines ...)
       
       #'(register-component name schema: (object ) init: lines ...)]
      [(_ name schema: obj init: lines ...)
       (with-syntax ([name-s (symbol->string (syntax->datum #'name))]
                     [->html (format-id stx "~a->html" #'name)])
         #`(begin
             (hash-set! all-components name-s #t)
             (define-attribute name () "")
             (urlang
              (urmodule name
                        (import AFRAME Math this); module name
                        (AFRAME.registerComponent
                         name-s
                         (object [schema obj]
                                 [init (λ()
                                         lines ...
                                         )]
                                 ))))))]))


(define (remote-url->local-url str)
  (~a "js/" (last (string-split str "/"))))

(define-syntax (register-remote-component stx)
    (syntax-case stx ()
      [(_ name path)
       (with-syntax ([name-s (symbol->string (syntax->datum #'name))]
                     [->html (format-id stx "~a->html" #'name)])
         #`(begin
             (hash-set! all-components name-s (remote-url->local-url path))
             (define-attribute name (data-string) "~a")))]
      [(_ name)
       (with-syntax ([path (format "https://unpkg.com/aframe-~a-component/dist/aframe-~a-component.min.js"
                                   (syntax->datum #'name)
                                   (syntax->datum #'name))])
         #'(register-remote-component name path))]))


;For generating imports


  (define (local-name n)
    (format "~a.js" n))

  (define (is-remote? kv)
    (define k (cdr kv))
    (string? k))
  
  (define (import-tag kv)
    (define name (if (is-remote? kv) ;If it's false, it's a normal component, otherwise remote
                     (cdr kv)
                     (local-name (car kv))))
    `(script ((src ,(string-append name )))))
  
  (define (component-imports)
    (map import-tag (hash->list all-components)))

;-------------------- all of this was in advanced-components.rkt
(provide mouseenter
         mouseleave
         on-click)

(define (mouseenter hash)
  (event-set__mouseenter hash))

(define (on-click hash)
  (event-set__click hash))

(define (mouseleave hash)
  (event-set__mouseleave hash))

(register-remote-component dynamic-body          "https://unpkg.com/aframe-physics-system@1.4.0/dist/aframe-physics-system.min.js")
(register-remote-component static-body           "https://unpkg.com/aframe-physics-system@1.4.0/dist/aframe-physics-system.min.js")
(register-remote-component star-system           "https://cdn.rawgit.com/matthewbryancurtis/aframe-star-system-component/db4f1030/index.js")
(register-remote-component ocean                 "https://unpkg.com/aframe-extras.ocean@%5E3.8.x/dist/aframe-extras.ocean.min.js")
(register-remote-component mountain              "https://unpkg.com/aframe-mountain-component@0.3.x/dist/aframe-mountain-component.min.js")
(register-remote-component particle-system       "https://unpkg.com/aframe-particle-system-component@1.0.9/dist/aframe-particle-system-component.min.js")
(register-remote-component environment           "https://unpkg.com/aframe-environment-component@%5E1.0.x/dist/aframe-environment-component.min.js")
(register-remote-component event-set__click      "https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js")
(register-remote-component event-set__mouseenter "https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js")
(register-remote-component event-set__mouseleave "https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js")
(register-remote-component random-bounce         "https://ts-ballpit-complete.glitch.me/randomizer.js")
(register-remote-component random-color          "https://ts-ballpit-complete.glitch.me/randomizer.js")
(register-remote-component random-position       "https://ts-ballpit-complete.glitch.me/randomizer.js")

;Not needed?
#|(register-component random-color2
                    init:
                    (var (randR (Math.floor (* (Math.random) 255)))
                         (randG (Math.floor (* (Math.random) 255)))
                         (randB (Math.floor (* (Math.random) 255))))
                    (this.el.setAttribute "color" (+ "rgb(" randR "," randG "," randB ")" ))) |#