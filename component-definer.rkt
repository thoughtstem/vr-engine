#lang racket

(provide register-component
         register-remote-component
         component-imports
         all-components
         (all-from-out urlang)
         tester-define)

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


(define-syntax (register-remote-component stx)
    (syntax-case stx ()
      [(_ name path)
       (with-syntax ([name-s (symbol->string (syntax->datum #'name))]
                     [->html (format-id stx "~a->html" #'name)])
         #`(begin
             (hash-set! all-components name-s path)
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




