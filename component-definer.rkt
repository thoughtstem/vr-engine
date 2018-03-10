#lang racket

(provide register-component
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
         #'(begin
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

