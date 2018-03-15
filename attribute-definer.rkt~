#lang racket

(provide define-attribute
         fancy-define ;dumb test
         render)

(define-syntax (fancy-define stx)
    (syntax-case stx ()
      [(_ name val)
       #'(struct name () #:transparent)]))

(require (for-syntax racket/syntax))
(require (for-syntax racket))

(define-for-syntax (repeat-str s n)
  (map (thunk* s) (range n)))



(define (render a)
  (send a render))

(define-syntax (define-attribute stx)
  (syntax-case stx ()
    [(_ name (vars ...) format-str)
     (with-syntax* ([classname      (format-id stx "~a%" #'name)]
                    [vars/sym    (map syntax-e (syntax->list #'(vars ...)))]
                    [vars/id     (map (λ(s) (format-id stx "~a-~a" #'name s))
                                      (syntax->list #'vars/sym))]
                    [init-fields (map (λ(s) #`(init-field #,(syntax->datum s))) (syntax-e #'(vars ...)))]
                    [alt-vars    (map (λ(s) (format-id stx "~a-alt" s)) (syntax-e #'(vars ...)))]
                    [setters     (map list (syntax-e #'(vars ...)) (syntax-e #'alt-vars) )]
                    [accessors   (map (λ(s) #`(dynamic-get-field '#,(syntax->datum s) this)) (syntax-e #'(vars ...)))]
                    [name-s      (symbol->string (syntax->datum #'name))])
       #`(begin
           (provide name)
           (define classname
             (class object%
               #,@#'init-fields
               (define/public (render)
                 (format format-str 
                         #,@#'accessors))
               (define/public (my-name)
                 (string->symbol name-s))
               (super-new)))
           (define (name #,@#'alt-vars)
             (new classname #,@#'setters))))]))

;(define-attribute test2 (a b c) "~a ~a ~a")