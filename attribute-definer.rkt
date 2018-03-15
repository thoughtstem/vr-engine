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

(require 2htdp/image
         ;pict
         net/base64
         file/convertible)

(define-for-syntax (repeat-str s n)
  (map (thunk* s) (range n)))



(define (render a)
  (send a render))


(define all-imgs (make-hash ))

(define (next-filename)
  (string-append (number->string (length (hash-keys all-imgs))) ".png"))

(define (saved-img i)
  (define file-name (next-filename))
  (hash-set! all-imgs i file-name)
  (save-image i file-name)
  file-name)

(define (image->filename i)
  (if (hash-has-key? all-imgs i)
      (hash-ref all-imgs i)
      (saved-img i)))

(define (convert-attr attr)
  (cond [(hash? attr) (string-join (map (λ(x) (format "~a:~a" (car x) (convert-attr (cdr x))))
                                        (hash->list attr)) ";")]
        [(image? attr) (image->filename attr)]
        [else attr]))

(define (convert-attrs . attrs)
  (map convert-attr attrs))

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
                 (apply (curry format format-str) 
                        (convert-attrs #,@#'accessors)))
               (define/public (my-name)
                 (string->symbol name-s))
               (super-new)))
           (define (name #,@#'alt-vars)
             (new classname #,@#'setters))))]))

