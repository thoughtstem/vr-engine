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
  (displayln (string-append "SECOND " (path->string (current-directory))))
  (define file-name (next-filename))
  (define save-path (string-append (path->string (current-directory)) file-name))
  (hash-set! all-imgs i file-name)
  (save-image i save-path)
  (string-append "./" file-name))

(define (image->filename i)
  (if (hash-has-key? all-imgs i)
      (hash-ref all-imgs i)
      (saved-img i)))

(define (path->filename p)
  (define file-name (path->string (file-name-from-path p)))
  (define save-path (build-path (current-directory) file-name))
  ;(delete-directory/files build-assets-path #:must-exist? #f) ;TODO delete and save to assets folder
  (copy-file p save-path #t)
  file-name)

(define (convert-attr attr)
  (cond [(hash? attr) (string-join (map (λ(x) (format "~a:~a" (car x) (convert-attr (cdr x))))
                                        (hash->list attr)) ";")]
        [(image? attr) (image->filename attr)]
        [(path? attr) (path->filename attr)]
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
                    [alt-vars    (map (λ(s) (format-id stx "~a-alt" s))
                                      (syntax-e #'(vars ...)))]
                    [setters     (map list (syntax-e #'(vars ...)) (syntax-e #'alt-vars) )]
                    [accessors   (map (λ(s) #`(dynamic-get-field '#,(syntax->datum s) this)) (syntax-e #'(vars ...)))]
                    [name-s      (symbol->string (syntax->datum #'name))]
                    [alt-vars-with-defaults
                     (map (λ(s) `(,s ""))
                                      (syntax-e #'alt-vars))])
       #`(begin
           (provide name
                    classname)
           (define classname
             (class object%
               #,@#'init-fields
               (define/public (render)
                 (apply (curry format format-str) 
                        (convert-attrs #,@#'accessors)))
               (define/public (my-name)
                 (string->symbol name-s))
               (super-new)))
           (define (name #,@#'alt-vars-with-defaults)
             (new classname #,@#'setters))))]))

