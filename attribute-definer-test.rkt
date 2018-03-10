#lang racket

#;(
(define test2%
  (class object%
    (init-field a)
    (init-field b)
    (init-field c)
    (define/public
      (render)
      (format "~a ~a ~a" a b c))
    (super-new)))

(send (new test2% (a 1)
                  (b 2)
                  (c 3)) render)

)

#;(
(require "./attribute-definer.rkt")
(require "./component-definer.rkt")

(define-attribute test2 (a b c) "~a ~a ~a")

(render (test2 1 2 3))

(tester-define test3 )
(render (test3 42))
)
