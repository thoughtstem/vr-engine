#lang racket


(provide
 (all-from-out ;"./assets.rkt"
               "./attribute-definer.rkt"
               "./component-definer.rkt"
               "./my-ip-qr.rkt"              
               "./vr-helpers.rkt"
               )
  (except-out (all-from-out "./vr.rkt")
                          color
                           position)
 ;(all-from-out 2htdp/image) 
 )

(require "./my-ip-qr.rkt"
         ;"./assets.rkt"
         "./component-definer.rkt"
         "./attribute-definer.rkt"
         (except-in "./vr.rkt"
                    color
                    position)
         "./vr-helpers.rkt"
         )
