#lang vr-engine

(define (my-box n)
  (box
   (position -1 n -3)
   (rotation 0 45 0)
   (color 76 195 (* n 50) 255)))
 
(define my-scene
  (scene
    
   (my-box 1)
   (my-box 2)
   (my-box 3)

   (sky (color 0 0 0 0))))

(send-to-browser my-scene) 
