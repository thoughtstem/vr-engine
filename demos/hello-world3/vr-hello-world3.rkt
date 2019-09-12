#lang vr-engine

(define (my-box n)
  (box
   (position -1 n -3)
   (rotation 0 (* n 10) 0)
   (color 76 195 (* n 50) 255)))
 
(define my-scene
  (scene
   ;NOTE: A list of entities gets grouped under a single parent entity
   (map my-box (range 10)) 
   (sky (color 0 0 0 0))))

(send-to-browser my-scene)  
