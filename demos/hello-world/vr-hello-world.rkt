#lang vr-engine
 
(define my-scene
  (scene
 
   (box
    (position -1 0.5 -3)
    (rotation 0 45 0)
    (color 76 195 217 255))

   (sky (color 0 0 0 0))))

(send-to-browser my-scene) 
