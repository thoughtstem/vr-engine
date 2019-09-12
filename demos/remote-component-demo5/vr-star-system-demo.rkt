#lang vr-engine

(register-component random-color
                    init:
                    (var (randomHue (Math.floor (* (Math.random) 360))))
                    (this.el.setAttribute "color" (+ "hsl(" randomHue ",100%,50%)")))

(register-remote-component star-system "https://cdn.rawgit.com/matthewbryancurtis/aframe-star-system-component/db4f1030/index.js")

(define (my-box n)
  (box
   (position -1 n -3)
   (rotation 0 (* n 10) 0)
   (src "./stone-texture.png")
   (random-color)))
 
(define my-scene
  (scene
   (map my-box (range 10))
    
   (basic (star-system (hash "count" 1000
                             "radius" 40
                             "depth" 0
                             "texture" (h:overlay (h:circle 10 "solid" "black")     ;Note the h: prefix on 2htdp/image functions
                                                  (h:star   50 "solid" "green")))))
   (sky (color 0 0 0 0))))

(send-to-browser my-scene) 
