#lang vr-engine

;Todos:
;   Register components: Auto-create the attributes (from the schema...)
;   Easy way to register remote components...
;   Test fancy components.  Particle systems?
;   Get 2htdp images into the ecosystem
;     E.g. particle-system="preset: dust; texture: ./images/star2.png; color: #0000FF,#00FF00,#FF0000"
;   Fun!!


(register-component random-color
                    init:
                    (var (randomHue (Math.floor (* (Math.random) 360))))
                    (this.el.setAttribute "color" (+ "hsl(" randomHue ",100%,50%)")))

(register-component random-color2
                    init:
                    (var (randR (Math.floor (* (Math.random) 255)))
                         (randG (Math.floor (* (Math.random) 255)))
                         (randB (Math.floor (* (Math.random) 255))))
                    (this.el.setAttribute "color" (+ "rgb(" randR "," randG "," randB ")" )))


(register-remote-component particle-system)

(define my-scene
  (scene
   
   (entity "sky" (random-color2))

   (entity "entity" (particle-system "preset: snow")
                    (position 0 0 -10))
 
   (entity "box"
           (position -1 0.5 -3)
           (random-color)
           (scale 1 1 1)
           (rotation 0 45 0))

   (entity "cylinder"
           (position 0 0.5 -3)
           (random-color)
           (height 1.5)
           (radius 0.5)
           (scale 1 1 1))

   (entity "sphere"
           (position 0 1.25 -5)
           (radius 1.25)
           (random-color)
           (entity "animation"
                   (attribute "position")
                   (to -3 1.5 3)
                   (direction "alternate")
                   (dur "400")
                   (repeat "indefinite")))

   (entity "plane"
           (position 0 0 -4)
           (rotation -90 0 0)
           (width 4)
           (height 4) 
           (random-color))))

(send-to-browser my-scene)
