#lang vr-lang

;Todo: Pull this into the html and auto-create the attributes (from the schema...)
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


(define (test-scene)

  ;Make a define-entity macro?
  (define test-sky
    (entity "sky"
            (random-color2)  ;Broken at the moment.  Need to create an attribute (->html)
            ))

  (define test-box
    (entity "box"
            (position -1 0.5 -3)
            (random-color)
            (scale 1 1 1)
            (rotation 0 45 0)))

  (define test-cyl
    (entity "cylinder"
            (position 0 0.5 -3)
            (random-color)
            (height 1.5)
            (radius 0.5)
            (scale 1 1 1)))


  (define test-sphere
    (entity "sphere"
            (position 0 1.25 -5)
            (radius 1.25)
            (random-color)
            (entity "animation"
                    (attribute "position")
                    (to -3 1.5 3)
                    (direction "alternate")
                    (dur "400")
                    (repeat "indefinite"))))

  (define test-plane
    (entity "plane"
            (position 0 0 -4)
            (rotation -90 0 0)
            (width 4)
            (height 4) 
            (random-color)
            ))

  (scene test-sky
         test-sphere
         test-box
         test-cyl
         test-plane))

;(scene->html (test-scene))


(send-to-browser (test-scene))
