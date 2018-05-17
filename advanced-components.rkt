#lang vr-lang

(provide mouseenter
         mouseleave
         on-click)

(register-remote-component dynamic-body "https://unpkg.com/aframe-physics-system@1.4.0/dist/aframe-physics-system.min.js")
(register-remote-component static-body "https://unpkg.com/aframe-physics-system@1.4.0/dist/aframe-physics-system.min.js")
(register-remote-component star-system "https://cdn.rawgit.com/matthewbryancurtis/aframe-star-system-component/db4f1030/index.js")
(register-remote-component ocean "https://unpkg.com/aframe-extras.ocean@%5E3.8.x/dist/aframe-extras.ocean.min.js")
(register-remote-component mountain "https://unpkg.com/aframe-mountain-component@0.3.x/dist/aframe-mountain-component.min.js")
(register-remote-component particle-system "https://unpkg.com/aframe-particle-system-component@1.0.9/dist/aframe-particle-system-component.min.js")
(register-remote-component environment "https://unpkg.com/aframe-environment-component@%5E1.0.x/dist/aframe-environment-component.min.js")
(register-remote-component event-set__click "https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js")
(register-remote-component event-set__mouseenter "https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js")
(register-remote-component event-set__mouseleave "https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js")
(register-remote-component random-bounce "https://ts-ballpit-complete.glitch.me/randomizer.js")
(register-remote-component random-color "https://ts-ballpit-complete.glitch.me/randomizer.js")
(register-remote-component random-position "https://ts-ballpit-complete.glitch.me/randomizer.js")

(define (mouseenter hash)
  (event-set__mouseenter hash))

(define (on-click hash)
  (event-set__click hash))

(define (mouseleave hash)
  (event-set__mouseleave hash))

