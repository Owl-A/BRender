#lang racket

(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt" "shaders.rkt" "render.rkt")
; examples

(define Scene1
  (new Scene%
       [objects (list (new primitive% [color (color 255 0 0)]
                           [mesh (new smooth-sphere% [center '(15 0 0)] [radius 5])])
                      (new primitive% [color (color 0 255 0)]
                           [mesh (new parrallelogram% [P1 '(35 -6 20)][P2 '(35 -6 -20)][P3 '(10 -6 -20)]  )])
                      
                      ;(new primitive% [color (color 0 255 0)]
                      ;     [mesh (new triangle% [P1 '(6 0 10)][P2 '(6 6 -10)][P3 '(6 -6 -10)]  )])
                      ;(new primitive% [color (color 0 0 255)]
                      ;     [mesh (new triangle% [P1 '(16 0 -10)][P2 '(16 6 10)][P3 '(16 -6 10)]  )])
                      )]
       [I-ambi 0.1]
       [I-d 0.9]
       [global-light '(1 -1 -1)]))

;(define Scene0
;  (new Scene%
;       [objects '()]
;       [I-ambi 0.5]))

(define default-cam
  (new camera%
       [from '(0 0 0)]
       [to '(10 0 0)]
       [temp_y '(0 1 0)]
       [image-wid 1200]
       [image-hgt 900]))
