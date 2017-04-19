#lang racket

(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt" "shaders.rkt" "render.rkt")
; examples

(define Scene1
  (new Scene%
       [objects (list (new primitive% [color (color 255 0 0)]
                           [mesh (new smooth-sphere% [center '(10 0 0)] [radius 5])])
                      (new primitive% [color (color 0 255 0)]
                           [mesh (new triangle% [P1 '(6 0 10)][P2 '(6 6 -10)][P3 '(6 -6 -10)]  )])
                      (new primitive% [color (color 0 0 255)]
                           [mesh (new triangle% [P1 '(16 0 -10)][P2 '(16 6 10)][P3 '(16 -6 10)]  )])
                      )]
       [I-ambi 0.5]))

(define Scene0
  (new Scene%
       [objects '()]
       [I-ambi 0.5]))

(define default-cam
  (new camera%
       [from '(1 1 1)]
       [to '(10 2 -1)]
       [temp_y '(0 1 0)]
       [image-wid 400]
       [image-hgt 300]))
