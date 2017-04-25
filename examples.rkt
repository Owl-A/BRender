#lang racket
;; Do not consider this as code this is just for demonstration purposes ;;

(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt" "shaders.rkt" "render.rkt")
; examples

(define Scene1
  (new Scene%
       [objects (list (new primitive% [color (color 255 0 0)]
                           [mesh (new smooth-sphere% [center '(15 0 0)] [radius 5])])
                      (new primitive% [color (color 0 255 0)] 
                           [mesh (new parrallelogram% [P1 '(35 -7 20)][P2 '(35 -7 -20)][P3 '(10 -7 -20)]  )])
                     )]
       [mode 'global]
       [global '(1 -1 -1)]
       [lamp-center '(0 10 10)]
       [lamp-radius 5]))

(define Scene2
  (new Scene%
       [objects (list (new primitive% [color (color 0 0 255)]
                           [mesh (new smooth-sphere% [center '(20 0 -5)] [radius 5])])
                      (new primitive% [color (color 0 255 0)] 
                           [mesh (new parrallelogram% [P1 '(35 -7 20)][P2 '(35 -7 -20)][P3 '(10 -7 -20)]  )])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new triangle% [P1 '(15 -9 3)][P2 '(15 0 0)][P3 '(12 -9 0)])])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new triangle% [P1 '(12 -9 0)][P2 '(15 0 0)][P3 '(15 -9 -3)]  )])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new triangle% [P1 '(15 -9 3)][P2 '(15 0 0)][P3 '(18 -9 0)])])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new triangle% [P1 '(18 -9 0)][P2 '(15 0 0)][P3 '(15 -9 -3)]  )])
                    )]
       [mode 'global]
       [global '(1 -1 -1)]
       [lamp-center '(0 10 10)]
       [lamp-radius 5]))

(define Scene3
  (new Scene%
       [objects (list (new primitive% [color (color 0 0 255)]
                           [mesh (new smooth-sphere% [center '(20 0 -5)] [radius 5])])
                      (new primitive% [color (color 0 255 0)] 
                           [mesh (new parrallelogram% [P1 '(35 -7 20)][P2 '(35 -7 -20)][P3 '(10 -7 -20)]  )])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new smooth-triangle% [P1 '(15 -9 3)][P2 '(15 0 0)][P3 '(12 -9 0)]
                                      [nP1 '(0 0 1)][nP2 '(0 1 0)][nP3 '(-1 0 0)])])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new smooth-triangle% [P1 '(12 -9 0)][P2 '(15 0 0)][P3 '(15 -9 -3)]
                                      [nP1 '(-1 0 0)][nP2 '(0 1 0)][nP3 '(0 0 -1)])])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new smooth-triangle% [P1 '(15 -9 3)][P2 '(15 0 0)][P3 '(18 -9 0)]
                                      [nP1 '(0 0 -1)][nP2 '(0 1 0)][nP3 '(1 0 0)])])
                     (new primitive% [color (color 255 0 0)] 
                           [mesh (new smooth-triangle% [P1 '(18 -9 0)][P2 '(15 0 0)][P3 '(15 -9 -3)]
                                      [nP1 '(1 0 0)][nP2 '(0 1 0)][nP3 '(0 0 1)])])
                    )]
       [mode 'distrib]
       [global '(1 -1 -1)]
       [lamp-center '(0 10 10)]
       [lamp-radius 5]))

(define Scene4
  (new Scene%
       [objects (list (new primitive% [color (color 0 0 255)]
                           [mesh (new smooth-sphere% [center '(15 2 7)] [radius 0.5])])
                      (new primitive% [color (color 10 0 245)]
                           [mesh (new smooth-sphere% [center '(15 1 7)] [radius 0.5])])
                      (new primitive% [color (color 20 0 235)]
                           [mesh (new smooth-sphere% [center '(15 0 7)] [radius 0.5])])
                      (new primitive% [color (color 30 0 225)]
                           [mesh (new smooth-sphere% [center '(15 -1 7)] [radius 0.5])])
                      (new primitive% [color (color 40 0 215)]
                           [mesh (new smooth-sphere% [center '(15 -2 7)] [radius 0.5])])
                      (new primitive% [color (color 50 0 205)]
                           [mesh (new smooth-sphere% [center '(15 0 6)] [radius 0.5])])
                      (new primitive% [color (color 60 0 195)]
                           [mesh (new smooth-sphere% [center '(15 0 5)] [radius 0.5])])
                      (new primitive% [color (color 70 0 185)]
                           [mesh (new smooth-sphere% [center '(15 2 4)] [radius 0.5])])
                      (new primitive% [color (color 80 0 175)]
                           [mesh (new smooth-sphere% [center '(15 1 4)] [radius 0.5])])
                      (new primitive% [color (color 90 0 165)]
                           [mesh (new smooth-sphere% [center '(15 0 4)] [radius 0.5])])
                      (new primitive% [color (color 100 0 155)]
                           [mesh (new smooth-sphere% [center '(15 -1 4)] [radius 0.5])])
                      (new primitive% [color (color 110 0 145)]
                           [mesh (new smooth-sphere% [center '(15 -2 4)] [radius 0.5])])
                      (new primitive% [color (color 120 0 135)]
                           [mesh (new smooth-sphere% [center '(15 2 1)] [radius 0.5])])
                      (new primitive% [color (color 130 0 125)]
                           [mesh (new smooth-sphere% [center '(15 1 1)] [radius 0.5])])
                      (new primitive% [color (color 140 0 115)]
                           [mesh (new smooth-sphere% [center '(15 0 1)] [radius 0.5])])
                      (new primitive% [color (color 150 0 105)]
                           [mesh (new smooth-sphere% [center '(15 -1 1)] [radius 0.5])])
                      (new primitive% [color (color 160 0 95)]
                           [mesh (new smooth-sphere% [center '(15 -2 1)] [radius 0.5])])
                      (new primitive% [color (color 170 0 85)]
                           [mesh (new smooth-sphere% [center '(15 2 2)] [radius 0.5])])
                      (new primitive% [color (color 180 0 75)]
                           [mesh (new smooth-sphere% [center '(15 2 0)] [radius 0.5])])
                      (new primitive% [color (color 190 0 65)]
                           [mesh (new smooth-sphere% [center '(15 -2 2)] [radius 0.5])])
                      (new primitive% [color (color 200 0 55)]
                           [mesh (new smooth-sphere% [center '(15 -2 0)] [radius 0.5])])
                      (new primitive% [color (color 0 255 0)]
                           [mesh (new parrallelogram% [P1 '(16 -20 20)][P2 '(16 20 20)][P3 '(16 20 -20)]  )])
                     )]
       [mode 'distrib]
       [global '(1 -1 -1)]
       [lamp-center '(0 10 10)]
       [lamp-radius 5]))

(define default-cam
  (new camera%
       [from '(6 0 4)]
       [to '(15 0 4)]
       [temp_y '(0 1 0)]
       [image-wid 1200]
       [image-hgt 900]))
