#lang racket

(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt")

;(define (render Scene cam)
;  )

(define camera%
  (class object%
    [init-field from]
    [init-field to]
    [init temp_y]
    [field
       [temp (subs to from)]
       [leng2 (len2 temp)]
       [z_ (normalise leng2 temp)]
       [y_ (make-orthonormal-to z_ temp_y)]
       [x_ (cross y_ z_)]]
    [super-new]
    ))