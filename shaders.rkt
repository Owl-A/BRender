#lang racket
(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt")
(provide Scene% depth tracer)

(define Scene%
  (class object%
    (super-new)
    [init-field objects
                I-ambient]))

(define depth 5) ; number of permitted hits of a ray

(define (tracer ray Scene) ; null shader at this point 
  (color 255 0 0)
  ;(check-hit 'no ray (get-field Scene objects))
  )

;(define (check-hit stat ray1 obj)
;  (define (closest li pt)
;    )
;  (cond
;      ((null? obj) stat)
;      [let((pt (send (car obj) intersect? ray)))
;            (pt )]))