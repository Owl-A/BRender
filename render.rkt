#lang racket

(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt" racket/draw)

;(define (render Scene cam)
;  )

(define camera%
  (class object%
    [init-field from]
    [init-field to]
    [init temp_y]
    [init-field image-wid]
    [init-field image-hgt] ; width:height
    [define aspect (/ image-wid image-hgt)]
    ; (0,0) for the canvas, wise men said keep it at top left

    (define ctr-x 0)
    (define ctr-y 0)
    [field
       [temp (subs to from)]
       [leng2 (len2 temp)]
       [z_ (normalise leng2 temp)]
       [y_ (make-orthonormal-to z_ temp_y)]
       [x_ (cross y_ z_)]]
    (define ret-x (scale (- 0 aspect) x_))
    (define ret-y (scale -1 y_))
    (define del-x (scale (/ aspect image-hgt) x_)); delta-x
    (define del-y (scale (/ -1 image-hgt) y_)); delta-y
    (field [base (add (add (add z_ (add (scale 0.5 y_) (scale (* -0.5 aspect) x_)))
                      (scale 0.5 del-x)) (scale 0.5 del-y))])
    [super-new]
    (define/public (get-next-ray)
      (begin
        ;return ray :- to be added
        (cond ((< ctr-x image-wid) (begin
                                     (get-field base this)
                                     (set! ctr-x (+ ctr-x 1))
                                     (set! base (add base del-x))))
              ((< ctr-y image-hgt) (begin (set! ctr-x 0)
                                          (set! ctr-y (+ ctr-y 1))
                                          (set! base (add base (add ret-x del-y)))
                                          base
                                          ))
              (else (begin (set! ctr-x 0)
                           (set! ctr-y 0)
                           (set! base (add base (add ret-x ret-y)))
                           'done)))));keep track of position
    ))

;; TODO : error the center of the rays is not 1,0,0 BALANCE IT!