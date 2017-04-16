#lang racket

(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt" racket/draw)
(struct color (red green blue) #:transparent )

(define (render Scene lights cam shader)
  (define Scr (send cam init-screen))
  (define (ren-loop)
    [let[[new-ray (send cam get-next-ray)]]
      (if (= new-ray 'done) (send Scr save-buffer)
          (begin  ; code to render 1 single pixel
            (ren-loop)))])
  (ren-loop))

(define Screen%
  (class object%
    (init l b)
    (define buffer (make-object bitmap% l b))
    (super-new)
    (define/public (paint x y Clr)
      (send buffer set-argb-pixels x   ; x
            y              ; y
            1              ; width
            1              ; height
            (list->bytes 
             (list 0                              ; alpha
                   (color-red Clr)                ; red
                   (color-green Clr)              ; green 
                   (color-blue Clr))))            ; blue
      )
    (define/public (save-buffer)
      (send buffer save-file "image.png" 'png))))

(define camera%
  (class object%
    [init-field from]
    [init-field to]
    [init temp_y]
    [init-field image-wid]
    [init-field image-hgt] ; width:height
    [define aspect (/ image-wid image-hgt)]
    ; (0,0) for the canvas, wise men said keep it at top left

    (define ctr-x -1)
    (define ctr-y 0)
    [field
       [temp (subs to from)]
       [leng2 (len2 temp)]
       [z_ (normalise leng2 temp)]
       [y_ (make-orthonormal-to z_ temp_y)]
       [x_ (cross y_ z_)]]
    (define del-x (scale (/ aspect image-wid) x_)); delta-x
    (define del-y (scale (/ -1 image-hgt) y_)); delta-y     
    (define ret-x (add (scale (- 0 aspect) x_) del-x))
    (define ret-y (add y_ del-y) )
    (define img-hgt-1 (- image-hgt 1))
    (define img-wid-1 (- image-wid 1))
    (field [base  (add (subs z_ (add (scale (* 0.5 aspect) x_ )(scale 0.5 del-x)))
                              (scale 0.5 (add y_ del-y)))])
    [super-new]
    (define/public (get-next-ray)  ; getting the next ray
      (begin
        (cond ((< ctr-x img-wid-1) (begin
                                     (set! ctr-x (+ ctr-x 1))
                                     (set! base (add base del-x))
                                     (cons (cons ctr-x ctr-y) (make-ray from (subs base from)))))
              ((< ctr-y img-hgt-1) (begin (set! ctr-x 0)
                                          (set! ctr-y (+ ctr-y 1))
                                          (set! base (add (add base ret-x) del-y))
                                          (cons (cons ctr-x ctr-y) (make-ray from (subs base from)))
                                          ))
              (else (begin (set! ctr-x -1)
                           (set! ctr-y 0)
                           (set! base (add base (add
                                                 (subs ret-x del-x) ret-y)))
                           'done)))));keep track of position
    (define/public (init-screen) (new Screen% [l image-wid] [b image-hgt]))
    )); apparently superfluous state variables are being added as a memory tradeoff for less time complexity

