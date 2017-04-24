#lang racket
(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt")
(provide Scene% depth tracer)
(define background-color (color 0 0 0))
(define samples 30)

(define light%
  (class smooth-sphere%
    (super-new)
    [init-field [lightIn '(1 1 1)]]
    [init-field (sampler (new sampler% [c (get-field center this)] [r (get-field radius this)]))]))

(define sampler%
  (class object%
    (super-new)
    [init c r]
    [init-field [sampling-pts (repeat samples
                 (lambda (acc) (cons (add (scale (* r (random)) (randdir)) c) acc)) '())]]
    (define/public (final-dir pt)
      (foldr (lambda (x y) (subs y x)) (scale samples pt) sampling-pts))
    ))

(define (repeat n func acc) 
  [if (= 0 n)  acc
      (repeat (- n 1) func (func acc))])

(define Scene%
  (class object%
    (super-new)
    [init-field objects]
    [init global-light]
    [field [vec-GlobalLight (normalise (len2 global-light) global-light)]]
    [init [lamp-center '(20 20 20)] [lamp-radius 5]]
    [field [lamp (new light% [center lamp-center] [radius lamp-radius])]]
    (define/public (getlight p) (send (get-field sampler lamp) final-dir p))))

(define depth 5) ; number of permitted hits of a ray

(define (tracer ray1 Scene) ; ambient shader at this point 
  [let ((process (check-hit 'no ray1 (get-field objects Scene))))
           (cond ((eq? process 'no) background-color)
                 (else [let* [[col (material-color (send (car process) state))]
                              [templight (send Scene getlight (cadr process))]
                              [light (normalise (len2 templight) templight)]
                              [shadow-ray (make-ray
                                           (subs (cadr process) (scale bias (ray-direction ray1)))
                                           (neg light))]
                              [process1 (check-hit 'no shadow-ray (get-field objects Scene))]
                              [state (send (car process) state)]
                              [I-am (material-ambient state)]
                              [I-d (material-diffuse state)]]
                         (multC (+ I-am  (* I-d                   ;;;;;;;;;;;;;;;;;; # todo make get light function ;;;;;;;;;;;;;;;;
                                            (max (dot (neg light) (caddr process)) 0))) col)]))])

(define (check-hit stat ray1 obj)
  (define orig (ray-origin ray1))
  (define (closer a b)
                  (cond ((eq? (cdr b) #f) a)
                        ((eq? a 'no) b)
                        ((> (len2 (subs (cadr a) orig)) (len2 (subs (cadr b) orig))) b)
                        (else a)))
  (if (null? obj) stat
        [let((pt (send (car obj) intersect? ray1)))
          (check-hit (closer stat (cons (car obj) pt)) ray1 (cdr obj))] ; Update
        ))
