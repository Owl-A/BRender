#lang racket
(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt")
(provide Scene% depth tracer check-hit)
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
    (define/public (final-dir pt Scene)
      (foldr (lambda (x y) ;(subs y x))
               [let* [[dir (subs pt x)]
                     [doubt (check-hit 'no (make-ray x dir)
                                     (get-field objects Scene))]]
                 (if (or (eq? doubt 'no) (>= (len2 (subs (cadr doubt) x)) (len2 dir)))
                     (cons (add (car y) dir) (+ (cdr y) 1)) y)])
             (cons (list 0 0 0) 0) sampling-pts))
    ))

(define (repeat n func acc) 
  [if (= 0 n)  acc
      (repeat (- n 1) func (func acc))])

(define Scene%
  (class object%
    (super-new)
    [init-field objects]
    [init [lamp-center '(20 20 20)] [lamp-radius 5]]
    [field [lamp (new light% [center lamp-center] [radius lamp-radius])]]
    (define/public (getlight p) (send (get-field sampler lamp) final-dir p this))))

(define depth 5) ; number of permitted hits of a ray

(define (tracer ray1 Scene) ; ambient shader at this point 
  [let ((process (check-hit 'no ray1 (get-field objects Scene))))
           (cond ((eq? process 'no) background-color)
                 (else (shader-occlusion Scene process)))])

(define (shader-occlusion Scene process)
  [let* [[col (material-color (send (car process) state))]
         [templight (send Scene getlight (add (cadr process) (scale bias (caddr process))))]
         [light (normalise (len2 (car templight)) (car templight))]
         [intensity (/ (cdr templight) samples)]
         [state (send (car process) state)]
         [I-am (material-ambient state)]
         [I-d (material-diffuse state)]
         [InF (+ I-am  (* I-d intensity (max (dot (neg light) (caddr process)) 0)))]]
    (multC  InF col)])

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
