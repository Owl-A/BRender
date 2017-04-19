#lang racket
(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt")
(provide Scene% depth tracer)

(define Scene%
  (class object%
    (super-new)
    [init-field objects I-ambi]))

(define depth 5) ; number of permitted hits of a ray

(define (tracer ray1 Scene) ; ambient shader at this point 
  (define I-am (get-field I-ambi Scene))
  [let ((process (check-hit 'no ray1 (get-field objects Scene))))
           (if (eq? process 'no) (color 0 0 0)
        (multC I-am (material-color (send (car process) state))) ;;; bugfixes needed 
        )])

(define (check-hit stat ray1 obj)
  (define orig (ray-origin ray1))
  (define (closer a b)
           (cond ((eq? (cdr b) #f) a)
                 ((eq? a 'no) b)
                 ((> (len2 (subs (cadr a) orig)) (len2 (subs (cadr a) orig))) b)
                 (else a)))
  (if (null? obj) stat
        [let((pt (send (car obj) intersect? ray1)))
          (check-hit (closer stat (cons (car obj) pt)) ray1 (cdr obj))] ; Update
        ))