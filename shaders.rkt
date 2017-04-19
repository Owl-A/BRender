#lang racket
(require "vectorLib.rkt" "matrixLib.rkt" "primitives.rkt")
(provide Scene% depth tracer)
(define background-color (color 0 0 0))

(define Scene%
  (class object%
    (super-new)
    [init-field objects
                I-ambi
                I-d ; temp not the real deal
                ]
    [init global-light]
    [field [vec-GlobalLight (normalise (len2 global-light) global-light)] ]))

(define depth 5) ; number of permitted hits of a ray

(define (tracer ray1 Scene) ; ambient shader at this point 
  (define light (get-field vec-GlobalLight Scene))
  (define I-am (get-field I-ambi Scene))
  (define I-d (get-field I-d Scene))
  [let ((process (check-hit 'no ray1 (get-field objects Scene))))
           (cond ((eq? process 'no) background-color)
        (else [let* [[col (material-color (send (car process) state))]
                     [shadow-ray (make-ray
                                  (subs (cadr process) (scale bias (ray-direction ray1)))
                                  (neg light))]
                     [process1 (check-hit 'no shadow-ray (get-field objects Scene))]]
                (if (eq? process1 'no)
                    (multC (+ I-am  (* I-d (dot (neg light) (caddr process)))) col)
                    (multC I-am col))])
        )])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define background-color (color 0 0 0))
;
;(define Scene%
;  (class object%
;    (super-new)
;    [init-field objects
;                I-ambi
;                I-d ; temp not the real deal
;                ]
;    [init global-light]
;    [field [vec-GlobalLight (normalise (len2 global-light) global-light)] ]))
;
;(define depth 5) ; number of permitted hits of a ray
;
;(define (tracer ray1 Scene) ; ambient shader at this point 
;  (define light (get-field vec-GlobalLight Scene))
;  (define I-am (get-field I-ambi Scene))
;  (define I-d (get-field I-d Scene))
;  [let ((process (check-hit 'no ray1 (get-field objects Scene))))
;           (cond ((eq? process 'no) background-color)
;        (else [let* [[col (material-color (send (car process) state))]
;                     [shadow-ray (make-ray (subs (cadr process) (scale bias (ray-direction ray1))) (neg light))]
;                     [process1 (check-hit 'no shadow-ray (get-field objects Scene))]]
;                (if (eq? process1 'no) (addC (multC I-am col) (multC (dot light (caddr process)) col)) (multC I-am col))])
;        )])
;
;(define (check-hit stat ray1 obj)
;  (define orig (ray-origin ray1))
;  (define (closer a b)
;                  (cond ((eq? (cdr b) #f) a)
;                        ((eq? a 'no) b)
;                        ((> (len2 (subs (cadr a) orig)) (len2 (subs (cadr b) orig))) b)
;                        (else a)))
;  (if (null? obj) stat
;        [let((pt (send (car obj) intersect? ray1)))
;          (check-hit (closer stat (cons (car obj) pt)) ray1 (cdr obj))] ; Update
;        ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;