#lang racket
(require "matrixLib.rkt")
(provide add)
(provide subs)
(provide neg)
(provide scale)
(provide len2)
(provide normalise)
(provide cross)

;;; 3D vector library

; taking a list to be a vector
;define vector operations
;assume that the vectors have same length

(define (merge-help acc opT op l1 l2)   ; Tail recursive version
    (if (null? l1) acc
        (merge-help (opT (op (car l1) (car l2)) acc) opT op (cdr l1) (cdr l2))))
;; list comprehension can also be used 

;;;function of merge-help fair and square:
;;;  starts from head of both lists and performs op on first elements of the lists
;;;  and successively perform (opT result acc)


(define (add l1 l2)
        (reverse (merge-help '() cons + l1 l2)))


;; Abstractions used:-
;;; reverse treated as black box
;;; list being treated as a vector
;;; merge-help 

(define (subs l1 l2)
      (reverse (merge-help '() cons - l1 l2)))

(define (neg l)
  (subs (make-list (length l) 0) l))

;; negetive of a vector is defined in this way even though invoking length procedure
;; means iterating over the list twice however it allows us to use SUBS as an ABSTRACTION

(define (scale k l)
  (foldr (lambda (x y) (cons (* k x) y)) '() l))

;; THIS IS FOLDR (not sparta)!!!!


(define (len2 l)
  (dot l l))

(define (normalise len2l l) ; using to avoid recalculation of length
  (scale (/ 1 (sqrt len2l)) l))

(define (cross v1 v2)
  [let* [(v1c (cdr v1))
         (v2c (cdr v2))
         (x1 (car v1)) (y1 (car v1c)) (z1 (cadr v1c))
         (x2 (car v2)) (y2 (car v2c)) (z2 (cadr v2c))]
    (list
     (- (* y1 z2) (* z1 y2))
     (- (* z1 x2) (* z2 x1))
     (- (* x1 y2) (* x2 y1))
          )])