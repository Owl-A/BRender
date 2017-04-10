#lang racket
(provide get-col0 transpose add-mat dot scale-mat subs-mat prod-mat rot-mat2)
(define (identity n)
  [let [(n1 (- n 1))]
    (if (= n 1) '((1))
        (cons
         (cons 1 (make-list n1 0))
          (map (lambda (row) (cons 0 row))(identity n1))))])
(define I2 (identity 2))
(define I3 (identity 3))
(define I4 (identity 4))
;; matrix defined as list of lists
(define (get-col0 mat)
  (foldr
   (lambda (x y) (cons (cons (car x) (car y)) (cons (cdr x) (cdr y))))
   '(())
   mat))

(define (transpose mat )
  (define (helper mat tra)
    (if (null? (car mat))
        (reverse tra)
        [let* [(mat2 (get-col0 mat))]
          (helper (cdr mat2) (cons (car mat2) tra))]))
  (helper mat '()))

(define (zip f l1 l2)
  (if (null? l1) '()
      (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))))

;zip used as an abstraction for add and subs
(define (add-mat mat1 mat2)
  (zip
   (lambda (a b)
     (zip + a b))
       mat1 mat2))

(define (subs-mat mat1 mat2)
  (zip
   (lambda (a b)
     (zip - a b))
       mat1 mat2))

;scale uses map abstraction
(define (scale-mat k mat)
  (map (lambda (row) (map (lambda (t) (* k t)) row)) mat))


(define (dot l1 l2 )
  (if (null? l1) 0
      (+ (* (car l1) (car l2)) (dot (cdr l1) (cdr l2)))))

(define (prod-mat mat1 mat2)
  (define tra2 (transpose mat2))
  (map (lambda (v1)(map (lambda (v2) (dot v1 v2)) tra2)) mat1))

(define (rot-mat2 theta)
  [let* [(cosx (cos theta))
         (sinx (sin theta))]
    (list
     (list cosx (- 0 sinx))
     (list sinx cosx))])
