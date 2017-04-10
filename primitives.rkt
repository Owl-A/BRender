#lang racket
(require "vectorLib.rkt")
(require "matrixLib.rkt")
(provide material color ray make-ray smooth-sphere%)

(define epsilon (expt 10 -9))(define -epsilon (- 0 epsilon))(define 1epsilon (+ 1 epsilon))

(define bias (expt 10 -4))

(struct ray (origin direction) #:transparent )

(define (make-ray origin direction)
    (ray origin (normalise (len2 direction ) direction )))

(struct material (reflect refract absorb color emit-color) #:transparent)          ;;add to this with increase in fields in primitive

(struct color (red green blue) #:transparent )

;defines an object with the properties and shape defined by a mesh object
(define primitive%
  (class object%
    (super-new)
    (init-field mesh
                Kre
                Krf
                Kab
                color
                emit-color
                ;more to come
                )
    (define/public (intersect? ray) (send mesh intersect? ray))
    (define/public (state) (material Kre Krf Kab color emit-color))))

(define mesh%
  (class object%
    (super-new)
    (abstract intersect?)
    (abstract normal?)))

;defines only the wireframe geometry
(define smooth-sphere%
  (class mesh%
    (super-new)
    (init-field center
                radius)
    (define (intersect? ray)
      [let*[(radius2 (* radius radius))
            (orig (ray-origin ray))
            (sep (subs center orig))
            (dir (ray-direction ray))
            (comp (dot sep dir))
            (sep2 (dot sep sep))
            (comp2 (* comp comp))
            (norm2 (- sep2 comp2))
            (offset2 (- radius2 norm2))
            (offset (if (>= offset2 0) (sqrt offset2) #f))]
        (if offset [let* [(P1 (scale (- comp offset) dir))
                          (P2 (scale (+ comp offset) dir))]
                     (list (list P1 P2)
                         (list (normal? P1) (normal? P2)))] #f)
        ])
    (define (normal? p)
      [let* [(normal (subs p center))
             (dist2 (len2 normal))]
        (normalise dist2 normal)])
    (override normal?)
    (override intersect?)
      ))

;intersect function uses Moller-Trumbore method
(define triangle%
  (class mesh%
    (super-new)
    (init-field P1
                P2
                P3)
    (field [s1 (subs P2 P1)]
           [s2 (subs P3 P1)]
           [normal
            [let* [(te-norm (cross s1 s2))
                          (norm-fac (len2 te-norm))]
                     (normalise norm-fac te-norm)]])
    (define (intersect? ray)
      [let* [(orig (ray-origin ray))
             (dir (ray-direction ray))
             (T (subs orig P1))
             [P (cross dir s2)]
             (P.s1 (dot P s1))]
        (if (and (< P.s1 epsilon)(> P.s1 -epsilon)) #f
            [let* [
              [Q (cross T s1)]
              [P.T (dot P T)]
              [Q.D (dot Q dir)]
              [u (/ P.T P.s1)]
              [v (/ Q.D P.s1)]]
            (if (and (>=  u -epsilon) (>= v -epsilon) (<= (+ u v) 1epsilon))
                (list (list (add P1  (add (scale u s1) (scale v s2))))
                      (list normal))
                #f)]
            )])
    (define (normal?) normal)
    (override intersect?)
    (override normal?)))

;for my convenience only
;reflection of point 2 (P2) is taken in line P1 P3
(define parrallelogram%
  (class mesh%
    (super-new)
    (init-field P1
                P2
                P3)
    (field [P4 (subs (add P1 P3) P2)]
           [T1 (new triangle% [P1 P1] [P2 P2] [P3 P3])]
           [T2 (new triangle% [P1 P1] [P2 P4] [P3 P3])])
    (define (intersect? ray)
      [let[(p1 (send T1 intersect? ray))]
        (if p1 p1 (send T2 intersect? ray))] )
    (define (normal?) (send T1 normal?))
    (override normal?)
    (override intersect?)))