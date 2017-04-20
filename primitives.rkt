#lang racket
(require "vectorLib.rkt")
(require "matrixLib.rkt")
(provide smooth-triangle% primitive% parrallelogram% triangle% material color ray make-ray smooth-sphere% color-red color-green color-blue material-color multC ray-origin addC ray-direction)

(struct ray (origin direction) #:transparent )

(define (make-ray origin direction)
    (ray origin (normalise (len2 direction ) direction )))

(struct material ( color) #:transparent)   ;;add to this with increase in fields in primitive

(struct color (red green blue) #:transparent )

(define (multC sca col) ;; bugfix list->bytes works only on integers fix it
(color (exact-round (* sca (color-red col)))
       (exact-round (* sca (color-green col)))
       (exact-round (* sca (color-blue col)))))

(define (addC col1 col2) ;; bugfix list->bytes works only on integers fix it
(color (+ (color-red col1) (color-red col2))
       (+ (color-green col1) (color-green col2))
       (+ (color-blue col1) (color-blue col2))))

;defines an object with the properties and shape defined by a mesh object
(define primitive%
  (class object%
    (super-new)
    (init-field mesh
                color
		;more to come
                )
    (define/public (intersect? ray) (send mesh intersect? ray))
    (define/public (state) (material color ))))

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
                          (P2 (scale (+ comp offset) dir))
                          (P1.dir (dot P1 dir))
                          (P2.dir (dot P2 dir))
                          (tP1 (add P1 orig))
                          (tP2 (add P2 orig))]
                     (cond ((> P1.dir 0) (list tP1 (normal? tP1)))
                           ((> P2.dir 0) (list tP2 (normal? tP2)))
                           (else #f))] #f)
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
    (init-field P1 P2 P3)
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
                [let [(int (add P1  (add (scale u s1) (scale v s2))))]
                      (if (> (dot (subs int orig) dir) 0)
                          (list int (normal? u v dir)) #f)]
                #f)]
            )])
    (define (normal? u v dir)
      (if (> (dot normal dir) 0) (neg normal) normal))
    (override intersect?)
    (override normal?)))

; smooth shading using linear interpolation
(define smooth-triangle% 
  (class triangle%
    (super-new)
    (init-field nP1 nP2 nP3)
    (define (normal? u v dir)
      [let* ((te (add (add (scale (- 1 (+ u v)) nP1) (scale u nP2)) (scale v nP3)))
             (temp (normalise (len2 te) te)))
        (if (> (dot temp dir ) 0) (neg temp) temp)])
    (override normal?)
    ))

;for my convenience only
;reflection of point 2 (P2) is taken in line P1 P3

(define parrallelogram%
  (class mesh%
    (super-new)
    (init-field P1 P2 P3)
    (field [P4 (subs (add P1 P3) P2)]
           [T1 (new triangle% [P1 P1] [P2 P2] [P3 P3])]
           [T2 (new triangle% [P1 P1] [P2 P4] [P3 P3])])
    (define (intersect? ray)
      [let[(p1 (send T1 intersect? ray))]
        (if p1 p1 (send T2 intersect? ray))] )
    (define (normal?) 'invalid)
    (override intersect?)
    (override normal?)))
