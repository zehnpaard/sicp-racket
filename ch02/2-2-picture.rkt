#lang racket
(require sicp-pict)

(define (ptwo p)
  (beside p (flip-vert p)))

(define (pfour p)
  (below (ptwo p) (ptwo p)))

(define (right-split p n)
  (if (zero? n)
    p
    (let ((q (right-split p (- n 1))))
      (beside p (below q q)))))

(define (up-split p n)
  (if (zero? n)
    p
    (let ((q (up-split p (- n 1))))
      (below p (beside q q)))))

(define (corner-split p n)
  (if (zero? n)
    p
    (let ((u (up-split p (- n 1)))
          (r (right-split p (- n 1))))
      (beside (below p (beside u u))
              (below (below r r) (corner-split p (- n 1)))))))

(define (square-limit p n)
  (let ((q (corner-split p n)))
    (let ((h (beside (flip-horiz q) q)))
      (below (flip-vert h) h))))

(define (square-of-four tl tr bl br)
  (lambda (p)
    (below (beside (bl p) (br p))
           (beside (tl p) (tr p)))))

;2.45
(define (split f1 f2)
  (define (s p n)
    (if (zero? n)
      p
      (let ((q (s p (- n 1))))
        (f1 p (f2 q q)))))
  s)

;2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (do-to-coords f)
  (lambda (a b)
    (make-vect (f (xcor-vect a) (xcor-vect b))
               (f (ycor-vect a) (ycor-vect b)))))

(define add-vect (do-to-coords +))
(define sub-vect (do-to-coords -))

(define (scale-vect v n)
  (make-vect (* n (xcor-vect v))
             (* n (ycor-vect v))))

;2.47
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f)) ; (list origin edge1 edge2)
(define (edge2-frame* f)
  (cddr f)) ; (cons origin (cons edge1 edge2))
