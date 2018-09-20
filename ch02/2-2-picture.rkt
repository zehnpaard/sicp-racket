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
