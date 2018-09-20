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
