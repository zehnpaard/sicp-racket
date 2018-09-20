#lang racket
(require sicp-pict)

(define (ptwo p)
  (beside p (flip-vert p)))

(define (pfour p)
  (below (ptwo p) (ptwo p)))

