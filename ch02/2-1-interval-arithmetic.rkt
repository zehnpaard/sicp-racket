#lang racket

;2.7
(define (make-interval a b)
  (cons a b))
(define (upper-bound i)
  (max (car i) (cdr i)))
(define (lower-bound i)
  (min (car i) (cdr i)))
;alternatively if the contract is explicitly that a <= b
(define (upper-bound* i)
  (cdr i))
(define (lower-bound* i)
  (car i))

;2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
