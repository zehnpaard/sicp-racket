#lang racket

;2.17
(define (last-pair xs)
  (if (= 1 (length xs))
    xs
    (last-pair (cdr xs))))

;2.18
(define (reverse xs)
  (define (reverse-acc xs ys)
    (if (null? xs)
      ys
      (reverse-acc (cdr xs) (cons (car xs) ys))))
  (reverse-acc xs '()))

;2.19
(define (cc n coins)
  (cond
    ((= n 0) 1)
    ((or (< n 0) (null? coins)) 0)
    (else
     (+ (cc n (cdr coins))
        (cc (- n (car coins)) coins)))))

;2.20
(define (filter p xs)
  (cond
    ((null? xs)
     xs)
    ((p (car xs))
     (cons (car xs) (filter p (cdr xs))))
    (else
     (filter p (cdr xs)))))

(define (same-parity? x . xs)
  (define (p y)
    (= (modulo y 2) (modulo x 2)))
  (cons x (filter p xs)))