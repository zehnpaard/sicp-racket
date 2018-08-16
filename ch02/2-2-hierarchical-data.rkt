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

;2.25
(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))
;(car (cdr (car (cdr (cdr a)))))
;(car (car b))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

;2.27
(define (deep-reverse xs)
  (define (deep-reverse-acc xs ys)
    (cond
      ((not (list? xs)) xs)
      ((null? xs) ys)
      (else
       (let ((rev-x (deep-reverse-acc (car xs) '())))
         (deep-reverse-acc (cdr xs) (cons rev-x ys))))))
  (deep-reverse-acc xs '()))

;2.28
(define (fringe xt)
  (define (fringe-acc xt ys)
    (cond
      ((not (list? xt)) (cons xt ys))
      ((null? xt) ys)
      (else
       (fringe-acc (cdr xt) (fringe-acc (car xt) ys)))))
  (reverse (fringe-acc xt '())))