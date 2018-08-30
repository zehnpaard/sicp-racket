#lang racket
(define accumulate foldr)

(define (map f seq)
  (accumulate (lambda (x y) (cons (f x) y)) '() seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;2.34
(define (horner-eval x a-seq)
  (accumulate (lambda (a hts) (+ a (* x hts)))
              0
              a-seq))

;2.35
(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x)
                     (if (not (pair? x))
                         1
                         (count-leaves x)))
                   t)))

