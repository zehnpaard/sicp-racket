#lang racket
;2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (x y) x)))
(define (cdr z)
  (z (lambda (x y) y)))

;2.5
(define (cons* x y)
  (* (exp 2 x)
     (exp 3 y)))

(define (count-divides n d a)
  (if (not (zero? (remainder n d)))
    a
    (count-divides (/ n d) d (+ a 1))))
(define (car* z)
  (count-divides z 2 0))
(define (cdr* z)
  (count-divides z 3 0))

;2.6
(define zero
  (lambda (f)
    (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; (add-1 (lambda (f) (lambda (x) x)))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))
(define one
  (lambda (f)
    (lambda (x) (f x))))

; (add-1 one)
; (add-1 (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))