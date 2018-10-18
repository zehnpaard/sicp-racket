#lang racket
;2.60
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define adjoin-set cons)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) (set2))))
        (else
         (intersection-set (cdr set1) (set2)))))

(define union-set append)

;TODO discuss running time and trade-offs