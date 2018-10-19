#lang racket
;2.60
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
; Worst-case running time is linear in length of "set"
; (making the simplifying assumption that equal? is a constant time operation)
; Previously the length was upperbounded by # of unique values encountered

(define adjoin-set cons)
; Constant time operation
; Previously linear-in-number-of-unique-elements due to use of element-of-set?

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) (set2))))
        (else
         (intersection-set (cdr set1) (set2)))))
; Running time upperbound of n * m
; where n, m are the lengths of the first and second inputs
; Previously also n * m, but n and m were upperbounded by # of unique elements

(define union-set append)
; Linear time operation in the length of the first input
; Previously running time upperbound of n * m
; where n is the length of the first input (which contains only unique elements)
; and m is the length of the second input (which contains only unique elements)

;TODO discuss trade-offs