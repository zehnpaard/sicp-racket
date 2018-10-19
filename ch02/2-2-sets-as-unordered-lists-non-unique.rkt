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
               (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))
; Running time upperbound of n * m
; where n, m are the lengths of the first and second inputs
; Previously also n * m, but n and m were upperbounded by # of unique elements

(define union-set append)
; Linear time operation in the length of the first input
; Previously running time upperbound of n * m
; where n is the length of the first input (which contains only unique elements)
; and m is the length of the second input (which contains only unique elements)

; Trade-offs
; Not deduping the list has positive impact on adjoin-set
; - adjoin-set: linear -> constant
; Potentially positive impact on union-set
; - union-set: n * m -> n'
; Negative on element-of-set? and intersection-set
; - element-of-set?: n -> n'
; - intersection-set: n * m -> n' * m'

; Whether or not union-set is better deduped depends on the number of overlapping
; elements - if the number is very high, it is possible that deduped n*m is better than
; undeduped n'.

; Also adjoin-set and union-set can be considered adding data,
; elements-of-set? and intersection-set can be considered querying data
; Adding data is (likely) cheaper without deduping, query with deduping
; If the use-case requires storing a lot of new data and infrequently querying it, then
; not deduping can make sense

; In summary, though the relationships are somewhat more subtle, broadly speaking
; raw (rare) <- duplication in data -> frequent) dedupe
; raw (store) <- balance of actions on data -> (query) dedupe