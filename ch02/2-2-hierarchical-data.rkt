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

;2.21
(define (square x) (* x x))
(define (square-list1 items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list1 (cdr items)))))
(define (square-list2 items)
  (map square items))

;2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things)) answer))))
  (iter items '()))
; Results in reverse order because the procedure cons'es the first square to answer, then the second square, etc

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer (square (car things))))))
  (iter items '()))
; Final result will not be a list, as nil is the first element of the cons chain, not the last

;2.23
(define (for-each f xs)
  (if (null? xs)
    #t
    (begin
      (f (car xs))
      (for-each f (cdr xs)))))

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

;2.30
(define (square-tree t)
  (cond
    ((null? t) t)
    ((pair? t) (cons (square-tree (car t))
                     (square-tree (cdr t))))
    (else (square t))))

;using map
(define (square-tree-map t)
  (define (square-subtree t)
    (if (pair? t)
      (square-tree-map t)
      (square t)))
  (map square-subtree t))

;2.31
(define (tree-map f t)
  (cond
    ((null? t) t)
    ((pair? t) (cons (tree-map f (car t))
                     (tree-map f (cdr t))))
    (else (f t))))

;using map
(define (tree-map-map f t)
  (define (subtree-map t)
    (if (pair? t)
      (tree-map-map f t)
      (f t)))
  (map subtree-map t))

;2.32
(define (subsets xs)
  (if (null? xs)
    (list xs)
    (let ((rs          (subsets (cdr xs)))
          (cons-car-xs (lambda (s)
                         (cons (car xs) s))))
      (append rs (map cons-car-xs rs)))))

;Proof by Induction
;Base case '() -> '(())
;Given all subsets of (cdr xs), the subsets of xs are either those same subsets or those subsets with (car xs) cons'ed
;(Any arbitrary subset of xs either does or does not include (car xs), and the rest is a subset of (cdr xs))