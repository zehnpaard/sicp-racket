#lang racket

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;2.61
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((y (car set)))
        (cond ((= x y)
               set)
              ((< y x)
               (cons y
                (adjoin-set x (cdr set))))
              ((< x y)
               (cons x set))))))
