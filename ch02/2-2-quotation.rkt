#lang racket

; 2.53
(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)

; 2.54
(define (equal? x y)
  (cond
    ((and (symbol? x) (symbol? y))
     (eq? x y))
    ((and (number? x) (number? y))
     (= x y))
    ((and (list? x) (list? y))
     (and (equal? (car x) (car y))
          (equal? (cdr x) (cdr y))))
    (else
     #f)))

; 2.55
; why is (car ''abc) -> 'quote ?
; 'abc is short-hand for (quote abc)
; ''abc is short-hand for (quote (quote abc)) = '(quote abc)
; (car '(x y)) -> 'x
; (car '(quote abc)) -> 'quote
