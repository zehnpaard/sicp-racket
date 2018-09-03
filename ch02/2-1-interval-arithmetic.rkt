#lang racket

;2.7
(define (make-interval a b)
  (cons a b))
(define (upper-bound i)
  (max (car i) (cdr i)))
(define (lower-bound i)
  (min (car i) (cdr i)))
;alternatively if the contract is explicitly that a <= b
(define (upper-bound* i)
  (cdr i))
(define (lower-bound* i)
  (car i))

;2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

;2.9
(define (width i)
  (- (upper-bound i)
     (lower-bound i)))

;(width (add-interval x y))
;(width (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y))))
;(- (+ (upper-bound x) (upper-bound y)) (+ (lower-bound x) (lower-bound y)))
;(- (+ a b) (+ c d)) -> (+ (- a c) (- b d))
;(+ (- (upper-bound x) (lower-bound-x)) (- (upper-bound y) (lower-bound y)))
;(+ (width x) (width y))

;(width (sub-interval x y))
;(width (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y))))
;(- (- (upper-bound x) (lower-bound y)) (- (lower-bound x) (upper-bound y)))
;(- (- a b) (- c d)) -> (+ (- (- a b) c) d) -> (+ (- (- a c) b) d) -> (- (- a c) (- b d))
;(- (- (upper-bound x) (lower-bound x)) (- (lower-bound y) (upper-bound y)))
;(+ (- (upper-bound x) (lower-bound x)) (- (upper-bound y) (lower-bound y)))
;(+ (width x) (width y))


;2.10
(define (div-interval x y)
  (if (and (>= 0 (lower-bound y))
           (<= 0 (upper-bound y)))
    "Dividing by interval spanning 0"
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

;2.11
(define (mul-interval x y)
  (let ((xu (upper-bound x))
        (xl (lower-bound x))
        (yu (upper-bound y))
        (yl (lower-bound y)))
    (cond
      ((< xu 0)
       (cond
         ((< yu 0)
          (make-interval (* xu yu) (* xl yl)))
         ((< yl 0)
          (make-interval (* xl yu) (* xl yl)))
         (else
          (make-interval (* xl yu) (* xu yl)))))
      ((< xl 0)
       (cond
         ((< yu 0)
          (make-interval (* xu yl) (* xl yl)))
         ((< yl 0)
          (make-interval (min (* xl yu) (* xu yl))
                         (max (* xl yl) (* xu yu))))
         (else
          (make-interval (* xl yu) (* xu yu)))))
      (else
       (cond
         ((< yu 0)
          (make-interval (* xu yl) (* xl yu)))
         ((< yl 0)
          (make-interval (* xu yl) (* xu yu)))
         (else
          (make-interval (* xl yl) (* xu yu))))))))

;2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))


(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w)))
(define (percent i)
  (* 100 (/ (width i) (center i))))

;2.13
; Assuming intervals a, b spanning positive numbers with proportional widths x, y
; Multiplied interval c has:
;  lower bound: (lower-bound a) * (lower-bound b)
;               (a * (1 - x)) * (b * (1 - y))
;               (a * b * (1 - x) * (1 - y))
;               (a * b * (1 - (x + y) + (x * y)))
;  upper bound: (upper-bound a) * (upper-bound b)
;               (a * (1 + x)) * (b * (1 + y))
;               (a * b * (1 + x) * (1 + y))
;               (a * b * (1 + (x + y) + (x * y)))
; Assuming that x and y are small, x * y is even smaller and can be ignored in an approximation
; (a * b * (1 - (x + y))) ~ (a * b * (1 + (x + y)))
; Proportional width is approximately x + y, around approximate center of a * b

;2.14
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define interval1
  (make-center-percent 10 2))
(define interval2
  (make-center-percent 15 3))
;> (par1 interval1 interval2)
;'(5.559064327485381 . 6.471868583162218)
;> (par2 interval1 interval2)
;'(5.855852156057495 . 6.143859649122806)

;2.15
; Eva Lu Ator is correct
; Repeating a variable in the formula always leads to an increase in the width of the resulting interval
; even when the repetition is arithmetically equivalent
; As such, par2 (with the variables occurring just once) leads to a tighter bound than par1
; The tighter bound given by par1 is  correct, as the bounds for par1 assume that the variables can assume
; the value of different bounds depending on its position in the formula, which is not valid

;2.16
; As explained in 2.15, a variable occurring repeatedly in cancellable positions changes the position of the bounds
; because evaluating arithmetic expressions locally means that the evaluation allows for the same variable to take
; different sides of the upper/lower bounds at different parts of the formula
; If the evaluation strategy is to perform atomic expression evaluations locally and then aggregate up, then this is
; inevitable, and the possibility of differing results for equivalent arithmetic expressions can not be eliminated
; Some kind of globally context-aware evaluation can solve this
; One method is to evaluate the whole expression, substituting each variable with either the upper bound or the
; lower bound.  This results in 2^(# of variables) possible results.  The lower bound is the min, the upper bound is
; the max of these possible results.
; This solution is inefficient, as the number of calculations required grows exponentially with the number of variables.
; Another method is to use symbollic manipulation to convert equivalent arithmetic expressions to a single identical
; canonical representation.