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
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

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
