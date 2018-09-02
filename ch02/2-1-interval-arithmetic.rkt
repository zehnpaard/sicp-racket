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