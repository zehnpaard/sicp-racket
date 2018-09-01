#lang racket
;2.1
(define (gcd a b)
  (if (= 0 b)
    (abs a)
    (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (m ((if (negative? d) -1 1))))
    (cons (* (/ n g) m)
          (* (/ d g) m))))

;2.2
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (midpoint-segment line)
  (let ((s (start-segment line))
        (e (end-segment line)))
    (let ((sx (x-point s))
          (sy (y-point s))
          (ex (x-point e))
          (ey (y-point e)))
      (cons (/ (+ sx ex) 2)
            (/ (+ sy ey) 2)))))

;2.3
;make-rectangle takes two segments with the following properties
; 1) the segments start at the same point
; 2) the segments are perpendicular to each other

(define make-rectangle cons)
(define side1-rectangle car)
(define side2-rectangle cdr)

;alternative representation
(define (make-rectangle* side1 side2)
  (list (start-segment side1)
        (end-segment side1)
        (end-segment side2)))
(define (side1-rectangle* r)
  (cons (car r) (cadr r)))
(define (side2-rectangle* r)
  (cons (car r) (caddr r)))

(define (perimeter r)
  (+ (* 2 (length (side1-rectangle r)))
     (* 2 (length (side2-rectangle r)))))
(define (area r)
  (* (length (side1-rectangle r))
     (length (side2-rectangle r))))
(define (length line)
  (let ((s (start-segment line))
        (e (end-segment line)))
    (let ((dx (- (x-point s)
                 (x-point e)))
          (dy (- (y-point s)
                 (y-point e))))
      (sqrt (+ (* dx dx) (* dy dy))))))
