#lang racket
(define accumulate foldr)


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;2.34
(define (horner-eval x a-seq)
  (accumulate (lambda (a hts) (+ a (* x hts)))
              0
              a-seq))

;2.35
(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x)
                     (if (not (pair? x))
                         1
                         (count-leaves x)))
                   t)))

(define (count-leaves2 t)
  (accumulate + 0
              (map (lambda (x)
                     (length (flatten x)))
                   t)))

(define (count-leaves3 t)
  (accumulate (lambda (x y) (+ (length x) y)) 0
              (map flatten t)))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define mat '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define mat2 '((1 2 3 4) (4 5 6 6) (6 7 8 9) (1 1 1 1)))
(define mat3 '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))
(define mat4 '((1 0 0) (0 1 0) (0 0 1) (0 0 0)))

(define mat5 '((1 2) (1 0)))

;2.38
(define (fold-left op e seq)
  (define (iter acc seq)
    (if (null? seq)
        acc
        (iter (op acc (car seq)) (cdr seq))))
  (iter e seq))

; (foldr / 1 '(1 2 3))
; 3/2
; (foldl / 1 '(1 2 3))
; 1/6
; (foldr list '() '(1 2 3))
; (1 (2 (3 ())))
; (foldl list '() '(1 2 3))
; (((() 1) 2) 3)

;Either:
; op is commutative s.t (a op b) = (b op a) -> (a op b op e) = (e op a op b)
; or
; op and e form a monoid, s.t. (a op e) = a -> (a op b op e) = (e op a op b)

;2.39
(define fold-right accumulate)
(define (reverse seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))
(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

;2.40
(define (enumerate-interval start end)
  (if (> start end)
    '()
    (cons start (enumerate-interval (+ 1 start) end))))

(define (flatmap f seq)
  (accumulate append '() (map f seq)))

(define (unique-pairs n)
  (define (pairs-starting-with i)
    (map (lambda (j) (list i j))
         (enumerate-interval 1 (- i 1))))
  (flatmap pairs-starting-with (enumerate-interval 1 n)))

;2.41
(define (unique-triplets n)
  (define (triplets-starting-with k)
    (map (lambda (pair) (cons k pair))
         (unique-pairs (- k 1))))
  (flatmap triplets-starting-with (enumerate-interval 1 n)))

(define (sum xs)
  (accumulate + 0 xs))

(define (sum-triplets n)
  (filter (lambda (xs) (= n (sum xs))) (unique-triplets n)))

;2.42
(define (queen board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
       (lambda (positions) (safe? k positions))
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position row col rest)
  (cons row rest))

; slightly cheating (not using k)
(define (safe? k positions)
  (define (check xs)
    (zero?
     (count (lambda (x) (= x (car xs)))
       (cdr xs))))
  (let ((diag1 (map (lambda (n) (+ n (list-ref positions n)))
                    (enumerate-interval 0 (- (length positions) 1))))
        (diag2 (map (lambda (n) (- n (list-ref positions n)))
                    (enumerate-interval 0 (- (length positions) 1)))))
    (and (check positions) (check diag1) (check diag2))))

; not cheating but possibly inefficient
(define (safe?* k positions)
  (define (check k xs)
    (let ((k-col (- (length xs) k)))
      (let ((k-val (list-ref xs k-col)))
        (= 1
         (count (lambda (x) (= x k-val))
           xs)))))
  (let ((diag1 (map (lambda (n) (+ n (list-ref positions n)))
                    (enumerate-interval 0 (- (length positions) 1))))
        (diag2 (map (lambda (n) (- n (list-ref positions n)))
                    (enumerate-interval 0 (- (length positions) 1)))))
    (and (check k positions) (check k diag1) (check k diag2))))
