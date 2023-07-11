#lang sicp

(display "ch1.3")

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 1000)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-iter pi-term a pi-next b))

(* 8 (pi-sum 1 100000))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-mult a b)
  (define (pi-term n)
    (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
  (define (pi-next n) (+ n 1))
  (* (product pi-term a pi-next b)) 1.0)

(* (pi-mult 1 10000) 4)

(define (pi n)
  (define (term x) (* x x))
  (define (next x) (+ x 2))
  (define limit (* n 2))
  (* 4 (/ (/ (* 2 (product term 4 next (+ limit 2)))
             (+ limit 2))
          (product term 3 next (+ limit 1)))))

(* 1.0 (pi 10000))


(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

(sum identity 1 inc 5)

(define (filtered-accumulate combiner null-value term a next b filter?)
  (if (> a b)
    null-value
    (combiner (if (filter? a)
                (term a)
                null-value) 
              (filtered-accumulate combiner null-value term (next a) next b filter?))))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

#|

(define (f g) (g 2))
(f square) -> 4
-> (f ((x)(* x x))
-> (((x) (* x x)) 2)
-> (* 2 2)
-> 4

(f f)
-> (f (lambda (f') (f' 2)))
-> ((lambda (f') (f' 2)) 2)
-> (2 2)
-> error

|#


(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond
          ((positive? test-value) (search f neg-point midpoint))
          ((negative? test-value) (search f midpoint pos-point))
          (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value)) (search f a b))
          ((and (negative? b-value)
                (positive? a-value)) (search f b a))
          (else
            (error "Values are not opposite sign" a b))))) 


(half-interval-method sin 2.0 4.0)     


(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)


