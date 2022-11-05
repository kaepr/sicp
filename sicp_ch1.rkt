#lang sicp
;; Chapter 1

;; Parameters are name placeholders
(define (square x)
  (* x x))

;; Arguments are what the values goes into the fxn
(square -1)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 5 6)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)
;; Conditional expressions

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) x)
        ((< x 0) (- x))))

(abs 5)
(abs 0)
(abs -5)

;; General form is
;; (cond (<p1> <e1>) [ (<p> <e>) is called a clause ]
;;       (<p2> <e2>)
;;        .........
;;       (<pn> <en>))


(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

;; (if <pred> <consequent> <alternative>)
(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(define (in-range x)
  (and (> x 5) (< x 10)))

(in-range 1)


;; Exercise 1.1 in evernote

;; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3(+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3

(define (sum-square-of-larger-2 x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> x y) (> z y)) (sum-of-squares x z))
        ((and (> z x) (> y x)) (sum-of-squares z y))))

(sum-square-of-larger-2 1 2 3)
(sum-square-of-larger-2 1 5 2)

;; Exercise 1.4
;; Operation changes based on the sign on b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 4 0)


;; Exercise 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; (p)

;; (test 0 (p))


;; Square Root

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve y x)
  (average y (/ x y)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? x y)
  (< (abs (- (square x) y)) 0.00001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2)


(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))

;; Exercise 1.6
;; original if is special form
;; new-if is a function, thus following applicative
;; order evaluation, so it recurses infinitely
;; as sqrt-iter2 is called
(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter2 (improve guess x) x)))

;; (sqrt-iter2 1 5)

;; Exercise 1.7

;; (sqrt 500000000000) fails for bigger numbers

(define (good-enough2? prev curr)
  (< (abs (- prev curr)) 0.001))

(define (sqrt-iter3 guess x)
  (if (good-enough2? guess (improve guess x))
      guess
      (sqrt-iter3 (improve guess x) x)))

(sqrt-iter3 1.0 500000000000)

;; Exercise 1.8

(define (c-improve y x)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (c-root guess x)
  (if (good-enough2? guess (c-improve guess x))
      guess
      (c-root (c-improve guess x) x)))

(c-root 1.0 27)

;; block structure, defining fxns inside fxns

(define (sqrt3 x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)(average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt3 100)

;; Not using x as parameter in internal fns
(define (sqrt-4 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt-4 10)

;; Recursive definition of factorial
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1 )))))

  
;; Iterative definition of factorial

(define (fact n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Writing without using extra function
(define (fact2 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(fact2 20)










