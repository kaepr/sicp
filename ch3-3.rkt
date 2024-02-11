#lang sicp


(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

z

(define x (list 'a 'b))
(define z1 (cons x x))

z1

(define z2 (cons '(a b) '(a b)))
z2

;; ex 3.16

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;; Using set-cdr! it's possible to make a recursive list like shown above
;;  #0=(cons 'a (cons 'b  #0))
;;
;;  For 3

(count-pairs (list 'a 'b 'c))

;; For 4
(count-pairs '(a (b c)))

;; For 7
(define a7 '(1))
(define b7 (cons a7 a7))
(define c7 (cons b7 b7))
(count-pairs c7)

;; ex 3.17
; Use memq to test for equality of constants
; Use a secondary list to hold all the lists already encountered
(memq 5 '(1 2 5))

(define (count-pairs-memq x)
  (define pairs '())
  (define (count x)
    (if (not (pair? x))
      0
      (+
        (count (car x))
        (count (cdr x))
        (if (memq x pairs)
          0
          (begin 
            (set! pairs (cons x pairs)) 
            1)))))
 (count x))

(count-pairs-memq c7)


;; ex 3.18, modify above solution, but check in the cdr 
;; of a pair if that value already exists, then it becomes infinite



(define (my-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond
      ((eq? m 'car) x)
      ((eq? m 'cdr) y)
      ((eq? m 'set-car!) set-x!)
      ((eq? m 'set-cdr!) set-y!)
      (else (error "Undefined operation: CONS" m))))
  dispatch)

(define (my-car z) (z 'car))
(define (my-cdr z) (z 'cdr))
(define (my-set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (my-set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; ex 3.20

(define x (cons 1 2))
(define z (cons x x))
x
z
(set-car! (cdr z) 17)
(car x)
(cdr x)


;; TODO: Get back to remaining 3.3 sections

