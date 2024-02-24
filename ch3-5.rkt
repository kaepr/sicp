#lang sicp

;;
;; (cons-stream x y)
;; (head s)
;; (tail s)
;; empty-stream
;;
;; for any x and y
;; (head (cons-stream x y)) -> x
;; (tail (cons-stream x y)) -> y
;;

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream
      (proc (stream-car s))
      (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin 
          (set! result (proc))
          (set! already-run? true)
          result)
        result))))

;; ex 3.50

(define (stream-map-generic proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map-generic 
             (cons proc (map stream-cdr argstreams))))))


(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(stream-enumerate-interval 1 10)


;; ex 3.51

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map
   show
   (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
(stream-ref x 1)

;; ex 3.52

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter
           pred
           (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

(display newline)
                    
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter
   (lambda (x) (= (remainder x 5) 0)) seq))
(stream-ref y 7)
(display-stream z)

(define (integers-starting-from n)
  (cons-stream
   n
   (integers-starting-from (+ n 1))))

(define (divisible? x y) (= (remainder x y) 0))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes
  (sieve (integers-starting-from 2)))

(stream-ref primes 50)

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map-generic + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))



;; ex 3.53

(define s (cons-stream 1 (add-streams s s)))

(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)


;; 1 + 1, 2 + 2, 4 + 4 ...


;; ex 3.54

(define (mul-streams s1 s2)
  (stream-map-generic * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

(stream-ref factorials 6)

;; ex 3.55

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (stream-cdr s) (partial-sums s))))


(partial-sums integers)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr x) (stream-cdr t)))))
































































