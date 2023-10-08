#lang racket
(#%require r5rs/init)

(define nil '())

(cons (cons 1 2) (cons 3 4))

(list 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons 
      (car list1)
      (append (cdr list1) list2))))

; ex2.17
; Input is always a non empty list
(define (last-pair items)
  (if (null? (cdr items)) 
    (list (car items))
    (last-pair (cdr items))))

(last-pair (list 1))
(last-pair (list 1 2 3 4))
 
; ex2.18
(define (reverse items)
  (if (null? items)
    items
    (append (reverse (cdr items))
            (cons (car items) nil))))

(define (reverse-2 items)
  (if (null? items)
    items
    (append (reverse-2 (cdr items))
            (list (car items)))))

(reverse (list 1))

(reverse (list 1 2 3 4))

(reverse-2 (list 1 2 3 4))


; ex 2.20
(define (same-parity fst . rst)
  (define parity (remainder fst 2))
  (define (iter items)
    (if (null? items)
      items
      (if (= parity (remainder (car items) 2))
        (cons (car items) (iter (cdr items)))
        (iter (cdr items)))))
 (cons fst (iter rst)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

; Mapping over lists

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(map abs (list -10 2.4 11.9))

; map isolates the implementation of procedures that transforms a list
; from the details of how the elements are extracted and combined
;

; ex 2.21

(define (square-list items)
  (if (null? items)
    nil
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

(square-list-map (list 1 2 3 4))

; ex 2.22

(define square (lambda (x) (* x x)))

(define (square-list-22-1 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
 (iter items nil))

(square-list-22-1 (list 1 2 3 4))

; the list is constructed `bottom_up` in a sense. All the computation happens
; in the arguments, and is consed from the last level.

(define (square-list-22-2 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
              (square (car things))))))
 (iter items nil))

(square-list-22-2 (list 1 2 3 4))

; changing arguments to cons does fix the combination order
; but con's a list onto a list, creating a nested sort of data structure



; ex 2.23

(define (for-each proc list) 
  (if (null? list) 
      '() 
      ((lambda () 
         (proc (car list)) (for-each proc (cdr list))))))

(for-each
  (lambda (x) (newline) (display x))
  (list 1 2 3))


; hiearchical data structures

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))



; ex2.24

(list 1 (list 2 (list 3 4)))
; => (1 (2 (3 4)))

; ex 2.25

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

(car (car (list (list 7))))

; ex 2.26

(define x-26 (list 1 2 3))
(define y-26 (list 4 5 6))

(display (append x-26 y-26))
; (1 2 3 4 5 6)
(cons x-26 y-26)
; ((1 2 3) 4 5 6)
(list x-26 y-26)
; ((1 2 3) (4 5 6))


; ex 2.27
(list? (list 1 2 3))
(define (deep-reverse items)
  (if (null? items)
    items
    (append (deep-reverse (cdr items))
            (cons 
              (if (list? (car items))
                (deep-reverse (car items))
                (car items))
              nil))))

(display (deep-reverse (list (list 1 2) (list 3 4))))

; ex 2.28

(define (fringe tree))
  
