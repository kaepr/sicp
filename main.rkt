#lang racket
;(display "Hello")
;"Hello World"
;foo
;(1 2)
(+ 1 2)
(lambda (x) (* x 2))
(define double (lambda (x) (* x 2)))
(double 5)
(define name "Jane")
(string-append "hello " name "!")
(define greet
  (lambda (name)
    (string-append "Hello " name "!")))
(greet name)
(define (greet2 name)
  (string-append "hello " name "!"))

(greet2 "Shagun")

((lambda (name)
    (string-append "Hello " name "!")) "Morgan")

(greet "f.rift")
name

(let (( name "Horace"))
  (string-append "GREETINGS " name "!!!"))

(define num-list '(1 2 3))
(apply + num-list)

(define (chatty-add chatty-name . nums)
  (format "~a if you add those together you get ~a !\n"
          chatty-name (apply + nums)))

(chatty-add "John" 2 4 5 6)

(string? "apple")

(if #f
    "That true"
    "Thats false")

(> 8 9)

(define (goldilocks n smallest-ok biggest-ok)
  (if (< n smallest-ok)
      "Too small!"
      (if (> n biggest-ok)
          "Too big!"
          "Just right!")))

(goldilocks 11 10 20)

(define name2 "Foo")
name2

(define (goldilocks1 n smallest-ok biggest-ok)
  (cond
    [(< n smallest-ok)
     "Too small!"]
    [(> n biggest-ok)
     "Too big!"]
    [else "Just right"]))

(goldilocks1 123 2 3)

(define a-list (list 1 2 3))
(define b-list (list 1 2 3))

(equal? a-list b-list) ; same value?
(eq? a-list b-list) ; same object in memory?








































