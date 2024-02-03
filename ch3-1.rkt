
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond 
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request: MAKE-ACCOUNT " m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50) 
((acc 'deposit) 50) 
      
;; ex 3.1

(define (make-accumulator acc-value)
  (lambda (v)
    (begin (set! acc-value (+ acc-value v)) acc-value)))

(define A (make-accumulator 5))
(A 10)
(A 10)


;; ex 3.2


(define (add-one x) (+ x 1))
(add-one 5)

(define (make-monitored f)
  (let ((count 0))
    (lambda (input)
      (cond 
        ((eq? input 'how-many-call?) count)
        ((eq? input 'reset-count) (set! count 0))
        (else (begin (set! count (+ count 1)) (f input)))))))

(define monitored-add-one (make-monitored add-one))

(monitored-add-one 'how-many-call?)
(monitored-add-one 'reset-count)
(monitored-add-one 5)

;; adding assignment leads to variables now referring to 
;; a place where a value can be stored, and changed

;; "equals can be substituted for equals" in an expression, without changing
;; the value of the expression is said to be referentially transparent

;; ex 3.8

;; http://community.schemewiki.org/?sicp-ex-3.8 
;; state switch solution
(define f
  (let ((state 0))
    (lambda (x)
      (if (= state 1)
        (begin (set! state 0) state)
        (begin (set! state 1) x)))))

(+ (f 0) (f 1))
(+ (f 1) (f 0)) 



