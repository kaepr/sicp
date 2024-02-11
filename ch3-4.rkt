;; Serialization implements the following idea: Processes will execute 
;; concurrently, but there will be certain collections of procedures that
;; cannot be executed concurrently.


;; Serialization creates distinguished sets of procedures such that only one 
;; execution of a procedure in each serialized set is permitted to happen at a 
;; time. If some procedure in the set is being executed, then a process that
;; attempts to execute any procedure in the set will be forced to wait until
;; the first execution has finished.
;;


;; ex 3.39
;; 
;; 101, 121 -> run's sequentially 
;; 100 -> Squarer run's read and write separately, thus increment can happen
;; in b/w those times, thus effectively ignoring it's values
;;
;; ex 3.40
;; 
;; (10^2)^3: always get this value
;;
;; ex 3.41
;; shouldn't affect the operations. It still reads the correct value of balance
;; even if the other two transactions are still running, it is correct behaviour 
;; to show the balance as it is. 
;;
;; ex 3.42
;; procedures inside a serialized set are made sure to not run concurrently
;; the withdraws and deposits are still part of the same set
;; this would imply that no change should take place to functioning of the code
;;

;; TODO: 43, 44, 45

;; Implementing serializers

#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond 
        ((eq? m 'acquire) (if (test-and-set! cell)
                              (the-mutex 'acquire)))
        ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


;; TODO: Revise concurrency part again
