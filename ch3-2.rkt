
;; environment is a sequence of frames
;; each frame is table of bindings, associating variable names with their values
;; each frame also has a pointer to its enclosing environment
;; 
;; expression acquires meaning only with respect to some environment in which 
;; it is evaluated
;;

(define square
  (λ (x) (* x x)))

;; above expression evaluates (λ (x) (* x x)) and binds square to the resulting values
;; in the global environment
;; 
;; new binding which associates the procedure object with symbol square
;; has been added to global frame
;;

;; ex 3.10
;; the second version contains an extra environment E2. This E2 is what's returned to the global 
;; environment when object is constructed. balance belongs to E2. 
;; behaviour wise both are identical, but the second approach contains more environment hops


;; names of local procedures do not interfere with names external to the enclosing
;; procedure, as local procedure names will be bound in the frame that the procedure
;; creates when it is run, rather than being bound in global environment
;;
;; local procedures can access the arguments of the enclosing procedure, simply 
;; by using parameter names as free variables. as the body of local procedure is
;; evaluated in an environment that is subordinate to the evaluation environment
;; for the enclosing procedure
;;



