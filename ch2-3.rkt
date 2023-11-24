#lang racket 

(define a 1)
(define b 1)
(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))
(list 'car (list 'quote '(a b c)))

(define (memq item x)
  (cond 
    ((null? x) false)
    ((eq? item (car x)) x)
    (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x apple (apple sauce) y apple pear))

;; ex 2.53
(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)
 

(car '((red shoes) (blue socks))) 
(memq '(red shoes) '((red shoes) (blue socks))) 
(equal? '(red shoes) '(red shoes)) 

;; ex 2.54
(define (my-equal? l1 l2)
  (define (is-same? x y)
    (eq? x y))
  (if (and (null? l1) (null? l2)) 
    #t
    (if (or (null? l1) (null? l2))
      #f
      (and 
        (eq? (car l1) (car l2)) 
        (my-equal? (cdr l1) (cdr l2))))))

(my-equal? '(red shoes) '(red shoes)) 

;; ex 2.55

; ' takes everything and puts it into list
; '' is (') -> (quote)

;; ''a
;; (car ''abracadabra)
;; 'quote

;; derivation ex 2.56, ex 2.57

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op 
      (car sequence) 
      (accumulate op initial (cdr sequence)))))

(list 5)
(cons 4 (cons 5 '()))

(define (sum xs) (accumulate + 0 xs))

(sum '(1 2 3 4))

(filter number? '(1 2 3 x))

(filter (compose not number?) '(1 2 3 x))

(length '(1 3 3 4))
(empty? '())

(define (make-sum a1 a2)
  (cond 
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2)))) 

(list? '())

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
  (accumulate make-sum 0 (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (accumulate make-product 1 (cddr p)))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    ((=number? base 1) 1)
    (else (list '** base exponent))))

(define (deriv exp var)
  (cond 
    ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
       (make-product
         (multiplier exp)
         (deriv (multiplicand exp) var))
       (make-product 
         (deriv (multiplier exp) var)
         (multiplicand exp))))
    ; (deriv (** b e)) -> (* e (b ** (- e 1)) (deriv b)) 
    ((exponentiation? exp)
     (make-product
       (make-product 
         (exponent exp)
         (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
       (deriv (base exp) var)))
    (else (error "unknown expression type: DERIV" exp))))

(deriv '(+ (* 5 x y) 5 5 5 5 x (* 10 x)) 'x)

(deriv '(* (* 5 5) x) 'x)
(deriv '(+ 0 (* 5 5)) 'x)

;; TODO: ex 2.58

;; representing sets

(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((element-of-set? (car set1) set2) 
     (cons (car set1) (intersection-set (cdr set1) set2)))
    (else (intersection-set (cdr set1) set2))))
    
(intersection-set '(1 2 3) '(4 2 6))

;; ex 2.59
(define (union-set set1 set2)
  (cond 
    ((null? set1) set2)
    ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
    (else (cons (car set1) (union-set (cdr set1) set2)))))


(union-set '(1 2 3) '(4 2 6))

;; ex 2.60

; element of set is same
; intersection is not exactly clear
(define union-set-duplicates append)
(define adjoin-set-duplicates cons)

;; Ordered Sets 

(define (element-of-set-ordered? x set)
  (cond 
    ((null? set) false)
    ((= x (car set)) true)
    ((< x (car set)) false)
    (else (element-of-set-ordered? x (cdr set)))))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let 
      ((x1 (car set1)) 
       (x2 (car set2)))
      (cond 
        ((= x1 x2) (cons x1 (intersection-set-ordered (cdr set1) (cdr set2))))
        ((< x1 x2) (intersection-set-ordered (cdr set1) set2))
        ((< x2 x1) (intersection-set-ordered set1 (cdr set2)))))))

(element-of-set-ordered? 1 '(1 2 3 4))
      
(intersection-set-ordered '(1 2 3) '(1 5 6))

;; ex 2.61

(define (adjoin-set-ordered x set)
  (cond 
    ((null? set) (list x))
    ((= x (car set)) set)
    ((> x (car set)) (cons (car set) (adjoin-set-ordered x (cdr set))))
    ((< x (car set)) (cons x set))))

(adjoin-set-ordered 9 '(1 2 3 10))

;; ex 2.62

(define (union-set-ordered set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
      (let ((x (car set1))
            (y (car set2)))
        (cond 
          ((= x y) (cons x (union-set-ordered (cdr set1) (cdr set2))))
          ((< x y) (cons x (union-set-ordered (cdr set1) set2)))
          ((> x y) (cons y (union-set-ordered set1 (cdr set2))))))))) 

(union-set-ordered '(1 2 3 4) '(3 4 5 6))

;; sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set-tree? x set)
  (cond
    ((null? set) false)
    ((= x (entry set)) true)
    ((< x (entry set)) (element-of-set-tree? x (left-branch set)))
    ((> x (entry set)) (element-of-set-tree? x (right-branch set))))) 

(define (adjoin-set-tree x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set)) (make-tree 
                         (entry set) 
                         (adjoin-set-tree x (left-branch set))
                         (right-branch set)))
    ((> x (entry set)) (make-tree
                         (entry set)
                         (left-branch set)
                         (adjoin-set-tree x (right-branch set))))))

;; ex 2.63

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append
      (tree->list-1 (left-branch tree))
      (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list
        (left-branch tree)
        (cons (entry tree)
              (copy-to-list (right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))

(define t (make-tree 
            3 
            (make-tree 1 '() '())
            (make-tree
              7
              (make-tree 5 '() '())
              (make-tree 
                9 
                '() 
                (make-tree 11 '() '())))))

(tree->list-1 t)

(tree->list-2 t)

; first code is much worse, it's appending everything at every level
; which adds some to time complexity
;
; second option would be faster as its doing a direct dfs, and adding elements 
; as its recursing down. it should be O(n)

;; ex 2.64

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
     (let ((left-result (partial-tree elts left-size)))
       (let ((left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1))))
         (let ((this-entry (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size)))
           (let ((right-tree (car right-result))
                 (remaining-elts (cdr right-result)))
             (cons (make-tree this-entry left-tree right-tree) 
                   remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(list->tree '(1 3 5 7 9 11))

;; http://jots-jottings.blogspot.com/2011/12/sicp-exercise-264-constructing-balanced.html

;; ex 2.65

(define (union-set-balanced-tree a b)
  (list->tree (union-set-ordered (tree->list-1 a) (tree->list-1 b))))

(define (intersection-set-balanced-tree a b)
  (list->tree (intersection-set-ordered (tree->list-1 a) (tree->list-1 b))))

(list->tree '(1 2 3 5 7 9 46))

(union-set-ordered
  '(1 2 3 5 7 9 46)
  '(5 6 10 11 20 23 46))

(union-set-balanced-tree (list->tree '(1 2 3 5 7 9 46))
                         (list->tree '(5 6 10 11 20 23 46)))

;; ex 2.66

(equal? 5 5)

(define (lookup given-key set-of-records)
  (element-of-set? given-key set-of-records))

;; huffman tree

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (make-code-tree left right)
  (list 
    left
    right
    (append (symbols left)
            (symbols right))
    (+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let 
        ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
  
(define (adjoin-set x set)
  (cond 
    ((null? set) (list x))
    ((< (weight x) (weight (car set)))
     (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set
        (make-leaf (car pair) (cadr pair))
        (make-leaf-set (cdr pairs))))))


;; ex 2.67

(define sample-tree 
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

sample-tree

(decode sample-message sample-tree)

;; ex 2.68

(define (encode-symbol symbol tree)
  (cond
    ((leaf? tree) '())
    ((memq symbol (symbols (left-branch tree))) 
     (cons 0 
           (encode-symbol symbol (left-branch tree))))
    ((memq symbol (symbols (right-branch tree))) 
     (cons 1 
           (encode-symbol symbol (right-branch tree))))
    (else (error "missing symbol: ENCODE-SIGNAL" symbol))))

(define (encode message tree)
  (if (null? message)
    '()
    (append
      (encode-symbol (car message) tree)
      (encode (cdr message) tree))))

(encode-symbol 'A sample-tree)

(encode '(A D A B B C A) sample-tree)

;; ex 2.69
