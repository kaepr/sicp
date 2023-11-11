lang racket/base

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

(define (my-map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (my-map proc (cdr items)))))

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

(count-leaves (cons (list 1 2) (list 3 4)))

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

(define (fringe tree)
  (if (null? tree)
    '()
    (let ((x (car tree)))
      (append (if (list? x)
                (fringe x)
                (list x))
              (fringe (cdr tree))))))

(fringe (list (list 1 2) (list 1 2)))


; ex 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))


; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree factor))
        (else
          (cons (scale-tree (car tree)
                            factor)
                (scale-tree (cdr tree)
                            factor)))))

(scale-tree (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7))
            10)

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree-map sub-tree factor)
           (* sub-tree factor)))
       tree))


;; ex 2.31


(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree tree) 
  (tree-map (lambda (x) (* x x)) tree))

(square-tree 
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

;; ex 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map 
                     (lambda (x) 
                       (cons (car s) x))
                     rest)))))

(subsets (list 1 2 3))

;; Sequence Operations

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
    ((predicate (car sequence))
     (cons (car sequence)
           (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op
                    initial
                    (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4))


(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 10)

(define (enumerate-tree tree)
  (cond 
    ((null? tree) nil)
    ((not (pair? tree)) (list tree))
    (else
      (append
        (enumerate-tree (car tree))
        (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))


;; ex 2.33

(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map-acc (lambda (x) (+ 1 x)) (list 1 2 3))

(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1))

(append-acc (list 1 2 3) (list 4 5 6))

(define (length-acc seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(length-acc (list 1 2 3))

;; ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate 
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* x higher-terms)))
    0 
    coefficient-sequence)) 

(horner-eval 2 (list 1 3 0 5 0 1))

;; ex 2.35

;; flattens the list recursively to numbers,
;; then accumulates
(define (count-leaves-acc t)
  (accumulate 
    + 
    0 
    (map 
      (lambda (sub-tree)
        (cond 
          ((null? sub-tree) 0)
          ((not (pair? sub-tree)) 1)
          (else (count-leaves-acc sub-tree)))) 
      t))) 

(define ct-tree (cons (list 1 (list 2 5)) (list 3 4)))
(count-leaves ct-tree)
(count-leaves-acc ct-tree)

;; ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;; ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (map-n proc seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate proc (car (car seqs)) (cdr (map car seqs)))
          (map-n proc (map cdr seqs)))))

(define (map-n-acc-n proc seqs)
  (accumulate-n))

(map-n * '((1 2 3) (1 2 3)))

(define (my-dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list (v w)))))

(accumulate + 0 (accumulate-n * 1 (list (list 1 2 3) (list 4 5 6))))

(map * '(1 2 3) '(4 5 6))

(dot-product '(1 2 3) '(4 5 6))

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product row v)) m))

(define mat '((0 1 2) (1 2 3) (2 3 4)))

(matrix-*-vector mat '(1 2 3))

(define (transpose m)
  (accumulate-n cons '() m))

(transpose '((1 2 3) (4 5 6)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (row) (matrix-*-vector cols row)) m)))

;; ex 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left  / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

;; they must satisfy associativity property

;; ex 2.39
(define (reverse-fold-right seq)
  (fold-right (λ (cur acc) (append acc (list cur))) nil seq))

(reverse-fold-right '(1 2 3))

(define (reverse-fold-left seq)
  (fold-left (λ (cur acc) (cons acc cur)) nil seq))

(reverse-fold-left '(1 2 3))

;; Nested Mappings

(accumulate 
  append 
  nil 
  (map (λ (i)
         (map (λ (j)
                (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 10)))

;; combination of mapping and accumulating with append is common
;; and called a flatmap

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(require math/number-theory)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum?
               (flatmap (λ (i) 
                          (map (λ (j) 
                                 (list i j)) 
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(prime-sum-pairs 5)

(flatmap (λ (x) (list (+ x 1))) (enumerate-interval 1 5))

(define (remove item sequence)
  (filter (λ (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (λ (x)
               (map (λ (p)
                      (cons x p))
                    (permutations (remove x s))))
             s))) 

(permutations '(1 2 3))

;; ex 2.40
  
(define (range n)
  (define (range-iter cur acc)
    (if (= 0 cur)
      acc 
      (range-iter (- cur 1) (cons cur acc))))
  (range-iter n '()))

(flatmap (λ (i)
           (map (λ (j) 
                  (list j i)) (range (- i 1)))) (range 9)) 

(flatmap (λ (i) (map (λ (j) (list j i)) (range (- i 1)))) (range 10))

(flatmap (λ (x) x) (map (λ (x) (list x 5)) (range 4)))

(define (unique-pairs n)
  (define (range n)
    (define (range-iter cur acc)
      (if (= 0 cur)
        acc 
        (range-iter (- cur 1) (cons cur acc))))
    (range-iter n '()))
  (define (make-pair x y) (list x y))
  (flatmap (λ (i)
             (map (λ (j) 
                    (make-pair j i)) (range (- i 1)))) 
           (range n)))

(unique-pairs 5)  

(define (prime-sum-pairs-improv n)
  (map make-pair-sum
   (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs-improv 5)


;; ex 2.41

(define (unique-triples-with-sum n s)
  (define (sum xs)
    (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))
  (define (sum-equals-s? xs)
    (= (sum xs) s))
  (define (range n)
    (define (range-iter cur acc)
      (if (= 0 cur)
        acc 
        (range-iter (- cur 1) (cons cur acc))))
    (range-iter n '()))
  (define (unique-triples n)
    (flatmap 
      (λ (i) 
        (flatmap 
          (λ (j) 
            (map 
              (λ (k) (list k j i)) 
              (range (- j 1))))
          (range (- i 1)))) 
      (range n)))
  (filter sum-equals-s? (unique-triples n)))

(flatmap 
  (λ (i) 
    (flatmap 
      (λ (j) 
        (map (λ (k) (list k j i)) (range (- j 1)))) (range (- i 1)))) 
  (range 10))

(map 
  (λ (k) (list k)) 
  (range (- 10 1)))
  
(unique-triples-with-sum 5 7)

;; ex 2.42

;; representation could be (row, col)

(define (empty-board) '())

(define (makr-pos r c)
  (list (r c)))

(define (get-row pos)
  (car pos))

(define (get-col pos)
  (cadr pos))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-pos new-row k) rest-of-queens))

; make sure new column is not hit by queens in same row, same column 
; or diagonal
(define (safe? k positions)
  (define (row-same? pos1 pos2)
    (= (get-row pos1) (get-row pos2)))
  (define (diagonal-same? pos1 pos2)
    (= (abs (- (get-row pos1) (get-row pos2)) (abs (- (get-col pos1) (get-col pos2))))))
  (define (pos-safe? pos1 pos2)
    (if (or (row-same? pos1 pos2) (diagonal-same? pos1 pos2))
      #f
      #t))
  ())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter 
        (λ (positions) (safe? k positions)) 
        (flatmap 
          (λ (rest-of-queens) 
            (map 
               (λ (new-row) 
                 (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


