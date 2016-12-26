;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Functional) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; DATA DEFINITIONS
;; ***************************************************
;; A Phone Record is (make-phone-record Str Nat)
(define-struct phone-record (name number))
;;A Node is a (make-node Nat Str Tree Tree)
(define-struct node (ssn name left right))
;; A Tree is one of:
;; * BinarySearchTree
;; * SearchTree

;; A SearchTree is one of:
;; * empty
;; * (list Node (listof SearchTree) (listof SearchTree))

;; A BinarySearchTree is one of: (BST)
;; * empty
;; * (list Node (listof BinarySearchTree) (listof BinarySearchTree))
;; requires: all values of key in left < key of node
;; requires: all values of ley in right > key of node 
(define tree (make-node 200 "R"
                                  (make-node 175 "r"
                                             (make-node 165 "l" empty empty)
                                             (make-node 176 "r" empty empty))
                                  (make-node 250 "T"
                                             (make-node 225 "r"
                                                        (make-node 220 "y" empty empty)
                                                        (make-node 230 "k"  empty empty))
                                             (make-node 280 "r"
                                                        (make-node 275 "r" empty empty)
                                                        (make-node 290 "t" empty empty)))))
(define-struct triangle (point1 point2 point3))

(define Triangle1 (make-triangle (make-posn 8 1) (make-posn 18 1) (make-posn 10 1)))
(define Triangle2 (make-triangle (make-posn 8 5) (make-posn 18 5) (make-posn 10 5)))
(define Triangle3 (make-triangle (make-posn 1 0) (make-posn 3 0) (make-posn 1.5 3)))
;; ***************************************************
;; FUNCTION DEFINITIONS
;; ***************************************************
;; (unzip listofpairs) consumes a list of Pairs and produces a lists of two lists, where the
;;  first list contains the first element of every Pair and the second list
;;   contains the second element of every  Pair 
;; listofpairs: (listof Pairs) -> (listof (listof Any) (listof Any))
;; Examples: 
(check-expect (unzip (list (list empty empty)))(list (list empty) (list empty)))
(check-expect (unzip (list (list true "tree") (list 3 false) (list "polly" 4)))
              (list (list true 3 "polly") (list "tree" false 4)))

(define (unzip listofpairs)
(local [(define list1
          (map (lambda (e)
                 (first e)) listofpairs))
        (define list2
          (map (lambda (e)
                 (second e))listofpairs))]
  (list list1 list2)))  
;; Tests:
(check-expect (unzip (list (list empty 5) (list "string" false)))
              (list (list empty "string") (list 5 false)))
(check-expect (unzip (list (list true "tree") (list 3 false) (list "polly" 4)))
              (list (list true 3 "polly") (list "tree" false 4)))
(check-expect (unzip (list (list "a" "b") (list "c" "d") (list "e" "f")))
              (list (list "a" "c" "e") (list "b" "d" "f")))
(check-expect (unzip  (list '(1 2) '(3 4) '(5 6)))(list '(1 3 5) '(2 4 6)))
;; ***************************************************
;; (de-dup list) consumes a list of any values and produces a new list with
;;   only one occurence of each element of the original list, in any order
;; de-dup: (listof Any) -> (listof Any)
;; Examples:
(check-expect (de-dup (list 5 25 10 5))(list 25 10 5))
(check-expect (de-dup empty)empty)

(define (de-dup list)
  (foldr
     (lambda (x y)
       (cond
         [(member? x y) y]
         [else (cons x y)])) empty list))
;;Tests:
(check-expect (de-dup (list 10 12 15)) (list 10 12 15))
(check-expect (de-dup (list "a" "b" "c" "d" "c" "e" "c")) (list "a" "b" "d" "e" "c"))
(check-expect (de-dup (list true false true false true false)) (list true false))
(check-expect (de-dup (list 1 2 34 6 38 20 0 90 10 11 45 19 0))
              (list 1 2 34 6 38 20 90 10 11 45 19 0))
(check-expect (de-dup '(1 3 5 1))'(3 5 1))
;; ***************************************************
;; (occurrences list value) consumes a list and a value and produces the
;;   number of times that the given value occurs in the list 
;; occurrences: (listof Any) Any -> Nat
;; Examples: 
(check-expect (occurrences (list 5 10 5 10 100) 5)2)
(check-expect (occurrences empty "string")0)

(define (occurrences list value)
  (length
   (filter
    (lambda (x)
      (cond
        [(equal? x value) true]
        [else false])) list)))
;; Tests:
(check-expect (occurrences (list 1 2 3 4 5) 100)0)
(check-expect (occurrences (list true true false true true true)false)1)
(check-expect (occurrences (list (list 2 false)
                                (list "Hello" 9) (list 10 empty)) (list 10 empty)) 1)
(check-expect (occurrences (list 5 5 5 5 5)5)5)
(check-expect (occurrences (list 5 5 5 5 5)"string")0)
(check-expect (occurrences (list 1 1 1) 1)3)
(check-expect (occurrences (list 0 0)0)2)
(check-expect (occurrences (list -3 -5 -6 -7 -3)-3)2)
(check-expect (occurrences (list -3 -5 -6 -7 -3 0)0)1)
(check-expect (occurrences (list -3 -5 -6 -7 -3)-9)0)
(check-expect (occurrences (list 19 39 27 19)0)0)
(check-expect (occurrences (list 6 2 81 2) 10)0)
;; ***************************************************
;; (occurrences-al list) consumes a list of numbers and produces a NAL
;;   where the keys are all the unique elements of the list and the values
;;    are the corresponding number of occurrences   
;; occurrences-al: (listof Num)-> NAL 
;; Examples: 
(check-expect (occurrences-al (list 1 1 2 3)) (list (list 1 2) (list 2 1) (list 3 1)))
(check-expect (occurrences-al empty)empty) 

(define (occurrences-al list-of-values)
(de-dup (map (lambda (x)
       (list x
             (local [(define value x)]
               (occurrences list-of-values value)))) list-of-values)))
;; Tests:
(check-expect (occurrences-al (list 5 4 3 2)) (list (list 5 1) (list 4 1) (list 3 1) (list 2 1)))
(check-expect (occurrences-al (list 2)) (list (list 2 1)))
(check-expect (occurrences-al (list 5 5 5 5 5)) (list (list 5 5)))
(check-expect (occurrences-al (list 0 0)) (list (list 0 2)))
(check-expect (occurrences-al '(1 1 2 3))'((1 2)(2 1)(3 1)))
(check-expect (occurrences-al (list -1 -1 -1 0 2 3))
              (list (list -1 3) (list 0 1) (list 2 1) (list 3 1)))
(check-expect (occurrences-al (list -3 -5 -6 -7 -3))
              (list (list -5 1) (list -6 1) (list -7 1) (list -3 2)))
(check-expect (occurrences-al (list -3 -5 -6 -7 -3))
              (list (list -5 1) (list -6 1) (list -7 1) (list -3 2)))
(check-expect (occurrences-al (list -3 -5 -6 -7 -3 0))
              (list (list -5 1) (list -6 1) (list -7 1) (list -3 2) (list 0 1)))
;; ***************************************************
;; (find-max-pair nal) consumes a NAL and produces the pair that has the largest key
;; find-max-pair: NAL -> (list Num Any)
;; Examples: 
(check-expect (find-max-pair empty)empty)
(check-expect (find-max-pair (list (list 10 "any") (list 5 "string") (list 15 false))) (list 15 false))

(define (find-max-pair nal)
 (cond
   [(empty? nal) empty]
   [else 
   (first (sort nal (lambda (e1 e2)
                      (> (first e1) (first e2)))))]))
;; Tests:
(check-expect (find-max-pair (list (list 100 5) (list 80 10) (list 1000 10)))(list 1000 10))
(check-expect (find-max-pair (list (list -3 (list "a" "b" "c")) (list -9 true)))
              (list  -3 (list "a" "b" "c")))
(check-expect (find-max-pair (list (list 0 true) (list 0 false) (list -1 "bob"))) (list 0 true))
(check-expect (find-max-pair (list (list 5 "a") (list 5 "b"))) (list 5 "a"))
(check-expect (find-max-pair '((1 2) (2 1) (3 1)))'(3 1))
(check-expect (find-max-pair (list (list -5 "f") (list -4 "f") (list -7 "f") (list -7 "t"))) (list -4 "f"))
;; ***************************************************
;; (euclidean-dist list1 list2) produces the euclidean distance between two consumed lists of numbers
;; euclidean-dist: (listof Num) (listof Num) -> Nat
;; requires: the consumed lists must be of equal length 
;; Examples: 
(check-within (euclidean-dist (list 1 1) (list 2 2))1.414 0.001)
(check-expect (euclidean-dist empty empty)0)

(define (euclidean-dist list1 list2)
  (sqrt (foldr + 0 (map
   (lambda (x y)
     (sqr (- x y))) list1 list2))))
  
;; Tests:
(check-expect (euclidean-dist (list 5 10 -1 -24) (list 9 6 3 -20))8)
(check-expect (euclidean-dist (list 15 30 -3 -72) (list 27 18 9 -60)) 24)
(check-expect (euclidean-dist (list 5 8 10 0) (list 5 5 10 -4)) 5)
(check-expect (euclidean-dist (list 10 16 20 0) (list 10 10 20 -8))10)
(check-expect (euclidean-dist (list 0 0) (list 0 0)) 0)
(check-expect (euclidean-dist (list -10) (list -10)) 0)
(check-expect (euclidean-dist (list 15) (list 15))0)
(check-within (euclidean-dist (list 15 -2 3 0) (list 15 0 4 -1))2.449 0.001)
(check-within (euclidean-dist (list 0 -3 0) (list -3 0 -3))5.1961 0.0001)
;; ***************************************************
;; (sequence x y n symbol) consumes the first two elements of a sequence, the number of elements
;;   in that sequence and a symbol that is either arithmetic or geometric
;; sequence: Num Num Nat Sym -> (listof Num)
;; requires: a is not 0 when symbol is 'geometric 
;; Examples: 
(check-expect (sequence 10 10 0 'arithmetic)empty)
(check-expect (sequence 10 10 0 'geometric)empty)
(check-expect (sequence 5 4.9 10 'arithmetic)
              (list 5 4.9 4.8 4.7 4.6 4.5 4.4 4.3 4.2 4.1))

(define (sequence a a2 n symbol)
  (cond
    [(symbol=? symbol 'arithmetic)
     (local
       [(define d (- a2 a))]
       (build-list n (lambda (e)
                       (+ a (* e d)))))]
    [(symbol=? symbol 'geometric)
     (local
       [(define r (/ a2 a))]
       (build-list n (lambda (e)
                       (* a (expt r e)))))]))

;; Tests:
(check-expect (sequence 1 5 4 'geometric)(list 1 5 25 125))
(check-expect (sequence 10 10 0 'geometric)empty)
(check-expect (sequence 5 0 3 'geometric) (list 5 0 0))
(check-expect (sequence 5 0 3 'arithmetic) (list 5 0 -5))
(check-expect (sequence 1 1 10 'arithmetic)
              (list 1 1 1 1 1 1 1 1 1 1))
(check-expect (sequence 1 1 5 'geometric)
              (list 1 1 1 1 1))
(check-expect (sequence 1 0 3 'geometric)
              (list 1 0 0))
(check-expect (sequence -5 -6 4 'arithmetic)
              (list -5 -6 -7 -8))
(check-expect (sequence 0 -2 5 'arithmetic)
              (list 0 -2 -4 -6 -8))
(check-expect (sequence 2 0 6 'arithmetic)
              (list 2 0 -2 -4 -6 -8))
(check-expect (sequence 1 5 6 'arithmetic)
              (list 1 5 9 13 17 21))
(check-expect (sequence -3 2 3 'arithmetic)
              (list -3 2 7))
;; ***************************************************
;; (arithmetic? list) consumes a list of numbers and produces true if the list
;;   is an arithmetic sequence and false otherwise 
;; sequence?: (listof Num) -> Bool
(define (arithmetic? list)
  (or (empty? list)
      (empty? (rest list))
      (local [(define d (- (second list) (first list)))]
        (number?
         (foldr (lambda (first rest)
                  (cond
                    [(empty? rest) first]
                    [(false? rest) false]
                    [(= d (- rest first)) first]
                    [else false])) empty list)))))

(check-expect (arithmetic? empty) true)
(check-expect (arithmetic? (list 2 4 8 6)) false)
(check-expect (arithmetic? (list 0 0 0 0))true)
(check-expect (arithmetic? (list 5 5)) true)
(check-expect (arithmetic? (list 4))true)
(check-expect (arithmetic? (list -2 -2 -2 0))false)
(check-expect (arithmetic? (list 2 4 6 8 16 32 64))false)
(check-expect (arithmetic? (list 100 50 150 200 250))false)
(check-expect (arithmetic? (list 500 1000))true)
(check-expect (arithmetic? (list 5 4.9 4.8 4.7 4.6 4.5 4.4 4.3 4.2 4.1)) true)
(check-expect (arithmetic? (list -90128 0))true)
(check-expect (arithmetic? (list 93722 0))true)
;; ***************************************************
;; (string-replace word x y) consumes a string (word) and replaces all instances of the first consumed character in the word
;; with the second consumed character
;; string-replace: Str Char Char-> str
(define (string-replace word x y)
  (list->string (map (lambda (char)
          (cond
            [(char=? x char) y]
          [else char]))
   (string->list word))))

(check-expect (string-replace "apples" #\p #\r)"arrles")
(check-expect (string-replace "apples" #\x #\y)"apples")
;; ***************************************************
;; (dot-product v1 v2) consumes two lists (which represent vectors) and produces their dot product
;; empty represents the zero vector
;; dot-product: (listof Num) (listof Num) -> Num
(define (dot-product v1 v2)
(foldr + 0 (map * v1 v2)))

(check-expect (dot-product (list 1 2 3) (list 2 3 4)) 20)
(check-expect (dot-product empty empty) 0)
;; ***************************************************
;; (cross los lon) consumes a list of symbols and a list of numbers and produces a list of all
;;   possible pairs of symbols and numbers
;; cross: (listof Sym) (listof Num)-> (listof (list Sym Num))
(define (cross los lon)
  (foldr (lambda (s rest)
          (append (map (lambda (n) (list s n)) lon) rest)) empty los))

(check-expect  (cross '( 1 2 3 4) '( a b c d e f))
(list
 (list 1 'a)(list 1 'b)(list 1 'c) (list 1 'd)(list 1 'e)
 (list 1 'f)(list 2 'a)(list 2 'b)(list 2 'c)(list 2 'd)
 (list 2 'e) (list 2 'f)(list 3 'a)(list 3 'b)(list 3 'c)
 (list 3 'd) (list 3 'e) (list 3 'f) (list 4 'a)(list 4 'b)
 (list 4 'c) (list 4 'd) (list 4 'e)(list 4 'f)))
;; ***************************************************
;; (zip lon lop) combines two lists with the consumes function funct 
;; zip: (listof X) (listof Y)-> (listof Z)
;; requires: two lists must be of equal length

(define (zip lon lop funct)
  (map (lambda (n p) (funct n p)) lon lop))

;; Examples:
;; The above function can be used to make a list of Phone Records 
(check-expect (zip '( "Riya" "Josh" "Pratyush") '(2358271 304726 39363) make-phone-record)
(list (make-phone-record "Riya" 2358271) (make-phone-record "Josh" 304726) (make-phone-record "Pratyush" 39363)))
;; The above function can be used to make an association list
(check-expect (zip '(1 2 3 4 5 6 7) (list 'x 'y 'z 'a 'b 'c 'd) list)
              (list (list 1 'x) (list 2 'y) (list 3 'z) (list 4 'a) (list 5 'b) (list 6 'c) (list 7 'd)))
;; ***************************************************
;; (lists-equiv? lst1 lst2) consumes two lists and checks whether they're equal, regardless of order of elements
;; lists-equiv?: (listof Any) (listof any)-> Bool
(define (lists-equiv? lst1 lst2)
  (local [(define check (length lst1))]
  (and (= check (length lst2))
       (= check (length (filter (lambda (s) (member? s lst1)) lst2)))
       (= check (length (filter (lambda (s) (member? s lst2)) lst1))))))

(check-expect (lists-equiv? (list 1 2 3) (list 2 1 3))true)
(check-expect (lists-equiv? (list 1 2) (list 1 2 3 4)) false)
(check-expect (lists-equiv? (list 1 2 2) (list 1 2 3))false)
(check-expect (lists-equiv? (list 1 2 3) (list 3 2 1))true)
(check-expect (lists-equiv? empty empty) true)
(check-expect (lists-equiv? empty (list 1 2 3))false)
;; ***************************************************
;; (prefix list1 list2) consumes two lists and checks whether list1 is a prefix of list2
;; prefix: (listof Any) (listof Any)-> Bool
(define (prefix list1 list2)
(cond
  [(empty? list1) true]
  [(empty? list2) false]
  [(equal? (first list1) (first list2)) (prefix (rest list1) (rest list2))]
  [else false]))

(check-expect (prefix (list "a" "b" "c") (list "a" "b" "c" "d"))true)
(check-expect (prefix (list "a" "b" "b") (list "a" "b" "c"))false)
(check-expect (prefix (list "a" "b" "c" "d" "e") (list "a" "b" "c"))false)
(check-expect (prefix '( (make-posn 2 3) a "a") '((make-posn 2 3) a "a" 'b)) true)
(check-expect (prefix '( a (make-posn 2 3) "a") '((make-posn 2 3) 'a "a" 'b)) false)
(check-expect (prefix '( (make-posn 2 3) a "a") '("lal" 'a "a" 'b)) false)
;; ***************************************************
;;(a-funtion=? f1 f2 n1 n2) checks whether the result of the two functions consumed
;;   in the range of the two numbers consumed are equal.
;; a-function=? (Num Num-> Any) (Num Num-> Any) Nat Nat-> Bool
(define (a-function=? f1 f2 n1 n2)
  (cond [(= n1 n2) (equal? (f1 n1) (f2 n1))]
        [else 
  (equal? (map f1 (build-list (- n2 n1) (lambda (x) (+ n1 x))))
          (map f2 (build-list (- n2 n1) (lambda (x) (+ n1 x)))))]))

(check-expect (a-function=? (lambda (x) (+ 0 x)) (lambda (x) (* 1 x)) 5 10)true)
(check-expect (a-function=? even? odd? 2 8)false)
(check-expect (a-function=? (lambda (x) (= 2 x)) even? 2 2)true)
(check-expect (a-function=? (lambda (x) (* 3 x)) odd? 5 5)false)
;; ***************************************************
;; (my-gcd n m) finds the gcd of n and m, using generative recursion
;; my-gcd Int Int -> Nat
;; remember: gcd(-x, y)= gcd(x,y)
(define (my-gcd n m)
  (cond [(zero? m) (abs n)]
        [else (my-gcd m (remainder n m))]))

(check-expect (my-gcd 91 13)13)
(check-expect (my-gcd 100 40)20)
(check-expect (my-gcd 6 0)6)
(check-expect (my-gcd 6 1)1)
(check-expect (my-gcd -5 4)1)
(check-expect (my-gcd 9391381 318)1)
(check-expect (my-gcd 5 -3)1)
(check-expect (my-gcd 5 -5 )5)
;; ***************************************************
;; (in-order btree) consumes a binary tree and produces a list of all ssn numbers in the tree, in the correct order
;; in-order: BinarySearchTree -> (listof Nat)
(define (in-order btree)
  (cond
    [(empty? btree) empty]
    [else (append (in-order (node-left btree)) (list (node-ssn btree)) (in-order (node-right btree)))]))

(check-expect (in-order tree)
              (list 165 175 176 200 220 225 230 250 275 280 290))
;; ***************************************************
;; (insert-in-tree btree n s) inserts a key and value pair
;; insert-in-tree: BinarySearchTree Num Str-> BinarySearchTree
(define (insert-in-tree btree n s )
  (cond
    [(empty? btree) (make-node n s empty empty)]
    [ (> n (node-ssn btree)) (make-node  (node-ssn btree) (node-name btree)
                                         (node-left btree)
                                        (insert-in-tree (node-right btree) n s))]
    [else (make-node (node-ssn btree) (node-name btree)
                     (insert-in-tree (node-left btree) n s)
                     (node-right btree))]))
;; ***************************************************
;; (create lst) consumes a list of numbers and names and produces a Binary Search Tree
;; create: (listof (list Nat Str))-> BinarySearchTree
(define (create lst)
  (cond
    [(empty? lst) empty]
    [ else (insert-in-tree (create (rest lst)) (first (first lst)) (second (first lst)))]))

(check-expect (create (list (list 5 "t") (list 6 "r") (list 4 "q")))
              (make-node 4 "q" '() (make-node 6 "r" (make-node 5 "t" '() '()) '())))
(check-expect (create '((1 a) (18 b) (2 g)))
(make-node 2 'g (make-node 1 'a '() '()) (make-node 18 'b '() '())))
;; ***************************************************
;; (search-bst key bst) produces the associated value to a consumed key in the consumed bst
;; search-bst: Num BinarySearchTree -> (anyof false Str)
(define (search-bst key bst)
  (cond [(empty? bst) false]
         [(= key (node-ssn bst)) (node-name bst)]
         [(> key (node-ssn bst)) (search-bst key (node-right bst))]
         [else (search-bst key (node-left bst))]))

(check-expect (search-bst 290 tree)"t")
(check-expect (search-bst 500 tree)false)
(check-expect (search-bst 220 tree)"y")
;; ***************************************************
;; (depth tree) finds the maximum length of the branches of the BST consumed
;; depth: BinarySearchTree -> Nat
(define (depth tree)
  (cond [(empty? tree) 0]
        [else (+ 1 (max (depth (node-right tree)) (depth (node-left tree))))]))

(check-expect (depth tree)4)
;; ***************************************************
;; (find-max bst) consumes a bst and produces the biggest ssn 
;; find-max:  BinarySearchTree-> Nat
(define (find-max bst)
  (cond
    [(empty? bst) empty]
    [(and (empty? (node-left bst)) (empty? (node-right bst)))
     (node-ssn bst)]
    [else (find-max (node-right bst))]))

(check-expect (find-max tree)290)
;; ***************************************************
;; (find-min bst) consumes a BST and produces the smallest ssn
;; find-max: BinarySearchTree-> Nat
(define (find-min bst)
  (cond
    [(empty? bst) empty]
    [(and (empty? (node-left bst)) (empty? (node-right bst)))
     (node-ssn bst)]
    [else (find-min (node-left bst))]))

(check-expect (find-min tree) 165)
;; ***************************************************
;; (verify-bst tree) consumes a tree and checks whether it's a valid binary search tree or not
;; verify: BinarySearchTree -> Bool 
(define (verify-bst tree)
  (local [(define (check-branch tree branch)
            (cond
              [(and (< (node-ssn tree) (node-ssn (node-right tree)))
           (< (node-ssn tree) (find-min (node-right tree))))
               (and (verify-bst (node-right branch)) (verify-bst (node-left branch)))]
              [else false]))]

  (cond
    [(empty? tree)true]
    [(and (empty? (node-right tree)) (empty? (node-left tree)))true]
    [(empty? (node-right tree)) (check-branch tree (node-left tree))]
    [(empty? (node-left tree)) (check-branch tree (node-right tree))]
    [ else (and (check-branch tree (node-left tree)) (check-branch tree (node-right tree)))])))

(check-expect (verify-bst tree)true)
(check-expect (verify-bst empty) true)
(check-expect (verify-bst (make-node 2 "r" (make-node -11 "s" empty empty)
                                     (make-node 21 "t" (make-node 1 "r" empty empty)
                                                (make-node 2 "r" empty empty))))false)
;; ***************************************************
;; (produce-path node tree) produces a list of nodes that can be travelled through from the root of
;;   the consumed tree to the consumed node
;; produce-path: Num BinarySearchTree -> (anyof false (listof Nodes))
(define (produce-path key tree)
  (cond
    [(false? (search-bst key tree)) false]
    [(= key (node-ssn tree)) empty]
    [(> key (node-ssn tree)) (cons (node-ssn tree)
                                               (produce-path key (node-right tree)))]
    [else (cons (node-ssn tree) (produce-path key (node-left tree)))]))
(check-expect (produce-path  165 tree) (list 200 175))
(check-expect (produce-path 230 tree) (list 200 250 225))
(check-expect (produce-path 500 tree)false)
;; ***************************************************
;; (triangle-area Triangle) uses heron's formula to check whether the three points are collinear or not.
;;     If they're not collinear, it computes the area of the triangle made by the three points.
;;        Collinearity is checked by using the concept that the area between three points on a line is zero.
;; triangle-area: Struct -> Nat
;; Examples: 
(check-expect (triangle-area Triangle2 )false)

(define (triangle-area Triangle)
  ;;(heronformula point1 point2 point3) calculates the area of a triangle made by three points by taking input
    ;;the coordinates of those three points and using heron's formula 
  (local [(define (heronformula point1 point2 point3)
  (* 0.5 (abs (-(* (- (posn-x point1) (posn-x point3))
             (- (posn-y point2) (posn-y point1)))
           (*(- (posn-x point1) (posn-x point2))
            (- (posn-y point3) (posn-y point1)))))))]
  (cond
    [ (not (= 0
    (heronformula (triangle-point1 Triangle) (triangle-point2 Triangle) (triangle-point3 Triangle))))
      (heronformula (triangle-point1 Triangle) (triangle-point2 Triangle) (triangle-point3 Triangle))] 
    [ else false])))
  
;; Tests:
(check-expect (triangle-area Triangle1)false)
(check-expect (triangle-area Triangle3) 3)

;; ***************************************************
;; (digits->nat list) consumes a  list of numbers and produces a natural number where the first
;;   digit in the consumed list becomes the least significant digit in the produced number
;;    and the last digit becomes the most significant digit
;; digits->nat: (listof Nat) -> Nat
;; requires that numbers in the list is between 0 and 9 inclusive
;; Examples: 
(check-expect (digits->nat (list 1 2 0 0 0))21)
(check-expect (digits->nat (list 5 6 0 8 9 0))98065)
(check-expect (digits->nat empty) 0)

(define (digits->nat list)
  (local [(define (expt-increase list n)
(cond
  [ (empty? list) 0] 
  [ else (+ (expt-increase (rest list) (+ 1 n)) (* (first list)(expt 10 n)))]))]

  (expt-increase list 0)))

;; Tests:
(check-expect (digits->nat (list 0 0 0 0)) 0)
(check-expect (digits->nat empty) 0)
(check-expect (digits->nat '(1)) 1)
(check-expect (digits->nat (list 1 2 3))321)