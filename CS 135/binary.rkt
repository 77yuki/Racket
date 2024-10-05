;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binary) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Final, Problem 4 
;; ***************************************************
;;


(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; Requires: key > every key in left BST
;;           key < every key in right BST


;; A Binary Search Tree (BST) is one of
;; * empty
;; * Node



;; Examples:
(check-expect (sorted-list->bst empty) empty)
(check-expect (sorted-list->bst '(1 2 3 4 5))
              (make-node 3
                         (make-node 2
                                    (make-node 1 empty empty)
                                    empty)
                         (make-node 5 (make-node 4 empty empty)
                                    empty)))

;; sorted-list->bst: (listof Nat) -> BST
;; Requires: lst is sorted in strictly increasing order, with no duplicates 
(define (sorted-list->bst lst)
  (cond [(empty? lst) empty]
        [else
         (local [;; find-midpoint: (listof Num) Nat -> Num
                 (define (find-midpoint lon n)
                   (cond [(odd? (length lon))
                          (find-midpoint-odd lon n)]
                         [else (find-midpoint-even lon n)]))

                 ;; find-midpoint-odd: (listof Num) Nat -> Num
                 ;; Requires: the length of lon is odd
                 (define (find-midpoint-odd lon n)
                   (cond [(= n 1) (first lon)]
                         [else (find-midpoint-odd (rest lon) (- n 1))]))

                 ;; find-midpoint-even: (listof Num) Nat -> Num
                 ;; Requires: the length of lon is even
                 (define (find-midpoint-even lon n)
                   (cond [(= n 0) (first lon)]
                         [else (find-midpoint-even (rest lon) (- n 1))]))
 
                 (define mid-point
                   (find-midpoint lst (ceiling (/ (length lst) 2))))

                 ;; find-left-side: (listof Num) Num -> (listof Num)
                 (define (find-left-side lon n)
                   (cond [(empty? lon) empty]
                         [(= n (first lon)) empty]
                         [else
                          (cons (first lon) (find-left-side (rest lon) n))]))
                 
                 (define left-side (find-left-side lst mid-point))

                 ;; find-right-side: (listof Num) Num -> (listof Num)
                 (define (find-right-side lon n)
                   (cond [(empty? lon) empty]
                         [(or (> n (first lon))
                              (= n (first lon)))
                          (append empty (find-right-side (rest lon) n))]
                         [else
                          (cons (first lon) (find-left-side (rest lon) n))]))
                 
                 (define right-side (find-right-side lst mid-point))]
           
           (make-node mid-point
                    (sorted-list->bst left-side)
                    (sorted-list->bst right-side)))]))

;; Tests:
(check-expect (sorted-list->bst '(0 1 3 4 8))
              (make-node 3
                         (make-node 1
                                    (make-node 0 empty empty)
                                    empty)
                         (make-node 8 (make-node 4 empty empty)
                                    empty)))
(check-expect (sorted-list->bst '(3 9 10 39))
              (make-node 10
                         (make-node 9
                                    (make-node 3 empty empty)
                                    empty)
                         (make-node 39 empty empty)))
(check-expect (sorted-list->bst '(2 4 5 8 10 12 13 16))
              (make-node 10
                         (make-node 5
                                    (make-node 4
                                               (make-node 2 empty empty)
                                               empty)
                                    (make-node 8 empty empty))
                         (make-node 13
                                    (make-node 12 empty empty)
                                    (make-node 16 empty empty))))
(check-expect (sorted-list->bst '(2 23 32))
              (make-node 23
                         (make-node 2 empty empty)
                         (make-node 32 empty empty)))

