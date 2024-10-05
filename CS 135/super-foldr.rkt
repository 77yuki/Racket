;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super-foldr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 08, Problem 5
;; ************************************
;;


;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))


;;
;; a)
;;
;; (super-foldr combine base nested-lst) generalizes foldr to work on nested
;;   lists of non-list elements, folding each nested list with the given
;;   function to produce the result to be used in the parent list's fold
;; Examples:
(check-expect (super-foldr + 0 '(1 (5 5 (1 3)) (10 2) 2)) 29)
(check-expect (super-foldr - 0 '(1 (5 5 (1 3)) (10 2) 2)) 9)

;; super-foldr: (((anyof X (listof X)) Y) -> Y) Y (nested-listof X) -> Y
(define (super-foldr combine base nested-lst)
  (cond [(empty? nested-lst) base]
        [else
         (cond [(not (list? (first nested-lst)))
                (combine (first nested-lst)
                         (super-foldr combine base (rest nested-lst)))]
               [else (combine
                      (super-foldr combine base (first nested-lst))
                      (super-foldr combine base (rest nested-lst)))])]))

;; Tests:
(check-expect (super-foldr + 5 '((()))) 15)
(check-expect (super-foldr cons empty '(5 (4 3) (2 1) 0)) '(5 (4 3) (2 1) 0))
(check-expect (super-foldr + 0 (list 23 empty 0)) 23)


;;
;; b)
;;
;; (magnitudes nl) produces the sum of all the absolute values of the
;;   numbers in nl
;; Examples:
(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2) 2)) 29)
(check-expect (magnitudes '()) 0)

;; magnitudes: (nested-listof Num) -> Num
(define (magnitudes nl)
  (super-foldr (lambda (x y)
                 (cond [(< x 0) (- y x)]
                       [else (+ x y)])) 0 nl))

;; Tests:
(check-expect (magnitudes '(3 25 (0.2 -1.3) -2 (-10))) 41.5)
(check-expect (magnitudes '(0 2 -8)) 10)


;;
;; c)
;;
;; (super-filter pred? lst) keeps all the items on lst that satisfy pred?
;; Examples:
(check-expect (super-filter even? (list 5 (list 1 2 3 (list 3 6) 7) 9 10))
              (list (list 2 (list 6)) 10))
(check-expect (super-filter odd? (list 3 0 5))
              (list 3 5))

;; super-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-filter pred? lst)
  (super-foldr (lambda (x y)
                 (cond [(not (list? x))
                        (cond [(pred? x) (cons x y)]
                              [else y])]
                       [else (cons x y)])) empty lst))

;; Tests:
(check-expect (super-filter even? empty) empty)
(check-expect (super-filter char? '(1 (#\3 4) 2 3 (#\a b) 2))
              '((#\3) (#\a)))
(check-expect (super-filter (lambda (x) (< x 29)) '(1 (2 48) 23 29 (3) 2))
              '(1 (2) 23 (3) 2))



