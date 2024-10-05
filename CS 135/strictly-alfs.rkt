;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname strictly-alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 08, Problem 3
;; ************************************
;;


;;
;; a)
;;
;; (occurrences num lst) produces the number of times that num occurs
;;   in the lst
;; Examples:
(check-expect (occurrences 3 '(1 2 4 6 2 3 5 3 3 3 0)) 4)
(check-expect (occurrences 2 '(3 1 5 2)) 1)

;; occurrences: Num (listof Num) -> Nat
(define (occurrences num lst)
  (length (filter (lambda (n) (= n num)) lst)))

;; Tests:
(check-expect (occurrences 10 '(2 3 5 1)) 0)
(check-expect (occurrences 0.1 '(0.1 20 3.1 0.1 0.1 4)) 3)
(check-expect (occurrences -2.3 '(-1.2 -2.3 2.3 -2.3 50)) 2)


;;
;; b)
;;
;; (zip lst1 lst2) produces a list of pairs (two elements lists) where
;;   the ith contains the ith element of lst1 followed by the ith
;;   element of lst2
;; Examples:
(check-expect (zip '(1 3 5) '(2 4 6)) '((1 2) (3 4) (5 6)))
(check-expect (zip '(a b c) '(0 3 8)) '((a 0) (b 3) (c 8)))

;; zip: (listof X) (listof Y) -> (listof (list X Y))
;; Requires:
;; the length of lst1 and lst2 is the same
(define (zip lst1 lst2)
  (map (lambda (x y) (list x y)) lst1 lst2))

;; Tests:
(check-expect (zip '(2 a /) '(waterloo 10 2)) '((2 waterloo) (a 10) (/ 2)))
(check-expect (zip '() '()) '())


;;
;; c)
;;
;; (unzip lst) produces a list of two lists - the first list contains
;;   the first element from each pair of lst, and the second list contains
;;   the second element from each pair of lst, in the original order.
;; Examples:
(check-expect (unzip '((1 2) (3 4) (5 6))) '((1 3 5) (2 4 6)))
(check-expect (unzip '((b a) (c e) (9 2))) '((b c 9) (a e 2)))

;; unzip: (listof (list X Y)) -> (list (listof X) (listof Y))
(define (unzip lst)
  (list (foldr (lambda (v l) (cons (first v) l)) '() lst)
        (foldr (lambda (v l) (cons (second v) l)) '() lst)))

;; Tests:
(check-expect (unzip '((b a) (3 m) (c /))) '((b 3 c) (a m /)))
(check-expect (unzip '()) '(()()))


;;
;; d)
;;
;; (subsequence lst from to) produces subsequence from lst that begins
;;   at index from and ends just before index to
;; Examples:
(check-expect (subsequence '(a b c d) 1 4) '(b c d))
(check-expect (subsequence '(waterloo waterloo great) 0 99)
              '(waterloo waterloo great))

;; subsequence: (listof X) Nat Nat -> (listof X)
;; Requires: 0 <= from <= to
(define (subsequence lst from to)
    (filter (lambda (x) (not (empty? x)))
            (map (lambda (a y) (cond [(and (< (- from 1) y) (< y to)) a]
                                     [else empty]))
                 lst (build-list (length lst) (lambda (x) x)))))

;; Tests:
(check-expect (subsequence '(water water water loo loo loo) 0 6)
              '(water water water loo loo loo))
(check-expect (subsequence '(a b 5 2) 1 1) '())
(check-expect (subsequence '(2 54 6) 2 3) '(6))
(check-expect (subsequence '() 0 10) '())





