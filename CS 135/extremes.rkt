;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname extremes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 02, Problem 2
;; ************************************
;;

;;
;; Problem 2(a)
;;
;; (smallest lst) produces the smallest number in the given list
;; Examples:
(check-expect (smallest (cons -5 (cons 2 (cons 10.5 empty)))) -5)

;; smallest: (listof Num) -> Num
;; Requires:
;; the given list is not empty
(define (smallest lst)
  (cond
    [(= (length lst) 1) (first lst)]
    [(> (first lst) (first (rest lst))) (smallest (rest lst))]
    [else (smallest (cons (first lst) (rest (rest lst))))]))

;; Tests
(check-expect (smallest (cons 3 (cons -11 (cons -11 empty)))) -11)
(check-expect (smallest (cons 9 empty)) 9)
(check-expect
 (smallest (cons 0.5 (cons 3.5 (cons 3.1 (cons 2 (cons 0.5 empty))))))
 0.5)

;;
;; Problem 2(b)
;;
;; (largest lst) produces the largest number in the given list
;; Examples:
(check-expect (largest (cons -5 (cons 2 (cons 10.5 empty)))) 10.5)
(check-expect (largest (cons 4 (cons -7 (cons 0 empty)))) 4)

;; largest: (listof Num) -> Num
;; Requires:
;; the given list is not empty
(define (largest lst)
  (cond
    [(= (length lst) 1) (first lst)]
    [(> (first lst) (first (rest lst)))
     (largest (cons (first lst) (rest (rest lst))))]
    [else (largest (rest lst))]))

;; Tests:
(check-expect (largest (cons 3 (cons 3.5 (cons 3.5 empty)))) 3.5)
(check-expect (largest (cons 10000 (cons 100 (cons -2 empty)))) 10000)
(check-expect (largest (cons 1 empty)) 1)


;;
;; Problem 2(c)
;;
;; (max-diff lst) produces the largest difference between
;;   any two elements in the given list
;; Examples:
(check-expect (max-diff (cons -5 (cons 2 (cons 10.5 empty)))) 15.5)
(check-expect (max-diff (cons 3 empty)) 0)

;; max-diff: (listof Num) -> Num
;; Requires:
;; the given list is not empty
(define (max-diff lst)
  (- (largest lst) (smallest lst)))

;; Tests:
(check-expect (max-diff (cons 2 (cons -14.3 (cons 5 (cons -20 empty))))) 25)
(check-expect (max-diff (cons -3 (cons -7 empty))) 4)
(check-expect (max-diff (cons 4.5 (cons 4.5 empty))) 0)