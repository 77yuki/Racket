;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname realative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 03, Problem 3
;; ************************************
;;


;; (differences lst) produces a list of the differences between each
;;   number and the number before it
;; Examples:
(check-expect (differences (cons 4 (cons 7 (cons 1 empty))))
              (cons 3 (cons -6 empty)))

;; differences: (listof Num) -> (listof Num)
;; Requires:
;;   lst is a non-empty list
(define (differences lst)
  (cond [(= (length lst) 1) empty]
        [else(cons(- (first (rest lst)) (first lst))
                  (differences (rest lst)))]))

;; Tests:
(check-expect (differences (cons 7.2 (cons 3.8 empty)))
              (cons -3.4 empty))
(check-expect (differences (cons 399 empty)) empty)
(check-expect (differences (cons 0 (cons -3 empty))) (cons -3 empty))
