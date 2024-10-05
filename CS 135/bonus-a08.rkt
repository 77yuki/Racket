;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus-a08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 08, Bonus
;; ************************************
;;


;;
;; a)
;;
;; (subsets1 lst) produces a list of all of subsets of lst
;; Examples:
(check-expect (subsets1 '(1 2)) (list '(1 2) '(1) '(2) '()))

;; subsets: (listof Num) -> (listof (listof Num))
;; Requires: lst does not contain any duplicates
(define (subsets1 lst)
  (local [(define l (build-list (expt 2 (length lst)) (lambda (x) (x))))]
    (map )))
