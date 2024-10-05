;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alternate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 03, Problem 4
;; ************************************
;;


;;
;; Problem 4(a)
;;
;; alt-nat-template: Nat -> Any
(define (alt-nat-template n)
  (cond [(zero? n) ...]
        [(even? n) (... n ...
                    ... (alt-nat-template (/ n 2)) ...)]
        [(odd? n) (... n ...
                   ... (alt-nat-template (/ (sub1 n) 2)) ...)]))


;;
;; Problem 4(b)
;;
;; (powers-of-2 n) determines how many times n can be divided by 2
;;   before reaching 0 or an odd number
;; Examples:
(check-expect (powers-of-2 12) 2)
(check-expect (powers-of-2 0) 0)

;; powers-of-2: Nat -> Nat
(define (powers-of-2 n)
  (cond [(zero? n) 0]
        [(even? n) (+ 1 (powers-of-2 (/ n 2)))]
        [(odd? n) 0]))

;; Tests:
(check-expect (powers-of-2 7) 0)
(check-expect (powers-of-2 16) 4)
