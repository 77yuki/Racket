;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname division) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 03, Problem 5
;; ************************************
;;


;; (divide a b) produces a list of exactly two numbers, which includes
;;   the quotient and the remainder when a is divided by b
;; Examples:
(check-expect (divide 17 5) (cons 3 (cons 2 empty)))

;; divide: Nat Nat -> (listof Nat)
;; Requires:
;; b > 0
(define (divide a b)
  (cons (get-quotient a b) (cons (get-remainder a b) empty)))

;; Tests:
(check-expect (divide 0 1) (cons 0 (cons 0 empty)))
(check-expect (divide 18 2) (cons 9 (cons 0 empty)))
(check-expect (divide 2 9) (cons 0 (cons 2 empty)))


;; (get-quotient a b) produces the quotient when a is divided by b
;; Examples:
(check-expect (get-quotient 17 5) 3)
(check-expect (get-quotient 0 3) 0)

;; get-quotient: Nat Nat -> Nat
;; Requires:
;; b > 0
(define (get-quotient a b)
  (cond [(< a b) 0]
        [else (add1 (get-quotient (- a b) b))]))


;; (get-remainder a b) produces the remainder when a is divided by b
;; Examples:
(check-expect (get-remainder 17 5) 2)
(check-expect (get-remainder 0 19) 0)

;; get-remainder: Nat Nat -> Nat
;; Requires:
;; b > 0
(define (get-remainder a b)
  (cond [(< a b) a]
        [else (get-remainder (- a b) b)]))
