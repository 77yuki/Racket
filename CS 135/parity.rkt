;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname parity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 02, Problem 5
;; ************************************
;;


;; (parity str) determines the number of 1's is odd or even in the
;;   given string
;; Examples:
(check-expect (parity "110101") 'even)
(check-expect (parity "1110011") 'odd)

;; parity: Str -> Sym
(define (parity str)
  (cond
    [(odd? (number-of-1 str)) 'odd]
    [(even? (number-of-1 str)) 'even]))

;; Tests:
(check-expect (parity "0000") 'even)
(check-expect (parity "1") 'odd)


;; (number-of-1 str) counts the number of occurrences of 1's
;;   in the given string
;; Examples:
(check-expect (number-of-1 "110101") 4)
(check-expect (number-of-1 "1110011") 5)

;; number-of-1: Str -> Nat
(define (number-of-1 str)
  (number-of-1/list (string->list str)))


;; (number-of-1/list lst) counts the number of occurrences of 1's
;;   in the given list
;; Examples:
(check-expect
 (number-of-1/list
  (cons #\1 (cons #\1 (cons #\0 (cons #\1 (cons #\0 (cons #\1 empty))))))) 4)

;; number-of-1/list: (listof Nat) -> Nat
(define (number-of-1/list lst)
  (cond
    [(empty? lst) 0]
    [else (+ (cond [(char=? #\1 (first lst)) 1]
             [else 0])
             (number-of-1/list (rest lst)))]))




