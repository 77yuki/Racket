;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname factors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 03, Problem 6
;; ************************************
;;


;;
;; Problem 6(a)
;;
;; (all-factors n) produces a list of all natural numbers x
;;   where 0 < x < n and x divides n evenly
;; Examples:
(check-expect
 (all-factors 30)
 (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))

;; all-factors: Nat -> (listof Nat)
;; Requires:
;; n > 0
(define (all-factors n)
  (factors n 0))

;; Tests:
(check-expect (all-factors 1) empty)
(check-expect (all-factors 2) (cons 1 empty))
(check-expect (all-factors 11) (cons 1 empty))


;; (factors a b) produces a list of all natural numbers x
;;   where 0 < x < a and x divides a evenly
;; Examples:
(check-expect
 (factors 30 0)
 (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))
(check-expect (factors 1 0) empty)

;; factors: Nat Nat -> (listof Nat)
(define (factors a b)
  (cond [(= a (add1 b)) empty]
        [(= (remainder a (add1 b)) 0)
         (cons (add1 b) (factors a (add1 b)))]
        [else (factors a (add1 b))]))


;;
;; Problem 6(b)
;;
;; (is-prime? n) produces true if n is prime, and false otherwise
;; Examples:
(check-expect (is-prime? 30) false)
(check-expect (is-prime? 7) true)

;; is-prime?: Nat -> Bool
;; Requires:
;; n > 0
(define (is-prime? n)
  (cond [(= (length (all-factors n)) 1) true]
        [else false]))

;; Tests:
(check-expect (is-prime? 1) false)
(check-expect (is-prime? 9) false)
(check-expect (is-prime? 31) true)


;;
;; Problem 6(c)
;;
;; (is-composite? n) produces true if n is a composite number,
;;   and false otherwise
;; Examples:
(check-expect (is-composite? 30) true)
(check-expect (is-composite? 7) false)

;; is-composite?: Nat -> Bool
;; Requires:
;; n > 0
(define (is-composite? n)
  (cond [(> (length (all-factors n)) 1) true]
        [else false]))

;; Tests:
(check-expect (is-composite? 1) false)
(check-expect (is-composite? 8) true)