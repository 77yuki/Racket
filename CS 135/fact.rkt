;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fact) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 01, Problem 2
;; *********************************************
;;


;; Examples:
(check-expect (factorialize (cons 1 (cons 2 (cons 4 (cons 3 empty)))))
              (cons 1 (cons 2 (cons 24 (cons 6 empty)))))

;; factorialize: (listof Nat) -> (listof Nat)
(define (factorialize lst)
  (cond [(empty? lst) empty]
        [else (cons (factorialize-nat (first lst))
                    (factorialize (rest lst)))]))

;; Tests:
(check-expect (factorialize empty) empty)
(check-expect (factorialize (cons 7 (cons 0 empty)))
              (cons 5040 (cons 1 empty)))


;; Examples:
(check-expect (factorialize-nat 4) 24)

;; factorialize-nat: Nat -> Nat
(define (factorialize-nat n)
  (cond [(= 0 n) 1]
        [else (* n (factorialize-nat (- n 1)))]))

;; Tests:
(check-expect (factorialize-nat 0) 1)
(check-expect (factorialize-nat 1) 1)
