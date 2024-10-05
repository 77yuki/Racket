;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname translations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 01, Problem 2
;; ************************************
;;


;;
;; Problem 2(a)
;;
;; (volume r) produces the volume of a sphere
;; Examples:
(check-within (volume 1) 4.18 0.01)
(check-within (volume 4) 268.08 0.01)

;; volume: Num -> Num
;; requires: r > 0
(define (volume r) (* (/ 4 3) pi (* r r r)))

;; Tests:
(check-within (volume 1.2) 7.23 0.01)
(check-within (volume 22/7) 130.03 0.01)


;;
;; Problem 2(b)
;;
(define phi (/ (+ 1 (sqrt 5)) 2))
;; (fib n) computes Fibonacci numbers 
;; Examples:
(check-within (fib 2) 1 0.01)
(check-within (fib 7) 13 0.01)

;; fib: Int -> Num
;; requires: n > 0
(define (fib n) (/ (- (expt phi n) (expt (- phi) (- n)))
                   (- (* 2 phi) 1)))
;; Tests:
(check-within (fib 13) 233 0.01)
(check-within (fib 1) 1 0.01)