;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Midterm 02, Problem 3
;; ************************************
;;


(define original 0)


;; Examples:
(check-expect (relative->absolute '(4 7 1)) '(4 11 12))
(check-expect (relative->absolute empty) empty)

;; relative->absolute: (listof Int) -> (listof Int)
(define (relative->absolute lst)
  (add-lst lst original))

;; Tests:
(check-expect (relative->absolute '(5 -2 6)) '(5 3 9))
(check-expect (relative->absolute '(1 -10 2 20 -6)) '(1 -9 -7 13 7))
(check-expect (relative->absolute '(200)) '(200))
(check-expect (relative->absolute '(10 -5)) '(10 5))
(check-expect (relative->absolute '(5 7 5 5 -20 5)) '(5 12 17 22 2 7))


;; Examples:
(check-expect (add-lst '(4 7 1 4) original) '(4 11 12 16))
(check-expect (add-lst '(1) original) '(1))
(check-expect (add-lst '(9 10) original) '(9 19))
(check-expect (add-lst empty original) empty)

;; add-lst: (listof Int) Nat -> (listof Int)
(define (add-lst lst num)
  (cond [(empty? lst) empty]
        [else (cons (+ num (first lst))
                    (add-lst (rest lst) (+ num (first lst))))]))