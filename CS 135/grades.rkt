;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 02, Problem 3
;; ************************************
;;


;; (cs135-grade sc assgn mt1 mt2 fe) produces the final grade received
;;   in CS135 out of 100
;; Examples:
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0.95) 68.3)
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0) 46)

;; cs135-grade: Num Num Num Num Num -> Num
(define (cs135-grade sc assgn mt1 mt2 fe)
  (cond
    [(or (< (* (/ (+ (* mt1 0.07) (* mt2 0.07) (* fe 0.16)) 0.3) 100) 50)
         (< (* assgn 100) 50))
     (minimum (* (+ (* sc 0.1) (* assgn 0.6) (* mt1 0.07) (* mt2 0.07)
                (* fe 0.16))100) 46)]
    [else (* (+ (* sc 0.1) (* assgn 0.6) (* mt1 0.07) (* mt2 0.07)
                (* fe 0.16))100)]))

;; Tests:
(check-expect (cs135-grade 0.4 0.3 0.2 0.5 0) 26.9)
(check-expect (cs135-grade 0.7 0.5 0.2 0.4 0.8) 54)
(check-expect (cs135-grade 0.5 0.4 0.6 0.8 0.9) 46)


;; (minimum a b) produces the smallest number between
;;   these two given numbers
;; Examples:
(check-expect (minimum 24.3 78.5) 24.3)
(check-expect (minimum -34 0) -34)

;; minimum: Num Num -> Num
(define (minimum a b)
  (cond [(< a b) a] 
        [else b]))