;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname crl-points) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 01, Problem 4
;; ************************************
;;


;; (crl-points first second third level-of-violation)computes
;;   the number of points for a given competitor in the tour
;; Examples:
(check-expect (crl-points 0 2 4 'minor-violation) 90)
(check-expect (crl-points 1 1 1 'major-violation) 60)

;; crl-points: Nat Nat Nat Sym -> Nat
(define (crl-points first second third level-of-violation)
  (cond
    [(and (symbol=? level-of-violation 'minor-violation)
     (>= (+ first second third) 5))
     (floor (* (+ (* 50 first) (* 20 second) (* 10 third) 15) 0.95))]
    [(and (symbol=? level-of-violation 'minor-violation)
     (< (+ first second third) 5))
     (floor (* (+ (* 50 first) (* 20 second) (* 10 third)) 0.95))]
    [(and (symbol=? level-of-violation 'major-violation)
     (>= (+ first second third) 5))
     (floor (* (+ (* 50 first) (* 20 second) (* 10 third) 15) 0.75))]
    [(and (symbol=? level-of-violation 'major-violation)
     (< (+ first second third) 5))
     (floor (* (+ (* 50 first) (* 20 second) (* 10 third)) 0.75))]
    [(and (symbol=? level-of-violation 'good-standing)
     (>= (+ first second third) 5))
     (+ (* 50 first) (* 20 second) (* 10 third) 15)]
    [(and (symbol=? level-of-violation 'good-standing)
     (< (+ first second third) 5))
     (+ (* 50 first) (* 20 second) (* 10 third))]
    [else 0]))

;; Tests:
(check-expect (crl-points 2 4 5 'good-standing) 245)
(check-expect (crl-points 1 3 7 'disqualified) 0)
(check-expect (crl-points 1 1 2 'minor-violation) 85)
(check-expect (crl-points 3 1 4 'major-violation) 168)
(check-expect (crl-points 2 1 0 'good-standing) 120)
(check-expect (crl-points 0 1 2 'disqualified) 0)
  