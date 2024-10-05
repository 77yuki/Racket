;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 04, Problem 2
;; *********************************************
;;


;;
;; Problem 2(a)
;;
;; (euclidean-norm lst) produces the Euclidean Norm of a vector
;;   according to lst, which is a list of numbers of arbitrary length
;; Examples:
(check-within (euclidean-norm (list 3 4)) 5 0.01)

;; euclidean-norm: (listof Num) -> Num
(define (euclidean-norm lst)
  (sqrt (sum-of-squares lst)))

;; Tests:
(check-within (euclidean-norm (list -1 0 7)) 7.07 0.01)
(check-within (euclidean-norm (list 3.2 1 4.2 -2)) 5.73 0.01)


;; (sum-of-squares lst) produces the sum of the squares of each
;;   element in lst
;; Examples:
(check-expect (sum-of-squares (list 3 4)) 25)

;; sum-of-squares: (listof Num) -> Num
(define (sum-of-squares lst)
  (cond [(empty? lst) 0]
        [else (+ (sqr (first lst)) (sum-of-squares (rest lst)))]))


;;
;; Problem 2(b)
;;
;; (unit-vector lst) produces aligned unit vector of a given vector
;; Examples:
(check-within (unit-vector (list 3 4)) (list 0.6 0.8) 0.01)

;; unit-vector: (listof Num) -> (listof Num)
;; Requires:
;; lst is not an empty list, which means that v will not be a zero vector
(define (unit-vector lst)
  (unit-vector-sum lst (euclidean-norm lst)))

;; Tests:
(check-within (unit-vector (list -1 0 7)) (list -0.14 0 0.99) 0.01)
(check-within (unit-vector (list 3.2 1 4.2 -2))
              (list 0.56 0.17 0.73 -0.35) 0.01)


;; (unit-vector-sum lst e-norm) produces aligned unit vector of
;;   a given vector
;; Examples:
(check-within (unit-vector-sum (list 3 4) 5) (list 0.6 0.8) 0.01)

;; unit-vector-sum: (listof Num) Num -> (listof Num)
(define (unit-vector-sum lst e-norm)
  (cond [(empty? lst) empty]
        [(cons? lst) (cons (/ (first lst) e-norm)
                    (unit-vector-sum (rest lst) e-norm))]))


;;
;; Problem 2(c)
;;
;; (cos-between lst1 lst2) produces the cosine of the angle between a(lst1)
;;   and b(lst2)
;; Examples:
(check-within (cos-between (list 3 4) (list 0 6)) 0.8 0.01)

;; cos-between: (listof Num) (listof Num) -> Num
;; Requires:
;; lst1 and lst2 are non-empty lists and they have the same length
(define (cos-between lst1 lst2)
  (cos-between-unit (unit-vector lst1) (unit-vector lst2)))

;; Tests:
(check-within (cos-between (list -1 0 7) (list 3.2 1 -2)) -0.62 0.01)


;; (cos-between-unit lst1 lst2) produces the cosine of the angle between
;;   a and b
;; Examples:
(check-within (cos-between (list 0.6 0.8) (list 0 1)) 0.8 0.01)

;; cos-between-unit: (listof Num) (listof Num) -> Num
(define (cos-between-unit lst1 lst2)
  (cond [(empty? lst1) 0]
        [(cons?  lst1) (+ (* (first lst1)
                            (first lst2))
                          (cos-between-unit (rest lst1) (rest lst2)))]))


