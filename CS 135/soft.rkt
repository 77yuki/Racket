;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname soft) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 01, Problem 5
;; *********************************************
;;


;; commonly-used helper functions that are used to define test-lists
(define test0 (cons 1.0 (cons 2.0 (cons -1.0 (cons 3.0 empty)))))
(define test1 (cons 4.2 (cons 1.7 empty)))
(define test2 (cons -3.2 (cons 0 empty)))


;;
;; Problem 5(a)
;;
;; Examples:
(check-within (compute-norm test0) 30.56 0.01)

;; compute-norm: (listof Num) -> Num
;; Requires:
;; lst is a non-empty list
(define (compute-norm lst)
  (cond [(empty? lst) 0]
        [else (+ (exp (first lst)) (compute-norm (rest lst)))]))

;; Tests:
(check-within (compute-norm test1) 72.16 0.01)
(check-within (compute-norm test2) 1.04 0.01)


;;
;; Problem 5(b)
;;
;; Examples:
(check-within (first (apply-norm 30.56 test0)) 0.09 0.01)
(check-within (second (apply-norm 30.56 test0)) 0.24 0.01)

;; apply-norm: Num (listof Num) -> (listof Num)
(define (apply-norm norm lst)
  (cond [(empty? lst) empty]
        [else (cons (/ (exp (first lst)) norm)
                    (apply-norm norm (rest lst)))]))

;; Tests:
(check-within (first (apply-norm 72.16 test1)) 0.92 0.01)
(check-within (second (apply-norm 1.04 test2)) 0.96 0.01)


;;
;; Problem 5(c)
;;
;; Examples:
(check-within (first (softmax test0)) 0.09 0.01)
(check-within (first (rest (softmax test0))) 0.24 0.01)

;; softmax: (listof Num) -> (listof Num)
(define (softmax lst)
  (apply-norm (compute-norm lst) lst))

;; Tests:
(check-within (first (softmax test1)) 0.92 0.01)
(check-within (first (rest (softmax test2))) 0.96 0.01)
