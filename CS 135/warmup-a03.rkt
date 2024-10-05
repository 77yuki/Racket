;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 03, Problem 1
;; ************************************
;;

;;
;; Problem 1(a)
;;
;; (replace-word first-string second-string lst) produces a new list
;;   where all occurrences of the first-string in lst have been
;;   replaced by the second-string
;; Examples:
(check-expect
 (replace-word "exam" "assessment"
               (cons "content"
                     (cons "exam"
                           (cons "assignment" empty))))
 (cons "content" (cons "assessment" (cons "assignment" empty))))

;; replace-word: Str Str (listof Str) -> (listof Str)
(define (replace-word first-string second-string lst)
  (cond
    [(empty? lst) empty]
    [(string=? (first lst) first-string)
     (cons second-string (replace-word first-string second-string
                                       (rest lst)))]
    [else (cons (first lst) (replace-word first-string second-string
                                          (rest lst)))]))

;; Tests:
(check-expect
 (replace-word "apple" "pear"
               (cons "apple"
                     (cons "banana"
                           (cons "apple" empty))))
 (cons "pear" (cons "banana" (cons "pear" empty))))
(check-expect
 (replace-word "train" "plane" empty) empty)


;;
;; Problem 1(b)
;;
;; (add Num1 Num2) produces the sum of Num1 and Num2
;; Examples:
(check-expect (add 2 3) 5)

;; add: Nat Nat -> Nat
(define (add Num1 Num2)
  (cond [(zero? Num2) Num1]
        [else (add (add1 Num1) (sub1 Num2))]))

;; Tests:
(check-expect (add 15 0) 15)
(check-expect (add 0 3) 3)


;;
;; Problem 1(c)
;;
;; (mult Num1 Num2) produces the product of Num1 and Num2
;; Examples:
(check-expect (mult 2 3) 6)

;; mult: Nat Nat -> Nat
(define (mult Num1 Num2)
  (mult-constant Num1 Num1 Num2))

;; Tests:
(check-expect (mult 3 0) 0)
(check-expect (mult 0 12) 0)
(check-expect (mult 9 14) 126)


;; (mult-constant constant Num1 Num2) produces the product of Num1 and Num2
;; Examples:
(check-expect (mult-constant 2 2 3) 6)

;; mult-constant: Nat Nat Nat -> Nat
(define (mult-constant constant Num1 Num2)
  (cond [(zero? Num2) 0]
        [(= Num2 1) Num1]
        [(> (sub1 Num2) 0) (mult-constant constant (add Num1 constant) (sub1 Num2))]))


