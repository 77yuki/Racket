;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname stats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 02, Problem 7
;; *********************************************
;;


;; Examples:
(check-expect (median (list 2 2 2 2 9)) 2)
(check-expect (median (list 2 1 6 10.2)) 4)

;; median: (listof Num) -> Num
;; Requires: lst is non-empty
(define (median lst)
  (cond [(odd? (length lst))
         (median/odd (sort lst) (floor (/(length lst) 2)))]
        [(even? (length lst))
         (median/even (sort lst) (/ (length lst) 2))]))

;; Tests:
(check-expect (median (list 1 32 5.1 -5 23 60 1200)) 23)
(check-expect (median (list 1)) 1)
(check-expect (median (list 3.9 10)) 6.95)
(check-expect (median (list 9 2.73 5.16 10)) 7.08)


;; Examples:
(check-expect (median/odd (list 2 2 2 2 9) 2) 2)
(check-expect (median/odd (list 2.6) 0) 2.6)

;; median/odd: (listof Num) Nat -> Num
;; Requires: the length of lst is odd
;;           lst is non-decreasing
(define (median/odd lst pos)
  (cond [(= pos 0) (first lst)]
        [else (median/odd (rest lst) (- pos 1))]))


;; Examples:
(check-expect (median/even (list 2 2 2 2 3 9) 3) 2)
(check-expect (median/even (list 3.9 10) 1) 6.95)

;; median/even: (listof Num) Nat -> Num
;; Requires: the length of lst is even
;;           lst is non-decreasing
(define (median/even lst pos)
  (cond [(= pos 1) (/ (+ (first lst) (second lst)) 2)]
        [else (median/even (rest lst) (- pos 1))]))


;; The following two functions are in Slide 3 to Slide 8 in M08 in the notes

;; (sort lon) sorts the elements of lon in non-decreasing order
;; Example:
(check-expect (sort (cons 3 (cons 4 (cons 2 (cons 5 (cons 1 empty))))))
              (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))

;; sort: (listof Num) --> (listof Num)
(define (sort lon)
  (cond [(empty? lon) empty]
        [else (insert (first lon) (sort (rest lon)))]))


;; (insert n slon) inserts the number n into the sorted list slon
;;     so that the resulting list is also sorted.
;; Example:
(check-expect (insert 3 (cons 1 (cons 4 (cons 5 empty))))
              (cons 1 (cons 3 (cons 4 (cons 5 empty)))))

;; insert: Num (listof Num) --> (listof Num)
;;     requires: slon is sorted in non-decreasing order
(define (insert n slon)
  (cond [(empty? slon) (cons n empty)]
        [(<= n (first slon)) (cons n slon)]
        [else (cons (first slon) (insert n (rest slon)))]))
