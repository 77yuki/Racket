;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 01, Problem 6
;; *********************************************
;;


;; helper functions that are used to define test-lists
(define test0 (cons 15.0 (cons 22.0 (cons -273.15 (cons 20.0 empty)))))
(define test1 (cons 8.0 (cons 12.0 (cons 35.0 (cons 9.0 empty)))))
(define test2 (cons 2.0 (cons 7.0 (cons -1.3 (cons 12.0 empty)))))
(define test3 (cons 8.0 (cons 13.4 (cons 15.2 (cons 9.3 empty)))))


;; Examples:
(check-expect (weather test0) 'sad)
(check-expect (weather test1) 'happy)

;; weather: (listof Num) -> Sym
(define (weather lst)
  (cond [(empty? lst) 'okay]
        [(> (below-eight lst) 0) 'sad]
        [(> (first lst) 20) 'happy]
        [else (weather (rest lst))]))

;; Tests:
(check-expect (weather test2) 'sad)
(check-expect (weather test3) 'okay)


;; Examples:
(check-expect (below-eight test0) 1)
(check-expect (below-eight test1) 0)

;; below-eight: (listof Num) -> Num
(define (below-eight lst)
  (cond [(empty? lst) 0]
        [(< (first lst) 8) (+ 1 (below-eight (rest lst)))]
        [else (below-eight (rest lst))]))

;; Tests:
(check-expect (below-eight test2) 3)
(check-expect (below-eight test3) 0)