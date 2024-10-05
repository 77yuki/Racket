;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 02, Problem 7
;; ************************************
;;


;; (sudoku-valid? lst) determines whether the list of numbers contains only
;;   each of the numbers one through nine exactly once and nothing else
;; Examples:
(define valid (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 9 (cons 8
               (cons 7 (cons 6 empty))))))))))
(check-expect (sudoku-valid? valid) true)
(check-expect (sudoku-valid? (cons 1 valid)) false)
(check-expect (sudoku-valid? (cons 0 valid)) false)

;; sudoku-valid?: (listof Num) -> Bool
(define (sudoku-valid? lst)
  (cond [(empty? lst) false]
        [(and (= (count 1 lst) 1) (= (length lst) 9))
         (cond [(= (count 2 lst) 1)
                (cond [(= (count 3 lst) 1)
                       (cond [(= (count 4 lst) 1)
                              (cond [(= (count 5 lst) 1)
                                     (cond [(= (count 6 lst) 1)
                                            (cond [(= (count 7 lst) 1)
                                                   (cond [(= (count 8 lst) 1)
                                                          (cond [(= (count 9 lst) 1) true])])])])])])])])]
        [else false]))

;; Tests:
(define valid1 empty)
(check-expect (sudoku-valid? valid1) false)
(check-expect (sudoku-valid? (cons 1 valid1)) false)



;; (count num lst) counts the number of num in the given list lst
;; Examples:
(check-expect (count 1 (cons 1 valid)) 2)
(check-expect (count 1 valid) 1)

;; count: Num (listof Num) -> Nat
(define (count num lst)
  (cond [(empty? lst) 0]
        [else (cond [(= num (first lst))
                     (+ 1 (count num (rest lst)))]
                    [else (count num (rest lst))])]))