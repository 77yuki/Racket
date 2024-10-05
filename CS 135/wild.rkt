;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wild) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 01, Problem 4
;; *********************************************
;;


;; Examples:
(check-expect
 (waterloo-wildlife (cons 'car (cons 'emu (cons 'tree empty))))
 (cons 'car (cons 'tree empty)))

;; waterloo-wildlife: (listof Sym) -> (listof Sym)
(define (waterloo-wildlife lst)
  (cond [(empty? lst) empty]
        [(or (equal? (first lst) 'emu)
             (equal? (first lst) 'penguin))
         (waterloo-wildlife (rest lst))]
        [else (cons (first lst) (waterloo-wildlife (rest lst)))]))

;; Tests:
(check-expect (waterloo-wildlife empty) empty)
(check-expect (waterloo-wildlife (cons 'emu (cons 'penguin empty)))
              empty)
(check-expect
 (waterloo-wildlife (cons 'tree (cons 'building (cons 'penguin empty))))
 (cons 'tree (cons 'building empty)))