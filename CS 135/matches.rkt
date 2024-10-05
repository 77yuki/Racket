;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matches) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 08, Problem 4
;; ************************************
;;


;; (matches-func? f lst) porduces true if each pair in the lst is of
;;   the form (x (f x)), which is the second value in the pair is the
;;   result of applying f to the first value in the pair, and false
;;   otherwise
;; Examples:
(check-expect (matches-func? sqr '((5 25) (3 9) (2 4) (6 36))) true)
(check-expect (matches-func? sqrt '((36 6) (20 4) (49 7))) false)

;; matches-func?: (X -> Y) (listof (list X Y)) -> Bool
(define (matches-func? f lst)
  (local [;; (is-same? f1 lst1) produces true if (f1 (first lst1)) equals
          ;;   to (second lst1), and false otherwise
          ;; is-same?: (X -> Y) (listof (list X Y)) -> Bool
          (define (is-same? f1 lst1)
            (cond [(equal? (f1 (first lst1)) (second lst1)) true]
                  [else false]))]
    
    (foldr (lambda (x rror) (and (is-same? f x) rror)) true lst)))

;; Tests:
(check-expect (matches-func? add1 '((5 6) (3 4) (7 empty))) false)
(check-expect (matches-func?
               (lambda (x) (cond [(zero? x) true]
                                 [else false]))
               (list (list 5 false) (list 0 true) (list 10 false))) true)
(check-expect (matches-func? add1 empty) true)



