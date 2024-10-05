;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Final, Problem 2
;; ***************************************************
;;


;; Q2a)

;; Examples:
(check-expect (hamming-distance '(a b c d e f g)
                                '(a b c e d g f)) 4)
(check-expect (hamming-distance '() '()) 0)

;; hamming-distance: (listof Sym) (listof Sym) -> Nat
;; Requires: los1 and los2 have the same length
(define (hamming-distance los1 los2)
  (cond [(empty? los1) 0]
        [else (length (filter (lambda (x) (equal? x #false))
                              (map symbol=? los1 los2)))]))

;; Tests:
(check-expect (hamming-distance '(a) '(a)) 0)
(check-expect (hamming-distance '(a) '(b)) 1)
(check-expect (hamming-distance '(a b c d) '(a b c d)) 0)
(check-expect (hamming-distance '(b a b a b a a a b b)
                                '(a b a b b a a b b a)) 6)