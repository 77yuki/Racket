;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **************************************************
;;      Yuqi Gu (20884580)
;;      CS 135 Fall 2020
;;      Assignment 01, Problem 3
;; **************************************************
;;


;; (sequence-type a b c d) produces the symbol that
;;   corresponds to the type of sequence
;; Examples:
(check-expect (sequence-type 1 2 3 4) 'arithmetic)
(check-expect (sequence-type 3 9 27 81) 'geometric)
(check-expect (sequence-type 1 1 1 1) 'both)
(check-expect (sequence-type 1 3 9 10) 'neither)

;; sequence-type: Num Num Num Num -> Sym
(define (sequence-type a b c d) (cond [(and (is-arithmetic a b c d)
                                           (is-geometric a b c d)) 'both]
                                      [(is-arithmetic a b c d) 'arithmetic]
                                      [(is-geometric a b c d) 'geometric]
                                      [else 'neither]))
;;Tests
(check-expect (sequence-type 8.8 6.6 4.4 2.2) 'arithmetic)
(check-expect (sequence-type 3.2 9.6 28.8 86.4) 'geometric)
(check-expect (sequence-type 2.4 2.4 2.4 2.4) 'both)
(check-expect (sequence-type 4.2 3.2 6.4 1.2) 'neither)


;; (is-arithmetic a b c d) determines whether the sequence is arithmetic
;; Examples:
(check-expect (is-arithmetic 3 5 7 9) true)
(check-expect (is-arithmetic 1 4 5 20) false)

;; is-arithmetic: Num Num Num Num -> Bool
(define (is-arithmetic a b c d) (cond [(= (- b a) (- c b) (- d c)) true] 
                                     [else false]))


;; (is-geometric a b c d) determines whether the sequence is geometric
;; Examples:
(check-expect (is-geometric 4 8 16 32) true)
(check-expect (is-geometric 3 6 7 23) false)

;; is-geometric: Num Num Num Num -> Bool
(define (is-geometric a b c d) (cond [(= (/ b a) (/ c b) (/ d c)) true] 
                                     [else false]))