;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vowels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 03, Problem 2
;; ************************************
;;


;; (total-vowels lst) produces the total number of lower-case vowels
;;   in all of the strings in lst 
;; Examples:
(check-expect (total-vowels (cons "test" (cons "look" empty))) 3)

;; total-vowels: (listof Str) -> Nat
(define (total-vowels lst)
  (cond [(empty? lst) 0]
        [else (+ (total-vowels/str (first lst))
                 (total-vowels (rest lst)))]))

;; Tests:
(check-expect (total-vowels empty) 0)
(check-expect (total-vowels (cons "area" (cons "cook" empty))) 5)


;; (total-vowels/str str) produces the total number of lower-case
;;   vowels in str
;; Examples:
(check-expect (total-vowels/str "test") 1)

;; total-vowels/str: Str -> Nat
(define (total-vowels/str str)
  (total-vowels/lst (string->list str)))


;; (total-vowels/lst lst)produces the total number of lower-case
;;   vowels in lst
;; Examples:
(check-expect (total-vowels/lst
               (cons #\t (cons #\e (cons #\s (cons #\t empty))))) 1)

;; total-vowels/lst: (listof Str) -> Nat
(define (total-vowels/lst lst)
  (cond [(empty? lst) 0]
        [(or (char=? (first lst) #\a)
         (char=? (first lst) #\e)
         (char=? (first lst) #\i)
         (char=? (first lst) #\o)
         (char=? (first lst) #\u))
         (+ 1 (total-vowels/lst (rest lst)))]
        [else (total-vowels/lst (rest lst))]))

