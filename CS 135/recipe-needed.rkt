;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recipe-needed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 09, Problem 5
;; ***************************************************
;;


;; (m1 a) determines whether a is symmetrical, which means that the
;;   original a is the same as a in reversed order
;; Examples:
(check-expect (m1 "abcba") true)
(check-expect (m1 "aabb") false)
(check-expect (m1 "1230321") true)

;; m1: Str -> Bool
(define (m1 a)
  (local
    [(define b (string->list a))]
    (foldr
     (lambda (x y z)
       (and (char=? x y) z))
     true
     b
     (foldl cons empty b))))

;; Tests:
(check-expect (m1 "") true)
(check-expect (m1 "abcdef") false)
(check-expect (m1 "]]]") true)
(check-expect (m1 "[]") false)



;; (m2 a) creates a function that determines whether a string is a reverse
;;   of any string in a or not, and they would not be the same
;; Examples:
(check-expect ((m2 (list "abc")) "cba") true)
(check-expect ((m2 (list "a" "b" "c")) "c") false)

;; m2: (listof Str) -> (Str -> Bool) 
(define (m2 a)
  (lambda (s)
    (local
      [(define (r x) (quicksort (string->list x) char<?))
       (define u (map (lambda (x) (list x (r x))) a))
       (define (t x) (foldr (lambda (y z) (add1 z)) 0 x))]
      (foldr
       (lambda (x y)
         (cond
           [(string=? s (first x)) y]
           [(not (= (t (r s)) (t (second x)))) y]
           [else
            (or y (foldr
                   (lambda (x y)
                     (cond
                       [(and x y) y]
                       [else false]))
                   true
                   (map (lambda (a b) (char=? a b)) (r s) (second x))))]))
       false
       u))))

;; Tests:
(check-expect ((m2 (list)) "") false)
(check-expect ((m2 (list "ab" "cc" "1234")) "4321") true)
(check-expect ((m2 (list "waterloo" "great" "cc")) "cc") false)
(check-expect ((m2 (list "a" "water" "loo" "ool")) "loo") true)




;; (m3 a b) applies a on the result of the largest number minus the smallest
;;   number in all integers of b
;; Examples:
(check-expect (m3 add1 (list 1 2 3)) 3)
(check-expect (m3 odd? (list 3.2 2 20)) false)
(check-expect (m3 add1 (list 3.2 2 20)) 19)

;; m3: (Int -> X) (listof Any) -> X
;; Requires: b should contain at least one integer
(define (m3 a b)
   (local
     [(define c (filter integer? b))
      (define (d e f)
        (foldl (lambda (x y)
                 (cond
                   [(f x y) x]
                   [else y]))
               (first e)
               e))]
     (a (- (d c >) (d c <)))))

;; Tests:
(check-expect (m3 add1 (list 3)) 1)
(check-expect (m3 (lambda (x) (> x 2)) (list 10.8 30 #\a 20 #\w 23 137)) true)
(check-expect (m3 sub1 (list 10 "waterloo" 30 20 'great 23 137)) 126)
(check-expect (m3 (lambda (x) (+ x 13)) (list 20.8 3 "our" 'future)) 13)