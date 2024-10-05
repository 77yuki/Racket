;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 07, Problem 1
;; ************************************
;;


;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))

;;
;; a)
;;
;; (super-filter pred? lst) keeps all the items on lst that satisfy pred?.
;; Examples:
(check-expect (super-filter even? (list 5 (list 1 2 3 (list 3 6) 7) 9 10))
              (list (list 2 (list 6)) 10))
(check-expect (super-filter odd? (list 3 0 5))
              (list 3 5))

;; super-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-filter pred? lst)
  (cond [(empty? lst) empty]
        [(not (list? (first lst)))
         (cond [(pred? (first lst))
                (cons (first lst) (super-filter pred? (rest lst)))]
               [else (super-filter pred? (rest lst))])]
        [else (cons (super-filter pred? (first lst))
                    (super-filter pred? (rest lst)))]))

;; Tests:
(check-expect (super-filter odd? (list 1 (list 10 60 23) 6 5 (list 11 2) 1230))
              (list 1 (list 23) 5 (list 11)))
(check-expect (super-filter even? empty) empty)


;;
;; b)
;;
;; (ruthless lst) removes the symbol 'ruth from lst
;; Examples:
(check-expect (ruthless (list 'apple (list 'pear 'ruth (list 'hello)) 'ruth))
              (list 'apple (list 'pear (list 'hello))))
(check-expect (ruthless (list 'ruth 'apple 'purple))
              (list 'apple 'purple))

;; ruthless: (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless lst)
  (local [(define (not-symbol-ruth? item) (not (symbol=? item 'ruth)))]
    (super-filter not-symbol-ruth? lst)))

;; Tests:
(check-expect (ruthless empty) empty)
(check-expect (ruthless (list 'ruth 'apple (list 'hi 'Waterloo)
                              (list 'a 'b 'ruth) 'ruth 'p))
              (list 'apple (list 'hi 'Waterloo) (list 'a 'b) 'p))


;;
;; c)
;;
;; (supersize n lst) removes all numbers less than n from lst
;; Examples:
(check-expect (supersize 20 (list 1 3 (list 10 60 (list 7)) 20))
              (list (list 60 (list)) 20))

;; supersize: Num (nested-listof Num) -> (nested-listof Num)
(define (supersize n lst)
  (local [(define (not-less-than-n? item) (not (< item n)))]
    (super-filter not-less-than-n? lst)))

;; Tests:
(check-expect (supersize 7 (list 1 6 20)) (list 20))
(check-expect (supersize 100 (list 1 (list 3 (list 10 22) 10 )6))
              (list (list (list))))
(check-expect (supersize 43 empty) empty)
(check-expect (supersize -200 (list -2 5 (list 7 0) -199))
              (list -2 5 (list 7 0) -199))


;;
;; d)
;;
;; (super-keeper pred? lst) produces a list with the elements of lst for
;;   which the predicate pred? produces false value 
;; Examples:
(check-expect (super-keeper even? (list 1 (list 3 9 2 (list 7 10) 0 3)))
              (list 1 (list 3 9 (list 7) 3)))

;; super-keeper: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-keeper pred? lst)
  (local [(define (not-pred? item) (not (pred? item)))]
    (super-filter not-pred? lst)))

;; Tests:
(check-expect (super-keeper odd? (list 10 23 (list 3 5 (list 1 5)
                                                   (list 2 3)) 7))
              (list 10 (list (list) (list 2))))
(check-expect (super-keeper even? empty) empty)



