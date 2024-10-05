;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 02, Problem 4
;; ************************************
;;

;; commonly used helpers
(define prices1
  (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))))
(define prices2 (cons 72.30 empty))


;; commonly used helper
;; (sum-of-costs costs) produces the sum of costs
;; Examples:
(check-expect (sum-of-costs prices1) 93.93)
(check-expect (sum-of-costs prices2) 72.3)

;; sum-of-costs: (listof Num) -> Num
;; Requires:
;; costs contains at least one positive numbers
(define (sum-of-costs costs)
  (cond [(= (length costs) 1) (first costs)]
        [else (+ (first costs) (sum-of-costs (rest costs)))]))


;;
;; Problem 4(a)
;;
;; (change-due costs paid) produces the amount of change the customer
;;   should receive back
;; Examples:
(check-expect (change-due prices1 100) 6.07)
(check-expect (change-due prices1 93.93) 0)

;; change-due: (listof Num) Num -> Num
;; Requires:
;; paid is positive
;; costs contains at least one positive numbers
;; paid > sum of costs
(define (change-due costs paid)
  (- paid (sum-of-costs costs)))

;; Tests:
(check-expect (change-due prices2 90) 17.7)
(check-expect (change-due prices2 72.5) 0.2)


;;
;; Problem 4(b)
;;
;; (paid-enough? costs paid) produces whether or not the customer
;;   has paid enough to cover the costs
;; Examples:
(check-expect (paid-enough? prices1 100) true)
(check-expect (paid-enough? prices1 69) false)

;; paid-enough?: (listof Num) Num -> Bool
;; paid is positive
;; costs contains at least one positive numbers
(define (paid-enough? costs paid)
  (cond [(> (sum-of-costs costs) paid) false]
        [else true]))

;; Tests:
(check-expect (paid-enough? prices2 30) false)
(check-expect (paid-enough? prices2 90) true)


;;
;; Problem 4(c)
;;
;; helper function that is used to define prices3 
(define prices3
  (cons 2.65 (cons 59.99 (cons 7.99 (cons 23.30 empty)))))


;; (free-item costs paid) produces the first item in the list that is
;;   large enough that if reduced to 0 would mean the customer
;;   has enough to pay for their bill
;; Examples:
(check-expect (free-item prices1 80) 23.3)
(check-expect (free-item prices1 60) 59.99)

;; free-item: (listof Num) Num -> Num
;; Requires:
;; paid is positive
;; costs contains at least one positive number
;; paid < sum of costs
;; there is one item in the list large enough to cover the difference
(define (free-item costs paid)
  (cond [(> (first costs) (- (sum-of-costs costs) paid)) (first costs)]
        [else (free-item (rest costs) (- paid (first costs)))]))

;; Tests:
(check-expect (free-item prices3 80) 59.99)
(check-expect (free-item prices2 70) 72.30)