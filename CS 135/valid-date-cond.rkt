;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date-cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 01, Problem 5(d)
;; ************************************
;;


;; (leap-year? given-year)determines whether the given year represents
;;  a leap year
;; Examples:
(check-expect (leap-year? 1900) false)
(check-expect (leap-year? 1904) true)

;; leap-year?: Nat -> Bool
(define (leap-year? given-year)
  (cond
    [(= (remainder given-year 400) 0) true]
    [(= (remainder given-year 100) 0) false]
    [(= (remainder given-year 4) 0) true]
    [else false]))

;; Tests:
(check-expect (leap-year? 360) true)
(check-expect (leap-year? 5) false)
(check-expect (leap-year? 1972) true)
(check-expect (leap-year? 1600) true)



;; (valid-date? given-date) determines whether the given date
;;   represents a valid date
;; Examples:
(check-expect (valid-date? 17520904) false)
(check-expect (valid-date? 123456789) false)
(check-expect (valid-date? 20200723) true)
(check-expect (valid-date? 40229) true)


;; valid-date?: Nat -> Bool
;; Requires: given-date has at least 5 digits
(define (valid-date? given-date)
  (cond
    [(member given-date (range 17520903 17520914 1)) false]
    [(member (month-day given-date) (range 0101 0132 1))true]
    [(member (month-day given-date) (range 0301 0332 1))true]
    [(member (month-day given-date) (range 0401 0431 1))true]
    [(member (month-day given-date) (range 0501 0532 1))true]
    [(member (month-day given-date) (range 0601 0631 1))true]
    [(member (month-day given-date) (range 0701 0732 1))true]
    [(member (month-day given-date) (range 0801 0832 1))true]
    [(member (month-day given-date) (range 0901 0931 1))true]
    [(member (month-day given-date) (range 1001 1032 1))true]
    [(member (month-day given-date) (range 1101 1131 1))true]
    [(member (month-day given-date) (range 1201 1232 1))true]
    [(member (month-day given-date) (range 0201 0229 1))true]
    [(> (month-day given-date) 0229) false]
    [(leap-year? (year given-date)) true]
    [else false]
    ))

;; Tests:
(check-expect (valid-date? 38214) false)
(check-expect (valid-date? 19770224) true)
(check-expect (valid-date? 12340413) true)
(check-expect (valid-date? 120431) false)
(check-expect (valid-date? 20131) true)


;; (year given-date)determines which year given the date
;; Examples:
(check-expect (year 10243291) 1024)
(check-expect (year 32431) 3)

;; year: Nat -> Nat
;; Requires: given-date has at least 5 digits
(define (year given-date)
  (string->number
   (substring (number->string given-date)
              0
              (- (string-length (number->string given-date)) 4))))


;; (month-day given-date)extracts the month and day from the given date
;;   and combines them
;; Examples:
(check-expect (month-day 23041203) 1203)
(check-expect (month-day 13012) 3012)

;; month-day: Nat -> Nat
;; Requires: given-date has at least 5 digits
(define (month-day given-date)
  (string->number
   (substring (number->string given-date)
              (- (string-length (number->string given-date)) 4)
              (string-length (number->string given-date)))))