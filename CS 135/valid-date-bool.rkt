;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date-bool) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 01, Problem 5(c)
;; ************************************
;;


;; (leap-year? given-year)determines whether the given year represents
;;  a leap year
;; Examples:
(check-expect (leap-year? 1900) false)
(check-expect (leap-year? 1904) true)

;; leap-year?: Nat -> Bool
(define (leap-year? given-year)
  (or (= (remainder given-year 400) 0)
      (and (not (= (remainder given-year 400) 0))
           (not (= (remainder given-year 100) 0))
           (= (remainder given-year 4) 0))))
   
;; Tests:
(check-expect (leap-year? 360) true)
(check-expect (leap-year? 5) false)
(check-expect (leap-year? 1972) true)
(check-expect (leap-year? 1600) true)



;; (valid-date? given-date) determines whether the given date
;;   represents a valid date
;; Examples:
(check-expect (valid-date? 123456789) false)
(check-expect (valid-date? 20200723) true)
(check-expect (valid-date? 40212) true)
(check-expect (valid-date? 17520904) false)

;; valid-date?: Nat -> Bool
;; Requires: given-date has at least 5 digits
(define (valid-date? given-date)
  (and (or
       (and (equal? (leap-year? (year given-date)) false)
            (= (month given-date) 02) (<= (day given-date) 28)
            (> (day given-date)00))
       (and (equal? (leap-year? (year given-date)) true)
            (= (month given-date) 02) (<= (day given-date) 29)
            (> (day given-date) 00))
       (and (or (= (month given-date) 01) (= (month given-date) 03)
                (= (month given-date) 05) (= (month given-date) 07)
                (= (month given-date) 08) (= (month given-date) 10)
                (= (month given-date) 12))
            (<= (day given-date) 31)(> (day given-date) 00))
       (and (or (= (month given-date) 04) (= (month given-date) 06)
                (= (month given-date) 09) (= (month given-date) 11))
            (<= (day given-date) 30)(> (day given-date) 00)))
       (not (and (= (year given-date) 1752) (= (month given-date) 09)
          (< (day given-date) 14) (> (day given-date) 2)))))

;; Tests:
(check-expect (valid-date? 38214) false)
(check-expect (valid-date? 19770224) true)
(check-expect (valid-date? 12340413) true)
(check-expect (valid-date? 120431) false)


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


;; (month given-date)determines which month given the date
;; Examples:
(check-expect (month 20030419) 04)
(check-expect (month 194302) 43)

;; month: Nat -> Nat
;; Requires: given-date has at least 5 digits
(define (month given-date)
  (string->number
   (substring (number->string given-date)
              (- (string-length (number->string given-date)) 4)
              (- (string-length (number->string given-date)) 2))))


;; (day given-date)determines which day given the date
;; Examples:
(check-expect (day 20430205) 05)
(check-expect (day 204132) 32)

;; day: Nat -> Nat
;; Requires: given-date has at least 5 digits
(define (day given-date)
  (string->number
   (substring (number->string given-date)
              (- (string-length (number->string given-date)) 2)
              (string-length (number->string given-date)))))