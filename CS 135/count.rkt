;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname count) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Final, Problem 5
;; ***************************************************
;;


;; A (nested-listof X) is one of:
;; * empty
;; * (cons X (nested-listof X))
;; * (cons (nested-listof X) (nested listof X))


;; Examples:
(check-expect (count-starting-with #\1 empty) 0)
(check-expect (count-starting-with #\5 '("9" "5")) 1)
(check-expect
 (count-starting-with #\a '("alpha" ("bata") ("gamma" ("apple")))) 2)

;; count-starting-with: Char (nested-listof Str) -> Nat
(define (count-starting-with ch nls)
  (local [;; one-level: (nested-listof Str) -> (listof Str)
          (define (one-level l)
            (cond [(empty? l) empty]
                  [(string? (first l))
                   (cons (first l) (one-level (rest l)))]
                  [else (one-level (rest l))]))
          
          (define o-level (one-level nls))

          ;; rest-level: (nested-listof Str) -> (nested-listof Str)
          (define (rest-level l)
            (cond [(empty? l) empty]
                  [(list? (first l))
                   (cons (first l) (rest-level (rest l)))]
                  [else (rest-level (rest l))]))
          
          (define r-level (rest-level nls))

          ;; contain?: Char (listof Str) -> Nat
          (define (contain? ch l)
            (cond [(empty? l) 0]
                  [else
                   (cond [(char=? ch (first (string->list (first l)))) 1]
                         [else (contain? ch (rest l))])]))]
    
    (cond [(empty? nls) 0]
          [(string? (first nls))
           (+ (contain? ch o-level)
              (count-starting-with ch r-level))]
          [else (+ (count-starting-with ch (first nls))
                   (count-starting-with ch (rest nls)))])))

;; Tests:
(check-expect (count-starting-with #\a '("alpha" "apple" "gamma")) 1)
(check-expect
 (count-starting-with #\b '("banana" ("waterloo" "baby") "boy" ("great"))) 2)
(check-expect
 (count-starting-with #\2 '("2" ("2m" "2n" ("2c")) "2e" ("2h" "2g"))) 4)
(check-expect (count-starting-with #\e '("eat" "emm" ("orange"))) 1)
(check-expect (count-starting-with #\m '(("mom" "me") ("map"))) 2)
(check-expect (count-starting-with #\3 '(("3c" "3m"))) 1)
(check-expect
 (count-starting-with #\i '((("i" "in" ("out" "integer"))) "inn")) 3)
