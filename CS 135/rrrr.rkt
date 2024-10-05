;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rrrr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 01, Problem 7
;; *********************************************
;;


;; Examples:
(check-expect (string->pirate "na") "narrrr")
(check-expect (string->pirate "Waterloo") "Warrrrterloo")
(check-expect (string->pirate "Aardvark") "Arrrrarrrrrdvarrrrrk")

;; string->pirate: Str -> Str
(define (string->pirate str)
  (list->string (list->pirate (string->list str))))

;; Tests:
(check-expect (string->pirate "Aa") "Arrrrarrrr")
(check-expect (string->pirate "dog") "dog")


;; Examples:
(check-expect (list->pirate (cons #\n (cons #\a empty)))
              (cons #\n (cons #\a (cons #\r (cons #\r (cons #\r
              (cons #\r empty)))))))

;; list->pirate: (listof Str) -> (listof Str)
(define (list->pirate lst)
  (cond [(empty? lst) empty]
        [(equal? (first lst) #\a)
         (cons #\a (cons #\r (cons #\r (cons #\r (cons #\r
         (list->pirate (rest lst)))))))]
        [(equal? (first lst) #\A)
         (cons #\A (cons #\r (cons #\r (cons #\r (cons #\r
         (list->pirate (rest lst)))))))]
        [else (cons (first lst) (list->pirate (rest lst)))]))

;; Tests:
(check-expect (list->pirate (cons #\d (cons #\o (cons #\g empty))))
              (cons #\d (cons #\o (cons #\g empty))))
(check-expect (list->pirate (cons #\A (cons #\a empty)))
              (cons #\A (cons #\r (cons #\r (cons #\r (cons #\r
              (cons #\a (cons #\r (cons #\r (cons #\r (cons #\r empty)))))))))))
