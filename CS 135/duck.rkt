;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname duck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 01, Problem 3
;; *********************************************
;;


;;
;; Problem 3(a)
;;
;; Examples:
(check-expect (animal-size 200000) 'large)

;; animal-size: Nat -> Sym
(define (animal-size n)
  (cond [(< n 500) 'small]
        [(> n 10000) 'large]
        [else 'medium]))

;; Tests:
(check-expect (animal-size 0) 'small)
(check-expect (animal-size 999) 'medium)


;;
;; Problem 3(b)
;;
;; Examples:
(check-expect (goose? true true true 200000) true)

;; goose?: Bool Bool Bool Nat -> Bool
(define (goose? swims? flies? angry? weight)
  (cond [(and (equal? swims? true)
              (equal? flies? true)
              (equal? angry? true)
              (equal? (animal-size weight) 'large))
         true]
        [else false]))

;; Tests:
(check-expect (goose? true false false 100000) false)
(check-expect (goose? true true true 400) false)


;;
;; Problem 3(c)
;;
;; Examples:
(check-expect (what-animal true true true 200000) 'goose)

;; what-animal: Bool Bool Bool Nat -> Sym
(define (what-animal swims? flies? angry? weight)
  (cond [(goose? swims? flies? angry? weight) 'goose]
        [(and (equal? swims? true) (equal? flies? true))
         (cond [(equal? angry? false) 'duck]
               [else 'gull])]
        [(and (equal? swims? false) (equal? flies? true))
         (cond [(equal? (animal-size weight) 'small) 'sparrow]
               [else 'crow])]
        [(and (equal? swims? false) (equal? flies? false))
         (cond [(equal? (animal-size weight) 'small) 'squirrel]
               [else 'emu])]
        [else 'penguin]))
        
;; Tests:
(check-expect (what-animal true false false 200) 'penguin)
(check-expect (what-animal true true false 1000) 'duck)
(check-expect (what-animal true true true 900) 'gull)
(check-expect (what-animal false true true 1000) 'crow)
(check-expect (what-animal false false true 200) 'squirrel)
(check-expect (what-animal false true false 100) 'sparrow)
(check-expect (what-animal false false false 899) 'emu)







