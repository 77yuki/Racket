;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname resource) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 02, Problem 6
;; *********************************************
;;


;; A Resource is (anyof 'fire 'wood 'water)

(define-struct card (type power))
;; A Card is a (make-card Resource Nat)

;; An Outcome is (anyof 'player-1 'player-2 'tie-game)


;;
;; 6a)
;;
;; Examples:
(check-expect (beats? 'fire 'wood) true)
(check-expect (beats? 'wood 'wood) false)

;; beats?: Resource Resource -> Bool
(define (beats? r1 r2)
  (cond [(and (symbol=? r1 'wood) (symbol=? r2 'water)) true]
        [(and (symbol=? r1 'water) (symbol=? r2 'fire)) true]
        [(and (symbol=? r1 'fire) (symbol=? r2 'wood)) true]
        [else false]))

;; Tests:
(check-expect (beats? 'water 'fire) true)
(check-expect (beats? 'wood 'water) true)
(check-expect (beats? 'water 'wood) false)


;;
;; 6b)
;;
;; Examples:
(check-expect (winner (make-card 'wood 9) (make-card 'water 10))
              'player-1)
(check-expect (winner (make-card 'fire 8) (make-card 'fire 10))
              'player-2)

;; winner: Card Card -> Outcome
(define (winner c1 c2)
  (cond [(beats? (card-type c1) (card-type c2)) 'player-1]
        [(beats? (card-type c2) (card-type c1)) 'player-2]
        [else (cond [(> (card-power c1) (card-power c2)) 'player-1]
                    [(> (card-power c2) (card-power c1)) 'player-2]
                    [else 'tie-game])]))

;; Tests:
(check-expect (winner (make-card 'fire 8) (make-card 'water 10))
              'player-2)
(check-expect (winner (make-card 'wood 10) (make-card 'wood 1))
              'player-1)
(check-expect (winner (make-card 'water 5) (make-card 'water 5))
              'tie-game)


;;
;; 6c)
;;
(define testcards
  (list (make-card 'fire 4)
        (make-card 'water 9)
        (make-card 'wood 12)
        (make-card 'water 4)))
(define testcards1
  (list (make-card 'fire 2)
        (make-card 'wood 5)
        (make-card 'wood 3)))


;; Examples:
(check-expect (winner-table testcards)
              (list (list 'tie-game 'player-2 'player-1 'player-2)
                    (list 'player-1 'tie-game 'player-2 'player-1)
                    (list 'player-2 'player-1 'tie-game 'player-1)
                    (list 'player-1 'player-2 'player-2 'tie-game)))

;; winner-table: (listof Card) -> (listof (listof Outcome))
(define (winner-table cards)
  (winner-table/2 cards cards))

;; Tests:
(check-expect (winner-table empty) empty)
(check-expect (winner-table testcards1)
              (list (list 'tie-game 'player-1 'player-1)
                    (list 'player-2 'tie-game 'player-1)
                    (list 'player-2 'player-2 'tie-game)))


;; Examples:
(check-expect (winner-table/2 testcards testcards)
              (list (list 'tie-game 'player-2 'player-1 'player-2)
                    (list 'player-1 'tie-game 'player-2 'player-1)
                    (list 'player-2 'player-1 'tie-game 'player-1)
                    (list 'player-1 'player-2 'player-2 'tie-game)))
(check-expect (winner-table/2 empty empty) empty)

;; winner-table/2: (listof Card) (listof Card) -> (listof (listof Outcome))
(define (winner-table/2 cards constantcards)
  (cond [(empty? cards) empty]
        [else (cons (winner-table1 (first cards) constantcards)
                    (winner-table/2 (rest cards) constantcards))]))


;; Examples:
(check-expect (winner-table1 (make-card 'fire 4) testcards)
              (list 'tie-game 'player-2 'player-1 'player-2))

;; winner-table1: Card (listof Card) -> (listof Outcome)
(define (winner-table1 c1 cards)
  (cond [(empty? cards) empty]
        [else (cons (winner c1 (first cards))
                    (winner-table1 c1 (rest cards)))]))


;;
;; 6d)
;;
;; Examples:
(check-expect (cheat testcards)
              (list (make-card 'fire 6)
                     (make-card 'water 9)
                     (make-card 'wood 12)
                     (make-card 'water 6)))
(check-expect (cheat empty) empty)

;; cheat: (listof Card) -> (listof Card)
(define (cheat testcards)
  (cond [(empty? testcards) empty]
        [(> (card-power (first testcards)) 5)
         (cons (first testcards)
               (cheat (rest testcards)))]
        [else
          (cons (make-card (card-type (first testcards))
                           (+ 2 (card-power (first testcards))))
                (cheat (rest testcards)))]))

;; Tests:
(check-expect (cheat (list (make-card 'water 6)
                           (make-card 'fire 10)))
              (list (make-card 'water 6)
                    (make-card 'fire 10)))
(check-expect (cheat testcards1)
              (list (make-card 'fire 4)
                    (make-card 'wood 7)
                    (make-card 'wood 5)))