;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Final, Problem 2
;; ***************************************************
;;


;; Q2b)


;; A Substitution Cipher (SCipher) is a (listof (list Char Char))
;; Requires: The first Char in each pair (the key) is unique


;; Useful constant for examples and testing
(define vowel-shift '((#\a #\e) (#\o #\i) (#\i #\m)
                      (#\e #\o) (#\u #\y) (#\y #\a) (#\c #\n)))




;; Q2bi)

;; Examples:
(check-expect (substitute "plain text!" vowel-shift) "plemn toxt!")
(check-expect (substitute "" vowel-shift) "")
(check-expect (substitute "abcd" vowel-shift) "ebnd")

;; substitute: Str SCipher -> Str
(define (substitute message cipher)
  (list->string
   (map
    (lambda (x)
      (cond [(= (length (filter (lambda (y) (char=? x (first y))) cipher)) 0) x]
            [else (second (first (filter
                                  (lambda (y) (char=? x (first y))) cipher)))]))
        (string->list message))))

;; Tests:
(check-expect (substitute "abcdef" empty) "abcdef")
(check-expect (substitute "waterloo is great" vowel-shift) "wetorlii ms groet")
(check-expect (substitute "student" '((#\s #\t) (#\u #\m))) "ttmdent")
(check-expect (substitute "apple" '((#\m #\n))) "apple")




;; Q2bii)

;; Examples:
(check-expect (reverse-cipher vowel-shift)
              '((#\e #\a) (#\i #\o) (#\m #\i)
                (#\o #\e) (#\y #\u) (#\a #\y) (#\n #\c)))
(check-expect (reverse-cipher '((#\a #\b) (#\c #\b))) false)

;; reverse-cipher: SCipher -> (Anyof SCipher Bool)
(define (reverse-cipher sc)
  (local [(define all-second (map second sc))

          ;; count-num: Char (listof Char) -> Nat
          (define (count-num a lst)
            (cond [(empty? lst) 0]
                  [(char=? a (first lst)) (+ 1 (count-num a (rest lst)))]
                  [else (+ 0 (count-num a (rest lst)))]))

          ;; not-duplicate-lst?: (listof Char) (listof Char) -> Bool
          (define (not-duplicate-lst? lst1 lst2)
            (cond [(empty? lst1) true]
                  [else
                   (cond [(> (count-num (first lst1) lst2) 1) false]
                         [else (not-duplicate-lst? (rest lst1) lst2)])]))

          ;; reverse-element: (list Char Char) -> (list Char Char)
          (define (reverse-element s)
            (list (second s) (first s)))

          ;; reverse-lst: SCipher -> SCipher
          (define (reverse-lst c)
            (cond [(empty? c) empty]
                  [else (cons (reverse-element (first c))
                              (reverse-lst (rest c)))]))]
    
    (cond [(empty? sc) empty]
          [(not-duplicate-lst? all-second all-second) (reverse-lst sc)]
          [else false])))

;; Tests:
(check-expect (reverse-cipher empty) empty)
(check-expect (reverse-cipher '((#\a #\e) (#\c #\a) (#\m #\n)))
              '((#\e #\a) (#\a #\c) (#\n #\m)))
(check-expect (reverse-cipher '((#\c #\c)))
              '((#\c #\c)))
(check-expect (reverse-cipher '((#\m #\c)))
              '((#\c #\m)))
(check-expect (reverse-cipher '((#\a #\e) (#\c #\e) (#\o #\e) (#\m #\e)))
              false)

