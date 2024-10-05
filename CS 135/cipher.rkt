;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cipher) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 02, Problem 5
;; *********************************************
;;


;; A Substitution Cipher (SCipher) is a (listof (list Char Char))
;; Requires: The first Char in each pair (the key) is unique
(define vowel-shift '((#\a #\e) (#\o #\i) (#\i #\m)
                      (#\e #\o) (#\u #\y) (#\y #\a)
                      (#\c #\n)))


;; Examples:
(check-expect (substitute "plain text!" vowel-shift)
              "plemn toxt!")
(check-expect (substitute "" vowel-shift)
              "")

;; substitute: Str SCipher -> Str
(define (substitute str scipher)
  (list->string (substitute/lst (string->list str) scipher)))

;; Tests:
(check-expect (substitute "Dog is cute." vowel-shift)
              "Dig ms nyto.")
(check-expect (substitute "CS135 is hard." vowel-shift)
              "CS135 ms herd.")
(check-expect (substitute "Jim is important." empty)
              "Jim is important.")


;; Examples:
(check-expect (substitute/lst (list #\p #\l #\a #\i #\n #\space #\t
                                    #\e #\x #\t #\!) vowel-shift)
              (list #\p #\l #\e #\m #\n #\space #\t #\o #\x #\t #\!))

;; substitute/lst: (listof Char) SCipher -> (listof Char)
(define (substitute/lst lst scipher)
  (cond [(empty? lst) empty]
        [else (cons (change (first lst) scipher)
                    (substitute/lst (rest lst) scipher))]))


;; Examples:
(check-expect (change #\a vowel-shift) #\e)
(check-expect (change #\m vowel-shift) #\m)
(check-expect (change #\o vowel-shift) #\i)

;; change: Char SCipher -> Char
(define (change c scipher)
  (cond
    [(empty? scipher) c]
    [(char=? c (first(first scipher))) (second (first scipher))]
    [else (change c (rest scipher))]))