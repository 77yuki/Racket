;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 09, Problem 1
;; ************************************
;;


;;
;; a)
;;
;; (flip-case lst) produces a list of strings, whose first two strings remain
;;   the same, and if the sum of length of the previous two strings is even, the
;;   string behind them should be in upper case, and if it is odd, the string
;;   behind them should be in lower case.
(check-expect (flip-case '("Waterloo" "Great!")) '("Waterloo" "Great!"))
(check-expect (flip-case '("I" "LoVe" "CS" "so" "MuCh"))
              '("I" "LoVe" "cs" "SO" "MUCH"))

;; flip-case: (listof Str) -> (listof Str)
(define (flip-case lst)
  (cond
    [(< (length lst) 3) lst]
    [else
     (append (list (first lst)) (list(second lst))
             (local
               [;; (flip-case/f lst) produces a list of strings according
                ;;   to the following rules: if the sum of length of the previous
                ;;   two strings is even, the string behind them should be in upper case,
                ;;   and if it is odd, the string behind them should be in lower case.
                ;; flip-case/f: (listof Str) -> (listof Str)
                (define (flip-case/f lst)
                  (cond [(< (length lst) 3) empty]
                        [else
                         (local [(define l
                                   (+ (string-length (first lst))
                                      (string-length (second lst))))]
                           (cond [(odd? l) (cons (string-downcase (third lst))
                                                 (flip-case/f (rest lst)))]
                                 [(even? l) (cons (string-upcase (third lst))
                                                  (flip-case/f (rest lst)))]))]))]
               
               (flip-case/f lst)))]))

;; Tests:
(check-expect (flip-case '("Water" "waTeR" "wAter" "LOO" "LoO" "loo"))
              '("Water" "waTeR" "WATER" "LOO" "LOO" "LOO"))
(check-expect (flip-case '()) '())
(check-expect (flip-case '("I" "LIke" "Dogs" "They" "aRe" "Cute"))
              '("I" "LIke" "dogs" "THEY" "ARE" "cute"))


;;
;; b)
;;
;; (function-go-round fn-list data-list) applies each element of fn-list to
;;   corresponding element of data-list, and if the length of fn-list is smaller
;;   than the length of data-list, then fn-list "rotates", which means
;;   that we apply the first element of fn-list to the next element of data-list
;; Examples:
(check-expect (function-go-round
               (list string-length string-upcase
                     (lambda (x) (string-append x "!!")))
               '("joy" "anger" "disgust" "sadness" "fear"))
              '(3 "ANGER" "disgust!!" 7 "FEAR"))

;; function-go-round: (listof (X -> Y)) (listof X) -> (listof Y)
;; Requires: fn-list is non-empty list
(define (function-go-round fn-list data-list)
  (local [(define fn fn-list)

          ;; (f fnl datal) applies each element of fnl to corresponding element
          ;;   of datal, and if the length of fnl is smaller than the length of
          ;;   datal, then fnl "rotates", which means that we apply the first element
          ;;   of fnl to the next element of datal
          (define (f fnl datal)
            (cond [(empty? datal) empty]
                  [(empty? fnl) (f fn datal)]
                  [else (cons ((first fnl) (first datal))
                    (f (rest fnl) (rest datal)))]))]
    
    (f fn-list data-list)))

;; Tests:
(check-expect (function-go-round (list string-upcase) '()) '())
(check-expect (function-go-round
               (list string-upcase string-downcase string-length)
               '("Amy" "Anger" "flower"))
              '("AMY" "anger" 6))
(check-expect (function-go-round (list odd? (lambda (x) (< 10 x)) add1)
                                 '(2 5 9 11 3 6))
              (list false false 10 true false 7))
(check-expect (function-go-round (list even?) '(25 51 42 61 82))
              (list false false true false true))


