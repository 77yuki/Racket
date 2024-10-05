;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brackets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 09, Problem 2
;; ************************************
;;


;; (balanced? str) determines whether str is balanced or not, according
;;   to whether every opening bracket has a corresponding, appropriately
;;   nested closing bracket
;; Examples:
(check-expect (balanced? "(<>[])") true)
(check-expect (balanced? "<[>]()") false)

;; balanced?: Str -> Bool
;; Requires: str is bracket strings, which only contains #\(, #\), #\[,
;;           #\], #\<, and #\>
(define (balanced? str)
  (local [;; (match? open close) determines whether open and close are
          ;;   corresponding brackets or not
          ;; match?: Char Char -> Bool
          (define (match? open close)
            (cond [(or (and (char=? open #\<) (char=? close #\>))
                       (and (char=? open #\[) (char=? close #\]))
                       (and (char=? open #\() (char=? close #\))))
                   true]
                  [else false]))

          ;; (b? lst stack) determines whether the combination of lst
          ;;   and stack is balanced or not, according to whether every opening
          ;;   bracket has a corresponding, appropriately nested closing bracket
          ;; b?: (listof Char) (listof Char) -> Bool
          (define (b? lst stack)
            (cond [(empty? lst)
                   (cond [(empty? stack) true]
                         [else false])]
                  [(empty? stack) (b? (rest lst) (cons (first lst) stack))]
                  [else
                   (cond [(match? (first stack) (first lst))
                          (b? (rest lst) (rest stack))]
                         [else (b? (rest lst) (cons (first lst) stack))])]))]
    
    (b? (rest (string->list str)) (list (first (string->list str))))))

;; Tests:
(check-expect (balanced? "<><>") true)
(check-expect (balanced? "()[<]>") false)
(check-expect (balanced? "(((())))") true)
(check-expect (balanced? "[<(>)]") false)






  

