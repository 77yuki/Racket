;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname not-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 05, Problem 1
;; ***************************************************
;;


(define-struct ls (first rest))
;; A (lsof X) is one of:
;; * 'nothing
;; * (make-ls X (lsof X))

;;
;; 1a)
;;
;; (lst-length ls) produces the number of values in ls
;; Examples:
(check-expect (ls-length
               (make-ls "!" (make-ls 'huh (make-ls 42 'nothing)))) 3)

;; ls-length: (lsof Any) -> Nat
(define (ls-length ls)
  (cond [(ls? ls)  (+ 1 (ls-length (ls-rest ls)))]
        [else 0]))

;; Tests:
(check-expect (ls-length
               (make-ls 'nothing
                        (make-ls "1"(make-ls (make-ls 'a (make-ls 2'nothing))
                        (make-ls "waterloo" 'nothing))))) 4)


;;
;; 1b)
;;
;; (ls-max ls) produces the largest value in ls
;; Examples:
(check-expect (ls-max (make-ls 5 (make-ls 9 (make-ls 7 'nothing)))) 9)

;; ls-max: (lsof Num) -> Num
(define (ls-max ls)
  (ls-max/maxnum (ls-rest ls) (ls-first ls)))

(define (ls-max/maxnum ls maxnum)
  (cond
    [(ls? ls) (cond [(> (ls-first ls) maxnum)
                     (ls-max/maxnum (ls-rest ls) (ls-first ls))]
                    [else (ls-max/maxnum (ls-rest ls) maxnum)])]
    
    [else maxnum]))

;; Tests:
(check-expect (ls-max
               (make-ls 0 (make-ls -15 (make-ls 1 (make-ls -2000 'nothing)))))
              1)
