;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname general) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Final, Problem 6
;; ***************************************************
;;


;; an (alistof X Y) is a (listof (list X Y))
;; Requires: each X is unique (no duplicates)



;; Examples:
(check-expect (alist-combine '((a 1) (b 2)) '((c 1) (a 3)) +)
              '((a 4) (b 2) (c 1)))
(check-expect (alist-combine '((1 1) (2 2)) empty +)
              '((1 1) (2 2)))
(check-expect (alist-combine '((1 "One") (3 "Three") (2 "Two"))
                             '((2 "Deux") (1 "Un") (3 "Trois"))
                             (lambda (s1 s2)
                               (cond [(< (string-length s1)
                                         (string-length s2))
                                      s1]
                                     [else s2])))
              '((1 "Un") (3 "Trois") (2 "Two")))

;; alist-combine: (alistof X Y) (alistof X Y) (Y Y -> Y) -> (alistof X Y)
(define (alist-combine alst1 alst2 combine)
  (local [;; process: (list X Y) (alistof X Y) -> (list X Y)
          (define (process a lst)
            (cond [(empty? lst) a]
                  [(equal? (first a)
                           (first (first lst)))
                   (list (first a) (combine (second a) (second (first lst))))]
                  [else (process a (rest lst))]))

          ;; contain?: (list X Y) (alistof X Y) -> Bool
          (define (contain? b lst)
            (cond [(empty? lst) false]
                  [(equal? (first b) (first (first lst))) true]
                  [else (contain? b (rest lst))]))

          ;; extract-alst2: (alistof X Y) (alistof X Y) -> (alistof X Y)
          (define (extract-alst2 lst1 lst2)
            (cond [(empty? lst2) empty]
                  [else
                   (cond [(contain? (first lst2) lst1)
                          (extract-alst2 lst1 (rest lst2))]
                         [else
                          (cons (first lst2)
                                (extract-alst2 lst1 (rest lst2)))])]))
          
          (define extra-lst2 (extract-alst2 alst1 alst2))
          
          ;; alist-combine-extra: (alistof X Y) (alistof X Y) (Y Y -> Y) (alistof X Y) -> (alistof X Y)
          (define (alist-combine-extra a1 a2 combine extra2)
            (cond [(empty? a1) extra2]
                  [else
                   (cons (process (first a1) a2)
                         (alist-combine-extra (rest a1) a2 combine extra2))]))]
    
    (alist-combine-extra alst1 alst2 combine extra-lst2)))
         
;; Tests:
(check-expect (alist-combine '((2 2) (3 3) (4 5))
                             '((3 5) (4 10) (1 2)) +)
              '((2 2) (3 8) (4 15) (1 2)))
(check-expect (alist-combine '((c "mouth") (b "b"))
                             '((b "box") (c "cat") (d "dog"))
                             (lambda (s1 s2)
                               (cond [(> (string-length s1)
                                         (string-length s2))
                                      s1]
                                     [else s2])))
              '((c "mouth") (b "box") (d "dog")))
(check-expect (alist-combine empty empty -) empty)
(check-expect (alist-combine '((2 30) (9 100)) empty -)
              '((2 30) (9 100)))
(check-expect (alist-combine '((2 6) (4 "beautiful"))
                             '((3 "waterloo") (4 11)) list)
              '((2 6) (4 ("beautiful" 11)) (3 "waterloo")))
(check-expect (alist-combine '((5 a) (4 128))
                             '((5 "waterloo") (4 b)) list)
              '((5 (a "waterloo")) (4 (128 b))))
(check-expect (alist-combine '((a 1) (b 2) (c 3))
                             '((d 4) (e 5) (f 6)) +)
              '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6)))
(check-expect (alist-combine '((a 10) (b 2))
                             '((b "baby") (a "apple")) list)
              '((a (10 "apple")) (b (2 "baby"))))