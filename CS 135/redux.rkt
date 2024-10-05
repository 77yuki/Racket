;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname redux) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 08, Problem 2
;; ************************************
;;


;;
;; a)
;;
;; (parity str) determines the number of 1's is odd or even in the
;;   given string
;; Examples:
(check-expect (parity "110101") 'even)
(check-expect (parity "1110011") 'odd)

;; parity: Str -> Sym
(define (parity str)
  (local [;; (is-1? item) produces true if item is #\1
          ;; is-1?: Char -> Bool
          (define (is-1? item) (char=? item #\1))]
    
    (local [(define n (length (filter is-1? (string->list str))))]
      
      (cond [(odd? n) 'odd]
            [(even? n) 'even]))))

;; Tests:
(check-expect (parity "0000") 'even)
(check-expect (parity "1") 'odd)


;;
;; b)
;;
;; (replace-word first-string second-string lst) produces a new list
;;   where all occurrences of the first-string in lst have been
;;   replaced by the second-string
;; Examples:
(check-expect
 (replace-word "exam" "assessment"
               (cons "content"
                     (cons "exam"
                           (cons "assignment" empty))))
 (cons "content" (cons "assessment" (cons "assignment" empty))))

;; replace-word: Str Str (listof Str) -> (listof Str)
(define (replace-word first-string second-string lst)
  (foldr (lambda (str l)
           (cons (cond [(equal? str first-string) second-string]
                       [else str])
                 l))
         '() lst))

;; Tests:
(check-expect
 (replace-word "apple" "pear"
               (cons "apple"
                     (cons "banana"
                           (cons "apple" empty))))
 (cons "pear" (cons "banana" (cons "pear" empty))))
(check-expect
 (replace-word "train" "plane" empty) empty)


;;
;; c)
;;
;; (all-factors n) produces a list of all natural numbers x
;;   where 0 < x < n and x divides n evenly
;; Examples:
(check-expect
 (all-factors 30)
 (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))

;; all-factors: Nat -> (listof Nat)
;; Requires:
;; n > 0
(define (all-factors n)
  (local [;; (is-factor? fac) produces true if fac is a factor of n and
          ;;   fac is smaller than n
          ;; is-factor?: Nat -> Bool
          (define (is-factor? fac)
            (and (< fac n) (= (remainder n fac) 0)))]
    
    (filter is-factor? (build-list n (lambda (x) (+ x 1))))))

;; Tests:
(check-expect (all-factors 1) empty)
(check-expect (all-factors 2) (cons 1 empty))
(check-expect (all-factors 11) (cons 1 empty))


;;
;; d)
;;
;; (mean-relative lst) produces a list of symbols where
;;   each symbol corresponds to the relationship between each number
;;   at location in the list and the mean of the list
;; Examples:
(check-expect (mean-relative (cons 5 (cons 7 (cons 9 (cons 12 empty)))))
              (cons 'below-mean
                    (cons 'below-mean
                          (cons 'above-mean
                                (cons 'above-mean empty)))))

;; mean-relative: (listof Num) -> (listof Sym)
(define (mean-relative lst)
  (cond [(empty? lst) empty]
        [else
         (local [(define mean-of-list (/ (foldr + 0 lst) (length lst)))

                 ;; (m? n) produces a symbol, which corresponds to the
                 ;;   relationship between n and mean-of-list
                 ;; m?: Num -> Sym
                 (define (m? n)
                   (cond [(< n mean-of-list) 'below-mean]
                         [(> n mean-of-list) 'above-mean]
                         [(= n mean-of-list) 'mean]))]
           
           (map m? lst))]))

;; Tests:
(check-expect (mean-relative (cons 3 (cons -9 (cons 1 empty))))
              (cons 'above-mean
                    (cons 'below-mean
                          (cons 'above-mean empty))))
(check-expect (mean-relative (cons 4 empty))
              (cons 'mean empty))
(check-expect (mean-relative empty) empty)