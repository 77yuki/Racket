;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mean-relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 02, Problem 6
;; ************************************
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
  (cond
    [(empty? lst) empty]
    [(< (first lst) (mean-of-list lst))
     (cons 'below-mean (keep-mean-relative (mean-of-list lst) (rest lst)))]
    [(> (first lst) (mean-of-list lst))
     (cons 'above-mean (keep-mean-relative (mean-of-list lst) (rest lst)))]
    [else (cons 'mean (keep-mean-relative (mean-of-list lst) (rest lst)))]
        ))

;; Tests:
(check-expect (mean-relative (cons 3 (cons -9 (cons 1 empty))))
              (cons 'above-mean
                    (cons 'below-mean
                          (cons 'above-mean empty))))
(check-expect (mean-relative (cons 4 empty))
              (cons 'mean empty))
(check-expect (mean-relative empty) empty)


;; (sum lst) produces the sum of the list
;; Examples:
(check-expect (sum (cons 5 (cons 7 (cons 9 (cons 12 empty))))) 33)

;; sum: (listof Num) -> Num
(define (sum lst)
  (cond [(= (length lst) 1) (first lst)]
        [else (+ (first lst) (sum (rest lst)))]))


;; (mean-of-list lst) produces the mean of the list
;; Examples:
(check-expect (mean-of-list (cons 5 (cons 7 (cons 9 (cons 12 empty))))) 8.25)

;; mean-of-list: (listof Num) -> Num
(define (mean-of-list lst)
  (/ (sum lst) (length lst)))


;; a helper function that keeps the mean of the list constant
;; (keep-mean-relative constant lst) produces a list of symbols where
;;   each symbol corresponds to the relationship between each number
;; Example:
(check-expect (mean-relative (cons 5 (cons 7 (cons 9 (cons 12 empty)))))
              (cons 'below-mean
                    (cons 'below-mean
                          (cons 'above-mean
                                (cons 'above-mean empty)))))

;; keep-mean-relative: Num (listof Num) -> (listof Sym)
(define (keep-mean-relative constant lst)
  (cond
    [(empty? lst) empty]
    [(< (first lst) constant)
     (cons 'below-mean (keep-mean-relative constant (rest lst)))]
    [(> (first lst) constant)
     (cons 'above-mean (keep-mean-relative constant (rest lst)))]
    [else (cons 'mean (keep-mean-relative constant (rest lst)))]
        ))



