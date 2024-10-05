;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname shortans) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PART A;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-it-so x k)
  (foldr (lambda (x k)
           (cond [(odd? x) k]
                 [else (cons (* 2 x) k)]))
         empty
         (map (lambda (x) (+ x k)) x)))

;; Complete the function definition for make-it-so2 here
(define (make-it-so2 x k) ; you can change the parameter names if you want to
  (cond [(empty? x) empty]
        [else
         (cond [(even? (first x)) (append empty (make-it-so2 (rest x) k))]
               [else (append (list (* 2 (+ (first x) k)))
                             (make-it-so2 (rest x) k))])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PART B;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write your answer as a comment here
;; The contract of the function and the base case do not match. To be specific,
;;   the contract shows us that the result is Num, which means that the function
;;   fun would produce a number. However, when lst is empty, an empty list is
;;   produced. Number is not a list. Thus, the contract and the base case do
;;   not match.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PART C;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct xnode (label children))
;; an exam tree node (XNode) is a (make-xnode Num (listof XTree))
;; an exam tree (XTree) is one of:
;; * a XNode or
;; * a Num.

;; Fill in the contract for g here
;; g: XTree -> Num

;; This is commented out because it cannot be run without a definition for h
;(define (g t)
;  (cond [(number? t) t]
;        [else
;         (h (xnode-label t)
;            (foldr
;             + 0 (map g
;                      (xnode-children t))))]))

;; Fill in the contract for h here
;; h: XNode -> Num

