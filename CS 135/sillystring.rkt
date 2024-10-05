;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sillystring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 05, Problem 3
;; ***************************************************
;;


(define-struct silly-string (first middle last))
;; A SillyStringStruct is a (make-silly-string Char SillyStr Char)

;; A SillyStr is one of:
;; * empty
;; * a Char
;; * a SillyStringStruct


;;
;; 3a)
;;
;; (sillyfy s) produces the corresponding SillyStr of s
;; Examples:
(check-expect (sillify "Babbage")
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))

;; sillify: Str -> SillyStr
(define (sillify s)
 (sillify/lst (string->list s)))

;; Tests:
(check-expect (sillify "Lovelace")
              (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))
(check-expect (sillify "Apple")
              (make-silly-string
               #\A
               (make-silly-string
                #\p
                #\p
                #\l)
               #\e))
(check-expect (sillify "abc")
              (make-silly-string
               #\a
               #\b
               #\c))


;; (sillify/lst lst) produces the corresponding SillyStr of lst
;; Examples:
(check-expect (sillify/lst (list #\B #\a #\b #\b #\a #\g #\e))
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
(check-expect (sillify/lst (list #\a #\b #\c))
              (make-silly-string
               #\a
               #\b
               #\c))

;; sillify/lst: (listof Char) -> SillyStr
(define (sillify/lst lst)
  (cond
    [(= (remainder (length lst) 2) 1)
     (cond [(= (length lst) 3) (make-silly-string (first lst) (second lst) (third lst))]
           [else (make-silly-string (first lst)
                                    (sillify
                                     (substring (list->string lst) 1 (- (string-length (list->string lst)) 1)))
                                    (last-element lst))])]
    [else
     (cond [(= (length lst) 2) (make-silly-string (first lst) empty (second lst))]
           [else (make-silly-string (first lst)
                                    (sillify
                                     (substring (list->string lst) 1 (- (string-length (list->string lst)) 1)))
                                    (last-element lst))])]))


;; (last-element lst) produces the last element in lst
;; Examples:
(check-expect (last-element (list #\a #\b #\c)) #\c)
(check-expect (last-element (list #\B #\a #\b #\b #\a #\g #\e)) #\e)

;; last-element: (listof Char) -> Char 
(define (last-element lst)
  (cond [(= 1 (length lst)) (first lst)]
        [else (last-element (rest lst))]))


;;
;; 3b)
;;
;; (unsillify ss) produces the corresponding Str of ss
;; Examples:
(check-expect (unsillify
               (make-silly-string
                #\L
                (make-silly-string
                 #\o
                 (make-silly-string
                  #\v
                  (make-silly-string
                   #\e
                   empty
                   #\l)
                  #\a)
                 #\c)
                #\e))
              "Lovelace")

;; unsillify: SillyStr -> Str
(define (unsillify ss)
  (list->string (unsillify/lst ss ss)))




(check-expect (unsillify/lst
               (make-silly-string
                #\L
                (make-silly-string
                 #\o
                 (make-silly-string
                  #\v
                  (make-silly-string
                   #\e
                   empty
                   #\l)
                  #\a)
                 #\c)
                #\e)
               (make-silly-string
                #\L
                (make-silly-string
                 #\o
                 (make-silly-string
                  #\v
                  (make-silly-string
                   #\e
                   empty
                   #\l)
                  #\a)
                 #\c)
                #\e))
              (list #\L #\o #\v #\e #\l #\a #\c #\e))


(check-expect (unsillify/lst
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e)
             (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
              (list #\B #\a #\b #\b #\a #\g #\e))

;; unsillify: SillyStr SillyStr-> (listof Char)
(define (unsillify/lst ss constantss)
  (cond [(and (not (silly-string? (silly-string-middle ss)))
              (equal? (silly-string-middle ss) empty))
         (cons (silly-string-first ss)
               (cons (silly-string-last ss)
                     (reverse (add-last ss constantss))))]
        [(and (not (silly-string? (silly-string-middle ss)))
              (char=? (silly-string-middle ss)))
         (cons (silly-string-first ss)
               (cons (silly-string-middle ss)
                     (cons (silly-string-last ss)
                           (reverse (add-last ss constantss)))))]
        [else (cons (silly-string-first ss)
                    (unsillify/lst (silly-string-middle ss) constantss))]))

(define (add-last ss constantss)
  (cond [(silly-string? (silly-string-middle constantss))
         (cons (silly-string-last constantss)
               (add-last ss (silly-string-middle constantss)))]
        [else empty]))

