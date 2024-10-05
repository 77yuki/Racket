;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname dist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *********************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Midterm 02, Problem 4
;; *********************************************
;;


(define g1 '(t a a a g a a t t))
(define g2 '(t a g a a g a t a))
(define packet1 '(b a b b b a a a))
(define packet2 '(a b a b b a a a))
(define packet3 '(a b a b a b a a))

;;
;; 4a)
;;
;; Examples:
(check-expect (hamming-distance g1 g2) 4)
(check-expect (hamming-distance empty empty) 0)

;; hamming-distance: (listof Sym) (listof Sym) -> Int
;; Requires: the length of lst1 is the same as the length of lst2
(define (hamming-distance lst1 lst2)
  (cond [(and (empty? lst1) (empty? lst2)) 0]
        [(symbol=? (first lst1) (first lst2))
         (hamming-distance (rest lst1) (rest lst2))]
        [else (+ 1 (hamming-distance (rest lst1) (rest lst2)))]))

;; Tests:
(check-expect (hamming-distance packet1 packet3) 5)
(check-expect (hamming-distance packet1 packet2) 3)




;;
;; 4b)
;;

;; A HamPair is a (list Str (listof Sym))
    
;; A HamList is a (listof HamPair)
;; Requires: every (listof Sym) in the HamList has the 
;;   same length. Each Str in each HamPair is distinct.

(define channel-list (list
                      (list "channel1" '(b a b b b a a a))
                      (list "channel2" '(a b a b b b b a))
                      (list "channel3" '(a b a a a a a a))
                      (list "channel4" '(a b a b a b a b))))


;; Examples:
(check-expect (min-hamming packet1 empty) empty)
(check-expect (min-hamming packet1 channel-list) '("channel1"))

;; min-hamming: (listof Sym) HamList -> (listof Str)
;; Requires: if the hamlst is nonempty, lst has the same length as
;;   each (listof Sym) in hamlst. If the hamlst is empty, we don't
;;   care what the length of lst is.
(define (min-hamming lst hamlst)
  (cond [(empty? hamlst) empty]
        [(empty? (rest hamlst)) (list (first (first hamlst)))]
        [else
         (become-list lst hamlst
            (min-length lst (rest hamlst)
                        (hamming-distance lst (second (first hamlst)))))]))

;; Tests:
(check-expect (min-hamming packet2 channel-list)
              '("channel2" "channel3"))
(check-expect (min-hamming packet1
                           (list (list "channel5" '(a a a b a b a b))))
              '("channel5"))
(check-expect (min-hamming packet3 channel-list)
              '("channel4"))


;; Examples:
(check-expect (become-list packet1 channel-list 0) '("channel1"))
(check-expect (become-list packet2 channel-list 2) '("channel2" "channel3"))
(check-expect (become-list packet3 channel-list 1) '("channel4"))

;; become-list: (listof Sym) HamList Nat -> (listof Str)
;; Requires: hamlst is nonempty,
;;           lst has the same length as each (listof Sym) in hamlst. 
(define (become-list lst hamlst minlength)
  (cond [(empty? hamlst) empty]
        [else (cond [(= (hamming-distance lst (second (first hamlst)))
                        minlength)
                     (cons (first (first hamlst))
                           (become-list lst (rest hamlst) minlength))]
                    [else (become-list lst (rest hamlst) minlength)])]))


;; Examples:
(check-expect (min-length packet1 (rest channel-list)
                          (hamming-distance
                           packet1 (second (first channel-list))))
              0)
(check-expect (min-length packet2 (rest channel-list)
                          (hamming-distance
                           packet2 (second (first channel-list))))
              2)
(check-expect (min-length packet3 (rest channel-list)
                          (hamming-distance
                           packet3 (second (first channel-list))))
              1)

;; min-length: (listof Sym) HamList -> Nat
;; Requires: hamlst is nonempty,
;;           lst has the same length as each (listof Sym) in hamlst.
(define (min-length lst hamlst minlength)
  (cond
    [(empty? hamlst) minlength]
    [(> minlength (hamming-distance lst (second (first hamlst))))
     (min-length lst (rest hamlst)
                 (hamming-distance lst (second (first hamlst))))]
    [else (min-length lst (rest hamlst) minlength)]))
