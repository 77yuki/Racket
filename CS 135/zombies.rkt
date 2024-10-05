;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname zombies) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 10, Problem 1
;; ***************************************************
;;


;; A Location is a Nat


;; A Town is a (listof (list Location (listof Location)))
;; Requires: Town represents a valid graph as defined in Module 16
;;           Towns have at least one location and that every location
;;           has at least one route out


;; A Horde is a (listof (list Location Nat))
;; Requires: Hordes are non-empty


;; Useful constant for examples and testing
(define waterloo '((0 (1 2 3))
                   (1 (2 3))
                   (2 (0 4))
                   (3 (1))
                   (4 (5))
                   (5 (3))))



;;
;; Exercise I
;;
;; (infect town zombies) produces a horde with many zombies at each
;;   location according to the given town and zombies
;; Examples:
(check-expect (infect waterloo 1000)
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))
(check-expect (infect waterloo 240)
              (list (list 0 240) (list 1 240) (list 2 240)
                    (list 3 240) (list 4 240) (list 5 240)))

;; infect: Town Nat -> Horde
(define (infect town zombies)
  (cond [(empty? town) empty]
        [else (cons (list (first (first town)) zombies)
                    (infect (rest town) zombies))]))

;; Tests:
(check-expect (infect empty 100) empty)
(check-expect (infect waterloo 0)
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))




;;
;; Exercise II
;;
;; (sink horde) produces a list of two elements, the first element is
;;   the total number of zombies that sink into the earth; the second
;;   element is the horde after those zombies have sunken into the earth
;;   according to the given horde
;; Examples:
(check-expect (sink (infect waterloo 1000))
              (list 300 (list (list 0 950) (list 1 950) (list 2 950)
                              (list 3 950) (list 4 950) (list 5 950))))
(check-expect (sink (infect waterloo 38))
              (list 12 (list (list 0 36) (list 1 36) (list 2 36)
                             (list 3 36) (list 4 36) (list 5 36))))

;; sink: Horde -> (list Nat Horde)
(define (sink horde)
  (local [;; (i h) produces the total number of 5% of the zombies at
           ;;   each location of h
           ;; i: Horde -> Nat
           (define (i h)
             (cond [(empty? h) empty]
                   [else (cons
                          (list (first (first h))
                                (round (* (second (first h)) 0.95)))
                          (i (rest h)))]))
          
           ;; (b h) produces the horde after those zombies (5% of the
           ;;   zombies at each location of h) have sunken into the earth
           ;; b: Horde -> Horde
           (define (b h) (cond [(empty? h) 0]
                              [else (+ (round (* (second (first h)) 0.05))
                                       (b (rest h)))]))]
    
    (list (b horde) (i horde))))

;; Tests:
(check-expect (sink (infect waterloo 0))
              (list 0 (list (list 0 0) (list 1 0) (list 2 0)
                            (list 3 0) (list 4 0) (list 5 0))))
(check-expect (sink (infect waterloo 240))
              (list 72 (list (list 0 228) (list 1 228) (list 2 228)
                             (list 3 228) (list 4 228) (list 5 228))))
(check-expect (sink (infect waterloo 29))
              (list 6 (list (list 0 28) (list 1 28) (list 2 28)
                            (list 3 28) (list 4 28) (list 5 28))))
(check-expect (sink (list (list 0 475) (list 1 1267) (list 2 792)
                          (list 3 1741) (list 4 475) (list 5 950)))
              (list 286 (list (list 0 451) (list 1 1204) (list 2 752)
                              (list 3 1654) (list 4 451) (list 5 902))))



;;
;; Exercise III
;;
;; (apportion zombies n) produces a list of exactly n natural numbers
;;   that must add up to the number of zombies and the difference between
;;   any two numbers can't be greater than 1
;; Examples:
(check-expect (apportion 100 3) (list 34 33 33))
(check-expect (apportion 1 3) (list 1 0 0))

;; apportion: Nat Nat -> (listof Nat)
;; Requires: n > 0 
(define (apportion zombies n)
  (cond [(= (remainder zombies n) 0)
         (a1 zombies n)]
        [else (a2 zombies n)]))

;; Tests:
(check-expect (apportion 20 4) (list 5 5 5 5))
(check-expect (apportion 39 4) (list 10 10 10 9))
(check-expect (apportion 12 1) (list 12))
(check-expect (apportion 7 4) (list 2 2 2 1))
(check-expect (apportion 1267 2) (list 634 633))
    


;; (a1 zombies n) produces a list of exactly n natural numbers
;;   that must add up to the number of zombies and the difference between
;;   any two numbers can't be greater than 1
;; Examples:
(check-expect (a1 99 3) (list 33 33 33))
(check-expect (a1 20 5) (list 4 4 4 4 4))

;; a1: Nat Nat -> (listof Nat)
;; Requires: zombies is the multiple of n
(define (a1 zombies n)
  (local [(define e (/ zombies n))

          ;; (s n1) produces a list of exactly n natural numbers that
          ;;   must add up to the number of zombies and the difference between
          ;;   any two numbers can't be greater than 1
          ;; s: Nat -> (listof Nat)
          (define (s n1)
            (cond [(= n1 0) empty]
                  [else (cons e (s (- n1 1)))]))]
    
    (s n)))



;; (a2 zombies n) produces a list of exactly n natural numbers
;;   that must add up to the number of zombies and the difference between
;;   any two numbers can't be greater than 1
;; Examples:
(check-expect (a2 100 3) (list 34 33 33))
(check-expect (a2 1 3) (list 1 0 0))
(check-expect (a2 2 10) (list 1 1 0 0 0 0 0 0 0 0))

;; a2: Nat Nat -> (listof Nat)
;; Requires: zombies is not a multiple of n
(define (a2 zombies n)
  (cond [(= (remainder zombies n) 1)
         (local [(define e2 (floor (/ zombies n)))
                 
                 ;; (s2 n2) produces a list of natural numbers and the total
                 ;;   number of these numbers is n2 and these numbers are
                 ;;   all the result of e2 plus 1
                 ;; s2: Nat -> (listof Nat)
                 (define (s2 n2)
                   (cond [(= n2 1) empty]
                         [else (cons e2 (s2 (- n2 1)))]))]
           
           (cons (+ e2 1) (s2 n)))]
        [else
         (local [(define e3 (floor (/ zombies n)))

                 ;; (s3 n3) produces a list of natural numbers and the total
                 ;;    number of these numbers is n3 minus the remainder of
                 ;;    zombies divided by n and these numbers are all e3
                 ;; s3: Nat -> (listof Nat)
                 (define (s3 n3)
                   (cond [(= n3 (remainder zombies n)) empty]
                         [else (cons e3 (s3 (- n3 1)))]))

                 ;; (distribute n4) produces a list of natural numbers and the
                 ;;    total number of these numbers is n4 and these numbers
                 ;;    are all the result of e3 plus 1
                 (define (distribute n4)
                   (cond [(= n4 0) empty]
                         [else (cons (+ e3 1) (distribute (- n4 1)))]))]
           
           (append (distribute (remainder zombies n)) (s3 n)))]))



;;
;; Exercise IV
;;
;; (shamble town horde) produces the horde that results from all the
;;   zombies at each location apportioning themselves into nearly equal
;;   groups and shambling along the edges connceting the locations according
;;   to the given town and horde
;; Example:
(check-expect (shamble waterloo (second (sink (infect waterloo 1000))))
              (list (list 0 475) (list 1 1267) (list 2 792)
                    (list 3 1741) (list 4 475) (list 5 950)))

;; shamble: Town Horde -> Horde
(define (shamble town horde)
  (local [(define original 0)

          ;; (find-sum p a n) produces the sum of all elements of a, which
          ;;     corresponds to the elements of p when p equals to n
          ;; find-sum: Town Horde Nat -> Nat
          (define (find-sum p a n)
            (cond [(empty? p) 0]
                  [(= (first p) n) (+ (first a) (find-sum (rest p) (rest a) n))]
                  [else (find-sum (rest p) (rest a) n)]))

          ;; (s t h n o) produces the horde that results from all the
          ;;   zombies at each location apportioning themselves into nearly
          ;;   equal groups and shambling along the edges connceting the locations
          ;;   according to given t, h, n and o 
          ;; s: Town Horde Nat Nat -> Horde
          (define (s t h n o)
            (cond [(= o n) empty]
                  [else (cons (list o (find-sum t h o))
                              (s t h n (+ o 1)))]))]
    
    (s (change-form (extract-path town)) (change-form (put-apportion town horde))
       (length town) original)))

;; Test:
(check-expect (shamble waterloo (list (list 0 475) (list 1 1267) (list 2 792)
                                      (list 3 1741) (list 4 475) (list 5 950)))
              (list (list 0 396) (list 1 1900) (list 2 792)
                    (list 3 1741) (list 4 396) (list 5 475)))
(check-expect (shamble waterloo (list (list 0 24) (list 1 30) (list 2 14)
                                      (list 3 79) (list 4 56) (list 5 43)))
              (list (list 0 7) (list 1 87) (list 2 23)
                    (list 3 66) (list 4 7) (list 5 56)))



;; (extract-path t) extracts all Locations that are out-neighbours in t
;; Example:
(check-expect (extract-path waterloo)
              (list (list 1 2 3) (list 2 3) (list 0 4) (list 1) (list 5) (list 3)))

;; extract-path: Town -> (listof Location)
(define (extract-path t)
  (cond [(empty? t) empty]
        [else (cons (second (first t))
                    (extract-path (rest t)))]))



;; (put-apportion to h) produces a list of lists, which exactly contains
;;    corresponding results of using apportion on each second element of h
;;    and the length of each second element of t
;; Example:
(check-expect (put-apportion waterloo (second (sink (infect waterloo 1000))))
              (list (list 317 317 316) (list 475 475) (list 475 475)
                    (list 950) (list 950) (list 950)))

;; put-apportion: Town Horde -> Horde
(define (put-apportion to h)
  (cond [(empty? h) empty]
        [else
         (cons (apportion (second (first h)) (length (second (first to))))
                          (put-apportion (rest to) (rest h)))]))



;; (change-form lst) produces a list of natural numbers according to the
;;    given lst
;; Example:
(check-expect (change-form (list (list 1 2 3) (list 2 3) (list 0 4)
                                 (list 1) (list 5) (list 3)))
              (list 1 2 3 2 3 0 4 1 5 3))

;; change-form: (listof (listof Nat)) -> (listof Nat)
(define (change-form lst)
  (cond [(empty? lst) empty]
        [else
         (cond [(list? (first lst)) (append (change-form (first lst))
                                            (change-form (rest lst)))]
               [else (cons (first lst) (change-form (rest lst)))])]))



;;
;; Exercise V
;;
;; (rise zombies horde) produces a new horde with those zombies added
;;   to the horde, apportioned as equally as possible between the locations
;; Examples:
(check-expect (rise 300 (shamble waterloo (second (sink (infect waterloo 1000)))))
              (list (list 0 525) (list 1 1317) (list 2 842)
                    (list 3 1791) (list 4 525) (list 5 1000)))
(check-expect (rise 30 (shamble waterloo (second (sink (infect waterloo 1000)))))
              (list (list 0 480) (list 1 1272) (list 2 797)
                    (list 3 1746) (list 4 480) (list 5 955)))

;; rise: Nat Horde -> Horde
(define (rise zombies horde)
  (local [;; (produce e h) produces a new horde with e added to each
          ;;    second element of h
          ;; produce: Nat Horde -> Horde
          (define (produce e h)
            (cond [(empty? e) empty]
                  [else
                   (cons (list (first (first h))
                               (+ (first e) (second (first h))))
                         (produce (rest e) (rest h)))]))]
    
    (produce (apportion zombies (length horde)) horde)))

;; Tests:
(check-expect (rise 300 (shamble waterloo (list (list 0 475) (list 1 1267) (list 2 792)
                                                (list 3 1741) (list 4 475) (list 5 950))))
              (list (list 0 446) (list 1 1950) (list 2 842)
                    (list 3 1791) (list 4 446) (list 5 525)))
(check-expect (rise 0 (shamble waterloo (second (sink (infect waterloo 1000)))))
              (list (list 0 475) (list 1 1267) (list 2 792)
                    (list 3 1741) (list 4 475) (list 5 950)))



;;
;; Exercise VI
;;
;; (night town horde) produces a new horde after the horrors of a single
;;   night have passed according to the given town and horde
;; Examples:
(check-expect (night waterloo (infect waterloo 1000))
              (list (list 0 525) (list 1 1317) (list 2 842)
                    (list 3 1791) (list 4 525) (list 5 1000)))
(check-expect (night waterloo (list (list 0 475) (list 1 1267) (list 2 792)
                                    (list 3 1741) (list 4 475) (list 5 950)))
              (list (list 0 424) (list 1 1853) (list 2 800)
                    (list 3 1702) (list 4 423) (list 5 498)))

;; night: Town Horde -> Horde
(define (night town horde)
  (local [(define s (first (sink horde)))
          
          (define r (second (sink horde)))]
    
    (rise s (shamble town (second (sink horde))))))

;; Tests:
(check-expect (night waterloo (infect waterloo 23))
              (list (list 0 12) (list 1 31) (list 2 19)
                    (list 3 41) (list 4 12) (list 5 23)))
(check-expect (night waterloo (infect waterloo 203))
              (list (list 0 107) (list 1 268) (list 2 171)
                    (list 3 363) (list 4 106) (list 5 203)))
(check-expect (night waterloo (infect waterloo 11))
              (list (list 0 6) (list 1 15) (list 2 9)
                    (list 3 19) (list 4 6) (list 5 11)))



;;
;; Exercise VII
;;
;; (apocalypse town infection nights) produces the horde after that many
;;   nights have passed according to given town, infection and nights
;; Exmaples:
(check-expect (apocalypse waterloo 1000 3)
              (list (list 0 450) (list 1 1894) (list 2 1104)
                    (list 3 1625) (list 4 450) (list 5 477)))
(check-expect (apocalypse waterloo 1000 28)
              (list (list 0 545) (list 1 1723) (list 2 1042)
                    (list 3 1578) (list 4 545) (list 5 567)))

;; apocalypse: Town Nat Nat -> Horde
(define (apocalypse town infection nights)
  (local [(define orig (infect town infection))

          ;; (apo t h n) produces the horde after that many n have passed
          ;;   according to given t, h and n
          ;; apo: Town Horde Nat -> Horde
          (define (apo t h n)
            (cond [(= n 0) h]
                  [else (apo t (night t h) (- n 1))]))]
    
    (apo town orig nights)))

;; Tests:
(check-expect (apocalypse waterloo 382 1)
              (list (list 0 201) (list 1 503) (list 2 322)
                    (list 3 684) (list 4 200) (list 5 382)))
(check-expect (apocalypse waterloo 275 10000)
              (list (list 0 151) (list 1 474) (list 2 286)
                    (list 3 433) (list 4 150) (list 5 156)))
