;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 04, Problem 3
;; ***************************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;; 3a)
;; These are helper functions. See assignment for design recipe requirements.

;;
;; 3ai)
;;
;; (get-x Point) produces the x-coordinate of Point 
;; Examples:
(check-expect (get-x (list 0 100)) 0)

;; get-x: Point -> Num
(define (get-x Point) (first Point))

;; (get-y Point) produces the y-coordinate of Point
;; Examples:
(check-expect (get-y (list 0 100)) 100)

;; get-y: Point -> Num
(define (get-y Point) (second Point))


;;
;; 3aii)
;;
;; (translate-gesture Gesture x-offset y-offset) produces a new gesture
;;   such that each Point((list x y)) in Gesture now has value
;;   (list (+ x x-offset) (+ y y-offset)) in the new gesture
;; Examples:
(check-expect (translate-gesture (list (list 20 100) (list 70 20)) 20 10)
              (list (list 40 110) (list 90 30)))
(check-expect (translate-gesture empty 5 6) empty)

;; translate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture Gesture x-offset y-offset)
  (cond [(empty? Gesture) empty]
        [(cons? Gesture)
         (cons (cons (+ (get-x (first Gesture)) x-offset)
                     (cons (+ (get-y (first Gesture)) y-offset) empty))
               (translate-gesture (rest Gesture) x-offset y-offset))]))


;;
;; 3aiii)
;;
;; (scale-gesture Gesture x-scale y-scale) produces a new stroke such
;;   that each Point((list x y)) in the Gesture now has value
;;   (list (* x x-scale) (* y y-scale)) in the new gesture
;; Examples:
(check-expect (scale-gesture (list (list 20 90) (list 10 23)) 2 4)
              (list (list 40 360) (list 20 92)))
(check-expect (scale-gesture empty 3 6) empty)

;; scale-gesture: Gesture Num Num -> Gesture
;; Requires:
;; x-scale > 0
;; y-scale > 0
(define (scale-gesture Gesture x-offset y-offset)
  (cond [(empty? Gesture) empty]
        [(cons? Gesture)
         (cons (cons (* (get-x (first Gesture)) x-offset)
                     (cons (* (get-y (first Gesture)) y-offset) empty))
               (scale-gesture (rest Gesture) x-offset y-offset))]))


;;
;; 3aiv)
;;
;; (get-b-box Gesture) produces Gesture's BoundingBox, which includes
;;   two points, the top left point located at point (list min-x min-y)
;;   and the bottom right point located at (list max-x max-y)
;; Examples:
(check-expect (get-b-box (list (list 100 0) (list 200 100) (list 100 200)
                               (list 0 100)))
              (list (list 0 0) (list 200 200)))
(check-expect (get-b-box (list (list 39.1 20.13) (list 38.24 89.22)
                               (list 23 100)))
              (list (list 23 20.13) (list 39.1 100)))
(check-expect (get-b-box (list (list 12 30)))
              (list (list 12 30) (list 12 30)))
(check-expect (get-b-box (list (list 23 10) (list 30 10) (list 50 10)))
              (list (list 23 10) (list 50 10)))

;; get-b-box: Gesture -> BoundingBox
;; Requires:
;; Gesture is a non-empty list
(define (get-b-box Gesture)
  (cond [(= (length Gesture) 1) (list (first Gesture) (first Gesture))]
        [else (list (list (min-x-constant (get-x (first Gesture)) (rest Gesture))
                          (min-y-constant (get-y (first Gesture)) (rest Gesture)))
                    (list (max-x-constant (get-x (first Gesture)) (rest Gesture))
                          (max-y-constant (get-y (first Gesture)) (rest Gesture))))]))


;; (min-x-constant min-constant Gesture)produces the smallest value of
;;   x-coordinate in the given Gesture compared with min-constant
;; Examples:
(check-expect (min-x-constant 100 (list (list 200 100) (list 100 200)
                                        (list 0 100)))
              0)
(check-expect (min-x-constant 500 (list (list 30.5 20) (list 80 20)))
              30.5)

;; min-x-constant: Num Gesture -> Num
;; Requires:
;; Gesture is a non-empty list
(define (min-x-constant min-constant Gesture)
  (cond
    [(= (length Gesture) 1)
     (cond [(> min-constant (get-x (first Gesture)))
            (get-x (first Gesture))]
           [else min-constant])]
    [(> min-constant (get-x (first Gesture)))
     (min-x-constant (get-x (first Gesture)) (rest Gesture))]
    [else (min-x-constant min-constant (rest Gesture))]))


;; (min-y-constant min-constant Gesture)produces the smallest value of
;;   y-coordinate in the given Gesture compared with min-constant
;; Examples:
(check-expect (min-y-constant 0 (list (list 200 100) (list 100 200)
                                        (list 0 100)))
              0)
(check-expect (min-y-constant 80 (list (list 12.19 78.1) (list 100 50)))
              50)

;; min-y-constant: Num Gesture -> Num
;; Requires:
;; Gesture is a non-empty list
(define (min-y-constant min-constant Gesture)
  (cond
    [(= (length Gesture) 1)
     (cond [(> min-constant (get-y (first Gesture)))
            (get-y (first Gesture))]
           [else min-constant])]
    [(> min-constant (get-y (first Gesture)))
     (min-y-constant (get-y (first Gesture)) (rest Gesture))]
    [else (min-y-constant min-constant (rest Gesture))]))


;; (max-x-constant max-constant Gesture)produces the largest value of
;;   x-coordinate in the given Gesture compared with max-constant
;; Examples:
(check-expect (max-x-constant 100 (list (list 200 100) (list 100 200)
                                        (list 0 100)))
              200)
(check-expect (max-x-constant 20 (list (list 100 40.89) (list 899 10)))
              899)

;; max-x-constant: Num Gesture -> Num
;; Requires:
;; Gesture is a non-empty list
(define (max-x-constant max-constant Gesture)
  (cond
    [(= (length Gesture) 1)
     (cond [(> max-constant (get-x (first Gesture)))
            max-constant]
           [else (get-x (first Gesture))])]
    [(> max-constant (get-x (first Gesture)))
     (max-x-constant max-constant (rest Gesture))]
    [else (max-x-constant (get-x (first Gesture)) (rest Gesture))]))


;; (max-y-constant max-constant Gesture)produces the largest value of
;;   y-coordinate in the given Gesture compared with max-constant
;; Examples:
(check-expect (max-y-constant 0 (list (list 200 100) (list 100 200)
                                        (list 0 100)))
              200)
(check-expect (max-y-constant 100.9 (list (list 200 87.3) (list 3 110)))
              110)

;; max-y-constant: Num Gesture -> Num
;; Requires:
;; Gesture is a non-empty list
(define (max-y-constant max-constant Gesture)
  (cond
    [(= (length Gesture) 1)
     (cond [(> max-constant (get-y (first Gesture)))
            max-constant]
           [else (get-y (first Gesture))])]
    [(> max-constant (get-y (first Gesture)))
     (max-y-constant max-constant (rest Gesture))]
    [else (max-y-constant (get-y (first Gesture)) (rest Gesture))]))



;; 3b)
;; Full design recipe required.

;;
;; 3bi)
;;
;; (gesture-length Gesture) produces the length of Gesture, which is
;;   the sum of the distances between adjacent points in the Gesture,
;;   according to the Euclidean formula
;; Examples:
(check-within (gesture-length (list (list 100 200) (list 10 20)
                                    (list 30 70)))
              255.1 0.01)

;; gesture-length: Gesture -> Num
(define (gesture-length Gesture)
  (cond [(< (length Gesture) 2) 0]
        [(= (length Gesture) 2)
         (distance (first Gesture) (second Gesture))]
        [else (+ (distance (first Gesture) (second Gesture))
                 (gesture-length (rest Gesture)))]))

;; Tests:
(check-within (gesture-length (list (list 100 200) (list 10 20)))
              201.25 0.01)
(check-expect (gesture-length (list (list 7.25 0.34))) 0)
(check-within (gesture-length (list (list 30 10.5) (list 24 36)
                                    (list 16 14) (list 43 36.2)))
              84.56 0.01)


;; (distance p1 p2) produces the distance betwenn p1 and p2 according to
;;   the Euclidean formula
;; Examples:
(check-within (distance (list 100 200) (list 10 20)) 201.25 0.01)
(check-within (distance (list 20 5) (list 3.4 1)) 17.08 0.01)

;; distance: Point Point -> Num
;; p1 and p2 are non-empty lists
(define (distance p1 p2)
  (sqrt (+ (sqr (- (first p2) (first p1)))
           (sqr (- (second p2) (second p1))))))


;;
;; 3bii)
;;
(define mygest (list (list 100 0) (list 200 100) (list 100 200)
                     (list 0 100) (list 100 50)))
(define mygest1 (list (list 3 10) (list 2 5.7) (list 1 6)))
(define mygest2 (list (list 2.3 6.1)))

;; (get-points g lst) produces a Gesture where each Point in the 
;;   produced Gesture is indexed by one element of the lst (the list
;;   of Nat consumed) based on g
;; Examples:
(check-expect (get-points mygest (list 0 0 2 4 4))
              (list (list 100 0) (list 100 0) (list 100 200)
                    (list 100 50) (list 100 50)))

;; get-points: Gesture (listof Nat) -> Gesture
;; Requires:
;; g is a non-empty list
;; lst is a non-decreasing list of Nat in the range [0...(n-1)], where
;;   n is the number of Points in g
(define (get-points g lst)
  (cond
    [(empty? lst) empty]
    [else (get-points-helper g lst (first lst))]))

;; Tests:
(check-expect (get-points mygest1 (list 2 2))
              (list (list 1 6) (list 1 6)))
(check-expect (get-points mygest2 (list 0)) (list (list 2.3 6.1)))
(check-expect (get-points mygest empty) empty)


;; (get-points-helper g lst first-lst) produces a Gesture where each
;;   Point in the produced Gesture is indexed by one element of the
;;   lst (the list of Nat consumed) based on g with the help of first-lst
;; Examples:
(check-expect (get-points-helper mygest (list 0 0 2 4 4) 0)
              (list (list 100 0) (list 100 0) (list 100 200)
                    (list 100 50) (list 100 50)))
(check-expect (get-points-helper (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)) (list 0 1 2 3 4) 0)
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))

;; get-points-helper: Gesture (listof Nat) Nat -> Gesture
;; Requires:
;; g is a non-empty list
;; lst is a non-decreasing list of Nat in the range [0...(n-1)], where
;;   n is the number of Points in g
;; first-lst is the first element in lst
(define (get-points-helper g lst first-lst)
  (cond
    [(empty? (rest lst)) (cons (last-element g) empty)]
    [(= first-lst 0)
     (cons (first g) (get-points-helper g (rest lst) (first (rest lst))))]
    [(> first-lst 0)
     (get-points-helper (rest g) (subtract-lst 1 lst) (- first-lst 1))]))


;; (subtract-lst num lst) produces a new list, which includes each
;;    difference between each element in lst and num
;; Examples:
(check-expect (subtract-lst 2 (list 2 4 4)) (list 0 2 2))

;; subtract-lst: Nat (listof Nat) -> (listof Nat)
(define (subtract-lst num lst)
  (cond [(empty? lst) empty]
        [else (cons (- (first lst) num) (subtract-lst num (rest lst)))]))


;; (last-element g) produces the last element of g
;; Examples:
(check-expect (last-element (list (list 10 2) (list 0 6) (list 7 2.5)))
              (list 7 2.5))

;; last-element: Gesture -> Point
;; Requires:
;; g is a non-empty list
(define (last-element g)
  (cond [(= (length g) 1) (first g)]
        [else (last-element (rest g))]))



;; 3c) Starter code definitions

;; 3ci)
;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample gesture)
  (get-points gesture (list 0 (floor (* 0.25 (length gesture)))
                            (floor (* 0.5 (length gesture)))
                            (floor (* 0.75 (length gesture)))
                            (- (length gesture) 1))))

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))




;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture (translate-gesture gesture (- (get-x (first (get-b-box gesture))))
                     (- (get-y (first (get-b-box gesture))))) x-scale y-scale))

;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))





;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)
(define not-change 1)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)
              
;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
  (cond [(equal? (get-b-box gesture) (list (list 0 0) (list 200 200))) gesture]
        [(< (- (first (second (get-b-box gesture)))
               (first (first (get-b-box gesture)))) min-width)
         (move-and-scale gesture not-change
           (/ norm-size (second (second (get-b-box (translate-gesture gesture
                                         (- (get-x (first (get-b-box gesture))))
                                         (- (get-y (first (get-b-box gesture))))))))))]
        [(< (- (second (second (get-b-box gesture)))
               (second (first (get-b-box gesture)))) min-height)
         (move-and-scale gesture
           (/ norm-size (first (second (get-b-box (translate-gesture gesture
                                        (- (get-x (first (get-b-box gesture))))
                                        (- (get-y (first (get-b-box gesture)))))))))
           not-change)]
        [else (move-and-scale gesture
                (/ norm-size (first (second (get-b-box (translate-gesture gesture
                                             (- (get-x (first (get-b-box gesture))))
                                             (- (get-y (first (get-b-box gesture)))))))))
                (/ norm-size (second (second (get-b-box (translate-gesture gesture
                                              (- (get-x (first (get-b-box gesture))))
                                              (- (get-y (first (get-b-box gesture))))))))))]))



;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 200 200)))
              (list (list 0 0) (list 200 200)) 0.01)




;; 3civ)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
;;           gesture1 and gesture2 are non-empty
(define (geometric-5match gesture1 gesture2)
  (/ (+ (distance (first (normalize-gesture (five-sample gesture1)))
                  (first (normalize-gesture (five-sample gesture2))))
        (distance (second (normalize-gesture (five-sample gesture1)))
                  (second (normalize-gesture (five-sample gesture2))))
        (distance (third (normalize-gesture (five-sample gesture1)))
                  (third (normalize-gesture (five-sample gesture2))))
        (distance (fourth (normalize-gesture (five-sample gesture1)))
                  (fourth (normalize-gesture (five-sample gesture2))))
        (distance (fifth (normalize-gesture (five-sample gesture1)))
                  (fifth (normalize-gesture (five-sample gesture2)))))
     5))

;; Tests:
(check-within (geometric-5match
               (list (list 10 10) (list 20 5))
               (list (list 35 10) (list 40 20)))
               200.04 0.01)
(check-within (geometric-5match
               (list (list 200 2) (list 14 50) (list 100 0) (list 24 50)
                     (list 100 60) (list 12 53))
               (list (list 14 60) (list 10 60) (list 200 200) (list 10 68)
                     (list 102 60) (list 10 0)))
              118.60 0.01)



;; 3cv)
(define testz (list (list 81 68.5) (list 82 68.5) (list 83 68.5) (list 85 68.5) (list 86 68.5) (list 88 68.5) (list 90 68.5) (list 104 68.5) (list 119 68.5) (list 131 68.5) (list 155 68.5) (list 160 68.5) (list 162 67.5) (list 167 66.5) (list 169 66.5) (list 174 65.5) (list 181 65.5) (list 186 65.5) (list 210 65.5) (list 218 65.5) (list 233 65.5) (list 238 65.5) (list 238 68.5) (list 238 72.5) (list 236 75.5) (list 229 87.5) (list 220 100.5) (list 215 109.5) (list 201 130.5) (list 199 133.5) (list 192 143.5) (list 186 151.5) (list 183 154.5) (list 174 170.5) (list 171 176.5) (list 168 182.5) (list 163 192.5) (list 159 198.5) (list 158 205.5) (list 153 214.5) (list 150 220.5) (list 146 227.5) (list 140 242.5) (list 138 245.5) (list 134 254.5) (list 132 258.5) (list 130 262.5) (list 122 268.5) (list 117 273.5) (list 113 278.5) (list 107 284.5) (list 105 285.5) (list 102 288.5) (list 100 292.5) (list 99 294.5) (list 96 297.5) (list 96 298.5) (list 96 298.5) (list 98 298.5) (list 101 298.5) (list 110 298.5) (list 129 298.5) (list 141 299.5) (list 184 302.5) (list 206 302.5) (list 220 302.5) (list 237 302.5) (list 240 302.5) (list 241 302.5) (list 242 302.5) (list 244 302.5) (list 250 302.5) (list 260 302.5) (list 263 303.5) (list 264 303.5) (list 265 303.5) (list 267 303.5) (list 275 303.5) (list 284 302.5) (list 286 302.5) (list 290 302.5) (list 291 302.5) (list 292 302.5) (list 293 302.5)))

;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)

;; five-point-rec: Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec candidate template-library)
  (five-point-rec-helper
   (geometric-5match candidate (second (first template-library)))
   (first (first template-library))
   candidate
   (rest template-library)))

;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)
(check-expect (five-point-rec testa templates) 'a)
(check-expect (five-point-rec testz templates) 'z)


;; (five-point-rec-helper min-distance c1 candidate template-library)
;;   produces the symbol in template-library closest to candidate with
;;   the help of min-distance, which is the average distance between
;;   two points on the candidate and (second (first template-library)),
;;   and c1, which is (first (first template-library))
;; Examples:
(check-expect (five-point-rec-helper
               (geometric-5match testy (second (first templates)))
               'a testy templates) 'y)
(check-expect (five-point-rec-helper
               (geometric-5match testz (second (first templates)))
               'a testz templates) 'z)

;; five-point-rec-helper: Num Sym Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec-helper min-distance c1 candidate template-library)
  (cond
    [(= (length template-library) 1)
     (cond [(> min-distance (geometric-5match candidate (second (first template-library))))
            (first (first template-library))]
           [else c1])]
    [(> min-distance (geometric-5match candidate (second (first template-library))))
     (five-point-rec-helper (geometric-5match candidate (second (first template-library)))
                            (first (first template-library))
                            candidate
                            (rest template-library))]
    [else (five-point-rec-helper min-distance c1 candidate (rest template-library))]))



;; 3d)

;;
;; 3di)
;;
(define first-position 0)

;;(sub-sample gesture k) produces a sampling of gesture k points
;;  the first, n/(k-1)th, 2n/(k-1)th, ... (k-2)n/(k-1)th and last point.
;; Examples:
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 3)
              (list (list 1 1) (list 2 2) (list 2 2)))
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)) 4)
              (list (list 1 1) (list 3 3) (list 6 6) (list 8 8)))
(check-expect (sub-sample (list (list 2 0) (list 24 5) (list 10 50)
                                  (list 21 5) (list 0 1)) 5)
              (list (list 2 0) (list 24 5) (list 10 50) (list 21 5) (list 0 1)))

;; sub-sample: Gesture Nat -> Gesture
;; requires: gesture is non-empty
;;           k is greater than 2
(define (sub-sample gesture k)
  (get-points gesture (position first-position gesture k)))


;; (position num gesture k) produces a list of numbers, representing
;;   the positions of each Point of sub-sample in the gesture according
;;   to k, with the help of num
;; Examples:
(check-expect (position first-position (list (list 1 1) (list 2 2)) 3)
              (list 0 1 1))

;; position: Nat Gesture Nat -> (listof Nat)
;; requires: num is first-position
;;           gesture is non-empty
;;           k is greater than 2
(define (position num gesture k)
  (cond
    [(= num 1) (cons (- (length gesture) 1) empty)]
    [else (cons (floor (* num (length gesture)))
                (position (+ (/ 1 (- k 1)) num) gesture k))]))


;;
;; 3dii)
;;

;;(geometric-match gesture1 gesture2 k) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)
               16.16 0.01)
(check-within (geometric-match
               (list (list 100 20) (list 27.1 60) (list 10 2) (list 0 6))
               (list (list 1 3) (list 10 6.88) (list 10.2 60) (list 10 6)) 3)
               200.50 0.01)

;; geometric-match: Gesture Gesture Nat -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
;;           gesture1 and gesture2 are non-empty
;;           k is greater than 2
(define (geometric-match gesture1 gesture2 k)
  (/ (sum-of-distance (normalize-gesture gesture1) (normalize-gesture gesture2) k) k))


;; (sum-of-distance gesture1 gesture2 k) produces the sum of distances
;;   between each pair of corresponding points in gesture1 and gesture2
;; Examples:
(check-within (sum-of-distance 
               (list (list 0 0) (list 200 200)) 
               (list (list 0 0) (list 0 200) (list 200 200)) 2)
              200 0.01)

;; sum-of-distance: Gesture Gesture Nat -> Num
(define (sum-of-distance gesture1 gesture2 k)
  (cond
    [(= k 1) (distance (first gesture1) (first gesture2))]
    [else (+ (distance (first (sub-sample gesture1 k))
                       (first (sub-sample gesture2 k)))
             (sum-of-distance (rest gesture1) (rest gesture2) (- k 1)))]))

;;
;; 3diii)
;;

;;(k-point-rec candidate template-library k) produces the symbol in
;;  template-library closest to candidate with the help of k
;; Examples:
(check-expect (k-point-rec testd templates 8) 'd)
(check-expect (k-point-rec testk templates 10) 'k)
(check-expect (k-point-rec testz templates 20) 'z)

;; k-point-rec: Gesture TL Nat -> Sym
;; requires: candidate is not both vertical and horizontal
;;           k is greater than 2
(define (k-point-rec candidate template-library k)
  (k-point-rec-helper
   (geometric-match candidate (second (first template-library)) k)
   (first (first template-library))
   candidate
   (rest template-library)
   k))


;; (k-point-rec-helper min-distance c1 candidate template-library k)
;;   produces the symbol in template-library closest to candidate with
;;   the help of min-distance, which is the average distance between
;;   two points on the candidate and (second (first template-library)),
;;   and c1, which is (first (first template-library))
;; Examples:
(check-expect (k-point-rec-helper 
               (geometric-match testy (second (first templates)) 10)
               'a testy templates 10) 'y)
(check-expect (k-point-rec-helper
               (geometric-match testz (second (first templates)) 6)
               'a testz templates 6) 'z)


;; k-point-rec-helper: Num Sym Gesture TL Nat -> Sym
;; requires: candidate is not both vertical and horizontal
;;           k is greater than 2
(define (k-point-rec-helper min-distance c1 candidate template-library k)
  (cond
    [(= (length template-library) 1)
     (cond [(> min-distance (geometric-match candidate (second (first template-library)) k))
            (first (first template-library))]
           [else c1])]
    [(> min-distance (geometric-match candidate (second (first template-library)) k))
     (k-point-rec-helper (geometric-match candidate (second (first template-library)) k)
                            (first (first template-library))
                            candidate
                            (rest template-library) k)]
    [else (k-point-rec-helper min-distance c1 candidate (rest template-library) k)]))
