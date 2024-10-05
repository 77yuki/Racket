;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tttttt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Examples:
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local [(define trees (build-dt examples label))
          (define (pred? lst)
            (local [(define (exist lst tree)
                      (cond [(not (list? tree)) tree]
                            [else
                             (cond [(empty? lst) (exist lst (third tree))]
                                   [else
                                    (cond [(equal? (first tree) (first lst))
                                           (exist (rest lst) (second tree))]
                                          [else (exist (rest lst) (third tree))])])]))]
              (exist lst trees)))]
    pred?))

;; constants for testing Examples:
(define goose? (train-classifier (random-animals 1000) 'goose))



;;
;; ************************************
;;     Yuqi Gu (20884580)
;;     CS 135 Fall 2020
;;     Assignment 07, Problem 2
;; ************************************
;;


(require "animals.rkt")
;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)


(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies)
   (list 'crow 'medium 'angry)
   (list 'sparrow 'small 'flies)))
(define seen2
  (list
   (list 'goose 'large 'swims 'angry)
   (list 'goose 'medium 'swims 'angry)
   (list 'crow 'large 'angry)))


;; (all-attributes examples) produces all attributes in examples
;; Examples:
(check-expect (all-attributes seen)
              (list 'small 'angry 'large 'swims 'flies 'medium 'angry
                    'small 'flies))

;; all-attributes: (listof Example) -> (listof Sym)
(define (all-attributes examples)
          (cond [(empty? examples) empty]
                [else (append (rest (first examples))
                              (all-attributes (rest examples)))]))


;;
;; a)
;;
(define original-attributes empty)


;; (collect-attributes examples) produces a list of attributes contained
;;   in the examples with no duplicates
;; Examples:
(check-expect (collect-attributes seen) (list 'small 'angry 'large
                                              'swims 'flies 'medium))

;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local
    [(define (collect-attributes/final all final)
       (cond [(empty? all) final]
             [else (cond [(member? (first all) final)
                          (collect-attributes/final (rest all) final)]
                         [else
                          (collect-attributes/final
                           (rest all) (append final (list (first all))))])]))]
    (collect-attributes/final (all-attributes examples) original-attributes)))

;; Tests:
(check-expect (collect-attributes (list
                                   (list 'squirrel 'small 'angry)
                                   (list 'crow 'large 'angry)))
              (list 'small 'angry 'large))
(check-expect (collect-attributes empty) empty)


;;
;; b)
;;
;; (split-examples examples symbol) splits the examples on symbol
;; Examples:
(check-expect (split-examples seen 'goose)
              (list
               (list
                (list 'goose 'large 'swims 'flies))
               (list
                (list 'squirrel 'small 'angry)
                (list 'crow 'medium 'angry)
                (list 'sparrow 'small 'flies))))
(check-expect (split-examples empty 'flies) (list empty empty))

;; split-examples: (listof Example) Sym -> (listof (listof Example))
(define (split-examples examples symbol)
  (local [(define (split-examples symbol)
            (local [(define (contain-sym? examples) (member? symbol examples))]
              contain-sym?))
          (define (not-split-examples symbol)
            (local [(define (contain-sym? examples)
                      (not (member? symbol examples)))]
              contain-sym?))]
    (cons
     (filter (split-examples symbol) examples)
     (cons (filter (not-split-examples symbol) examples)empty))))

;; Tests:
(check-expect (split-examples seen 'small)
              (list
               (list
                (list 'squirrel 'small 'angry)
                (list 'sparrow 'small 'flies))
               (list
                (list 'goose 'large 'swims 'flies)
                (list 'crow 'medium 'angry))))
(check-expect (split-examples seen2 'angry)
              (list
               (list
                (list 'goose 'large 'swims 'angry)
                (list 'goose 'medium 'swims 'angry)
                (list 'crow 'large 'angry))
               (list)))


;;
;; c)
;;
;; (histogram examples) produces a list of attribute/count pairs, with
;;   each pair indicating how many times that attribute appears in the
;;   examples
;; Examples:
(check-expect (histogram seen)
              (list
               (list 'small 2) (list 'angry 2) (list 'large 1)
               (list 'swims 1) (list 'flies 2) (list 'medium 1)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local
    [(define (count symbol attributes)
      (cond [(empty? attributes) 0]
            [else (cond [(equal? symbol (first attributes))
                         (+ 1 (count symbol (rest attributes)))]
                        [else (count symbol (rest attributes))])]))
    (define (histogram/att attributes collect-att)
      (cond [(empty? collect-att) empty]
            [else (cons (list (first collect-att)
                              (count (first collect-att) attributes))
                        (histogram/att attributes (rest collect-att)))]))]
    (histogram/att (all-attributes examples) (collect-attributes examples))))

;; Tests:
(check-expect (histogram seen2)
              (list
               (list 'large 2) (list 'swims 2) (list 'angry 3)
               (list 'medium 1)))
(check-expect (histogram empty) empty)
(check-expect (histogram (list (list 'cow))) empty)


;;
;; d)
;;
;; (augment-histogram histogram attributes total) augments histogram
;;   with attributes and total according to what the question states
;; Examples:
(check-expect (augment-histogram (list (list 'a 100) (list 'c 20))
                                 (list 'a 'b 'c 'd) 200)
              (list (list 'a 100 100) (list 'b 0 200) (list 'c 20 180)
                    (list 'd 0 200)))
(check-expect (augment-histogram (list (list 'x 20) (list 'y 34))
                                 (list 'x 'y 'z) 56)
              (list (list 'x 20 36) (list 'y 34 22) (list 'z 0 56)))

;; augment-histogram: Histogram (listof Sym) Nat -> AH
(define (augment-histogram histogram attributes total)
  (cond [(empty? attributes) empty]
        [(empty? histogram) (cons (list (first attributes) 0 total)
                                  (augment-histogram histogram (rest attributes) total))]
        [else (cond [(equal? (first attributes) (first (first histogram)))
                     (cons (list (first (first histogram))
                                 (second (first histogram))
                                 (- total (second (first histogram))))
                           (augment-histogram (rest histogram) (rest attributes) total))]
                    [else (cons (list (first attributes) 0 total)
                                (augment-histogram histogram (rest attributes) total))])]))

;; Tests:
(check-expect (augment-histogram empty empty 10) empty)
(check-expect (augment-histogram empty (list 'x 'y 'z) 100)
              (list (list 'x 0 100) (list 'y 0 100) (list 'z 0 100)))
(check-expect (augment-histogram (list (list 'b 24) (list 'c 30) (list 'd 67))
                                 (list 'a 'b 'c 'd) 100)
              (list (list 'a 0 100) (list 'b 24 76) (list 'c 30 70) (list 'd 67 33)))


;;
;; e)
;;
;; (entropy positive-counts negative-counts) produces the entropy of
;;    positive-counts and negative-counts according to the question
;; Examples:
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0.0 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361)) 0.582 0.001)

;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) -> Num
;; Requires: positive-counts and negative-counts are non-empty
(define (entropy positive-counts negative-counts)
  (local [(define (p n m)
            (cond [(> (+ n m) 0) (/ n (+ n m))]
                  [(= (+ n m) 0) 0.5]))
          (define (e p)
            (cond [(and (< 0 p) (<= p 1)) (-(* p (log p 2)))]
                  [(= p 0) 0]))
          (define a (second positive-counts))
          (define b (second negative-counts))
          (define c (third positive-counts))
          (define d (third negative-counts))]
    (+ (* (p (+ a b) (+ c d)) (+ (e (p a b)) (e (p b a))))
       (* (p (+ c d) (+ a b)) (+ (e (p c d)) (e (p d c)))))))

;; Tests:
(check-within (entropy (list 'large 126 59) (list 'large 146 669)) 0.566 0.001)
 

;;
;; f)
;;
;; (entropy-attributes positive negative) computes the entropy of each
;;   attribute, producing a list of attribute/entropy pairs according 
;;   to positive and negative
;; Examples:
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large 0.566) (list 'angry 0.645)
               (list 'small 0.582) (list 'flies 0.670)
               (list 'swims 0.602) (list 'medium 0.690)) 0.001)

;; entropy-attributes: AH AH -> EAL
;; Requires: The same attributes will be included in both positive and
;;           negative and the attributes will appear in the same oder
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))


;;
;; g)
;;
;; (best-attribute entropies) produces the attribute with the minimum
;;   entropy in entropies
;; Examples:
(check-expect (best-attribute (list
                               (list 'large 0.566) (list 'angry 0.645)
                               (list 'small 0.582) (list 'flies 0.670)
                               (list 'swims 0.602) (list 'medium 0.690)))
              'large)

;; best-attribute: EAL -> Sym
(define (best-attribute entropies)
  (local [(define (smallest-entropy entropies small attr)
          (cond [(empty? entropies) attr]
                [else (cond
                        [(< (second (first entropies)) small)
                         (smallest-entropy (rest entropies)
                                           (second (first entropies))
                                           (first (first entropies)))]
                        [else (smallest-entropy (rest entropies) small attr)])]))]
    (smallest-entropy (rest entropies) (second (first entropies))
                      (first (first entropies)))))

;; Tests:
(check-expect (best-attribute (list
                               (list 'angry 0.2036) (list 'flies 0.1235)
                               (list 'swims 0.3092) (list 'medium 0.215)))
              'flies)
(check-expect (best-attribute (list
                               (list 'small 0.2135) (list 'flies 0.3515)
                               (list 'swims 0.2536) (list 'medium 0.2135)))
              'small)


;;
;; h)
;;
;; (build-dt examples label) builds the decisiontress according to
;;   examples and label and what the question states

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local [(define list-of-attr (collect-attributes examples))
          (define currentexamples (split-examples examples label))
          (define list-of-po (first currentexamples))
          (define list-of-ne (second currentexamples))]
    (cond [(empty? list-of-attr)
           (cond [(> (length list-of-po) (length list-of-ne)) true]
                 [else false])]
          [else
           (local [(define root-attribute
                     (best-attribute
                      (entropy-attributes
                       (augment-histogram
                        (histogram list-of-po) list-of-attr (length examples))
                       (augment-histogram
                        (histogram list-of-ne) list-of-attr (length examples)))))
                   (define currentexamples2 (split-examples examples root-attribute))
                   (define list-of-with (first currentexamples2))
                   (define list-of-without (second currentexamples2))
                  (define (remove-element/one one sym)
                     (cond [(empty? one) empty]
                           [(equal? (first one) sym)
                            (rest one)]
                           [else (cons (first one) (remove-element/one (rest one) sym))]))
                  (define (remove-element currentlists sym)
                    (cond [(empty? currentlists) empty]
                          [else (cons (remove-element/one (first currentlists) sym)
                                      (remove-element (rest currentlists) sym))]))
                  (define list-of-with-current (remove-element list-of-with root-attribute))]
             (local [(define subtree1 (build-dt list-of-with-current label))
                     (define subtree2 (build-dt list-of-without label))]
               (cond [(equal? subtree1 subtree2) subtree1]
                     [else (list root-attribute subtree1 subtree2)])
                    ))])))