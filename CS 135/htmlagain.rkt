;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htmlagain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 09, Problem 3
;; ***************************************************
;;

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))


;;
;; a)
;;
;; (tokenize str) produces a list of strings representing the opening
;;   tags, closing tags, and strings in the str
;; Examples:
(check-expect (tokenize  "<p><h1>Heading</h1>Text</p>")
              '("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "") empty)
(check-expect (tokenize "a b  c") '("a b  c"))

;; tokenize: Str -> (listof Str)
(define (tokenize str)
  (tokenize/loc (string->list str)))

;; Tests:
(check-expect (tokenize "<p>Text<h1>Hello</h1>Beautiful</p>")
              '("<p>" "Text" "<h1>" "Hello" "</h1>" "Beautiful" "</p>"))
(check-expect (tokenize "<h1></h1>")
              '("<h1>" "</h1>"))
(check-expect (tokenize "Waterloo Great") '("Waterloo Great"))



;; (tokenize/loc loc) produces a list of strings representing the opening
;;   tags, closing tags, and strings in the loc
;; Examples:
(check-expect (tokenize/loc (string->list "<p><h1>Heading</h1>Text</p>"))
              '("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize/loc (string->list "")) empty)
(check-expect (tokenize/loc (string->list "a b  c")) '("a b  c"))

;; tokenize/loc: (listof Char) -> (listof Str)
(define (tokenize/loc loc)
  (local [(define f (first-one loc))
          (define r (rest-lst loc))]
    (cond [(empty? loc) empty]
          [else (cons (list->string f)
                      (tokenize/loc r))])))



;; (first-one loc) produces all characters that is enclosed by first pairs
;;   of brackets and brackets themselves or all characters before the first
;;   opening bracket or all characters in loc
;; Examples:
(check-expect (first-one (string->list "<p><h1>Heading</h1>Text</p>"))
              '(#\< #\p #\>))
(check-expect (first-one (string->list "a b  c"))
              '(#\a #\space #\b #\space #\space #\c))
(check-expect (first-one (string->list "head<h1>")) '(#\h #\e #\a #\d))

;; first-one: (listof Char) -> (listof Char)
(define (first-one loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\<) (cons (first loc) (find-closing (rest loc)))]
        [else (cons (first loc) (find-opening (rest loc)))]))



;; (find-closing loc) produeces all characters before the first closing
;;   bracket and the closing bracket itself, which is #\>, in loc
;; Examples:
(check-expect (find-closing (string->list "p><h1>Heading</h1>Text</p>"))
              '(#\p #\>))
(check-expect (find-closing (string->list "a><h2>text</h2></p>"))
              '(#\a #\>))

;; find-closing: (listof Char) -> (listof Char)
(define (find-closing loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\>) (list #\>)]
        [else (cons (first loc) (find-closing (rest loc)))]))



;; (find-opening loc) produces all characters before the first opening,
;;   which is #\<, in loc
;; Examples:
(check-expect (find-opening (string->list "ead<h1>"))
              '(#\e #\a #\d))
(check-expect (find-opening (string->list "a b c"))
              '(#\a #\space #\b #\space #\c))

;; find-opening: (listof Char) -> (listof Char)
(define (find-opening loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\<) empty]
        [else (cons (first loc) (find-opening (rest loc)))]))



;; (rest-lst loc) produces all characters behind the first closing bracket or
;;   all characters behind the first opening bracket if there are some characters
;;   before the first opening bracket or empty
;; Examples:
(check-expect (rest-lst (string->list "<p><h1>Heading</h1>Text</p>"))
              '(#\< #\h #\1 #\> #\H #\e #\a #\d #\i #\n #\g #\< #\/
                    #\h #\1 #\> #\T #\e #\x #\t #\< #\/ #\p #\>))
(check-expect (rest-lst (string->list "a b   c"))
              empty)
(check-expect (rest-lst (string->list "heading<h1>"))
              '(#\< #\h #\1 #\>))

;; rest-lst: (listof Char) -> (listof Char)
(define (rest-lst loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\<) (r1 (rest loc))]
        [else (r2 (rest loc))]))



;; (r1 loc) produces all characters behind the first closing brackets
;; Example:
(check-expect (r1 (string->list "<p><h1>Heading</h1>Text</p>"))
              (list #\< #\h #\1 #\> #\H #\e #\a #\d #\i #\n #\g #\<
                    #\/ #\h #\1 #\> #\T #\e #\x #\t #\< #\/ #\p #\>))

;; r1: (listof Char) -> (listof Char)
(define (r1 loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\>) (rest loc)]
        [else (r1 (rest loc))]))



;; (r2 loc) produces all characters behind the first opening bracket and
;;   the openning bracket itself or empty
;; Examples:
(check-expect (r2 (string->list "a b   c")) empty)
(check-expect (r2 (string->list "heading<h1>")) '(#\< #\h #\1 #\>))

;; r2: (listof Char) -> (listof Char)
(define (r2 loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\<) loc]
        [else (r2 (rest loc))]))




;;
;; b)
;;
;; (string->html str) produces an HI representing str
;; Examples:
(check-expect (string->html "<p>Waterloo<h>cute</h>fly</p>")
              '(p "Waterloo" (h "cute") "fly"))
(check-expect (string->html "<a>cute<b>blue</b></a>")
              '(a "cute" (b "blue")))

;; string->html: Str -> HI
(define (string->html str)
  (make-l (string->html/helper (rest (tokenize str))
                               (list (first (tokenize str))))))

;; Tests:
(check-expect (string->html "<p>great<a>Love</a>peace</p>")
              '(p "great" (a "Love") "peace"))
(check-expect (string->html "<c><p>Heading</p>Text</c>")
              '(c (p "Heading") "Text"))




;; (make-l lst) produces an HI representing lst
;; Examples:
(check-expect (make-l (list "great" "Waterloo" #\/ "cute" 'h "fly" 'p))
              '(p "great" "Waterloo" (h "cute") "fly"))

;; make-l: (listof Any) -> HI
(define (make-l lst)
  (append (list (first (reverse-lst lst)))
          (break-part/1 lst)
          (list (append (list(find-next-symbol (rest (reverse-lst lst)))) (break-part/2 lst)))
              (break-part/3 lst)))



;; (break-part/1 lst) produces all elements in lst before #\/
;; Example:
(check-expect (break-part/1 (list "great" "Waterloo" #\/ "cute" 'h "fly" 'p))
              '("great" "Waterloo"))

;; break-part/1: (listof Any) -> (listof Str)
(define (break-part/1 lst)
  (cond [(empty? lst) empty]
        [(char? (first lst)) empty]
        [(symbol? (first lst)) empty]
        [else (cons (first lst) (break-part/1 (rest lst)))]))



;; (break-part/2 lst) produces all elements in lst between #\/ and the
;;   followed symbol
;; Example:
(check-expect (break-part/2 (list "great" "Waterloo" #\/ "cute" 'h "fly" 'p))
              '("cute"))

;; break-part/2: (listof Any) -> (listof Str)
(define (break-part/2 lst)
  (cond [(char? (first lst)) (break-part/1 (rest lst))]
        [else (break-part/2 (rest lst))]))



;; (break-part/3 lst) produces all elements in lst between two symbols
;; Example:
(check-expect (break-part/3 (list "great" "Waterloo" #\/ "cute" 'h "fly" 'p))
              '("fly"))

;; break-part/3: (listof Any) -> (listof Str)
(define (break-part/3 lst)
  (cond [(symbol? (first lst)) (break-part/1 (rest lst))]
        [else (break-part/3 (rest lst))]))



;; (find-next-symbol lst) produces the first symbol in the lst
;; Example:
(check-expect (find-next-symbol (list "great" "Waterloo" #\/ "cute" 'h "fly" 'p))
              'h)

;; find-next-symbol: (listof Any) -> Sym
(define (find-next-symbol lst) 
  (cond [(symbol? (first lst)) (first lst)]
        [else (find-next-symbol (rest lst))]))



;; (reverse-lst lst) reverses lst, which means that putting lst in the
;;   reversed order
;; Example:
(check-expect (reverse-lst (list "great" "Waterloo" #\/ "cute" 'h "fly" 'p))
              '(p "fly" h "cute" #\/ "Waterloo" "great"))

;; reverse-lst: (listof Any) -> (listof Any)
(define (reverse-lst lst)
  (foldl cons '() lst))




;; (string->html/helper lst stack) converts "<Any>" of lst into symbol
;;   and sort lst with the help of stack
;; Example:
(check-expect (string->html/helper '("Waterloo" "<h1>" "cute" "cat" "</h1>" "fly" "</p>")
                                   '("<p>"))
              (list "Waterloo" #\/ "cute" "cat" 'h1 "fly" 'p))

;; string->html/helper: (listof Str) -> (listof Any)
(define (string->html/helper lst stack)
  (cond [(empty? lst) empty]
        [else
         (cond [(not (string=? (substring (first lst) 0 1) "<"))
                (cons (first lst) (string->html/helper (rest lst) stack))]
               [(match? (first stack) (first lst))
                (cons
                 (string->symbol (substring (first stack) 1 (- (string-length (first stack)) 1)))
                 (string->html/helper (rest lst) (rest stack)))]
                [else (cons #\/ (string->html/helper (rest lst) (cons (first lst) stack)))]
               )]))



;; (match? a b) determines whether a and b are corresponding or not
;; Example:
(check-expect (match? "<p>" "</p>") true)

;; match?: Str Str -> Bool
(define (match? a b)
  (cond [(string=? a (string-append (substring b 0 1) (substring b 2)))
         true]
        [else false]))



