;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 06, Problem 2
;; ***************************************************
;;

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))


;; =============================================================================
;; constants For Q2 examples
;; =============================================================================

(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))
(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))


;;
;; 2a)
;;
;; (html->string hi) produces the equivalent HTML text of hi
;; Examples:
(check-expect (html->string "text") "text")
(check-expect (html->string short-example)
              (string-append
               "<p><h1>Heading</h1>"
               "Text</p>"))

;; html->string: HI -> Str
(define (html->string hi)
  (cond [(empty? hi) ""]
        [(string? hi) hi]
        [(list? hi) (h->str (first hi) (rest hi))]))

;; Tests:
(check-expect (html->string html-example)
              (string-append
               "<html><head><title>CS135</title></head>"
               "<body><h1>Welcome</h1>More text...</body></html>"))


;; (h->str f-hi r-hi) produces the equivalent HTML text of f-hi and r-hi
;; Examples:
(check-expect (h->str '(head (title "CS135"))
                      '(body (h1 "Welcome")"More text..."))
              (string-append
               "<head><title>CS135</title></head>"
               "<body><h1>Welcome</h1>More text...</body>"))

;; h->str: (anyof Str Sym (listof HI)) -> Str
(define (h->str f-hi r-hi)
  (cond [(string? f-hi) f-hi]
        [(symbol? f-hi)
         (string-append (change-form f-hi)
                        (html->string r-hi)
                        (change-form/ f-hi))]
        [(list? f-hi)
         (string-append (html->string f-hi)
                        (html->string r-hi))]))


;; (change-form sym) produces the sym in the form of "<sym>"
;; Examples:
(check-expect (change-form 'html) "<html>")
(check-expect (change-form 'head) "<head>")

;; change-form: Sym -> Str
(define (change-form sym)
  (string-append "<"(symbol->string sym)">"))


;; (change-form/ sym) produces the sym in the form of "</sym>"
;; Examples:
(check-expect (change-form/ 'html) "</html>")
(check-expect (change-form/ 'head) "</head>")

;; change-form/: Sym -> Str
(define (change-form/ sym)
  (string-append "<" "/" (symbol->string sym) ">"))


;;
;; 2b)
;;
;; (remove-tag sym hi) removes all occurrences of sym in hi
;; Examples:
(check-expect (remove-tag 'b html-example) html-example)
(check-expect (remove-tag 'p '(p "Hello," (b "World") "!"))
              '("Hello," (b "World") "!"))

;; remove-tag: Sym HI -> (listof HI)
(define (remove-tag sym hi)
  (cond [(string? hi) hi]
        [(empty? hi) hi]
        [(symbol? hi) (cond [(symbol=? sym hi)
                             empty]
                            [else hi])]
        [(list? hi) (r-t sym (first hi) (rest hi))]))

;; Tests:
(check-expect (remove-tag 'b "html") "html")
(check-expect (remove-tag 'b '(p "Hello," (b "World") "!"))
              '(p "Hello," "World" "!"))
(check-expect (remove-tag 'b '(p "Hello," (b "World" "World") "!"))
              '(p "Hello," "World" "World" "!"))
(check-expect (remove-tag 'p '(p (b (p "Text")))) '((b "Text")))


;; (r-t sym f-hi r-hi) removes all occurrences of sym in f-hi and r-hi
;; Examples:
(check-expect (r-t 'b 'p (list "Hello," '(b "World") "!"))
              '(p "Hello," "World" "!"))

;; r-t: Sym HI HI -> HI
(define (r-t sym f-hi r-hi)
  (cond [(string? f-hi) (cons f-hi (remove-tag sym r-hi))]
        [(symbol? f-hi)
         (cond [(and (symbol=? sym f-hi) (is-Tag? r-hi)) (remove-tag sym r-hi)]
               [(and (symbol=? sym f-hi) (list? r-hi)) (remove-tag sym r-hi)]
               [(and (symbol=? sym f-hi) (string? r-hi)) r-hi]
               [else (cons f-hi (remove-tag sym r-hi))])]
        [(list? f-hi)
         (cond
           [(symbol=? sym (first f-hi))
            (append (remove-tag sym f-hi) (remove-tag sym r-hi))]
           [else(cons (remove-tag sym f-hi) (remove-tag sym r-hi))]
               )]))


;; (is-Tag? lst) determines whehter lst is Tag or not
;; Examples:
(check-expect (is-Tag? html-example) true)
(check-expect (is-Tag? (list "html")) false)

;; is-Tag?: (listof HI) -> Bool
(define (is-Tag? lst)
  (cond [(symbol? (first lst)) true]
        [else false]))

;;
;; 2c)
;;
;; (okay-tags? hi) produces true if hi has followed the rules given
;;   according to the question, and produces false otherwise
;; Examples:
(check-expect (okay-tags? html-example) true)
(check-expect (okay-tags? '(body (hr "hello"))) false)


;; okay-tags: HI -> Bool
(define (okay-tags? hi)
  (cond [(string? hi) true]
        [else (o-t (first hi) (rest hi))]))

;; Tests:
(check-expect (okay-tags? '(body (hr))) true)


;; (o-t f-hi r-hi) produces true if fi (which is made of f-hi and r-hi)
;;   has followed the rules given according to the question, and
;;   produces false otherwise
;; Examples:
(check-expect (o-t 'body '(hr "hello")) false)

;; o-t: (anyof Sym HI) -> Bool
(define (o-t f-hi r-hi)
  (cond [(symbol? f-hi)
         (cond [(symbol=? 'hr f-hi)
                (cond [(empty? r-hi) true]
                      [else false])]
               [else (o-t (first r-hi) (rest r-hi))])]
        [(and (list? f-hi) (empty? r-hi))
         (o-t (first f-hi) (rest f-hi))]
        [(list? f-hi)
         (or (o-t (first f-hi) (rest f-hi))
             (o-t (first r-hi) (rest r-hi)))]
        [else true]))



