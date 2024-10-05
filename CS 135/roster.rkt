;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;       Yuqi Gu (20884580)
;;       CS 135 Fall 2020
;;       Assignment 06, Problem 1
;; ***************************************************
;;


;; A StudentID is a Nat with at most 8 digits (i.e. 0 <= id <= 99999999)

;; A Grade is one of:
;; * false
;; * Nat
;;   Requires: Nat is between 0 and 100 (inclusive)

(define-struct student (id name grade))
;; A Student is a (make-student StudentID Str Grade)


(define-struct rnode (student left right))
;; A Roster Node (RN) is a (make-rnode Student Roster Roster)
;; Requires: all students in the left subtree have an ID < student's ID
;;           all students in the right subtree have an ID > student's ID

;; A Roster is one of 
;; * empty
;; * RN


;; =============================================================================
;; constants used in Q1 examples
;; =============================================================================

(define beth (make-student 12345678 "Beth" 96))
(define jenny (make-student 08675309 "Jenny" 81))
(define john1 (make-student 48975311 "John" 95))
(define jenny/new (make-student 08675309 "Jen" 81))
(define john2 (make-student 20488192 "John" false))
(define beth/new (make-student 20152353 "Bob" false))
(define beth2 (make-student 10253013 "Beth" 89))
(define beth3 (make-student 98120421 "Beth" false))

(define sample-roster
  (make-rnode beth ; root
              (make-rnode jenny empty empty)   ; left child
              (make-rnode john1 empty empty))) ; right child

(define sample-roster-2
  (make-rnode beth 
              (make-rnode jenny/new empty empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))

(define sample-roster-3
  (make-rnode beth/new
              empty
              (make-rnode john2 empty empty)
              ))

(define sample-roster-4
  (make-rnode beth/new
              (make-rnode beth
                          (make-rnode beth2 empty empty)
                          empty)
              (make-rnode beth3 empty empty)
              ))

;;
;; 1a)
;;
;; (find-student id ros) produces the matching student in ros according
;;   to id
;; Examples:
(check-expect (find-student 12345678 sample-roster) beth)
(check-expect (find-student 87654321 sample-roster) false)

;; find-student: StudentID Roster -> (anyof Student Bool)
(define (find-student id ros)
  (cond [(empty? ros) false]
        [(= id (student-id (rnode-student ros)))
         (rnode-student ros)]
        [(< id (student-id (rnode-student ros)))
         (find-student id (rnode-left ros))]
        [(> id (student-id (rnode-student ros)))
         (find-student id (rnode-right ros))]))

;; Tests:
(check-expect (find-student 08675309 sample-roster) jenny)
(check-expect (find-student 48975311 sample-roster) john1)
(check-expect (find-student 20152353 sample-roster-3) beth/new)
(check-expect (find-student 12423235 sample-roster-2) false)


;;
;; 1b)
;;
;; (class-average ros) produces the class average of ros
;; Examples:
(check-expect (class-average sample-roster) (+ 90 2/3))
(check-expect (class-average empty) 'N/A)

;; class-average: Roster -> (anyof Num Char)
(define (class-average ros)
  (cond [(or (empty? ros) (= (number-of-students ros) 0)) 'N/A]
        [else (/ (sum-of-grade ros) (number-of-students ros))]))

;; Tests:
(check-expect (class-average sample-roster-2) (+ 90 2/3))
(check-expect (class-average sample-roster-3) 'N/A)


;; (sum-of-grade ros) produces the sum of grades of students in ros
;; Examples:
(check-expect (sum-of-grade sample-roster) 272)
(check-expect (sum-of-grade sample-roster-2) 272)
(check-expect (sum-of-grade empty) 0)

;; sum-of-grade: Roster -> Num
(define (sum-of-grade ros)
  (cond [(empty? ros) 0]
        [(equal? (student-grade (rnode-student ros)) false)
         (+ (sum-of-grade (rnode-left ros))
            (sum-of-grade (rnode-right ros)))]
        [else (+ (student-grade (rnode-student ros))
                 (sum-of-grade (rnode-left ros))
                 (sum-of-grade (rnode-right ros)))]))


;; (number-of-students ros) produces the number of students with grades
;;   that are not false of ros
;; Examples:
(check-expect (number-of-students sample-roster) 3)
(check-expect (number-of-students sample-roster-2) 3)
(check-expect (number-of-students sample-roster-3) 0)

;; number-of-students: Roster -> Nat
(define (number-of-students ros)
  (cond [(empty? ros) 0]
        [(equal? (student-grade (rnode-student ros)) false)
         (+ (number-of-students (rnode-left ros))
            (number-of-students (rnode-right ros)))]
        [else (+ 1 (number-of-students (rnode-left ros))
                 (number-of-students (rnode-right ros)))]))


;;
;; 1c)
;;
;; (find-student/name str ros) produces a list of students in ros with
;;   exact name that equals to str
;; Examples:
(check-expect (find-student/name "Beth" sample-roster) (list beth))
(check-expect (find-student/name "Dan" sample-roster) empty)

;; find-student/name: Str Roster -> (listof Student)
(define (find-student/name str ros)
  (find-name-helper str ros (list)))

;; Tests:
(check-expect (find-student/name "John" sample-roster-2) (list john2 john1))
(check-expect (find-student/name "Beth" sample-roster-4) (list beth2 beth beth3))
(check-expect (find-student/name "Divid" sample-roster-3) empty)


;; (find-name-helper str ros lst) produces a list of students in ros
;;   with exact name that equals to str and put them into lst
;; Examples:
(check-expect (find-name-helper "Beth" sample-roster (list)) (list beth))
(check-expect (find-name-helper "Dan" sample-roster (list)) empty)
(check-expect (find-name-helper "John" sample-roster-2 (list)) (list john2 john1))

;; find-name-helper: Str Roster (listof Student) -> (listof Student)
(define (find-name-helper str ros lst)
  (cond [(empty? ros) empty]
        [(string=? str (student-name (rnode-student ros)))
         (append lst 
                 (find-name-helper str (rnode-left ros) lst)
                 (list (rnode-student ros))
                 (find-name-helper str (rnode-right ros) lst))]
        [else (append lst
                      (find-name-helper str (rnode-left ros) lst)
                      (find-name-helper str (rnode-right ros) lst))]))


;;
;; 1d)
;;
;; (add-students lst ros) produces a new Roster equal to ros but with
;;   each new student from lst added
;; Examples:
(check-expect (add-students (list (list 20488192 "John")
                                 (list 8675309 "Jen")) sample-roster)
              sample-roster-2)

;; add-student: (listof (list StudentID Str)) Roster -> Roster
(define (add-students lst ros)
  (cond [(empty? lst) ros]
        [(contains lst ros)
         (add-students (rest lst) (change-name (first lst) ros))]
        [else (add-students (rest lst) (addin (first lst) ros))]))

;; Tests:
(check-expect (add-students (list (list 23015323 "Amy")
                                 (list 00123152 "Lisa")) sample-roster-2)
              (make-rnode beth
                 (make-rnode jenny/new
                    (make-rnode (make-student 00123152 "Lisa" false)
                                empty empty)
                    empty)
                 (make-rnode john1
                    (make-rnode john2
                                empty
                                (make-rnode
                                 (make-student 23015323 "Amy" false)
                                   empty empty))
                    empty)))
(check-expect (add-students (list (list 91235102 "Divid")
                                 (list 5201314 "Jim")) sample-roster-3)
              (make-rnode beth/new
                          (make-student 5201314 "Jim" false)
                          (make-rnode john2 empty
                                      (make-rnode
                                       (make-student 91235102 "Divid" false)
                                       empty empty))))


;; (contains lst ros) determines whether ros contains StudentID of lst
;; Exmaples:
(check-expect (contains (list (list 20488192 "John")) sample-roster) false)

;; contains: (listof (list StudentID Str)) Roster -> Bool
(define (contains lst ros)
  (cond
    [(empty? ros) false]
    [(= (first (first lst)) (student-id (rnode-student ros)))
         true]
    [(< (first (first lst)) (student-id (rnode-student ros)))
         (contains lst (rnode-left ros))]
    [(> (first (first lst)) (student-id (rnode-student ros)))
         (contains lst (rnode-right ros))]))


;; (change-name lst ros) changes the name of Student who has the same
;;   StudentID of lst to the name of lst in ros
;; Examples:
(check-expect (change-name (list 8675309 "Jen") sample-roster)
              (make-rnode beth
                          (make-rnode jenny/new empty empty)
                          (make-rnode john1 empty empty)))
(check-expect (change-name (list 20488192 "Jim") sample-roster-3)
              (make-rnode beth/new
                          empty
                          (make-rnode (make-student 20488192 "Jim" false)
                                      empty empty)))

;; change-name: (list StudentID Str) Roster -> Roster
(define (change-name lst ros)
  (cond [(= (first lst) (student-id (rnode-student ros)))
         (make-rnode (make-student (student-id (rnode-student ros))
                                   (second lst)
                                   (student-grade (rnode-student ros)))
                     (rnode-left ros)
                     (rnode-right ros))]
        [(< (first lst) (student-id (rnode-student ros)))
         (make-rnode (rnode-student ros)
                     (change-name lst (rnode-left ros))
                     (rnode-right ros))]
        [(> (first lst) (student-id (rnode-student ros)))
         (make-rnode (rnode-student ros)
                     (rnode-left ros)
                     (change-name lst (rnode-right ros)))]))


;; (addin lst ros) adds Student whose name and StudentID are the same
;;   as those in lst into ros
;; Examples:
(check-expect (addin (list 23015323 "Amy") sample-roster)
              (make-rnode beth
                          (make-rnode jenny empty empty)
                          (make-rnode john1
                                      (make-rnode
                                       (make-student 23015323 "Amy" false)
                                       empty
                                       empty)
                                      empty)))
(check-expect (addin (list 23015323 "Amy") sample-roster-2)
              (make-rnode beth
                 (make-rnode jenny/new
                    empty
                    empty)
                 (make-rnode john1
                    (make-rnode john2
                                empty
                                (make-rnode
                                 (make-student 23015323 "Amy" false)
                                   empty empty))
                    empty)))

;; addin: (list StudentID Str) Roster -> Roster
(define (addin lst ros)
  (cond [(empty? ros) (make-student (first lst) (second lst) false)]
        [(and (empty? (rnode-left ros)) (empty? (rnode-right ros)))
         (cond [(and (empty? (rnode-left ros))
                     (< (first lst) (student-id (rnode-student ros))))
                (make-rnode (rnode-student ros)
                            (make-rnode
                             (make-student (first lst) (second lst) false)
                             empty
                             empty)
                            (rnode-right ros))]
               [(and (empty? (rnode-right ros))
                     (> (first lst) (student-id (rnode-student ros))))
                (make-rnode (rnode-student ros)
                            (rnode-left ros)
                            (make-rnode
                             (make-student (first lst) (second lst) false)
                             empty
                             empty))])]
        [(< (first lst) (student-id (rnode-student ros)))
         (make-rnode (rnode-student ros)
                     (addin lst (rnode-left ros))
                     (rnode-right ros))]
        [(> (first lst) (student-id (rnode-student ros)))
         (make-rnode (rnode-student ros)
                     (rnode-left ros)
                     (addin lst (rnode-right ros)))]))