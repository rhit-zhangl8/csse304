#lang racket

(require racket/trace)
(require "chez-init.rkt")

(define-datatype person person?
 [student (name string?)
           (id number?)
           (gpa number?)]
  [professor (name string?)
                 (salary number?)])

(define print-name
  (lambda (p)
    (cases person p
      [student (name id gpa)
               (display name)])))

(define-datatype bintree bintree?
  [leaf-node
   (datnum number?)]
  [interior-node
   {key symbol?}
   (left bintree?)
   (right bintree?)])

(define-datatype math math?
  (number (value number?))
  (operation (left math?)
             (op symbol?)
             (right math?)))

(define parse-math
  (lambda (m)
    (cond [(number? (car m))
           (cons (number (car m))
                 (cdr m))]
          [else
           (let* ((result1 (parse-math (cdr m)))
                  (math1 (car result1))
                  (rest1 (cdr result1))
                  (op (car rest1))
                  (result2 (parse-math (cdr rest1)))
                  (math2 (car result2))
                  (rest2 (cdr result2)))
             (cons (operation math1 op math2) (cdr rest2)))]
          )))

(parse-math '(< < 2 + 3 > + 4 >))


(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      [leaf-node (datnum) datnum]
      [interior-node (key left right)
                     (+ (leaf-sum left)
                        (leaf-sum right))])))

(define bintree-height
  (lambda (b)
    (cases bintree b
      [leaf-node (datnum) 1]
      [interior-node (key left right) (+ 1 (max (bintree-height left) (bintree-height right)))])))