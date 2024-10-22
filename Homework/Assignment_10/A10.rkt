#lang racket

(provide free-vars bound-vars lexical-address un-lexical-address convert-multip-calls convert-multip-lambdas convert-ifs)

;Worked with Jack Silkaitis on convert-multip-calls

(require racket/trace)

(define free-vars
  (lambda (a)
    (free-finder a '() '())))

;i did not know there was a built-in function called "remove-duplicates"
;please excuse the messy code

(define free-finder
  (lambda (lst out bound)
    (cond [(null? lst) out]
          [(equal? (car lst) (quote lambda))
           (free-finder (cddr lst) out (append (cadr lst) bound))]
          [(and (symbol? (car lst)) (not (member (car lst) bound)) (not (member (car lst) out)))
           (free-finder (cdr lst) (cons (car lst) out) bound)]
          [(symbol? (car lst))
           (free-finder (cdr lst) out bound)]
          [(null? (cdr lst))
           (free-finder (car lst) out bound)]
          [(list? (car lst))
           (append (free-finder (car lst) out bound) (free-finder (cdr lst) out bound))]
          [else '()]
          )))
 
(define bound-vars
  (lambda (a)
    (bound-finder a '() '())))

(define bound-finder
  (lambda (lst out bound)
    (cond [(null? lst) out]
          [(equal? (car lst) (quote lambda))
           (bound-finder (cddr lst) out (append (cadr lst) bound))]
          [(and (symbol? (car lst)) (member (car lst) bound) (not (member (car lst) out)))
           (bound-finder (cdr lst) (cons (car lst) out) bound)]
          [(symbol? (car lst))
           (bound-finder (cdr lst) out bound)]
          [(null? (cdr lst))
           (bound-finder (car lst) out bound)]
          [(list? (car lst))
           (append (bound-finder (car lst) out bound) (bound-finder (cdr lst) out bound))]
          [else '()]
          )))

(define convert-multip-calls
  (lambda (lcexp)
    (cond [(null? lcexp) '()]
          [(not (list? lcexp)) lcexp]; identifier
          [(equal? (car lcexp) 'lambda) ;lambda
           (list (car lcexp) (cadr lcexp) (convert-multip-calls (caddr lcexp)))]
          [(and (list? lcexp) (null? (cdr lcexp)))
           (car lcexp)]
          [else
           (calls-helper (list (convert-multip-calls (first lcexp))
                               (convert-multip-calls (second lcexp))) (cddr lcexp))])))

          
(define calls-helper
  (lambda (out lst)
    (cond [(null? lst) out]
          [(null? (cdr lst)) (list out (convert-multip-calls (car lst)))]
          [else (calls-helper (list out (convert-multip-calls (car lst))) (cdr lst))])))

;(trace convert-multip-calls calls-helper)
            
(define convert-multip-lambdas
  (lambda (lcexp)
    (cond [(null? lcexp) '()]
          [(not (list? lcexp)) lcexp] ;symbol case
          [(equal? (car lcexp) 'lambda) ;lambda case
           (append (lambda-helper (cadr lcexp) (convert-multip-lambdas (caddr lcexp))))]
          [else ;list case
           (cons (convert-multip-lambdas (car lcexp))
                   (convert-multip-lambdas (cdr lcexp)))]
          )))

(define lambda-helper
  (lambda (inputs body)
    (cond [(null? inputs) body]
          [else
           (list  'lambda (list (car inputs)) (lambda-helper (cdr inputs) body))])))
    
;(trace convert-multip-lambdas lambda-helper)

(define convert-ifs
  (lambda (exp)
    (cond [(null? exp) '()]
          [(not (list? exp)) exp] ;symbol case
          [(equal? (car exp) '#f)
           (cons (quote (lambda (thenval elseval) elseval)) (convert-ifs (cdr exp)))]
          [(equal? (car exp) '#t)
           (cons (quote (lambda (thenval elseval) thenval)) (convert-ifs (cdr exp)))]
          [(equal? (car exp) 'if) ;if case
           (convert-ifs (cdr exp))]
          [else ;list case
           (cons (convert-ifs (car exp))
                   (convert-ifs (cdr exp)))]
          )))

;(trace convert-ifs)

(define lexical-address
  (lambda (a)
    (lex-helper a '())))

;i have no excuse for the messy code
(define lex-helper
  (lambda (lst bound)
    (cond [(null? lst) '()]
          [(not (list? lst)) (replace-depth lst bound 0)]
          [(equal? (car lst) 'lambda)
           (list 'lambda (second lst) (lex-helper (caddr lst) (bound-helper-lambda (second lst) bound)))]
          [(equal? (car lst) 'let)
           (list 'let (map (lambda (x) (list (first x) (lex-helper (second x) bound))) (second lst))
                 (lex-helper (caddr lst) (bound-helper-let (second lst) bound)))]
          [(equal? (car lst) 'if)
           (cons 'if (lex-helper (cdr lst) bound))]
          [(not (list? (car lst)))
           (cons (replace-depth (car lst) bound 0) (lex-helper (cdr lst) bound))]
          [else
           (cons (lex-helper (car lst) bound)
                 (lex-helper (cdr lst) bound))])))
          

(define bound-helper-lambda
  (lambda (lst bound)
    (cons lst bound)))

(define bound-helper-let
  (lambda (lst bound)
    (cons (map car lst) bound)))

(define replace-depth
  (lambda (a bound depth)
    (cond [(null? bound) (list ': 'free a)]
          [else (let ((result (replace-pos a (car bound) depth 0)))
                  (if (not (null? result)) result
                      (replace-depth a (cdr bound) (add1 depth))))])))

(define replace-pos
  (lambda (a bound depth pos)
    (cond [(null? bound) '()]
          [(equal? a (car bound))
           (list ': depth pos)]
          [else
           (replace-pos a (cdr bound) depth (add1 pos))])))


(define un-lexical-address
  (lambda (a)
    (un-lex-helper a '())))

(define un-lex-helper
  (lambda (lst bound)
    (cond [(null? lst) '()]
          [(equal? ': (car lst)) (unreplace-depth lst bound)]
          [(equal? (car lst) 'lambda)
           (list 'lambda (second lst) (un-lex-helper (caddr lst) (bound-helper-lambda (second lst) bound)))]
          [(equal? (car lst) 'let)
           (list 'let (map (lambda (x) (list (first x) (un-lex-helper (second x) bound))) (second lst))
                 (un-lex-helper (caddr lst) (bound-helper-let (second lst) bound)))]
          [(equal? (car lst) 'if)
           (cons 'if (un-lex-helper (cdr lst) bound))]
          [(not (list? (car lst)))
           (cons (unreplace-depth (car lst) bound) (un-lex-helper (cdr lst) bound))]
          [else
           (cons (un-lex-helper (car lst) bound)
                 (un-lex-helper (cdr lst) bound))])))



(define unreplace-depth
  (lambda (a bound)
    (cond [(equal? 'free (second a)) (third a)]
          [(not (equal? 0 (second a)))
           (unreplace-depth (list ': (- (second a) 1) (third a)) (cdr bound))]
          [else
           (unreplace-pos a (car bound))])))

(define unreplace-pos
  (lambda (a bound)
    (cond [(not (equal? 0 (third a)))
           (unreplace-pos (list ': 0 (- (third a) 1)) (cdr bound))]
          [else
           (car bound)])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
