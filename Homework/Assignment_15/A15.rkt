#lang racket

(require "../chez-init.rkt")
(provide slist-subst-cps remove-cps free-vars-cps continuation? init-k list-k)

(define-datatype continuation continuation? 
  [init-k] 
  [list-k] ; <- leave this in we use it for testing
  [step1 (slist list?) (old symbol?) (new symbol?) (k continuation?)]
  [step3 (recur-car list?) (k continuation?)]
  [step2 (slist list?) (old symbol?) (new symbol?) (k continuation?)]
  [remove1 (sym symbol?) (los list?) (k continuation?)]
  [free1 (exp list?) (k continuation?)]
  [free2 (exp list?) (k continuation?)]
  [free3 (recur-car list?) (k continuation?)]
  )


(define apply-k
  (lambda (k v)
    (cases continuation k
      [init-k () v]
      [list-k () (list v)] ; <- leave this in we use it for testing
      ; more cases here
      [step1 (slist old new k1)
                  (apply-k k1 (cons (if (eqv? (car slist) old) new (car slist)) v))]
      [step3 (recur-car k2) 
             (apply-k k2 (cons recur-car v))]
      [step2 (slist old new k3)
             (slist-subst-cps (cdr slist) old new (step3 v k3))]
      [remove1 (sym los kr1)
               (apply-k kr1 (cons (car los) v))]
      [free1 (exp kf1)
             (remove-cps (car (second exp)) v kf1)]
      [free2 (exp kf2)
             (free-vars-cps (second exp) (free3 v kf2))]
      [free3 (recur-var kf3)
              (apply-k kf3 (union recur-var v))]
               )))

(define slist-subst-cps
  (lambda (slist old new k)
    (cond [(null? slist) (apply-k k '())]
          [(symbol? (car slist))
           (slist-subst-cps (cdr slist) old new (step1 slist old new k))]
          [else (slist-subst-cps (car slist) old new (step2 slist old new k))])))
; I'm not gonna make you convert union to cps, just so you
; have a little less to do.  Lets call it a built in 
; function :)

; this one you don't have to convert
(define union ; s1 and s2 are sets of symbols.
    (lambda (s1 s2)
        (let loop ([s1 s1])
            (cond [(null? s1) s2]
                [(memq (car s1) s2) (loop (cdr s1))]
                [else (cons (car s1) (loop (cdr s1)))]))))

; convert this helper to cps
(define remove-cps ; removes the first occurrence of sym from los.
    (lambda (sym los k)
        (cond [(null? los) (apply-k k '())]
            [(eq? sym (car los)) (apply-k k (cdr los))]
            [else (remove-cps sym (cdr los) (remove1 sym los k))])))

; convert this helper to cps
(define free-vars-cps ; convert to CPS. 
    (lambda (exp k)   
        (cond [(symbol? exp) (apply-k k (list exp))]
            [(eq? (first exp) 'lambda)
             (free-vars-cps (third exp) (free1 exp k))]
             ;(remove-cps (car (2nd exp))
             ;            (free-vars-cps (3rd exp))))]      
            [else
             (free-vars-cps (first exp) (free2 exp k))]
             ;(union (free-vars-cps (1st exp)	       
             ;      (free-vars-cps (2nd exp)))])))

            )))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
