#lang racket
;; Grammar (Λ)
;; t := λ t | app t t | i

;; Domain
;; D := neu D_ne | fun [(var -> var) -> D → D]
;; D_ne := var i | app D_ne D

(define (tm? a)
  (match a
    ['zero true]
    [`(succ ,a) (tm? a)]
    [`(if-zero ,a ,b ,c) (and (tm? a) (tm? b) (tm? c))]
    [`(λ ,a) (tm? a)]
    [`(app ,a ,b) (and (tm? a) (tm? b))]
    [`(var ,i) (exact-nonnegative-integer? i)]
    [_ false]))

(define-syntax-rule (ap a b)
  (match (force a)
    [`(fun ,f) (force (f b))]
    [`(neu ,u) `(neu (app ,u ,b))]
    [_ (error "ap: type error")]))

(define-syntax-rule (ifz a b c)
  (match (force a)
    ['zero (force b)]
    [`(succ ,u) (ap c u)]
    [`(neu ,u) `(neu (if-zero ,u ,b ,c))]))

(define-syntax-rule (ext ρ a)
  (lambda (i)
    (if (zero? i)
        a
        (ρ (- i 1)))))

(define-syntax-rule (interp-fun a ρ)
  (list 'fun (λ (x) (interp a (ext ρ x)))))

(define (interp a ρ)
  (delay (match a
    [`(var ,i) (force (ρ i))]
    ['zero 'zero]
    [`(succ ,a) `(succ ,(interp a ρ))]
    [`(if-zero ,a ,b ,c) (ifz (interp a ρ) (interp b ρ) (interp-fun c ρ))]
    [`(λ ,a) (interp-fun a ρ)]
    [`(app ,a ,b) (ap (interp a ρ) (interp b ρ))])))

(define (reify n a)
  (match (force a)
    ['zero 'zero]
    [`(succ ,a) `(succ ,(reify n a))]
    [`(fun ,f) (list 'λ (reify (+ n 1) (f `(neu (var ,n)))))]
    [`(neu ,a) (reify-neu n a)]))

(define (extract-body a)
  (match a
    [`(λ ,a) a]
    [_ (error "reify-neu: not reifiable")]))

(define (reify-neu n a)
  (match a
    [`(if-zero ,a ,b ,c) (list 'if (reify-neu n a) (reify n b) (extract-body (reify n c)))]
    [`(app ,u ,v) (list 'app (reify-neu n u) (reify n v))]
    [`(var ,i) (list 'var (- n (+ i 1)))]))

(define (idsub s i) `(neu (var ,(- s (+ i 1)))))

(define (scope a)
  (match a
    ['zero 0]
    [`(succ ,a) (scope a)]
    [`(if-zero ,a ,b ,c) (max (scope a) (scope b) (scope c))]
    [`(λ ,a) (max 0 (- (scope a) 1))]
    [`(app ,a ,b) (max (scope a) (scope b))]
    [`(var ,i) (+ i 1)]))

(define (normalize a)
  (let ([sa (scope a)])
    (reify sa (interp a (curry idsub sa)))))

(define (subst ρ a)
  (match a
    [`(var ,i) (ρ i)]
    [`(app ,a ,b) `(app ,(subst ρ a) ,(subst ρ b))]
    [`(λ ,a) `(λ ,(subst (ext (compose (curry subst (λ (i) `(var ,(+ i 1)))) ρ)
                              '(var 0)) a))]))

(define (idsub-tm i) `(var ,i))
(define (subst1 b a)
  (subst (ext idsub-tm b) a))

(define (eval-tm a)
  (match a
    [(list 'var _) a]
    [(list 'λ a) `(λ ,(eval-tm a))]
    [(list 'app a b)
     (match (eval-tm a)
       [(list 'λ a) (eval-tm (subst1 b a))]
       [v `(app ,v ,(eval-tm b))])]))

(define (eval-tm-strict a)
  (match a
    [(list 'var _) a]
    [(list 'λ a) `(λ ,(eval-tm-strict a))]
    [(list 'app a b)
     (match (eval-tm-strict a)
       [(list 'λ a) (eval-tm-strict (subst1 (eval-tm-strict b) a))]
       [v `(app ,v ,(eval-tm-strict b))])]))

;; Coquand's algorithm but for β-normal forms
(define (η-eq? a b)
  (match (list a b)
    ['(zero zero) true]
    [`((succ ,a) (succ ,b)) (η-eq? a b)]
    [`((if-zero ,a ,b ,c) (if-zero ,a0 ,b0 ,c0))
     (and (η-eq? a a0) (η-eq? b b0) (η-eq? c c0))]
    [`((λ ,a) (λ ,b)) (η-eq? a b)]
    [`((λ ,a) ,u) (η-eq? a `(app ,(subst (λ (i) `(var ,(+ i 1))) u) (var 0)))]
    [`(,u (λ ,a)) (η-eq? `(app ,(subst (λ (i) `(var ,(+ i 1))) u) (var 0)) a)]
    [`((app ,u0 ,v0) (app ,u1 ,v1)) (and (η-eq? u0 u1) (η-eq? v0 v1))]
    [`((var ,i) (var ,j)) (eqv? i j)]
    [_ false]))


(define (βη-eq? a b)
  (η-eq? (normalize a) (normalize b)))

(define (β-eq? a b)
  (equal? (normalize a) (normalize b)))

(provide eval-tm eval-tm-strict reify interp normalize tm? η-eq? βη-eq? β-eq?)
