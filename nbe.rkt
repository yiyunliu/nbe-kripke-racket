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
  (match a
    [`(fun ,f) (f identity b)]
    [`(neu ,u) `(neu (app ,u ,b))]
    [_ (error "ap: type error")]))

(define-syntax-rule (ifz a b c)
  (match a
    ['zero b]
    [`(succ ,u) (ap c u)]
    [`(neu ,u) `(neu (if-zero ,u ,b ,c))]))

(define compose-ren compose)
(define (compose-ren-sub ξ ρ) (compose (curry ren-dom ξ) ρ))
(define-syntax-rule (ext ρ a)
  (lambda (i)
    (if (zero? i)
        a
        (ρ (- i 1)))))

(define (ren-ne-dom ξ a)
  (match a
    [`(var ,i) `(var ,(ξ i))]
    [`(app ,a ,b) `(app ,(ren-ne-dom ξ a) ,(ren-dom ξ b))]
    [`(if-zero ,a ,b ,c) `(if-zero ,(ren-ne-dom ξ a) ,(ren-dom ξ b) ,(ren-dom ξ c))]))

(define (ren-dom ξ a)
  (match a
    ['zero 'zero]
    [`(succ ,a) `(succ ,(ren-dom ξ a))]
    [`(neu ,a) `(neu ,(ren-ne-dom ξ a))]
    [`(fun ,f) `(fun ,(λ (ξ0 α) (f (compose-ren ξ0 ξ) α)))]))

(define-syntax-rule (interp-fun a ρ)
  (list 'fun (λ (ξ x) (interp a (ext (compose-ren-sub ξ ρ) x)))))

(define (interp a ρ)
  (match a
    [`(var ,i) (ρ i)]
    ['zero 'zero]
    [`(succ ,a) `(succ ,(interp a ρ))]
    [`(if-zero ,a ,b ,c) (ifz (interp a ρ) (interp b ρ) (interp-fun c ρ))]
    [`(λ ,a) (interp-fun a ρ)]
    [`(app ,a ,b) (ap (interp a ρ) (interp b ρ))]))

(define (reify a)
  (match a
    ['zero 'zero]
    [`(succ ,a) `(succ ,(reify a))]
    [`(fun ,f) (list 'λ (reify (f  (curry + 1) '(neu (var 0)))))]
    [`(neu ,a) (reify-neu a)]))

(define (extract-body a)
  (match a
    [`(λ ,a) a]
    [_ (error "reify-neu: not reifiable")]))

(define (reify-neu a)
  (match a
    [`(if-zero ,a ,b ,c) (list 'if (reify-neu a) (reify b) (extract-body (reify c)))]
    [`(app ,u ,v) (list 'app (reify-neu u) (reify v))]
    [`(var ,i) a]))

(define (idsub i) `(neu (var ,i)))

(define (normalize a)
  (reify (interp a idsub)))

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
