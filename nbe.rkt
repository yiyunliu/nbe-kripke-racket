#lang racket
;; Grammar (Λ)
;; t := λ t | app t t | i

;; Domain
;; D ≃ neu D_ne | fun [D → D]
;; D_ne = var i | app D_ne D
;; Define as we go

(define (ap a b)
  (match a
    [`(fun ,f) (f identity b)]
    [`(neu ,u) `(neu (app ,u ,b))]))

(define compose-ren compose)
(define (compose-ren-sub ξ ρ) (compose (curry ren-dom ξ) ρ))
(define (ext ρ a)
  (lambda (i)
    (if (zero? i)
        a
        (ρ (- i 1)))))

(define (ren-ne-dom ξ a)
  (match a
    [`(var ,i) `(var ,(ξ i))]
    [`(app ,a ,b) `(app ,(ren-ne-dom ξ a) ,(ren-dom ξ b))]))

(define (ren-dom ξ a)
  (match a
    [`(neu ,a) `(neu ,(ren-ne-dom ξ a))]
    [`(fun ,f) `(fun ,(λ (ξ0 α) (f (compose-ren ξ0 ξ) α)))]))

(define (interp a ρ)
  (match a
    [`(var ,i) (ρ i)]
    [`(λ ,a)  (list 'fun (λ (ξ x) (interp a (ext (compose-ren-sub ξ ρ) x))))]
    [`(app ,a ,b) (ap (interp a ρ) (interp b ρ))]))

(define (reify a)
  (match a
    [`(fun ,f) (list 'λ (reify (f  (curry + 1) '(neu (var 0)))))]
    [`(neu ,a) (reify-neu a)]))

(define (reify-neu a)
  (match a
    [`(app ,u ,v) (list 'app (reify-neu u) (reify v))]
    [`(var ,i) a]))

(define (idsub i) `(neu (var ,i)))

(define (normalize a)
  (reify (interp a idsub)))

(define (tm? a)
  (match a
    [`(λ ,a) (tm? a)]
    [`(app ,a ,b) (and (tm? a) (tm? b))]
    [`(var ,i) (exact-nonnegative-integer? i)]
    [_ false]))

(provide reify interp normalize tm?)
