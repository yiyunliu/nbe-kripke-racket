#lang racket
(require racket/treelist)

;; Grammar (Λ)
;; t := λ t | app t t | i

;; Domain
;; D ≃ Λ + [D → D]
;; D_ne = var i | app D_ne D
;; Define as we go
(define (ap a b)
  (match a
    [`(fun ,f) (f b)]))

(define (ren-ne-dom ξ a)
  (match a
    [`(var ,i) `(var ,(treelist-ref ξ i))]
    [`(app ,a ,b) `(app ,(ren-ne-dom ξ a) ,(ren-dom ξ b))]))

(define (ren-dom ξ a)
  (match a
    [`(neu ,a) `(neu ,(ren-ne-dom a))]
    [`(fun ,f) `(fun ,(λ (ξ0 α) (f (compose ξ0 ξ) α)))]))

(define (interp a ρ)
  (match a
    [`(var ,i) (treelist-ref ρ i)]
    [`(λ ,a)  (list 'fun (λ (ξ x) (interp a (treelist-cons (compose ren-dom ρ) x))))]
    [`(app ,a ,b) (ap (interp a ρ) (interp b ρ))]))



(define (reflect a)
  (list 'neu a))


(define (reify a)
  (match a
    [`(fun ,f) (reify (f (reflect '(var 0))))]))
