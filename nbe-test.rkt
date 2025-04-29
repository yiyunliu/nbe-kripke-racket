#lang racket

(require rackunit "nbe.rkt")
(define tm-id '(λ (var 0)))
(define tm-fst '(λ (λ (var 1))))
(define tm-snd '(λ (λ (var 0))))
(define tm-pair '(λ (λ (λ (app (app (var 0) (var 2)) (var 1))))))

(check-equal? (normalize `(app ,tm-id ,tm-id)) tm-id)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-snd)) tm-fst)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-fst)) tm-id)
