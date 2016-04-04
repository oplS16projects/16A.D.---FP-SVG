;Dmitri Kheifets
;Adam Contardo

#lang racket
(provide tool-call0)
(provide button-call0)

(define tool-call0 (λ (object e) (display "\nYou clicked a toolbar button!\n")))
(define button-call0 (λ (object e) (display "\nYou clicked a button!\n")))
