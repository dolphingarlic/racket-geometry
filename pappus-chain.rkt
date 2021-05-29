#lang racket

(require pict)

(define circle-count 50)
(define rainbow `("red" "orange" "yellow" "green" "blue" "purple"))
(define width-pcnt 2/5)

(define canvas (pin-over (pin-over (blank 400 200)
                                   0 0
                                   (disk 400 #:color "black"))
                         0 (* width-pcnt 200)
                         (disk (* (- 1 width-pcnt) 400) #:color "white")))

(define base-diameter (* width-pcnt 400))
(define ratio (/ (- 400 base-diameter) 400))

(define pappus-chain (for/fold ([canvas canvas])
                               ([i circle-count])
                       (define x (* 400 (/ (* ratio (+ 1 ratio)) (* 2 (+ ratio (* i i (- 1 ratio) (- 1 ratio)))))))
                       (define y (- 200 (* 400 (/ (* i ratio (- 1 ratio)) (+ ratio (* i i (- 1 ratio) (- 1 ratio)))))))
                       (define radius (* 400 (/ (* ratio (- 1 ratio)) (* 2 (+ ratio (* i i (- 1 ratio) (- 1 ratio)))))))
                       (pin-over canvas
                                 (- x radius) (- y radius)
                                 (disk (* 2 radius) #:color (list-ref rainbow (modulo i 6))))))

pappus-chain
