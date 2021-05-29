#lang racket

(require pict)

(define num-iters 5000)

(define sierpinski
  (for/fold ([canvas (blank 300 300)]
             [curr '(0 0)]
             #:result canvas)
            ([_ num-iters])
    (define dest (case (random 3)
                   ([0] '(150 0))
                   ([1] '(0 300))
                   ([2] '(300 300))))
    (define midpoint `(,(/ (+ (car dest) (car curr)) 2.0)
                       ,(/ (+ (cadr dest) (cadr curr)) 2.0)))
    (values (pin-over canvas
                      (car midpoint) (cadr midpoint)
                      (colorize (disk 1) "black"))
            midpoint)))

sierpinski
