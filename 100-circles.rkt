#lang racket

(require pict racket/draw)

(define num-circles 100)

(for/fold ([canvas (blank 300 200)])
          ([_ num-circles])
  (define sz (random 15 36))
  (define dx (random (- 300 sz)))
  (define dy (random (- 200 sz)))
  (define color (make-color (random 256) (random 256) (random 256)))
  (pin-over canvas
            dx dy
            (colorize (disk sz) color)))