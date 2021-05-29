#lang racket

(require pict)

(define rainbow `("red" "orange" "yellow" "green" "blue" "purple"))

(struct point (x y))

(define (dist p1 p2)
  (let ([dx (- (point-x p1) (point-x p2))])
    (let ([dy (- (point-y p1) (point-y p2))])
      (sqrt (+ (* dx dx) (* dy dy))))))

(define (midpoint p1 p2)
  (point (/ (+ (point-x p1) (point-x p2)) 2) (/ (+ (point-y p1) (point-y p2)) 2)))

(define (invert-point p center)
  (define d (dist p center))
  (define inv-ratio (/ 1 (* d d)))
  (let ([dx (- (point-x p) (point-x center))])
    (let ([dy (- (point-y p) (point-y center))])
      (point (+ (point-x center) (* inv-ratio dx))
             (+ (point-y center) (* inv-ratio dy))))))

(struct circle (center radius))

(define canvas (blank 400 200))

(define (draw-circle circ color)
  (set! canvas (pin-over canvas ; Kinda bad practice but whatever I've spend long enough on this :(
                         (- (point-x (circle-center circ)) (circle-radius circ))
                         (- (point-y (circle-center circ)) (circle-radius circ))
                         (disk (* 2 (circle-radius circ)) #:color color))))

(define (common-tangent c1 c2)
  (define ratio (/ (circle-radius c1) (+ (circle-radius c1) (circle-radius c2))))
  (let ([dx (- (point-x (circle-center c2)) (point-x (circle-center c1)))])
    (let ([dy (- (point-y (circle-center c2)) (point-y (circle-center c1)))])
      (point (+ (point-x (circle-center c1)) (* dx ratio)) (+ (point-y (circle-center c1)) (* dy ratio))))))

(define (invert-circle circ center)
  (define x1 (point-x center))
  (define y1 (point-y center))
  (define x2 (point-x (circle-center circ)))
  (define y2 (point-y (circle-center circ)))
  (define r (circle-radius circ))
  (define m (/ (- y2 y1) (- x2 x1)))
  ; First, we solve the system of equations
  ; y = m * x + y2 - m * x2 and (x - x2)^2 + (y - y2)^2 = r^2
  (define ratio (/ (* r r) (+ 1 (* m m)))) ; (x - x2)^2 = ratio
  (define x1-inv (+ x2 (sqrt ratio)))
  (define y1-inv (+ y2 (* -1 x2 m) (* x1-inv m)))
  (define x2-inv (+ x2 (* -1 (sqrt ratio))))
  (define y2-inv (+ y2 (* -1 x2 m) (* x2-inv m)))
  ; Next, we invert the points (x1-inv, y1-inv) and (x2-inv, y2-inv)
  (define p1 (invert-point (point x1-inv y1-inv) center))
  (define p2 (invert-point (point x2-inv y2-inv) center))
  ; ... and we get the diameter of the inverted circle!
  (circle (midpoint p1 p2) (/ (dist p1 p2) 2)))

(define (make-gasket c1 c2 c3 tangent layer-of-recursion)
  ; `c1`, `c2`, and `c3` are the three circles we're concerned with
  ; `tangent` is the point of tangency between `c1` and `c2`
  ; `layer-of-recursion` is there to determine the colour of the circle
  ; =====================================================
  ; Step 1: Find `c3` inverted through `tangent`
  (define c3-inv (invert-circle c3 tangent))
  ; Step 2: Find the slope of the line perpendicular to the one joining `c1`'s and `c2`'s centers
  (define c1-c2-inv-slope
    (if (equal? (point-y (circle-center c2)) (point-y (circle-center c1)))
        0
        (let ([dx (- (point-x (circle-center c2)) (point-x (circle-center c1)))])
          (let ([dy (- (point-y (circle-center c2)) (point-y (circle-center c1)))])
            (* -1 (/ dx dy)))))) ; Ugh division by 0 is possible, but whatever :(
  ; Step 3: Find the circles tangent to this and the two inverted lines, and pick the closer one
  (define c4-c5-dx
    (if (equal? (point-y (circle-center c2)) (point-y (circle-center c1)))
        0
        (* 2 (circle-radius c3-inv) (sqrt (/ 1 (+ 1 (expt c1-c2-inv-slope 2)))))))
  (define c4-c5-dy
    (if (equal? (point-y (circle-center c2)) (point-y (circle-center c1)))
        (* -2 (circle-radius c3-inv))
        (* c4-c5-dx c1-c2-inv-slope)))
  (define c4-inv (circle (point (+ (point-x (circle-center c3-inv)) c4-c5-dx)
                                (+ (point-y (circle-center c3-inv)) c4-c5-dy))
                         (circle-radius c3-inv)))
  (define c5-inv (circle (point (- (point-x (circle-center c3-inv)) c4-c5-dx)
                                (- (point-y (circle-center c3-inv)) c4-c5-dy))
                         (circle-radius c3-inv)))
  (define c-to-invert
    (if (>= (dist (circle-center c4-inv) tangent) (dist (circle-center c5-inv) tangent))
        c4-inv c5-inv))
  ; Step 4: Invert the the circle again and draw it, along with the other recursively drawn circles
  (define res (invert-circle c-to-invert tangent))
  (define tmp1
    (if (< layer-of-recursion 8)
        (make-gasket c1 res c3 (common-tangent c1 res) (+ layer-of-recursion 1))
        (void)))
  (define tmp2
    (if (< layer-of-recursion 8)
        (make-gasket c2 res c3 (common-tangent c2 res) (+ layer-of-recursion 1))
        (void)))
  (define tmp3
    (if (< layer-of-recursion 8)
        (make-gasket c1 c2 res tangent (+ layer-of-recursion 1))
        (void)))
  (draw-circle res (list-ref rainbow (modulo layer-of-recursion 6))))

(draw-circle (circle (point 200 200) 200) "black")
(draw-circle (circle (point 120 200) 120) "red")
(draw-circle (circle (point 320 200) 80) "red")
(make-gasket (circle (point 120 200) 120)
             (circle (point 320 200) 80)
             (circle (point 200 200) 200)
             (point 240 200) 1)

canvas
