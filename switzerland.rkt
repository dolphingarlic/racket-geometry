(require pict)

(define ratio (+ 1 (sqrt 2))) ; Ratio between successive lengths

(struct rect (w h x y))

; Returns a sequence of **white** rectangles to draw over a red background
(define (switzerland x y sz depth) ; (x, y): top left corner; sz: side length of bounding square
  (define corner (/ sz ratio))
  (define middle-short (- sz (* 2 corner)))
  (define middle-long (- sz (* 2 middle-short)))
  (let ([cross `(,(rect middle-long middle-short (+ x middle-short) (+ y corner))
                 ,(rect middle-short middle-long (+ x corner) (+ y middle-short)))])
    (if (< depth 5)
        (append cross
                ; Corner squares
                (switzerland x y corner (+ depth 1))
                (switzerland (+ x corner middle-short) y corner (+ depth 1))
                (switzerland x (+ y corner middle-short) corner (+ depth 1))
                (switzerland (+ x corner middle-short) (+ y corner middle-short) corner (+ depth 1))
                ; Middle squares
                (switzerland (+ x corner) y middle-short (+ depth 2))
                (switzerland (+ x corner) (+ y middle-short middle-long) middle-short (+ depth 2))
                (switzerland x (+ y corner) middle-short (+ depth 2))
                (switzerland (+ x middle-short middle-long) (+ y corner) middle-short (+ depth 2)))
        cross)))

(for/fold ([canvas (filled-rectangle 400 400 #:color "Red" #:border-color "Red")])
           ([rect (switzerland 0 0 400 0)])
   (pin-over canvas
             (rect-x rect) (rect-y rect)
             (filled-rectangle (rect-w rect) (rect-h rect) #:color "White" #:border-color "White")))
