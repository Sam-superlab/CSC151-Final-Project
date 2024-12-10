(import canvas)
(import reactive)
(import lab)
(import image)
(import html)


;---------------------------------------------Background
;; generate-background.scm
;; By Sam Hopkins
(define background-width 1390)
(define background-height 730)
(define aspect-ratio 
  (/ background-width
     background-height))

;;; (generate-background s) -> drawing?
;;;    s : nonnegative-integer?
;;;    w : nonnegative-integer?
;;;    h : nonnegative-integer?
;;; Generates a background, configurable by size `s`.
;;; For our purposes, we will typically pass `background-height` 
;;; (see above) for `s`.
;;; Note: this procedure is long, but consists only of basic
;;; drawing procedures like `beside`, `above`, `overlay`, and
;;; a little bit of `path`. Therefore, a let* binding seemed
;;; more appropriate than a series of definitions, to keep this
;;; part of the program self-contained.
(define generate-background
  (lambda (s)
    (let* ([colors ; all in one place for easy changing
            (vector (rgb 245 245 220 255) ; beige, 0
                    (rgb 0 0 0 255) ; black, 1
                    (rgb 135 206 235 255) ; skyblue, 2
                    (rgb 167 238 255 255) ; lighter skyblue, 3
                    (rgb 215 255 255 255) ; even lighter skyblue, 4
                    (rgb 0 100 0 255) ; darkgreen, 5
                    (rgb 0 68 0 255) ; darker darkgreen, 6
                    (rgb 128 128 128 255) ; gray, 7
                    (rgb 85 0 0 255) ; brown, 8
                    (rgb 0 0 128 255) ; navy, 9
                    (rgb 0 0 96 255) ; darker navy, 10
                    (rgb 0 0 0 0) ; empty color, 11
                    (rgb 159 44 0 255))] ; darker orange, 12
           [c
            (lambda (n)
              (vector-ref colors n))]
           [wall
            (solid-rectangle (* s aspect-ratio) s (c 0))]
           [vertical-beam
            (solid-rectangle (* 0.015 s) (* s 0.45) (c 1))]
           [horizontal-beam
            (solid-rectangle (* s 0.45) (* 0.015 s) (c 1))]
           [gs ; glass size
            (* s 0.44)]
           [gs2 (* gs 0.6)]
           [gs3 (* gs 0.45)]
           [glass
            (overlay (path gs gs 
                           (list (pair gs2 0) (pair (* gs 0.65) 0)
                                 (pair 0 (* gs 0.65)) (pair 0 gs2)
                                 (pair gs2 0))
                           "solid" (c 4))
                     (path gs gs
                           (list (pair gs3 0) (pair gs2 0)
                                 (pair 0 gs2) (pair 0 gs3)
                                 (pair gs3 0))
                           "solid" (c 3))
                     (solid-square gs (c 2)))]
           [window
            (overlay horizontal-beam vertical-beam
                     glass
                     (solid-square (* s 0.47) (c 1)))]
           [make-curtain-column
            (lambda (w h color)
              (overlay/align "middle" "top"
                (solid-rectangle w (- h (* w 0.5)) color)
                (overlay/align "middle" "bottom"
                  (solid-circle (* w 0.5) color)
                  (solid-rectangle w h (c 11)))))]
           [curtain-column
            (overlay (make-curtain-column (* s 0.038) (* s 0.64) (c 5))
                     (make-curtain-column (* s 0.045) (* s 0.65) (c 6)))]
           [curtains
            (apply beside (make-list 6 curtain-column))]
           [curtain-rod
            (solid-rectangle (* s 1.1) (/ s 55) (c 7))]
           [spacer 
            (solid-square (* s 0.1) (c 11))]
           [window-with-curtains
            (above curtain-rod
                   (beside/align "top" curtains window curtains)
                   spacer)]
           [wall-with-window
            (overlay window-with-curtains wall)]
           [make-pot-base
            (lambda (n color)
              (path n n
                    (list (pair (* 0.1 n) 0) (pair (* 0.9 n) 0)
                          (pair (* 0.65 n) n) (pair (* 0.35 n) n)
                          (pair (* 0.1 n) 0))
                    "solid" color))]
           [make-pot-rim
            (lambda (n color)
              (solid-ellipse (* 0.8 n) (* 0.2 n) color))]
           [ps ;pot size
            (* s 0.3)]
           [empty-pot
            (overlay/align "middle" "top"
              (overlay (make-pot-rim (* s 0.27) (c 9))
                       (make-pot-rim ps (c 10)))
              (overlay/align "middle" "bottom"
                (overlay (make-pot-base (* s 0.29) (c 9))
                         (make-pot-base ps (c 10)))
                (solid-rectangle ps (* 1.1 ps) (c 11))))]
           [spacer2
            (solid-square (* s 0.0085) (c 11))]
           [dirt
            (make-pot-rim (* s 0.25) (c 8))]
           [pot
            (overlay/align "middle" "top" 
                           (above spacer2 dirt) 
                           empty-pot)]
           [pots 
            (above (beside pot pot pot)
                   (solid-rectangle (* s aspect-ratio) (* s 0.01) (c 12)))]
           [background
            (overlay/align "middle" "bottom" pots wall-with-window)])
          background)))

;; This call fills up my (Sam Hopkins) laptop's screen entirely 
;; when viewed in a new tab with the third button. 
;; Uncomment to view:
; (generate-background background-height)

;; We can also call it at a smaller size, for easier viewing
;; in the exploration pane on the right:
;; Uncomment to view:
; (generate-background 200)




;--------------------------------------------------------------------







;;;"helper functions for drawing"
(define origin (pair 0 0))

;;; (coord-normal->bad p w h) -> pair?
;;;    p : pair?, elements are numbers
;;;    w : nonnegative-integer?
;;;    h : nonnegative-integer?
;;; Takes a pair of coordinates with respect to an origin and rewrites them with 
;;; respect to the top left corner of a bounding box with width `w` and height `h`
(define coord-normal->bad
  (lambda (p w h)
    (match p
      [(pair x y) 
       (pair (+ (/ w 2) x) 
             (- (/ h 2) y))])))

;;; (coord-bad->normal p w h) -> pair?
;;;    p : pair?, elements are numbers
;;;    w : nonnegative-integer?
;;;    h : nonnegative-integer?
;;; Takes a pair of bad coordinates and makes them normal again (opposite of
;;; coord-normal->bad, see above)
(define coord-bad->normal
  (lambda (p w h)
    (match p
      [(pair x y)
       (pair (- x (/ w 2)) 
             (- (/ h 2) y))])))

;;; (shift-pt pt dx dy) -> pair?
;;;    pt : pair?, elements are numbers
;;;    dx : number?
;;;    dy : dumber?
;;; Returns the result of shifting the coordinate pair `pt` horizontally `dx` units
;;; and vertically `dy` units
(define shift-pt
  (lambda (pt dx dy)
    (pair (+ (car pt) dx)
          (+ (cdr pt) dy))))

;;; (add-pt p1 p2) -> pair?
;;;    p1 : pair?, elements are numbers
;;;    p2 : pair?, elements are numbers
;;; Returns the point which is the sum of the input coordinate 
;;; points, i.e. `p1` plus `p2`
(define add-pt
  (lambda (p1 p2)
    (shift-pt p1 (car p2) (cdr p2))))

;;; (subtract-pt p1 p2) -> pair?
;;;    p1 : pair?, elements are numbers
;;;    p2 : pair?, elements are numbers
;;; Returns the point which is the difference of the input coordinate 
;;; points, i.e. `p1` minus `p2`
(define subtract-pt
  (lambda (p1 p2)
    (shift-pt p1 (* -1 (car p2)) (* -1 (cdr p2)))))

;;; (magnitude p) -> nonnegative-number?
;;;    p : pair?, elements are numbers
;;; Returns the magnitude of `p`, treated as a 2D vector
(define magnitude
  (section match _
    [(pair x y)
     (sqrt (+ (* x x) (* y y)))]))

;;; (distance p1 p2) -> nonnegative-number?
;;;    p1 : pair?, elements are numbers
;;;    p2 : pair?, elements are numbers
;;; Returns the distance between the coordinate points `p1` and `p2`
(define distance 
  (lambda (p1 p2)
    (magnitude (subtract-pt p1 p2))))

;;; (dot p1 p2) -> number?
;;;    p1 : pair?, elements are numbers
;;;    p2 : pair?, elements are numbers
;;; Returns the dot product of the coordinate points `p1` and `p2`
(define dot
  (lambda (p1 p2)
    (+ (* (car p1) (car p2))
       (* (cdr p1) (cdr p2)))))

;;; (angle1 p) -> number?
;;;    p : pair?, elements are numbers
;;; Returns the angle between the coordinate point `p` and the
;;; positive x-axis
(define angle1
  (lambda (p)
    (atan (/ (cdr p) (car p)))))

;;; (angle2 p1 p2) -> number?
;;;    p1 : pair?, elements are numbers
;;;    p2 : pair?, elements are numbers
;;; Returns the angle between the coordinate points `p1` and `p2`
(define angle2
  (lambda (p1 p2)
    (acos (/ (dot p1 p2)
             (* (magnitude p1)
                (magnitude p2))))))

;;; (polar->cartesian r t) -> pair?
;;;    r : number?
;;;    t : number?
;;; Returns a pair of Cartesian coordinates from the given radius `r` and angle `t`
(define polar->cartesian
  (lambda (r t)
    (pair (* r (cos t))
          (* r (sin t)))))

;;; (genlt p a b c d) -> pair?, elements are numbers
;;;    p : pair?, elements are numbers
;;;    a : number?
;;;    b : number?
;;;    c : number?
;;;    d : number?
;;; Returns a variant of the coordinate pair `p` with some generalized linear
;;; transformation from R2 to R2 applied to it. `a`, `b`, `c`, and `d` represent
;;; the entries in the 2x2 matrix that encodes the transformation.
(define genlt
  (lambda (p a b c d)
    (let* ([x (car p)]
           [y (cdr p)])
          (pair (+ (* a x) (* b y))
                (+ (* c x) (* d y))))))

;;; (scale-rotate p det theta) -> pair?
;;;    p : pair?, elements are numbers
;;;    det : number?
;;;    theta : number?
;;; Returns a variant of the coordinate point `p` with a linear transformation
;;; applied to it. The transformation scales `p` by a factor of `det` and rotates
;;; `p` by `theta` radians counterclockwise about the origin. 
(define scale-rotate
  (lambda (p det theta)
    (let* ([c (* (cos theta) det)]
           [s (* (sin theta) det)]
           [-s (* -1 s)])
          (genlt p c -s s c))))

(part "Core drawing procedures")

;;; (spiral pts det theta w h color1 color2) -> drawing?
;;;    pts : (list-of pair?)
;;;    det : number?, between 0 and 1
;;;    theta : number?
;;;    epsilon : positive-number?
;;;    w : nonnegative-integer?
;;;    h : nonnegative-integer?
;;;    color1 : color?
;;;    color2 : color?
;;; Recursively creates a spiral shape within a rectangle of dimensions
;;; `w` by `h`, by repeatedly applying scale-rotate with parameters `det`
;;; and `theta` to a polygon of color `color1` and outline color `color2`
;;; defined by the points contained in `pts`. Recurses until the fist radius
;;; in `pts` is less than the tolerance `epsilon`.
(define spiral
  (lambda (pts det theta epsilon w h color1 color2)
    (spiral/helper pts det theta epsilon 
                   w h color1 color2 
                   (solid-rectangle w h (rgb 0 0 0 0)))))

;;; (spiral/helper pts det theta w h color1 color2 so-far) -> drawing?
;;;    pts : (list-of pair?)
;;;    det : number?, between 0 and 1
;;;    theta : number?
;;;    epsilon : positive-number?
;;;    w : nonnegative-integer?
;;;    h : nonnegative-integer?
;;;    color1 : color?
;;;    color2 : color?
;;;    so-far : drawing?
;;; Completes tail recursion for `spiral` (see above).
(define spiral/helper
  (lambda (pts det theta epsilon w h color1 color2 so-far)
    (if (< (magnitude (car pts)) epsilon)
        so-far
        (spiral/helper (map (section scale-rotate _ det theta) pts) 
                       det theta epsilon w h color1 color2 
                       (let ([badpts (map (section coord-normal->bad _ w h) pts)])
                            (overlay (path w h badpts "outline" color2)
                                     (path w h badpts "solid" color1)
                                     so-far))))))

;;; (polar-plot f tmin tmax step) -> (list-of pair?)
;;;    f : procedure?, takes a number and returns a number
;;;    tmin : number?
;;;    tmax : number?, greater than tmin
;;;    step : positive-number? small for best results
;;; Returns a list of points in Cartesian coordinates with respect to the origin, 
;;; originally computed from the radius function `f` on the range from `tmin` to 
;;; `tmax`. `step` controls the point density.
(define polar-plot
  (lambda (f tmin tmax step)
    (|> (range tmin tmax step)
        (section map (lambda (t) (list (f t) t)) _)
        (section map (section apply polar->cartesian _) _))))

;;; (polar-path-solid f tmin tmax step w h color) -> drawing?
;;;    f : procedure?, takes a number and returns a number
;;;    tmin : number?
;;;    tmax : number?, greater than tmin
;;;    step : positive-number? small for best results
;;;    w : nonnegative-integer?
;;;    h : nonnegative-integer?
;;;    color : color?
;;; Returns a drawing of width `w` and height `h` containing a polygon of color
;;; `color`, defined by points from the polar equation whose radius is given by `f`
;;; as a function of theta between `tmin` and `tmax`. `step` controls the point
;;; density, reducing it makes potentially higher fidelity images at the cost 
;;; of performance
(define polar-path-solid
  (lambda (f tmin tmax step w h color)
    (|> (polar-plot f tmin tmax step)
        (section map (section coord-normal->bad _ w h) _)
        (section path w h _ "solid" color))))

(part "Plants where a basic version exists already, but needs refinement:")

(problem "Sunflower")

;;; (sunflower-outer-petals size color1 color2) -> drawing?
;;;    size : nonnegative-number?
;;;    color1 : color?
;;;    color2 : color?
;;; Returns a drawing with configurable size and color that looks
;;; like the outer petals of a sunflower. `color1` controls the main
;;; color and `color2` controls the outline color. 
(define sunflower-outer-petals
  (lambda (size color1 color2)
    (|> (polar-plot (lambda (t) (+ size (* 0.55 size (cos (* 9 t))))) 0 6.3 0.03)
        (section spiral _ 0.9 10.2 size (* size 3.25) (* size 3.25) color1 color2))))

;;; (sunflower-inner-petals size color1 color2) -> drawing?
;;;    size : nonnegative-number?
;;;    color1 : color?
;;;    color2 : color?
;;; Returns a drawing with configurable size and color that looks
;;; like the inner petals of a sunflower. `color1` controls the main
;;; color and `color2` controls the outline color. 
(define sunflower-inner-petals
  (lambda (size color1 color2)
    (|> (let* ([x1 (* size 0.4)]
               [x2 (* size 0.2)])
              (list (pair x1 x1) (pair x1 x2) (pair x2 x1) (pair x1 x1)))
        (section spiral _ 0.995 0.2 1 (* size 3.25) (* size 3.25) color1 color2))))

;;; (sunflower-seeds size color1 color2) -> drawing?
;;;    size : nonnegative-number?
;;;    color1 : color?
;;;    color2 : color?
;;; Returns a drawing with configurable size and color that looks
;;; like the seeds of a sunflower. `color1` controls the main
;;; color and `color2` controls the outline color. 
(define sunflower-seeds
  (lambda (size color1 color2)
    (|> (polar-plot (lambda (t) (* size 0.06)) 0 6.3 (* 0.1 pi))
        (section map (section shift-pt _ (* size 0.32) (* size 0.32)) _)
        (section spiral _ 0.993 3.8832215 1 (* size 3.25) (* size 3.25) color1 color2))))

;;; (sunflower-head size oc1 oc2 ic1 ic2 sc1 sc2) -> drawing?
;;;    size : nonnegative-number?
;;;    oc1 : color?
;;;    oc2 : color?
;;;    ic1 : color?
;;;    ic2 : color?
;;;    sc1 : color?
;;;    sc2 : color?
;;; Returns a drawing with configurable size and color that looks
;;; like the head of a sunflower, by combining calls to the above 3
;;; procedures. `oc1` and `oc2` control the color of the outer petals,
;;; `ic1` and `ic2` control the color of the inner petals, and
;;; `sc1` and `sc2` control the color of the seeds. 
(define sunflower-head
  (lambda (size oc1 oc2 ic1 ic2 sc1 sc2)
    (overlay (sunflower-seeds size sc1 sc2)
             (sunflower-inner-petals size ic1 ic2)
             (sunflower-outer-petals size oc1 oc2))))

;;; (sunflower size oc1 oc2 ic1 ic2 sc1 sc2) -> drawing?
;;;    size : nonnegative-number?
;;;    oc1 : color?
;;;    oc2 : color?
;;;    ic1 : color?
;;;    ic2 : color?
;;;    sc1 : color?
;;;    sc2 : color?
;;; Returns a drawing with configurable size and color that looks
;;; like a sunflower. `oc1` and `oc2` control the color of the outer petals,
;;; `ic1` and `ic2` control the color of the inner petals and the stem, and
;;; `sc1` and `sc2` control the color of the seeds. 
(define sunflower
  (lambda (size oc1 oc2 ic1 ic2 sc1 sc2)
    (overlay/align "middle" "top"
      (sunflower-head size oc1 oc2 ic1 ic2 sc1 sc2)
      (above 
        (solid-square size (rgb 0 0 0 0))
        (overlay (solid-rectangle (* 0.2 size) (* 3.9 size) ic1)
                 (solid-rectangle (* 0.3 size) (* 4 size) ic2))))))

;; example of sunflower's configurable size and colors
; (sunflower 50 "purple" "yellow" "pink" "green" "cyan" "black")

;; default sunflower
; (sunflower 100 "orange" "yellow" "darkgreen" "green" "brown" "black")

(problem "Stem/branch")

;;; These are the points that will make up our first branch.
;;; In the future, this should be generalized to remove the "magic numbers".
;;; We have the branch roots (BR) and branch ends (BE).
(define BR-1 (pair 50 25))
(define BR-2 (pair 80 40))
(define BR-3 (pair 120 60))
(define BE-1 (pair 70 100))
(define BE-2 (pair 130 20))
(define BE-3 (pair 140 95))
(define BE-final (pair 170 85))

(define example-pts 
  (list origin 
        BR-1 BE-1 BR-1
        BR-2 BE-2 BR-2
        BR-3 BE-3 BR-3 
        BE-final origin))

;;; The branch lengths (BL)
(define BL-total (magnitude BE-final))
(define BL-1 (distance BR-1 BE-1))
(define BL-2 (distance BR-2 BE-2))
(define BL-3 (distance BR-3 BE-3))

;;; Branch lengths as fractions of total length. 
;;; These will be the det values for scale-rotate
(define BL-1f (/ BL-1 BL-total))
(define BL-2f (/ BL-2 BL-total))
(define BL-3f (/ BL-3 BL-total))

;;; Branch angles (BA) 
;;; These will be the theta values for scale-rotate
(define BA-1 (angle2 BE-final (subtract-pt BE-1 BR-1)))
(define BA-2 (angle2 BE-final (subtract-pt BE-2 BR-2)))
(define BA-3 (angle2 BE-final (subtract-pt BE-3 BR-3)))

;;; (branch-pts so-far n) -> (list-of pair?)
;;;    so-far : (list-of pair?)
;;;    n : nonnegative-integer?
;;; Creates a branch with `n` levels of recursion.
(define branch-pts
  (lambda (so-far n)
    (match n
      [0 so-far]
      [_ (branch-pts (append (list (pair 0 0))
                             (map (o (section shift-pt _ (car BR-1) (cdr BR-1))
                                     (section scale-rotate _ BL-1f BA-1))
                                  so-far)
                             (map (o (section shift-pt _ (car BR-2) (cdr BR-2))
                                     (section scale-rotate _ BL-2f (* -1 BA-2)))
                                  so-far)
                             (map (o (section shift-pt _ (car BR-3) (cdr BR-3))
                                     (section scale-rotate _ BL-3f BA-3))
                                  so-far) ; visually, this looks like there is
                             (list BE-final origin)) ; redundancy to remove
                     (- n 1))])))

(define stem
  (|> example-pts
      (section branch-pts _ 6)
      (section map (section scale-rotate _ 2 0.9) _)
      ; (section stitch _ 1)
      (section map (section coord-normal->bad _ 1000 1000) _)
      (section path 1000 1000 _ "outline" "lightgreen")))


(problem "Sakura")

(define echo-pts-shallow
  (lambda (p1 p2 d)
    (let* ([v (subtract-pt p2 p1)]
           [m (/ d (magnitude v))]
           [m1 (* m (car v))]
           [m2 (* m (cdr v))]
           [ccw (pair (* -1 m2) m1)])
          (list (list (add-pt p1 ccw))
                (list (subtract-pt p1 ccw))))))

(define echo-pts-deep
  (lambda (p1 p2 d)
    (let* ([v (subtract-pt p2 p1)]
           [m (/ d (magnitude v))]
           [m1 (* m (car v))]
           [m2 (* m (cdr v))]
           [ccw (pair (* -1 m2) m1)])
          (pair (add-pt p2 ccw)
                (subtract-pt p2 ccw)))))

(define stitcher
  (lambda (obj p2 d)
    (match obj
      [(cons p1 (cons l1 (cons l2 null)))
       (let* ([adns (echo-pts-deep p1 p2 d)]
              [h1 (car adns)]
              [h2 (cdr adns)])
             (list p2 (cons h1 l1)
                      (cons h2 l2)))])))

(define stitch
  (lambda (l d)
    (let* ([p1 (car l)]
           [p2 (car (cdr l))]
           [sh (echo-pts-shallow p1 p2 d)]
           [iv (list p1 (car sh) (list-ref sh 1))]
           [r (fold (section stitcher _ _ d) iv (cdr l))]
           [l1 (reverse (list-ref r 1))]
           [l2 (list-ref r 2)])
          (append l1
                  l2 
                  (list (car l1))))))

;;; (tree-pts so-far n) -> (list-of pair?)
;;;    so-far : (list-of pair?)
;;;    n : nonnegative-integer?
;;; Creates the points of a tree with `n` levels of recursion. 
(define tree-pts
  (lambda (so-far n)
    (match n
      [0 so-far]
      [_ 
       (tree-pts 
         (let* ([shift-by (section shift-pt _ 0 250)]
                [rotation1 (section scale-rotate _ 0.5 (/ pi 2))]
                [rotation2 (section scale-rotate _ 0.5 (/ pi -10))]
                [make-left (section map (o shift-by rotation1) _)]
                [make-right (section map (o shift-by rotation2) _)]
                [all-pts (car so-far)]
                [endpts (cdr so-far)]
                [left (make-left all-pts)]
                [right (make-right all-pts)])
               (pair (append (list origin)
                             left
                             (reverse left)
                             right
                             (reverse right))
                     (append (make-left endpts)
                             (make-right endpts))))
                   (- n 1))])))


;; Process the points into something amenable to overlay/offset
(define my-tree-pts-base
  (|> (pair (list origin (pair 0 500)) (list (pair 0 500)))
      (section tree-pts _ 6)
      (section cdr _)
      (section map (section coord-normal->bad _ 1000 1000) _)))

(define my-tree-pts-trunk
  (|> (pair (list origin (pair 0 500)) (list (pair 0 500)))
      (section tree-pts _ 4)
      (section car _)
      (section stitch _ 3)
      (section map (section coord-normal->bad _ 1000 1000) _)
      (section path 1000 1000 _ "solid" "brown")))

(define my-tree-pts-leaves
  (|> my-tree-pts-base
      (section map (section pair (solid-circle 6 (rgb 255 0 255 50)) _) _)))

;;; (transform-tree-pts l w h)-> image?
;;;    l : (list-of pair?)
;;;    w : nonnegative-integer?
;;;    h : nonnegative-integer?
;;; Takes a list of image-coordinate pairs and overlays them appropriately
(define transform-tree-pts
  (lambda (l w h)
    (fold (lambda (img p) 
            (match p
              [(pair next-img (pair x y)) 
               (overlay/offset x y img next-img)]))
          (solid-rectangle w h (rgb 0 0 0 0))
          l)))


; (overlay/offset 6 6
;   (transform-tree-pts my-tree-pts-leaves 1000 1000)
;   my-tree-pts-trunk)

(define sakura
  (overlay/offset 6 6
    (transform-tree-pts my-tree-pts-leaves 1000 1000)
    my-tree-pts-trunk))


(problem "Pumpkin")

(define number-of-loops 5)
(define box-size 400)

(define pumpkin-vector
  (|> (range 0 6.4 (* 0.01 pi))
      (section map (lambda (t) 
                     (list (sin t) t))
                   _)
      (section make-vector number-of-loops _)))

(define pumpkin-helper
  (lambda (l i)
    (match l
      [(cons r (cons t null))
       (list (* 170 (expt r (expt 3 (- i 2)))) t)])))

(define repeat-lighter
  (lambda (c i)
    ((apply o (make-list i rgb-lighter)) c)))

(define repeat-darker
  (lambda (c i)
    ((apply o (make-list i rgb-darker)) c)))

(define pick-orange
  (lambda (i)
    (repeat-lighter (repeat-darker (color-name->rgb "orange") 3) (+ i 1))))

(define pumpkin (begin
  (vector-for-each
    (lambda (i)
      (vector-set! pumpkin-vector i (|> (vector-ref pumpkin-vector i)
                                        (section map (section pumpkin-helper _ i) _))))
    (vector-range 0 number-of-loops))
  (vector-map! (section map (section apply polar->cartesian _) _) pumpkin-vector)
  (vector-map! (section map (section coord-normal->bad _ box-size box-size) _) pumpkin-vector)
  (vector-for-each
    (lambda (i)
      (vector-set! pumpkin-vector i (|> (vector-ref pumpkin-vector i)
                                        (section path box-size box-size _ "solid" (pick-orange (* 2 i))))))
    (vector-range 0 (vector-length pumpkin-vector)))
  (|> pumpkin-vector
      vector->list
      reverse
      (section apply overlay _))))


;;;-----------------------------------------------------------------------------------;;;

;; Adjusted State Structure
(struct state (
  options-visible?         ; boolean, whether the main options are displayed
  plants-options-visible?  ; boolean, whether the Plants options are displayed
  selected-plant           ; string, the selected plant
  sunflower-visible?       ; boolean, whether the sunflower is visible
  pot-options-visible?     ; boolean, whether the pot options are displayed
  selected-pot             ; string, the selected pot
  pot1-plant               ; string, the plant in pot1
  pot2-plant               ; string, the plant in pot2
  pot3-plant               ; string, the plant in pot3
))

;; Initial state
(define initial-state (state #f #f "" #f #f "" "" "" ""))

;; Canvas dimensions
(define width background-width)
(define height background-height)



;; View function to render the canvas based on the state
(define view
  (lambda (st canv)
    (match st
      [(state options-visible? plants-options-visible? selected-plant sunflower-visible? pot-options-visible? selected-pot pot1-plant pot2-plant pot3-plant)
       (begin
         ;; Clear the canvas
         (canvas-drawing! canv 0 0 (generate-background background-height))

         ;; Main "Options" button
         (canvas-rectangle! canv 100 50 100 50 "solid" "lightblue")
         (canvas-text! canv 115 75 "Options" 20 "solid" "black")

         ;; Render based on visibility
         (cond
           ;; Case: Plants options are visible
           [plants-options-visible?
            (begin
              (canvas-rectangle! canv 0 150 100 50 "solid" "green")
              (canvas-text! canv 10 175 "Sunflower" 20 "solid" "white")
              (canvas-rectangle! canv 0 210 100 50 "solid" "green")
              (canvas-text! canv 10 235 "Daisy" 20 "solid" "white")
              (canvas-rectangle! canv 0 270 100 50 "solid" "green")
              (canvas-text! canv 10 295 "Sakura" 20 "solid" "white")
              (canvas-rectangle! canv 0 330 100 50 "solid" "green")
              (canvas-text! canv 10 355 "Pumpkin" 20 "solid" "white")
              (canvas-rectangle! canv 0 390 100 50 "solid" "green")
              (canvas-text! canv 10 415 "Bamboo" 20 "solid" "white"))]

           ;; Case: Main options are visible
           [options-visible?
            (begin
              (canvas-rectangle! canv 100 150 100 50 "solid" "lightgreen")
              (canvas-text! canv 130 175 "Plants" 20 "solid" "black")
              (canvas-rectangle! canv 100 220 100 50 "solid" "lightcoral")
              (canvas-text! canv 130 245 "Water" 20 "solid" "black"))]

           ;; Default case: Do nothing
           [else #f])

         ;; Render pot options if visible
         (if pot-options-visible?
           (begin
             (canvas-ellipse! canv 465 625 50 25 0 0 (* 2 pi) "solid" "brown")
             (canvas-text! canv 435 625 "Pot1" 20 "solid" "white")
             (canvas-ellipse! canv 695 625 50 25 0 0 (* 2 pi) "solid" "brown")
             (canvas-text! canv 665 625 "Pot2" 20 "solid" "white")
             (canvas-ellipse! canv 915 625 50 25 0 0 (* 2 pi) "solid" "brown")
             (canvas-text! canv 885 625 "Pot3" 20 "solid" "white"))void)

         ;; Draw the plants in the pots
         (cond
           [(string=? pot1-plant "Sunflower") (canvas-drawing! canv 370 200 (sunflower 60 "orange" "yellow" "darkgreen" "green" "brown" "black"))]
           [(string=? pot1-plant "Daisy") (canvas-drawing! canv 370 200 (sunflower 60 "white" "yellow" "darkgreen" "green" "yellow" "black"))]
           [(string=? pot1-plant "Sakura") (canvas-drawing! canv 0 0 sakura)]
           [(string=? pot1-plant "Pumpkin") (canvas-drawing! canv 270 300 pumpkin)]
           [(string=? pot1-plant "Bamboo") (canvas-drawing! canv 380 150 (rotate 0 stem))]
           [else #f])
         (cond
           [(string=? pot2-plant "Sunflower") (canvas-drawing! canv 600 200 (sunflower 60 "orange" "yellow" "darkgreen" "green" "brown" "black"))]
           [(string=? pot2-plant "Daisy") (canvas-drawing! canv 600 200 (sunflower 60 "white" "yellow" "darkgreen" "green" "yellow" "black"))]
           [(string=? pot2-plant "Sakura") (canvas-drawing! canv 180 0 sakura)]
           [(string=? pot2-plant "Pumpkin") (canvas-drawing! canv 500 300 pumpkin)]
           [(string=? pot2-plant "Bamboo") (canvas-drawing! canv 600 150 (rotate 0 stem))]
           [else #f])
         (cond
           [(string=? pot3-plant "Sunflower") (canvas-drawing! canv 820 200 (sunflower 60 "orange" "yellow" "darkgreen" "green" "brown" "black"))]
           [(string=? pot3-plant "Daisy") (canvas-drawing! canv 820 200 (sunflower 60 "white" "yellow" "darkgreen" "green" "yellow" "black"))]
           [(string=? pot3-plant "Sakura") (canvas-drawing! canv 400 0 sakura)]
           [(string=? pot3-plant "Pumpkin") (canvas-drawing! canv 730 300 pumpkin)]
           [(string=? pot3-plant "Bamboo") (canvas-drawing! canv 820 150 (rotate 0 stem))]
           [else #f]))])))

;; Update function to handle events
(define update
  (lambda (msg st)
    (match msg
      [(event-mouse-click _ cx cy)
       (cond
         ;; Main "Options" button
         [(and (> cx 100) (< cx 200) (> cy 50) (< cy 100))
          (state (not (state-options-visible? st)) #f (state-selected-plant st) (state-sunflower-visible? st) #f (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st))]

         ;; "Plants" button
         [(and (state-options-visible? st) (> cx 100) (< cx 200) (> cy 150) (< cy 200))
          (state #t #t (state-selected-plant st) (state-sunflower-visible? st) #f (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st))]

         ;; Plant selection
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Sunflower")) (state #t #f "Sunflower" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st))]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Daisy")) (state #t #f "Daisy" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st))]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Sakura")) (state #t #f "Sakura" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st))]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Pumpkin")) (state #t #f "Pumpkin" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st))]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Bamboo")) (state #t #f "Bamboo" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st))]

         ;; Pot selection
         [(and (state-pot-options-visible? st) (pot-clicked? cx cy "Pot1")) (state (state-options-visible? st) (state-plants-options-visible? st) (state-selected-plant st) (state-sunflower-visible? st) #f "Pot1" (state-selected-plant st) (state-pot2-plant st) (state-pot3-plant st))]
         [(and (state-pot-options-visible? st) (pot-clicked? cx cy "Pot2")) (state (state-options-visible? st) (state-plants-options-visible? st) (state-selected-plant st) (state-sunflower-visible? st) #f "Pot2" (state-pot1-plant st) (state-selected-plant st) (state-pot3-plant st))]
         [(and (state-pot-options-visible? st) (pot-clicked? cx cy "Pot3")) (state (state-options-visible? st) (state-plants-options-visible? st) (state-selected-plant st) (state-sunflower-visible? st) #f "Pot3" (state-pot1-plant st) (state-pot2-plant st) (state-selected-plant st))]

         ;; Default: Return current state
         [else st])]
      ;; Default: Return current state
      [else st])))

;; Helper function to check if a plant button was clicked
(define plant-clicked?
  (lambda (cx cy plant)
    (cond
      [(string=? plant "Sunflower")
       (and (> cx 0) (< cx 100) (> cy 150) (< cy 200))]
      [(string=? plant "Daisy")
       (and (> cx 0) (< cx 100) (> cy 210) (< cy 260))]
      [(string=? plant "Sakura")
       (and (> cx 0) (< cx 100) (> cy 270) (< cy 320))]
      [(string=? plant "Pumpkin")
       (and (> cx 0) (< cx 100) (> cy 330) (< cy 380))]
      [(string=? plant "Bamboo")
       (and (> cx 0) (< cx 100) (> cy 390) (< cy 440))]
      [else #f])))

;; Helper function to check if a pot button was clicked
(define pot-clicked?
  (lambda (cx cy pot)
    (cond
      [(string=? pot "Pot1")
       (and (> cx 400) (< cx 500) (> cy 600) (< cy 650))]
      [(string=? pot "Pot2")
       (and (> cx 630) (< cx 730) (> cy 600) (< cy 650))]
      [(string=? pot "Pot3")
       (and (> cx 850) (< cx 950) (> cy 600) (< cy 650))]
      [else #f])))

;; Reactive canvas with subscriptions
(display
 (reactive-canvas
   width height
   initial-state
   view
   update
   (on-mouse-click)))
