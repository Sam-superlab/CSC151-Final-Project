;;; individual-plants.scm
;;; Made by Sam Hopkins
(import lab)
(import image)

(title "Individual plants")
(part "Point manipulation utilities")

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
(sunflower 100 "orange" "yellow" "darkgreen" "green" "brown" "black")

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


(overlay/offset 6 6
  (transform-tree-pts my-tree-pts-leaves 1000 1000)
  my-tree-pts-trunk)

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

(begin
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
      (section apply overlay _)))


(problem "Water lily")

(define water-lily
  (lambda (size color)
    (|> (range (* -1 size) (+ size 0.01) (* 0.5 size))
        (section map (section pair size _) _)
        (section map (lambda (p)
                       (lambda (t)
                         (* (magnitude p)
                            (cos (* 3 (- t (angle1 p))))))) _)
        (section map (section polar-path-solid _ 0 6.29 0.01
                                                 (* size 4) (* size 4)
                                                 color) _)
        (section apply overlay _)
        (section rotate -90 _)
        (lambda (img) 
          (overlay/align "middle" "bottom"
                         (solid-ellipse (image-width img)
                                        (* 0.6 (image-height img))
                                        "green")
                         img)))))

(water-lily 90 (rgb 255 0 0 40))

(problem "Hydrangea")

(define hydrangea
  (|> (polar-plot (lambda (t) (* 20 (cos (* 2 t)))) 0 6.4 0.03)
      (section map (section shift-pt _ 90 90) _)
      (section spiral _ 0.99 1 1 370 370 "blue" "indigo")))

hydrangea

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
      (section path 1000 1000 _ "outline" "green")))

(rotate 0 stem)

(part "Not yet implemented, will take some additional tinkering")

(problem "Daisy")
; should be very similar to sunflower

(problem "Fern")
; can do with variations of the branch/tree trunk algorithms

(problem "Rose")
; some ideas, but not sure of the best way. 
