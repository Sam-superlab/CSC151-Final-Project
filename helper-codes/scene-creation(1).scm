;;; scene-creation.scm
;;; Made by Sam Hopkins
(import lab)
(import image)

(title "Scene creation")
(description "by Sam Hopkins")
(part "Background")

;;; Initial definitions to establish the size of the outputted scene.
;;; If everything is done properly, changing these will appropriately
;;; change everything else in the program to match. For this to work,
;;; the aspect ratio should be reasonable. 
(define background-width 1390) ; 1390 for Sam H's laptop
(define background-height 730) ; 730 for Sam H's laptop
(define aspect-ratio 
  (/ background-width
     background-height))

;;; (generate-background s) -> drawing?
;;;    s : nonnegative-integer?
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
            (* s 0.4)]
           [empty-pot
            (overlay/align "middle" "top"
              (overlay (make-pot-rim (* ps 0.85) (c 9))
                       (make-pot-rim ps (c 10)))
              (overlay/align "middle" "bottom"
                (overlay (make-pot-base (* ps 0.9) (c 9))
                         (make-pot-base ps (c 10)))
                (solid-rectangle ps (* 1.1 ps) (c 11))))]
           [spacer2
            (solid-square (* s 0.0085) (c 11))]
           [dirt
            (make-pot-rim (* ps 0.78) (c 8))]
           [pot
            (overlay/align "middle" "top" 
                           (above spacer2 dirt) 
                           empty-pot)]
           [pots 
            (above (beside pot spacer pot spacer pot spacer pot)
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

(part "Point manipulation utilities")

;;; The center of a plane
(define origin (pair 0 0))

;;; (identity x) -> any?
;;;    x : any?
;;; Returns `x` unchanged (useful for testing).
(define identity
  (lambda (x)
    x))

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

;;; (draw-at d pts cw ch) -> drawing?
;;;    d : drawing?
;;;    pts : (list-of pair?)
;;;    cw : nonnegative-integer?
;;;    ch : nonnegative-integer?
;;; Returns a drawing of width `cw` and height `ch`, consisting of 
;;; the drawing `d` at every point given in `pts`. Note: the points
;;; are assumed to be written with respect to a 'normal' coordinate system,
;;; with the origin at the center of the drawing. Each pair in `pts` marks
;;; where the center of a drawing will go, rather than the top-left corner.
;;; The first point in the list is the one whose drawing goes on top. 
(define draw-at
  (lambda (d pts cw ch)
    (let* ([xshift (* -0.5 (image-width d))]
           [yshift (* -0.5 (image-height d))]
           [badpts 
            (map (section coord-normal->bad _ cw ch) pts)])
          (fold 
            (lambda (i p)
              (overlay/offset
                (+ xshift (car p))
                (+ yshift (cdr p))
                i d))
            (solid-rectangle cw ch (rgb 0 0 0 0))
            badpts))))

(part "Tree procedures")

;;; Struct definitions for handling tree structures
;;; (in the very literal sense of making visual art of trees)
(struct node (coord dir depth branches))

;;; Next, we establish some helper functions to make operating over trees
;;; more convenient. Note that while the documentation strings sometimes
;;; indicate parameters should be of a certain type as a reminder of their
;;; purpose (e.g., depth will be a number), these requirements will not always
;;; be enforced in our functions' implementation (e.g. the `tree?` predicate),
;;; rather we will assume some preconditions are met. 

;;; (true? v) -> boolean?
;;;    v : any?
;;; Returns #t iff v is #t
(define true?
  (section equal? #t _))

;;; (tree? T) -> boolean?
;;;    T : any?
;;; Returns #t iff `T` is a tree.
(define tree?
  (lambda (T)
    (match T 
      [(node _ _ _ branches)
       (and (list? branches)
            ((list-of true?)
             (map tree? branches)))]
      [_ #f])))

;;; (leaf coord dir depth) -> node?
;;;    coord : pair?
;;;    dir : any?
;;;    depth : number?
;;; Returns a node of degree one (in the graph-theoretic sense), 
;;; i.e., the only adjacent node is the parent, there are no children. 
;;; This is sometimes called a singleton.
(define leaf
  (lambda (coord dir depth)
    (node coord dir depth null)))

;;; (leaf? T) -> boolean?
;;;    T : any?
;;; Returns true iff `T` is a leaf in the sense defined above.
(define leaf?
  (lambda (T)
    (and (node? T)
         (null? (node-branches T)))))

;;; (root coord dir depth branch) -> node?
;;;    coord : pair?
;;;    dir : any?
;;;    depth : number?
;;;    branch : tree?
;;; Returns a node with only one branch, for the base of
;;; a tree (in this case it is technically a leaf as well, 
;;; although our procedures will not treat it as such)
(define root
  (lambda (coord dir depth branch)
    (node coord dir depth (cons branch null))))

;;; (root? T) -> boolean?
;;;    T : tree?
;;; Returns true iff `T` is a root in the sense that it
;;; has only one branch. 
(define root?
  (lambda (T)
    (match T
      [(node _ _ _ (cons (node _ _ _ _) null))
       #t]
      [_ #f])))

;;; (node-branch1 T) -> tree?
;;;    T : tree?, not a leaf
;;; Returns the first branch of `T`. 
(define node-branch1
  (lambda (T)
    (if (leaf? T)
        (error
         "node-branch1 expected a node with branches, received a leaf")
        (car (node-branches T)))))

;;; (node-branch T) -> tree?
;;;    T : tree?, not a leaf
;;; Alias for `node-branch1` (see above)
(define node-branch node-branch1)

;;; (node-branch2 T) -> tree?
;;;    T : tree?, not a leaf or root
;;; Returns the second branch of `T`. 
(define node-branch2
  (lambda (T)
    (cond 
      [(leaf? T)
       (error
         "node-branch2 expected a node with branches, received a leaf")]
      [(root? T)
       (error
         "node-branch2 expected a node with >=2 branches, received a root")]
      [(node? T)
       (car (cdr (node-branches T)))]
      [_ (error "node-branch2 expected a tree, received something else")])))

;;; (tree-map f T) -> tree?
;;;    f : procedure? (takes and outputs a pair of numbers)
;;;    T : tree?
;;; Transforms all the coord fields in `T` by applying `f`. Also increments
;;; each `depth` field. 
(define tree-map
  (lambda (f T)
    (match T
      [(node coord dir depth branches)
       (node (f coord) dir (+ 1 depth) 
             (map (section tree-map f _) branches))]
      [_
       (error "tree-map expected a tree, received something else")])))

;;; (tree->list T min-depth) -> list?
;;;    T : tree?
;;;    min-depth : positive-integer?
;;; Returns a list of the coord fields contained in `T`.
(define tree->list
  (lambda (T min-depth)
    (match T
      [(node coord _ depth null)
       (if (>= depth min-depth)
           (list coord)
           null)]
      [(node coord _ depth branches)
       (if (>= depth min-depth)
           (cons coord (apply append (map (section tree->list _ min-depth) branches)))
           (apply append (map (section tree->list _ min-depth) branches)))])))

;;; (evolve-tree T ht det1 det2 theta1 theta2) -> tree?
;;;    T : root?
;;;    ht : nonnegative-number?
;;;    det1 : number?
;;;    det2 : number?
;;;    theta1 : number?
;;;    theta2 : number?
;;; Increases the number of coordinates in `T` by means of rotating,
;;; scaling, and duplicating the existing tree's coordinates. In particular, 
;;; two copies of `T` are made, the first of which is rotated by `theta1`
;;; and scaled by `det1` and the second of which is rotated by `theta2`
;;; and scaled by `det2`. Both copies are then shifted upward by `ht`
;;; (generally the height of the central node).
(define evolve-tree
  (lambda (T ht det1 det2 theta1 theta2)
    (match T
      [(node coord dir depth (cons branch null))
       (let* ([inc-ht (section shift-pt _ 0 ht)]
              [f1 (o inc-ht (section scale-rotate _ det1 theta1))]
              [f2 (o inc-ht (section scale-rotate _ det2 theta2))])
             (root coord dir depth 
                   (node (node-coord branch)
                         (node-dir branch)
                         (node-depth branch)
                         (list (tree-map f1 branch)
                               (tree-map f2 branch)))))])))

;;; (update-dir branch c) -> node?
;;;    branch : node?
;;;    c : pair?, elements are numbers
;;; Returns a new node based on `branch` with the direction
;;; field updated so that it contains the coordinate point
;;; representing the change in direction of the coord field
;;; of `branch` with respect to the point `c`
(define update-dir
  (lambda (branch c)
    (match branch
      [(node coord _ depth branches)
       (node coord
             (subtract-pt coord c)
             depth branches)])))

;;; (calculate-dir T) -> tree?
;;;    T : tree?
;;; Recursively updates all the direction fields contained in the tree `T`
;;; so that they indicate the change in direction of each coordinate
;;; compared to the coordinate of the parent node
(define calculate-dir
  (lambda (T)
    (match T
      [(node coord dir depth branches)
       (node coord dir depth 
             (map 
               (lambda (branch)
                 (if (leaf? branch)
                     (update-dir branch coord)
                     (calculate-dir (update-dir branch coord))))
               branches))]
      [_ (error "calculate-dir expected a tree, received something else")])))

;;; (trace-tree T) -> (list-of pair?)
;;;    T : node?
;;;    d : positive-number?
;;;    so-far : (list-of pair?)
;;; Attempts to create a list of points that traces around the given tree `T`.
;;; `d` controls the thickness of the trace, and `so-far` is the initial
;;; list of points. 
(define trace-tree
  (lambda (d T so-far)
    (let* ([coord (node-coord T)])
          (if (leaf? T)
              (cons coord so-far)
              (let* ([v (node-dir T)]
                     [m (/ d
                           (magnitude v)
                           (node-depth T))]
                     [m1 (* m (car v))]
                     [m2 (* m (cdr v))]
                     [ccw (pair (* -1 m2) m1)]
                     [l (add-pt coord ccw)]
                     [r (subtract-pt coord ccw)]
                     [r+ (cons r so-far)]
                     [c (shift-pt coord m1 m2)])
                    (if (root? T)
                        (cons l
                          (trace-tree d
                            (node-branch T)
                            r+))
                        (cons l
                          (trace-tree d
                            (node-branch1 T)
                            (cons c
                              (trace-tree d
                                (node-branch2 T)
                                r+))))))))))

;;; (tree-pts size n ht det1 det2 theta1 theta2 boost) -> tree?
;;;    size : nonnegative-integer?
;;;    n : positive-integer?
;;;    ht : positive-number?
;;;    det1 : number?
;;;    det2 : number?
;;;    theta1 : number?
;;;    theta2 : number?
;;;    boost : boolean?
;;; Returns a tree of points. This is done by repeatedly
;;; evolving a base tree with `evolve-tree`, where `ht`, `det1`, `det2`, `theta1`,
;;; and `theta2` are the respective shift in height, determinants, and angles given
;;; as parameters to that transformation, and `n` is the number of times it is repeated.
;;; If `boost` is true, there will be one additional iteration with a determinant of 1
;;; for both branches. `size` is a general multiplier on the points. 
(define tree-pts
  (lambda (size n ht det1 det2 theta1 theta2 boost)
    (let* ([new-ht (* size ht)])
          (|> (root (pair 0 0) (pair 0 1) 1
                    (node (pair 0 new-ht) 0 2
                    null))
              (lambda (T)
                (if boost
                    (evolve-tree T new-ht 1 1 theta1 theta2)
                    T))
              (lambda (T) 
                ((apply o
                  (make-list n
                    (section evolve-tree _ new-ht
                             det1 det2 theta1 theta2))) 
                 T))
              (section calculate-dir _)))))

;;; (draw-trunk T color cw ch d) -> drawing?
;;;    T : tree? (from `tree-pts`)
;;;    color : color?
;;;    cw : nonnegative-integer?
;;;    ch : nonnegative-integer?
;;;    d : positive-number?
;;; Returns a drawing of `T` that may resemble a tree trunk. For this to work,
;;; `T` should come from `tree-pts` (see above). The drawing has width `cw` and height `cw`
;;; with color `color`, and is made via `trace-tree` and `path`. `d` is a parameter given 
;;; to `trace-tree`and controls the branch thickness. 
(define draw-trunk
  (lambda (T color cw ch d)
    (|> T
        (section calculate-dir _)
        (section trace-tree d _ null)
        (section map (section coord-normal->bad _ cw ch) _)
        (section map (section shift-pt _ 0 (* 0.5 ch)) _)
        (section path cw ch _ "solid" color))))

(part "Individual plants")

(problem "Plants which are more or less polished:")
(description "(could potentially add bonus features and details,
and may still need to optimize for interactivity or animation)")

(description "Sunflower")

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

;;; (stem-leaves size c1 c2) -> drawing?
;;;    size : nonnegative-integer?
;;;    c1 : color?
;;;    c2 : color?
;;; Returns a drawing that resembles a pair of leaves.
(define stem-leaves
  (lambda (size c1 c2)
    (let* ([f1 (lambda (t) (* 0.9 size (cos (* 2 t))))]
           [f2 (lambda (t) (* size (cos (* 2 t))))]
           [cs (* 2 size)] ; canvas size
           [left1
            (polar-path-solid 
              f1 2.356 3.927 0.03 cs cs c1)]
           [left2
            (polar-path-solid 
              f2 2.356 3.927 0.03 cs cs c2)]
           [right1
            (polar-path-solid 
              f1 -0.785 0.785 0.03 cs cs c1)]
           [right2
            (polar-path-solid 
              f2 -0.785 0.785 0.03 cs cs c2)])
          (overlay left1 right1 left2 right2))))

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
        (overlay (stem-leaves size ic1 ic2)
                 (solid-rectangle (* 0.2 size) (* 3.9 size) ic1)
                 (solid-rectangle (* 0.3 size) (* 4 size) ic2))))))

;; example of sunflower's configurable size and colors
; (sunflower 50 "purple" "yellow" "pink" "green" "cyan" "black")

;; default sunflower
(sunflower 80 "orange" "yellow" "darkgreen" "green" "brown" "black")

(description "Fern")

;;; (fern size color) -> drawing?
;;;    size : nonnegative-integer?
;;;    color : color?
;;; Returns a drawing that somewhat resembles a fern, by calling draw-trunk 
;;; and `tree-pts` with some specific parameters. `size` controls the size 
;;; and `color` controls the color. 
(define fern
  (lambda (size color)
    (|> (tree-pts size 9 0.5 0.65 0.55 0.15 -1.5 #t)
        (section draw-trunk _ color (* 2 size) (* 2 size) 5))))

(fern 200 "green")

(description "Sakura")

;;; (sakura size c1 c2) -> drawing?
;;;    size : nonnegative-integer?
;;;    c1 : color?
;;;    c2 : color?
;;; Returns a drawing that resembles a sakura tree. `size` controls the size
;;; while `c1` and `c2` control the colors. 
(define sakura
  (lambda (size c1 c2)
    (let* ([pts (tree-pts size 4 0.5 0.5 0.5 1.5 -0.3 #t)]
           [cs (* 2 size)]
           [trunk (draw-trunk pts c2 cs cs 4)]
           [flower (solid-circle (* 0.02 size) c1)]
           [leaf-pts (map 
                       (section shift-pt _ 0 (* -1 size))
                       (tree->list pts 4))]
           [leaves (draw-at flower leaf-pts cs cs)])
          (overlay 
            leaves 
            trunk))))

(sakura 300 "violet" "brown")

(description "Hydrangea")

;;; (define baby-hydrangea size color) -> drawing?
;;;    size : nonnegative-integer?
;;;    color : color?
;;; Returns a drawing that resembles an individual flower. `size`
;;; controls the length of the petals. `color` configures the color.
(define baby-hydrangea
  (lambda (size color)
    (polar-path-solid
      (lambda (t) (* size (cos (* 2 t))))
      0 6.4 0.03 (* size 2) (* 2 size) color)))

;;; (list-hydrangea-pts size) -> (list-of pair?)
;;;     size : nonnegative-number?
;;; Returns a list of points for use with hydrangea (see below).
(define list-hydrangea-pts
  (lambda (size)
    (let* ([indices (range 0.2 1 0.1)]
           [unrotated-pts 
            (map
              (lambda (v) 
                (let* ([new-v (* size v)])
                      (pair new-v new-v)))
              indices)]
           [angles
            (map
              (section * _ 9)
              indices)])
          (map (section scale-rotate _ 1 _) unrotated-pts angles))))

;;; (hydrangea size c1 c2 c3) -> drawing?
;;;     size : nonnegative-integer?
;;;     c1 : color?
;;;     c2 : color?
;;;     c3 : color?
;;; Returns a drawing that resembles a hydrangea. `size` controls the
;;; size, while `c1`, `c2`, and `c3` control the color of the inner flowers,
;;; the outer flowers, and the stems, respectively. 
(define hydrangea
  (lambda (size c1 c2 c3)
    (let* ([baby-size (* 0.4 size)]
           [baby1 (baby-hydrangea (* 0.7 baby-size) c1)]
           [baby2 (baby-hydrangea baby-size c2)]
           [baby (overlay baby1 baby2)]
           [pts (list-hydrangea-pts size)]
           [raised-pts (map (section shift-pt _ 0 (* 1 0)) pts)]
           [canvas-size (* size 6)]
           [base (pair 0 (* -3 size))]
           [flowers (draw-at baby raised-pts canvas-size canvas-size)]
           [stem-pts (fold 
                       (lambda (l p)
                         (cons base (cons p l)))
                       (list base)
                       raised-pts)]
           [stems (path canvas-size canvas-size
                        (map (section coord-normal->bad _ canvas-size canvas-size)
                             stem-pts)
                        "outline" c3)])
          (overlay flowers stems))))

(hydrangea 60 "blue" "violet" "green")

(problem "Plants where a basic version exists already:")
(description "(may need refinement to style or functionality)")

(description "Pumpkin")

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

; (begin
;   (vector-for-each
;     (lambda (i)
;       (vector-set! pumpkin-vector i (|> (vector-ref pumpkin-vector i)
;                                         (section map (section pumpkin-helper _ i) _))))
;     (vector-range 0 number-of-loops))
;   (vector-map! (section map (section apply polar->cartesian _) _) pumpkin-vector)
;   (vector-map! (section map (section coord-normal->bad _ box-size box-size) _) pumpkin-vector)
;   (vector-for-each
;     (lambda (i)
;       (vector-set! pumpkin-vector i (|> (vector-ref pumpkin-vector i)
;                                         (section path box-size box-size _ "solid" (pick-orange (* 2 i))))))
;     (vector-range 0 (vector-length pumpkin-vector)))
;   (|> pumpkin-vector
;       vector->list
;       reverse
;       (section apply overlay _)))

(description "Water lily")

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
        (section rotate 90 _)
        ; (lambda (img) 
        ;   (overlay/align "middle" "bottom"
        ;                  (solid-ellipse (image-width img)
        ;                                 (* 0.6 (image-height img))
        ;                                 "green")
        ;                  img))
      )))

; (water-lily 10 (rgb 255 0 255 50))

(description "Bamboo")

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
      (section stitch _ 1)
      (section map (section coord-normal->bad _ 1000 1000) _)
      (section path 1000 1000 _ "outline" "green")))

; (rotate 0 stem)

(problem "Plants which could realistically be added")
(description "(should be simple variations of existing plants,
requiring minimal tinkering)")

(description "Daisy")
; should be very similar to sunflower

(description "Rose")
; some ideas, but not sure of the best way. 

