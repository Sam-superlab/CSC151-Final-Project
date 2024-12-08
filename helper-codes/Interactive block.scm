(import canvas)
(import reactive)
(import lab)
(import image)


;---------------------------------------------Background
;; generate-background.scm
;; By Sam Hopkins
(import image)

(define background-width 1390)
(define background-height 730)
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
(generate-background 200)




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

(define sunflower-outer-petals
  (lambda ()
    (|> (polar-plot (lambda (t) (+ 100 (* 55 (cos (* 9 t))))) 0 6.3 0.03)
        (section spiral _ 0.9 10.2 100 400 400 "orange" "yellow"))))

(define sunflower-inner-petals
  (lambda ()
    (|> (list (pair 40 40) (pair 40 20) (pair 20 40) (pair 40 40))
        (section spiral _ 0.995 0.2 1 400 400 "darkgreen" "green"))))

(define sunflower-seeds
  (lambda ()
    (|> (polar-plot (lambda (t) 6) 0 6.3 (* 0.1 pi))
        (section map (section shift-pt _ 32 32) _)
        (section spiral _ 0.993 3.8832215 1 400 400 "brown" "black"))))

(define sunflower-head
  (overlay (sunflower-seeds) 
           (sunflower-inner-petals)
           (sunflower-outer-petals)))



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


; (problem "Hydrangea")

; (define hydrangea
;   (|> (polar-plot (lambda (t) (* 20 (cos (* 2 t)))) 0 6.4 0.03)
;       (section map (section shift-pt _ 90 90) _)
;       (section spiral _ 0.99 1 1 370 370 "blue" "indigo")))



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


;;;-----------------------------------------------------------------------------------;;;

;; Adjusted State Structure
(struct state (
  options-visible?         ; boolean, whether the main options are displayed
  plants-options-visible?  ; boolean, whether the Plants options are displayed
  selected-plant           ; string, the selected plant
))

;; Initial state
(define initial-state (state #f #f ""))

;; Canvas dimensions
(define width 800)
(define height 600)



;; View function to render the canvas based on the state
(define view
  (lambda (st canv)
    (match st
      [(state options-visible? plants-options-visible? selected-plant)
       (begin
         ;; Clear the canvas
         (canvas-drawing! canv 0 0 (generate-background 500))

         ;; Main "Options" button
         (canvas-rectangle! canv 150 50 100 50 "solid" "lightblue")
         (canvas-text! canv 165 75 "Options" 20 "solid" "black")

         ;; Render based on visibility
         (cond
           ;; Case: Plants options are visible
           [plants-options-visible?
            (begin
              (canvas-rectangle! canv 50 150 100 50 "solid" "green")
              (canvas-text! canv 60 175 "Sunflower" 20 "solid" "white")
              (canvas-rectangle! canv 250 150 100 50 "solid" "green")
              (canvas-text! canv 280 175 "Daisy" 20 "solid" "white")
              (canvas-rectangle! canv 50 220 100 50 "solid" "green")
              (canvas-text! canv 85 245 "Tulip" 20 "solid" "white")
              (canvas-rectangle! canv 150 220 100 50 "solid" "green")
              (canvas-text! canv 165 245 "Lily" 20 "solid" "white")
              (canvas-rectangle! canv 250 220 100 50 "solid" "green")
              (canvas-text! canv 275 245 "Bamboo" 20 "solid" "white"))]

           ;; Case: Main options are visible
           [options-visible?
            (begin
              (canvas-rectangle! canv 150 150 100 50 "solid" "lightgreen")
              (canvas-text! canv 180 175 "Plants" 20 "solid" "black")
              (canvas-rectangle! canv 150 220 100 50 "solid" "lightcoral")
              (canvas-text! canv 180 245 "Water" 20 "solid" "black"))]

           ;; Default case: Do nothing
           [else #f])

         ;; Draw the selected plant
         (cond
           [(string=? selected-plant "Sunflower") (canvas-drawing! canv 400 200 sunflower-head)]
           [(string=? selected-plant "Daisy") (canvas-rectangle! canv 180 300 50 50 "solid" "black")]
           [(string=? selected-plant "Tulip") (canvas-rectangle! canv 180 300 50 50 "solid" "pink")]
           [(string=? selected-plant "Lily") (canvas-drawing! canv 400 200 (water-lily 150 "pink"))]
           [(string=? selected-plant "Bamboo") (canvas-drawing! canv 450 150 (rotate 0 stem))]
           [else #f]))])))

;; Update function to handle events
(define update
  (lambda (msg st)
    (match msg
      [(event-mouse-click _ cx cy)
       (cond
         ;; Main "Options" button
         [(and (> cx 150) (< cx 250) (> cy 50) (< cy 100))
          (state (not (state-options-visible? st)) #f "")]

         ;; "Plants" button
         [(and (> cx 150) (< cx 250) (> cy 150) (< cy 200) (state-options-visible? st))
          (state #t #t "")]

         ;; Plant selection
         [(plant-clicked? cx cy "Sunflower") (state #t #f "Sunflower")]
         [(plant-clicked? cx cy "Daisy") (state #t #f "Daisy")]
         [(plant-clicked? cx cy "Tulip") (state #t #f "Tulip")]
         [(plant-clicked? cx cy "Lily") (state #t #f "Lily")]
         [(plant-clicked? cx cy "Bamboo") (state #t #f "Bamboo")]

         ;; Default: Return current state
         [else st])]
      ;; Default: Return current state
      [else st])))

;; Helper function to check if a plant button was clicked
(define plant-clicked?
  (lambda (cx cy plant)
    (cond
      [(string=? plant "Sunflower")
       (and (> cx 50) (< cx 150) (> cy 150) (< cy 200))]
      [(string=? plant "Sunflower")
       (and (> cx 150) (< cx 250) (> cy 150) (< cy 200))]
      [(string=? plant "Daisy")
       (and (> cx 250) (< cx 350) (> cy 150) (< cy 200))]
      [(string=? plant "Tulip")
       (and (> cx 50) (< cx 150) (> cy 220) (< cy 270))]
      [(string=? plant "Lily")
       (and (> cx 150) (< cx 250) (> cy 220) (< cy 270))]
      [(string=? plant "Bamboo")
       (and (> cx 250) (< cx 350) (> cy 220) (< cy 270))]
      [else #f])))

;; Reactive canvas with subscriptions
(display
 (reactive-canvas
   width height
   initial-state
   view
   update
   (on-mouse-click)))
