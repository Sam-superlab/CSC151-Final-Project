; animation-practice2.scm

(import image)
(import canvas)
(import reactive)
(import html)
(import lab)
(import image)

(define coord-normal->bad
  (lambda (p w h)
    (match p
      [(pair x y) 
       (pair (+ (/ w 2) x) 
             (- (/ h 2) y))])))

(define coord-bad->normal
  (lambda (p w h)
    (match p
      [(pair x y)
       (pair (- x (/ w 2)) 
             (- (/ h 2) y))])))


(define polar->cartesian
  (lambda (r t)
    (pair (* r (cos t))
          (* r (sin t)))))

(define angle1
  (lambda (p)
    (atan (/ (cdr p) (car p)))))

(define magnitude
  (section match _
    [(pair x y)
     (sqrt (+ (* x x) (* y y)))]))

(define polar-plot
  (lambda (f tmin tmax step)
    (|> (range tmin tmax step)
        (section map (lambda (t) (list (f t) t)) _)
        (section map (section apply polar->cartesian _) _))))

(define polar-path-solid
  (lambda (f tmin tmax step w h color)
    (|> (polar-plot f tmin tmax step)
        (section map (section coord-normal->bad _ w h) _)
        (section path w h _ "solid" color))))

(define shift-pt
  (lambda (pt dx dy)
    (pair (+ (car pt) dx)
          (+ (cdr pt) dy))))

(define spiral
  (lambda (pts det theta epsilon w h color1 color2)
    (spiral/helper pts det theta epsilon 
                   w h color1 color2 
                   (solid-rectangle w h (rgb 0 0 0 0)))))

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

(define genlt
  (lambda (p a b c d)
    (let* ([x (car p)]
           [y (cdr p)])
          (pair (+ (* a x) (* b y))
                (+ (* c x) (* d y))))))

(define scale-rotate
  (lambda (p det theta)
    (let* ([c (* (cos theta) det)]
           [s (* (sin theta) det)]
           [-s (* -1 s)])
          (genlt p c -s s c))))

(define sunflower-outer-petals
  (lambda (size color1 color2)
    (|> (polar-plot (lambda (t) (+ size (* 0.55 size (cos (* 9 t))))) 0 6.3 0.03)
        (section spiral _ 0.9 10.2 size (* size 3.25) (* size 3.25) color1 color2))))

(define sunflower-inner-petals
  (lambda (size color1 color2)
    (|> (let* ([x1 (* size 0.4)]
               [x2 (* size 0.2)])
              (list (pair x1 x1) (pair x1 x2) (pair x2 x1) (pair x1 x1)))
        (section spiral _ 0.995 0.2 1 (* size 3.25) (* size 3.25) color1 color2))))

(define sunflower-seeds
  (lambda (size color1 color2)
    (|> (polar-plot (lambda (t) (* size 0.06)) 0 6.3 (* 0.1 pi))
        (section map (section shift-pt _ (* size 0.32) (* size 0.32)) _)
        (section spiral _ 0.993 3.8832215 1 (* size 3.25) (* size 3.25) color1 color2))))

(define sunflower-head
  (lambda (size oc1 oc2 ic1 ic2 sc1 sc2)
    (overlay (sunflower-seeds size sc1 sc2)
             (sunflower-inner-petals size ic1 ic2)
             (sunflower-outer-petals size oc1 oc2))))

(define sunflower
  (lambda (size oc1)
    (overlay/align "middle" "top"
      (sunflower-head size oc1 "yellow" "darkgreen" "green" "brown" "black")
      (above 
        (solid-square size (rgb 0 0 0 0))
        (overlay (solid-rectangle (* 0.2 size) (* 3.9 size) "darkgreen")
                 (solid-rectangle (* 0.3 size) (* 4 size) "green"))))))

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

(define canv
  (make-canvas 1000 1000))

(display canv)

(define plant-grow2
  (lambda (plant color)
    (ignore
      (animate-with
        (lambda (time)
          (let ([size (if (< (round time) 1000) 10 (if (< (round time) 2000) 25 50))])
               (begin
                 (canvas-rectangle! canv 0 0 500 500 "solid" "white")
                 (canvas-drawing! canv 0 0 (plant size color)) #t)))))))

(plant-grow2 water-lily "blue")
(plant-grow2 sunflower "orange")
