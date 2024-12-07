(import canvas)
(import reactive)

(struct state (
  options-visible?
  plants-options-visible?
  selected-plant
))

(define width 400)
(define height 400)

(define initial-state (state #f #f 'none))

(define clear-canvas
  (lambda (canv)
    (begin
      (canvas-rectangle! canv 0 0 width height "solid" "white"))))

(define render-main-options-button
  (lambda (canv)
    (begin
      (canvas-rectangle! canv 150 50 100 50 "solid" "lightblue")
      (canvas-text! canv 165 75 "Options" 20 "solid" "black"))))

(define render-main-options
  (lambda (canv)
    (begin
      (canvas-rectangle! canv 150 150 100 50 "solid" "lightgreen")
      (canvas-text! canv 180 175 "Plants" 20 "solid" "black")

      (canvas-rectangle! canv 150 220 100 50 "solid" "lightcoral")
      (canvas-text! canv 180 245 "Water" 20 "solid" "black"))))

(define render-plant-options
  (lambda (canv)
    (begin
      (canvas-rectangle! canv 50 150 100 50 "solid" "green")
      (canvas-text! canv 85 175 "Rose" 20 "solid" "white")

      (canvas-rectangle! canv 150 150 100 50 "solid" "green")
      (canvas-text! canv 160 175 "Sunflower" 20 "solid" "white")

      (canvas-rectangle! canv 250 150 100 50 "solid" "green")
      (canvas-text! canv 280 175 "Daisy" 20 "solid" "white")

      (canvas-rectangle! canv 50 220 100 50 "solid" "green")
      (canvas-text! canv 85 245 "Tulip" 20 "solid" "white")

      (canvas-rectangle! canv 150 220 100 50 "solid" "green")
      (canvas-text! canv 165 245 "Lily" 20 "solid" "white")

      (canvas-rectangle! canv 250 220 100 50 "solid" "green")
      (canvas-text! canv 275 245 "Cactus" 20 "solid" "white"))))

(define overlay-canvas
  (let ([oc (make-canvas 50 50)])
    (begin (canvas-rectangle! oc 0 0 50 50 "solid" "blue")
    oc)))


(define render-selected-plant
  (lambda (canv selected-plant)
    (if (equal? selected-plant 'none)
        (void) ; No plant selected, do nothing
        (canvas-rectangle! canv 180 300 50 50 "solid" "blue"))))




(define view
  (lambda (st canv)
    (begin
      (match st
        [(state options-visible? plants-options-visible? selected-plant)
         (begin
           (clear-canvas canv)
           (render-main-options-button canv)
           (cond
             [plants-options-visible? (render-plant-options canv)]
             [options-visible? (render-main-options canv)]
             [else #f])
           (render-selected-plant canv selected-plant))]))))

(define main-options-clicked?
  (lambda (cx cy)
    (begin
      (and (> cx 150) (< cx 250) (> cy 50) (< cy 100)))))

(define plants-button-clicked?
  (lambda (cx cy st)
    (begin
      (and (state-options-visible? st) (> cx 150) (< cx 250) (> cy 150) (< cy 200)))))

(define water-button-clicked?
  (lambda (cx cy st)
    (begin
      (and (state-options-visible? st) (> cx 150) (< cx 250) (> cy 220) (< cy 270)))))

(define plant-clicked?
  (lambda (cx cy plant)
    (begin
      (cond
        [(equal? plant 'rose)
         (and (> cx 50) (< cx 150) (> cy 150) (< cy 200))]
        [(equal? plant 'sunflower)
         (and (> cx 150) (< cx 250) (> cy 150) (< cy 200))]
        [(equal? plant 'daisy)
         (and (> cx 250) (< cx 350) (> cy 150) (< cy 200))]
        [(equal? plant 'tulip)
         (and (> cx 50) (< cx 150) (> cy 220) (< cy 270))]
        [(equal? plant 'lily)
         (and (> cx 150) (< cx 250) (> cy 220) (< cy 270))]
        [(equal? plant 'cactus)
         (and (> cx 250) (< cx 350) (> cy 220) (< cy 270))]
        [else #f]))))

(define update
  (lambda (msg st)
    (begin
      (match msg
        [(event-mouse-click _ cx cy)
         (begin
           (cond
             [(main-options-clicked? cx cy) (toggle-main-options st)]
             [(plants-button-clicked? cx cy st) (show-plants-options st)]
             [(water-button-clicked? cx cy st) (start-water-animation st)]
             [(plant-clicked? cx cy 'rose) (select-plant st 'rose)]
             [(plant-clicked? cx cy 'sunflower) (select-plant st 'sunflower)]
             [(plant-clicked? cx cy 'daisy) (select-plant st 'daisy)]
             [(plant-clicked? cx cy 'tulip) (select-plant st 'tulip)]
             [(plant-clicked? cx cy 'lily) (select-plant st 'lily)]
             [(plant-clicked? cx cy 'cactus) (select-plant st 'cactus)]
             [else st]))]
        [else st]))))

(define toggle-main-options
  (lambda (st)
    (begin
      (state (not (state-options-visible? st)) #f 'none))))

(define show-plants-options
  (lambda (st)
    (begin
      (state #t #t (state-selected-plant st)))))

(define start-water-animation
  (lambda (st)
    (begin
      ;; Animation logic placeholder
      st)))

(define select-plant
  (lambda (st plant)
    (begin
      (state #t #f plant))))

(reactive-canvas
 width height
 initial-state
 view
 update
 (on-mouse-click))
