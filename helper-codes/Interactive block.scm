;; (description "code stored in interactive-plant.scm")
;;; (my-canvas-drawing! canvas x y drawing) -> void?
;;;    canvas : canvas?
;;;    x : nonnegative-integer?
;;;    y : nonnegative-integer?
;;;    drawing : drawing?
;;; Mutates `canvas` so that `drawing` appears with its center
;;; aligned with `x` and its bottom aligned with `y`.
(define my-canvas-drawing!
  (lambda (canvas x y drawing)
    (let* ([adj-x (- x (* 0.5 (image-width drawing)))]
           [adj-y (- y (image-height drawing))])
          (canvas-drawing! canvas (round adj-x) (round adj-y) drawing))))

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
  water-message-visible?   ; boolean, whether the water message is visible
))

;; Initial state
(define initial-state (state #f #f "" #f #f "" "" "" "" #f))

;; Canvas dimensions
(define width background-width)
(define height background-height)

;; Pot positions
(define pot1-x  (* 0.34 width))
(define pot1-y  (* 0.68 height))
(define pot2-x (* 0.5 width))
(define pot2-y  (* 0.68 height))
(define pot3-x  (* 0.66 width))
(define pot3-y (* 0.68 height))

;;; (my-button canv x y color text) -> void
;;;    canv : canvas
;;;    x : number
;;;    y : number
;;;    color : string
;;;    text : string
;;; Draws a button with the given parameters on the canvas.
(define my-button
  (lambda (canv x y color text)
    (begin
    (canvas-rectangle! canv x y 100 50 "solid" color)
    (canvas-text! canv (+ x 15) (+ y 25) text 20 "solid" "black"))))

;;; (pot-button canv x y label) -> void
;;;    canv : canvas
;;;    x : number
;;;    y : number
;;;    label : string
;;; Draws a pot button with the given parameters on the canvas.
(define pot-button
  (lambda (canv x y label)
    (begin
      (canvas-ellipse! canv x y 50 25 0 0 (* 2 pi) "solid" "brown")
      (canvas-text! canv (- x 30) y label 20 "solid" "white"))))

;;; (draw-plant-in-pot canv x y plant) -> void
;;;    canv : canvas
;;;    x : number
;;;    y : number
;;;    plant : string
;;; Draws the specified plant in the pot at the given coordinates.
(define draw-plant-in-pot
  (lambda (canv x y plant)
    (cond
      [(string=? plant "Sunflower") (my-canvas-drawing! canv x y sunflower-drawing)]
      [(string=? plant "Fern") (my-canvas-drawing! canv x y (fern 200 "lightgreen"))]
      [(string=? plant "Sakura") (my-canvas-drawing! canv x y (sakura 200 "pink" "brown"))]
      [(string=? plant "Pumpkin") (my-canvas-drawing! canv x y (hydrangea 60 "cyan" "violet" "lightgreen"))]
      [(string=? plant "Bamboo") (my-canvas-drawing! canv x y bamboo-drawing)]
      [else #f])))

;;; (view st canv) -> void
;;;    st : state
;;;    canv : canvas
;;; Renders the canvas based on the current state.
(define view
  (lambda (st canv)
    (match st
      [(state options-visible? plants-options-visible? selected-plant sunflower-visible? pot-options-visible? selected-pot pot1-plant pot2-plant pot3-plant water-message-visible?)
       (begin
         ;; Clear the canvas
         (canvas-drawing! canv 0 0 (generate-background background-height))

         ;; Main "Options" button
         (my-button canv 100 50 "lightblue" "Options")

         ;; Render based on visibility
         (cond
           ;; Case: Plants options are visible
           [plants-options-visible?
            (begin
              (my-button canv 0 150 "green" "Sunflower")
              (my-button canv 0 210 "green" "Fern")
              (my-button canv 0 270 "green" "Sakura")
              (my-button canv 0 330 "green" "Pumpkin")
              (my-button canv 0 390 "green" "Bamboo"))]

           ;; Case: Main options are visible
           [options-visible?
            (begin
              (my-button canv 100 150 "lightgreen" "Plants")
              (my-button canv 100 220 "lightcoral" "Water"))]

           ;; Default case: Do nothing
           [else #f])

         ;; Render pot options if visible
         (if pot-options-visible?
           (begin
             (pot-button canv 465 625 "Pot1")
             (pot-button canv 695 625 "Pot2")
             (pot-button canv 915 625 "Pot3"))
           void)

         ;; Draw the plants in the pots
         (draw-plant-in-pot canv pot1-x pot1-y pot1-plant)
         (draw-plant-in-pot canv pot2-x pot2-y pot2-plant)
         (draw-plant-in-pot canv pot3-x pot3-y pot3-plant)

         ;; Render water message if visible
         (if water-message-visible?
           (begin
             (canvas-ellipse! canv 800 200 150 50 0 0 (* 2 pi) "solid" "lightblue")
             (canvas-text! canv 735 200 "Plants are happy :D" 20 "solid" "black"))void)
       )])))

;;; (update msg st) -> state
;;;    msg : event
;;;    st : state
;;; Updates the state based on the event message.
(define update
  (lambda (msg st)
    (match msg
      [(event-mouse-click _ cx cy)
       (let* ([plant-selection-helper
               (lambda (s)
                 (and (state-plants-options-visible? st)
                      (plant-clicked? cx cy s)))]
              [update-state-field-3
               (lambda (s)
                 (match st
                   [(state a b c d e f g h i j)
                    (state a #f s d #t f g h i j)]))]
              [pot-selection-helper
               (lambda (s)
                 (and (state-pot-options-visible? st)
                      (pot-clicked? cx cy s)))]
              [update-state-field-6
                (lambda (s n)
                 (match st
                   [(state a b c d e f g h i j)
                    (cond
                      [(= n 1)
                       (state a b c d #f s c h i #f)]
                      [(= n 2)
                       (state a b c d #f s g c i #f)]
                      [(= n 3) ; change these to make color options visible
                       (state a b c d #f s g h c #f)])]))])
             (cond
               ;; Main "Options" button
               [(and (> cx 100) (< cx 200) (> cy 50) (< cy 100))
                (state (not (state-options-visible? st)) #f (state-selected-plant st) (state-sunflower-visible? st) #f (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]
      
               ;; "Plants" button
               [(and (state-options-visible? st) (> cx 100) (< cx 200) (> cy 150) (< cy 200))
                (state #t #t (state-selected-plant st) (state-sunflower-visible? st) #f (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]
      
               ;; Plant selection
               [(plant-selection-helper "Sunflower") (update-state-field-3 "Sunflower")]
               [(plant-selection-helper "Fern") (update-state-field-3 "Fern")]
               [(plant-selection-helper "Sakura") (update-state-field-3 "Sakura")]
               [(plant-selection-helper "Pumpkin") (update-state-field-3 "Pumpkin")]
               [(plant-selection-helper "Bamboo") (update-state-field-3 "Bamboo")]

               ;; Pot selection
               [(pot-selection-helper "Pot1") (update-state-field-6 "Pot1" 1)]
               [(pot-selection-helper "Pot2") (update-state-field-6 "Pot2" 2)]
               [(pot-selection-helper "Pot3") (update-state-field-6 "Pot3" 3)]
      
               ;; "Water" button
               [(and (state-options-visible? st) (> cx 100) (< cx 200) (> cy 220) (< cy 270))
                (match st
                   [(state a b c d e f g h i j)
                    (state a b c d e f g h i #t)])]

               ;; Default: Return current state
               [else st]))]
      ;; Default: Return current state
      [else st])))

;;; (plant-clicked? cx cy plant) -> boolean
;;;    cx : number
;;;    cy : number
;;;    plant : string
;;; Checks if a plant button was clicked based on the coordinates.
(define plant-clicked?
  (lambda (cx cy plant)
    (cond
      [(string=? plant "Sunflower")
       (and (> cx 0) (< cx 100) (> cy 150) (< cy 200))]
      [(string=? plant "Fern")
       (and (> cx 0) (< cx 100) (> cy 210) (< cy 260))]
      [(string=? plant "Sakura")
       (and (> cx 0) (< cx 100) (> cy 270) (< cy 320))]
      [(string=? plant "Pumpkin")
       (and (> cx 0) (< cx 100) (> cy 330) (< cy 380))]
      [(string=? plant "Bamboo")
       (and (> cx 0) (< cx 100) (> cy 390) (< cy 440))]
      [else #f])))

;;; (pot-clicked? cx cy pot) -> boolean
;;;    cx : number
;;;    cy : number
;;;    pot : string
;;; Checks if a pot button was clicked based on the coordinates.
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

;;; (reactive-canvas width height initial-state view update on-mouse-click) -> void
;;;    width : number
;;;    height : number
;;;    initial-state : state
;;;    view : (state canvas -> void)
;;;    update : (event state -> state)
;;;    on-mouse-click : event
;;; Creates a reactive canvas with the given dimensions, initial state,
;;; view function, update function, and mouse click event subscription.
(display
 (reactive-canvas
   width height
   initial-state
   view
   update
   (on-mouse-click)))
