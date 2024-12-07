(import canvas)
(import reactive)

;; Adjusted State Structure
(struct state (
  options-visible?         ; boolean, whether the main options are displayed
  plants-options-visible?  ; boolean, whether the Plants options are displayed
  selected-plant           ; string, the selected plant
))

;; Initial state
(define initial-state (state #f #f ""))

;; Canvas dimensions
(define width 400)
(define height 400)

;; View function to render the canvas based on the state
(define view
  (lambda (st canv)
    (match st
      [(state options-visible? plants-options-visible? selected-plant)
       (begin
         ;; Clear the canvas
         (canvas-rectangle! canv 0 0 width height "solid" "white")

         ;; Main "Options" button
         (canvas-rectangle! canv 150 50 100 50 "solid" "lightblue")
         (canvas-text! canv 165 75 "Options" 20 "solid" "black")

         ;; Render based on visibility
         (cond
           ;; Case: Plants options are visible
           [plants-options-visible?
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
              (canvas-text! canv 275 245 "Cactus" 20 "solid" "white"))]

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
           [(string=? selected-plant "Rose") (canvas-rectangle! canv 180 300 50 50 "solid" "red")]
           [(string=? selected-plant "Sunflower") (canvas-rectangle! canv 180 300 50 50 "solid" "yellow")]
           [(string=? selected-plant "Daisy") (canvas-rectangle! canv 180 300 50 50 "solid" "white")]
           [(string=? selected-plant "Tulip") (canvas-rectangle! canv 180 300 50 50 "solid" "pink")]
           [(string=? selected-plant "Lily") (canvas-rectangle! canv 180 300 50 50 "solid" "purple")]
           [(string=? selected-plant "Cactus") (canvas-rectangle! canv 180 300 50 50 "solid" "green")]
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
         [(plant-clicked? cx cy "Rose") (state #t #f "Rose")]
         [(plant-clicked? cx cy "Sunflower") (state #t #f "Sunflower")]
         [(plant-clicked? cx cy "Daisy") (state #t #f "Daisy")]
         [(plant-clicked? cx cy "Tulip") (state #t #f "Tulip")]
         [(plant-clicked? cx cy "Lily") (state #t #f "Lily")]
         [(plant-clicked? cx cy "Cactus") (state #t #f "Cactus")]

         ;; Default: Return current state
         [else st])]
      ;; Default: Return current state
      [else st])))

;; Helper function to check if a plant button was clicked
(define plant-clicked?
  (lambda (cx cy plant)
    (cond
      [(string=? plant "Rose")
       (and (> cx 50) (< cx 150) (> cy 150) (< cy 200))]
      [(string=? plant "Sunflower")
       (and (> cx 150) (< cx 250) (> cy 150) (< cy 200))]
      [(string=? plant "Daisy")
       (and (> cx 250) (< cx 350) (> cy 150) (< cy 200))]
      [(string=? plant "Tulip")
       (and (> cx 50) (< cx 150) (> cy 220) (< cy 270))]
      [(string=? plant "Lily")
       (and (> cx 150) (< cx 250) (> cy 220) (< cy 270))]
      [(string=? plant "Cactus")
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
