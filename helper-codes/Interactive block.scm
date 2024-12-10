(import canvas)
(import reactive)
(import lab)
(import image)
(import html)

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



;; View function to render the canvas based on the state
(define view
  (lambda (st canv)
    (match st
      [(state options-visible? plants-options-visible? selected-plant sunflower-visible? pot-options-visible? selected-pot pot1-plant pot2-plant pot3-plant water-message-visible?)
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
           [else #f])

         ;; Render water message if visible
         (if water-message-visible?
           (begin
             (canvas-ellipse! canv 800 200 150 50 0 0 (* 2 pi) "solid" "lightblue")
             (canvas-text! canv 735 200 "Plants are happy :D" 20 "solid" "black"))void)
       )])))

;; Update function to handle events
(define update
  (lambda (msg st)
    (match msg
      [(event-mouse-click _ cx cy)
       (cond
         ;; Main "Options" button
         [(and (> cx 100) (< cx 200) (> cy 50) (< cy 100))
          (state (not (state-options-visible? st)) #f (state-selected-plant st) (state-sunflower-visible? st) #f (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]

         ;; "Plants" button
         [(and (state-options-visible? st) (> cx 100) (< cx 200) (> cy 150) (< cy 200))
          (state #t #t (state-selected-plant st) (state-sunflower-visible? st) #f (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]

         ;; Plant selection
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Sunflower")) (state #t #f "Sunflower" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Daisy")) (state #t #f "Daisy" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Sakura")) (state #t #f "Sakura" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Pumpkin")) (state #t #f "Pumpkin" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]
         [(and (state-plants-options-visible? st) (plant-clicked? cx cy "Bamboo")) (state #t #f "Bamboo" #f #t (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]

         ;; Pot selection
         [(and (state-pot-options-visible? st) (pot-clicked? cx cy "Pot1")) (state (state-options-visible? st) (state-plants-options-visible? st) (state-selected-plant st) (state-sunflower-visible? st) #f "Pot1" (state-selected-plant st) (state-pot2-plant st) (state-pot3-plant st) #f)]
         [(and (state-pot-options-visible? st) (pot-clicked? cx cy "Pot2")) (state (state-options-visible? st) (state-plants-options-visible? st) (state-selected-plant st) (state-sunflower-visible? st) #f "Pot2" (state-pot1-plant st) (state-selected-plant st) (state-pot3-plant st) #f)]
         [(and (state-pot-options-visible? st) (pot-clicked? cx cy "Pot3")) (state (state-options-visible? st) (state-plants-options-visible? st) (state-selected-plant st) (state-sunflower-visible? st) #f "Pot3" (state-pot1-plant st) (state-pot2-plant st) (state-selected-plant st) #f)]

         ;; "Water" button
         [(and (state-options-visible? st) (> cx 100) (< cx 200) (> cy 220) (< cy 270))
          (state (state-options-visible? st) (state-plants-options-visible? st) (state-selected-plant st) (state-sunflower-visible? st) (state-pot-options-visible? st) (state-selected-pot st) (state-pot1-plant st) (state-pot2-plant st) (state-pot3-plant st) #t)]

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
