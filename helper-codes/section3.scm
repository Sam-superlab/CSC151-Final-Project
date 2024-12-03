(import canvas)
(import reactive)

;; Define the state structure
(struct state (
  options-visible?  ; boolean, whether the options are displayed
))

;; Canvas dimensions
(define width 400)
(define height 400)

;; Initial state
(define initial-state (state #f))

;; View function to render the canvas based on the state
(define view
  (lambda (st canv)
    (match st
      [(state options-visible?)
       (begin
         ;; Clear the canvas
         (canvas-rectangle! canv 0 0 width height "solid" "white")
         ;; Main "Options" button
         (canvas-rectangle! canv 150 50 100 50 "solid" "lightblue")
         (canvas-text! canv 165 75 "options" 20 "solid" "black")
         ;; Display additional buttons if options are visible
         (if options-visible?
             (begin
               (canvas-rectangle! canv 150 150 100 50 "solid" "lightgreen")
               (canvas-text! canv 180 175 "Color" 20 "solid" "black")
               (canvas-rectangle! canv 150 220 100 50 "solid" "lightcoral")
               (canvas-text! canv 180 245 "Size" 20 "solid" "black")
               (canvas-rectangle! canv 150 290 100 50 "solid" "lightyellow")
               (canvas-text! canv 175 315 "Pattern" 20 "solid" "black")) #f))])))

;; Update function to handle events
(define update
  (lambda (msg st)
    (match msg
      [(event-mouse-click btn cx cy)
       ;; Toggle options visibility if main button is clicked
       (if (and (> cx 150) (< cx 250) (> cy 50) (< cy 100))
           (state (not (state-options-visible? st)))
           st)]
      [else st])))

;; Reactive canvas with subscriptions
(display
 (reactive-canvas
   width height
   initial-state
   view
   update
   (on-mouse-click)))