(import canvas)
(import reactive)

;; Define the state structure
(struct state (
  options-visible?         ; boolean, whether the main options are displayed
  color-options-visible?   ; boolean, whether the color options are displayed
  size-options-visible?    ; boolean, whether the size options are displayed
  pattern-options-visible? ; boolean, whether the pattern options are displayed
))

;; Canvas dimensions
(define width 400)
(define height 400)
(define current-size (ref 100))  ; Default size as a percentage

;; Initial state
(define initial-state (state #f #f #f #f))

;; View function to render the canvas based on the state
(define view
  (lambda (st canv)
    (match st
      [(state options-visible? color-options-visible? size-options-visible? pattern-options-visible?)
       (begin
         ;; Clear the canvas
         (canvas-rectangle! canv 0 0 width height "solid" "white")
         ;; Main "Options" button
         (canvas-rectangle! canv 150 50 100 50 "solid" "lightblue")
         (canvas-text! canv 165 75 "Options" 20 "solid" "black")
         
         ;; Render based on visibility
         (cond
           ;; Case: Color options are visible
           [color-options-visible?
            (begin
              (canvas-rectangle! canv 50 150 100 50 "solid" "red")
              (canvas-text! canv 85 175 "Red" 20 "solid" "white")
              (canvas-rectangle! canv 150 150 100 50 "solid" "green")
              (canvas-text! canv 180 175 "Green" 20 "solid" "white")
              (canvas-rectangle! canv 250 150 100 50 "solid" "blue")
              (canvas-text! canv 280 175 "Blue" 20 "solid" "white"))]

           ;; Case: Size options are visible
           [size-options-visible?
            (begin
              (canvas-rectangle! canv 50 150 100 50 "solid" "lightgray")
              (canvas-text! canv 70 175 "50%" 20 "solid" "black")
              (canvas-rectangle! canv 150 150 100 50 "solid" "lightgray")
              (canvas-text! canv 165 175 "100%" 20 "solid" "black")
              (canvas-rectangle! canv 250 150 100 50 "solid" "lightgray")
              (canvas-text! canv 265 175 "150%" 20 "solid" "black"))]

           ;; Case: Pattern options are visible
           [pattern-options-visible?
            (begin
              (canvas-rectangle! canv 50 150 100 50 "solid" "lightyellow")
              (canvas-text! canv 85 175 "n=3" 20 "solid" "black")
              (canvas-rectangle! canv 150 150 100 50 "solid" "lightyellow")
              (canvas-text! canv 180 175 "n=4" 20 "solid" "black")
              (canvas-rectangle! canv 250 150 100 50 "solid" "lightyellow")
              (canvas-text! canv 280 175 "n=5" 20 "solid" "black"))]

           ;; Case: Main options are visible
           [options-visible?
            (begin
              (canvas-rectangle! canv 150 150 100 50 "solid" "lightgreen")
              (canvas-text! canv 180 175 "Color" 20 "solid" "black")
              (canvas-rectangle! canv 150 220 100 50 "solid" "lightcoral")
              (canvas-text! canv 180 245 "Size" 20 "solid" "black")
              (canvas-rectangle! canv 150 290 100 50 "solid" "lightyellow")
              (canvas-text! canv 175 315 "Pattern" 20 "solid" "black"))]

           ;; Default case: Do nothing
           [else #f]))])))

;; Update function to handle events
(define update
  (lambda (msg st)
    (match msg
      [(event-mouse-click _ cx cy)
       (cond
         ;; Main "Options" button
         [(and (> cx 150) (< cx 250) (> cy 50) (< cy 100))
          (state (not (state-options-visible? st)) #f #f #f)]
         
         ;; "Color" button
         [(and (> cx 150) (< cx 250) (> cy 150) (< cy 200) (state-options-visible? st))
          (state #t #t #f #f)]

         ;; "Size" button
         [(and (> cx 150) (< cx 250) (> cy 220) (< cy 270) (state-options-visible? st))
          (state #t #f #t #f)]

         ;; "Pattern" button
         [(and (> cx 150) (< cx 250) (> cy 290) (< cy 340) (state-options-visible? st))
          (state #t #f #f #t)]

         ;; "Red" option
         [(and (> cx 50) (< cx 150) (> cy 150) (< cy 200) (state-color-options-visible? st))
          (begin
            (canvas-rectangle! canv 0 0 width height "solid" "red")
            (state #t #f #f #f))]
         
         ;; "Green" option
         [(and (> cx 150) (< cx 250) (> cy 150) (< cy 200) (state-color-options-visible? st))
          (begin
            (canvas-rectangle! canv 0 0 width height "solid" "green")
            (state #t #f #f #f))]

         ;; "Blue" option
         [(and (> cx 250) (< cx 350) (> cy 150) (< cy 200) (state-color-options-visible? st))
          (begin
            (canvas-rectangle! canv 0 0 width height "solid" "blue")
            (state #t #f #f #f))]

         ;; "50%" option
         [(and (> cx 50) (< cx 150) (> cy 150) (< cy 200) (state-size-options-visible? st))
          (begin
            (ref-set! current-size 50)
            (state #t #f #f #f))]

         ;; "100%" option
         [(and (> cx 150) (< cx 250) (> cy 150) (< cy 200) (state-size-options-visible? st))
          (begin
            (ref-set! current-size 100)
            (state #t #f #f #f))]

         ;; "150%" option
         [(and (> cx 250) (< cx 350) (> cy 150) (< cy 200) (state-size-options-visible? st))
          (begin
            (ref-set! current-size 150)
            (state #t #f #f #f))]

         ;; Default: Return current state
         [else st])]
      ;; Default: Return current state
      [else st])))

;; Reactive canvas with subscriptions
(display
 (reactive-canvas
   width height
   initial-state
   view
   update
   (on-mouse-click)))