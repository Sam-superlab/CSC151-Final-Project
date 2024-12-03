;; section4.scm 
;; Basic bulid block for interactive canvas where it change values when click on the button
(import canvas)
(import reactive)

;; Define the state structure
(struct state (
  options-visible?  ; boolean, whether the options are displayed
  color             ; string, color of the shape
  size              ; number, size of the shape
  pattern           ; string, pattern of the shape
))

;; Canvas dimensions
(define width 400)
(define height 400)

;; Initial state
(define initial-state (state #f "black" 20 "solid"))
