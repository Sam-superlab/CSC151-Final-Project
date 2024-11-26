;;section3.scm file interactivity helper functions
;;This file contains helper functions for the interactivity section of the project


(import image)
(import lab)
(import canvas)
(import html)

(define interaction-canvas (make-canvas 400 400))

;; Function to handle color change
(define handle-color-change
  (lambda ()
    (begin
      (canvas-rectangle! interaction-canvas 0 0 400 400 "solid" "lightblue"))))

;; Function to handle size change
(define handle-size-change
  (lambda ()
    (begin
      (canvas-rectangle! interaction-canvas 0 0 400 400 "solid" "lightgray")
      (canvas-circle! interaction-canvas 200 200 50 "solid" "green"))))

;; Function to handle string input
(define handle-string-input
  (lambda (input)
    (begin
      (canvas-rectangle! interaction-canvas 0 0 400 400 "solid" "white")
      (canvas-text! interaction-canvas 50 200 input 30 "solid" "black" "20px Arial"))))

;; State to track if options are displayed
(define options-visible? (ref #f))

;; Create buttons and input dynamically
(define toggle-options
  (lambda ()
    (if (deref options-visible?)
        (begin
          (tag-set-children! options-container (list)) ; Hide options
          (ref-set! options-visible? #f))
        (begin
          (tag-set-children!
           options-container
           (list
            (button "Color"
                    (lambda () (handle-color-change)))
            (button "Size"
                    (lambda () (handle-size-change)))
            (tag "div"
                 (list
                  (text-area "string-input")
                  (button "Submit"
                          (lambda ()
                            (let ([input (text-area-get (text-area "string-input"))])
                              (handle-string-input input))))))))
          (ref-set! options-visible? #t)))))

(define options-container (tag "div"))

(define main-button
  (button "Options" (lambda () (toggle-options))))

(display (tag "div"
              (list
               interaction-canvas
               main-button
               options-container)))

(canvas-rectangle! interaction-canvas 0 0 400 400 "solid" "white"
