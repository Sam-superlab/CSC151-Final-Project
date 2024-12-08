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
; (generate-background 200)





