;;section 1: Helper functions for multimedia processing.
;;author: Xuyi(Sam) Ren
;;

(import image)
(import lab)
(import test)
(import canvas)

(part "Helper Functions")

;;; (bound-rgb-value v) -> number?
;;;   v: number?
;;; Returns v, but bound to the range [0, 255].
(define bound-rgb-value
  (lambda (v)
    (min (max v 0) 255)))

;;; (rgb-transformer transform-component) -> color-function?
;;;    component-transformer : function from integer to integer
;;; Returns a new function that takes a color as input and applies
;;; `component-trasformer` to each, yielding a new color.
(define rgb-transformer
  (lambda (transform-component)
    (let ([xform (o bound-rgb-value transform-component)])
         (lambda (color)
           (rgb (xform (rgb-red color))
                (xform (rgb-green color))
                (xform (rgb-blue color)))))))

;;; (rgb-darker-32 color) -> rgb?
;;;   color : rgb?
;;; Create a darker version of color by attempting to subtract 32
;;; from each component.
(define rgb-darker-32 (rgb-transformer (section - _ 32)))

;;; (rgb-lighter-16 color) -> rgb?
;;;   color : rgb?
;;; Create a lighter version of color by attempting to add 16
;;; to each component.
(define rgb-lighter-16 (rgb-transformer (section + _ 16)))

;;; (rgb-cyclically-add-64 color) -> rgb?
;;;   color : rgb?
;;; Add 64 to each component of color, cycling when we exceed 255.
(define cyclically-add-64
  (rgb-transformer (lambda (c) (remainder (+ c 64) 256))))

;;; (solid-right-triangle width height color [description]) -> image?
;;;   width : positive-real?
;;;   height : positive-real?
;;;   color : color?
;;; Make a right triangle of the given width, height, and color.
(define solid-right-triangle
  (lambda (width height color)
    (path width height
          (list (pair 0 0) (pair width height) (pair 0 height))
          "solid"
          color)))

;;; (thinly-outlined-square size color) -> image?
;;;   size : non-negative-integer?
;;;   color : color?
;;; Create square of the given color with a thin black outline.
(define thinly-outlined-square
  (lambda (size color)
    (overlay (outlined-square (- size 2) "black" 1)
             (solid-square size color))))


(part "fractal functions")

;;; (carpet pattern size color-x color-y n) -> image?
;;;   pattern ; string? (length 9, composed only of x, X, y, and Y)
;;;   size : positive real?
;;;   color-x : color?
;;;   color-y : color?
;;;   n : non-negative integer.
;;; Create a `size`-by-`size` image of a fractal carpet with `n` levels
;;; of recursion, using `color-x` as the "primary" color and `color-y`
;;; as the "secondary" color.
;;;
;;; The pattern is given by the letters in pattern, where `X` means
;;; "recurse" keeping colors as they are", `Y` means "recurse swapping
;;; the two colors", `x` means "square in `color-x`" and `y` means
;;; "square in `color-y`".
;;;
;;; The positions of the letters correspond to the parts of the pattern
;;;
;;;      0 1 2
;;;      3 4 5
;;;      6 7 8
(define carpet
  (lambda (pattern size color-x color-y n)
    (let ([third-size (/ size 3)])
      (cond
        [(= n 0)
         (solid-square size color-x)]
        [(= (string-length pattern) 9)
         (let* ([sub-images
                 (map (lambda (ch)
                        (cond
                          [(char=? ch #\X) (carpet pattern third-size color-x color-y (- n 1))]
                          [(char=? ch #\Y) (carpet pattern third-size color-y color-x (- n 1))]
                          [(char=? ch #\x) (solid-square third-size color-x)]
                          [(char=? ch #\y) (solid-square third-size color-y)]
                          [else "wrong pattern character" ]))
                      (string->list pattern))]
                [row1 (beside (list-ref sub-images 0) (list-ref sub-images 1) (list-ref sub-images 2))]
                [row2 (beside (list-ref sub-images 3) (list-ref sub-images 4) (list-ref sub-images 5))]
                [row3 (beside (list-ref sub-images 6) (list-ref sub-images 7) (list-ref sub-images 8))])
           (above row1 row2 row3))]
        [else "Pattern must be exactly 9 characters long"]))))

;; Functions create triangle fractals
;; 



