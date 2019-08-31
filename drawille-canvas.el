(require 'cl-lib)
(require 'eieio)

;; Canvas

(defconst drawille-pixel-table
  [[#x01 #x08]
   [#x02 #x10]
   [#x04 #x20]
   [#x40 #x80]])

(defclass drawille-canvas ()
  ((width :type integer
          :initarg :width
          :initform (* 10 2)
          :reader drawille-width)
   (height :type integer
           :initarg :height
           :initform (* 10 4)
           :reader drawille-height)
   (content :accessor drawille-content)))

(defmethod initialize-instance :after ((canvas drawille-canvas) &key)
  (with-slots (width height content)
      canvas
    (setf width (* (ceiling width 2) 2)
          height (* (ceiling height 4) 4)
          content (make-vector (/ (* width height) 8) 0))))

(defgeneric drawille-set (canvas x y))
(defgeneric drawille-unset (canvas x y))
(defgeneric drawille-toggle (canvas x y))
(defgeneric drawille-clear (canvas))
(defgeneric drawille-frame (canvas))

(defun drawille--get-position (x y)
  (let* ((x (round x))
         (y (round y))
         (dot-x (mod x 2))
         (dot-y (mod y 4))
         (row (floor x 2))
         (col (floor y 4)))
    (cl-values (cons col row)
               (cons dot-x dot-y))))

(defun drawille--canvas-apply (canvas x y f)
  (with-slots (width height content) canvas
    (when (and (<= 0 x (1- width))
               (<= 0 y (1- height)))
      (destructuring-bind ((row . col)
                           (dot-x . dot-y))
          (drawille--get-position x y)
        (let* ((coord (+ col (* (/ width 2) row)))
               (char (aref content coord))
               (mask (or (elt (elt drawille-pixel-table dot-y) dot-x)
                         0)))
          (aset content coord (funcall f char mask)))))))

(defmethod drawille-set ((canvas drawille-canvas) x y)
  (drawille--canvas-apply canvas x y #'logior))

(defmethod drawille-unset ((canvas drawille-canvas) x y)
  (drawille--canvas-apply canvas x y (lambda (char mask)
                                      (logand char (lognot mask)))))

(defmethod drawille-toggle ((canvas drawille-canvas) x y)
  (drawille--canvas-apply canvas x y #'logxor))

(defmethod drawille-clear ((canvas drawille-canvas))
  (with-slots (content) canvas
    (fillarray content 0)))

(defmethod drawille-frame ((canvas drawille-canvas))
  (with-slots (width content) canvas
    (cl-loop for c across content
             for j from 0
             when (= j (/ width 2)) concat "\n" into s and do (setf j 0)
             concat (char-to-string (+ #x2800 c)) into s
             finally return (concat s "\n"))))

;; Utilities

(defun drawille-canvas--interpolate (d0 d1 r0 r1 x)
  (let* ((fx (float x))
         (p (/ fx (- d1 d0)))
         (l (+ (* (- 1 p) r0)
               (* p r1))))
    l))

(defun drawille--bresenham (x0 y0 x1 y1 &optional fn)
  (let* (arr
         (dx (- x1 x0))
         (dy (- y1 y0))
         (adx (abs dx))
         (ady (abs dy))
         (eps 0)
         (sx (signum dx))
         (sy (signum dy)))
    (when (not fn)
      (let ((arr (list)))
        (setf fn (lambda (x y)
                   (push (cons x y) arr)))))
    (if (> adx ady)
        (let ((xs (if (< sx 0)
                      (cl-loop for x from x0 downto x1 collect x)
                    (cl-loop for x from x0 to x1 collect x))))
          (cl-loop with y = y0
                   for x in xs
                   do (progn
                        (funcall fn x y)
                        (incf eps ady)
                        (when (>= (lsh eps 1) adx)
                          (incf y sy)
                          (decf eps adx)))))
      (let ((ys (if (< sy 0)
                    (cl-loop for y from y0 downto y1 collect y)
                  (cl-loop for y from y0 to y1 collect y))))
        (cl-loop with x = x0
                 for y in ys
                 do (progn
                      (funcall fn x y)
                      (incf eps adx)
                      (when (>= (lsh eps 1) ady)
                        (incf x sx)
                        (decf eps ady))))))
    (nreverse arr)))

(defun drawille-canvas-draw-line (canvas x0 y0 x1 y1)
  (drawille--bresenham x0 y0 x1 y1 (lambda (x y) (drawille-set canvas x y))))

;; Context
