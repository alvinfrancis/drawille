(require 'json)

(defvar drawille-start-lat)
(defvar drawille-start-lng)
(defvar drawille-end-lat)
(defvar drawille-end-lng)

(defun drawille--lng-to-canvas-x (canvas lng)
  (let ((min-lng 0)
        (max-lng 360)
        (adjusted-lng (+ 180 lng))
        (lng-to-canvas-x 0))
    (cond ((= adjusted-lng min-lng)
           (setf lng-to-canvas-x 0))
          ((= adjusted-lng max-lng)
           (setf lng-to-canvas-x (+ 0 (drawille-width canvas))))
          (t
           (setf lng-to-canvas-x (+ 0
                                    (* (- adjusted-lng min-lng)
                                       (/ (float (drawille-width canvas))
                                          (- max-lng min-lng)))))))
    (floor lng-to-canvas-x)))

(defun drawille--lat-to-canvas-y (canvas lat)
  (let ((min-lat 0)
        (max-lat 180)
        (adjusted-lat (+ 90 lat))
        (lat-to-canvas-y 0))
    (cond ((= adjusted-lat min-lat)
           (setf lat-to-canvas-y (drawille-height canvas)))
          ((= adjusted-lat max-lat)
           (setf lat-to-canvas-y 0))
          (t
           (setf lat-to-canvas-y (+
                                  (- (drawille-height canvas)
                                     (* (- adjusted-lat min-lat)
                                        (/ (float (drawille-height canvas))
                                           (- max-lat min-lat))))
                                  0))))
    (floor lat-to-canvas-y)))

(defun drawille--world-map (width height)
  (let* ((canvas (make-instance 'drawille-canvas
                                :width width
                                :height height))
         (shapes (json-read-file "worldData.json")))
    (cl-loop for shape across shapes
             do (cl-loop with last-x = nil
                         with last-y = nil
                         for coord across shape
                         do (progn
                              (let ((y (drawille--lat-to-canvas-y canvas (string-to-number (cdr (assoc 'lat coord)))))
                                        ;(y (floor (drawille-canvas--interpolate -180 180 0 width (string-to-number (cdr (assoc 'lat coord))))))
                                    (x (drawille--lng-to-canvas-x canvas (string-to-number (cdr (assoc 'lon coord)))))
                                        ;(x (floor (drawille-canvas--interpolate -180 180 0 width (string-to-number (cdr (assoc 'lat coord))))))
                                    )
                                (when (and last-x last-y)
                                  (drawille-canvas-draw-line canvas last-x last-y x y))
                                (setf last-x x
                                      last-y y)))))
    canvas))

(drawille-frame
 (drawille--world-map 64 48))
