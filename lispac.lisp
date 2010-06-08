;;; lispac.lisp

;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>
;; Copyrigth (C) 2010 Mario Castelan Castro <marioxcc>

;; Special thanks to a _not_ anonymous for us, but for everybody else,
;; who wrote and donated the base of lispac.

;; This file is part of lispac.

;; lispac is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lispac is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lispac.  If not, see <http://www.gnu.org/licenses/>.

(in-package :lispac)

;;; Board
(defvar *width*  600)
(defvar *height* 400)
(defvar *board*)
(defvar *board-width*)
(defvar *board-height*)
(defvar *board-surface*)
(defvar *tile-size* 12)

;;; Time handling
(defvar *fps* 60)
(defvar *ticks* 0)
(defvar *speed* 2)
(defvar *score* 0)
(defvar *orange* (color :r 255 :g 127 :b 0))
;;; Background color
(defvar *background* *black*)
(defvar *point-radius* 2)

(defclass pacman ()
  (;; TODO: Implement pacman upon a surface, in order to we can use GFX
   ;; to rotation and more.
   ;; (surface ...)
   (direction
    :initarg :direction
    :type (member :up :down :left :right)
    :initform :right
    :accessor pacman-direction)
   (x
    :initarg :x
    :type fixnum
    :initform (round *width* 2)
    :accessor pacman-x)
   (y
    :initarg :y
    :type fixnum
    :initform (round *height* 2)
    :accessor pacman-y)
   (radius
    :initarg :radius
    :type fixnum
    :initform 12
    :accessor pacman-radius)))

(defclass point ()
  (
   (count :type integer
          :accessor point-count
          :initform 0
          :initarg :count)
   (x :type fixnum
      :accessor point-x
      :initform 0
      :initarg :x)
   (y :type fixnum
      :accessor point-y
      :initform 0
      :initarg :y)))

;;; The yellow ball :-)

(defvar *pacman*)
(defvar *points* ())

(defun* add-point ((integer count x y))
  (push (make-instance 'point :count count :x x :y y) *points*))

(defun* pacman-add-point ((pacman pac) (integer count))
  (with-slots ((r radius) x y direction)
      pac
    (ecase direction
      (:up (add-point count x (+ y r)))
      (:down (add-point count x (- y r)))
      (:left (add-point count (+ x r) y))
      (:right (add-point count (- x r) y)))))

(defun* pacman-eat-point-p ((point point))
  (< (distance-* (pacman-x *pacman*) (pacman-y *pacman*)
                 (point-x point) (point-y point))
     (+ (pacman-radius *pacman*) *point-radius*)))

(defgeneric draw (pac)
  (:method ((pac pacman))
    (with-slots ((r radius) x y direction)
        pac
      (let ((a (round (* 60 (abs (cos (* (/ *ticks* *fps*) 2 pi)))))))
        (draw-filled-circle-* x y r :color *yellow* :stroke-color *background*)
        (ecase direction
          (:up
           (sdl-gfx:draw-filled-pie-* x y r (- 270 (round a 2)) (+ 270 (round a 2)) :color *background*)
           (draw-filled-circle-* (- x (round r 2)) y (round r 5) :color *black*))
          (:down
           (sdl-gfx:draw-filled-pie-* x y r (- 90 (round a 2)) (+ 90 (round a 2)) :color *background*)
           (draw-filled-circle-* (- x (round r 2)) y (round r 5) :color *black*))
          (:left
           (sdl-gfx:draw-filled-pie-* x y r (- 180 (round a 2)) (- (round a 2) 180) :color *background*)
           (draw-filled-circle-* x (- y (round r 2)) (round r 5) :color *black*))
          (:right
           (sdl-gfx:draw-filled-pie-* x y r (- 360 (round a 2)) (round a 2) :color *background*)
           (draw-filled-circle-* x (- y (round r 2)) (round r 5) :color *black*)))))))

(defun generate-dumb-board ()
  (dotimes (x *board-width*)
    (dotimes (y *board-height*)
      (setf (aref *board* x y) (and (divisiblep x 4)
                                    (divisiblep y 4))))))

(defun keypress (key)
  (case key
    (:sdl-key-escape
     (push-quit-event))
    (:sdl-key-up
     (setf (pacman-direction *pacman*) :up))
    (:sdl-key-down
     (setf (pacman-direction *pacman*) :down))
    (:sdl-key-left
     (setf (pacman-direction *pacman*) :left))
    (:sdl-key-right
     (setf (pacman-direction *pacman*) :right))
    (:sdl-key-f1
     (setf *background* *red*))
    (:sdl-key-f2
     (setf *background* *black*))
    (:sdl-key-f3
     (setf *background* *white*))
    (:sdl-key-f4
     (setf *background* *magenta*))
    (:sdl-key-f5
     (setf *background* *blue*))
    (:sdl-key-f6
     (setf *background* *yellow*))
    (:sdl-key-a
     (incf *speed*))
    (:sdl-key-s
     (decf *speed*))
    (:sdl-key-d
     (incf (pacman-radius *pacman*)))
    (:sdl-key-f
     (decf (pacman-radius *pacman*)))
    (:sdl-key-x
     (pacman-add-point *pacman* 5))
    (:sdl-key-q
     (incf *point-radius*))
    (:sdl-key-w
     (decf *point-radius*))
    (:sdl-key-t
     (setf (pacman-x *pacman*) (/ *width* 2))
     (setf (pacman-y *pacman*) (/ (- *height* 100) 2)))))

(defun update-board ()
  (dotimes (y *board-height*)
    (dotimes (x *board-width*)
      (draw-box-* (* *tile-size* x) (* *tile-size* y)
                  *tile-size* *tile-size*
                  :surface *board-surface*
                  :color (if (aref *board* x y)
                             *red*
                             *black*)))))

(defun update-points ()
  (loop with new-points = nil
        for point in *points*
        do (cond
             ((pacman-eat-point-p point)
              (incf *score*))
             (t 
              (draw-filled-circle-* (point-x point) (point-y point) 
                                    *point-radius* :color *orange*)
              (push point new-points)))
        finally (setf *points* new-points)))
             
(defun update-state ()
  (with-surface (panel-surface (create-surface *width* 100))
    (fill-surface *black*)
    (draw-string-solid-* "Lispac" 10 10)
    (draw-string-solid-* (format nil "FPS ~d Speed ~d" (frame-rate) *speed*) 
                         10 35)
    (draw-string-solid-* (format nil "Radius ~d" (pacman-radius *pacman*)) 
                         10 60)
    (draw-string-solid-* (format nil ":: Score ~d ::" *score*) 
                         (/ *width* 2) 60 :justify :right)
    (blit-surface panel-surface *default-display*)))

(defun update ()
  (setf *ticks* (mod (1+ *ticks*) *fps*))
  (clear-display *background* :surface *default-surface*)
  (blit-surface *board-surface*)
  (draw *pacman*)
  (with-slots (x y direction radius)
      *pacman*
    (let ((r (+ radius 5)))
      (case direction
        (:up
         (unless (< y r)
           (decf y *speed*)))
        (:down
         (unless (<  (- *height* r) y)
           (incf y *speed*)))
        (:left
         (unless (< x r)
           (decf x *speed*)))
        (:right
         (unless (< (- *width* r) x)
           (incf x *speed*))))))
  (update-state)
  (update-points)
  (draw-rectangle-* 0 100 *width* *height* :color *red* 
                    :surface *default-display*)
  (update-display))

;;; Run pacman
(defun run-and-wait ()
  (with-init (sdl-init-video)
    (let ((screen (window *width* (+ *height* 100) :title-caption "Lispac")))
      (setf (frame-rate) *fps*)
      (clear-display *black*)
      (initialise-default-font *font-10x20*)
      (setf *pacman* (make-instance 'pacman))
      (setf *board-width* (floor (/ *width* *tile-size*)))
      (setf *board-height* (floor (/ *height* *tile-size*)))
      (setf *board* (make-array (list *board-width* *board-height*)
                                :element-type '(member t nil)))
      (generate-dumb-board)
      (with-surface (*default-surface* (create-surface *width* *height* :y 100))
        (setf *board-surface* (create-surface *width* *height*))
        (update-board)
        (with-events ()
          (:quit-event () t)
          (:key-down-event (:key key) (keypress key))
          (:idle ()
                 (update)
                 (blit-surface *default-surface* screen)))))))

(defun run ()
  #+sb-thread (sb-thread:make-thread #'run-and-wait)
  #-sb-thread (run-and-wait)
  (values))

;;; lispac.lisp ends here
