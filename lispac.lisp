;;; lispac.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lispbuilder-sdl)
  (require :lispbuilder-sdl-gfx))

(defpackage :pacman
  (:use :cl :lispbuilder-sdl))

(in-package :pacman)

;;; Board dimmensions
(defvar *width*  600)
(defvar *height* 400)

;;; Time handling
(defvar *fps* 60)
(defvar *ticks* 0)

;;; Background color
(defvar *background* *black*)

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
    :reader pacman-radius)))

;;; The yellow ball :-)
(defvar *pacman*)

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
     (setf (pacman-direction *pacman*) :right))))


(defun update ()
  (setf *ticks* (mod (1+ *ticks*) *fps*))
  (clear-display *background* :surface *default-surface*)
  (draw-rectangle-* 0 0 *width* *height* :color *blue*)
  (draw *pacman*)
  (with-slots (x y direction radius)
      *pacman*
    (let ((r (+ radius 5)))
      (case direction
        (:up
         (unless (< y r)
           (decf y 2)))
        (:down
         (unless (<  (- *height* r) y)
           (incf y 2)))
        (:left
         (unless (< x r)
           (decf x 2)))
        (:right
         (unless (< (- *width* r) x)
           (incf x 2))))))
  (update-display))

;;; Run pacman
(defun run-and-wait ()
  (with-init (sdl-init-video)
    (let ((screen (window *width* (+ *height* 100) :title-caption "Lispac")))
      (setf (frame-rate) *fps*)
      (clear-display *black*)
      (initialise-default-font *font-10x20*)
      (draw-string-solid-* "Lispac" 10 10 )
      (let ((*pacman* (make-instance 'pacman)))
        (with-surface (*default-surface* (create-surface *width* *height* :y 100))
          (with-events ()
            (:quit-event () t)
            (:key-down-event (:key key) (keypress key))
            (:idle ()
                   (update)
                   (blit-surface *default-surface* screen))))))))

(defun run ()
  #+sb-thread (sb-thread:make-thread #'run-and-wait)
  #-sb-thread (run-and-wait)
  (values))

;;; lispac.lisp ends here
