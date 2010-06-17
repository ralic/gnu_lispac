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
(defvar *score* 0)
(defvar *orange* (color :r 255 :g 127 :b 0))
;;; Background color
(defvar *background* *black*)
(defvar *target-radius* 2)

;; If non-nil, print the tiles the units uses.
(defvar *print-units-rectangles-p* nil)

(defgeneric draw (unit))

(defclass pacman ()
  (;; TODO: Implement pacman upon a surface, in order to we can use GFX
   ;; to rotation and more.
   ;; (surface ...)
   (direction
    :initarg :direction
    :type (member :up :down :left :right)
    :initform :right
    :accessor pacman-direction)
   (next-direction
    :type (member :up :down :left :right)
    :initform :right
    :accessor pacman-next-direction)
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
    :accessor pacman-radius)
   (speed
    :initarg :speed
    :type fixnum
    :initform 2
    :accessor pacman-speed)))

(defclass target ()
  ((count :type integer
          :accessor target-count
          :initform 0
          :initarg :count)
   (x :type fixnum
      :accessor target-x
      :initform 0
      :initarg :x)
   (y :type fixnum
      :accessor target-y
      :initform 0
      :initarg :y)))

(defclass game-clock ()
  ((ticks :type fixnum
          :accessor game-clock-ticks
          :initform 0
          :initarg :t)
   (seconds :type fixnum
            :accessor game-clock-seconds
            :initform 0
            :initarg :s)
   (minutes :type fixnum
            :accessor game-clock-minutes
            :initform 0
            :initarg :m)
   (hours :type fixnum
          :accessor game-clock-hours
          :initform 0
          :initarg :h)
   (stop :accessor game-clock-stop
         :initform nil)))

;;; The yellow ball :-)

(defvar *pacman*)
(defvar *targets* ())
(defvar *clock* (make-instance 'game-clock))
(defvar *user-clocks* (make-hash-table))

(defun* clock-toggle ((game-clock clock))
  (setf (game-clock-stop clock) (not (game-clock-stop clock))))

(defun* clock-tick ((game-clock clock))
  (unless (game-clock-stop clock)
    (incf (game-clock-ticks clock))
    (if (= (game-clock-ticks clock) *fps*)
        (progn (incf (game-clock-seconds clock))
               (if (= (game-clock-seconds clock) 60)
                   (progn (incf (game-clock-minutes clock))
                          (if (= (game-clock-minutes clock) 60)
                              (progn (incf (game-clock-hours clock))
                                     (zerof (game-clock-minutes clock))))
                          (zerof (game-clock-seconds clock))))
               (zerof (game-clock-ticks clock))))))

(defun* add-clock ((string name))
  (setf (gethash name *user-clocks*) (make-instance 'game-clock)))

(defun* get-clock ((string name))
  (gethash name *user-clocks*))

(defun* delete-clock ((string name))
  (setf (gethash name *user-clocks*) nil))

(defun clocks-tick ()
  (loop for i being the hash-value of *user-clocks*
       do (clock-tick i)))

(defun* format-clock ((game-clock clock))
  (with-slots (ticks seconds minutes hours)
      clock
  (format nil "~d:~d:~d - ~d" hours minutes seconds ticks)))

(defun* add-target ((integer count x y))
  (push (make-instance 'target :count count :x x :y y) *targets*))

(defmacro with-timer ((name var func) &body body)
  (add-clock name)
  `(let ((,var ,(get-clock name)))
     ,func
     (clock-toggle ,var)
     (with-slots (ticks seconds minutes hours)
         ,var
       ,@body)))

(defun* pacman-add-target ((pacman pac) (integer count))
  (with-slots ((r radius) x y direction)
      pac
    (ecase direction
      (:up (add-target count x (+ y r)))
      (:down (add-target count x (- y r)))
      (:left (add-target count (+ x r) y))
      (:right (add-target count (- x r) y)))))

(defun* pacman-eat-target-p ((target target))
  (< (distance-* (pacman-x *pacman*) (pacman-y *pacman*)
                 (target-x target) (target-y target))
     (+ (pacman-radius *pacman*) *target-radius*)))

(defmethod draw ((target target))
  (with-slots (x y) target
    (draw-filled-circle-* x y *target-radius* :color *orange*)))

(defmethod draw ((pacman pacman))
  (with-slots ((r radius) x y direction)
      pacman
    (let ((a (round (* 60 (abs (cos (* (/ (game-clock-ticks *clock*)
					  *fps*) 2 pi)))))))
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
	 (draw-filled-circle-* x (- y (round r 2)) (round r 5) :color *black*))))))

(defun generate-dumb-board ()
  (dotimes (x *board-width*)
    (dotimes (y *board-height*)
      (setf (aref *board* x y) (and (divisiblep x 4)
                                    (divisiblep y 4))))))

(defun board-square-clear-p (left top right bottom)
  (block function
    (dorange (x left right)
      (dorange (y top bottom)
        (when (aref *board* x y)
          (return-from function nil))))
    t))

(defun board-row-clear-p (y &optional left right)
  (board-square-clear-p (or left 0) y
                        (or right (1- *board-width*)) y))

(defun board-column-clear-p (x &optional top bottom)
  (board-square-clear-p x (or top 0)
                        x (or bottom (1- *board-height*))))

(defun keypress (key)
  (case key
    (:sdl-key-escape
     (push-quit-event))
    (:sdl-key-up
     (setf (pacman-next-direction *pacman*) :up))
    (:sdl-key-down
     (setf (pacman-next-direction *pacman*) :down))
    (:sdl-key-left
     (setf (pacman-next-direction *pacman*) :left))
    (:sdl-key-right
     (setf (pacman-next-direction *pacman*) :right))
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
     (incf (pacman-speed *pacman*)))
    (:sdl-key-s
     (decf (pacman-speed *pacman*)))
    (:sdl-key-d
     (incf (pacman-radius *pacman*)))
    (:sdl-key-f
     (decf (pacman-radius *pacman*)))
    (:sdl-key-x
     (pacman-add-target *pacman* 5))
    (:sdl-key-q
     (incf *target-radius*))
    (:sdl-key-w
     (decf *target-radius*))
    (:sdl-key-t
     (setf (pacman-x *pacman*) (/ *width* 2))
     (setf (pacman-y *pacman*) (/ (- *height* 100) 2)))
    (:sdl-key-c
     (clock-toggle *clock*))))

(defun update-board ()
  (dotimes (y *board-height*)
    (dotimes (x *board-width*)
      (draw-box-* (* *tile-size* x) (* *tile-size* y)
                  *tile-size* *tile-size*
                  :surface *board-surface*
                  :color (if (aref *board* x y)
                             *red*
                             *black*)))))

(defun update-targets ()
  (loop with new-targets = nil
        for target in *targets*
        do (cond
             ((pacman-eat-target-p target)
              (incf *score*))
             (t
              (draw target)
              (push target new-targets)))
        finally (setf *targets* new-targets)))

(defun update-state ()
  (with-surface (panel-surface (create-surface *width* 100))
    (fill-surface *black*)
    (draw-string-solid-* "Lispac" 10 10)
    (draw-string-solid-*
     (format nil "FPS ~d Speed ~d" (frame-rate) (pacman-speed *pacman*))
                         10 35)
    (draw-string-solid-* (format nil "Radius ~d" (pacman-radius *pacman*))
                         10 60)
    (draw-string-solid-* (format nil ":: Score ~d ::" *score*)
                         (/ *width* 2) 60 :justify :right)
    (draw-string-solid-* (format-clock *clock*)
                         (/ *width* 2) 30 :justify :right)
    (blit-surface panel-surface *default-display*)))

(defun update-pacman ()
  (with-slots (x y (r radius) speed direction next-direction)
      *pacman*
    (let (
          ;; Tiles wich pacman use as a square
          (left (floor (- x r) *tile-size*))
          (top (floor (- y r) *tile-size*))
          (right (floor (+ x r -1) *tile-size*))
          (bottom (floor (+ y r -1) *tile-size*))

          displacement)

      (labels
          (
           ;; Try to move X pixels in the given direction, stop if
           ;; pacman hits a wall or the board corners.
           (move-up (pixels)
             (setf displacement
                   (- y
                      (max (cond
                             ((zerop top)
                              r)
                             ((board-row-clear-p (1- top) left right)
                              (+ (* *tile-size* (1- top)) r))
                             (t
                              (+ (* *tile-size* top) r)))
                           (- y pixels))))
             (decf y displacement))
           (move-down (pixels)
             (setf displacement
                   (- (min (cond
                             ((= bottom (1- *board-height*))
                              (- (* *tile-size* *board-height*) r 1))
                             ((board-row-clear-p (1+ bottom) left right)
                              (- (* *tile-size* (+ bottom 2)) r))
                             (t
                              (- (* *tile-size* (1+ bottom)) r)))
                           (+ y pixels))
                      y))
             (incf y displacement))
           (move-left (pixels)
             (setf displacement
                   (- x
                      (max (cond
                             ((zerop left)
                              r)
                             ((board-column-clear-p (1- left) top bottom)
                              (+ (* *tile-size* (1- left)) r))
                             (t
                              (+ (* *tile-size* left) r)))
                           (- x pixels))))
             (decf x displacement))
           (move-right (pixels)
             (setf displacement
                   (- (min (cond
                             ((= right (1- *board-width*))
                              (- (* *tile-size* *board-width*) r 1))
                             ((board-column-clear-p (1+ right) top bottom)
                              (- (* *tile-size* (+ right 2)) r))
                             (t
                              (- (* *tile-size* (1+ right)) r)))
                           (+ x pixels))
                      x))
             (incf x displacement))

           ;; Move in the requested direction and return how much
           ;; pixels pacman moved
           (move (pixels direction)
             (case direction
               (:up (move-up pixels))
               (:down (move-down pixels))
               (:left (move-left pixels))
               (:right (move-right pixels)))
             displacement))
        (declare (inline move))

        ;; Print pacman used tiles square if requested
        (when *print-units-rectangles-p*
          (let ((pacman-square (rectangle-from-edges-*
                                (* *tile-size* left)
                                (* *tile-size* top)
                                (1- (* *tile-size* (1+ right)))
                                (1- (* *tile-size* (1+ bottom))))))
            (draw-rectangle pacman-square :color *white*)))

        ;; Do the actual pacman moves
        ;;
        ;; TODO: Document it!
        (if (zerop (move speed next-direction))
	    (progn
	      (let* ((pixels-to-next-tile
		      (ecase direction
			(:up
			 (- y (if (> (+ r y) (* *tile-size* (1+ top)))
				  (- (* *tile-size* (1+ top)) r)
				  (- (* *tile-size* top) r))))
			(:down
			 (- (if (< (- y r) (* *tile-size* bottom))
				(+ r (* *tile-size* bottom))
				(+ r (* *tile-size* (1+ bottom))))
			    y))
			(:left
			 (- x (if (> (+ r x) (* *tile-size* (1+ left)))
				  (- (* *tile-size* (1+ left)) r)
				  (- (* *tile-size* left) r))))
			(:right
			 (- (if (< (- x r) (* *tile-size* right))
				(+ r (* *tile-size* right))
				(+ r (* *tile-size* (1+ right))))
			    x)))))
		(move (min speed pixels-to-next-tile) direction)))
	    (setf direction next-direction)))))

  (draw *pacman*))

(defun update ()
  (blit-surface *board-surface*)
  (clock-tick *clock*)
  (clocks-tick)
  (update-pacman)
  (update-state)
  (update-targets)
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
