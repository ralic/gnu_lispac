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


;;; Code:

(in-package :lispac)

;;; Game settings

(defvar *fps* 60)
(defvar *score* 0)
(defvar *background* *black*)
(defvar *target-radius* 2)

;; Width & height of the frame in pixels
(defvar *width*  600)
(defvar *height* 400)

;; Board stored as a board object.
(defvar *board*)

;; If non-nil, print the tiles the units uses.
(defvar *print-units-rectangles-p* nil)

(defvar *pacman*)                       ; The yellow ball :-)
(defvar *targets* ())

;; Hmm... do draw & *orange* belong to this section? - MXCC
(defgeneric draw (unit))

(defvar *orange* (color :r 255 :g 127 :b 0))

;;; Board

(defclass board ()
  ((tile-size
    :initarg :tile-size
    :type fixnum
    :accessor board-tile-size)
   (tiles
    :initarg :tiles
    :accessor board-tiles)
   (surface
    :initarg :surface
    :type surface
    :accessor board-surface)))

(defun make-board (tile-size width height &optional tile)
  (make-instance 'board
                 :tile-size tile-size
                 :tiles (make-array (list width height)
                                    :element-type '(member t nil)
                                    :initial-element tile)))

(defun tile (board x y)
  (declare (board board)
           (fixnum x y))
  (aref (board-tiles board) x y))

(defun set-tile (board x y value)
  (setf (aref (board-tiles board) x y) value))

(defsetf tile set-tile)

(defun board-width (board)
  (declare (board board))
  (array-dimension (board-tiles board) 0))

(defun board-height (board)
  (declare (board board))
  (array-dimension (board-tiles board) 1))

(defun generate-dumb-board (tile-size width height)
  (let ((board (make-board tile-size width height)))
    (dotimes (x width)
      (dotimes (y height)
        (setf (tile board x y)
              (and (divisiblep x 4)
                   (divisiblep y 4)))))
    board))

;; Load the board from a portable bit map.

;; 0 = way, 1 = wall.
(defun load-board-from-pbm (stream tile-size)
  (let* ((dimensions (read-pbm-header stream))
         (width (elt dimensions 0))
         (height (elt dimensions 1))
         (board (make-board tile-size width height)))
    (do-pbm-pixels
        (pixel)
        (dimensions x y)
        stream
      (setf (tile board x y) (= 1 pixel)))
    board))

(defun load-board-from-pbm-file (file tile-size)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (load-board-from-pbm s tile-size)))

(defun board-square-clear-p (left top right bottom)
  (block function
    (dorange (x left right)
      (dorange (y top bottom)
        (when (tile *board* x y)
          (return-from function nil))))
    t))

(defun board-row-clear-p (y &optional left right)
  (board-square-clear-p (or left 0) y
                        (or right (1- (board-width *board*))) y))

(defun board-column-clear-p (x &optional top bottom)
  (board-square-clear-p x (or top 0)
                        x (or bottom (1- (board-height *board*)))))

;;; Clock

(defclass game-clock ()
  ((ticks
    :type fixnum
    :accessor game-clock-ticks
    :initform 0
    :initarg :t)
   (seconds
    :type fixnum
    :accessor game-clock-seconds
    :initform 0
    :initarg :s)
   (minutes
    :type fixnum
    :accessor game-clock-minutes
    :initform 0
    :initarg :m)
   (hours
    :type fixnum
    :accessor game-clock-hours
    :initform 0
    :initarg :h)
   (stop
    :accessor game-clock-stop
    :initform nil)))

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

(defmacro with-timer ((name var func) &body body)
  (add-clock name)
  `(let ((,var ,(get-clock name)))
     ,func
     (clock-toggle ,var)
     (with-slots (ticks seconds minutes hours)
         ,var
       ,@body)))

;;; Targets

(defclass target ()
  ((count
    :type integer
    :accessor target-count
    :initform 0
    :initarg :count)
   (x
    :type fixnum
    :accessor target-x
    :initform 0
    :initarg :x)
   (y
    :type fixnum
    :accessor target-y
    :initform 0
    :initarg :y)))

(defun* add-target ((integer count x y))
  (push (make-instance 'target :count count :x x :y y) *targets*))

(defun* pacman-add-target ((pacman pac) (integer count))
  (with-slots ((r radius) x y direction)
      pac
    (ecase direction
      (:up (add-target count x (+ y r)))
      (:down (add-target count x (- y r)))
      (:left (add-target count (+ x r) y))
      (:right (add-target count (- x r) y)))))

(defun* pacman-eat-target-p ((target target))
  (< (distance-* (unit-x *pacman*) (unit-y *pacman*)
                 (target-x target) (target-y target))
     (+ (unit-radius *pacman*) *target-radius*)))

(defmethod draw ((target target))
  (with-slots (x y) target
    (draw-filled-circle-* x y *target-radius* :color *orange*)))

;;; Units

(defclass unit ()
  ((x
    :initarg :x
    :type fixnum
    :initform (round *width* 2)
    :accessor unit-x)
   (y
    :initarg :y
    :type fixnum
    :initform (round *height* 2)
    :accessor unit-y)
   (radius
    :initarg :radius
    :type fixnum
    :initform 12
    :accessor unit-radius)
   (speed
    :initarg :speed
    :type fixnum
    :initform 2
    :accessor unit-speed)))

(defclass pacman (unit)
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
    :accessor pacman-next-direction)))

;; Tiles wich pacman use as a square
(defmacro with-unit-boundary ((unit tile-size &optional prefix) &body body)
  (flet ((intern* (name)
           (if prefix
               (intern (concatenate 'string prefix name))
               (intern name))))
    (let ((left (intern* "LEFT"))
          (top (intern* "TOP"))
          (right (intern* "RIGHT"))
          (bottom (intern* "BOTTOM")))
      (with-gensyms (tile-size-tmp)
        `(with-slots (x y radius) ,unit
           (let* ((,tile-size-tmp ,tile-size)
                  (,left (floor (- x r) ,tile-size-tmp))
                  (,top (floor (- y r) ,tile-size-tmp))
                  (,right (floor (+ x r -1) ,tile-size-tmp))
                  (,bottom (floor (+ y r -1) ,tile-size-tmp)))
             ,@body))))))

;; Move in the requested direction and return how much
;; pixels the unit  moved

;; Note: We can't corretly move more than one tile.
(defmethod move-unit ((unit unit) pixels direction)
  (declare ;; (direction direction) and define direction type - MXCC
           (fixnum pixels))
  (let ((board-width (board-width *board*))
        (board-height (board-height *board*))
        (tile-size (board-tile-size *board*)))
    (with-slots (x y (r radius)) unit
      (with-unit-boundary (unit tile-size)
        (case direction
          (:up
           (decf* y (min pixels
                         (- y
                            (cond
                              ((zerop top)
                               r)
                              ((board-row-clear-p (1- top) left right)
                               (+ (* tile-size (1- top)) r))
                              (t
                               (+ (* tile-size top) r)))))))
          (:down
           (incf* y (min pixels
                         (- (cond
                              ((= bottom (1- board-height))
                               (- (* tile-size board-height) r))
                              ((board-row-clear-p (1+ bottom) left right)
                               (- (* tile-size (+ bottom 2)) r))
                              (t
                               (- (* tile-size (1+ bottom)) r)))
                            y))))
          (:left
           (decf* x (min pixels
                         (- x
                            (cond
                              ((zerop left)
                               r)
                              ((board-column-clear-p (1- left) top bottom)
                               (+ (* tile-size (1- left)) r))
                              (t
                               (+ (* tile-size left) r)))))))
          (:right
           (incf* x (min pixels
                         (- (cond
                              ((= right (1- board-width))
                               (- (* tile-size board-width) r))
                              ((board-column-clear-p (1+ right) top bottom)
                               (- (* tile-size (+ right 2)) r))
                              (t
                               (- (* tile-size (1+ right)) r)))
                            x)))))))))

(defmethod draw ((pacman pacman))
  (with-slots ((r radius) x y direction)
      pacman
    (let ((a (round (* 60 (abs (cos (* (/ (game-clock-ticks *clock*)
                                          *fps*) 2 pi)))))))
      (draw-filled-circle-* x y r :color *yellow* :stroke-color *background*)
      (ecase direction
        (:up
         (sdl-gfx:draw-filled-pie-* x y r
                                    (- 270 (round a 2))
                                    (+ 270 (round a 2))
                                    :color *background*)
         (draw-filled-circle-* (- x (round r 2)) y (round r 5)
                               :color *black*))
        (:down
         (sdl-gfx:draw-filled-pie-* x y r
                                    (- 90 (round a 2))
                                    (+ 90 (round a 2))
                                    :color *background*)
         (draw-filled-circle-* (- x (round r 2)) y (round r 5)
                               :color *black*))
        (:left
         (sdl-gfx:draw-filled-pie-* x y r
                                    (- 180 (round a 2))
                                    (- (round a 2) 180)
                                    :color *background*)
         (draw-filled-circle-* x (- y (round r 2)) (round r 5)
                               :color *black*))
        (:right
         (sdl-gfx:draw-filled-pie-* x y r
                                    (- 360 (round a 2))
                                    (round a 2)
                                    :color *background*)
         (draw-filled-circle-* x (- y (round r 2)) (round r 5)
                               :color *black*))))))

;;; Game loop

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
     (incf (unit-speed *pacman*)))
    (:sdl-key-s
     (decf (unit-speed *pacman*)))
    (:sdl-key-d
     (incf (unit-radius *pacman*)))
    (:sdl-key-f
     (decf (unit-radius *pacman*)))
    (:sdl-key-x
     (pacman-add-target *pacman* 5))
    (:sdl-key-q
     (incf *target-radius*))
    (:sdl-key-w
     (decf *target-radius*))
    (:sdl-key-t
     (setf (unit-x *pacman*) (/ *width* 2))
     (setf (unit-y *pacman*) (/ (- *height* 100) 2)))
    (:sdl-key-c
     (clock-toggle *clock*))))

(defun update-board ()
  (with-slots (tile-size surface) *board*
    (let ((width (board-width *board*))
          (height (board-height *board*)))
      (setf surface (create-surface (* tile-size width)
                                    (* tile-size height)))
      (dotimes (y height)
        (dotimes (x width)
          (draw-box-* (* tile-size x) (* tile-size y)
                      tile-size tile-size
                      :surface surface
                      :color (if (tile *board* x y)
                                 *red*
                                 *black*)))))))

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
     (format nil "FPS ~d Speed ~d" (frame-rate) (unit-speed *pacman*))
                         10 35)
    (draw-string-solid-* (format nil "Radius ~d" (unit-radius *pacman*))
                         10 60)
    (draw-string-solid-* (format nil ":: Score ~d ::" *score*)
                         (/ *width* 2) 60 :justify :right)
    (draw-string-solid-* (format-clock *clock*)
                         (/ *width* 2) 30 :justify :right)
    (blit-surface panel-surface *default-display*)))

(defun update-pacman ()
  (with-slots (x y (r radius) speed direction next-direction)
      *pacman*
    (let* ((tile-size (board-tile-size *board*)))

      (with-unit-boundary (*pacman* tile-size)
        ;; Print pacman used tiles square if requested
        (when *print-units-rectangles-p*
          (let ((pacman-square (rectangle-from-edges-*
                                (* tile-size left)
                                (* tile-size top)
                                (1- (* tile-size (1+ right)))
                                (1- (* tile-size (1+ bottom))))))
            (draw-rectangle pacman-square :color *white*)))

        ;; Do the actual pacman moves
        ;;
        ;; TODO: Document it!
        (if (zerop (move-unit *pacman* speed next-direction))
            (progn
              (let* ((pixels-to-next-tile
                      (ecase direction
                        (:up
                         (- y
                            (if (> (+ r y) (* tile-size (1+ top)))
                                (- (* tile-size (1+ top)) r)
                                (- (* tile-size top) r))))
                        (:down
                         (- (if (< (- y r) (* tile-size bottom))
                                (+ r (* tile-size bottom))
                                (+ r (* tile-size (1+ bottom))))
                            y))
                        (:left
                         (- x
                            (if (> (+ r x) (* tile-size (1+ left)))
                                (- (* tile-size (1+ left)) r)
                                (- (* tile-size left) r))))
                        (:right
                         (- (if (< (- x r) (* tile-size right))
                                (+ r (* tile-size right))
                                (+ r (* tile-size (1+ right))))
                            x)))))
                (move-unit *pacman* (min speed pixels-to-next-tile) direction)))
            (setf direction next-direction)))))

  (draw *pacman*))

(defun update ()
  (blit-surface (board-surface *board*))
  (clock-tick *clock*)
  (clocks-tick)
  (update-pacman)
  (update-state)
  (update-targets)
  (draw-rectangle-* 0 100 *width* *height* :color *red*
                    :surface *default-display*)
  (update-display))

;;;; Run pacman

;; Use `generate-dumb-board' or `load-board-from-pbm-file' to load a
;; non-trivial-map.  The default one contains no walls at all.
(defun run-and-wait ()
  (with-init (sdl-init-video)
    (let ((screen (window *width* (+ *height* 100) :title-caption "Lispac")))
      (setf (frame-rate) *fps*)
      (clear-display *black*)
      (initialise-default-font *font-10x20*)
      (setf *pacman* (make-instance 'pacman))
      (with-surface
          (*default-surface* (create-surface *width* *height* :y 100))
        (update-board)
        (with-events ()
          (:quit-event () t)
          (:key-down-event (:key key) (keypress key))
          (:idle ()
                 (update)
                 (blit-surface *default-surface* screen)))))))

;; Non-locking run (Run in another thread, if SBCL threads are available).
(defun run ()
  #+sb-thread (sb-thread:make-thread #'run-and-wait)
  #-sb-thread (run-and-wait)
  (values))

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: us-ascii-unix
;; End:

;;; lispac.lisp ends here
