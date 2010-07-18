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

;;;; Display

(defvar *background* *black*)

;; If non-nil, print the tiles the units uses.
(defvar *print-units-rectangles-p* nil)

;; If non-nil, print tile background in grayscale according to it
;; distance to respawn point.
(defvar *print-respawn-gradient* nil)

;; Board stored as a board object.
(defvar *board*)

;; Width of the frame in pixels
(defvar *width*  600)

;; Height of the frame in pixels
(defvar *height* 400)

(defvar *tile-size* 12)

(defvar *target-radius* 2)
(defvar *pacman*)                       ; The yellow ball :-)
(defvar *targets* ())
(defvar *monsters* ())

;;; Board

(defclass board ()
  ((tiles
    :initarg :tiles
    :accessor board-tiles)
   (surface
    :initarg :surface
    :type surface
    :accessor board-surface)
   (respawn
    :initarg :respawn
    :type point
    :accessor board-respawn)
   (respawn-gradient
    :initarg :respawn-gradient
    :accessor board-respawn-gradient)))

(defun make-board (width height &optional tile)
  (make-instance 'board
                 :tiles
                 (make-array (list width height)
                             :element-type '(member t nil)
                             :initial-element tile)
                 :respawn-gradient
                 (make-array (list width height)
                             :element-type 'fixnum
                             :initial-element most-positive-fixnum)))

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

(defun generate-dumb-board (width height)
  (let ((board (make-board width height)))
    (dotimes (x width)
      (dotimes (y height)
        (setf (tile board x y)
              (and (divisiblep x 4)
                   (divisiblep y 4)))))
    board))

;; Load the board from a portable bit map.

;; 0 = way, 1 = wall.
(defun load-board-from-pbm (stream)
  (let* ((dimensions (read-pbm-header stream))
         (width (elt dimensions 0))
         (height (elt dimensions 1))
         (board (make-board width height)))
    (do-pbm-pixels
        (pixel)
        (dimensions x y)
        stream
      (setf (tile board x y) (= 1 pixel)))
    board))

(defun load-board-from-pbm-file (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (load-board-from-pbm s)))

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

;;;; Gradients

;; TODO: Write documentation
(defun board-compute-gradient (board gradient x y)
  (declare (board board))
  (let ((next-layer (list (point :x x :y y)))
        this-layer
        visited)
    (loop while next-layer
          for distance = 0 then (1+ distance)
          do (progn
               (setf visited this-layer)
               (setf this-layer next-layer)
               (nilf next-layer)
               (dolist (tile this-layer)
                 (when (find tile visited :test #'equalp)
                   (error "alredy visited"))
                 (let ((x (x tile))
                       (y (y tile)))
                   ;; TODO: Clean up.

                   ;; (format t "visiting ~d.~d~%" x y)
                   ;; (format t " visited ~a~%" visited)
                   (setf (aref gradient (x tile) (y tile)) distance)
                   (loop for x in (list (1- x) x x (1+ x))
                         for y in (list y (1- y) (1+ y) y)
                         do (let ((point (point :x x :y y)))
                              (or (< x 0)
                                  (< y 0)
                                  (<= (board-width board) x)
                                  (<= (board-height board) y)
                                  (tile board x y)
                                  (find point visited :test #'equalp)
                                  (find point next-layer :test #'equalp)
                                  (find point this-layer :test #'equalp)
                                  (push point next-layer))))
                   ;; (format t " pending ~a~%" next-layer)
                   ))))))

;; Update slot `gradient' of `board'
(defun board-update-respawn-gradient (board &optional x y)
  (declare (board board))
  (with-slots (respawn respawn-gradient) board
    (when (or x y)
      (setf respawn (point :x x :y y)))
    (board-compute-gradient board respawn-gradient x y)))

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
   (speed
    :initarg :speed
    :type fixnum
    :initform 2
    :accessor unit-speed)
   (controller
    :initarg :controller
    :type function
    :accessor unit-controller)))

(defgeneric draw (unit))

;; Tiles wich pacman use as a square
(defmacro with-unit-boundary ((unit &optional prefix) &body body)
  (flet ((intern* (name)
           (if prefix
               (intern (concatenate 'string prefix name))
               (intern name))))
    (let ((left (intern* "LEFT"))
          (top (intern* "TOP"))
          (right (intern* "RIGHT"))
          (bottom (intern* "BOTTOM")))
      (with-gensyms (x y r)
        `(with-slots ((,x x) (,y y)) ,unit
           (let* ((,r (/ *tile-size* 2))
                  (,left (floor (- ,x ,r) *tile-size*))
                  (,top (floor (- ,y ,r) *tile-size*))
                  (,right (floor (+ ,x ,r -1) *tile-size*))
                  (,bottom (floor (+ ,y ,r -1) *tile-size*)))
             ,@body))))))

;; Move in the requested direction and return how much
;; pixels the unit  moved

;; Note: We can't corretly move more than one tile.
(defmethod move-unit ((unit unit) pixels direction)
  (declare ;; (direction direction) and define direction type - MXCC
           (fixnum pixels))
  (let ((board-width (board-width *board*))
        (board-height (board-height *board*))
        (r (/ *tile-size* 2)))
    (with-slots (x y) unit
      (with-unit-boundary (unit)
        (case direction
          (:up
           (decf* y (min pixels
                         (- y
                            (cond
                              ((zerop top)
                               r)
                              ((board-row-clear-p (1- top) left right)
                               (+ (* *tile-size* (1- top)) r))
                              (t
                               (+ (* *tile-size* top) r)))))))
          (:down
           (incf* y (min pixels
                         (- (cond
                              ((= bottom (1- board-height))
                               (- (* *tile-size* board-height) r))
                              ((board-row-clear-p (1+ bottom) left right)
                               (- (* *tile-size* (+ bottom 2)) r))
                              (t
                               (- (* *tile-size* (1+ bottom)) r)))
                            y))))
          (:left
           (decf* x (min pixels
                         (- x
                            (cond
                              ((zerop left)
                               r)
                              ((board-column-clear-p (1- left) top bottom)
                               (+ (* *tile-size* (1- left)) r))
                              (t
                               (+ (* *tile-size* left) r)))))))
          (:right
           (incf* x (min pixels
                         (- (cond
                              ((= right (1- board-width))
                               (- (* *tile-size* board-width) r))
                              ((board-column-clear-p (1+ right) top bottom)
                               (- (* *tile-size* (+ right 2)) r))
                              (t
                               (- (* *tile-size* (1+ right)) r)))
                            x)))))))))

;; Move `unit' up to `max-pixels' to the begin of the next
;; left/right/top/left row or column according to `direction'.  Return
;; the pixels moved count.
(defun unit-align (unit max-pixels direction)
  (with-slots (x y) unit
    (with-unit-boundary (unit)
      (let* ((r (/ *tile-size* 2))
             (pixels-to-next-tile
              (ecase direction
                (:up
                 (- y
                    (if (> (+ r y) (* *tile-size* (1+ top)))
                        (- (* *tile-size* (1+ top)) r)
                        (- (* *tile-size* top) r))))
                (:down
                 (- (if (< (- y r) (* *tile-size* bottom))
                        (+ r (* *tile-size* bottom))
                        (+ r (* *tile-size* (1+ bottom))))
                    y))
                (:left
                 (- x
                    (if (> (+ r x) (* *tile-size* (1+ left)))
                        (- (* *tile-size* (1+ left)) r)
                        (- (* *tile-size* left) r))))
                (:right
                 (- (if (< (- x r) (* *tile-size* right))
                        (+ r (* *tile-size* right))
                        (+ r (* *tile-size* (1+ right))))
                    x)))))
        (move-unit unit (min max-pixels pixels-to-next-tile) direction)))))

(defun unit-act (unit)
  (declare (unit unit))
  (funcall (unit-controller unit) unit))

;;;; Pacman

(defclass pacman (unit)
  (;; TODO: Implement pacman upon a surface, in order to we can use GFX
   ;; to rotation and more.
   ;; (surface ...)
   (direction
    :initarg :direction
    :type (member :up :down :left :right)
    :initform :right
    :accessor pacman-direction)
   (controller
    :initform #'standard-controller)))

(defmethod draw ((pacman pacman))
  (with-slots (x y direction)
      pacman
    (let ((a (round (* 60 (abs (cos (* (/ (game-clock-ticks *clock*)
                                          *fps*) 2 pi))))))
          (r (/ *tile-size* 2)))
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

(defvar *next-direction* :right)

(defun standard-controller (unit)
  (declare (unit unit))
  (with-slots (x y speed direction) unit
    (if (zerop (move-unit unit speed *next-direction*))
        (unit-align unit speed direction)
        (setf direction *next-direction*))))

;;;; Monster

(defclass monster (unit)
  ((crazyp
    :initarg :crazyp
    :type boolean
    :accessor crazyp)
   (livep
    :initarg :livep
    :type boolean
    :accessor livep)
   ;; This is unrelated to pacman-direction
   (direction
    :initarg :direction
    :type (member :up :down :left :right)
    :accessor monster-direction)))

(defmethod draw ((monster monster))
  (with-slots (x y) monster
    (let ((r (/ *tile-size* 2)))
      (draw-filled-circle-* x y r :color *orange*)
      (draw-box-* (- x (/ r 2)) (- y (/ r 2)) r r :color *blue*)
      )))

;; Move a <<dead>> monster towards the respawn point
(defun spirit-controller (monster)
  (declare (monster monster))
  (flet ((gradient (x y)
           (aref (board-respawn-gradient *board*) x y)))
    (with-slots (speed) monster
      (with-unit-boundary (monster)
        (cond
          ;; Monster is between two tiles horizontaly
          ((/= left right)
           (if (< (gradient left top) (gradient right top))
               (unit-align monster speed :left)
               (unit-align monster speed :right)))
          ;; Monster is between two tiles verticaly
          ((/= top bottom)
           (if (< (gradient left top) (gradient left bottom))
               (unit-align monster speed :up)
               (unit-align monster speed :down)))
          ;; Monster is just on one tile
          (t
           (let ((current-gradient-value (gradient left top))
                 (rightmost-tile (1- (board-width *board*)))
                 (bottomost-tile (1- (board-height *board*))))
             ;; Climb to a neighbor tile with lower gradient value.
             ;; Note that the difference in gradient value should
             ;; always be 1, so it isn't nessesary to check other
             ;; neighbors.
             (cond
               ((and (< 0 left)
                     (< (gradient (1- left) top) current-gradient-value))
                (unit-align monster speed :left))
               ((and (> rightmost-tile left)
                     (< (gradient (1+ left) top) current-gradient-value))
                (unit-align monster speed :right))
               ((and (< 0 top)
                     (< (gradient left (1- top)) current-gradient-value))
                (unit-align monster speed :up))
               ((and (> bottomost-tile top)
                     (< (gradient left (1+ top)) current-gradient-value))
                (unit-align monster speed :down))))))))))

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
  (with-slots (x y direction)
      pac
    (let ((r (/ *tile-size* 2)))
      (ecase direction
        (:up (add-target count x (+ y r)))
        (:down (add-target count x (- y r)))
        (:left (add-target count (+ x r) y))
        (:right (add-target count (- x r) y))))))

(defun* pacman-eat-target-p ((target target))
  (< (distance-* (unit-x *pacman*) (unit-y *pacman*)
                 (target-x target) (target-y target))
     (+ *tile-size* *target-radius*)))

(defmethod draw ((target target))
  (with-slots (x y) target
    (draw-filled-circle-* x y *target-radius* :color *orange*)))

;;; Game loop

(defun keypress (key)
  (case key
    (:sdl-key-escape
     (push-quit-event))
    (:sdl-key-up
     (setf *next-direction* :up))
    (:sdl-key-down
     (setf *next-direction* :down))
    (:sdl-key-left
     (setf *next-direction* :left))
    (:sdl-key-right
     (setf *next-direction* :right))
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
  (with-slots (surface respawn-gradient) *board*
    (let ((width (board-width *board*))
          (height (board-height *board*)))
      (setf surface (create-surface (* *tile-size* width)
                                    (* *tile-size* height)))
      (dotimes (y height)
        (dotimes (x width)
          (let* ((gradient-value (* 5 (aref respawn-gradient x y)))
                 (color (if (tile *board* x y)
                            *red*
                            (if *print-respawn-gradient*
                                (color :r gradient-value
                                       :g gradient-value
                                       :b gradient-value)
                                *black*))))
            (draw-box-* (* *tile-size* x) (* *tile-size* y)
                        *tile-size* *tile-size*
                        :surface surface
                        :color color)))))))

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
    (draw-string-solid-* (format nil "Tile size ~d" *tile-size*)
                         10 60)
    (draw-string-solid-* (format nil ":: Score ~d ::" *score*)
                         (/ *width* 2) 60 :justify :right)
    (draw-string-solid-* (format-clock *clock*)
                         (/ *width* 2) 30 :justify :right)
    (blit-surface panel-surface *default-display*)))

(defun update-pacman ()
  (with-slots (x y speed direction next-direction)
      *pacman*
    (with-unit-boundary (*pacman*)
      ;; Print pacman used tiles square if requested
      (when *print-units-rectangles-p*
        (let ((pacman-square (rectangle-from-edges-*
                              (* *tile-size* left)
                              (* *tile-size* top)
                              (1- (* *tile-size* (1+ right)))
                              (1- (* *tile-size* (1+ bottom))))))
          (draw-rectangle pacman-square :color *white*)))
      ;; Do the actual pacman moves
      (unit-act *pacman*)))
  (draw *pacman*))

;; Move the monsters and check colisions.
(defun update-monsters ()
  (setf *monsters*
        (with-collecting
          (dolist (monster *monsters*)
            (declare (monster monster))
            (unit-act monster)
            (cond
              ;; Monster is in <<spirit>> form
              ((not (livep monster))
               (let ((respawn-x (x (board-respawn *board*)))
                     (respawn-y (y (board-respawn *board*))))
                 (with-unit-boundary (monster)
                   (when (and (= respawn-x left right)
                              (= respawn-y top bottom))
                     ;; TODO: Set monster controller to standard
                     ;; monster controller, when implemented.
                     (setf (livep monster) t)))
                 (draw monster)
                 (collect monster)))

              ;; No colision
              ((<= *tile-size*
                   (distance-* (unit-x monster) (unit-y monster)
                               (unit-x *pacman*) (unit-y *pacman*)))
               (draw monster)
               (collect monster))

              ;; Colision with hostile monster
              ((crazyp monster)

               ;; TODO: Put something more friendly here
               (error "Monster ate pacman")))))))

(defun update ()
  (blit-surface (board-surface *board*))
  (clock-tick *clock*)
  (clocks-tick)
  (update-monsters)
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
      (setf *pacman* (make-instance 'pacman
                                    :controller #'standard-controller))
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
