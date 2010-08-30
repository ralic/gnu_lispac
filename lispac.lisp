;;;; lispac.lisp

;;;; License

;;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>
;;; Copyrigth (C) 2010 Mario Castelan Castro <marioxcc>

;;; Special thanks to a _not_ anonymous for us, but for everybody else,
;;; who wrote and donated the base of lispac.

;;; This file is part of lispac.

;;; lispac is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; lispac is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with lispac.  If not, see <http://www.gnu.org/licenses/>.


;;;; Package

(in-package :lispac)

;;;; Game settings

(defvar *fps* 60)
(defvar *score* 0)

;;;;; Display

(defvar *background* *black*)

;; If non-nil, print the tiles the units uses.
(defvar *print-units-rectangles-p* nil)

;; If non-nil, print tile background in grayscale according to it
;; distance to respawn point.
(defvar *print-respawn-gradient* nil)

;; Board stored as a board object.
(defvar *board*)

;; Width of the frame in pixels
(defvar *width*)

;; Height of the frame in pixels
(defvar *height*)

(defvar *tile-size* 12)

(defvar *target-radius* 2)
(defvar *pacman*)                       ; The yellow ball :-)
(defvar *targets* ())
(defvar *monsters* ())

(defvar *pacman-gradient-center-x*)
(defvar *pacman-gradient-center-y*)
(defvar *pacman-gradient*)
(defvar *pacman-gradient-max-distance*)

;; Default ticks monsters will be vulnerable when pacman eat a super
;; target.
(defvar *monster-vulnerable-ticks*)


;;;; Board

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

;; Inside a `nil' block iterate over the neighbors of the tile pointed
;; by `x' and `y' (Evaluated).  Bind `x-var' and `y-var' (Evaluated)
;; to the X and Y of each neighbor, when it's "visited".
(defmacro do-neighbor-tiles (tiles (x-var x) (y-var y) &body body)
  (with-gensyms (tiles-tmp width height)
    `(let* ((,tiles-tmp ,tiles)
            (,width (array-dimension ,tiles-tmp 0))
            (,height (array-dimension ,tiles-tmp 1)))
       (loop for ,x-var in (list (1- ,x) ,x ,x (1+ ,x))
             for ,y-var in (list ,y (1- ,y) (1+ ,y) ,y)
             do (when (and (<= 0 ,x-var)
                           (<= 0 ,y-var)
                           (> ,width ,x-var)
                           (> ,height ,y-var)
                           (not (aref ,tiles ,x-var ,y-var)))
                  ,@body)))))

;; TODO: Write documentation
(defun board-map-connected-tiles (function board x y &optional max-distance)
  (declare (function function)
           (board board)
           (fixnum x y))
  (let ((next-layer (list (vector x y)))
        this-layer
        visited)
    (loop while next-layer
          for distance = 0 then (1+ distance)
          until (and max-distance (> distance max-distance))
          do (progn
               (shiftf visited this-layer next-layer nil)
               (dolist (current-tile this-layer)
                 (let ((current-x (x current-tile))
                       (current-y (y current-tile)))
                   (funcall function current-x current-y distance)
                   (do-neighbor-tiles
                       (board-tiles board)
                       (x current-x)
                       (y current-y)
                     (let ((neighbor (vector x y)))
                       (unless (or (find neighbor visited :test #'point=)
                                   (find neighbor next-layer :test #'point=)
                                   (find neighbor this-layer :test #'point=))
                         (push neighbor next-layer))))))))))

;; Return the count of neighbors non-wall tiles.
;; 0 for a isolated tile (Isolated vertex).
;; 1 for a dead-end
;; 2 for a corridor
;; 3 and 4 for intersections.
(defun tile-degree (tiles x y)
  (let ((degree 0))
    (declare (fixnum degree))
    (do-neighbor-tiles tiles (i x) (j y)
      (unless (aref tiles i j)
        (incf degree)))
    degree))

(defun adjacentp (ax ay bx by)
  (declare (fixnum ax ay bx by))
  (= 1 (+ (abs (- ax bx)) (abs (- ay by)))))

;;;;; Gradients

;; TODO: Write documentation
(defun board-compute-gradient (board gradient x y &optional max-distance)
  (declare (board board))
  (let ((function (lambda (x y distance)
          (setf (aref gradient x y) distance))))
    (board-map-connected-tiles function board x y max-distance)))

;; Update slot `gradient' of `board'
(defun board-update-respawn-gradient (board &optional x y)
  (declare (board board))
  (with-slots (respawn respawn-gradient) board
    (when (or x y)
      (setf respawn (point :x x :y y)))
    (board-compute-gradient board respawn-gradient x y)))

;;;;; Waypoints.

;;; Waypoints provide an alternative to raw gradients.  The waypoint
;;; graph is an abstract representation of the board as a graph.
;;; Every edge represents a corridor and every vertex represents the
;;; intersections.

;; Regarding waypoints there is a vertex in every intersection (Tiles
;; with 1, 3 or 4 neighbors).
(defstruct (vertex (:constructor make-vertex (x y)))
  x
  y
  edges)

;; Edges are directed (One-way).  The source vertex is implicit; only
;; the sink vertex is stored.
(defstruct (edge (:constructor make-edge (sink weight gateway-x gateway-y)))
  ;; Sink from the arrow viewpoint.
  sink
  ;; Distance from source vertex to sink vertex through this edge.
  weight
  ;; First tile of the path.
  gateway-x
  gateway-y)

(defun vertex-join (source sink gateway-x gateway-y weight)
  (declare (vertex source sink))
  (let ((new-edge (make-edge sink weight gateway-x gateway-y)))
    (dolist* (edge (vertex-edges source))
      (when (eq sink (edge-sink edge))
        (when (< weight (edge-weight edge))
          ;; If there is alredy a edge which connects the same
          ;; vertices with a weight greater than this one, substitute
          ;; it.
          (setf edge new-edge))
        ;; The optimal connection between sink and source is done.
        (return-from vertex-join)))
    (push new-edge (vertex-edges source))
    ;; Return value is meaningless by the moment, discard for avoid
    ;; possible bugs arising for it use.
    (values)))

;; TODO: Write documentation
(defstruct (explorer (:constructor make-explorer (parent-x
                                                  parent-y
                                                  current-x
                                                  current-y
                                                  tiles)))
  tiles
  parent-x
  parent-y
  current-x
  current-y)

;; Should a waypoint be in this tile?.
(defun waypointp (tiles x y)
  (/= (tile-degree tiles x y) 2))

;; In a corridor, advance to the next tile.
(defun explorer-step (explorer)
  (declare (explorer explorer))
  (with-slots (tiles parent-x parent-y current-x current-y) explorer
    (do-neighbor-tiles tiles (neighbor-x current-x) (neighbor-y current-y)
      (unless (and (= neighbor-x parent-x)
                   (= neighbor-y parent-y))
        (shiftf parent-x current-x neighbor-x)
        (shiftf parent-y current-y neighbor-y)
        ;; Don't visit other neighbors.
        (return-from explorer-step t)))))

(defun explorer-explore-to-waypoint (explorer)
  (declare (explorer explorer))
  (with-slots (tiles current-x current-y) explorer
    (loop for distance = 0 then (1+ distance)
          until (waypointp tiles current-x current-y)
          do (explorer-step explorer)
          finally (return (values current-x current-y distance)))))

(defun map-adjacent-waypoints (function board x y)
  (do-neighbor-tiles (board-tiles board) (gateway-x x) (gateway-y y)
    (multiple-value-bind (waypoint-x waypoint-y steps)
        (explorer-explore-to-waypoint (make-explorer x y
                                                     gateway-x gateway-y
                                                     (board-tiles board)))
      (funcall function waypoint-x waypoint-y (1+ steps) gateway-x gateway-y))))

;; Iterate through the waypoints from which there is a direct
;; connection with corridors.
(defmacro do-connected-waypoints ((board
                                   distance
                                   (x-var x) (y-var y)
                                   &optional
                                   gateway-x gateway-y)
                                  &body body)
  `(map-adjacent-waypoints (lambda (,x-var ,y-var
                                    ,distance
                                    ,(or gateway-x (gensym))
                                    ,(or gateway-y (gensym)))
                             ,@body)
                           ,board
                           ,x ,y))

;;;;; Generation and loading

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


;;;; Clock

(defclass clock ()
  ((ticks                               ; (From the start).
    :type fixnum
    :accessor clock-ticks
    :initform 0
    :initarg :t)))

(defvar *clock* (make-instance 'clock))

(defmethod clock-reset ((clock clock))
  (zerof (clock-ticks clock)))

(defmethod format-clock ((clock clock))
  (let* ((ticks (clock-ticks clock))
         (seconds (floor ticks *fps*))
         (minutes (mod (truncate seconds 60) 60))
         (hours (truncate seconds 3600)))
    (format nil "~d:~2,'0d:~2,'0d - ~6,'0d" hours minutes seconds ticks)))


;;;; Units

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

;;;;; Generic movement

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

;; Move `unit' up to `max-pixels' towards lower-values of the
;; `gradient' when `climbp' is non-nil, or higher ones otherwise.
(defun unit-climb-gradient (unit max-pixels gradient &optional (climbp t))
  (flet ((gradient (x y)
           (aref gradient x y)))
    (with-unit-boundary (unit)
      (cond
        ;; Unit is between two tiles horizontaly
        ((/= left right)
         (if (boolean= climbp (< (gradient left top) (gradient right top)))
             (unit-align unit max-pixels :left)
             (unit-align unit max-pixels :right)))
        ;; Unit is between two tiles verticaly
        ((/= top bottom)
         (if (boolean= climbp (< (gradient left top) (gradient left bottom)))
             (unit-align unit max-pixels :up)
             (unit-align unit max-pixels :down)))
        ;; Unit is just on one tile
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
                   (if climbp
                       (< (gradient (1- left) top) current-gradient-value)
                       (> (gradient (1- left) top) current-gradient-value)))
              (unit-align unit max-pixels :left))
             ((and (> rightmost-tile left)
                   (if climbp
                       (< (gradient (1+ left) top) current-gradient-value)
                       (> (gradient (1+ left) top) current-gradient-value)))
              (unit-align unit max-pixels :right))
             ((and (< 0 top)
                   (if climbp
                       (< (gradient left (1- top)) current-gradient-value)
                       (> (gradient left (1- top)) current-gradient-value)))
              (unit-align unit max-pixels :up))
             ((and (> bottomost-tile top)
                   (if climbp
                       (< (gradient left (1+ top)) current-gradient-value)
                       (> (gradient left (1+ top)) current-gradient-value)))
              (unit-align unit max-pixels :down)))))))))

;;;;; Controllers

(defun unit-act (unit)
  (declare (unit unit))
  (funcall (unit-controller unit) unit))

(defvar *next-direction* :right)

;; Move unit according to keyboard input.
(defun standard-controller (unit)
  (declare (unit unit))
  (with-slots (x y speed direction) unit
    (if (zerop (move-unit unit speed *next-direction*))
        (unit-align unit speed direction)
        (setf direction *next-direction*))))

(defmacro define-climber (name gradient climbp)
  (with-gensyms (unit)
    `(defun ,name (,unit)
       (declare (unit ,unit))
       (unit-climb-gradient ,unit (unit-speed ,unit) ,gradient ,climbp))))

;; Move a unit towards the respawn point for exaple, an spirit _dead_
;; monster
(define-climber spirit-controller (board-respawn-gradient *board*) t)

;; Move a unit towards pacman if Manhattan disatance to it <=
;; `*pacman-gradient-max-depth*'
(define-climber pacman-seeker-controller *pacman-gradient* t)

;; Move a unit farther from pacman if Manhattan disatance to it <=
;; `*pacman-gradient-max-depth*'
(define-climber flee-from-pacman-controller *pacman-gradient* nil)

;;;;; Pacman

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
    (let ((a (round (* 60 (abs (cos (* (/ (clock-ticks *clock*)
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

;;;;; Monster

(defclass monster (unit)
  ((livep
    :initarg :livep
    :type boolean
    :accessor monster-livep)
   ;; Monster will be vulnerable until the given tick (Noninclusive).
   (vulnerable-until
    :initarg :vulnerable-until
    :type fixnum
    :initform 0
    :accessor monster-vulnerable-until)
   ;; This is unrelated to pacman-direction
   (direction
    :initarg :direction
    :type (member :up :down :left :right)
    :accessor monster-direction)))

(defmethod monster-hostilep ((monster monster))
  (>= (clock-ticks *clock*) (monster-vulnerable-until monster)))

(defmethod monster-vulnerablep ((monster monster))
  (< (clock-ticks *clock*) (monster-vulnerable-until monster)))

(defmethod monster-spiritp ((monster monster))
  (not (monster-livep monster)))

;; Called for example, when pacman eat a super target.
(defgeneric monster-make-vulnerable (monster))

;; Turn the monster into a "spirit" (Dead and moving to the respawn
;; point).  Called for example, when pacman eat the monster.
(defgeneric monster-kill (monster))

;; Restore standard monster behaviour.
(defgeneric monster-restore (monster))

(defmethod monster-make-vulnerable ((monster monster))
  (setf (unit-controller monster) #'flee-from-pacman-controller)
  (setf (monster-vulnerable-until monster)
        (+ (clock-ticks *clock*) *monster-vulnerable-ticks*)))

(defmethod monster-kill ((monster monster))
  (setf (monster-vulnerable-until monster) (clock-ticks *clock*))
  (nilf (monster-livep monster))
  (setf (unit-controller monster) #'spirit-controller))

(defmethod monster-restore ((monster monster))
  (setf (monster-vulnerable-until monster) (clock-ticks *clock*))
  (tf (monster-livep monster))
  (setf (unit-controller monster) #'pacman-seeker-controller))

(defmethod draw ((monster monster))
  (with-slots (x y) monster
    (let ((background (cond
                        ((monster-spiritp monster)
                         (color :r 127 :g 127 :b 127))
                        ((monster-vulnerablep monster)
                         *cyan*)
                        ((monster-hostilep monster)
                         *red*)))
          (foreground (if (monster-vulnerablep monster)
                          *green*
                          *blue*)))
      (let ((r (/ *tile-size* 2)))
        (draw-filled-circle-* x y r :color background)
        (draw-box-* (- x (/ r 2)) (- y (/ r 2)) r r :color foreground)))))


;;;; Targets

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
    :initarg :y)
   (superp
    :type boolean
    :accessor target-superp
    :initform nil
    :initarg :superp)))

(defun add-target (count x y superp)
  (declare (integer count x y))
  (push (make-instance 'target :count count :x x :y y :superp superp)
        *targets*))

(defun pacman-add-target (pac count &optional superp)
  (declare (pacman pac)
           (integer count))
  (with-slots (x y direction)
      pac
    (let ((r (/ *tile-size* 2)))
      (ecase direction
        (:up (add-target count x (+ y r) superp))
        (:down (add-target count x (- y r) superp))
        (:left (add-target count (+ x r) y superp))
        (:right (add-target count (- x r) y superp))))))

(defun pacman-eat-target-p (target)
  (declare (target target))
  (< (distance-* (unit-x *pacman*) (unit-y *pacman*)
                 (target-x target) (target-y target))
     (+ (/ *tile-size* 2) *target-radius*)))

(defmethod draw ((target target))
  (with-slots (x y superp) target
    (let ((color  (if superp *red* *orange*)))
      (draw-filled-circle-* x y *target-radius* :color color))))


;;;; Game loop

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
    (:sdl-key-y
     (pacman-add-target *pacman* 5 t))
    (:sdl-key-q
     (incf *target-radius*))
    (:sdl-key-w
     (decf *target-radius*))
    (:sdl-key-t
     (setf (unit-x *pacman*) (/ *width* 2))
     (setf (unit-y *pacman*) (/ (- *height* 100) 2)))))

(defun update-board ()
  (with-slots (surface respawn-gradient) *board*
    (let ((width (board-width *board*))
          (height (board-height *board*)))
      (setf surface (create-surface (* *tile-size* width)
                                    (* *tile-size* height)))
      (dotimes (y height)
        (dotimes (x width)
          (let* ((gradient-value (min 255 (* 5 (aref respawn-gradient x y))))
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

(defun make-alive-monsters-vulnerable ()
  (dolist (monster *monsters*)
    (unless (monster-spiritp monster)
      (monster-make-vulnerable monster))))

(defun update-targets ()
  (loop with new-targets = nil
        for target in *targets*
        do (cond
             ((pacman-eat-target-p target)
              (print 'ate)
              (incf *score*)
              (when (target-superp target)
                ;; Make monsters vulnerable
                (make-alive-monsters-vulnerable)))
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

(defun update-pacman-gradient (new-x new-y)
  ;; Clear
  (loop with stack
        for current-tile = nil then (pop stack)
        for x = *pacman-gradient-center-x* then (x current-tile)
        for y = *pacman-gradient-center-y* then (y current-tile)
        do (progn
             (setf (aref *pacman-gradient* x y) most-positive-fixnum)
             (do-neighbor-tiles *pacman-gradient* (neighbor-x x) (neighbor-y y)
               (let ((neighbor (point :x neighbor-x :y neighbor-y)))
                 (declare (point neighbor))
                 (unless (find neighbor stack :test #'equalp)
                   (push neighbor stack))))
             (print stack))
        while stack)
  ;; Recompute (Update) gradient
  (board-compute-gradient *board*
                          *pacman-gradient*
                          new-x
                          new-y
                          *pacman-gradient-max-distance*)
  ;; Update variables
  (setf *pacman-gradient-center-x* new-x)
  (setf *pacman-gradient-center-y* new-y))

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
      (unit-act *pacman*)
      ;; If nessesary, update gradient
      (when (or (/= left *pacman-gradient-center-x*)
                (/= top *pacman-gradient-center-y*))
        (update-pacman-gradient left top))))
  (draw *pacman*))

;; Move the monsters and check colisions.
(defun update-monsters ()
  (dolist (monster *monsters*)
    (declare (monster monster))
    (unit-act monster)
    (when (= (monster-vulnerable-until monster) (clock-ticks *clock*))
      (monster-restore monster))
    (cond
      ;; Monster is in <<spirit>> form
      ((not (monster-livep monster))
       (let ((respawn-x (x (board-respawn *board*)))
             (respawn-y (y (board-respawn *board*))))
         (with-unit-boundary (monster)
           ;; Did monster reached the respawn point?
           (when (and (= respawn-x left right)
                      (= respawn-y top bottom))
             (monster-restore monster)))
         (draw monster)))

      ;; No colision
      ((<= *tile-size*
           (distance-* (unit-x monster) (unit-y monster)
                       (unit-x *pacman*) (unit-y *pacman*)))
       (draw monster))

      ;; Colision with hostile monster
      ((monster-hostilep monster)

       ;; TODO: Put something more friendly here
       (error "Monster ate pacman"))

      ;; Colision with vulnerable monster
      ((monster-vulnerablep monster)
       (monster-kill monster)))))

(defun update ()
  (blit-surface (board-surface *board*))
  (incf (clock-ticks *clock*))
  (update-monsters)
  (update-pacman)
  (update-state)
  (update-targets)
  (draw-rectangle-* 0 100 *width* *height* :color *red*
                    :surface *default-display*)
  (update-display))

;;;;; Run pacman

;; Use `generate-dumb-board' or `load-board-from-pbm-file' to load a
;; non-trivial-map.  The default one contains no walls at all.
(defun run-and-wait ()
  (with-init (sdl-init-video)
    (let* ((*width* (* *tile-size* (board-width *board*)))
           (*height* (* *tile-size* (board-height *board*)))
           (screen (window *width* (+ *height* 100) :title-caption "Lispac")))
      (setf (frame-rate) *fps*)
      (clear-display *black*)
      (initialise-default-font *font-10x20*)
      (setf *pacman*
            (make-instance 'pacman
                           :controller #'standard-controller))
      (setf *pacman-gradient*
            (make-array (list (board-width *board*) (board-height *board*))
                        :initial-element most-positive-fixnum))
      (with-unit-boundary (*pacman*)
        (declare (ignore right bottom))
        (setf *pacman-gradient-center-x* left)
        (setf *pacman-gradient-center-y* top)
        (board-compute-gradient *board*
                                *pacman-gradient*
                                left
                                top
                                *pacman-gradient-max-distance*))
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
;; mode: Lisp
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: us-ascii-unix
;; End:

;;; lispac.lisp ends here
