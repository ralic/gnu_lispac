;;;; lispac.lisp

;;;; License

;;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>
;;; Copyrigth (C) 2010, 2011 Mario Castelan Castro <marioxcc>

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

(defvar *print-waypoints* nil)

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

(defvar *next-direction* :right)
(defvar *pacman-wpt*)

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
   (pacman-spawn
    :initarg :pacman-spawn
    :type point
    :accessor board-pacman-spawn)
   (respawn
    :initarg :respawn
    :type waypoints-tree
    :accessor board-respawn)
   (waypoints
    :initarg :waypoints
    :type sparse-table
    :accessor board-waypoints)))

(defun make-board (width height &optional tile)
  (make-instance 'board
                 :tiles
                 (make-array (list width height)
                             :element-type '(member t nil)
                             :initial-element tile)))

(defun tile (board x y)
  (declare (board board)
           (fixnum x y))
  (aref (board-tiles board) x y))

(defun set-tile (board x y value)
  (setf (aref (board-tiles board) x y) value))

(defsetf tile set-tile)

(defun waypoint (board x y)
  (declare (board board)
           (fixnum x y))
  (stref (board-waypoints board) x y))

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

;;;;; Waypoints

;;; Waypoints provide an alternative to raw gradients.  The waypoint
;;; graph is an abstract representation of the board as a graph.
;;; Every edge represents a corridor and every vertex represents the
;;; intersections.

;; Should a waypoint be in this tile?.
(defun waypointp (tiles x y)
  (and (not (aref tiles x y))
       (/= (tile-degree tiles x y) 2)))

;; Regarding waypoints there is a vertex in every intersection (Tiles
;; with 1, 3 or 4 neighbors).
(defstruct (vertex (:constructor make-vertex (x y)))
  x
  y
  edges)

(defun vertex-position (vertex)
  (declare (vertex vertex))
  (values (vertex-x vertex) (vertex-y vertex)))

(defun vertex-position-= (vertex x y)
  (declare (vertex vertex)
           (fixnum x y))
  (and (= (vertex-x vertex) x)
       (= (vertex-y vertex) y)))

;; Edges are directed (One-way).  The source vertex is implicit; only
;; the sink vertex is stored.
(defstruct (edge (:constructor make-edge
                               (sink weight direction complement)))
  ;; Sink from the arrow viewpoint.
  sink
  ;; Distance from source vertex to sink vertex through this edge.
  weight
  ;; Direction to the first tile of the path.
  direction
  ;; The edge in the oposite direction.
  complement)

;; Add an edge and its complement between `a' and `b'.
(defun vertex-link (a b weight a-g-x a-g-y b-g-x b-g-y)
  (declare (vertex a b))
  (declare (fixnum a-g-x a-g-y b-g-x b-g-y))
  (let* ((a-g-d (direction (vertex-x a) (vertex-y a) a-g-x a-g-y))
         (a-e (make-edge b weight a-g-d nil))
         (b-g-d (direction (vertex-x b) (vertex-y b) b-g-x b-g-y))
         (b-e (make-edge a weight b-g-d a-e)))
    (setf (edge-complement a-e) b-e)
    (push a-e (vertex-edges a))
    (push b-e (vertex-edges b))))

;; Find a `edge' whose a gateway is in `direction' from `vertex'.
(defun vertex-edge-to (vertex direction)
  (declare (vertex vertex)
           (direction direction))
  (find direction (vertex-edges vertex) :key #'edge-direction))

;;;;;; Exploreres

;;; TODO: Write documentation

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

;; Iterate through the waypoints for which there is a direct
;; connection with corridor.
;;
;; If given, only directions in `direction' or `directions-not' are
;; explored or not explored, respectively; otherwise all directions
;; are explored.  `direction' is incompabile with `direction-not'.
;;
;; At the time `body' is evaluated (`gateway-x', `gateway-y') and
;; (`waypoint-gateway-x', `waypoint-gateway-y') if given are bound to
;; the origin-side and waypoint-side gateways, respectively.  I.e: The
;; coordinate of the first tile from the origin (`x', `y') or the
;; currently visited waypoint.
(defmacro do-connected-waypoints ((board
                                   distance
                                   (waypoint-x x) (waypoint-y y)
                                   &key
                                   directions
                                   directions-not
                                   gateway-x
                                   gateway-y
                                   waypoint-gateway-x
                                   waypoint-gateway-y)
                                  &body body)
  (with-gensyms (explorer
                 tiles
                 steps
                 (gateway-x gateway-x)
                 (gateway-y gateway-y))
    `(let ((,tiles (board-tiles ,board)))
       (do-neighbor-tiles
           ,tiles
           (,gateway-x ,x)
           (,gateway-y ,y)
         (let ((,explorer (make-explorer ,x ,y ,gateway-x ,gateway-y ,tiles)))
           (multiple-value-bind (,waypoint-x ,waypoint-y ,steps)
               (explorer-explore-to-waypoint ,explorer)
             (when ,(cond
                     (directions
                      `(member (direction ,x ,y ,gateway-x ,gateway-y)
                               ,directions))
                     (directions-not
                      `(not (member (direction ,x ,y ,gateway-x ,gateway-y)
                                    ,directions-not)))
                     (t t))
               (conditional-let ((,waypoint-gateway-x
                                  ,waypoint-gateway-x
                                  (explorer-parent-x ,explorer))
                                 (,waypoint-gateway-y
                                  ,waypoint-gateway-y
                                  (explorer-parent-y ,explorer))
                                 (,distance
                                  ,distance
                                  (1+ ,steps)))
                 ,@body))))))))

(defun connected-waypoints (board x y)
  (with-collecting
    (do-connected-waypoints (board
                             distance
                             (waypoint-x x)
                             (waypoint-y y)
                             :gateway-x gateway-x
                             :gateway-y gateway-y)
      (collect (list (direction x y gateway-x gateway-y)
                     distance
                     (waypoint board waypoint-x waypoint-y))))))

;;;;;; Computation

(defun %compute-waypoint-graph (board starting-x starting-y)
  (let* ((start (make-vertex starting-x starting-y))
         (dimensions (array-dimensions (board-tiles board)))
         (open (list start))
         (visited (make-sparse-table dimensions nil)))
    (setf (stref visited starting-x starting-y) start)
    (while open
      (let* ((this (pop open))
             (this-x (vertex-x this))
             (this-y (vertex-y this)))
        (do-connected-waypoints (board
                                 distance
                                 (that-x this-x)
                                 (that-y this-y)
                                 :directions-not (map 'list #'edge-direction (vertex-edges this))
                                 :gateway-x gateway-x
                                 :gateway-y gateway-y
                                 :waypoint-gateway-x that-gateway-x
                                 :waypoint-gateway-y that-gateway-y)
          (let ((that (or (stref visited that-x that-y)
                          (setf (stref visited that-x that-y)
                                (let ((new (make-vertex that-x that-y)))
                                  (push new open)
                                  new)))))
            (vertex-link this that distance gateway-x gateway-y that-gateway-x that-gateway-y)))))
    visited))

;; Compute `waypoints' slot of the given `board' according to its
;; `tiles'.
(defun board-compute-waypoint-graph (board)
  (with-slots (tiles) board
    (dotimes (x (board-width board))
      (dotimes (y (board-width board))
        (when (waypointp tiles x y)
          (setf (board-waypoints board)
                (%compute-waypoint-graph board x y))
          (return-from board-compute-waypoint-graph))))))

;; For an arbitrarial tile (`a', `b') return the direction of an
;; adjacent tile other than (`x', `y').  Specifically, for any tile in
;; a corridor, return the opposite neighbor.
(defun corridor-other-adjacent (board a b x y)
  (do-neighbor-tiles (board-tiles board)
      (neighbor-x a)
      (neighbor-y b)
    (when (or (/= neighbor-x x)
              (/= neighbor-y y))
      (return (direction a b neighbor-x neighbor-y)))))

;;;;;; Corridors

;; A corridor is a non-empty set of connected tiles delimited by two
;; gateways.  There may be more than one corridor between 2 gateways,
;; but always exactly one edge (And its `complement').  For instance:
;;
;;   +-W-+
;;   | | |
;;   | | |
;;   +-W-+
;;
;; Corridor structure represent a corridor, based on the topmost
;; leftmost waypoint and the direction of the first tile.  For
;; instance, the corridor maked with double line would be represented
;; as a `corridor' whose `vertex' is `W', and `direction' is `:right':
;;
;;   -W===V-
;;    |   |
;;    +---+
;;
;; A single tile may be precisely identified in relation to the
;; waypoint graph by the corrider where it's located, and the distance
;; to one of the delimiting gateways.
;;
;; When there is more than one possible value for direction first one
;; in a clockwise order starting from `:left' takes precedence.  This
;; is necessary to make coherent the representation of loops.
(defstruct corridor
  (vertex nil :type vertex)
  ;; Vertex at the other end (May or may not be the same as `vertex').
  (opposite nil :type vertex)
  (direction nil :type direction))

(defun reference (vertex-a direction-a vertex-b direction-b)
  (with-slots ((a-x x) (a-y y)) vertex-a
    (with-slots ((b-x x) (b-y y)) vertex-b
      (or (topmost-leftmost-p a-x a-y
                              b-x b-y)
          (and (= a-x b-x)
               (= a-y b-y)
               (direction< direction-a direction-b))))))

;; Compute the corridor which contains (`x', `y').  Return the
;; corridor, the distance from the reference vertex in the reference
;; direction, the direction to the reference vertex and the direction to
;; the other vertex.
;;
;; This is computationally expensive.  Shouldn't be used in main loop.
(defun corridor (board x y)
  (assert (= 2 (tile-degree (board-tiles board) x y)))
  (let (stored-vertex
        stored-direction
        stored-gateway
        distance-to-stored-vertex)
    (do-connected-waypoints (board
                             distance
                             (vertex-x x)
                             (vertex-y y)
                             :gateway-x gateway-x
                             :gateway-y gateway-y
                             :waypoint-gateway-x waypoint-gateway-x
                             :waypoint-gateway-y waypoint-gateway-y)
      (let ((vertex (waypoint board vertex-x vertex-y))
            (direction (direction vertex-x vertex-y
                                  waypoint-gateway-x waypoint-gateway-y))
            (gateway (direction x y gateway-x gateway-y)))
        (cond
          ;; First iteration.
          ((not stored-vertex)
           (setf stored-vertex vertex)
           (setf stored-direction direction)
           (setf stored-gateway gateway)
           (setf distance-to-stored-vertex distance))
          ;; Last iteration.
          (t
           (return-from corridor
             (if (reference vertex direction stored-vertex stored-direction)
                 (values (make-corridor :vertex vertex
                                        :opposite stored-vertex
                                        :direction direction)
                         distance
                         gateway
                         stored-gateway)
                 (values (make-corridor :vertex stored-vertex
                                        :opposite vertex
                                        :direction stored-direction)
                         distance-to-stored-vertex
                         stored-gateway
                         gateway)))))))))

(defun corridor= (a b)
  (declare (corridor a b))
  (and (eq (corridor-vertex a) (corridor-vertex b))
       (eq (corridor-direction a) (corridor-direction b))))

;;;;;; Trees

;; Waypoint trees represent a spanning tree of the board non-wall
;; tiles.  However, only explicit information is kept internally for
;; waypoints.  Corridor parents are computed when need from connected
;; waypoints.

;; Waypoint trees are meant to be used in pathfinding.
(defstruct (waypoints-tree (:constructor %make-waypoints-tree))
  board
  center-x
  center-y
  ;; Corridor which contains the center, if applicable.
  corridor
  ;; Distance to the `corridor' reference vertex.
  location
  ;; A hash-table of directions and `nil'.  `nil' means the waypoint
  ;; is the center.
  parents
  ;; Distance from waypoints to center.
  distances)

(defun make-waypoints-tree (board center-x center-y)
  (let ((waypoints-tree
         (multiple-value-bind (parents distances)
             (board-compute-vertices-parents board center-x center-y)
           (%make-waypoints-tree
            :center-x center-x
            :center-y center-y
            :parents parents
            :distances distances))))
    (unless (waypoint board center-x center-y)
      (setf (values (waypoints-tree-corridor waypoints-tree)
                    (waypoints-tree-location waypoints-tree))
            (corridor *board* center-x center-y)))
    waypoints-tree))

(defun waypoints-tree-parent (tree waypoint)
  (gethash waypoint (waypoints-tree-parents tree)))

(defun waypoints-tree-distance (tree waypoint)
  (gethash waypoint (waypoints-tree-distances tree)))

;; Is `waypoint' a gateway to the center of `tree'?.  The result is
;; only meaningful if the center itself is on a corridor.
(defun waypoints-tree-gateway-p (tree waypoint)
  (with-slots (corridor) tree
    (or (eq waypoint (corridor-vertex corridor))
        (eq waypoint (corridor-opposite corridor)))))

(defun board-compute-vertices-parents (board x y &optional max-distance)
  (declare (board board)
           (ignore max-distance))
  (let ((tiles (board-tiles board))
        ;; Each item in the pending vertex heap (Used as a priority
        ;; queue) consists of a list.  In order: Tentative cost;
        ;; Tentative predecessor (An `edge' or `nil' for the
        ;; gateways); and finally the vertex itself.
        (pending (make-instance 'cl-heap:binary-heap :key #'first))
        (distances (make-hash-table :test #'eq))
        (predecessors (make-hash-table :test #'eq)))
    ;; Enqueue starting tiles.
    (aif (waypoint *board* x y)
         (cl-heap:add-to-heap pending (list 0 nil it))
         (do-connected-waypoints
             (board
              distance
              (neighbor-x x)
              (neighbor-y y)
              :waypoint-gateway-x gateway-x
              :waypoint-gateway-y gateway-y)
           (cl-heap:add-to-heap pending
                                (list distance
                                      (direction neighbor-x neighbor-y
                                                 gateway-x gateway-y)
                                      (waypoint board neighbor-x neighbor-y)))))
    ;; Perform a uniform cost serach.
    (loop until (cl-heap:is-empty-heap-p pending)
          for item = (cl-heap:pop-heap pending)
          for cost = (first item)
          for predecessor = (second item)
          for current = (third item)
          for x = (vertex-x current)
          for y = (vertex-y current)
          ;; Don't (re-)visit `current' if it has been alredy visited
          ;; by another, cheaper path.
          when (< cost (gethash current
                                distances
                                most-positive-fixnum))
          do (progn
               (setf (gethash current distances) cost)
               (setf (gethash current predecessors) predecessor)
               (dolist (edge (vertex-edges current))
                 (let* ((neighbor (edge-sink edge))
                        (edge-weight (edge-weight edge))
                        (total (+ cost edge-weight))
                        (direction (edge-direction (edge-complement edge))))
                   (when (< total
                            (gethash neighbor distances most-positive-fixnum))
                     (cl-heap:add-to-heap pending
                                          (list total direction neighbor)))))))
    (values predecessors distances)))

;;;;;; Trackers

;; These, given a starting position in the board and the subsequent
;; moves (Modifications) can tell where it is located in absolute (X
;; and Y) or relative (Relative to the waypoint graph) terms.

;; A position in the board, and hence a tracker, can only be in either
;; a waypoint or a corridor.  This is represented by `waypoint' and
;; `corridor' slots respectively.
(defstruct (tracker (:constructor %make-tracker))
  x
  y
  board
  ;; Which corridor is the `tracker' on, if any?.  `nil' otherwise.
  corridor
  ;; Local copy for simplicity and maybe efficiency.
  corridor-length
  ;; Direction to the vertex used as reference in corridor
  ;; description.
  reference-gateway
  ;; Direction to the other end of the corridor.
  opposite-gateway
  ;; Distance to the reference vertex by the reference end.
  location
  ;; Which tile is the `tracker' on, if any?.  `nil' otherwise.
  waypoint)

(defun make-tracker (board x y)
  (acond
    ((waypoint board x y)
     (%make-tracker :x x
                    :y y
                    :board board
                    :waypoint it))
    (t
     (multiple-value-bind (corridor location gateway opposite-gateway)
         (corridor board x y)
       (%make-tracker :x x
                      :y y
                      :board board
                      :corridor corridor
                      :corridor-length (edge-weight
                                        (vertex-edge-to
                                         (corridor-vertex corridor)
                                         (corridor-direction corridor)))
                      :location location
                      :reference-gateway gateway
                      :opposite-gateway opposite-gateway)))))

;; Move the `tracker' one tile in the give `direction'.

;; TODO: Add "collision" checking (Not every direction is valid from
;; every possition in every possible board).
(defun tracker-move (tracker direction)
  (with-slots (x
               y
               board
               corridor
               corridor-length
               location
               reference-gateway
               opposite-gateway
               waypoint)
      tracker
    (let (new-x new-y new-to-old)
      (flet ((new-to-adjacent ()
               (corridor-other-adjacent board new-x new-y x y)))
        (setf (values new-x new-y) (displace x y direction))
        (setf new-to-old (direction new-x new-y x y))
        (cond
          (waypoint
           (let ((edge (vertex-edge-to waypoint direction)))
             (cond
               ;; From waypoint to waypoint move.
               ((= 1 (edge-weight edge))
                (setf waypoint (edge-sink edge)))
               ;; From waypoint to corridor move.
               (t
                (setf corridor-length (edge-weight edge))
                (let ((direction (edge-direction edge))
                      (end (edge-sink edge))
                      (end-direction (edge-direction (edge-complement edge))))
                  (if (reference waypoint direction end end-direction)
                      (setf corridor (make-corridor :vertex waypoint
                                                    :opposite end
                                                    :direction direction)
                            location 1
                            reference-gateway new-to-old
                            opposite-gateway (new-to-adjacent))
                      (setf corridor (make-corridor :vertex end
                                                    :opposite waypoint
                                                    :direction end-direction)
                            location (1- corridor-length)
                            reference-gateway (new-to-adjacent)
                            opposite-gateway new-to-old)))
                (nilf waypoint)))))
          ;; Corridor to waypoint move.
          ((vertex-position-= (corridor-vertex corridor) new-x new-y)
           (setf waypoint (corridor-vertex corridor))
           (setf corridor nil)
           (setf location nil))
          ((vertex-position-= (corridor-opposite corridor) new-x new-y)
           (setf waypoint (corridor-opposite corridor))
           (setf corridor nil)
           (setf location nil))
          ;; Corridor to corridor move.
          ((eq direction reference-gateway)
           (decf location)
           (setf reference-gateway (new-to-adjacent))
           (setf opposite-gateway new-to-old))
          ((eq direction opposite-gateway)
           (incf location)
           (setf reference-gateway new-to-old)
           (setf opposite-gateway (new-to-adjacent)))
          (t
           (error "BUG: No specific case matched.")))))
    (displacef x y direction))
  (values))

(defun tracker-parent (tracker tree)
  (with-slots (x
               y
               waypoint
               corridor
               location
               reference-gateway
               opposite-gateway)
      tracker
    (cond
      ((with-slots (center-x center-y) tree
         (and (= x center-x) (= y center-y)))
       nil)
      (waypoint
       (waypoints-tree-parent tree waypoint))
      ((and (waypoints-tree-corridor tree)
            (corridor= corridor (waypoints-tree-corridor tree)))
       (if (> location (waypoints-tree-location tree))
           reference-gateway
           opposite-gateway))
      ;; `tracker' is on a corridor which don't contains `tree'.
      (t
       (let ((reference-vertex-distance
              (waypoints-tree-distance tree (corridor-vertex corridor)))
             (opposite-vertex-distance
              (waypoints-tree-distance tree (corridor-opposite corridor))))
         (if (< reference-vertex-distance opposite-vertex-distance)
             reference-gateway
             opposite-gateway))))))

;; Move `tracker' towards the center of `tree' and return the
;; direction it just moved to.  If `tracker' is alredy there, return
;; `nil' and don't move.
(defun tracker-climb-tree (tracker tree)
  (let ((parent (tracker-parent tracker tree)))
    (when parent (tracker-move tracker parent))
    parent))

;; Debugging.
(defun tracker-exhaust-moves (tracker tree climbp)
  (loop for direction = (if climbp
                            (tracker-parent tracker tree)
                            (tracker-branch tracker tree))
        while direction
        do (progn
             (with-slots (x y) tracker (format t "~d,~d: ~a~%" x y direction))
             (tracker-move tracker direction))))

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
    :accessor unit-x)
   (y
    :initarg :y
    :type fixnum
    :accessor unit-y)
   (speed
    :initarg :speed
    :type fixnum
    :initform 2
    :accessor unit-speed)
   (tracker
    :initarg :tracker
    :type tracker
    :accessor unit-tracker)))

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

(defmethod initialize-instance :after ((unit unit) &rest initargs &key board)
  (declare (ignore initargs))
  (with-slots (x y tracker) unit
    (with-unit-boundary (unit)
      (setf tracker (make-tracker board left top)))))

(defgeneric draw (unit))

;;;;; Generic movement

;; Move in the requested direction and return how much
;; pixels the unit  moved

(defgeneric move-unit (unit pixels direction)
  (declare (fixnums pixels)))

;; Called when the unit moves from one tile to another (Open to
;; interpretation).
(defgeneric unit-update-tile (unit old-x old-y x y)
  (declare (fixnum old-x old-y x y)))

;; Note: We can't corretly move more than one tile.
(defun %move-unit (unit pixels direction)
  (declare ;; (direction direction) and define direction type - MXCC
   (unit unit)
   (fixnum pixels))
  (with-unit-boundary (unit)
    (let ((board-width (board-width *board*))
          (board-height (board-height *board*))
          (r (/ *tile-size* 2)))
      (with-slots (x y tracker) unit
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

(defmethod unit-update-tile ((unit unit) old-x old-y x y)
  (with-slots (tracker) unit
    (tracker-move tracker (direction old-x old-y x y))
    (assert (and (= (tracker-x tracker) x)
                 (= (tracker-y tracker) y)))))

(defmethod move-unit ((unit unit) pixels direction)
  (with-slots (tracker) unit
    (with-unit-boundary (unit)
      (prog1 (%move-unit unit pixels direction)
        (with-unit-boundary (unit "NEW-")
          (when (or (/= new-left left) (/= new-top top))
            (unit-update-tile unit left top new-left new-top)))))))

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

(defun unit-climb-tree (unit max-pixels tree)
  ;; TODO: Accound for the case in which the unit is between two tiles
  ;; with the same distance to the tree's center, and the unit is
  ;; farther from the one in which its tracker is centered.
  (with-unit-boundary (unit)
    (let ((parent (tracker-parent (unit-tracker unit) tree)))
      (cond
        ((/= left right)
         (if (eq parent :right)
             (unit-align unit max-pixels :right)
             (unit-align unit max-pixels :left)))
        ((/= top bottom)
         (if (eq parent :down)
             (unit-align unit max-pixels :down)
             (unit-align unit max-pixels :up)))
        (t
         (move-unit unit max-pixels parent))))))

;;;;; Pacman

(defclass pacman (unit)
  (;; TODO: Implement pacman upon a surface, in order to we can use GFX
   ;; to rotation and more.
   ;; (surface ...)
   (direction
    :initarg :direction
    :type (member :up :down :left :right)
    :initform :right
    :accessor pacman-direction)))

(defmethod initialize-instance :after ((pacman pacman) &rest initargs)
  (declare (ignore initargs))
  (with-unit-boundary (pacman)
    (setf *pacman-wpt* (make-waypoints-tree *board* left top))))

(defmethod unit-update-tile :after ((pacman pacman) old-x old-y new-x new-y)
  (setf *pacman-wpt* (make-waypoints-tree *board* new-x new-y)))

(defmethod unit-act ((pacman pacman))
  (with-slots (x y speed direction) pacman
    (if (zerop (move-unit pacman speed *next-direction*))
        (unit-align pacman speed direction)
        (setf direction *next-direction*))))

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

(defmethod monster-make-vulnerable ((monster monster))
  (setf (monster-vulnerable-until monster)
        (+ (clock-ticks *clock*) *monster-vulnerable-ticks*)))

(defmethod unit-act ((monster monster))
  (when (= (monster-vulnerable-until monster) (clock-ticks *clock*))
    (tf (monster-livep monster)))
  (cond
    ;; Monster is in <<spirit>> form
    ((not (monster-livep monster))
     (let ((respawn-point (board-respawn *board*)))
       (unit-climb-tree monster (unit-speed monster) respawn-point)
       (with-unit-boundary (monster)
         ;; Did monster reached the respawn point?
         (when (and (= left (waypoints-tree-center-x respawn-point))
                    (= top (waypoints-tree-center-y respawn-point)))
           (setf (monster-vulnerable-until monster) (clock-ticks *clock*))
           (tf (monster-livep monster))))))
    ;; No contact with pacman
    ((<= *tile-size*
         (distance-* (unit-x monster) (unit-y monster)
                     (unit-x *pacman*) (unit-y *pacman*)))
     (unit-climb-tree monster (unit-speed monster) *pacman-wpt*))
    ;; Pacman contact with hostile monster
    ((monster-hostilep monster)
     ;; TODO: Put something more friendly here
     (error "Monster ate pacman"))
    ;; Pacman contact with vulnerable monster
    ((monster-vulnerablep monster)
     (nilf (monster-livep monster)))))

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
  (with-slots (surface) *board*
    (let ((width (board-width *board*))
          (height (board-height *board*))
          (waypoints (board-waypoints *board*)))
      (setf surface (create-surface (* *tile-size* width)
                                    (* *tile-size* height)))
      (dotimes (y height)
        (dotimes (x width)
          (let ((color (cond
                         ((tile *board* x y)
                          *red*)
                         (*print-waypoints*
                          (if (stref waypoints x y)
                              *white*
                              *black*))
                         (t
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
      (unit-act *pacman*)))
  (draw *pacman*))

;; Move the monsters and check colisions.
(defun update-monsters ()
  (dolist (monster *monsters*)
    (declare (monster monster))
    (unit-act monster)
    (draw monster)))

(defun draw-waypoints (waypoints)
  (dolist (waypoint waypoints)
    (let ((x (* *tile-size* (vertex-x waypoint)))
          (y (* *tile-size* (vertex-y waypoint))))
      (draw-box-* x y 5 5))))

(defun update ()
  (blit-surface (board-surface *board*))
  (incf (clock-ticks *clock*))
  (update-monsters)
  (update-pacman)
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
           (pacman-spawn (board-pacman-spawn *board*))
           (screen (window *width* (+ *height* 100) :title-caption "Lispac")))
      (setf (frame-rate) *fps*)
      (clear-display *black*)
      (initialise-default-font *font-10x20*)
      (setf *pacman*
            (make-instance 'pacman
                           :x (%tile-center (x pacman-spawn))
                           :y (%tile-center (y pacman-spawn))
                           :board *board*))
      (with-unit-boundary (*pacman*)
        (declare (ignore right bottom)))
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
