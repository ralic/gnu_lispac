;; board.lisp - Map & terrain base.
;;
;; Copyright (C) 2010  Mario Castelan Castro <marioxcc>
;;
;; This file is part of Lispac.
;;
;; Lispac is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Lispac is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Lispac.  If not, see <http://www.gnu.org/licenses/>.

(in-package :lispac)

(defclass board ()
  ((tiles :accessor board-tiles-array
          :type (array tile))))

(defclass tile (printable-object)
  ((type :accessor tile-type)))

;;;; Constructors & accessors

(defun make-map (width height &optional tile)
  (let ((tiles (make-array (list width height)
                           :initial-element tile
                           :element-type tile)))
    (make-instance 'map
                   :tiles tiles)))

(define-inline-method tile-at ((board board) x y)
  (declare (integer x y))
  (aref (board-tiles-array board) x y))

(defmethod board-dimensions ((board board))
  (array-dimensions (board-tiles-array board)))

(defmethod board-width ((board board))
  (array-dimension (board-tiles-array board) 0))

(defmethod board-height ((board board))
  (declare (board board))
  (array-dimension (board-tiles-array board) 1))

;; map.lisp ends here
