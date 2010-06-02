;; engine.lisp
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

(defclass printable-object ()
  ((printer :accessor printer
            :type function)))

(defclass board ()
  ((tiles :accessor board-tiles-array
          :type (array tile))))

(defclass tile (printable-object)
  ((type :accessor tile-type)))

(defclass hook ()
  ((function :accessor function
             :type function)
   (action :accessor action)
   (target :accessor target)))

(defclass creature-sprites ()
  ((up :accessor up)
   (movement-up :accessor movement-up)
   (left :accessor left)
   (movement-left :accessor movement-left)
   (down :accessor down)
   (movement-down :accessor movement-down)
   (right :accessor right)
   (movement-right :accessor movement-right)))

(defclass monster-prototype ()
  ((sprites :accessor sprites)
   (points :accessor points
           :type integer)
   (delay :accessor delay
          :type integer)))

(defclass monster ()
  ((state :accessor state)
   (point :accessor point)
   (prototype :accessor prototype)
   (hooks :accessor hooks
          :type list)))

(defclass player ()
  ((sprites :accessor sprites)
   (lifes :accessor lifes
          :type integer)
   (score :accessor score
          :type integer)
   (point :accessor point
          :type integer)
   (hooks :accessor hooks
          :type list)))

;;; Constructors & accessors

(defun make-board (width height &optional tile)
  (let ((tiles (make-array (list width height)
                           :initial-element tile
                           :element-type '(or tile (member nil)))))
    (make-instance 'board
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

(defmethod add-hook ((monster monster) &key (action :view) function target)
  (declare (function function))
  (push (hooks monster) (make-instance 'hook :action action
                                       :function function
                                       :target target)))

(defmethod add-hook ((player player) &key (action :view) function target)
  (declare (function function))
  (push (hooks player) (make-instance 'hook :action action
                                       :function function
                                       :target target)))
;; engine.lisp ends here
