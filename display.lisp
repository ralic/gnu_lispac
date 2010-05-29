;; display.lisp
;;
;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>
;;
;; This file is part of lispac.
;;
;; lispac is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; lispac is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with lispac.  If not, see <http://www.gnu.org/licenses/>.

(in-package :lispac)

(defun in-screen-p (map-x map-y)
  (not (or (< map-x *camera-start-x*) 
           (> map-x *camera-end-x)
           (< map-y *camera-start-y*) 
           (> map-y *camera-end-y*))))

(defun distance-to-end ()
  (let (x y)
    (setq x (- (width SDL:*DEFAULT-SURFACE*) *camera-end-x*))
    (setq y (- (height SDL:*DEFAULT-SURFACE*) *camera-end-y*))
    (cons x y)))

(defun camera-move (x-relative y-relative)
  (let ((margins (distance-to-end)))
    (if (> (+ x-relative *camera-end-x*) (car margins))
        (setf (car margins) (- (+ x-relative *camera-end-x*) (car margins)))
      (setf (car margins) (x-relative)))
    (if (> (+ y-relative *camera-end-y*) (cdr margins))
        (setf (cdr margins) (- (+ y-relative *camera-end-y*) (cdr margins)))
      (setf (cdr margins) (y-relative)))
    (incf *camera-start-x* x-relative)
    (incf *camera-end-x* x-relative)
    (incf *camera-start-y* y-relative)
    (incf *camera-end-y* y-relative)))

(defun default-print (x y)
  (if (in-screen-p x y)
      (draw-surface-at-* +unknown-surface+ 
                         (* x +sprite-width+) 
                         (* y +sprite-height+))
    nil))

(defclass printable-object ()
  ((pfun :accessor print-function :initform 'default-print)))
