;; display.lisp
;;
;; Copyrigth (C) 2010  Kevin Mas Ruiz <sorancio>
;; Copyright (C) 2010  Mario Castelan Castro <marioxcc>
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

;; Start of help functions
(defun on-camera-p (point)
  (declare (point point))
  (and (<= *camera-x* (x point) (+ *camera-x* width))
       (<= *camera-y* (y point) (+ *camera-y* height))))

(defun on-camera-p* (x y)
  (declare (integer x y))
  (and (<= *camera-x* x (+ *camera-x* width))
       (<= *camera-y* y (+ *camera-y* height))))

;; End of help functions

;; The reason of a default-printer function is that undefined printing 
;; behavior can call this function and prevent fatal errors on sdl.
(defun default-printer (object x y)
  (declare (integer x y))
  (declare (printable-object object))
  (if (on-camera-p* x y)
      (sdl:draw-surface-at-* +unknown-surface+ x y)))

(defclass printable-object ()
  ((printer :accessor printer
            :type function :initform 'default-printer)))

;; display.lisp ends here
