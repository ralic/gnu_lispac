;; utils.lisp - Functions and macros than don't fit anywhere else.
;;
;; Copyright (C) 2010  Mario Castelan Castro
;; Copyright (C) 2010  Kevin Mas Ruiz <sorancio>
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

(defmacro defsurface (symbol path)
  `(defconst ,symbol (sdl:load-image ,(concatenate 'string "inc/" path))))
  
(defmacro defconst (name value)
  `(unless (boundp (quote ,name))
     (defconstant ,name ,value)))

(defclass position ()
  ((x :accessor x :initform 0)
   (y :accessor y :initform 0)))

(defun pos= (pos1 pos2)
  (and (= (x pos1) (x pos2))
       (= (y pos1) (y pos2))))

(defun in-range-p (start end point)
  (and (>= (x point) (x start))
       (<= (x point) (x end))
       (>= (y point) (y start))
       (<= (y point) (y end))))

(defun in-range-p* (start width height point)
  (let ((end (make-instance 'position 
                            :x (+ (x start) width)
                            :y (+ (y start) height))))
    (is-in-range start end point)))

;; utils.lisp ends here
