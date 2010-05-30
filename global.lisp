;; global.lisp - Functions and macros than don't fit anywhere else.
;;
;; Copyright (C) 2010  Mario Castelan Castro <marioxcc>
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

(defclass point ()
  ((x :accessor x
      :type integer)
   (y :accessor y
      :type integer)))

(defmethod point= (p1 p2)
  (declare (point p1 p2))
  (and (= (x p1) (x p2))
       (= (y p1) (y p2))))

(defun square-contains-p (start end point)
  (declare (point start end point))
  (and (<= (x start) (x point) (x end))
       (<= (x start) (y point) (y end))))

(defun square-contains-p* (top-left width height point)
  (declare (point top-left point))
  (declare (integer width height))
  (with-slots ((left x) (top y)) top-left
    (and (<= left (x point) (+ left width))
         (<= top (y point) (+ top height)))))

;; global.lisp ends here
