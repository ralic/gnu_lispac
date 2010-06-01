;; global.lisp - Functions and macros than don't fit anywhere else.
;;
;; Copyrigth (C) 2009, 2010  David Vázquez
;; Copyrigth (C) 2009, 2010  Mario Castelán Castro <marioxcc>
;; Copyright (C) 2010  Kevin Mas Ruiz <sorancio>
;;
;; This file is part of lispac.  Some macros are from cl-icalendar.
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


;;; Definitions

(defmacro defsurface (symbol path)
  `(defconst ,symbol (sdl:load-image ,(concatenate 'string "inc/" path))))
  
(defmacro defconst (name value)
  `(unless (boundp (quote ,name))
     (defconstant ,name ,value)))

;;; Like `defun' but declare the function as inline.
(defmacro definline (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))

(defmacro define-inline-method (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defmethod ,name ,args ,@body)))

(defmacro with-gensyms ((&rest vars) &body code)
  `(let ,(loop for i in vars
	       collect (etypecase i
			 (symbol `(,i (gensym ,(symbol-name i))))
			 (list `(,(first i) (gensym ,(second i))))))
     ,@code))

(defmacro compare-slots (a b (&rest slots) &optional (test 'eql))
  (with-gensyms (a-tmp b-tmp)
    `(let ((,a-tmp ,a)
           (,b-tmp ,b))
       (and ,@(loop for slot in slots
                    collect `(,test (slot-value ,a-tmp ',slot)
                                    (slot-value ,b-tmp ',slot)))))))

(defmacro dotimes* ((var from times) &body body)
  (with-gensyms (from-var)
    `(let ((,from-var ,from))
       (loop for ,var from ,from-var to (+ ,from-var ,times)
             do (progn ,@body)))))


;;; Geometry

(defclass point ()
  ((x :accessor x
      :type fixnum)
   (y :accessor y
      :type fixnum)))

(defclass rectangle ()
  ((left :accessor left
         :type fixnum
         :initarg :left)
   (top :accessor top
        :type fixnum
        :initarg :top)
   (right :accessor right
          :type fixnum
          :initarg :right)
   (bottom :accessor bottom
           :type fixnum
           :initarg :bottom)))

(defgeneric containsp (container object))
(defgeneric intersectsp (a b))

(defun make-point (x y)
  (declare (fixnum x y))
  (make-instance 'point
                 :x x
                 :y y))

(defun make-rectangle (left top width height)
  (declare (fixnum left top width height))
  (make-instance 'rectangle
                 :left left
                 :right (the fixnum (+ left width))
                 :top top
                 :bottom (the fixnum (+ top height))))

(defun make-rectangle-* (left top right bottom)
  (declare (fixnum left right top bottom))
  (make-instance 'rectangle
                 :left left
                 :right right
                 :top top
                 :bottom bottom))

(defun point= (a b)
  (declare (point a b))
  (compare-slots a b (x y) =))

(defun rectangle= (a b)
  (declare (rectangle a b))
  (compare-slots a b (left right top bottom) =))

;; TODO: test and document this funciton
(defmethod intersectsp ((a rectangle) (b rectangle))
  ;; given 2 ranges, described by boundaries, is their intersection
  ;; the void set?
  (flet ((range-intersects-p (a-min a-max b-min b-max)
           (<= (max a-min b-min) (min a-max b-max))))
    (and (range-intersects-p (left a) (right a) (left b) (right b))
         (range-intersects-p (top a) (bottom a) (top b) (bottom b)))))

(defmethod containsp ((container rectangle) (object rectangle))
  (with-slots ((container-left left)
               (container-right right)
               (container-top top)
               (container-bottom bottom))
      container
    (with-slots (left right top bottom) object
      (and (<= container-left left right container-right)
           (<= container-top top bottom container-bottom)))))
  
(defmethod containps ((container rectangle) (object point))
  (with-slots ((container-left left)
               (container-right right)
               (container-top top)
               (container-bottom bottom))
      container
    (with-slots (x y) object
      (and (<= container-left x container-right)
           (<= container-top y container-bottom)))))

(defun rectangle-contains-p* (top-left width height point)
  (declare (point top-left point))
  (declare (integer width height))
  (with-slots ((left x) (top y)) top-left
    (and (<= left (x point) (+ left width))
         (<= top (y point) (+ top height)))))

;; global.lisp ends here
