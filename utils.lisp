;;; utils.lisp - Generic macros & functions.

;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>

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

(in-package :lispac)


;;; Definitions & declarations

(defmacro defun* (name (&rest params) &body body)
  (let (declarations real-params)
    (loop for param in params
       do (unless (or (eq 'param '&rest)
                      (eq 'param '&key)
                      (eq 'param '&optional))
            (etypecase param
              (list (progn
                      (push `(declare (type ,(car param) ,@(cdr param))) 
                            declarations)
                      (loop for each-param in (cdr param)
                         do (push each-param real-params))))
              (symbol (push param real-params)))))
    `(defun ,name ,(reverse real-params)
       ,@declarations
       ,@body)))


(defmacro defcolor (symbol &key r g b)
  `(defconstant ,symbol ,(color :r r :g g :b b)))

(defmacro defcolor* (symbol &key r g b a)
  `(defconstant ,symbol ,(color :r r :g g :b b :a a)))

(defmacro defsurface (symbol width height &key (bpp 32) (alpha 255) x y)
  `(defvar ,symbol ,(create-surface width height :alpha t
                                    :bpp bpp 
                                    :pixel-alpha alpha
                                    :x x :y y)))


;;; Assignators

(defun* zerof (&rest (symbol vars))
  (loop for j in vars
       do (setf j 0)))

(defun* nilf (&rest (symbol vars))
  (loop for j in vars
       do (setf j nil)))


;;; utils.lisp ends here
