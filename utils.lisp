;;; utils.lisp - Generic macros & functions.

;; Copyrigth (C) 2009, 2010 Mario Castelán Castro <marioxcc>
;; Copyrigth (C) 2009, 2010 David Vázquez
;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>

;; This file is part of lispac.

;; with-gensyms and with-collecting are from 

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


;;; Misc

(defmacro with-gensyms ((&rest vars) &body code)
  `(let ,(loop for i in vars
	       collect (etypecase i
			 (symbol `(,i (gensym ,(symbol-name i))))
			 (list `(,(first i) (gensym ,(second i))))))
     ,@code))

(defmacro with-collecting (&body code)
  (with-gensyms (collected tail)
    `(let* ((,collected (list '#:collect))
            (,tail ,collected))
       (flet ((collect (x)
                (setf (cdr ,tail) (list x))
                (setf ,tail (cdr ,tail))))
         ,@code)
       (cdr ,collected))))

(defun divisiblep (m n)
  (declare (integer m n))
  (zerop (mod m n)))


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

(defmacro defconst (name value)
  `(unless (boundp (quote ,name))
     (defconstant ,name ,value)))

(defmacro defcolor (name r g b &optional a)
  (if a
      `(defconst ,name (color :r ,r :g ,g :b ,b :a ,a))
      `(defconst ,name (color :r ,r :g ,g :b ,b))))

(defmacro defsurface (symbol width height &key (bpp 32) (alpha 255) x y)
  `(defvar ,symbol ,(create-surface width height :alpha t
                                    :bpp bpp 
                                    :pixel-alpha alpha
                                    :x x :y y)))


;;; Assignators

(defmacro multiple-setf (value &rest places)
  (with-gensyms (tmp)
    `(let ((,tmp ,value))
       ,@(loop for place in places
               collect `(setf ,place ,tmp)))))

(defmacro zerof (&rest places)
  `(multiple-setf 0 ,@places))

(defmacro nilf (&rest places)
  `(multiple-setf nil ,@places))


;;; utils.lisp ends here
