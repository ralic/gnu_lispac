;;;; sparse-arrays.lisp - Dumb implementation of sparse tables.

;;;; License

;;; Copyrigth (C) 2010 Mario Castelan Castro <marioxcc>

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


;;; Sparse arrays

(defstruct (sparse-table
             (:constructor %make-sparse-table (array dimensions)))
  array
  dimensions)

(defun make-sparse-table (dimensions default-element)
  (%make-sparse-table (make-array dimensions :initial-element default-element)
                      dimensions))

(defun stref (table x y)
  (aref (sparse-table-array table) x y))

(defun set-stref (table x y value)
  (setf (aref (sparse-table-array table) x y) value))

(defsetf stref set-stref)


;; Local Variables:
;; mode: Lisp
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: us-ascii-unix
;; End:

;;; lispac.lisp ends here
