;;;; sparse-arrays.lisp - Dumb implementation of sparse tables.

;;;; License

;;; Copyrigth (C) 2010 Mario Castelan Castro <marioxcc>
;;; Copyright (C) 2010 Kevin Mas Ruiz

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
             (:constructor %make-sparse-table 
                           (hash-table dimensions default-element)))
  hash-table
  dimensions
  default-element)

(defun make-sparse-table (dimensions default-element)
  (%make-sparse-table (make-hash-table) dimensions default-element))

(defun make-sparse-table-key (x y)
  (+ (ash y (integer-length y)) x))

(defun sparse-table-size (table)
  (list 
   (hash-table-size (sparse-table-hash-table table))
   (sparse-table-dimensions table)))

(defun sparse-table-optimize-p (table)
  (>= (hash-table-size (sparse-table-hash-table table))
      (sparse-table-dimensions table) 2))

(defun sparse-table-optimize (table)
  (when (sparse-table-optimize-p table)
    (hash-table-rehash-size (sparse-table-hash-table table))))

(defun stref (table x y)
  (let ((val (gethash 
              (make-sparse-table-key x y)
              (sparse-table-hash-table table))))
    (if (null val)
        (sparse-table-default-element table)
        val)))

(defun set-stref (table x y value)
  (if (null value)
      (remhash 
       (make-sparse-table-key x y) 
       (sparse-table-hash-table table))
      (setf (gethash 
             (make-sparse-table-key x y) 
             (sparse-table-hash-table table)) value)))

(defsetf stref set-stref)
 

;; Local Variables:
;; mode: Lisp
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: us-ascii-unix
;; End:

;;; lispac.lisp ends here
