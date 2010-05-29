;; utils.lisp - Functions and macros than don't fit anywhere else.
;;
;; Copyright (C) 2010  Mario Castelan Castro
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

(defmacro defconst (name value)
  `(unless (boundp (quote ,name))
     (defconstant ,name ,value)))

;; utils.lisp ends here
