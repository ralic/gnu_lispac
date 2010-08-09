#!sbcl --script
;;; start.lisp - Launch lispac

;; Copyrigth (C) 2010 Mario Castelan Castro <marioxcc>

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

(require 'asdf)
(asdf:load-system "lispac")
(asdf:load-system "string-case")

(in-package :lispac)

(defvar *monster-count* 3)

(defmacro do-opt ((option-list option value) &body body)
  (with-gensyms (option-list-var)
    `(loop for ,option-list-var = (cdr ,option-list)
                                then (cddr ,option-list-var)
           while ,option-list-var
           for ,option = (first ,option-list-var)
           for ,value = (second ,option-list-var)
           do (progn ,@body))))

(do-opt (sb-ext:*posix-argv* opt val)
  (string-case:string-case (opt)
    ("-board"
     (setf *board* (load-board-from-pbm-file val)))
    ("-tile-size"
     (setf *tile-size* (parse-unsigned-integer val)))
    ("-monster-vulnerable-seconds"
     (setf *monster-vulnerable-ticks* (* *fps* (parse-unsigned-integer val))))
    ("-respawn"
     (let* ((respawn-center (split-string val #(#\.)))
            (x (parse-integer (first respawn-center)))
            (y (parse-integer (second respawn-center))))
       (board-update-respawn-gradient *board* x y)))
    (t (format t "Unvalid option: ~s~%" opt))))

(setf *pacman-gradient-max-distance* 10)
(setf *monsters* (list (make-instance 'monster
                                      :x 12
                                      :y 12
                                      :livep nil
                                      :controller #'spirit-controller)))

(run-and-wait)

;;; start.lisp ends here
