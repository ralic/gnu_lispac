;; types.lisp
;;
;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>
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

(in-package lispac)

;; tile edges mode
(defconstant +no-connection+ 0)
(defconstant +horitzontal-connection+ 1)
(defconstant +vertical-connection+ 2)
(defconstant +full-connection+ 3) ; (b-or 1 2)
;; Wall surfaces
(defconstant +top-wall-surface+ nil)
(defconstant +left-wall-surface+ nil)
(defconstant +bottom-wall-surface+ nil)
(defconstant +right-wall-surface+ nil)
; Point surfaces
(defconstant +bronze-coin-surface+ nil)
(defconstant +silver-coin-surface+ nil)
(defconstant +golden-coin-surface+ nil)
(defconstant +cherry-surface+ nil)
(defconstant +banana-surface+ nil)
(defconstant +melon-surface+ nil)
(defconstant +watermelon-surface+ nil)
(defconstant +golden-cherry-surface+ nil)
(defconstant +bit-0-surface+ nil)
(defconstant +bit-1-surface+ nil)
(defconstant +diamond-surface+ nil)
;; Player surfaces
(defconstant +player-left-surface+ nil)
(defconstant +player-left-moving-surface+ nil)
(defconstant +player-up-surface+ nil)
(defconstant +player-up-moving-surface+ nil)
(defconstant +player-right-surface+ nil)
(defconstant +player-right-moving-surface+ nil)
(defconstant +player-down-surface+ nil)
(defconstant +player-down-moving-surface+ nil)
(defconstant +player-life-surface+ nil)
;; Weak state monster surfaces
(defconstant +weak-monster-left-surface+ nil)
(defconstant +weak-monster-left-moving-surface+ nil)
(defconstant +weak-monster-up-surface+ nil)
(defconstant +weak-monster-up-moving-surface+ nil)
(defconstant +weak-monster-right-surface+ nil)
(defconstant +weak-monster-right-moving-surface+ nil)
(defconstant +weak-monster-down-surface+ nil)
(defconstant +weak-monster-down-moving-surface+ nil)
;; Normal state monster surfaces
(defconstant +monster-left-surface+ nil)
(defconstant +monster-left-moving-surface+ nil)
(defconstant +monster-up-surface+ nil)
(defconstant +monster-up-moving-surface+ nil)
(defconstant +monster-right-surface+ nil)
(defconstant +monster-right-moving-surface+ nil)
(defconstant +monster-down-surface+ nil)
(defconstant +monster-down-moving-surface+ nil)
;; Angry state monster surfaces
(defconstant +angry-monster-left-surface+ nil)
(defconstant +angry-monster-left-moving-surface+ nil)
(defconstant +angry-monster-up-surface+ nil)
(defconstant +angry-monster-up-moving-surface+ nil)
(defconstant +angry-monster-right-surface+ nil)
(defconstant +angry-monster-right-moving-surface+ nil)
(defconstant +angry-monster-down-surface+ nil)
(defconstant +angry-monster-down-moving-surface+ nil)
;; Other surfaces
(defconstant +life-surface+ nil)
;; Point value constants
(defconstant +bronze-coin+ 5)
(defconstant +silver-coin+ 10)
(defconstant +golden-coin+ 20)
(defconstant +cherry+ 40)
(defconstant +banana+ 80)
(defconstant +melon+ 160)
(defconstant +watermelon+ 320)
(defconstant +golden-cherry+ 640)
(defconstant +bit-0+ 1280)
(defconstant +bit-1+ 2560)
(defconstant +diamond+ 5120)

(defclass position ()
  ((x :accessor x :initform 0)
   (y :accessor y :initform 0)))

(defclass object ()
  ((position :accessor position :initform (make-instance 'position))))

(defclass tile (object)
  ((hooks :accessor hooks :initform ())
   (paint :accessor paint :initform nil)
   (connections :accessor connections :initform +no-connection+)))

(defclass creature (object)
  ((direction :accessor direction :initform :left)))

(defclass player (creature)
  ((lifes :accessor lifes :initform 1)
   (points :accessor points :initform 0)))

(defclass monster (creature)
  ((state :accessor state :initform :normal)))
