;; display.lisp
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

(in-package :lispac)

(defmacro defsurface (symbol path)
  `(defconst ,symbol (sdl:load-image ,(concatenate 'string "inc/" path))))

;; Sprite size
(defconst +sprite-height+ 32)
(defconst +sprite-width+ 32)
(defconst +sprite-size+ (* +sprite-height+ +sprite-width+))
(defconst +pixel-size+ 32) ; Truecolor + 8A (in bits)
(defparameter *camera-start-x* 0)
(defparameter *camera-start-y* 0)
;; Wall surfaces
(defsurface +top-wall-surface+          "walls/top.bmp")
(defsurface +left-wall-surface+         "walls/left.bmp")
(defsurface +bottom-wall-surface+       "walls/bottom.bmp")
(defsurface +right-wall-surface+        "walls/right.bmp")
(defsurface +top-left-wall-surface+     "walls/top-left.bmp")
(defsurface +top-right-wall-surface+    "walls/top-right.bmp")
(defsurface +bottom-left-wall-surface+  "walls/bottom-left.bmp")
(defsurface +bottom-right-wall-surface+ "walls/bottom-right.bmp")
; Point surfaces
(defsurface +bronze-coin-surface+       "points/bronze-coin.bmp")
(defsurface +silver-coin-surface+       "points/silver-coin.bmp")
(defsurface +golden-coin-surface+       "points/golden-coin.bmp")
(defsurface +cherry-surface+            "points/cherry.bmp")
(defsurface +banana-surface+            "points/banana.bmp")
(defsurface +melon-surface+             "points/melon.bmp")
(defsurface +watermelon-surface+        "points/watermelon.bmp")
(defsurface +golden-cherry-surface+     "points/golden-cherry.bmp")
(defsurface +bit-0-surface+             "points/bit-0.bmp")
(defsurface +bit-1-surface+             "points/bit-1.bmp")
(defsurface +diamond-surface+           "points/diamond.bmp")
;; Player surfaces
(defsurface +player-left-surface+         "player/left.bmp")
(defsurface +player-left-moving-surface+  "player/left-moving.bmp")
(defsurface +player-up-surface+           "player/up.bmp")
(defsurface +player-up-moving-surface+    "player/up-moving.bmp")
(defsurface +player-down-surface+         "player/down.bmp")
(defsurface +player-down-moving-surface+  "player/down-moving.bmp")
(defsurface +player-right-surface+        "player/right.bmp")
(defsurface +player-right-moving-surface+ "player/right-moving.bmp")
(defsurface +player-life-surface+         "player/life.bmp")
;; Weak state monster surfaces
(defsurface +weak-monster-left-surface+         "monster/weak/left.bmp")
(defsurface +weak-monster-left-moving-surface+  "monster/weak/left-moving.bmp")
(defsurface +weak-monster-up-surface+           "monster/weak/up.bmp")
(defsurface +weak-monster-up-moving-surface+    "monster/weak/up-moving.bmp")
(defsurface +weak-monster-down-surface+         "monster/weak/down.bmp")
(defsurface +weak-monster-down-moving-surface+  "monster/weak/down-moving.bmp")
(defsurface +weak-monster-right-surface+        "monster/weak/right.bmp")
(defsurface +weak-monster-right-moving-surface+ "monster/weak/right-moving.bmp")
;; Normal state monster surfaces
(defsurface +monster-left-surface+         "monster/normal/left.bmp")
(defsurface +monster-left-moving-surface+  "monster/normal/left-moving.bmp")
(defsurface +monster-up-surface+           "monster/normal/up.bmp")
(defsurface +monster-up-moving-surface+    "monster/normal/up-moving.bmp")
(defsurface +monster-down-surface+         "monster/normal/down.bmp")
(defsurface +monster-down-moving-surface+  "monster/normal/down-moving.bmp")
(defsurface +monster-right-surface+        "monster/normal/right.bmp")
(defsurface +monster-right-moving-surface+ "monster/normal/right-moving.bmp")
;; Angry state monster surfaces
(defsurface +angry-monster-left-surface+        "monster/angry/left.bmp")
(defsurface +angry-monster-left-moving-surface+ "monster/angry/left-moving.bmp")
(defsurface +angry-monster-up-surface+          "monster/angry/up.bmp")
(defsurface +angry-monster-up-moving-surface+   "monster/angry/up-moving.bmp")
(defsurface +angry-monster-down-surface+        "monster/angry/down.bmp")
(defsurface +angry-monster-down-moving-surface+ "monster/angry/down-moving.bmp")
(defsurface +angry-monster-right-surface+       "monster/angry/right.bmp")
(defsurface +angry-monster-right-moving-surface+ 
  "monster/angry/right-moving.bmp")
;; Other surfaces
(defsurface +life-object-surface+ "points/life.bmp")
(defsurface +unknown-surface+ "unknown.bmp")

;; parameteres.lisp ends here
