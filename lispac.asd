;; lispac.asd - ASDF system definition
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
;;

(defsystem :lispac
  :name "lispac"
  :license "GPLv3+"
  :depends-on (:lispbuilder-sdl)
  :components
  ((:static-file "COPYING")
   (:file "package")
   (:file "parameters"
          :depends-on ("package"))
   (:file "global"
          :depends-on ("package"))
   (:file "display"
          :depends-on ("global" "package" "parameters"))
   (:file "board"
          :depends-on ("display" "package"))
   (:file "lispac"
          :depends-on ("package" "parameters" "global" "display" "board"))))

;; lispac.asd ends here
