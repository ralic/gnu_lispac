(in-package lispac)

(defmacro b-or (&rest bits)
  `(logior ,@bits))

(defmacro b-and (&rest bits)
  `(logand ,@bits))

;; tile edges mode
(defconstant +no-connection+ 0)
(defconstant +horitzontal-connection+ 1)
(defconstant +vertical-connection+ 2)
(defconstant +full-connection+ 3) ; (b-or 1 2)

;; Surface constants (they are shared and read-only)
(defconstant +top-wall-surface+ nil)
(defconstant +left-wall-surface+ nil)
(defconstant +bottom-wall-surface+ nil)
(defconstant +right-wall-surface+ nil)
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
(defconstant +player-left-surface+ nil)
(defconstant +player-left-moving-surface+ nil)
(defconstant +player-up-surface+ nil)
(defconstant +player-up-moving-surface+ nil)
(defconstant +player-right-surface+ nil)
(defconstant +player-right-moving-surface+ nil)
(defconstant +player-down-surface+ nil)
(defconstant +player-down-moving-surface+ nil)
(defconstant +player-life-surface+ nil)

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

;; Monster state
(defconstant +weak+ 1) ;; Can be killed and moves slower
(defconstant +normal+ 2) ;; Normal state, can't be killed
(defconstant +angry+ 3) ;; Can't be killed and moves faster

(defstruct position 
  x 
  y)

(defclass tile () ;; Tiles should inherits object?
  ((surface :accessor set-surface :reader get-surface :initform nil)
   (position :accessor set-position :reader get-position 
             :initform (make-position :x 0 :y 0))
   (flags :accessor set-flags :reader get-flags :initform +no-connection+)))

(defclass object ()
  (position :accessor set-position :reader get-position))

(defclass life (object)
  (count :accessor set-count :reader get-count :initform 1))

(defclass point (object)
  (type :accessor set-type :reader get-type :initform +bronze-coin+))

(defclass player (object)
  (points :accessor set-points :reader get-points :initform 0)
  (lifes :accesor set-lifes :reader get-lifes :initform 1))

(defclass monster (object)
  (state :accessor get-state :reader set-state :initform +normal+))

(defmethod get-object-surface)