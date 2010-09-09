;;; utils.lisp - Generic macros & functions.

;; Copyrigth (C) 2009, 2010 Mario Castelán Castro <marioxcc>
;; Copyrigth (C) 2009, 2010 David Vázquez
;; Copyrigth (C) 2010 Kevin Mas Ruiz <sorancio>

;; This file is part of lispac.

;; with-gensyms and with-collecting are from cl-icalendar.

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

(defvar *orange* (color :r 255 :g 127 :b 0))

;;; Generate variable names based on `gensym'
(defmacro with-gensyms ((&rest vars) &body code)
  `(let ,(loop for var in vars
               collect (etypecase var
                         (symbol
                          `(,var (gensym ,(symbol-name var))))
                         (list
                          (let* ((name (first var))
                                 (value (second var))
                                 (hint (or (third var)
                                           (symbol-name (first var)))))
                            `(,name (or ,value (gensym ,hint)))))))
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

;; Bind `enqueue', `dequeue'. `peek-first' and `peek-last' to queue
;; handlers.
(defmacro with-queue ((&optional first-cons) &body body)
  (with-gensyms ((first-cons first-cons) last-cons)
    `(let (,first-cons ,last-cons)
       (declare (ignorable ,first-cons))
       (flet ((enqueue (item)
                (if ,first-cons         ; Read "if not void"
                    (let ((penultimate ,last-cons))
                      (setf ,last-cons (cons item nil))
                      (setf (cdr penultimate) ,last-cons))
                    ;; If queue is void initialize it
                    (multiple-setf (cons item nil) ,last-cons ,first-cons)))
              (dequeue ()
                (pop ,first-cons))
              (peek-first ()
                (car ,first-cons))
              (peek-last ()
                (car ,last-cons)))
         (declare (ignorable (function enqueue)
                             (function dequeue)
                             (function peek-first)
                             (function peek-last)))
         ,@body))))

(defmacro while (condition &body code)
  `(do ()
       ((not ,condition))
     ,@code))

;; `awhile' is to `while' as `aif' is to `if'.
(defmacro awhile (condition &body body)
  `(loop for it = ,condition
         while it
         do (progn ,@body)))

(defmacro dorange ((var min max &optional (step 1)) &body body)
  `(loop for ,var from ,min to ,max by ,step
         do (progn ,@body)))

;; Similar to `dolist' but uses `macrolet' so to enable the `body' to
;; modify list items.
(defmacro dolist* ((var list &optional (result nil)) &body body)
  (with-gensyms (cons)
    `(loop for ,cons on ,list
           do (symbol-macrolet ((,var (car ,cons)))
                ,@body)
           ,@(if result `(finally (return ,result))))))

;;; Compare 2 generalized booleans.
(defun boolean= (a b)
  (if a
      b
      (not b)))

(defun point= (a b)
  (declare ((vector integer) a b))
  (and (= (elt a 0) (elt b 0))
       (= (elt a 1) (elt b 1))))

(defun divisiblep (m n)
  (declare (integer m n))
  (zerop (mod m n)))

(defun read-bytes (stream n)
  (declare (fixnum n))
  (let ((buffer (make-array n)))
    (dotimes (i n)
      (setf (elt buffer i) (read-byte stream)))))

(defun parse-unsigned-integer (string)
  (loop with result = 0
        for i = 0 then (1+ i)
        while (/= i (length string))
        for char = (elt string i)
        for weight = (digit-char-p char)
        do (if weight
               (setf result (+ weight (* result 10)))
               (error "Found a non-digit: ~a" char))
        finally (return result)))

;;; Read a single byte and compare againsing the given value, return t
;;; if and only if they do *not* match.
(defun read-and-compare (stream value &optional (test #'eql))
  (not (funcall test value (read-byte stream))))

;;; Read from a binary stream and compare againsing a sequence of
;;; integers.  Return the index of the first non matching item if any
;;; or nil otherwise.
(defun read-and-compare-sequence (stream sequence &optional (test #'eql))
  (dotimes (i (length sequence))
    (unless (funcall test (elt sequence i) (read-byte stream))
      (return i))))


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

(defmacro define-dumb-printer (class)
  `(defmethod print-object ((object ,class) stream)
     (print-unreadable-object (object stream :type t))))

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

;; Like incf, but return delta
(defmacro incf* (place delta)
  (with-gensyms (delta-tmp)
    `(let ((,delta-tmp ,delta))
       (incf ,place ,delta-tmp)
       ,delta-tmp)))

;; Like decf, but return delta
(defmacro decf* (place delta)
  (with-gensyms (delta-tmp)
    `(let ((,delta-tmp ,delta))
       (decf ,place ,delta-tmp)
       ,delta-tmp)))

(defmacro multiple-setf (value &rest places)
  (with-gensyms (tmp)
    `(let ((,tmp ,value))
       ,@(loop for place in places
               collect `(setf ,place ,tmp)))))

(defmacro zerof (&rest places)
  `(multiple-setf 0 ,@places))

(defmacro nilf (&rest places)
  `(multiple-setf nil ,@places))

;; Set places to t.
(defmacro tf (&rest places)
  `(multiple-setf t ,@places))


;;; Portable bit map parser

;; ASCII for "P6"
(defconst +pbm-magic-number+ #(80 52))

(defun pnm-whitespace-p (byte)
  (declare ((unsigned-byte 8) byte))
  (or (= byte #x20)                     ; SP
      (= byte #x9)                      ; TAB
      (= byte #xA)                      ; LF
      (= byte #xD)                      ; CR
      ))

(defun read-pbm-header (stream)
  (flet ((read-token ()
           (with-output-to-string (result)
             (loop with comment-p = nil
                   for count = 0 then (1+ count)
                   for byte = (read-byte stream)
                   for char = (code-char byte)
                   do (cond
                        (comment-p
                         (if (or (char= #\newline char)
                                 (char= #\return char))
                             (setf comment-p nil)))

                        ((= byte 35)
                         (setf comment-p t))

                        ((pnm-whitespace-p byte)
                         (return))

                        (t
                         (write-char char result)))))))

    (unless (string= "P4" (read-token))
      (error "Magic number don't match"))

    (vector (parse-unsigned-integer (read-token))
            (parse-unsigned-integer (read-token)))))

;;; Read and iterate through the pixels of a portable bit map

;;; `pixel' (Non evaluated) is bound to the value of each individual
;;; pixel (1 for black).

;;; `byte' (Non evaluated) when giventh is bound to the byte where
;;; `pixel' is packed (8 pixels per byte, left to right, MSB to LSB).

;;; `dimensions' (Evaluated) is a #(width height).

;;; `x' and `y' (Non evaluated) when giventh are boud to the location
;;; of the current `pixel'.

;;; `at-row-end' forms, (Non evaluated) when giventh are evaluated
;;; after reading a whole row.

;;; `stream' (Evaluated) is the byte-stream to read the image from.
;;; Should be open before calling do-pbm-pixels and will remain open
;;; after calling do-pbm-pixels.

;;; `body' form (Non evaluated) are evaluated with `pixel',
;;; `byte'. `x' and `y' bound to their respective values, if
;;; requested.
(defmacro do-pbm-pixels ((pixel &optional byte)
                         (dimensions &optional x y &body at-row-end)
                         stream &body body)
  (with-gensyms ((byte byte)
                 (x x)
                 (y y)
                 dimensions-tmp
                 width
                 height)
    `(let* ((,dimensions-tmp ,dimensions)
            (,width  (elt ,dimensions-tmp 0))
            (,height (elt ,dimensions-tmp 1)))
       (dotimes (,y ,height)
         (let ((,byte 0))
           (declare ((unsigned-byte 8) ,byte))
           (dotimes (,x ,width)
             (when (divisiblep ,x 8)
               (setf ,byte (read-byte ,stream)))
             (let ((,pixel (logand 1 (ash ,byte (- (mod ,x 8) 7)))))
               (declare (bit ,pixel))
               ,@body)))
         ,@at-row-end))))

;;; Read a complete pixel
(defun read-and-print-pbm (stream)
  (do-pbm-pixels
      (pix byte)
      ((read-pbm-header stream) x y (write-line ""))
      stream
    (case pix
      (0 (write-char #\space))
      (1 (write-char #\#)))))

;;; utils.lisp ends here
