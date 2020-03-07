#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(deftype f32 ()
  'single-float)

(deftype f64 ()
  'double-float)

(deftype u32 ()
  '(unsigned-byte 32))

(deftype i32 ()
  '(signed-byte 32))

(defun type-prefix (type)
  (ecase type
    (f32 '||)
    (f64 'd)
    (u32 'u)
    (i32 'i)))

(define-template-type vec (<s> <t>)
    (compose-name NIL (type-prefix <t>) 'vec <s>)
  (loop for i from 0 below <s>
        for f in '(x y z w)
        do (field (compose-name NIL (type-prefix <t>) 'v f <s>)
                  :type <t>
                  :alias (list i f))))

(defmacro do-vec-combinations (template &rest other-template-args)
  `(do-combinations ,template ,@other-template-args (2 3 4) (f32 f64 u32 i32)))

(do-vec-combinations define-vec)
