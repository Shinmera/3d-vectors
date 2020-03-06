#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(defun type-prefix (type)
  (ecase type
    (single-float '||)
    (double-float 'd)
    (ub32 'u)
    (sb32 'i)))

(deftype ub32 ()
  '(unsigned-byte 32))

(deftype sb32 ()
  '(signed-byte 32))

(define-template-type vec (<s> <t>)
    (compose-name NIL (type-prefix <t>) 'vec <s>)
  (loop for i from 0 below <s>
        for f in '(x y z w)
        do (field (compose-name NIL (type-prefix <t>) 'v f <s>)
                  :type <t>
                  :alias (list i f))))

(defmacro do-vec-types (macro (<s> <t>))
  `(progn
     ,@(loop for combination in (enumerate-combinations '(2 3 4) '(single-float double-float ub32 sb32))
             collect `(,macro ,@combination))))

(do-vec-types define-vec)
