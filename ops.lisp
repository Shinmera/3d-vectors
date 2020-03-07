#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(define-template 2vecop <op> <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(name type) a b))
      (,(constructor type)
       ,@(loop for i from 0 below <s>
               collect `(,<op> (,(place type i) a)
                               (,(place type i) b)))))))

(do-vec-combinations define-2vecop (+ - * /))

(define-template 2nvecop <op> <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(name type) a b))
      (setf ,@(loop for i from 0 below <s>
                    collect `(,(place type i) a)
                    collect `(,<op> (,(place type i) a)
                                    (,(place type i) b))))
      a)))

(do-vec-combinations define-2nvecop (+ - * /))

(define-template vsqrlen <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(name type) a))
      (+ ,@(loop for i from 0 below <s>
                 collect `(expt (,(place type i) a) 2))))))

(do-vec-combinations define-vsqrlen)

(define-dispatch vsqrlen (a)
  vec (2 3 4) (single-float double-float ub32 sb32))
