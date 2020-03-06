#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;; FIXME: Need to be able to specify template arguments for the operation only,
;;        not for the type being templated, so that we can re-use the template
;;        for + - * / etc.
;;
;;        It occurs to me that we should always inherit the type's template
;;        parameters, so we only have to specify new ones here.
;;        Another thought is that we should be able to template on each argument
;;        type if so desired.
;;        That kind of information could also be saved somewhere else so that the
;;        dispatch generation could be automated more easily.
(define-template 2vec+ vec <s> <t> (a b)
  `(declare (type ,name a b))
  `(,constructor
    ,@(loop for i from 0 below <s>
            collect `(+ (,(place i) a)
                        (,(place i) b)))))

(do-vec-types define-2vec+)

(define-template 2nvec+ vec <s> <t> (a b)
  `(declare (type ,name a b))
  `(setf ,@(loop for i from 0 below <s>
                 collect `(,(place i) a)
                 collect `(+ (,(place i) a)
                             (,(place i) b))))
  'a)

(do-vec-types define-2nvec+)

(define-template vsqrlen vec <s> <t> (a)
  `(declare (type ,name a))
  `(+ ,@(loop for i from 0 below <s>
              collect `(expt (,(place i) a) 2))))

(do-vec-types define-vsqrlen)

(define-dispatch vsqrlen (a)
  vec (2 3 4) (single-float double-float ub32 sb32))
