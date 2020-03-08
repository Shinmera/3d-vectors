#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(define-template 2vecop <op> <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a b))
      (,(constructor type)
       ,@(loop for i from 0 below <s>
               collect `(,<op> (,(place type i) a)
                               (,(place type i) b)))))))

(define-template 2nvecop <op> <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a b))
      (setf ,@(loop for i from 0 below <s>
                    collect `(,(place type i) a)
                    collect `(,<op> (,(place type i) a)
                                    (,(place type i) b))))
      a)))

(define-template svecop <op> <s> <t> (a s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,<t> s))
      (,(constructor type)
       ,@(loop for i from 0 below <s>
               collect `(,<op> (,(place type i) a) s))))))

(define-template snvecop <op> <s> <t> (a s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,<t> s))
      (setf ,@(loop for i from 0 below <s>
                    collect `(,(place type i) a)
                    collect `(,<op> (,(place type i) a) s)))
      a)))

(define-template 1vecop <op> <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a))
      (,(constructor type)
       ,@(loop for i from 0 below <s>
               collect `(,<op> (,(place type i) a)))))))

(define-template 1nvecop <op> <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a))
      (setf ,@(loop for i from 0 below <s>
                    collect `(,(place type i) a)
                    collect `(,<op> (,(place type i) a))))
      a)))

(define-template 2vecreduce <red> <comb> <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a b))
      (,<red> ,@(loop for i from 0 below <s>
                      collect `(,<comb> (,(place type i) a)
                                        (,(place type i) b)))))))

(define-template vecreduce <red> <comb> <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a))
      (,<red> ,@(loop for i from 0 below <s>
                      collect `(,<comb> (,(place type i) a)))))))

(define-template clamp <s> <t> (lower a upper)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,<t> lower upper))
      (,(constructor type)
       ,@(loop for i from 0 below <s>
               collect `(max lower (min upper (,(place type i) a))))))))

(define-template lerp <s> <t> (from to x)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) from to)
               (type single-float x))
      (,(constructor type)
       ,@(loop for i from 0 below <s>
               collect `(,(case <t>
                            ((f32 f64) <t>) (T 'floor))
                         (lerp (,(place type i) from) (,(place type i) to) x)))))))

(define-template random <s> <t> (from to)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `(,(constructor type)
      ,@(loop for i from 0 below <s>
              collect `(type-random '<t> from to)))))

(do-vec-combinations define-2vecop (+ - * / min max mod))
(do-vec-combinations define-2nvecop (+ - * / min max mod))
(do-vec-combinations define-svecop (+ - * / min max mod))
(do-vec-combinations define-snvecop (+ - * / min max mod))
(do-vec-combinations define-1vecop (- / abs))
(do-vec-combinations define-1nvecop (- / abs))
(do-vec-combinations define-2vecreduce (and) (= /= < <= >= >))
(do-vec-combinations define-2vecreduce (+) (*)) ; dot
(do-vec-combinations define-vecreduce (+ max) (abs)) ;1norm inorm
(do-vec-combinations define-vecreduce (+ sqrt+) (sqr)) ;sqrlen 2norm
(do-vec-combinations define-clamp)
(do-vec-combinations define-lerp)

(defmacro define-2vec-dispatch (op)
  `(progn
     (define-templated-dispatch ,(compose-name NIL '2v op) (a b)
       ((vec-type real) svecop ,op)
       ((vec-type 0) 2vecop ,op))
     (define-templated-dispatch ,(compose-name NIL '2nv op) (a b)
       ((vec-type real) snvecop ,op)
       ((vec-type 0) 2nvecop ,op))))

(defmacro define-1vec-dispatch (name op &rest template-args)
  `(define-templated-dispatch ,name (a)
     ((vec-type) ,op ,@template-args)))

(define-2vec-dispatch +)
(define-2vec-dispatch -)
(define-2vec-dispatch *)
(define-2vec-dispatch /)
(define-2vec-dispatch min)
(define-2vec-dispatch max)
(define-2vec-dispatch mod)
(define-1vec-dispatch 1v- 1vecop -)
(define-1vec-dispatch 1nv- 1nvecop -)
(define-1vec-dispatch 1v/ 1vecop /)
(define-1vec-dispatch 1nv/ 1nvecop /)
(define-1vec-dispatch vabs 1vecop abs)
(define-1vec-dispatch nvabs 1nvecop abs)
(define-1vec-dispatch v1norm vecreduce + abs)
(define-1vec-dispatch vinorm vecreduce max abs)
(define-1vec-dispatch v2norm vecreduce sqrt+ sqr)
(define-1vec-dispatch vqsrlen vecreduce + sqr)
(define-templated-dispatch v. (a b)
  ((vec-type 0) 2vecreduce + *))
(define-templated-dispatch vclamp (low x up)
  ((real vec-type real) clamp))
(define-templated-dispatch vlerp (from to x)
  ((vec-type 0 single-float) lerp))

(define-alias vlength (a)
  (v2norm a))
(define-alias v1+ (a)
  (v+ a 1))
(define-alias v1- (a)
  (v- a 1))
(define-alias vincf (a &optional (d 1))
  (nv+ a d))
(define-alias vdecf (a &optional (d 1))
  (nv- a d))
(define-alias vlimit (vec limit)
  (vclamp (- limit) vec limit))
(define-alias nvlimit (vec limit)
  (nvclamp (- limit) vec limit))

(defun vc (a b)
  (%vec3 (- (* (vy3 a) (vz3 b))
            (* (vz3 a) (vy3 b)))
         (- (* (vz3 a) (vx3 b))
            (* (vx3 a) (vz3 b)))
         (- (* (vx3 a) (vy3 b))
            (* (vy3 a) (vx3 b)))))

;; FIXME: it seems that chained operators don't receive transforms for some reason.
(define-right-reductor v+ 2v+)
(define-right-reductor v- 2v- 1v- v+)
(define-right-reductor v* 2v*)
(define-right-reductor v/ 2v/ 1v/ v*)
(define-left-reductor nv+ 2nv+)

;; TODO: angle unit rot order swizzle rand dist align floor ceil round
