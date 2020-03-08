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

(do-vec-combinations define-2vecop (+ - * / min max))
(do-vec-combinations define-2nvecop (+ - * / min max))
(do-vec-combinations define-svecop (+ - * / min max mod))
(do-vec-combinations define-snvecop (+ - * / min max mod))
(do-vec-combinations define-1vecop (- /))
(do-vec-combinations define-1nvecop (- /))
(do-vec-combinations define-2vecreduce (and) (= /= < <= >= >))
(do-vec-combinations define-2vecreduce (+) (*))
(do-vec-combinations define-vecreduce (+ max) (abs)) ;1norm inorm
(do-vec-combinations define-vecreduce (+) (sqr)) ;sqrlen
(do-vec-combinations define-clamp)

(defmacro define-vecop (op)
  `(define-templated-dispatch ,(compose-name NIL '2v op) (a b)
     ((vec-type real) svecop ,op)
     ((vec-type 0) 2vecop ,op)))

(define-vecop +)
(define-vecop -)
(define-vecop *)
(define-vecop /)
(define-vecop min)
(define-vecop max)
(define-templated-dispatch 1v- (a)
  ((vec-type) 1vecop -))
(define-templated-dispatch 1v/ (a)
  ((vec-type) 1vecop /))

(define-reductor v+ 2v+)
(define-reductor v- 2v- 1v- v+)
(define-reductor v* 2v*)
(define-reductor v/ 2v/ 1v/ v*)

(defun vlimit (vec limit)
  (vclamp (- limit) vec limit))
(defun nvlimit (vec limit)
  (nvclamp (- limit) vec limit))

;; TODO: length 2norm pnorm 1+ 1- incf decf cross angle abs unit clamp lerp rot order swizzle rand align floor ceil round dist reflect
