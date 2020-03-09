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
    `((,(constructor type)
        ,@(loop for i from 0 below <s>
                collect `(type-random ',<t> from to))))))

(define-template round <op> <s> <t> (a divisor)
  (let ((type (type-instance 'vec-type <s> <t>))
        (op (ecase <op>
              (floor (case <t> ((f32 f64) 'ffloor) (T 'floor)))
              (round (case <t> ((f32 f64) 'fround) (T 'round)))
              (ceiling (case <t> ((f32 f64) 'fceiling) (T 'ceiling))))))
    `((declare (type ,(lisp-type type) a))
      (,(constructor type)
       ,@(loop for i from 0 below <s>
               collect `(,op (,(place type i) a) divisor))))))

(define-template nround <op> <s> <t> (a divisor)
  (let ((type (type-instance 'vec-type <s> <t>))
        (op (ecase <op>
              (floor (case <t> ((f32 f64) 'ffloor) (T 'floor)))
              (round (case <t> ((f32 f64) 'fround) (T 'round)))
              (ceiling (case <t> ((f32 f64) 'fceiling) (T 'ceiling))))))
    `((declare (type ,(lisp-type type) a))
      (setf ,@(loop for i from 0 below <s>
                    collect `(,(place type i) a)
                    collect `(,op (,(place type i) a) divisor)))
      a)))

(define-template pnorm <s> <t> (a p)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a))
      (expt (+ ,@(loop for i from 0 below <s>
                       collect `(expt (abs (,(place type i) a)) p)))
            (/ p)))))

(define-template swizzle <fields> <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>))
        (target (type-instance 'vec-type (length (string <fields>)) <t>)))
    `((declare (type ,(lisp-type type) a))
      (,(constructor target)
       ,@(loop for name across (string <fields>)
               for field = (intern (string name))
               collect (if (eql field '_)
                           `(,<t> 0)
                           `(,(place type field) a)))))))

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
(do-vec-combinations define-round (floor round ceiling))
(do-vec-combinations define-nround (floor round ceiling))
(do-vec-combinations define-pnorm)
;; FIXME: better naming
(do-vec-combinations define-random)

(defmacro define-2vec-dispatch (op)
  `(progn
     (define-templated-dispatch ,(compose-name NIL '2v op) (a b)
       ((vec-type real) svecop ,op)
       ((vec-type 0) 2vecop ,op))
     #+sbcl
     (sb-c:defoptimizer (,(compose-name NIL '2v op) sb-c:derive-type) ((a b))
       (declare (ignore b))
       (sb-c::lvar-type a))
     
     (define-templated-dispatch ,(compose-name NIL '2nv op) (a b)
       ((vec-type real) snvecop ,op)
       ((vec-type 0) 2nvecop ,op))
     #+sbcl
     (sb-c:defoptimizer (,(compose-name NIL '2nv op) sb-c:derive-type) ((a b))
       (declare (ignore b))
       (sb-c::lvar-type a))))

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
;; FIXME: Handle optionals in dispatch
(define-templated-dispatch vfloor (a divisor)
  ((vec-type real) round floor))
(define-templated-dispatch nvfloor (a divisor)
  ((vec-type real) nround floor))
(define-templated-dispatch vround (a divisor)
  ((vec-type real) round round))
(define-templated-dispatch nvround (a divisor)
  ((vec-type real) nround round))
(define-templated-dispatch vceiling (a divisor)
  ((vec-type real) round ceiling))
(define-templated-dispatch nvceiling (a divisor)
  ((vec-type real) nround ceiling))
(define-templated-dispatch vpnorm (a p)
  ((vec-type real) pnorm))

(define-right-reductor v+ 2v+)
(define-right-reductor v- 2v- 1v- v+)
(define-right-reductor v* 2v*)
(define-right-reductor v/ 2v/ 1v/ v*)
(define-left-reductor nv+ 2nv+)
(define-left-reductor nv- 2v- 1v-)
(define-left-reductor nv* 2nv*)
(define-left-reductor nv/ 2v/ 1v/)

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
(define-alias vangle (a b)
  (acos (/ (v. a b) (v2norm a) (v2norm b))))
(define-alias vunit (a)
  (v/ a (v2norm a)))
(define-alias nvunit (a)
  (nv/ a (v2norm a)))
(define-alias valign (a grid)
  (nv* (nvfloor (v+ a (/ grid 2)) grid) grid))
(define-alias nvalign (a grid)
  (nv* (nvfloor (nv+ a (/ grid 2)) grid) grid))
(define-alias vdistance (a b)
  (v2norm (v- a b)))

(defun vc (a b)
  (%vec3 (- (* (vy3 a) (vz3 b))
            (* (vz3 a) (vy3 b)))
         (- (* (vz3 a) (vx3 b))
            (* (vx3 a) (vz3 b)))
         (- (* (vx3 a) (vy3 b))
            (* (vy3 a) (vx3 b)))))

(defmacro %vecrot-internal (&body body)
  ;; https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  ;; vr = v*cos(phi) + (kxv)*sin(phi) + k*(k*v)*(1-cos(phi)
  `(let* ((k axis)
          (cos (cos phi))
          (sin (sin phi))
          (c (vc k v))
          (d (v. k v)))
     (macrolet ((arith (field)
                  `(+ (* (,field v) cos)
                      (* (,field c) sin)
                      (* (,field k) d (- 1 cos)))))
       ,@body)))

(defun vrot (v axis phi)
  (let ((phi (ensure-float phi)))
    (%vecrot-internal
      (vec3 (arith vx3) (arith vy3) (arith vz3)))))

(defun nvrot (v axis phi)
  (let ((phi (ensure-float phi)))
    (%vecrot-internal
      (setf (vx3 v) (arith vx3)
            (vy3 v) (arith vy3)
            (vz3 v) (arith vz3))
      v)))

(defmacro vsetf (vec x &optional y z w)
  (let ((vecg (gensym "VECTOR")))
    `(let ((,vecg ,vec))
       (psetf (vx ,vecg) ,x
              ,@(when y `((vy ,vecg) ,y))
              ,@(when z `((vz ,vecg) ,z))
              ,@(when w `((vw ,vecg) ,w)))
       ,vecg)))

(defmacro vapply (&environment env vec op &optional x y z w)
  (let ((vecg (gensym "VECTOR")))
    `(let ((,vecg ,vec))
       (etypecase ,vecg
         (vec2 (vec2 (,op (vx2 ,vecg) ,@(when x (list x)))
                     (,op (vy2 ,vecg) ,@(when y (list y)))))
         (vec3 (vec3 (,op (vx3 ,vecg) ,@(when x (list x)))
                     (,op (vy3 ,vecg) ,@(when y (list y)))
                     (,op (vz3 ,vecg) ,@(when z (list z)))))
         (vec4 (vec4 (,op (vx4 ,vecg) ,@(when x (list x)))
                     (,op (vy4 ,vecg) ,@(when y (list y)))
                     (,op (vz4 ,vecg) ,@(when z (list z)))
                     (,op (vw4 ,vecg) ,@(when w (list w)))))))))

(defmacro vapplyf (&environment env vec op &optional x y z w)
  (let ((vecg (gensym "VECTOR")))
    `(let ((,vecg ,vec))
       (vsetf ,vecg
              (,op (vx ,vecg) ,@(when x (list x)))
              (,op (vy ,vecg) ,@(when y (list y)))
              (,op (vz ,vecg) ,@(when z (list z)))
              (,op (vw ,vecg) ,@(when w (list w))))
       ,vecg)))

;; TODO: order swizzle with-vec constants
