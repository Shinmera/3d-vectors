#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;; Element-Wise vector operation
(define-template 2vecop <op> <s> <t> (x a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,<op> (,(place type i) a)
                                     (,(place type i) b))))
      x)))

;; Element-wise scalar operation
(define-template svecop <op> <s> <t> (x a s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type ,<t> s))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,<op> (,(place type i) a) s)))
      x)))

;; Element-wise operation
(define-template 1vecop <op> <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,<op> (,(place type i) a))))
      x)))

;; Element-wise vector reduce operation
(define-template 2vecreduce <red> <comb> <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a b))
      (,<red> ,@(loop for i from 0 below <s>
                      collect `(,<comb> (,(place type i) a)
                                        (,(place type i) b)))))))

;; Element-wise reduce operation
(define-template 1vecreduce <red> <comb> <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a))
      (,<red> ,@(loop for i from 0 below <s>
                      collect `(,<comb> (,(place type i) a)))))))

(define-template clamp <s> <t> (x lower a upper)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a x)
               (type ,<t> lower upper))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(clamp lower (,(place type i) a) upper)))
      x)))

(define-template lerp <s> <t> (x from to tt)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x from to)
               (type single-float tt))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,(case <t> ((f32 f64) <t>) (T 'floor))
                               (lerp (,(place type i) from) (,(place type i) to) x))))
      x)))

(define-template random <s> <t> (x from to)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x from to))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(let ((lo (,(place type i) from))
                                    (hi (,(place type i) to)))
                                (+ lo (random (- hi lo))))))
      x)))

(define-template round <op> <s> <t> (x a divisor)
  (let ((type (type-instance 'vec-type <s> <t>))
        (op (ecase <op>
              (floor (case <t> ((f32 f64) 'ffloor) (T 'floor)))
              (round (case <t> ((f32 f64) 'fround) (T 'round)))
              (ceiling (case <t> ((f32 f64) 'fceiling) (T 'ceiling))))))
    `((declare (type ,(lisp-type type) x a))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,op (,(place type i) a) divisor)))
      x)))

(define-template pnorm <s> <t> (a p)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a))
      (declare (type ,<t> p))
      (expt (+ ,@(loop for i from 0 below <s>
                       collect `(expt (abs (,(place type i) a)) p)))
            (/ p)))))

(define-template swizzle <fields> <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>))
        (target (type-instance 'vec-type (length (string <fields>)) <t>))
        (arity (loop for char across (string <fields>)
                     maximize (ecase (char-downcase char)
                                (#\_ 0) (#\x 1) (#\y 2) (#\z 3) (#\w 4)))))
    (when (< <s> arity)
      (error 'template-unfulfillable))
    `((declare (type ,(lisp-type type) a))
      (declare (type ,(lisp-type target) x))
      (psetf ,@(loop for name across (string <fields>)
                     for field = (intern (string name))
                     for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect (if (eql field '_)
                                 `(,<t> 0)
                                 `(,(place type field) a))))
      x)))

(define-template order <s> <t> (x a fields)
  (let ((type (type-instance 'vec-type <s> <t>)))
    (labels ((maybe-place (n)
               (if (<= <s> n)
                   `(error "Bad swizzle spec for vector with size ~a: ~a" ,<s> fields)
                   `(,(place type n) a)))
             (source (n)
               `(ecase (char fields ,n)
                  (#\X ,(maybe-place 0))
                  (#\Y ,(maybe-place 1))
                  (#\Z ,(maybe-place 2))
                  (#\W ,(maybe-place 3))
                  (#\_ (,(place-type type 0) 0)))))
      `((declare (type ,(lisp-type type) a))
        (let* ((fields (string fields))
               (len (length fields)))
          (when (< 4 len) (error "Bad swizzle spec: ~a" fields))
          (when (< 3 len) (setf (vw x) ,(source 3)))
          (when (< 2 len) (setf (vz x) ,(source 2)))
          (when (< 1 len) (setf (vy x) ,(source 1)))
          (setf (vx x) ,(source 0)))
        x))))

(define-template cross <s> <t> (x a b)
  (when (/= 3 <s>) (error 'template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b))
      (let ((ax (,(place type 0) a))
            (ay (,(place type 1) a))
            (az (,(place type 2) a))
            (bx (,(place type 0) b))
            (by (,(place type 1) b))
            (bz (,(place type 2) b)))
        (setf (,(place type 0) x) (- (* ay bz) (* az by))
              (,(place type 1) x) (- (* az bx) (* ax bz))
              (,(place type 2) x) (- (* ax by) (* ay bx)))
        x))))

(do-vec-combinations define-2vecop (+ - * / min max mod))
(do-vec-combinations define-svecop (+ - * / min max mod grid))
(do-vec-combinations define-1vecop (- / abs identity))
(do-vec-combinations define-2vecreduce (and) (= /= < <= >= >))
(do-vec-combinations define-2vecreduce (+) (*)) ; dot
(do-vec-combinations define-2vecreduce (+ sqrt+) (sqr2)) ; sqrdist dist
(do-vec-combinations define-1vecreduce (+ max) (abs)) ; 1norm inorm
(do-vec-combinations define-1vecreduce (+ sqrt+) (sqr)) ; sqrlen 2norm
(do-vec-combinations define-clamp)
(do-vec-combinations define-lerp)
(do-vec-combinations define-round (floor round ceiling))
(do-vec-combinations define-nround (floor round ceiling))
(do-vec-combinations define-pnorm)
(do-vec-combinations define-random)
(do-vec-combinations define-order)
(do-vec-combinations define-cross)
;; FIXME: Macro expanders for the order functions

;;;; Required RAW OPS:
;; [x] v= v/= v< v> v<= v>=
;; [x] vmin vmax
;; [x] vdistance vsqrdistance
;; [x] vlength vsqrlength
;; [x] v2norm v1norm vinorm vpnorm
;; [?] vapply
;; [x] v<-
;; [x] v+ v- v* v/
;; [x] v.
;; [x] vc
;; [ ] vangle
;; [x] vabs
;; [x] vmod
;; [ ] vunit vunit*
;; [ ] vscale
;; [x] vfloor vceiling vround
;; [x] vclamp vlerp
;; [ ] vlimit
;; [ ] vrot vrotv vrot2
;; [x] vrand
;; [x] valign
;; [ ] vcartesian vpolar
;; [x] vorder
;; [x] swizzle
