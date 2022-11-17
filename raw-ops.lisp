#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;; Element-Wise vector operation
(define-template 2vecop <op> <s> <t> (x a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type))
               inline)
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,<op> (,(place type i) a)
                                     (,(place type i) b))))
      x)))

;; Element-wise scalar operation
(define-template svecop <op> <st> <s> <t> (x a s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,(lisp-type type))
               inline)
      (let ((s (,<t> s)))
        (psetf ,@(loop for i from 0 below <s>
                       collect `(,(place type i) x)
                       collect `(,<op> (,(place type i) a) s))))
      x)))

;; Element-wise operation
(define-template 1vecop <op> <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               inline)
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,<op> (,(place type i) a))))
      x)))

;; Element-wise vector reduce operation
(define-template 2vecreduce <red> <comb> rtype <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) a b)
               (return-type ,rtype)
               inline)
      (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
       (,<red> ,@(loop for i from 0 below <s>
                       collect `(,<comb> (,(place type i) a)
                                         (,(place type i) b))))))))

;; Element-wise reduce operation
(define-template 1vecreduce <red> <comb> rtype <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) a)
               (return-type ,rtype)
               inline)
      (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
       (,<red> ,@(loop for i from 0 below <s>
                       collect `(,<comb> (,(place type i) a))))))))

(define-template svecreduce <red> <comb> <st> rtype <s> <t> (a s)
  (let ((type (type-instance 'vec-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) a)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,rtype)
               inline)
      (let ((s (,<t> s)))
        (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
         (,<red> ,@(loop for i from 0 below <s>
                         collect `(,<comb> (,(place type i) a) s))))))))

(define-template clamp <s> <t> (x lower a upper)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a x)
               (type ,<t> lower upper)
               (return-type ,(lisp-type type)))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(clamp lower (,(place type i) a) upper)))
      x)))

(define-template limit <s> <t> (x a l)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a x l)
               (return-type ,(lisp-type type)))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(clamp (- (,(place type i) l)) (,(place type i) a) (+ (,(place type i) l)))))
      x)))

(define-template lerp <s> <t> (x from to tt)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x from to)
               (type single-float tt)
               (return-type ,(lisp-type type)))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,(case <t> ((f32 f64) <t>) (T 'floor))
                               (lerp (,(place type i) from) (,(place type i) to) tt))))
      x)))

(define-template random <s> <t> (x from to)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x from to)
               (return-type ,(lisp-type type)))
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
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type)))
      (psetf ,@(loop for i from 0 below <s>
                     collect `(,(place type i) x)
                     collect `(,op (,(place type i) a) divisor)))
      x)))

(define-template pnorm <s> <t> (a p)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,<t> p)
               (return-type ,(case <t> (f32 'f32) (f64 'f64) (T 'real))))
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
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type target) x)
               (return-type ,(lisp-type target)))
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
      `((declare (type ,(lisp-type type) a)
                 (type *vec x)
                 (return-type *vec))
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
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type)))
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

(define-template rotate <s> <t> (x a axis phi)
  (when (/= 3 <s>) (error 'template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    (flet ((arith (field)
             `(+ (* (,field a) cos)
                 (* (,field c) sin)
                 (* (,field axis) d (- 1 cos)))))
      `((declare (type ,(lisp-type type) x a axis)
                 (type single-float phi)
                 (return-type ,(lisp-type type)))
        (let* ((cos (the single-float (cos phi)))
               (sin (the single-float (sin phi)))
               (c (,(constructor type) ,@(make-list <s> :initial-element (funcall <t> 0))))
               (d (,(compose-name #\/ '2vecreduce '+ '* <s> <t>) axis a)))
          (declare (dynamic-extent c))
          (,(compose-name #\/ 'cross <s> <t>) c axis a)
          (psetf (,(place type 0) x) (,<t> ,(arith (place type 0)))
                 (,(place type 1) x) (,<t> ,(arith (place type 1)))
                 (,(place type 2) x) (,<t> ,(arith (place type 2))))
          x)))))

(define-template rotate2 <s> <t> (x a phi)
  (when (/= 2 <s>) (error 'template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type single-float phi)
               (return-type ,(lisp-type type)))
      (let ((sin (sin phi))
            (cos (cos phi)))
        (psetf (,(place type 0) x) (,<t> (- (* (,(place type 0) a) cos) (* (,(place type 1) a) sin)))
               (,(place type 1) x) (,<t> (+ (* (,(place type 0) a) sin) (* (,(place type 1) a) cos))))
        x))))

(do-vec-combinations define-2vecop (+ - * / min max mod))
(do-vec-combinations define-svecop (+ - * / min max mod grid) (<t> real))
(do-vec-combinations define-1vecop (- / abs identity))
(do-vec-combinations define-2vecreduce (and) (= /= < <= >= >) boolean)
(do-vec-combinations define-svecreduce (and) (= /= < <= >= >) (<t> real) boolean)
(do-vec-combinations define-2vecreduce (+) (*) <t>) ; dot
(do-vec-combinations define-2vecreduce (sqrt+) (sqr2) float) ; dist
(do-vec-combinations define-2vecreduce (+) (sqr2) <t>) ; sqrdist
(do-vec-combinations define-1vecreduce (+ max) (abs) <t>) ; 1norm inorm
(do-vec-combinations define-1vecreduce (sqrt+) (sqr) float) ; 2norm
(do-vec-combinations define-1vecreduce (+) (sqr) <t>) ; sqrlen
(do-vec-combinations define-clamp)
(do-vec-combinations define-limit)
(do-vec-combinations define-lerp)
(do-vec-combinations define-round (floor round ceiling))
(do-vec-combinations define-pnorm)
(do-vec-combinations define-random)
(do-vec-combinations define-order)
(do-vec-combinations define-cross)
(do-vec-combinations define-rotate)
(do-vec-combinations define-rotate2)
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
;; [/] vangle
;; [x] vabs
;; [x] vmod
;; [/] vunit vunit*
;; [/] vscale
;; [x] vfloor vceiling vround
;; [x] vclamp vlimit vlerp
;; [x] vrot vrot2
;; [x] vrand
;; [x] valign
;; [ ] vcartesian vpolar
;; [x] vorder
;; [x] swizzle
