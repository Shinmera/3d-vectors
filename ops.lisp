#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;;;; Required OPS:
;; [ ] with-vecX
;; [ ] vsetf
;; [x] v= v/= v< v> v<= v>=
;; [ ] vmin vmax
;; [x] vdistance vsqrdistance
;; [x] vlength vsqrlength
;; [x] v2norm v1norm vinorm vpnorm
;; [ ] vapply
;; [x] v<-
;; [ ] v+ v- v* v/
;; [x] v1+ v1-
;; [x] vincf vdecf
;; [x] v.
;; [x] vc
;; [x] vangle
;; [ ] vabs
;; [ ] vmod
;; [x] vunit vunit*
;; [x] vscale
;; [ ] vfloor vceiling vround
;; [ ] vclamp vlerp
;; [ ] vlimit
;; [ ] vrot vrotv vrot2
;; [ ] vrand
;; [ ] valign
;; [ ] vcartesian vpolar
;; [ ] vorder
;; [ ] swizzle
;; [x] nvmin nvmax
;; [x] nv+ nv- nv* nv/
;; [x] nvabs
;; [x] nvmod
;; [x] nvfloor nvceiling nvround
;; [x] nvclamp nvlerp
;; [x] nvlimit
;; [x] nvrot nvrotv nvrot2
;; [x] nvalign
;; [ ] nvcartesian nvpolar
;; [x] nvorder

(defmacro define-2vec-dispatch (op)
  `(progn
     (define-templated-dispatch ,(compose-name NIL '!2v op) (x a b)
       ((fvec-type 0 single-float) svecop ,op <t>)
       ((dvec-type 0 double-float) svecop ,op <t>)
       ((ivec-type 0 (signed-byte 32)) svecop ,op <t>)
       ((uvec-type 0 (unsigned-byte 32)) svecop ,op <t>)
       ((vec-type 0 real) svecop ,op real)
       ((vec-type 0 0) 2vecop ,op))
     #+sbcl
     (sb-c:defoptimizer (,(compose-name NIL '!2v op) sb-c:derive-type) ((x a b))
       (declare (ignore a b))
       (sb-c::lvar-type x))))

(defmacro define-1vec-dispatch (name op &rest template-args)
  `(progn
     (define-templated-dispatch ,name (x a)
       ((vec-type 0) ,op ,@template-args))
     #+sbcl
     (sb-c:defoptimizer (,name sb-c:derive-type) ((x a))
       (declare (ignore a))
       (sb-c::lvar-type x))))

(defmacro define-veccomp-dispatch (op)
  `(define-templated-dispatch ,(compose-name NIL '2v op) (a b)
     ((fvec-type single-float) svecreduce and ,op <t>)
     ((dvec-type double-float) svecreduce and ,op <t>)
     ((ivec-type (signed-byte 32)) svecreduce and ,op <t>)
     ((uvec-type (unsigned-byte 32)) svecreduce and ,op <t>)
     ((vec-type real) svecreduce and ,op real)
     ((vec-type 0) 2vecreduce and ,op)))

(defmacro define-vec-reductor (name 2-op &optional 1-op)
  `(progn
     (defun ,name (target value &rest values)
       (cond ((null values)
              ,(if 1-op
                   `(,1-op target value)
                   `(v<- target value)))
             ((null (cdr values))
              (,2-op target value (first values)))
             (T
              (,2-op target value (first values))
              (dolist (value (rest values) target)
                (,2-op target target value)))))

     (define-compiler-macro ,name (target value &rest values)
       (cond ((null values)
              ,(if 1-op
                   ``(,',1-op ,target ,value)
                   ``(v<- ,target ,value)))
             ((null (cdr values))
              `(,',2-op ,target ,value ,(first values)))
             (T
              (let ((targetg (gensym "TARGET")))
                `(let ((,targetg ,target))
                   (,',2-op ,targetg ,value ,(first values))
                   ,@(loop for value in (rest values)
                           collect `(,',2-op ,targetg ,targetg ,value)))))))))

(defmacro define-value-reductor (name 2-op comb identity)
  `(progn
     (defun ,name (value &rest values)
       (cond ((null values)
              ,identity)
             ((null (cdr values))
              (,2-op value (first values)))
             (T
              (let* ((previous (first values))
                     (result (,2-op value previous)))
                (dolist (value (rest values) result)
                  (setf result (,comb result (,2-op previous value)))
                  (setf previous value))))))

     (define-compiler-macro ,name (value &rest values)
       (cond ((null values)
              ,identity)
             ((null (cdr values))
              `(,',2-op ,value ,(first values)))
             (T
              (let ((previous (gensym "PREVIOUS"))
                    (next (gensym "NEXT")))
                `(let ((,previous ,value))
                   (,',comb ,@(loop for value in values
                                    collect `(let ((,next ,value))
                                               (prog1 (,',2-op ,previous ,next)
                                                 (setf ,previous ,next))))))))))))

(define-2vec-dispatch +)
(define-2vec-dispatch -)
(define-2vec-dispatch *)
(define-2vec-dispatch /)
(define-2vec-dispatch min)
(define-2vec-dispatch max)
(define-2vec-dispatch mod)

(define-templated-dispatch !valign (x a grid)
  ((vec-type 0 single-float) svecop grid single-float)
  ((vec-type 0 double-float) svecop grid double-float)
  ((vec-type 0 real) svecop grid real))

(define-1vec-dispatch !1v- 1vecop -)
(define-1vec-dispatch !1v/ 1vecop /)
(define-1vec-dispatch !vabs 1vecop abs)
(define-1vec-dispatch v<- 1vecop identity)

;; FIXME: These do NOT work correctly for singles followed by vecs
(define-veccomp-dispatch =)
(define-veccomp-dispatch /=)
(define-veccomp-dispatch <)
(define-veccomp-dispatch <=)
(define-veccomp-dispatch >)
(define-veccomp-dispatch >=)

(define-vec-reductor !v+ !2v+)
(define-vec-reductor !v* !2v*)
(define-vec-reductor !v- !2v- !1v-)
(define-vec-reductor !v/ !2v/ !1v/)
(define-vec-reductor !vmin !2vmin)
(define-vec-reductor !vmax !2vmax)

(define-value-reductor v= 2v= and T)
(define-value-reductor v/= 2v/= and T)
(define-value-reductor v< 2v< and T)
(define-value-reductor v<= 2v<= and T)
(define-value-reductor v> 2v> and T)
(define-value-reductor v>= 2v>= and T)

(define-templated-dispatch v. (a b)
  ((vec-type 0) 2vecreduce + *))
(define-templated-dispatch vdistance (a b)
  ((vec-type 0) 2vecreduce sqrt+ sqr2))
(define-templated-dispatch vsqrdistance (a b)
  ((vec-type 0) 2vecreduce + sqr2))
(define-templated-dispatch v1norm (a)
  ((vec-type) 1vecreduce + abs))
(define-templated-dispatch vinorm (a)
  ((vec-type) 1vecreduce max abs))
(define-templated-dispatch v2norm (a)
  ((vec-type) 1vecreduce sqrt+ sqr))
(define-templated-dispatch vsqrlength (a)
  ((vec-type) 1vecreduce + sqr))

(define-templated-dispatch !vclamp (x low a up)
  ((vec-type real 0 real) clamp))
(define-templated-dispatch !vlimit (x a limit)
  ((vec-type 0 0) limit))
(define-templated-dispatch !vlerp (x from to tt)
  ((vec-type 0 0 single-float) lerp))
(define-templated-dispatch !vfloor (x a &optional (divisor 1))
  ((vec-type 0 real) round floor))
(define-templated-dispatch !vround (x a &optional (divisor 1))
  ((vec-type 0 real) round round))
(define-templated-dispatch !vceiling (x a &optional (divisor 1))
  ((vec-type 0 real) round ceiling))
(define-templated-dispatch vpnorm (a p)
  ((vec-type real) pnorm))
(define-templated-dispatch !vrandom (x from to)
  ((vec-type 0 0) random))
(define-templated-dispatch !vorder (x a fields)
  ((vec-type 0 T) order))
(define-templated-dispatch !vc (x a b)
  ((*vec3-type 0 0) cross))
(define-templated-dispatch !vrot (x a axis phi)
  ((*vec3-type 0 0 real) rotate))
(define-templated-dispatch !vrot2 (x a phi)
  ((*vec2-type 0 real) rotate2))

(define-alias nv+ (a &rest v)
  (apply #'!v+ a a v))
(define-alias nv- (a &rest v)
  (apply #'!v- a a v))
(define-alias nv* (a &rest v)
  (apply #'!v* a a v))
(define-alias nv/ (a &rest v)
  (apply #'!v/ a a v))
(define-alias nvmin (a &rest v)
  (apply #'!vmin a a v))
(define-alias nvmax (a &rest v)
  (apply #'!vmax a a v))
(define-alias nvunit (a)
  (!v/ a a (v2norm a)))
(define-alias nvunit* (a)
  (let ((length (v2norm a)))
    (if (= 0 length) a (!v/ a a length))))
(define-alias nvabs (v)
  (!vabs v v))
(define-alias nvmod (v modulus)
  (!2vmod v v modulus))
(define-alias nvfloor (v &optional (d 1))
  (!vfloor v v d))
(define-alias nvceiling (v &optional (d 1))
  (!vceiling v v d))
(define-alias nvround (v &optional (d 1))
  (!vround v v d))
(define-alias nvlimit (v limit)
  (!vlimit v v limit))
(define-alias nvrot (v axis phi)
  (!vrot v v axis phi))
(define-alias nvrotv (v by)
  (!vrot v v #.(vec 1 0 0) (vx by))
  (!vrot v v #.(vec 0 1 0) (vy by))
  (!vrot v v #.(vec 0 0 1) (vz by)))
(define-alias nvrot2 (v phi)
  (!vrot2 v v phi))
(define-alias nvalign (v grid)
  (!valign v v grid))
(define-alias nvorder (v fields)
  (!vorder v v fields))

(define-alias vincf (a &optional (d 1))
  (!2v+ a a d))
(define-alias vdecf (a &optional (d 1))
  (!2v- a a d))
(define-alias vlength (a)
  (v2norm a))
(define-alias v1+ (a)
  (v+ a 1))
(define-alias v1- (a)
  (v- a 1))
(define-alias vunit (a)
  (v/ a (v2norm a)))
(define-alias vunit* (a)
  (let ((len (v2norm a)))
    (if (= 0 len) (vcopy a) (v/ a len))))
(define-alias vscale (a s)
  (let ((x (vunit a)))
    (!2v* x x s)))
(define-alias vangle (a b)
  (let ((a (/ (v. a b)
              (v2norm a)
              (v2norm b))))
    (acos (clamp -1 a +1))))
