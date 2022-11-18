#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;;;; Required OPS:
;; [x] with-vec
;; [ ] vapply
;; [ ] swizzle
;; [x] vsetf
;; [x] v= v/= v< v> v<= v>=
;; [x] vmin vmax
;; [x] vdistance vsqrdistance
;; [x] vlength vsqrlength
;; [x] v2norm v1norm vinorm vpnorm
;; [x] v<-
;; [x] v+ v- v* v/
;; [x] v1+ v1-
;; [x] vincf vdecf
;; [x] v.
;; [x] vc
;; [x] vangle
;; [x] vabs
;; [x] vmod
;; [x] vunit vunit*
;; [x] vscale
;; [x] vfloor vceiling vround
;; [x] vclamp vlerp
;; [x] vlimit
;; [x] vrot vrotv vrot2
;; [x] vrand
;; [x] valign
;; [x] vcartesian vpolar
;; [x] vorder
;; [x] nvmin nvmax
;; [x] nv+ nv- nv* nv/
;; [x] nvabs
;; [x] nvmod
;; [x] nvfloor nvceiling nvround
;; [x] nvclamp nvlerp
;; [x] nvlimit
;; [x] nvrot nvrotv nvrot2
;; [x] nvalign
;; [x] nvcartesian nvpolar
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


(defmacro define-pure-alias (name args &optional (func (compose-name NIL '! name)))
  `(define-alias ,name ,args
     ,(if (find '&rest args)
          `(apply #',func (vzero ,(first args)) ,@(lambda-list-variables args))
          `(,func (vzero ,(first args)) ,@(lambda-list-variables args)))))

(defmacro define-modifying-alias (name args &optional (func (compose-name NIL '! name)))
  `(define-alias ,name ,args
     ,(if (find '&rest args)
          `(apply #',func ,(first args) ,@(lambda-list-variables args))
          `(,func ,(first args) ,@(lambda-list-variables args)))))

(defmacro define-simple-alias (name args &optional (func (compose-name NIL '! name)))
  `(progn (define-pure-alias ,name ,args ,func)
          (define-modifying-alias ,(compose-name NIL 'n name) ,args ,func)))

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

(define-templated-dispatch vsetf (a x y &optional z w)
  ((vec-type T T T) setf))

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
(define-templated-dispatch !vrand (x a var)
  ((vec-type 0 0) random))
(define-templated-dispatch !vorder (x a fields)
  ((vec-type 0 T) order))
(define-templated-dispatch !vc (x a b)
  ((*vec3-type 0 0) cross))
(define-templated-dispatch !vrot (x a axis phi)
  ((*vec3-type 0 0 real) rotate))
(define-templated-dispatch !vrot2 (x a phi)
  ((*vec2-type 0 real) rotate2))
(define-templated-dispatch !vcartesian (x a)
  ((*vec2-type 0) cartesian)
  ((*vec3-type 0) cartesian))
(define-templated-dispatch !vpolar (x a)
  ((*vec2-type 0) polar)
  ((*vec3-type 0) polar))

(define-simple-alias v+ (v &rest others))
(define-simple-alias v- (v &rest others))
(define-simple-alias v* (v &rest others))
(define-simple-alias v/ (v &rest others))
(define-simple-alias vmin (v &rest others))
(define-simple-alias vmax (v &rest others))
(define-simple-alias vabs (v))
(define-simple-alias vmod (v modulus) !2vmod)
(define-simple-alias vfloor (v &optional (d 1)))
(define-simple-alias vceiling (v &optional (d 1)))
(define-simple-alias vround (v &optional (d 1)))
(define-simple-alias vlimit (v limit))
(define-simple-alias vrot (v axis phi))
(define-simple-alias vrot2 (v phi))
(define-simple-alias valign (v grid))
(define-simple-alias vorder (v fields))
(define-simple-alias vcartesian (v))
(define-simple-alias vpolar (v))
(define-simple-alias vlerp (from to tt))
(define-simple-alias vrand (v var))

(define-alias vunit (a)
  (!v/ (vzero a) a (v2norm a)))
(define-alias nvunit (a)
  (!v/ a a (v2norm a)))
(define-alias vunit* (a)
  (let ((length (v2norm a)))
    (if (= 0 length) (vcopy a) (!v/ (vzero a) a length))))
(define-alias nvunit* (a)
  (let ((length (v2norm a)))
    (if (= 0 length) a (!v/ a a length))))
(define-alias nvrotv (v by)
  (let ((x (vzero v)))
    (!vrot x v #.(vec 1 0 0) (vx by))
    (!vrot x x #.(vec 0 1 0) (vy by))
    (!vrot x x #.(vec 0 0 1) (vz by))))
(define-alias nvrotv (v by)
  (!vrot v v #.(vec 1 0 0) (vx by))
  (!vrot v v #.(vec 0 1 0) (vy by))
  (!vrot v v #.(vec 0 0 1) (vz by)))
(define-alias vscale (a s)
  (!2v* (vzero a) a (/ s (v2norm a))))
(define-alias nvscale (a s)
  (!2v* a a (/ s (v2norm a))))
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
(define-alias vangle (a b)
  (let ((a (/ (v. a b)
              (v2norm a)
              (v2norm b))))
    (acos (clamp -1 a +1))))
(define-alias vclamp (low x high)
  (!vclamp (vzero x) low x high))
(define-alias nvclamp (low x high)
  (!vclamp x low x high))
