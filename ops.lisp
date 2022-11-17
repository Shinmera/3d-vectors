#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;;;; Required OPS:
;; [ ] with-vecX
;; [ ] vsetf
;; [ ] v= v/= v< v> v<= v>=
;; [ ] vmin vmax
;; [ ] vdistance vsqrdistance
;; [ ] vlength vsqrlength
;; [ ] v2norm v1norm vinorm vpnorm
;; [ ] vapply
;; [ ] v<-
;; [ ] v+ v- v* v/
;; [ ] v.
;; [ ] vc
;; [ ] vangle
;; [ ] vabs
;; [ ] vmod
;; [ ] vunit vunit*
;; [ ] vscale
;; [ ] vfloor vceiling vround
;; [ ] vclamp vlerp
;; [ ] vlimit
;; [ ] vrot vrotv vrot2
;; [ ] vrand
;; [ ] valign
;; [ ] vcartesian vpolar
;; [ ] vorder
;; [ ] swizzle

;; FIXME: Handle real -> <t> conversions
(defmacro define-2vec-dispatch (op)
  `(progn
     (define-templated-dispatch ,(compose-name NIL '!2v op) (x a b)
       ((vec-type 0 real) svecop ,op)
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
     ((vec-type real) svecreduce and ,op)
     ((vec-type 0) 2vecreduce and ,op)))

(define-2vec-dispatch +)
(define-2vec-dispatch -)
(define-2vec-dispatch *)
(define-2vec-dispatch /)
(define-2vec-dispatch min)
(define-2vec-dispatch max)
(define-2vec-dispatch mod)

(define-templated-dispatch !valign (x a grid)
  ((vec-type 0 real) svecop grid))

(define-1vec-dispatch 1v- 1vecop -)
(define-1vec-dispatch 1v/ 1vecop /)
(define-1vec-dispatch vabs 1vecop abs)
(define-1vec-dispatch v<- 1vecop identity)

(define-veccomp-dispatch =)
(define-veccomp-dispatch /=)
(define-veccomp-dispatch <)
(define-veccomp-dispatch <=)
(define-veccomp-dispatch >)
(define-veccomp-dispatch >=)

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
  ((vec-type real) round round))
(define-templated-dispatch !vceiling (x a &optional (divisor 1))
  ((vec-type real) round ceiling))
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

(define-right-reductor v+ 2v+)
(define-right-reductor v- 2v- 1v- v+)
(define-right-reductor v* 2v*)
(define-right-reductor v/ 2v/ 1v/ v*)

(define-alias vlength (a)
  (v2norm a))
(define-alias v1+ (a)
  (v+ a 1))
(define-alias v1- (a)
  (v- a 1))
(define-alias vunit (a)
  (v/ a (vlength a)))
(define-alias vunit* (a)
  (let ((len (vlength a)))
    (if (= 0 len) (vcopy a) (v/ a len))))
(define-alias vscale (a s)
  (let ((x (vunit a)))
    (!2v* x x s)))
