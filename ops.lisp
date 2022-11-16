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

;; TODO: order swizzle with-vec constants
