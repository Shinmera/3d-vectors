#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(define-template-type vec (<s> <t>)
    (compose-name NIL (type-prefix <t>) 'vec <s>)
  (loop for i from 0 below <s>
        for f in '(x y z w)
        do (field (compose-name NIL (type-prefix <t>) 'v f <s>)
                  :type <t>
                  :alias (list i f))))

(defmacro do-vec-combinations (template &rest other-template-args)
  `(do-combinations ,template ,@other-template-args (2 3 4) (f32 f64 u32 i32)))

(do-vec-combinations define-vec)

;; This reads like a war zone
;; FIXME: The deftransforms on REALs clobber more precise type information.
;; FIXME: Convert between vecs
(defmacro define-vec-constructors (type)
  (flet ((constructor (size)
           (constructor (type-instance 'vec-type size type)))
         (type (size)
           (lisp-type (type-instance 'vec-type size type)))
         (place (size i)
           (place (type-instance 'vec-type size type) i)))
    `(progn
       (define-type-dispatch ,(compose-name NIL (type-prefix type) 'vec 2) (&optional a b)
         ((null null) ,(type 2)
          (,(constructor 2) (,type 0) (,type 0)))
         ((real real) ,(type 2)
          (,(constructor 2) (,type a) (,type b)))
         ((real null) ,(type 2)
          (,(constructor 2) (,type a) (,type a)))
         ((,(type 2) null) ,(type 2)
          (,(compose-name NIL (type-prefix type) 'vec 2 '-copy) a)))

       (define-type-dispatch ,(compose-name NIL (type-prefix type) 'vec 3) (&optional a b c)
         ((null null null) ,(type 3)
          (,(constructor 3) (,type 0) (,type 0) (,type 0)))
         ((real real real) ,(type 3)
          (,(constructor 3) (,type a) (,type b) (,type b)))
         ((real null null) ,(type 3)
          (,(constructor 3) (,type a) (,type a) (,type a)))
         ((,(type 2) real null) ,(type 3)
          (,(constructor 3) (,(place 2 'x) a) (,(place 2 'y) a) (,type b)))
         ((real ,(type 2) null) ,(type 3)
          (,(constructor 3) (,type a) (,(place 2 'x) b) (,(place 2 'y) b)))
         ((,(type 3) null null) ,(type 3)
          (,(compose-name NIL (type-prefix type) 'vec 3 '-copy) a)))

       (define-type-dispatch ,(compose-name NIL (type-prefix type) 'vec 4) (&optional a b c d)
         ((null null null null) ,(type 4)
          (,(constructor 4) (,type 0) (,type 0) (,type 0) (,type 0)))
         ((real real real real) ,(type 4)
          (,(constructor 4) (,type a) (,type b) (,type b) (,type c)))
         ((real null null null) ,(type 4)
          (,(constructor 4) (,type a) (,type a) (,type a) (,type a)))
         ((,(type 2) ,(type 2) null null) ,(type 4)
          (,(constructor 4) (,(place 2 'x) a) (,(place 2 'y) a) (,(place 2 'x) b) (,(place 2 'y) b)))
         ((,(type 2) real real null) ,(type 4)
          (,(constructor 4) (,(place 2 'x) a) (,(place 2 'y) a) (,type b) (,type c)))
         ((real ,(type 2) real null) ,(type 4)
          (,(constructor 4) (,type a) (,(place 2 'x) b) (,(place 2 'y) b) (,type c)))
         ((real real ,(type 2) null) ,(type 4)
          (,(constructor 4) (,type a) (,type b) (,(place 2 'x) c) (,(place 2 'y) c)))
         ((,(type 3) real null null) ,(type 4)
          (,(constructor 4) (,(place 3 'x) a) (,(place 3 'y) a) (,(place 3 'z) a) (,type b)))
         ((real ,(type 3) null null) ,(type 4)
          (,(constructor 4) (,type a) (,(place 3 'x) b) (,(place 3 'y) b) (,(place 3 'z) b)))
         ((,(type 4) null null null) ,(type 4)
          (,(compose-name NIL (type-prefix type) 'vec 4 '-copy) a)))

       (define-type-dispatch ,(compose-name NIL (type-prefix type) 'vec) (&optional a b c d)
         ((real real null null) ,(type 2)
          (,(constructor 2) (,type a) (,type b)))
         ((real real real null) ,(type 3)
          (,(constructor 3) (,type a) (,type b) (,type c)))
         ((real real real real) ,(type 4)
          (,(constructor 4) (,type a) (,type b) (,type c) (,type d)))
         ((,(type 2) real null null) ,(type 3)
          (,(constructor 3) (,(place 2 'x) a) (,(place 2 'y) a) (,type b)))
         ((real ,(type 2) null null) ,(type 3)
          (,(constructor 3) (,type a) (,(place 2 'x) b) (,(place 2 'y) b)))
         ((,(type 2) ,(type 2) null null) ,(type 4)
          (,(constructor 4) (,(place 2 'x) a) (,(place 2 'y) a) (,(place 2 'x) b) (,(place 2 'y) b)))
         ((,(type 2) real real null) ,(type 4)
          (,(constructor 4) (,(place 2 'x) a) (,(place 2 'y) a) (,type b) (,type c)))
         ((real ,(type 2) real null) ,(type 4)
          (,(constructor 4) (,type a) (,(place 2 'x) b) (,(place 2 'y) b) (,type c)))
         ((real real ,(type 2) null) ,(type 4)
          (,(constructor 4) (,type a) (,type b) (,(place 2 'x) c) (,(place 2 'y) c)))
         ((,(type 3) real null null) ,(type 4)
          (,(constructor 4) (,(place 3 'x) a) (,(place 3 'y) a) (,(place 3 'z) a) (,type b)))
         ((real ,(type 3) null null) ,(type 4)
          (,(constructor 4) (,type a) (,(place 3 'x) b) (,(place 3 'y) b) (,(place 3 'z) b)))))))

(define-vec-constructors f32)
(define-vec-constructors f64)
(define-vec-constructors u32)
(define-vec-constructors i32)

(defmacro define-vec-accessor (name i)
  (let ((instances (loop for instance in (instances 'vec-type)
                         when (< i (<s> instance))
                         collect instance)))
    `(progn
       (define-type-dispatch ,name (vec)
         ,@(loop for type in instances
                 collect `((,(lisp-type type)) ,(<t> type)
                           (,(place type i) vec))))
       (define-type-dispatch (setf ,name) (value vec)
         ,@(loop for type in instances
                 collect `((,(<t> type) ,(lisp-type type)) ,(<t> type)
                           (setf (,(place type i) vec) value)))))))

(define-vec-accessor vx 0)
(define-vec-accessor vy 1)
(define-vec-accessor vz 2)
(define-vec-accessor vw 3)

(deftype vec () '(or vec2 vec3 vec4))
(deftype dvec () '(or dvec2 dvec3 dvec4))
(deftype ivec () '(or ivec2 ivec3 ivec4))
(deftype uvec () '(or uvec2 uvec3 uvec4))
(deftype *vec () '(or vec dvec ivec uvec))

