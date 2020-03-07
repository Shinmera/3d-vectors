#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(defmacro define-type-with-converter (name base-type (value) &body conversion)
  `(progn
     (deftype ,name ()
       ',base-type)
     (declaim (inline ,name))
     (declaim (ftype (function (T) ,base-type) ,name))
     (defun ,name (,value)
       ,@conversion)))

(define-type-with-converter f32 single-float (value)
  (float value 0f0))

(define-type-with-converter f64 double-float (value)
  (float value 0d0))

(define-type-with-converter u32 (unsigned-byte 32) (value)
  (check-type value (unsigned-byte 32))
  value)

(define-type-with-converter i32 (signed-byte 32) (value)
  (check-type value (signed-byte 32))
  value)

(defun type-prefix (type)
  (ecase type
    (f32 '||)
    (f64 'd)
    (u32 'u)
    (i32 'i)))

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

(define-type-dispatch vec2 (&optional a b)
  ((null null)
   (%vec2 0f0 0f0))
  ((real real)
   (%vec2 (f32 a) (f32 b)))
  ((real null)
   (%vec2 (f32 a) (f32 a))))

(define-type-dispatch vec3 (&optional a b c)
  ((null null null)
   (%vec3 0f0 0f0 0f0))
  ((real real real)
   (%vec3 (f32 a) (f32 b) (f32 b)))
  ((real null null)
   (%vec3 (f32 a) (f32 a) (f32 a)))
  ((vec2 real null)
   (%vec3 (vx2 a) (vy2 a) (f32 b)))
  ((real vec2 null)
   (%vec3 (f32 a) (vx2 b) (vy2 b)))
  ((vec3 null null)
   (vec3-copy a)))

(define-type-dispatch vec4 (&optional a b c d)
  ((null null null null)
   (%vec4 0f0 0f0 0f0 0f0))
  ((real real real real)
   (%vec4 (f32 a) (f32 b) (f32 b) (f32 c)))
  ((real null null null)
   (%vec4 (f32 a) (f32 a) (f32 a) (f32 a)))
  ((vec2 vec2 null null)
   (%vec4 (vx2 a) (vy2 a) (vx2 b) (vy2 b)))
  ((vec2 real real null)
   (%vec4 (vx2 a) (vy2 a) (f32 b) (f32 c)))
  ((real vec2 real null)
   (%vec4 (f32 a) (vx2 b) (vy2 b) (f32 c)))
  ((real real vec2 null)
   (%vec4 (f32 a) (f32 b) (vx2 c) (vy2 c)))
  ((vec3 real null null)
   (%vec4 (vx3 a) (vy3 a) (vz3 a) (f32 b)))
  ((real vec3 null null)
   (%vec4 (f32 a) (vx3 b) (vy3 b) (vz3 b)))
  ((vec4 null null null)
   (vec4-copy a)))
