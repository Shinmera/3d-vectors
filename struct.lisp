#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

#+3d-vectors-double-floats (pushnew :3d-vectors-double-floats *features*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *float-type*
    #+3d-vectors-double-floats 'double-float
    #-3d-vectors-double-floats 'single-float))

(declaim (inline ensure-float))
(declaim (ftype (function (real) #.*float-type*)))
(defun ensure-float (thing)
  (coerce thing '#.*float-type*))

(defun ensure-float-param (val env)
  (if (constantp val env)
      (typecase val
        (real (ensure-float val))
        (T `(load-time-value (ensure-float ,val))))
      `(ensure-float ,val)))

(defstruct (vec2 (:conc-name NIL)
                 (:constructor %vec2 (%vx %vy))
                 (:copier vcopy2)
                 (:predicate vec2-p))
  (%vx (ensure-float 0) :type #.*float-type*)
  (%vy (ensure-float 0) :type #.*float-type*))

(declaim (inline vec2))
(declaim (ftype (function (real real) vec2) vec2))
(defun vec2 (x y)
  (%vec (ensure-float x) (ensure-float y)))

(defmethod print-object ((v vec2) stream)
  (write (make-load-form v) :stream stream))

(defmethod make-load-form ((v vec2) &optional env)
  (declare (ignore env))
  `(vec2 ,(vx v) ,(vy v)))

(define-compiler-macro vec2 (&whole whole &environment env x y)
  `(%vec2 ,(ensure-float-param x env)
          ,(ensure-float-param y env)))

(defstruct (vec3 (:conc-name NIL)
                 (:constructor %vec3 (%vx %vy %vz))
                 (:copier vcopy3)
                 (:predicate vec3-p)
                 (:include vec2))
  (%vz (ensure-float 0) :type #.*float-type*))

(declaim (inline vec3))
(declaim (ftype (function (real real real) vec3) vec3))
(defun vec3 (x y z)
  (%vec3 (ensure-float x) (ensure-float y) (ensure-float z)))

(define-compiler-macro vec3 (&whole whole &environment env x y z)
  `(%vec3 ,(ensure-float-param x env)
          ,(ensure-float-param y env)
          ,(ensure-float-param z env)))

(defmethod make-load-form ((v vec3) &optional env)
  (declare (ignore env))
  `(vec3 ,(vx v) ,(vy v) ,(vz v)))

(defstruct (vec4 (:conc-name NIL)
                 (:constructor %vec4 (%vx %vy %vz %vw))
                 (:copier vcopy)
                 (:predicate vec-p)
                 (:include vec3))
  (%vw (ensure-float 0) :type #.*float-type*))

(declaim (inline vec4))
(declaim (ftype (function (real real real) vec4) vec4))
(defun vec4 (x y z w)
  (%vec4 (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)))

(define-compiler-macro vec4 (&whole whole &environment env x y z w)
  `(%vec4 ,(ensure-float-param x env)
          ,(ensure-float-param y env)
          ,(ensure-float-param z env)
          ,(ensure-float-param w env)))

(defmethod make-load-form ((v vec4) &optional env)
  (declare (ignore env))
  `(vec4 ,(vx v) ,(vy v) ,(vz v) ,(vw v)))

(defmacro define-vec-accessor (name accessor)
  `(progn
     (setf (fdefinition ',name)
           (fdefinition ',accessor))
     (defsetf ,name (vec) (value)
       `(setf (,',accessor ,vec) (ensure-float ,value)))))

(define-vec-accessor vx %vx)
(define-vec-accessor vy %vy)
(define-vec-accessor vz %vz)
(define-vec-accessor vw %vw)

;; Backwards-compat
(deftype vec () 'vec2)

(declaim (inline vec))
(declaim (ftype (function (real real &optional real real) vec) vec))
(defun vec (x y &optional z w)
  (cond (w (%vec4 (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)))
        (z (%vec3 (ensure-float x) (ensure-float y) (ensure-float z)))
        (T (%vec2 (ensure-float x) (ensure-float y)))))

(define-compiler-macro vec (&whole whole &environment env x y &optional z w)
  (cond (w `(%vec4 ,(ensure-float x) ,(ensure-float y) ,(ensure-float z) ,(ensure-float w)))
        (z `(%vec3 ,(ensure-float x) ,(ensure-float y) ,(ensure-float z)))
        (T `(%vec2 ,(ensure-float x) ,(ensure-float y)))))
