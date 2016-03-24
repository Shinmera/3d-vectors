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

(defstruct (vec (:conc-name NIL)
                (:constructor %vec (%vx %vy %vz))
                (:copier vcopy)
                (:predicate vec-p))
  (%vx 0 :type #.*float-type*)
  (%vy 0 :type #.*float-type*)
  (%vz 0 :type #.*float-type*))

(defmacro define-vec-accessor (name accessor)
  `(progn
     (setf (fdefinition ',name)
           (fdefinition ',accessor))
     (defsetf ,name (vec) (value)
       `(setf (,',accessor ,vec) (ensure-float ,value)))))

(define-vec-accessor vx %vx)
(define-vec-accessor vy %vy)
(define-vec-accessor vz %vz)

(declaim (inline vec))
(declaim (ftype (function (number number number) vec) vec))
(defun vec (x y z)
  (%vec (ensure-float x) (ensure-float y) (ensure-float z)))

(define-compiler-macro vec (&whole whole &environment env x y z)
  (let ((nx (ensure-float-param x env))
        (ny (ensure-float-param y env))
        (nz (ensure-float-param z env)))
    (if (not (and (eq nx x) (eq ny y) (eq nz z)))
        `(%vec ,nx ,ny ,nz)
        whole)))

(defmethod print-object ((v vec) stream)
  (if (eql (type-of v) 'vec)
      (write (make-load-form v) :stream stream)
      (call-next-method)))

(defmethod make-load-form ((v vec) &optional env)
  (declare (ignore env))
  `(vec ,(vx v) ,(vy v) ,(vz v)))
