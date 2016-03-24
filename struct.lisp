#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(defstruct (vec (:conc-name NIL)
                (:constructor %vec (%vx %vy %vz))
                (:copier vcopy)
                (:predicate vec-p))
  (%vx 0 :type double-float)
  (%vy 0 :type double-float)
  (%vz 0 :type double-float))

(defmacro define-vec-accessor (name accessor)
  `(progn
     (setf (fdefinition ',name)
           (fdefinition ',accessor))
     (defsetf ,name (vec) (value)
       `(setf (,',accessor ,vec) (float ,value 0.0d0)))))

(define-vec-accessor vx %vx)
(define-vec-accessor vy %vy)
(define-vec-accessor vz %vz)

(declaim (inline vec))
(declaim (ftype (function (number number number) vec) vec))
(defun vec (x y z)
  (%vec (float x 0.0d0) (float y 0.0d0) (float z 0.0d0)))

(defun ensure-double-float-param (val env)
  (if (constantp val env)
      (typecase val
        (double-float val)
        (real (float val 0.0d0))
        (T `(load-time-value (float ,val 0.0d0))))
      `(float ,val 0.0d0)))

(define-compiler-macro vec (&whole whole &environment env x y z)
  (let ((nx (ensure-double-float-param x env))
        (ny (ensure-double-float-param y env))
        (nz (ensure-double-float-param z env)))
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
