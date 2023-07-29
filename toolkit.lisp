#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-3d-vectors-u32 (push :3d-vectors-no-u32 *features*))

(defun enlist (list-ish &rest els)
  (if (listp list-ish) list-ish (list* list-ish els)))

(define-type-with-converter f32 single-float (value)
  (float value 0f0))

(define-type-with-converter f64 double-float (value)
  (float value 0d0))

(define-type-with-converter u32 (unsigned-byte 32) (value)
  (ldb (byte 32 0) (truncate value)))

(define-type-with-converter i32 (signed-byte 32) (value)
  (let ((i (truncate value)))
    (if (<= 0 i)
        (ldb (byte 31 0) i)
        (- (ldb (byte 32 0) (- i))))))

(defun type-prefix (type)
  (ecase type
    (f32 '||)
    (f64 'd)
    (u32 'u)
    (i32 'i)))

(declaim (inline sqr sqr2 grid))
(defun sqr (a)
  (expt a 2))

(defun sqr2 (a b)
  (expt (- a b) 2))

(defun grid (a g)
  (* g (floor (+ a (/ g 2)) g)))

(defun sqrt+ (&rest a)
  (sqrt (apply #'+ a)))

(define-compiler-macro sqrt+ (&rest a)
  `(sqrt (+ ,@a)))

(declaim (inline lerp))
(defun lerp (from to n)
  (declare (optimize speed (safety 0)))
  (+ (* from (- 1 n)) (* to n)))

(declaim (inline clamp))
(defun clamp (min x max)
  (declare (optimize speed (safety 0)))
  (min (max x min) max))

(defun type-random (type low high)
  (ecase type
    (f32 (+ (f32 low) (random (- (f32 high) (f32 low)))))
    (f64 (+ (f64 low) (random (- (f64 high) (f64 low)))))
    (u32 (+ (u32 low) (random (- (u32 high) (u32 low)))))
    (i32 (+ (i32 low) (random (- (i32 high) (i32 low)))))))

(declaim (inline ~=))
(defun ~= (a b &optional (eps 1.0e-6))
  (<= (abs (- a b)) eps))
