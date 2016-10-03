#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(defmacro define-vecx-accessor (name rel)
  `(progn
     (declaim (inline ,name))
     (declaim (ftype (function (vec) ,*float-type*) ,name))
     (defun ,name (vec) (,rel vec))
     #-(or ecl ccl)
     (defsetf ,name (&environment env vec) (value)
       `(setf (,',rel ,vec) ,(ensure-float-param value env)))
     ;; I don't know why they don't like the &environment, it is allowed per spec.
     #+(or ecl ccl)
     (defsetf ,name (vec) (value)
       `(setf (,',rel ,vec) ,(ensure-float-param value NIL)))))

(defstruct (vec2 (:conc-name NIL)
                 (:constructor %vec2 (%vx2 %vy2))
                 (:copier vcopy2)
                 (:predicate vec2-p))
  (%vx2 (ensure-float 0) :type #.*float-type*)
  (%vy2 (ensure-float 0) :type #.*float-type*))

(define-vecx-accessor vx2 %vx2)
(define-vecx-accessor vy2 %vy2)

(declaim (inline vec2))
(declaim (ftype (function (real real) vec2) vec2))
(define-ofun vec2 (x y)
  (%vec2 (ensure-float x) (ensure-float y)))

(defmethod print-object ((v vec2) stream)
  (write (make-load-form v) :stream stream))

(defmethod make-load-form ((v vec2) &optional env)
  (declare (ignore env))
  `(vec2 ,(vx2 v) ,(vy2 v)))

(define-compiler-macro vec2 (&whole whole &environment env x y)
  `(%vec2 ,(ensure-float-param x env)
          ,(ensure-float-param y env)))

(defstruct (vec3 (:conc-name NIL)
                 (:constructor %vec3 (%vx3 %vy3 %vz3))
                 (:copier vcopy3)
                 (:predicate vec3-p))
  (%vx3 (ensure-float 0) :type #.*float-type*)
  (%vy3 (ensure-float 0) :type #.*float-type*)
  (%vz3 (ensure-float 0) :type #.*float-type*))

(define-vecx-accessor vx3 %vx3)
(define-vecx-accessor vy3 %vy3)
(define-vecx-accessor vz3 %vz3)

(declaim (inline vec3))
(declaim (ftype (function (real real real) vec3) vec3))
(define-ofun vec3 (x y z)
  (%vec3 (ensure-float x) (ensure-float y) (ensure-float z)))

(define-compiler-macro vec3 (&whole whole &environment env x y z)
  `(%vec3 ,(ensure-float-param x env)
          ,(ensure-float-param y env)
          ,(ensure-float-param z env)))

(defmethod print-object ((v vec3) stream)
  (write (make-load-form v) :stream stream))

(defmethod make-load-form ((v vec3) &optional env)
  (declare (ignore env))
  `(vec3 ,(vx3 v) ,(vy3 v) ,(vz3 v)))

(defstruct (vec4 (:conc-name NIL)
                 (:constructor %vec4 (%vx4 %vy4 %vz4 %vw4))
                 (:copier vcopy4)
                 (:predicate vec4-p))
  (%vx4 (ensure-float 0) :type #.*float-type*)
  (%vy4 (ensure-float 0) :type #.*float-type*)
  (%vz4 (ensure-float 0) :type #.*float-type*)
  (%vw4 (ensure-float 0) :type #.*float-type*))

(define-vecx-accessor vx4 %vx4)
(define-vecx-accessor vy4 %vy4)
(define-vecx-accessor vz4 %vz4)
(define-vecx-accessor vw4 %vw4)

(declaim (inline vec4))
(declaim (ftype (function (real real real real) vec4) vec4))
(define-ofun vec4 (x y z w)
  (%vec4 (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)))

(define-compiler-macro vec4 (&whole whole &environment env x y z w)
  `(%vec4 ,(ensure-float-param x env)
          ,(ensure-float-param y env)
          ,(ensure-float-param z env)
          ,(ensure-float-param w env)))

(defmethod print-object ((v vec4) stream)
  (write (make-load-form v) :stream stream))

(defmethod make-load-form ((v vec4) &optional env)
  (declare (ignore env))
  `(vec4 ,(vx4 v) ,(vy4 v) ,(vz4 v) ,(vw4 v)))

;; Backwards-compat
(deftype vec () '(or vec2 vec3 vec4))

(defmacro define-vec-accessor (name a2 a3 a4)
  `(progn
     (declaim (inline ,name))
     (declaim (ftype (function (vec) ,*float-type*)))
     (define-ofun ,name (vec)
       (etypecase vec
         ,@(when a3 `((vec3 (,a3 vec))))
         ,@(when a4 `((vec4 (,a4 vec))))
         ,@(when a2 `((vec2 (,a2 vec))))))
     #+(or ecl ccl)
     (defsetf ,name (vec) (value)
       `(etypecase ,vec
          ,@(when ',a3 `((vec3 (setf (,',a3 ,vec) ,(ensure-float-param value NIL)))))
          ,@(when ',a4 `((vec4 (setf (,',a4 ,vec) ,(ensure-float-param value NIL)))))
          ,@(when ',a2 `((vec2 (setf (,',a2 ,vec) ,(ensure-float-param value NIL)))))))
     #-(or ecl ccl)
     (defsetf ,name (&environment env vec) (value)
       `(etypecase ,vec
          ,@(when ',a3 `((vec3 (setf (,',a3 ,vec) ,(ensure-float-param value env)))))
          ,@(when ',a4 `((vec4 (setf (,',a4 ,vec) ,(ensure-float-param value env)))))
          ,@(when ',a2 `((vec2 (setf (,',a2 ,vec) ,(ensure-float-param value env)))))))))

(define-vec-accessor vx %vx2 %vx3 %vx4)
(define-vec-accessor vy %vy2 %vy3 %vy4)
(define-vec-accessor vz NIL  %vz3 %vz4)
(define-vec-accessor vw NIL   NIL %vw4)

(declaim (inline vec))
(define-ofun vec-p (vec)
  (typecase vec (vec2 T) (vec3 T) (vec4 T)))

(declaim (inline vcopy))
(define-ofun vcopy (vec)
  (etypecase vec
    (vec2 (vec2 (vx2 vec) (vy2 vec)))
    (vec3 (vec3 (vx3 vec) (vy3 vec) (vz3 vec)))
    (vec4 (vec4 (vx4 vec) (vy4 vec) (vz4 vec) (vw4 vec)))))

(declaim (inline vec))
#+ecl (declaim (ftype (function (real real &optional (or null real) (or null real)) vec) vec))
#-ecl (declaim (ftype (function (real real &optional real real) vec) vec))
(define-ofun vec (x y &optional z w)
  (cond (w (%vec4 (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)))
        (z (%vec3 (ensure-float x) (ensure-float y) (ensure-float z)))
        (T (%vec2 (ensure-float x) (ensure-float y)))))

(define-compiler-macro vec (&environment env x y &optional z w)
  (cond (w `(%vec4 ,(ensure-float-param x env) ,(ensure-float-param y env) ,(ensure-float-param z env) ,(ensure-float-param w env)))
        (z `(%vec3 ,(ensure-float-param x env) ,(ensure-float-param y env) ,(ensure-float-param z env)))
        (T `(%vec2 ,(ensure-float-param x env) ,(ensure-float-param y env)))))
