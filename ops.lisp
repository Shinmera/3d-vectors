#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(defmacro define-veccomp (name op)
  `(progn
     (declaim (inline ,name))
     (declaim (ftype (function ((or vec real) &rest (or vec real)) boolean) ,name))
     (defun ,name (val &rest vals)
       (let (x y z)
         (etypecase val
           (real (setf x val y val z val))
           (vec (setf x (vx val) y (vy val) z (vz val))))
         (dolist (val vals T)
           (unless (etypecase val
                     (real (and (,op x val) (,op y val) (,op z val)
                                (setf x val y val z val)))
                     (vec (and (,op x (vx val)) (,op y (vy val)) (,op z (vz val))
                               (setf x (vx val) y (vy val) z (vz val)))))
             (return-from ,name NIL)))))))

(define-veccomp v= =)
(define-veccomp v/= /=)
(define-veccomp v< <)
(define-veccomp v<= <=)
(define-veccomp v> >)
(define-veccomp v>= >=)

(defmacro define-vector-constant (name x y &optional (z 0))
  `(defconstant ,name (cond ((not (boundp ',name))
                             (vec ,x ,y ,z))
                            ((v= (symbol-value ',name) (vec ,x ,y ,z))
                             (symbol-value ',name))
                            (T (error "Attempting to redefine constant vector ~a with value ~a to ~a."
                                      ',name (symbol-value ',name) (vec ,x ,y ,z))))))

(define-vector-constant +vx+ 1 0 0)
(define-vector-constant +vy+ 0 1 0)
(define-vector-constant +vz+ 0 0 1)

(declaim (inline vlength))
(declaim (ftype (function (vec) #.*float-type*) vlength))
(defun vlength (v)
  (sqrt (+ (expt (vx v) 2)
           (expt (vy v) 2)
           (expt (vz v) 2))))

(defmacro vsetf (&environment env &rest quads)
  (unless (= 0 (mod (length quads) 4))
    (error "Must supply a balanced set of vectors to three values."))
  (let ((v (gensym "VEC")))
    `(progn
       ,@(loop for (vec x y z) on quads by #'cddddr
               do (setf x (ensure-float-param x env)
                        y (ensure-float-param y env)
                        z (ensure-float-param z env))
               collect `(let ((,v ,vec))
                          (psetf (vx ,v) ,x
                                 (vy ,v) ,y
                                 (vz ,v) ,z)
                          ,v)))))

(defmacro vmodf (vec op &optional x y z)
  (let ((v (gensym "VEC")))
    `(let ((,v ,vec))
       (psetf (vx ,v) (,op (vx ,v) ,@(when x (list x)))
              (vy ,v) (,op (vy ,v) ,@(when y (list y)))
              (vz ,v) (,op (vz ,v) ,@(when z (list z))))
       ,v)))

(defmacro %vecop-internal (op el x y z)
  `(etypecase ,el
     (#.*float-type*
      (setf ,x (,op ,el ,x))
      (setf ,y (,op ,el ,y))
      (setf ,z (,op ,el ,z)))
     (real
      (setf ,x (,op ,el ,x))
      (setf ,y (,op ,el ,y))
      (setf ,z (,op ,el ,z)))
     (vec
      (setf ,x (,op (vx ,el) ,x))
      (setf ,y (,op (vy ,el) ,y))
      (setf ,z (,op (vz ,el) ,z)))))

(defmacro define-vecop1 (name init op)
  (let ((init (ensure-float init)))
    `(progn
       (declaim (inline ,name))
       (declaim (ftype (function (&rest (or vec real)) vec) ,name))
       (defun ,name (&rest vals)
         (let ((x ,init) (y ,init) (z ,init))
           (declare (type #.*float-type* x y z))
           (dolist (v vals (vec x y z))
             (%vecop-internal ,op v x y z)))))))

(defmacro define-vecop2 (name init op comp)
  (let ((init (ensure-float init)))
    `(progn
       (declaim (inline ,name))
       (declaim (ftype (function ((or vec real) &rest (or vec real)) vec) ,name))
       (defun ,name (val &rest vals)
         (let ((x ,init) (y ,init) (z ,init))
           (declare (type #.*float-type* x y z))
           (cond (vals
                  (dolist (v vals)
                    (%vecop-internal ,comp v x y z))
                  (%vecop-internal ,op val x y z))
                 (T
                  (%vecop-internal ,comp val x y z)
                  (setf x (,op x) y (,op y) z (,op z))))
           (vec x y z))))))

(defmacro define-nvecop (name init op &optional comp)
  (let ((init (ensure-float init)))
    `(progn
       (declaim (inline ,name))
       (declaim (ftype (function (vec &rest (or vec real)) vec) ,name))
       (defun ,name (val &rest vals)
         (cond (vals
                ,(if comp
                     `(let ((x ,init) (y ,init) (z ,init))
                        (declare (type #.*float-type* x y z))
                        (dolist (v vals)
                          (%vecop-internal ,comp v x y z))
                        (vmodf val ,op x y z))
                     `(dolist (v vals)
                        (etypecase v
                          (real (vmodf val ,op v v v))
                          (vec (vmodf val ,op (vx v) (vy v) (vz v)))))))
               ,@(when comp
                   `((T
                      (vmodf val ,op)))))
         val))))

(define-vecop1 v+ 0 +)
(define-vecop2 v- 0 - +)
(define-vecop1 v* 1 *)
(define-vecop2 v/ 1 / *)
(define-nvecop nv+ 0 +)
(define-nvecop nv- 0 - +)
(define-nvecop nv* 1 *)
(define-nvecop nv/ 1 / *)

(declaim (inline v1+))
(declaim (ftype (function (vec) vec) v1+))
(defun v1+ (v)
  (vec (1+ (vx v))
       (1+ (vy v))
       (1+ (vz v))))

(declaim (inline v1-))
(declaim (ftype (function (vec) vec) v1-))
(defun v1- (v)
  (vec (1- (vx v))
       (1- (vy v))
       (1- (vz v))))

(defmacro vincf (&environment env v &optional (delta 1))
  (let ((d (gensym "DELTA")))
    `(let ((,d ,(ensure-float-param delta env)))
       (vmodf ,v + ,d ,d ,d))))

(defmacro vdecf (&environment env v &optional (delta 1))
  (let ((d (gensym "DELTA")))
    `(let ((,d ,(ensure-float-param delta env)))
       (vmodf ,v - ,d ,d ,d))))

(declaim (inline v.))
(declaim (ftype (function (vec vec) #.*float-type*) v.))
(defun v. (a b)
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))
     (* (vz a) (vz b))))

(declaim (inline vc))
(declaim (ftype (function (vec vec) vec) vc))
(defun vc (a b)
  (vec (- (* (vy a) (vz b))
          (* (vz a) (vy b)))
       (- (* (vz a) (vx b))
          (* (vx a) (vz b)))
       (- (* (vx a) (vy b))
          (* (vy a) (vx b)))))

(declaim (inline vabs))
(declaim (ftype (function (vec) vec) vabs))
(defun vabs (a)
  (vec (abs (vx a))
       (abs (vy a))
       (abs (vz a))))

(declaim (inline nvabs))
(declaim (ftype (function (vec) vec) nvabs))
(defun nvabs (a)
  (setf (vx a) (abs (vx a))
        (vy a) (abs (vy a))
        (vz a) (abs (vz a)))
  a)

(declaim (inline vunit))
(declaim (ftype (function (vec) vec) vunit))
(defun vunit (a)
  (v/ a (vlength a)))

(declaim (inline nvunit))
(declaim (ftype (function (vec) vec) nvunit))
(defun nvunit (a)
  (nv/ a (vlength a)))

(defmacro %vecrot-internal (&body body)
  ;; https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  ;; vr = v*cos(phi) + (kxv)*sin(phi) + k*(k*v)*(1-cos(phi)
  `(let* ((k axis)
          (cos (cos phi))
          (sin (sin phi))
          (c (vc k v))
          (d (v. k v)))
     (macrolet ((arith (field)
                  `(+ (* (,field v) cos)
                      (* (,field c) sin)
                      (* (,field k) d (- 1 cos)))))
       ,@body)))

(declaim (inline vrot))
(declaim (ftype (function (vec vec real) vec) vrot))
(defun vrot (v axis phi)
  (%vecrot-internal
    (vec (arith vx) (arith vy) (arith vz))))

(declaim (inline nvrot))
(declaim (ftype (function (vec vec real) vec) nvrot))
(defun nvrot (v axis phi)
  ;; https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  ;; vr = v*cos(phi) + (kxv)*sin(phi) + k*(k*v)*(1-cos(phi)
  (%vecrot-internal
    (setf (vx v) (arith vx)
          (vy v) (arith vy)
          (vz v) (arith vz))
    v))

(declaim (inline vrotv))
(declaim (ftype (function (vec vec) vec) vrotv))
(defun vrotv (a b)
  (vrot (vrot (vrot a
                    +vz+ (vx b))
              +vy+ (vy b))
        +vz+ (vz b)))

(declaim (inline nvrotv))
(declaim (ftype (function (vec vec) vec) nvrotv))
(defun nvrotv (a b)
  (nvrot (nvrot (nvrot a
                       +vz+ (vx b))
                +vy+ (vy b))
         +vz+ (vz b)))
