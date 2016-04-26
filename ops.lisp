#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(defmacro with-vec ((x y z) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) ,x ,y ,z)
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar ,z ,valvar)))
         (vec (setf ,x (vx ,valvar) ,y (vy ,valvar) ,z (vz ,valvar))))
       (let ()
         (declare (type ,*float-type* ,x ,y ,z))
         ,@body))))

(defmacro define-veccomp (name op)
  `(progn
     (declaim (inline ,name))
     (declaim (ftype (function ((or vec real) &rest (or vec real)) boolean) ,name))
     (defun ,name (val &rest vals)
       (with-vec (x y z) val
         (dolist (val vals T)
           (with-vec (bx by bz) val
             (unless (and (,op x bx) (,op y by) (,op z by))
               (return-from ,name NIL))))))
     (define-compiler-macro ,name (&whole whole val &rest vals)
       (let ((ax (gensym "AX")) (ay (gensym "AY")) (az (gensym "AZ"))
             (bx (gensym "BX")) (by (gensym "BY")) (bz (gensym "BZ")))
         (case (length vals)
           (0 T)
           (1 `(with-vec (,ax ,ay ,az) ,val
                 (with-vec (,bx ,by ,bz) ,(first vals)
                   (and (,',op ,ax ,bx) (,',op ,ay ,by) (,',op ,az ,bz)))))
           (2 whole))))))

(define-veccomp v= =)
(define-veccomp v/= /=)
(define-veccomp v< <)
(define-veccomp v<= <=)
(define-veccomp v> >)
(define-veccomp v>= >=)

(defmacro define-vecreduce (name op)
  `(progn
     (declaim (inline ,name))
     (declaim (ftype (function ((or vec real) &rest (or vec real)) vec) ,name))
     (defun ,name (val &rest vals)
       (with-vec (x y z) val
         (dolist (val vals (vec x y z))
           (with-vec (bx by bz) val
             (setf x (,op x bx)
                   y (,op y by)
                   z (,op z bz))))))
     (define-compiler-macro ,name (&whole whole val &rest vals)
       (let ((ax (gensym "AX")) (ay (gensym "AY")) (az (gensym "AZ"))
             (bx (gensym "BX")) (by (gensym "BY")) (bz (gensym "BZ")))
         (case (length vals)
           (0 `(with-vec (,ax ,ay ,az) ,val (vec ,ax ,ay ,az)))
           (1 `(with-vec (,ax ,ay ,az) ,val
                 (with-vec (,bx ,by ,bz) ,(first vals)
                   (vec (,',op ,ax ,bx) (,',op ,ay ,by) (,',op ,az ,bz)))))
           (2 whole))))))

(define-vecreduce vmin min)
(define-vecreduce vmax max)

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
(defun nvabs (vec)
  (setf (vx vec) (abs (vx vec))
        (vy vec) (abs (vy vec))
        (vz vec) (abs (vz vec)))
  vec)

(declaim (inline vunit))
(declaim (ftype (function (vec) vec) vunit))
(defun vunit (a)
  (v/ a (vlength a)))

(declaim (inline nvunit))
(declaim (ftype (function (vec) vec) nvunit))
(defun nvunit (vec)
  (nv/ vec (vlength vec)))

(declaim (inline vscale))
(declaim (ftype (function (vec real) vec) vscale))
(defun vscale (a length)
  (nv* (vunit a) length))

(declaim (inline nvscale))
(declaim (ftype (function (vec real) vec) vscale))
(defun nvscale (vec length)
  (nv* (nvunit vec) length))

(declaim (inline vclamp))
(declaim (ftype (function ((or vec real) (or vec real) (or vec real)) vec) vclamp))
(defun vclamp (lower a upper)
  (with-vec (lx ly lz) lower
    (with-vec (x y z) a
      (with-vec (ux uy uz) upper
        (vec (min ux (max lx x))
             (min uy (max ly y))
             (min uz (max lz z)))))))

(declaim (inline nvclamp))
(declaim (ftype (function ((or vec real) vec (or vec real))) nvclamp))
(defun nvclamp (lower vec upper)
  (with-vec (lx ly lz) lower
    (with-vec (ux uy uz) upper
      (setf (vx vec) (min ux (max lx (vx vec)))
            (vy vec) (min uy (max ly (vy vec)))
            (vz vec) (min uz (max lz (vz vec))))
      a)))

(declaim (inline vlimit))
(declaim (ftype (function ((or vec real) (or vec real)) vec) vlimit))
(defun vlimit (a limit)
  (with-vec (x y z) a
    (with-vec (ux uy uz) limit
      (vec (min ux (max (- ux) x))
           (min uy (max (- uy) y))
           (min uz (max (- uz) z))))))

(declaim (inline nvlimit))
(declaim (ftype (function (vec (or vec real)) vec) nvlimit))
(defun nvlimit (vec limit)
  (with-vec (ux uy uz) limit
    (setf (vx vec) (min ux (max (- ux) (vx vec)))
          (vy vec) (min uy (max (- uy) (vy vec)))
          (vz vec) (min uz (max (- uz) (vz vec))))
    a))

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

(declaim (ftype (function (vec symbol &optional symbol symbol) vec) vorder))
(defun vorder (v x &optional y z)
  (flet ((component (n)
           (ecase n
             ((vx x :x) (vx v))
             ((vy y :y) (vy v))
             ((vz z :z) (vz v))
             ((NIL) 0.0))))
    (vec (component x) (component y) (component z))))

(define-compiler-macro vorder (&environment env vec x &optional y z)
  (let ((v (gensym "VEC")))
    (flet ((component (n)
             (if (constantp n env)
                 (ecase n
                   ((vx x :x) `(vx ,v))
                   ((vy y :y) `(vy ,v))
                   ((vz z :z) `(vz ,v))
                   ((NIL) 0.0))
                 `(ecase ,n
                    ((vx x :x) (vx v))
                    ((vy y :y) (vy v))
                    ((vz z :z) (vz v))
                    ((NIL) 0.0)))))
      `(let ((,v ,vec))
         (vec ,(component x) ,(component y) ,(component z))))))

(declaim (ftype (function (vec symbol &optional symbol symbol) vec) vorder))
(defun nvorder (v x &optional y z)
  (flet ((component (n)
           (ecase n
             ((vx x :x) (vx v))
             ((vy y :y) (vy v))
             ((vz z :z) (vz v))
             ((NIL) 0.0))))
    (psetf (vx v) (component x)
           (vy v) (component y)
           (vz v) (component z))
    v))

(define-compiler-macro nvorder (&environment env vec x &optional y z)
  (let ((v (gensym "VEC")))
    (flet ((component (n)
             (if (constantp n env)
                 (ecase n
                   ((vx x :x) `(vx ,v))
                   ((vy y :y) `(vy ,v))
                   ((vz z :z) `(vz ,v))
                   ((NIL) 0.0))
                 `(ecase ,n
                    ((vx x :x) (vx v))
                    ((vy y :y) (vy v))
                    ((vz z :z) (vz v))
                    ((NIL) 0.0)))))
      `(let ((,v ,vec))
         (psetf (vx ,v) ,(component x)
                (vy ,v) ,(component y)
                (vz ,v) ,(component z))
         ,v))))
