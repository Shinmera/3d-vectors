#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(defmacro with-vec2 ((x y) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) (,x #.(ensure-float 0)) (,y #.(ensure-float 0)))
       (declare (type ,*float-type* ,x ,y))
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar)))
         (vec2 (setf ,x (vx ,valvar) ,y (vy ,valvar))))
       ,@body)))

(defmacro with-vec3 ((x y z) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) (,x #.(ensure-float 0)) (,y #.(ensure-float 0)) (,z #.(ensure-float 0)))
       (declare (type ,*float-type* ,x ,y ,z))
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar ,z ,valvar)))
         (vec3 (setf ,x (vx ,valvar) ,y (vy ,valvar) ,z (vz ,valvar))))
       ,@body)))

(defmacro with-vec4 ((x y z w) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) (,x #.(ensure-float 0)) (,y #.(ensure-float 0)) (,z #.(ensure-float 0)) (,w #.(ensure-float 0)))
       (declare (type ,*float-type* ,x ,y ,z ,w))
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar ,z ,valvar ,w ,valvar)))
         (vec4 (setf ,x (vx ,valvar) ,y (vy ,valvar) ,z (vz ,valvar) ,w (vw ,valvar))))
       ,@body)))

(defmacro with-vec ((x y &optional (z (gensym "Z")) (w (gensym "W"))) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) (,x #.(ensure-float 0)) (,y #.(ensure-float 0)) (,z #.(ensure-float 0)) (,w #.(ensure-float 0)))
       (declare (type ,*float-type* ,x ,y ,z ,w))
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar ,z ,valvar ,w ,valvar)))
         (vec2 (setf ,x (vx ,valvar) ,y (vy ,valvar)))
         (vec3 (setf ,x (vx ,valvar) ,y (vy ,valvar) ,z (vz ,valvar)))
         (vec4 (setf ,x (vx ,valvar) ,y (vy ,valvar) ,z (vz ,valvar) ,w (vw ,valvar))))
       ,@body)))

(defmacro define-veccomp (name op)
  (let ((2vec-name (intern (format NIL "~a-~a" '2vec name))))
    `(progn
       (declaim (ftype (function ((or vec real) (or vec real)) boolean) ,2vec-name))
       (declaim (ftype (function ((or vec real) &rest (or vec real)) boolean) ,name))
       (declaim (inline ,name ,2vec-name))
       (defun ,2vec-name (a b)
         (etypecase a
           (real (let ((a (ensure-float a)))
                   (typecase b
                     (real (,op a b))
                     (vec4 (and (,op a (vx b)) (,op a (vy b)) (,op a (vz b)) (,op a (vw b))))
                     (vec3 (and (,op a (vx b)) (,op a (vy b)) (,op a (vz b))))
                     (vec2 (and (,op a (vx b)) (,op a (vy b)))))))
           (vec4 (etypecase b
                   (real (let ((b (ensure-float b))) (and (,op (vx a) b) (,op (vy a) b) (,op (vz a) b) (,op (vw a) b))))
                   (vec4 (and (,op (vx a) (vx b)) (,op (vy a) (vy b)) (,op (vz a) (vz b)) (,op (vw a) (vw b))))))
           (vec3 (etypecase b
                   (real (let ((b (ensure-float b))) (and (,op (vx a) b) (,op (vy a) b) (,op (vz a) b))))
                   (vec3 (and (,op (vx a) (vx b)) (,op (vy a) (vy b)) (,op (vz a) (vz b))))))
           (vec2 (etypecase b
                   (real (let ((b (ensure-float b))) (and (,op (vx a) b) (,op (vy a) b))))
                   (vec2 (and (,op (vx a) (vx b)) (,op (vy a) (vy b))))))))
       (defun ,name (val &rest vals)
         (loop for prev = val then next
               for next in vals
               always (,2vec-name prev next)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 T)
           (1 `(,',2vec-name ,val ,(first vals)))
           (T `(and ,@(loop for prev = val then next
                            for next in vals
                            collect `(,',2vec-name ,prev ,next)))))))))

(define-veccomp v= =)
(define-veccomp v/= /=)
(define-veccomp v< <)
(define-veccomp v<= <=)
(define-veccomp v> >)
(define-veccomp v>= >=)

(defmacro define-vecreduce (name op)
  (let ((2vec-name (intern (format NIL "~a-~a" '2vec name))))
    `(progn
       (declaim (ftype (function ((or vec real) (or vec real)) vec) ,2vec-name))
       (declaim (ftype (function ((or vec real) &rest (or vec real)) vec) ,name))
       (declaim (inline ,name ,2vec-name))
       (defun ,2vec-name (a b)
         (etypecase a
           (real (etypecase b
                   (vec4 (vec4 (,op a (vx b)) (,op a (vy b)) (,op a (vz b)) (,op a (vw b))))
                   (vec3 (vec3 (,op a (vx b)) (,op a (vy b)) (,op a (vz b))))
                   (vec2 (vec2 (,op a (vx b)) (,op a (vy b))))))
           (vec4 (with-vec4 (x y z w) a
                   (with-vec4 (bx by bz bw) b
                     (vec4 (,op x bx) (,op y by) (,op z bz) (,op w bw)))))
           (vec3 (with-vec3 (x y z) a
                   (with-vec3 (bx by bz) b
                     (vec3 (,op x bx) (,op y by) (,op z bz)))))
           (vec2 (with-vec2 (x y) a
                   (with-vec2 (bx by) b
                     (vec2 (,op x bx) (,op y by)))))))
       (defun ,name (val &rest vals)
         (loop for res = val then (,2vec-name res next)
               for next in vals
               finally (return res)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 (vcopy val))
           (1 `(,',2vec-name val ,(first vals)))
           (T `(,',2vec-name val (,',name ,@(rest vals)))))))))

(define-vecreduce vmin min)
(define-vecreduce vmax max)

(defmacro define-vector-constant (name x y &optional z w)
  `(defconstant ,name (cond ((not (boundp ',name))
                             (vec ,x ,y ,z ,w))
                            ((v= (symbol-value ',name) (vec ,x ,y ,z ,w))
                             (symbol-value ',name))
                            (T (error "Attempting to redefine constant vector ~a with value ~a to ~a."
                                      ',name (symbol-value ',name) (vec ,x ,y ,z ,w))))))
(define-vector-constant +vx2+ 1 0)
(define-vector-constant +vy2+ 0 1)

(define-vector-constant +vx3+ 1 0 0)
(define-vector-constant +vy3+ 0 1 0)
(define-vector-constant +vz3+ 0 0 1)

(define-vector-constant +vx4+ 1 0 0 0)
(define-vector-constant +vy4+ 0 1 0 0)
(define-vector-constant +vz4+ 0 0 1 0)
(define-vector-constant +vw4+ 0 0 0 1)

;; backwards-compat
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

(defmacro vmodf (&environment env vec op &optional x y z)
  (let ((v (gensym "VEC")))
    `(let ((,v ,vec))
       (psetf (vx ,v) (,op (vx ,v) ,@(when x (list (ensure-float-param x env))))
              (vy ,v) (,op (vy ,v) ,@(when y (list (ensure-float-param y env))))
              (vz ,v) (,op (vz ,v) ,@(when z (list (ensure-float-param z env)))))
       ,v)))

(defmacro %vecop-internal (&environment env op el x y z)
  `(etypecase ,el
     (#.*float-type*
      (setf ,x (,op ,el ,x))
      (setf ,y (,op ,el ,y))
      (setf ,z (,op ,el ,z)))
     (real
      (setf ,x (,op ,x ,(ensure-float-param el env)))
      (setf ,y (,op ,y ,(ensure-float-param el env)))
      (setf ,z (,op ,z ,(ensure-float-param el env))))
     (vec
      (setf ,x (,op (vx ,el) ,x))
      (setf ,y (,op (vy ,el) ,y))
      (setf ,z (,op (vz ,el) ,z)))))

;; FIXME: Generate compiler macros to optimise to two-arg- versions.
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
      vec)))

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
    vec))

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
                    +vx+ (vx b))
              +vy+ (vy b))
        +vz+ (vz b)))

(declaim (inline nvrotv))
(declaim (ftype (function (vec vec) vec) nvrotv))
(defun nvrotv (a b)
  (nvrot (nvrot (nvrot a
                       +vx+ (vx b))
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
