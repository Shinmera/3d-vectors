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

(defmacro %2vec-op (a b combination v2red &optional (v3red v2red) (v4red v2red))
  (let ((v2red (if (listp v2red) v2red (list v2red)))
        (v3red (if (listp v3red) v3red (list v3red)))
        (v4red (if (listp v4red) v4red (list v4red))))
    `(etypecase ,a
       (real (let ((,a (ensure-float ,a)))
               (etypecase ,b
                 (vec4 (,@v4red (,combination ,a (vx b)) (,combination a (vy b)) (,combination a (vz b)) (,combination a (vw b))))
                 (vec3 (,@v3red (,combination ,a (vx b)) (,combination a (vy b)) (,combination a (vz b))))
                 (vec2 (,@v2red (,combination ,a (vx b)) (,combination a (vy b)))))))
       (vec4 (etypecase b
               (real (let ((b (ensure-float b))) (,@v4red (,combination (vx a) b) (,combination (vy a) b) (,combination (vz a) b) (,combination (vw a) b))))
               (vec4 (,@v4red (,combination (vx a) (vx b)) (,combination (vy a) (vy b)) (,combination (vz a) (vz b)) (,combination (vw a) (vw b))))))
       (vec3 (etypecase b
               (real (let ((b (ensure-float b))) (,@v3red (,combination (vx a) b) (,combination (vy a) b) (,combination (vz a) b))))
               (vec3 (,@v3red (,combination (vx a) (vx b)) (,combination (vy a) (vy b)) (,combination (vz a) (vz b))))))
       (vec2 (etypecase b
               (real (let ((b (ensure-float b))) (,@v2red (,combination (vx a) b) (,combination (vy a) b))))
               (vec2 (,@v2red (,combination (vx a) (vx b)) (,combination (vy a) (vy b)))))))))

(defmacro define-veccomp (name op)
  (let ((2vec-name (intern (format NIL "~a-~a" '2vec name))))
    `(progn
       (declaim (ftype (function ((or vec real) (or vec real)) boolean) ,2vec-name))
       (declaim (ftype (function ((or vec real) &rest (or vec real)) boolean) ,name))
       (declaim (inline ,name ,2vec-name))
       (defun ,2vec-name (a b)
         (%2vec-op a b ,op and and and))
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
         (%2vec-op a b ,op vec2 vec3 vec4))
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
  (etypecase v
    (vec2 (sqrt (+ (expt (vx v) 2)
                   (expt (vy v) 2))))
    (vec3 (sqrt (+ (expt (vx v) 2)
                   (expt (vy v) 2)
                   (expt (vz v) 2))))
    (vec4 (sqrt (+ (expt (vx v) 2)
                   (expt (vy v) 2)
                   (expt (vz v) 2)
                   (expt (vw v) 2))))))

(defmacro %vsetf (&environment env v x y &optional z w)
  `(progn ,(cond (w `(psetf (vx ,v) ,(ensure-float-param x env)
                            (vy ,v) ,(ensure-float-param y env)
                            (vz ,v) ,(ensure-float-param z env)
                            (vw ,v) ,(ensure-float-param w env)))
                 (z `(psetf (vx ,v) ,(ensure-float-param x env)
                            (vy ,v) ,(ensure-float-param y env)
                            (vz ,v) ,(ensure-float-param z env)))
                 (T `(psetf (vx ,v) ,(ensure-float-param x env)
                            (vy ,v) ,(ensure-float-param y env))))
          ,v))

(defmacro vsetf (vec x y &optional z w)
  (let ((v (gensym "VEC")))
    `(let ((,v ,vec))
       (etypecase ,v
         (vec2 (%vsetf ,v ,x ,y))
         (vec3 (%vsetf ,v ,x ,y ,z))
         (vec4 (%vsetf ,v ,x ,y ,z ,w)))
       ,v)))

(defmacro vmod (&environment env vec op &optional x y z w)
  (let ((v (gensym "VEC")))
    `(let ((,v ,vec))
       (etypecase ,v
         (vec2 (vec2 (,op (vx ,v) ,@(when x (list (ensure-float-param x env))))
                     (,op (vy ,v) ,@(when y (list (ensure-float-param y env))))))
         (vec3 (vec3 (,op (vx ,v) ,@(when x (list (ensure-float-param x env))))
                     (,op (vy ,v) ,@(when y (list (ensure-float-param y env))))
                     (,op (vz ,v) ,@(when z (list (ensure-float-param z env))))))
         (vec4 (vec4 (,op (vx ,v) ,@(when x (list (ensure-float-param x env))))
                     (,op (vy ,v) ,@(when y (list (ensure-float-param y env))))
                     (,op (vz ,v) ,@(when z (list (ensure-float-param z env))))
                     (,op (vw ,v) ,@(when w (list (ensure-float-param w env))))))))))

(defmacro vmodf (&environment env vec op &optional x y z w)
  (let ((v (gensym "VEC")))
    `(let ((,v ,vec))
       (vsetf ,v (,op (vx ,v) ,@(when x (list (ensure-float-param x env))))
              (,op (vy ,v) ,@(when y (list (ensure-float-param y env))))
              (,op (vz ,v) ,@(when z (list (ensure-float-param z env))))
              (,op (vw ,v) ,@(when w (list (ensure-float-param w env)))))
       ,v)))

(defmacro define-vecop (name nname op)
  (let ((2vec-name (intern (format NIL "~a-~a" '2vec name))))
    `(progn
       (declaim (inline ,name ,2vec-name))
       (declaim (ftype (function ((or vec real) &rest (or vec real)) vec) ,name))
       (declaim (ftype (function ((or vec real) (or vec real)) vec) ,2vec-name))
       (defun ,2vec-name (a b)
         (%2vec-op a b ,op vec2 vec3 vec4))
       (defun ,name (val &rest vals)
         (cond ((cdr vals)
                (apply #',nname (,2vec-name val (first vals)) (rest vals)))
               (vals (,2vec-name val (first vals)))
               (T (vmod val ,op))))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(vmod ,val ,',op))
           (1 `(,',2vec-name ,val ,(first vals)))
           (T `(,',nname (,',2vec-name ,val ,(first val)) ,@(rest val))))))))

(defmacro define-nvecop (name op)
  (let ((2vec-name (intern (format NIL "~a-~a" '2vec name))))
    `(progn
       (declaim (inline ,name ,2vec-name))
       (declaim (ftype (function (vec &rest (or vec real)) vec) ,name))
       (declaim (ftype (function (vec (or vec real)) vec) ,2vec-name))
       (defun ,2vec-name (a b)
         (%2vec-op a b ,op (%vsetf a)))
       (defun ,name (val &rest vals)
         (if vals
             (loop for v in vals
                   do (,2vec-name val v)
                   finally (return val))
             (vmodf val ,op)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(vmodf ,val ,',op))
           (1 `(,',2vec-name ,val ,(first vals)))
           (T `(,',name (,',2vec-name ,val ,(first vals)) ,@(rest vals))))))))

(define-vecop v+ nv+ +)
(define-vecop v- nv- -)
(define-vecop v* nv* *)
(define-vecop v/ nv/ /)
(define-nvecop nv+ +)
(define-nvecop nv- -)
(define-nvecop nv* *)
(define-nvecop nv/ /)

(declaim (inline v1+))
(declaim (ftype (function (vec) vec) v1+))
(defun v1+ (v)
  (v+ v 1))

(declaim (inline v1-))
(declaim (ftype (function (vec) vec) v1-))
(defun v1- (v)
  (v- v 1))

(defmacro vincf (&environment env v &optional (delta 1))
  `(nv+ ,v ,(ensure-float-param delta env)))

(defmacro vdecf (&environment env v &optional (delta 1))
  `(nv- ,v ,(ensure-float-param delta env)))

(declaim (inline v.))
(declaim (ftype (function (vec vec) #.*float-type*) v.))
(defun v. (a b)
  (etypecase a
    (vec2 (+ (* (vx a) (vx b))
             (* (vy a) (vy b))))
    (vec3 (+ (* (vx a) (vx b))
             (* (vy a) (vy b))
             (* (vz a) (vz b))))
    (vec4 (+ (* (vx a) (vx b))
             (* (vy a) (vy b))
             (* (vz a) (vz b))
             (* (vw a) (vw b))))))

(declaim (inline vc))
(declaim (ftype (function (vec3 vec3) vec3) vc))
(defun vc (a b)
  (vec3 (- (* (vy a) (vz b))
           (* (vz a) (vy b)))
        (- (* (vz a) (vx b))
           (* (vx a) (vz b)))
        (- (* (vx a) (vy b))
           (* (vy a) (vx b)))))

(declaim (inline vabs))
(declaim (ftype (function (vec) vec) vabs))
(defun vabs (vec)
  (vmod vec abs))

(declaim (inline nvabs))
(declaim (ftype (function (vec) vec) nvabs))
(defun nvabs (vec)
  (vmodf vec abs))

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
(declaim (ftype (function (vec3 vec3 real) vec3) vrot))
(defun vrot (v axis phi)
  (%vecrot-internal
    (vec (arith vx) (arith vy) (arith vz))))

(declaim (inline nvrot))
(declaim (ftype (function (vec3 vec3 real) vec3) nvrot))
(defun nvrot (v axis phi)
  ;; https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  ;; vr = v*cos(phi) + (kxv)*sin(phi) + k*(k*v)*(1-cos(phi)
  (%vecrot-internal
    (setf (vx v) (arith vx)
          (vy v) (arith vy)
          (vz v) (arith vz))
    v))

(declaim (inline vrotv))
(declaim (ftype (function (vec3 vec3) vec3) vrotv))
(defun vrotv (a b)
  (vrot (vrot (vrot a
                    +vx+ (vx b))
              +vy+ (vy b))
        +vz+ (vz b)))

(declaim (inline nvrotv))
(declaim (ftype (function (vec3 vec3) vec3) nvrotv))
(defun nvrotv (a b)
  (nvrot (nvrot (nvrot a
                       +vx+ (vx b))
                +vy+ (vy b))
         +vz+ (vz b)))

(declaim (ftype (function (vec symbol &optional symbol symbol symbol) vec) vorder))
(defun vorder (v x &optional y z w)
  (flet ((component (n)
           (ecase n
             ((vx x :x) (vx v))
             ((vy y :y) (vy v))
             ((vz z :z) (vz v))
             ((vw w :w) (vw v))
             ((NIL) 0.0))))
    (etypecase v
      (vec2 (vec2 (component x) (component y)))
      (vec3 (vec3 (component x) (component y) (component z)))
      (vec4 (vec4 (component x) (component y) (component z) (component w))))))

(define-compiler-macro vorder (&environment env vec x &optional y z w)
  (let ((v (gensym "VEC")))
    (flet ((component (n)
             (if (constantp n env)
                 (ecase n
                   ((vx x :x) `(vx ,v))
                   ((vy y :y) `(vy ,v))
                   ((vz z :z) `(vz ,v))
                   ((vw w :w) `(vw ,v))
                   ((NIL) 0.0))
                 `(ecase ,n
                    ((vx x :x) (vx v))
                    ((vy y :y) (vy v))
                    ((vz z :z) (vz v))
                    ((vw w :w) (vw v))
                    ((NIL) 0.0)))))
      `(let ((,v ,vec))
         (etypecase ,v
           (vec2 (vec ,(component x) ,(component y)))
           (vec3 (vec ,(component x) ,(component y) ,(component z)))
           (vec4 (vec ,(component x) ,(component y) ,(component z) ,(component w))))))))

(declaim (ftype (function (vec symbol &optional symbol symbol symbol) vec) vorder))
(defun nvorder (v x &optional y z w)
  (flet ((component (n)
           (ecase n
             ((vx x :x) (vx v))
             ((vy y :y) (vy v))
             ((vz z :z) (vz v))
             ((vw w :w) (vw v))
             ((NIL) 0.0))))
    (vsetf v (component x) (component y) (component z) (component w))))

(define-compiler-macro nvorder (&environment env vec x &optional y z w)
  (let ((v (gensym "VEC")))
    (flet ((component (n)
             (if (constantp n env)
                 (ecase n
                   ((vx x :x) `(vx ,v))
                   ((vy y :y) `(vy ,v))
                   ((vz z :z) `(vz ,v))
                   ((vw w :w) `(vw ,v))
                   ((NIL) 0.0))
                 `(ecase ,n
                    ((vx x :x) (vx v))
                    ((vy y :y) (vy v))
                    ((vz z :z) (vz v))
                    ((vw w :w) (vw v))
                    ((NIL) 0.0)))))
      `(let ((,v ,vec))
         (vsetf ,v ,(component x) ,(component y) ,(component z) ,(component w))))))
