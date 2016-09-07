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
         (vec2 (setf ,x (vx2 ,valvar) ,y (vy2 ,valvar))))
       ,@body)))

(defmacro with-vec3 ((x y z) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) (,x #.(ensure-float 0)) (,y #.(ensure-float 0)) (,z #.(ensure-float 0)))
       (declare (type ,*float-type* ,x ,y ,z))
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar ,z ,valvar)))
         (vec3 (setf ,x (vx3 ,valvar) ,y (vy3 ,valvar) ,z (vz3 ,valvar))))
       ,@body)))

(defmacro with-vec4 ((x y z w) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) (,x #.(ensure-float 0)) (,y #.(ensure-float 0)) (,z #.(ensure-float 0)) (,w #.(ensure-float 0)))
       (declare (type ,*float-type* ,x ,y ,z ,w))
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar ,z ,valvar ,w ,valvar)))
         (vec4 (setf ,x (vx4 ,valvar) ,y (vy4 ,valvar) ,z (vz4 ,valvar) ,w (vw4 ,valvar))))
       ,@body)))

(defmacro with-vec ((x y &optional (z (gensym "Z")) (w (gensym "W"))) val &body body)
  (let ((valvar (gensym "VAL")))
    `(let ((,valvar ,val) (,x #.(ensure-float 0)) (,y #.(ensure-float 0)) (,z #.(ensure-float 0)) (,w #.(ensure-float 0)))
       (declare (type ,*float-type* ,x ,y ,z ,w))
       (etypecase ,valvar
         (real (let ((,valvar (ensure-float ,valvar)))
                 (declare (type ,*float-type* ,valvar))
                 (setf ,x ,valvar ,y ,valvar ,z ,valvar ,w ,valvar)))
         (vec2 (setf ,x (vx2 ,valvar) ,y (vy2 ,valvar)))
         (vec3 (setf ,x (vx3 ,valvar) ,y (vy3 ,valvar) ,z (vz3 ,valvar)))
         (vec4 (setf ,x (vx4 ,valvar) ,y (vy4 ,valvar) ,z (vz4 ,valvar) ,w (vw4 ,valvar))))
       ,@body)))

(defmacro %2vec-op (a b combination v2red &optional (v3red v2red) (v4red v2red))
  (let ((v2red (if (listp v2red) v2red (list v2red)))
        (v3red (if (listp v3red) v3red (list v3red)))
        (v4red (if (listp v4red) v4red (list v4red))))
    `(etypecase ,a
       (real (let ((,a (ensure-float ,a)))
               (etypecase ,b
                 (vec4 (,@v4red (,combination ,a (vx4 b)) (,combination a (vy4 b)) (,combination a (vz4 b)) (,combination a (vw4 b))))
                 (vec3 (,@v3red (,combination ,a (vx3 b)) (,combination a (vy3 b)) (,combination a (vz3 b))))
                 (vec2 (,@v2red (,combination ,a (vx2 b)) (,combination a (vy2 b)))))))
       (vec4 (etypecase b
               (real (let ((b (ensure-float b))) (,@v4red (,combination (vx4 a) b) (,combination (vy4 a) b) (,combination (vz4 a) b) (,combination (vw4 a) b))))
               (vec4 (,@v4red (,combination (vx4 a) (vx4 b)) (,combination (vy4 a) (vy4 b)) (,combination (vz4 a) (vz4 b)) (,combination (vw4 a) (vw4 b))))))
       (vec3 (etypecase b
               (real (let ((b (ensure-float b))) (,@v3red (,combination (vx3 a) b) (,combination (vy3 a) b) (,combination (vz3 a) b))))
               (vec3 (,@v3red (,combination (vx3 a) (vx3 b)) (,combination (vy3 a) (vy3 b)) (,combination (vz3 a) (vz3 b))))))
       (vec2 (etypecase b
               (real (let ((b (ensure-float b))) (,@v2red (,combination (vx2 a) b) (,combination (vy2 a) b))))
               (vec2 (,@v2red (,combination (vx2 a) (vx2 b)) (,combination (vy2 a) (vy2 b)))))))))

(defmacro define-veccomp (name op)
  (let ((2vec-name (intern (format NIL "~a-~a" '2vec name))))
    `(progn
       (declaim (ftype (function ((or vec real) (or vec real)) boolean) ,2vec-name))
       (declaim (ftype (function ((or vec real) &rest (or vec real)) boolean) ,name))
       (declaim (inline ,name ,2vec-name))
       (define-ofun ,2vec-name (a b)
         (%2vec-op a b ,op and and and))
       (define-ofun ,name (val &rest vals)
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
       (define-ofun ,2vec-name (a b)
         (%2vec-op a b ,op vec2 vec3 vec4))
       (define-ofun ,name (val &rest vals)
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
(define-ofun vlength (v)
  (etypecase v
    (vec2 (sqrt (+ (expt (vx2 v) 2)
                   (expt (vy2 v) 2))))
    (vec3 (sqrt (+ (expt (vx3 v) 2)
                   (expt (vy3 v) 2)
                   (expt (vz3 v) 2))))
    (vec4 (sqrt (+ (expt (vx4 v) 2)
                   (expt (vy4 v) 2)
                   (expt (vz4 v) 2)
                   (expt (vw4 v) 2))))))

(defmacro %vsetf (&environment env v x y &optional z w)
  `(progn ,(cond (w `(psetf (%vx4 ,v) ,(ensure-float-param x env)
                            (%vy4 ,v) ,(ensure-float-param y env)
                            (%vz4 ,v) ,(ensure-float-param z env)
                            (%vw4 ,v) ,(ensure-float-param w env)))
                 (z `(psetf (%vx3 ,v) ,(ensure-float-param x env)
                            (%vy3 ,v) ,(ensure-float-param y env)
                            (%vz3 ,v) ,(ensure-float-param z env)))
                 (T `(psetf (%vx2 ,v) ,(ensure-float-param x env)
                            (%vy2 ,v) ,(ensure-float-param y env))))
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
         (vec2 (vec2 (,op (vx2 ,v) ,@(when x (list (ensure-float-param x env))))
                     (,op (vy2 ,v) ,@(when y (list (ensure-float-param y env))))))
         (vec3 (vec3 (,op (vx3 ,v) ,@(when x (list (ensure-float-param x env))))
                     (,op (vy3 ,v) ,@(when y (list (ensure-float-param y env))))
                     (,op (vz3 ,v) ,@(when z (list (ensure-float-param z env))))))
         (vec4 (vec4 (,op (vx4 ,v) ,@(when x (list (ensure-float-param x env))))
                     (,op (vy4 ,v) ,@(when y (list (ensure-float-param y env))))
                     (,op (vz4 ,v) ,@(when z (list (ensure-float-param z env))))
                     (,op (vw4 ,v) ,@(when w (list (ensure-float-param w env))))))))))

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
       (define-ofun ,2vec-name (a b)
         (%2vec-op a b ,op vec2 vec3 vec4))
       (define-ofun ,name (val &rest vals)
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
       (define-ofun ,2vec-name (a b)
         (%2vec-op a b ,op (%vsetf a)))
       (define-ofun ,name (val &rest vals)
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
(define-ofun v1+ (v)
  (v+ v 1))

(declaim (inline v1-))
(declaim (ftype (function (vec) vec) v1-))
(define-ofun v1- (v)
  (v- v 1))

(defmacro vincf (&environment env v &optional (delta 1))
  `(nv+ ,v ,(ensure-float-param delta env)))

(defmacro vdecf (&environment env v &optional (delta 1))
  `(nv- ,v ,(ensure-float-param delta env)))

(declaim (inline v.))
(declaim (ftype (function (vec vec) #.*float-type*) v.))
(define-ofun v. (a b)
  (etypecase a
    (vec2 (+ (* (vx2 a) (vx2 b))
             (* (vy2 a) (vy2 b))))
    (vec3 (+ (* (vx3 a) (vx3 b))
             (* (vy3 a) (vy3 b))
             (* (vz3 a) (vz3 b))))
    (vec4 (+ (* (vx4 a) (vx4 b))
             (* (vy4 a) (vy4 b))
             (* (vz4 a) (vz4 b))
             (* (vw4 a) (vw4 b))))))

(declaim (inline vc))
(declaim (ftype (function (vec3 vec3) vec3) vc))
(define-ofun vc (a b)
  (vec3 (- (* (vy3 a) (vz3 b))
           (* (vz3 a) (vy3 b)))
        (- (* (vz3 a) (vx3 b))
           (* (vx3 a) (vz3 b)))
        (- (* (vx3 a) (vy3 b))
           (* (vy3 a) (vx3 b)))))

(declaim (inline vabs))
(declaim (ftype (function (vec) vec) vabs))
(define-ofun vabs (vec)
  (vmod vec abs))

(declaim (inline nvabs))
(declaim (ftype (function (vec) vec) nvabs))
(define-ofun nvabs (vec)
  (vmodf vec abs))

(declaim (inline vunit))
(declaim (ftype (function (vec) vec) vunit))
(define-ofun vunit (a)
  (v/ a (vlength a)))

(declaim (inline nvunit))
(declaim (ftype (function (vec) vec) nvunit))
(define-ofun nvunit (vec)
  (nv/ vec (vlength vec)))

(define-ofun vunit~ (a)
  (etypecase a
    (vec2 (let* ((x (vx2 a)) (y (vy2 a))
                 (m (- 1 (/ (sqrt 2))))
                 (r (/ (max x y)))
                 (r (* r (- (1+ m) (* r m (+ x y))))))
            (vec2 (* r x) (* r y))))
    (vec4 #+sbcl (multiple-value-bind (x y z w) (simd-vunit (vx4 a) (vy4 a) (vz4 a) (vw4 a))
                   (vec4 x y z w)))))

(define-ofun nvunit~ (a)
  (etypecase a
    (vec4 #+sbcl (multiple-value-bind (x y z w) (simd-vunit (vx4 a) (vy4 a) (vz4 a) (vw4 a))
                   (%vsetf a x y z w)))))

(declaim (inline vscale))
(declaim (ftype (function (vec real) vec) vscale))
(define-ofun vscale (a length)
  (nv* (vunit a) length))

(declaim (inline nvscale))
(declaim (ftype (function (vec real) vec) vscale))
(define-ofun nvscale (vec length)
  (nv* (nvunit vec) length))

(declaim (inline vclamp))
(declaim (ftype (function ((or vec real) vec (or vec real)) vec) vclamp))
(define-ofun vclamp (lower vec upper)
  (etypecase vec
    (vec2 (with-vec2 (lx ly) lower
            (with-vec2 (ux uy) upper
              (vec2 (min ux (max lx (vx2 vec)))
                    (min uy (max ly (vy2 vec)))))))
    (vec3 (with-vec3 (lx ly lz) lower
            (with-vec3 (ux uy uz) upper
              (vec3 (min ux (max lx (vx3 vec)))
                    (min uy (max ly (vy3 vec)))
                    (min uz (max lz (vz3 vec)))))))
    (vec4 (with-vec4 (lx ly lz lw) lower
            (with-vec4 (ux uy uz uw) upper
              (vec4 (min ux (max lx (vx4 vec)))
                    (min uy (max ly (vy4 vec)))
                    (min uz (max lz (vz4 vec)))
                    (min uw (max lw (vw4 vec)))))))))

(declaim (inline nvclamp))
(declaim (ftype (function ((or vec real) vec (or vec real))) nvclamp))
(define-ofun nvclamp (lower vec upper)
  (etypecase vec
    (vec2 (with-vec2 (lx ly) lower
            (with-vec2 (ux uy) upper
              (%vsetf vec (min ux (max lx (vx2 vec)))
                          (min uy (max ly (vy2 vec)))))))
    (vec3 (with-vec3 (lx ly lz) lower
            (with-vec3 (ux uy uz) upper
              (%vsetf vec (min ux (max lx (vx3 vec)))
                          (min uy (max ly (vy3 vec)))
                          (min uz (max lz (vz3 vec)))))))
    (vec4 (with-vec4 (lx ly lz lw) lower
            (with-vec4 (ux uy uz uw) upper
              (%vsetf vec (min ux (max lx (vx4 vec)))
                          (min uy (max ly (vy4 vec)))
                          (min uz (max lz (vz4 vec)))
                          (min uw (max lw (vw4 vec)))))))))

(declaim (inline vlimit))
(declaim (ftype (function (vec (or vec real)) vec) vlimit))
(define-ofun vlimit (vec limit)
  (vclamp (- limit) vec limit))

(declaim (inline nvlimit))
(declaim (ftype (function (vec (or vec real)) vec) nvlimit))
(define-ofun nvlimit (vec limit)
  (nvclamp (- limit) vec limit))

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
(define-ofun vrot (v axis phi)
  (let ((phi (ensure-float phi)))
    (%vecrot-internal
      (vec3 (arith vx3) (arith vy3) (arith vz3)))))

(declaim (inline nvrot))
(declaim (ftype (function (vec3 vec3 real) vec3) nvrot))
(define-ofun nvrot (v axis phi)
  ;; https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  ;; vr = v*cos(phi) + (kxv)*sin(phi) + k*(k*v)*(1-cos(phi)
  (let ((phi (ensure-float phi)))
    (%vecrot-internal
      (setf (vx3 v) (arith vx3)
            (vy3 v) (arith vy3)
            (vz3 v) (arith vz3))
      v)))

(declaim (inline vrotv))
(declaim (ftype (function (vec3 vec3) vec3) vrotv))
(define-ofun vrotv (a b)
  (vrot (vrot (vrot a
                    +vx+ (vx3 b))
              +vy+ (vy3 b))
        +vz+ (vz3 b)))

(declaim (inline nvrotv))
(declaim (ftype (function (vec3 vec3) vec3) nvrotv))
(define-ofun nvrotv (a b)
  (nvrot (nvrot (nvrot a
                       +vx+ (vx3 b))
                +vy+ (vy3 b))
         +vz+ (vz3 b)))

(declaim (ftype (function (vec symbol &optional symbol symbol symbol) vec) vorder))
(define-ofun vorder (v x &optional y z w)
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
(define-ofun nvorder (v x &optional y z w)
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

(defun permute (&rest lists)
  (cond ((cdr lists)
         (let ((sub (apply #'permute (rest lists))))
           (loop for item in (first lists)
                 append (loop for s in sub collect (list* item s)))))
        (lists
         (mapcar #'list (first lists)))
        (T
         NIL)))

(defun subsetü)

(defmacro define-swizzler (&rest comps)
  (flet ((%vec-accessor (dim type)
           (if (eql dim '_)
               (ensure-float 0)
               (list (intern (format NIL "~a~a~a" 'v dim (subseq (string type) 3))) 'vec))))
    (let ((name (intern (format NIL "~a~{~a~}" 'v comps))))
      `(progn
         (declaim (inline ,name))
         (declaim (ftype (function (vec) vec) ,name))
         (defun ,name (vec)
           (etypecase vec
             ,@(loop for d in (or (append (when (subsetp comps '(_ x y)) '(vec2))
                                          (when (subsetp comps '(_ x y z)) '(vec3))
                                          (when (subsetp comps '(_ x y z w)) '(vec4)))
                                  (error "Unknown comps: ~a" comps))
                     collect `(,d ,(ecase (length comps)
                                     (2 `(vec2 ,@(loop for comp in comps collect (%vec-accessor comp d))))
                                     (3 `(vec3 ,@(loop for comp in comps collect (%vec-accessor comp d))))
                                     (4 `(vec4 ,@(loop for comp in comps collect (%vec-accessor comp d)))))))))))))

(defmacro define-all-swizzlers (size)
  `(progn ,@(loop for comps in (apply #'permute (loop repeat size collect '(_ x y z w)))
                  collect `(define-swizzler ,@comps))))

(define-all-swizzlers 2)
(define-all-swizzlers 3)
(define-all-swizzlers 4)
