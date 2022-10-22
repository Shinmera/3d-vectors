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
       (declare (type ,*float-type* ,x ,y ,z ,w)
                (ignorable ,z ,w))
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
        (v4red (if (listp v4red) v4red (list v4red)))
        (ag (gensym "A")) (bg (gensym "B")))
    `(etypecase ,a
       (real (let ((,ag (ensure-float ,a)))
               (etypecase ,b
                 (vec4 (,@v4red (,combination ,ag (vx4 ,b)) (,combination ,ag (vy4 ,b)) (,combination ,ag (vz4 ,b)) (,combination ,ag (vw4 ,b))))
                 (vec3 (,@v3red (,combination ,ag (vx3 ,b)) (,combination ,ag (vy3 ,b)) (,combination ,ag (vz3 ,b))))
                 (vec2 (,@v2red (,combination ,ag (vx2 ,b)) (,combination ,ag (vy2 ,b)))))))
       (vec4 (etypecase ,b
               (real (let ((,bg (ensure-float ,b))) (,@v4red (,combination (vx4 ,a) ,bg) (,combination (vy4 ,a) ,bg) (,combination (vz4 ,a) ,bg) (,combination (vw4 ,a) ,bg))))
               (vec4 (,@v4red (,combination (vx4 ,a) (vx4 ,b)) (,combination (vy4 ,a) (vy4 ,b)) (,combination (vz4 ,a) (vz4 ,b)) (,combination (vw4 ,a) (vw4 ,b))))))
       (vec3 (etypecase ,b
               (real (let ((,bg (ensure-float ,b))) (,@v3red (,combination (vx3 ,a) ,bg) (,combination (vy3 ,a) ,bg) (,combination (vz3 ,a) ,bg))))
               (vec3 (,@v3red (,combination (vx3 ,a) (vx3 ,b)) (,combination (vy3 ,a) (vy3 ,b)) (,combination (vz3 ,a) (vz3 ,b))))))
       (vec2 (etypecase ,b
               (real (let ((,bg (ensure-float ,b))) (,@v2red (,combination (vx2 ,a) ,bg) (,combination (vy2 ,a) ,bg))))
               (vec2 (,@v2red (,combination (vx2 ,a) (vx2 ,b)) (,combination (vy2 ,a) (vy2 ,b)))))))))

(defmacro define-veccomp (name op &optional (bundle 'and))
  (let ((2vec-name (intern* '2vec "-" name)))
    `(progn
       (declaim (ftype (function ((or vec real) (or vec real)) boolean) ,2vec-name))
       (declaim (ftype (function ((or vec real) &rest (or vec real)) boolean) ,name))
       (declaim (inline ,name ,2vec-name))
       (define-ofun ,2vec-name (a b)
         (%2vec-op a b ,op ,bundle ,bundle ,bundle))
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
(define-veccomp v/= /= or)
(define-veccomp v< <)
(define-veccomp v<= <=)
(define-veccomp v> >)
(define-veccomp v>= >=)

(defmacro define-vecreduce (name op)
  (let ((2vec-name (intern* '2vec "-" name)))
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
           (0 `(vcopy ,val))
           (1 `(,',2vec-name ,val ,(first vals)))
           (T `(,',2vec-name ,val (,',name ,@(rest vals)))))))))

(define-vecreduce vmin min)
(define-vecreduce vmax max)

(defmacro define-vector-constant (name x y &optional z w)
  (let ((z (when z (list z))) (w (when w (list w))))
    `(defconstant ,name (cond ((not (boundp ',name))
                               (vec ,x ,y ,@z ,@w))
                              ((v= (symbol-value ',name) (vec ,x ,y ,@z ,@w))
                               (symbol-value ',name))
                              (T (error "Attempting to redefine constant vector ~a with value ~a to ~a."
                                        ',name (symbol-value ',name) (vec ,x ,y ,@z ,@w)))))))
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

(declaim (inline vdistance))
(declaim (ftype (function (vec vec) (#.*float-type* #.(ensure-float 0))) vdistance))
(define-ofun vdistance (a b)
  (etypecase a
    (vec2 (etypecase b
            (vec2 (sqrt (+ (expt (- (vx2 a) (vx2 b)) 2)
                           (expt (- (vy2 a) (vy2 b)) 2))))))
    (vec3 (etypecase b
            (vec3 (sqrt (+ (expt (- (vx3 a) (vx3 b)) 2)
                           (expt (- (vy3 a) (vy3 b)) 2)
                           (expt (- (vz3 a) (vz3 b)) 2))))))
    (vec4 (etypecase b
            (vec4 (sqrt (+ (expt (- (vx4 a) (vx4 b)) 2)
                           (expt (- (vy4 a) (vy4 b)) 2)
                           (expt (- (vz4 a) (vz4 b)) 2)
                           (expt (- (vw4 a) (vw4 b)) 2))))))))

(declaim (inline vsqrdistance))
(declaim (ftype (function (vec vec) (#.*float-type* #.(ensure-float 0))) vsqrdistance))
(define-ofun vsqrdistance (a b)
  (etypecase a
    (vec2 (etypecase b
            (vec2 (+ (expt (- (vx2 a) (vx2 b)) 2)
                     (expt (- (vy2 a) (vy2 b)) 2)))))
    (vec3 (etypecase b
            (vec3 (+ (expt (- (vx3 a) (vx3 b)) 2)
                     (expt (- (vy3 a) (vy3 b)) 2)
                     (expt (- (vz3 a) (vz3 b)) 2)))))
    (vec4 (etypecase b
            (vec4 (+ (expt (- (vx4 a) (vx4 b)) 2)
                     (expt (- (vy4 a) (vy4 b)) 2)
                     (expt (- (vz4 a) (vz4 b)) 2)
                     (expt (- (vw4 a) (vw4 b)) 2)))))))

(declaim (inline vsqrlength))
(declaim (ftype (function (vec) (#.*float-type* #.(ensure-float 0))) vsqrlength))
(define-ofun vsqrlength (v)
  (etypecase v
    (vec2 (+ (expt (vx2 v) 2)
             (expt (vy2 v) 2)))
    (vec3 (+ (expt (vx3 v) 2)
             (expt (vy3 v) 2)
             (expt (vz3 v) 2)))
    (vec4 (+ (expt (vx4 v) 2)
             (expt (vy4 v) 2)
             (expt (vz4 v) 2)
             (expt (vw4 v) 2)))))

(declaim (inline vlength))
(declaim (ftype (function (vec) (#.*float-type* #.(ensure-float 0))) vlength))
(define-ofun vlength (v)
  (sqrt (vsqrlength v)))

(declaim (inline v2norm))
(declaim (ftype (function (vec) (#.*float-type* #.(ensure-float 0))) v2norm))
(define-ofun v2norm (v)
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

(declaim (inline v1norm))
(declaim (ftype (function (vec) (#.*float-type* #.(ensure-float 0))) v1norm))
(define-ofun v1norm (v)
  (etypecase v
    (vec2 (+ (abs (vx2 v))
             (abs (vy2 v))))
    (vec3 (+ (abs (vx3 v))
             (abs (vy3 v))
             (abs (vz3 v))))
    (vec4 (+ (abs (vx4 v))
             (abs (vy4 v))
             (abs (vz4 v))
             (abs (vw4 v))))))

(declaim (inline vinorm))
(declaim (ftype (function (vec) (#.*float-type* #.(ensure-float 0))) vinorm))
(define-ofun vinorm (v)
  (etypecase v
    (vec2 (max (abs (vx2 v))
               (abs (vy2 v))))
    (vec3 (max (abs (vx3 v))
               (abs (vy3 v))
               (abs (vz3 v))))
    (vec4 (max (abs (vx4 v))
               (abs (vy4 v))
               (abs (vz4 v))
               (abs (vw4 v))))))

(declaim (inline vpnorm))
(declaim (ftype (function (vec (real 1)) (#.*float-type* #.(ensure-float 0))) vpnorm))
(define-ofun vpnorm (v p)
  (let ((p (ensure-float p)))
    (etypecase v
      (vec2 (expt (the #.(list *float-type* (ensure-float 0.0))
                       (+ (expt (abs (vx2 v)) p)
                          (expt (abs (vy2 v)) p)))
                  (/ p)))
      (vec3 (expt (the #.(list *float-type* (ensure-float 0.0))
                       (+ (expt (abs (vx3 v)) p)
                          (expt (abs (vy3 v)) p)
                          (expt (abs (vz3 v)) p)))
                  (/ p)))
      (vec4 (expt (the #.(list *float-type* (ensure-float 0.0))
                       (+ (expt (abs (vx4 v)) p)
                          (expt (abs (vy4 v)) p)
                          (expt (abs (vz4 v)) p)
                          (expt (abs (vw4 v)) p)))
                  (/ p))))))

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
         (vec3 (%vsetf ,v ,x ,y ,(or z `(vz3 ,v))))
         (vec4 (%vsetf ,v ,x ,y ,(or z `(vz4 ,v)) ,(or w `(vw4 ,v)))))
       ,v)))

(defmacro vapply (&environment env vec op &optional x y z w)
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

(defmacro vapplyf (&environment env vec op &optional x y z w)
  (let ((v (gensym "VEC")))
    `(let ((,v ,vec))
       (vsetf ,v (,op (vx ,v) ,@(when x (list (ensure-float-param x env))))
                 (,op (vy ,v) ,@(when y (list (ensure-float-param y env))))
                 (,op (vz ,v) ,@(when z (list (ensure-float-param z env))))
                 (,op (vw ,v) ,@(when w (list (ensure-float-param w env)))))
       ,v)))

(defmacro define-vecop (name nname op)
  (let ((2vec-name (intern* '2vec "-" name)))
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
               (T (vapply val ,op))))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(vapply ,val ,',op))
           (1 `(,',2vec-name ,val ,(first vals)))
           (T `(,',nname (,',2vec-name ,val ,(first vals)) ,@(rest vals))))))))

(defmacro define-nvecop (name op)
  (let ((2vec-name (intern* '2vec "-" name)))
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
             (vapplyf val ,op)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(vapplyf ,val ,',op))
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

(declaim (ftype (function (vec vec) #.*float-type*) vangle))
(define-ofun vangle (a b)
  (let ((a (/ (v. a b)
              (v2norm a)
              (v2norm b))))
    (acos (the (#.*float-type* -1f0 +1f0)
               (min #.(ensure-float +1) (max #.(ensure-float -1) a))))))

(declaim (inline vabs))
(declaim (ftype (function (vec) vec) vabs))
(define-ofun vabs (vec)
  (vapply vec abs))

(declaim (inline nvabs))
(declaim (ftype (function (vec) vec) nvabs))
(define-ofun nvabs (vec)
  (vapplyf vec abs))

(declaim (inline vmod))
(declaim (ftype (function (vec real) vec) vmod))
(define-ofun vmod (vec divisor)
  (vapply vec mod divisor divisor divisor divisor))

(declaim (inline nvmod))
(declaim (ftype (function (vec real) vec) nvmod))
(define-ofun nvmod (vec divisor)
  (vapplyf vec mod divisor divisor divisor divisor))

(declaim (inline vunit))
(declaim (ftype (function (vec) vec) vunit))
(define-ofun vunit (a)
  (v/ a (vlength a)))

(declaim (inline vunit*))
(declaim (ftype (function (vec) vec) vunit*))
(define-ofun vunit* (a)
  (if (v= a 0)
      (vcopy a)
      (v/ a (vlength a))))

(declaim (inline nvunit))
(declaim (ftype (function (vec) vec) nvunit))
(define-ofun nvunit (vec)
  (nv/ vec (vlength vec)))

(declaim (inline nvunit*))
(declaim (ftype (function (vec) vec) nvunit*))
(define-ofun nvunit* (vec)
  (if (v= vec 0)
      vec
      (nv/ vec (vlength vec))))

(declaim (inline vscale))
(declaim (ftype (function (vec real) vec) vscale))
(define-ofun vscale (a length)
  (nv* (vunit a) length))

(declaim (inline nvscale))
(declaim (ftype (function (vec real) vec) nvscale))
(define-ofun nvscale (vec length)
  (nv* (nvunit vec) length))

(macrolet ((define-roundfun (fun vname nvname)
             `(progn
                (declaim (inline ,vname))
                (declaim (ftype (function (vec &optional real) vec) ,vname))
                (define-ofun ,vname (vec &optional (divisor 1.0))
                  (let ((divisor (ensure-float divisor)))
                    (vapply vec ,fun divisor divisor divisor divisor)))

                (declaim (inline ,nvname))
                (declaim (ftype (function (vec &optional real) vec) ,nvname))
                (define-ofun ,nvname (vec &optional (divisor 1.0))
                  (let ((divisor (ensure-float divisor)))
                    (vapplyf vec ,fun divisor divisor divisor divisor))))))
  (define-roundfun floor vfloor nvfloor)
  (define-roundfun ceiling vceiling nvceiling)
  (define-roundfun round vround nvround))

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

(declaim (inline lerp))
(defun lerp (from to n)
  (+ (* from (- 1 n)) (* to n)))

(declaim (inline vlerp))
(declaim (ftype (function (vec vec (or vec real)) vec) vlerp))
(define-ofun vlerp (from to n)
  (etypecase from
    (vec2 (with-vec2 (tx ty) to
            (with-vec2 (nx ny) n
              (vec2 (lerp (vx2 from) tx nx)
                    (lerp (vy2 from) ty ny)))))
    (vec3 (with-vec3 (tx ty tz) to
            (with-vec3 (nx ny nz) n
              (vec3 (lerp (vx3 from) tx nx)
                    (lerp (vy3 from) ty ny)
                    (lerp (vz3 from) tz nz)))))
    (vec4 (with-vec4 (tx ty tz tw) to
            (with-vec4 (nx ny nz nw) n
              (vec4 (lerp (vx4 from) tx nx)
                    (lerp (vy4 from) ty ny)
                    (lerp (vz4 from) tz nz)
                    (lerp (vw4 from) tw nw)))))))

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

(declaim (ftype (function (vec3 vec3 real) vec3) nvrot))
(define-ofun nvrot (v axis phi)
  (let ((phi (ensure-float phi)))
    (%vecrot-internal
      (setf (vx3 v) (arith vx3)
            (vy3 v) (arith vy3)
            (vz3 v) (arith vz3))
      v)))

(declaim (ftype (function (vec3 vec3) vec3) vrotv))
(define-ofun vrotv (a b)
  (nvrot (nvrot (vrot a
                      +vx+ (vx3 b))
                +vy+ (vy3 b))
         +vz+ (vz3 b)))

(declaim (ftype (function (vec3 vec3) vec3) nvrotv))
(define-ofun nvrotv (a b)
  (nvrot (nvrot (nvrot a
                       +vx+ (vx3 b))
                +vy+ (vy3 b))
         +vz+ (vz3 b)))

(declaim (inline vrot2))
(declaim (ftype (function (vec2 real) vec2) vrot2))
(define-ofun vrot2 (vec phi)
  (let* ((angle (ensure-float phi))
         (sin (sin angle))
         (cos (cos angle)))
    (vec (- (* (vx2 vec) cos) (* (vy2 vec) sin))
         (+ (* (vx2 vec) sin) (* (vy2 vec) cos)))))

(declaim (ftype (function (vec2 real) vec2) nvrot2))
(define-ofun nvrot2 (vec phi)
  (let* ((angle (ensure-float phi))
         (sin (sin angle))
         (cos (cos angle)))
    (vsetf vec
           (- (* (vx2 vec) cos) (* (vy2 vec) sin))
           (+ (* (vx2 vec) sin) (* (vy2 vec) cos)))))

(declaim (inline v<-))
(declaim (ftype (function (vec vec) vec) v<-))
(define-ofun v<- (target source)
  (etypecase source
    (vec2 (vsetf target (vx2 source) (vy2 source)))
    (vec3 (vsetf target (vx3 source) (vy3 source) (vz3 source)))
    (vec4 (vsetf target (vx4 source) (vy4 source) (vz4 source) (vw4 source)))))

(declaim (ftype (function ((or vec real) (or vec real)) vec) vrand))
(define-ofun vrand (x var)
  (flet ((random* (x var)
           (if (= 0.0 var)
               x
               (+ x (- (random var) (/ var 2f0))))))
    (etypecase var
      (real
       (let ((var (ensure-float var)))
         (etypecase x
           (vec2 (vec (random* (vx2 x) var)
                      (random* (vy2 x) var)))
           (vec3 (vec (random* (vx3 x) var)
                      (random* (vy3 x) var)
                      (random* (vz3 x) var)))
           (vec4 (vec (random* (vx4 x) var)
                      (random* (vy4 x) var)
                      (random* (vz4 x) var)
                      (random* (vw4 x) var))))))
      (vec2 (etypecase x
              (real
               (let ((x (ensure-float x)))
                 (vec (random* x (vx2 var))
                      (random* x (vy2 var)))))
              (vec2 (vec (random* (vx2 x) (vx2 var))
                         (random* (vy2 x) (vy2 var))))))
      (vec3 (etypecase x
              (real
               (let ((x (ensure-float x)))
                 (vec (random* x (vx3 var))
                      (random* x (vy3 var))
                      (random* x (vz3 var)))))
              (vec3 (vec (random* (vx3 x) (vx3 var))
                         (random* (vy3 x) (vy3 var))
                         (random* (vz3 x) (vz3 var))))))
      (vec4 (etypecase x
              (real
               (let ((x (ensure-float x)))
                 (vec (random* x (vx4 var))
                      (random* x (vy4 var))
                      (random* x (vz4 var))
                      (random* x (vw4 var)))))
              (vec4 (vec (random* (vx4 x) (vx4 var))
                         (random* (vy4 x) (vy4 var))
                         (random* (vz4 x) (vz4 var))
                         (random* (vw4 x) (vw4 var)))))))))

(declaim (inline valign))
(declaim (ftype (function (vec real) vec) valign))
(define-ofun valign (vec grid)
  (let* ((grid (ensure-float grid))
         (grid/2 (* grid 0.5)))
    (vec (* grid (floor (+ (vx vec) grid/2) grid))
         (* grid (floor (+ (vy vec) grid/2) grid))
         (* grid (floor (+ (vz vec) grid/2) grid))
         (* grid (floor (+ (vw vec) grid/2) grid)))))

(declaim (inline nvalign))
(declaim (ftype (function (vec real) vec) nvalign))
(define-ofun nvalign (vec grid)
  (let* ((grid (ensure-float grid))
         (grid/2 (* grid 0.5)))
    (vsetf vec
           (* grid (floor (+ (vx vec) grid/2) grid))
           (* grid (floor (+ (vy vec) grid/2) grid))
           (* grid (floor (+ (vz vec) grid/2) grid))
           (* grid (floor (+ (vw vec) grid/2) grid)))))

(declaim (inline vcartesian))
(declaim (ftype (function (vec) vec) vcartesian))
(define-ofun vcartesian (vec)
  (etypecase vec
    (vec2 (vec2 (* (vx2 vec) (cos (vy2 vec)))
                (* (vx2 vec) (sin (vy2 vec)))))
    (vec3 (vec3 (* (vx3 vec) (cos (vy3 vec)) (sin (vz3 vec)))
                (* (vx3 vec) (sin (vy3 vec)) (sin (vz3 vec)))
                (* (vx3 vec)                 (cos (vz3 vec)))))))

(declaim (inline vpolar))
(declaim (ftype (function (vec) vec) vpolar))
(define-ofun vpolar (vec)
  (etypecase vec
    (vec2 (vec2 (vlength vec)
                (atan (vy vec) (vx vec))))
    (vec3 (let ((len (vlength vec)))
            (vec3 len
                  (atan (vy vec) (vx vec))
                  (acos (/ len (vz vec))))))))

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

(defmacro define-swizzler (&rest comps)
  (flet ((%vec-accessor (dim type)
           (if (eql dim '_)
               (ensure-float 0)
               (list (intern* 'v dim (subseq (string type) 3)) 'vec))))
    (let ((name (apply #'intern* 'v comps))
          (other-type (case (length comps) (2 'vec2) (3 'vec3) (4 'vec4))))
      `(progn
         (export ',name) ;; Haha no way, I'm not writing all these into the package listing.
         (declaim (inline ,name))
         (declaim (ftype (function (vec) vec) ,name))
         (define-ofun ,name (vec)
           ,(format NIL "Swizzles the vector into a ~aD one, filling its fields with the ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} components of the given vector respectively.
~:*
When used as a writer sets the fields of the vector, replacing the ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} components with the XYZW of the value respectively in that order.
Note that, unlike usual SETF functions that return the value you set to, this returns the modified vector."
                    (length comps) comps)
           (etypecase vec
             ,@(loop for d in (or (append (when (subsetp comps '(_ x y)) '(vec2))
                                          (when (subsetp comps '(_ x y z)) '(vec3))
                                          (when (subsetp comps '(_ x y z w)) '(vec4)))
                                  (error "Unknown comps: ~a" comps))
                     collect `(,d ,(ecase (length comps)
                                     (2 `(vec2 ,@(loop for comp in comps collect (%vec-accessor comp d))))
                                     (3 `(vec3 ,@(loop for comp in comps collect (%vec-accessor comp d))))
                                     (4 `(vec4 ,@(loop for comp in comps collect (%vec-accessor comp d)))))))))
         
         (declaim (inline (setf ,name)))
         (declaim (ftype (function ((or ,other-type number) vec) vec) (setf ,name)))
         (define-ofun (setf ,name) (val vec)
           ,(if (loop for c in comps always (eq c '_))
                `(declare (ignore val))
                `(etypecase vec
                   ,@(loop for d in (or (append (when (subsetp comps '(_ x y)) '(vec2))
                                                (when (subsetp comps '(_ x y z)) '(vec3))
                                                (when (subsetp comps '(_ x y z w)) '(vec4)))
                                        (error "Unknown comps: ~a" comps))
                           collect `(,d (etypecase val
                                          (number
                                           (let ((val (ensure-float val)))
                                             ,@(loop for comp in comps for i in '(x y z w)
                                                     unless (eql comp '_)
                                                     collect `(setf ,(%vec-accessor comp d) val))))
                                          (,other-type
                                           ,@(loop for comp in comps for i in '(x y z w)
                                                   unless (eql comp '_)
                                                   collect `(setf ,(%vec-accessor comp d) (,(first (%vec-accessor i other-type)) val)))))))))
           vec)))))

(defmacro define-all-swizzlers (size)
  (labels ((permute (&rest lists)
             (cond ((cdr lists)
                    (let ((sub (apply #'permute (rest lists))))
                      (loop for item in (first lists)
                            append (loop for s in sub collect (list* item s)))))
                   (lists
                    (mapcar #'list (first lists)))
                   (T
                    NIL))))
    `(progn ,@(loop for comps in (apply #'permute (loop repeat size collect '(_ x y z w)))
                    collect `(define-swizzler ,@comps)))))

(define-all-swizzlers 2)
(define-all-swizzlers 3)
(define-all-swizzlers 4)
