#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-3d-vectors-u32 (push :3d-vectors-no-u32 *features*))

#+3d-vectors-debug
(defun dbg (format &rest values)
  (format *debug-io* "~&3DV-DBG: ~?~%" format values))
#-3d-vectors-debug
(defun dbg (format &rest values)
  (declare (ignore format values))
  NIL)

(defun enlist (list-ish &rest els)
  (if (listp list-ish) list-ish (list* list-ish els)))

(defun format-name (format &rest args)
  (let ((str (format NIL "~?" format args)))
    (intern
     (ecase (readtable-case *readtable*)
       (:upcase (string-upcase str))
       (:downcase (string-downcase str))
       (:preserve str)
       (:invert (loop for i from 0 below (length str)
                      for char = (char str i)
                      do (setf (char str i) (if (upper-case-p char) (char-downcase char) (char-upcase char)))
                      finally (return str)))))))

(defun compose-name (separator &rest parts)
  (intern
   (with-output-to-string (out)
     (flet ((s (a)
              (let ((s (typecase a
                         (string a)
                         (symbol (symbol-name a))
                         (T (princ-to-string a)))))
                (write-string s out))))
       (s (first parts))
       (loop for part in (rest parts)
             do (when separator (write-char separator out))
                (s part))))))

(defun enumerate-combinations (&rest combinations)
  (if (cdr combinations)
      (loop for comb in (enlist (first combinations))
            nconc (loop for rest in (apply #'enumerate-combinations (rest combinations))
                        collect (list* comb rest)))
      (loop for comb in (enlist (first combinations))
            collect (list comb))))

(defun prefix-tree (combinations)
  (let ((table (make-hash-table :test 'eql)))
    (loop for (car . cdr) in combinations
          do (if (consp cdr)
                 (push cdr (gethash car table))
                 (setf (gethash car table) cdr)))
    (loop for key being the hash-keys of table
          for combinations being the hash-values of table
          collect (list* key (if (listp combinations)
                                 (prefix-tree combinations)
                                 combinations)))))

(defun declaration-p (thing)
  (and (listp thing) (eql 'declare (car thing))))

(defun declarations (forms)
  (loop for form = (pop forms)
        while (declaration-p form)
        append (rest form)))

(defun declared-variable-types (forms)
  (loop for declaration in (declarations forms)
        when (and (listp declaration) (eql 'type (first declaration)))
        append (loop with type = (second declaration)
                     for arg in (rest declaration)
                     collect (list arg type))))

(defun declared-return-type (forms)
  (loop for declaration in (declarations forms)
        when (and (listp declaration) (eql 'return-type (first declaration)))
        return (second declaration)
        finally (return T)))

(declaim (declaration return-type))

(defun lambda-list-variables (arglist)
  (loop for arg in arglist
        unless (find arg LAMBDA-LIST-KEYWORDS)
        collect (if (listp arg) (car arg) arg)))

(defmacro define-type-with-converter (name base-type (value) &body conversion)
  (let ((valueg (gensym "VALUE")))
    `(progn
       (deftype ,name ()
         ',base-type)
       
       (declaim (ftype (function (T) ,base-type) ,name))
       (defun ,name (,value)
         (declare (optimize speed (debug 1) (safety 1) (compilation-speed 0)))
         ,@conversion)

       (define-compiler-macro ,name (,valueg &environment env)
         (if (constantp ,valueg env)
             `(load-time-value
               (let ((,',value ,,valueg))
                 ,@',conversion))
             `(let ((,',value ,,valueg))
                ,@',conversion))))))

(define-type-with-converter f32 single-float (value)
  (float value 0f0))

(define-type-with-converter f64 double-float (value)
  (float value 0d0))

(define-type-with-converter u32 (unsigned-byte 32) (value)
  (ldb (byte 32 0) (truncate value)))

(define-type-with-converter i32 (signed-byte 32) (value)
  (check-type value (signed-byte 32))
  value)

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
