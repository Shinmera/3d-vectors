#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

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
      (loop for comb in (first combinations)
            nconc (loop for rest in (apply #'enumerate-combinations (rest combinations))
                        collect (list* comb rest)))
      (loop for comb in (first combinations)
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
  (check-type value (unsigned-byte 32))
  value)

(define-type-with-converter i32 (signed-byte 32) (value)
  (check-type value (signed-byte 32))
  value)

(defun type-prefix (type)
  (ecase type
    (f32 '||)
    (f64 'd)
    (u32 'u)
    (i32 'i)))

(declaim (inline sqr))
(defun sqr (a)
  (expt a 2))

(defun sqrt+ (&rest a)
  (sqrt (apply #'+ a)))

(define-compiler-macro sqrt+ (&rest a)
  `(sqrt (+ ,@a)))

(declaim (inline lerp))
(defun lerp (from to n)
  (+ (* from (- 1 n)) (* to n)))
