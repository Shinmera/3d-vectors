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

(declaim (inline sqr))
(defun sqr (a)
  (expt a 2))

(defun type-prefix (type)
  (ecase type
    (f32 '||)
    (f64 'd)
    (u32 'u)
    (i32 'i)))
