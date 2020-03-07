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

(declaim (inline sqr))
(defun sqr (a)
  (expt a 2))
