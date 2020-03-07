#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;;; Template type mechanism
(defgeneric type-instance (base-type &rest template-args))
(defgeneric template-arguments (template-type))
(defgeneric constructor (template-type))
(defgeneric lisp-type (template-type))
(defgeneric place (template-type qualifier))
(defgeneric instances (template-type))

(defmethod lisp-type (type)
  (check-type type (or symbol list))
  type)

(defclass template-type ()
  ((lisp-type :initarg :lisp-type :initform (error "lisp-type argument missing.") :reader lisp-type)
   (constructor :initarg :constructor :initform (error "constructor argument missing.") :reader constructor)
   (places :initarg :places :initform (error "") :reader places)))

(defmethod print-object ((type template-type) stream)
  (print-unreadable-object (type stream :type T)
    (format stream "~a <~{~a~^ ~}>" (lisp-type type)
            (template-arguments type))))

(defmethod type-instance ((type template-type) &rest targs)
  (loop for instance in (instances type)
        do (when (equal targs (template-arguments instance))
             (return instance))
        finally (error "No such type instance of~%  ~a~%with template arguments~%  ~a"
                       (type-of type) targs)))

(defmethod place ((type template-type) qualifier)
  (loop for (names place) in (places type)
        do (when (find qualifier names)
             (return place))
        finally (error "No such place~%  ~s~%on~%  ~s" qualifier type)))

(defmethod instances ((type class))
  (instances (allocate-instance type)))

(defmethod instances ((type symbol))
  (instances (find-class type)))

(defmethod type-instance ((base symbol) &rest template-args)
  (apply #'type-instance (allocate-instance (find-class base)) template-args))

(defmacro define-type-instance ((template-type name) &body args)
  `(let ((instance (make-instance ',template-type :lisp-type ',name ,@(loop for arg in args collect `',arg))))
     (setf (instances instance) (list* instance (remove ',name (instances instance) :key #'lisp-type)))))

(defun emit-template-type (parent name fields template-args)
  `(progn
     (define-type-instance (,parent ,name)
       :constructor ,(compose-name NIL '% name)
       :places ,(loop for (name type alias) in fields
                      collect `(,alias ,name))
       ,@template-args)
     
     (defstruct (,name (:constructor ,(compose-name NIL '% name)
                           ,(loop for (name type alias) in fields collect name))
                       (:copier ,(compose-name #\- name 'copy))
                       (:predicate ,(compose-name #\- name 'p))
                       (:conc-name NIL))
       ,@(loop for (name type alias) in fields
               collect `(,name NIL :type ,type)))))

(defmacro define-template-type (name template-args name-constructor &body body)
  (let ((fields (gensym "FIELDS"))
        (class (compose-name #\- name 'type)))
    `(progn
       (defclass ,class (template-type)
         ((instances :initform () :accessor instances :allocation :class)
          ,@(loop for arg in template-args
                  collect `(,arg :initform (error "template argument missing")
                                 :initarg ,arg
                                 :reader ,arg))))

       (defmethod template-arguments ((,class ,class))
         (list ,@(loop for arg in template-args
                       collect `(,arg ,class))))

       (defmacro ,(compose-name #\- 'define name) ,template-args
         (let ((,fields ()))
           (labels ((field (name &key (type T) alias)
                      (push (list name type alias) ,fields)))
             ,@body
             (emit-template-type ',class ,name-constructor (nreverse ,fields)
                                 (loop for arg in (list ,@template-args)
                                       for temp in ',template-args
                                       collect temp collect arg))))))))

;;; Template mechanism
(defmacro define-template (name &rest args)
  (let ((template-args (loop until (listp (car args))
                             collect (pop args))))
    (destructuring-bind (args . body) args
      `(defmacro ,(compose-name #\- 'define name) ,template-args
         `(defun ,(compose-name #\/ ',name ,@template-args) ,',args
            (declare (optimize speed (safety 1) (debug 1) (compilation-speed 0)))
            ,@(progn ,@body))))))

(defmacro do-combinations (template &rest argument-combinations)
  `(progn ,@(loop for combination in (apply #'enumerate-combinations argument-combinations)
                  collect `(,template ,@combination))))

;;; Dispatch mechanism
;; FIXME: + possible impl-specific type inference expanders
;; FIXME: handle args of different type combinations
;; FIXME: handle arg coercion
(defmacro define-dispatch (name template-name args template-type &rest template-args)
  `(defun ,name ,args
     (etypecase ,(first args)
       ,@(loop for type in (instances (allocate-instance (find-class template-type)))
               for op = (apply #'compose-name #\/ template-name (append template-args (template-arguments type)))
               collect `(,(lisp-type type) (,op ,@args))))))

(defun emit-type-dispatch (args parts)
  (let ((tree (prefix-tree (loop for (type . expansion) in parts
                                 for i from 0
                                 collect (append type i)))))
    (labels ((emit-dispatch (args types)
               `(etypecase ,(first args)
                  ,@(loop for (type . rest) in types
                          collect `(,type
                                    ,@(if (consp rest)
                                          (list (emit-dispatch (rest args) rest))
                                          (rest (nth rest parts))))))))
      (emit-dispatch args tree))))

(defmacro define-type-dispatch (name args &body expansions)
  (let ((argvars (remove '&optional args)))
    `(progn
       (defun ,name ,args
         ,(emit-type-dispatch argvars expansions))
       #+sbcl
       (sb-c:defknown ,name ,(loop for arg in args collect (if (find arg lambda-list-keywords) arg '*))
           *)
       #+sbcl
       ,@(loop for (type . body) in expansions
               collect `(sb-c:deftransform ,name (,args ,type T)
                          ,@body)))))

(defun enumerate-template-type-combinations (types)
  (labels ((expand-type (type)
             (cond ((integerp type)
                    (list type))
                   ((listp type)
                    (loop for sub in type append (expand-type sub)))
                   ((subtypep type 'template-type)
                    (instances type))
                   (T
                    (list type)))))
    (let ((expanded (apply #'enumerate-combinations (mapcar #'expand-type types))))
      ;; Perform back substitution of positional types
      (dolist (types expanded expanded)
        (loop for cons on types
              do (when (integerp (car cons))
                   (setf (car cons) (nth (car cons) types))))))))

(defun determine-template-arguments (types)
  (remove-duplicates
   (loop for type in types
         when (typep type 'template-type)
         append (template-arguments type))))

(defmacro define-templated-dispatch (name args &body expansions)
  `(define-type-dispatch ,name ,args
     ,@(loop for (types template . template-args) in expansions
             append (loop for type in (enumerate-template-type-combinations types)
                          for full-template-args = (append template-args (determine-template-arguments type))
                          collect `(,(mapcar #'lisp-type type)
                                    (,(apply #'compose-name #\/ template full-template-args) ,@args))))))
