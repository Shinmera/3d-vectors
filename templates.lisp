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
  (let ((constructor (compose-name NIL '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-type-instance (,parent ,name)
         :constructor ,(compose-name NIL '% name)
         :places ,(loop for (name type alias) in fields
                        collect `(,alias ,name))
         ,@template-args)

       (declaim (inline ,constructor ,@(mapcar #'first fields)))
       (defstruct (,name (:constructor ,constructor
                             ,(loop for (name type alias) in fields collect name))
                         (:copier ,(compose-name #\- name 'copy))
                         (:predicate ,(compose-name #\- name 'p))
                         (:conc-name NIL))
         ,@(loop for (name type alias) in fields
                 collect `(,name NIL :type ,type)))

       (defmethod print-object ((,name ,name) stream)
         (write (list ',name ,@(loop for field in fields collect `(,(first field) ,name)))
                :stream stream))

       (defmethod make-load-form ((,name ,name) &optional env)
         (list ',constructor ,@(loop for field in fields collect `(,(first field) ,name)))))))

(defmacro define-template-type (name template-args name-constructor &body body)
  (let ((fields (gensym "FIELDS"))
        (class (compose-name #\- name 'type)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
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

(defun emit-type-dispatch (args parts)
  (let ((tree (prefix-tree (loop for (type rettype . expansion) in parts
                                 for i from 0
                                 collect (append type i)))))
    (labels ((emit-dispatch (args types)
               `(etypecase ,(first args)
                  ,@(loop for (type . rest) in types
                          collect `(,type
                                    ,(if (consp rest)
                                         (emit-dispatch (rest args) rest)
                                         (destructuring-bind (rettype . body) (rest (nth rest parts))
                                           (if (eql T rettype)
                                               `(progn ,@body)
                                               `(the ,rettype (progn ,@body))))))))))
      (emit-dispatch args tree))))

(defmacro define-type-dispatch (name args &body expansions)
  (let ((argvars (remove '&optional args)))
    `(progn
       #-sbcl (declaim (inline ,name))
       (defun ,name ,args
         (declare (optimize speed (debug 1) (safety 1) (compilation-speed 0)))
         ,(emit-type-dispatch argvars expansions))
       #+sbcl
       (sb-c:defknown ,name ,(loop for arg in args collect (if (find arg lambda-list-keywords) arg '*)) *
           (sb-c:any)
         :overwrite-fndb-silently T)
       #+sbcl
       ,@(loop for (type result . body) in expansions
               ;; FIXME: this is not great. optional placement should be better.
               for opttypes = (remove 'null type)
               collect `(sb-c:deftransform ,name (,args ,opttypes)
                          (print '(expanding ,name ,opttypes))
                          ',@body)))))

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
                          collect `(,(mapcar #'lisp-type type) T
                                    (,(apply #'compose-name #\/ template full-template-args) ,@args))))))

(defmacro define-right-reductor (name 2-op &optional 1-op (rest-op name))
  `(progn
     (defun ,name (value &rest values)
       (cond ((null values)
              ,(if 1-op
                   `(,1-op value)
                   'value))
             ((null (cdr values))
              (,2-op value (first values)))
             (T
              (,2-op value (reduce #',rest-op values)))))

     (define-compiler-macro ,name (value &rest values)
       (cond ((null values)
              ,(if 1-op
                   ``(,',1-op ,value)
                   'value))
             ((null (cdr values))
              `(,',2-op ,value ,(first values)))
             (T
              `(,',2-op ,value (,',rest-op ,@values)))))))

(defmacro define-left-reductor (name 2-op &optional 1-op)
  `(progn
     (defun ,name (value &rest values)
       (cond ((null values)
              ,(if 1-op
                   `(,1-op value)
                   'value))
             ((null (cdr values))
              (,2-op value (first values)))
             (T
              (loop for other in values
                    do (setf value (,2-op value other))
                    finally (return value)))))

     (define-compiler-macro ,name (value &rest values)
       (cond ((null values)
              ,(if 1-op
                   ``(,',1-op ,value)
                   'value))
             ((null (cdr values))
              `(,',2-op ,value ,(first values)))
             (T
              `(,',name (,',2-op ,value ,(first values)) ,@(rest values)))))))

(defmacro define-alias (fun args &body expansion)
  `(progn
     (declaim (inline ,fun))
     (defun ,fun ,args
       ,@expansion)))
