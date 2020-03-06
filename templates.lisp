#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;;; Template type mechanism
(defclass template-type () ())
(defgeneric template-type (base-type template-args))
(defgeneric constructor (template-type))
(defgeneric name (template-type))
(defgeneric place (template-type qualifier))

(defmethod template-type ((base symbol) template-args)
  (template-type (make-instance base) template-args))

(defun emit-template-type (parent name fields)
  (let ((class (compose-name #\- name 'type)))
    `(progn
       (defclass ,class (,parent) ())
       (defmethod constructor ((_ ,class)) ',(compose-name NIL '% name))
       (defmethod name ((_ ,class)) ',name)
       (defmethod place ((_ ,class) qualifier)
         (ecase qualifier
           ,@(loop for (name type alias) in fields
                   collect `(,alias ',name))))
       
       (defstruct (,name (:constructor ,(compose-name NIL '% name)
                             ,(loop for (name type alias) in fields collect name))
                         (:copier ,(compose-name #\- name 'copy))
                         (:predicate ,(compose-name #\- name 'p))
                         (:conc-name NIL))
         ,@(loop for (name type alias) in fields
                 collect `(,name NIL :type ,type))))))

(defmacro define-template-type (name template-args name-constructor &body body)
  (let ((fields (gensym "FIELDS"))
        (targs (gensym "TEMPLATE-ARGS"))
        (class (compose-name #\- name 'type)))
    `(progn
       (defclass ,class (template-type) ())
       (defmethod template-type ((_ (eql ',name)) ,targs)
         (template-type (make-instance ',class) ,targs))
       (defmethod template-type ((_ ,class) ,targs)
         (destructuring-bind ,template-args ,targs
           (make-instance (compose-name #\- ,name-constructor 'type))))
       (defmacro ,(compose-name #\- 'define name) ,template-args
         (let ((,fields ()))
           (labels ((field (name &key (type T) alias)
                      (push (list name type alias) ,fields)))
             ,@body
             (emit-template-type ',class ,name-constructor (nreverse ,fields))))))))

;;; Template mechanism
(defmacro define-template (name &rest args)
  (destructuring-bind (base . template-args) (loop until (listp (car args))
                                                   collect (pop args))
    (destructuring-bind (args . body) args
      `(defmacro ,(compose-name #\- 'define name) ,template-args
         (let* ((type (template-type ',base (list ,@template-args)))
                (constructor (constructor type))
                (name (name type)))
           (flet ((place (qualifier)
                    (place type qualifier)))
             `(defun ,(compose-name #\/ ',name ,@template-args) ,',args
                ,@(list ,@body))))))))

;;; Dispatch mechanism
;; FIXME: + possible impl-specific type inference expanders
;; FIXME: handle args of different type combinations
(defmacro define-dispatch (name args &rest template-combinations)
  (destructuring-bind (base-type . combinations) template-combinations
    `(defun ,name ,args
       (etypecase ,(first args)
         ,@(loop for template in (apply #'enumerate-combinations combinations)
                 for type = (template-type base-type template)
                 for op = (apply #'compose-name #\/ name template)
                 collect `(,(name type) (,op ,@args)))))))
