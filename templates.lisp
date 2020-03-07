#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;;; Template type mechanism
(defgeneric type-instance (base-type &rest template-args))
(defgeneric constructor (template-type))
(defgeneric name (template-type))
(defgeneric place (template-type qualifier))

(defclass template-type ()
  ((name :initarg :name :initform (error "name argument missing.") :reader name)
   (constructor :initarg :constructor :initform (error "constructor argument missing.") :reader constructor)
   (places :initarg :places :initform (error "") :reader places)))

(defmethod place ((type template-type) qualifier)
  (loop for (names place) in (places type)
        do (when (find qualifier names)
             (return place))
        finally (error "No such place~%  ~s~%on~%  ~s" qualifier type)))

(defmethod type-instance ((base symbol) &rest template-args)
  (apply #'type-instance (allocate-instance (find-class base)) template-args))

(defmacro define-type-instance ((template-type name) &body args)
  `(let ((instance (make-instance ',template-type :name ',name ,@(loop for arg in args collect `',arg))))
     (setf (instances instance) (list* instance (remove ',name (instances instance) :key #'name)))))

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
        (targs (gensym "TEMPLATE-ARGS"))
        (class (compose-name #\- name 'type)))
    `(progn
       (defclass ,class (template-type)
         ((instances :initform () :accessor instances :allocation :class)
          ,@(loop for arg in template-args
                  collect `(,arg :initform (error "template argument missing")
                                 :initarg ,arg
                                 :reader ,arg))))

       (defmethod print-object ((,class ,class) stream)
         (print-unreadable-object (,class stream :type T)
           (format stream "~a <~{~a~^ ~}>" (name ,class)
                   (list ,@(loop for arg in template-args
                                 collect `(,arg ,class))))))
       
       (defmethod type-instance ((,class ,class) &rest ,targs)
         (destructuring-bind ,template-args ,targs
           (loop for type in (instances ,class)
                 do (when (and ,@(loop for arg in template-args
                                       collect `(equal ,arg (,arg type))))
                      (return type))
                 finally (error "No such type instance of~%  ~a~%with template arguments~%  ~a"
                                ',class ,targs))))

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
            ,@(progn ,@body))))))

(defmacro do-combinations (template &rest argument-combinations)
  `(progn ,@(loop for combination in (apply #'enumerate-combinations argument-combinations)
                  collect `(,template ,@combination))))

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
