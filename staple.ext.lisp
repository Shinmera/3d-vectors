(asdf:load-system :staple-markdown)
;; Filter out the swizzlers
(defmethod staple:definition-wanted-p ((func definitions:function) page)
  (flet ((swizzler-p (name)
           (let ((name (subseq name 1)))
             (and (<= 2 (length name))
                  (every (lambda (c) (find c "XYZW_")) name)))))
    (and (call-next-method)
         (not (swizzler-p (string (definitions:name func)))))))

(defmethod staple:definition-wanted-p ((_ definitions:setf-expander) page) NIL)
#+sbcl
(defmethod staple:definition-wanted-p ((_ definitions:source-transform) page) NIL)
