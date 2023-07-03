;;; IR1
(in-package #:org.shirakumo.flare.vector)

(sb-c:defknown %rsqrtss ((sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently T)

(sb-c:defknown %rsqrtps ((sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently T)

(sb-c:defknown %mulps ((sb-ext:simd-pack single-float)
                     (sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable sb-c:always-translatable)
  :overwrite-fndb-silently T)

(sb-c:defknown %haddps ((sb-ext:simd-pack single-float)
                      (sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable sb-c:always-translatable)
  :overwrite-fndb-silently T)

;;; IR2
(in-package #:sb-vm)

(define-vop (3d-vectors::%rsqrtss)
  (:policy :fast-safe)
  (:translate 3d-vectors::%rsqrtss)
  (:args (x :scs (single-sse-reg) :target r))
  (:arg-types simd-pack-single)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator 3
              (inst rsqrtss r x)))

(define-vop (3d-vectors::%rsqrtps)
  (:policy :fast-safe)
  (:translate 3d-vectors::%rsqrtps)
  (:args (x :scs (single-sse-reg) :target r))
  (:arg-types simd-pack-single)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator 3
              (inst rsqrtps r x)))

(define-vop (3d-vectors::%mulps)
  (:policy :fast-safe)
  (:translate 3d-vectors::%mulps)
  (:args (x :scs (single-sse-reg) :target r)
         (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator 4
              (cond ((location= r y)
                     (inst mulps y x))
                    (T
                     (move r x)
                     (inst mulps r y)))))

(define-vop (3d-vectors::%haddps)
  (:policy :fast-safe)
  (:translate 3d-vectors::%haddps)
  (:args (x :scs (single-sse-reg) :target r)
         (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator 4
              (cond ((location= r y)
                     (inst haddps y x))
                    (T
                     (move r x)
                     (inst haddps r y)))))

;;; High-level
(in-package #:org.shirakumo.flare.vector)

(macrolet ((stubdef (name args) `(defun ,name ,args (,name ,@args))))
  (stubdef %rsqrtss (x))
  (stubdef %rsqrtps (x))
  (stubdef %mulps (x y))
  (stubdef %haddps (x y)))

(declaim (inline rsqrtss))
(declaim (ftype (function (single-float) single-float) rsqrtss))
(defun isqrt~ (x)
  (nth-value 0 (sb-ext:%simd-pack-singles
                (%rsqrtss (sb-ext:%make-simd-pack-single x 0.0s0 0.0s0 0.0s0)))))

(declaim (inline f4*))
(declaim (ftype (function (single-float single-float single-float single-float single-float single-float single-float single-float)
                          (values single-float single-float single-float single-float)) f4*))
(defun f4* (a b c d x y z w)
  (sb-ext:%simd-pack-singles (%mulps (sb-ext:%make-simd-pack-single a b c d)
                                   (sb-ext:%make-simd-pack-single x y z w))))

(defun f4/+ (a b c d)
  (let* ((pack (sb-ext:%make-simd-pack-single a b c d))
         (pack (%haddps pack pack))
         (pack (%haddps pack pack)))
    (nth-value 0 (sb-ext:%simd-pack-singles pack))))

(defun simd-vunit (x y z w)
  (let* ((pk (sb-ext:%make-simd-pack-single x y z w))
         (sq (%mulps pk pk))
         (su (%haddps sq sq))
         (su (%haddps su su)))
    (%mulps pk (%rsqrtps su))))



(define-ofun vunit~ (a)
  (etypecase a
    (vec2 (let* ((x (vx2 a)) (y (vy2 a))
                 (m (- 1 (/ (sqrt 2))))
                 (r (/ (max x y)))
                 (r (* r (- (1+ m) (* r m (+ x y))))))
            (vec2 (* r x) (* r y))))
    (vec4 (multiple-value-bind (x y z w) (simd-vunit (vx4 a) (vy4 a) (vz4 a) (vw4 a))
            (vec4 x y z w)))))

(define-ofun nvunit~ (a)
  (etypecase a
    (vec4 (multiple-value-bind (x y z w) (simd-vunit (vx4 a) (vy4 a) (vz4 a) (vw4 a))
            (%vsetf a x y z w)))))
