#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(sb-c:defknown %rsqrtss ((sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently T)

(defun %rsqrtss (x)
  (%rsqrtss x))

(declaim (inline rsqrtss))
(declaim (ftype (function (single-float) single-float) rsqrtss))
(defun rsqrtss (x)
  (let ((pack (%rsqrtss (sb-ext:%make-simd-pack-single x 0.0s0 0.0s0 0.0s0))))
    (sb-c::%primitive sb-vm::%simd-pack-single-item pack 0)))

(sb-c:defknown %rsqrtps ((sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently T)

(defun %rsqrtps (x)
  (%rsqrtps x))

(sb-c:defknown %f4* ((sb-ext:simd-pack single-float)
                     (sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable sb-c:always-translatable)
  :overwrite-fndb-silently T)

(defun %f4* (x y)
  (%f4* x y))

(declaim (inline f4*))
(declaim (ftype (function (single-float single-float single-float single-float single-float single-float single-float single-float)
                          (values single-float single-float single-float single-float)) f4*))
(defun f4* (a b c d x y z w)
  (sb-ext:%simd-pack-singles (%f4* (sb-ext:%make-simd-pack-single a b c d)
                                   (sb-ext:%make-simd-pack-single x y z w))))

(sb-c:defknown %f4/+ ((sb-ext:simd-pack single-float)
                      (sb-ext:simd-pack single-float)) (sb-ext:simd-pack single-float)
    (sb-c:foldable sb-c:flushable sb-c:movable sb-c:always-translatable)
  :overwrite-fndb-silently T)

(defun %f4/+ (x y)
  (%f4/+ x y))

(defun f4/+ (a b c d)
  (let* ((pack (sb-ext:%make-simd-pack-single a b c d))
         (pack (%f4/+ pack pack))
         (pack (%f4/+ pack pack)))
    (sb-c::%primitive sb-vm::%simd-pack-single-item pack 0)))

(defun simd-vunit (x y z w)
  (let* ((pk (sb-ext:%make-simd-pack-single x y z w))
         (sq (%f4* pk pk))
         (su (%f4/+ sq sq))
         (su (%f4/+ su su)))
    (%f4* pk (%rsqrtps su))))

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

(define-vop (3d-vectors::%f4*)
  (:policy :fast-safe)
  (:translate 3d-vectors::%f4*)
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

(define-vop (3d-vectors::%f4/+)
  (:policy :fast-safe)
  (:translate 3d-vectors::%f4/+)
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
