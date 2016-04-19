#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:3d-vectors
  (:nicknames #:org.shirakumo.flare.vector)
  (:use #:cl)
  ;; ops.lisp
  (:export
   #:v=
   #:v<
   #:v<=
   #:v>
   #:v>=
   #:vmin
   #:vmax
   #:+vx+
   #:+vy+
   #:+vz+
   #:vlength
   #:vsetf
   #:v+
   #:v-
   #:v*
   #:v/
   #:nv+
   #:nv-
   #:nv*
   #:nv/
   #:v1+
   #:v1-
   #:vincf
   #:vdecf
   #:v.
   #:vc
   #:vabs
   #:nvabs
   #:vunit
   #:nvunit
   #:vscale
   #:nvscale
   #:vclamp
   #:nvclamp
   #:vlimit
   #:nvlimit
   #:vrot
   #:nvrot
   #:vrotv
   #:nvrotv)
  ;; struct.lisp
  (:export
   #:vec
   #:vec-p
   #:vcopy
   #:vx
   #:vy
   #:vz
   #:vec))
