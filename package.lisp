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
   #:with-vec2
   #:with-vec3
   #:with-vec4
   #:with-vec
   #:v=
   #:v/=
   #:v<
   #:v<=
   #:v>
   #:v>=
   #:vmin
   #:vmax
   #:+vx2+
   #:+vy2+
   #:+vx3+
   #:+vy3+
   #:+vz3+
   #:+vx4+
   #:+vy4+
   #:+vz4+
   #:+vw4+
   #:+vx+
   #:+vy+
   #:+vz+
   #:vlength
   #:v2norm
   #:v1norm
   #:vinorm
   #:vpnorm
   #:vsetf
   #:vapply
   #:vapplyf
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
   #:vmod
   #:nvmod
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
   #:nvrotv
   #:vorder
   #:nvorder
   ;; swizzlers are autoexport.
   )
  ;; struct.lisp
  (:export
   #:vec2
   #:vec2-p
   #:vcopy2
   #:vx2
   #:vy2
   #:vec2-random
   #:vec3
   #:vec3-p
   #:vcopy3
   #:vx3
   #:vy3
   #:vz3
   #:vec3-random
   #:vec4
   #:vec4-p
   #:vcopy4
   #:vx4
   #:vy4
   #:vz4
   #:vw4
   #:vec4-random
   #:vec
   #:vec-p
   #:vcopy
   #:vx
   #:vy
   #:vz
   #:vw
   #:vec))
