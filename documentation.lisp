#|
 This file is a part of 3d-vectors
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vectors)

;; types.lisp
(docs:define-docs
  (type fvec
    "Type encapsulating single-float vectors of any arity.

See VEC (function)
See VEC2 (type)
See VEC3 (type)
See VEC4 (type)")

  (type dvec
    "Type encapsulating double-float vectors of any arity.

See DVEC (function)
See DVEC2 (type)
See DVEC3 (type)
See DVEC4 (type)")

  (type ivec
    "Type encapsulating integer vectors of any arity.

See IVEC (function)
See IVEC2 (type)
See IVEC3 (type)
See IVEC4 (type)")

  (type *vec2
    "Type encapsulating 2-element vectors of any element type.

See VEC2 (type)
See DVEC2 (type)
See IVEC2 (type)")

  (type *vec3
    "Type encapsulating 3-element vectors of any element type.

See VEC3 (type)
See DVEC3 (type)
See IVEC3 (type)")

  (type *vec4
    "Type encapsulating 4-element vectors of any element type.

See VEC4 (type)
See DVEC4 (type)
See IVEC4 (type)")

  (type *vec
    "Type encapsulating vectors of any arity and element type.

See VEC2 (type)
See VEC3 (type)
See VEC4 (type)
See DVEC2 (type)
See DVEC3 (type)
See DVEC4 (type)
See IVEC2 (type)
See IVEC3 (type)
See IVEC4 (type)")

  (type vec
    "Type encapsulating vectors of any arity and element type.

See VEC2 (type)
See VEC3 (type)
See VEC4 (type)
See DVEC2 (type)
See DVEC3 (type)
See DVEC4 (type)
See IVEC2 (type)
See IVEC3 (type)
See IVEC4 (type)")

  (function vec
    "Construct a single-float vector of arbitrary arity.

You may pass in another vector of any type to create a single-float
copy of it:

    (vec (dvec4))

You may also splice multiple single-float vectors together or mix them
with single values to form a new vector:

    (vec 1 2 3)
    (vec (vec2) 3)
    (vec 1 (vec3))

Note that you cannot splice in vectors of another element-type.

See FVEC (type)
See VEC2
See VEC3
See VEC4")

  (type vec2
    "A single-float vector with two elements.

See VEC2
See VX2
See VY2")

  (type vec3
    "A single-float vector with three elements.

See VEC3
See VX3
See VY3
See VZ3")

  (type vec4
    "A single-float vector with four elements.

See VEC4
See VX4
See VY4
See VZ4
See VW4")

  (function vec2
    "Construct a 2-element single-float vector.

Elements that are not passed explicitly are initialized to zero.

See VEC
See VEC2 (type)")

  (function vec3
    "Construct a 3-element single-float vector.

Elements that are not passed explicitly are initialized to zero.

See VEC
See VEC3 (type)")

  (function vec4
    "Construct a 4-element single-float vector.

Elements that are not passed explicitly are initialized to zero.

See VEC
See VEC4 (type)")

  (function vx
    "Access the first element of the vector.

Works for vectors of any arity and element-type.

See *VEC (type)")

  (function vy
    "Access the second element of the vector.

Works for vectors of any arity and element-type.

See *VEC (type)")

  (function vz
    "Access the third element of the vector.

Works for vectors of arity 3 and 4 and any element-type.

See *VEC3 (type)
See *VEC4 (type)")

  (function vw
    "Access the fourth element of the vector.

Works for vectors of arity 4 and any element-type.

See *VEC4 (type)")

  (function vcopy
    "Create a copy of the vector.

Works for vectors of any arity and element-type.

See *VEC (type)")

  (function vzero
    "Create a copy of the vector with all fields zerod out.

Works for vectors of any arity and element-type.

See *VEC (type)")

  (function with-vec
    "Bind the variables to convenient accessors for the given elements of the vector.

See *VEC (type)
See VX
See VY
See VZ
See VW"))

;; ops.lisp
(docs:define-docs
  (function vsetf
    "")

  (function v<-
    "")

  (function v=
    "")

  (function v/=
    "")

  (function v<
    "")

  (function v<=
    "")

  (function v>
    "")

  (function v>=
    "")

  (function v.
    "")

  (function vdistance
    "")

  (function vsqrdistance
    "")

  (function v1norm
    "")

  (function vinorm
    "")

  (function v2norm
    "")

  (function vpnorm
    "")

  (function vsqrlength
    "")

  (function vlike
    "")

  (function v+
    "")

  (function v-
    "")

  (function v*
    "")

  (function v/
    "")

  (function vmin
    "")

  (function vmax
    "")

  (function vabs
    "")

  (function vmod
    "")

  (function vfloor
    "")

  (function vceiling
    "")

  (function vround
    "")

  (function vc
    "")

  (function vrot
    "")

  (function vrot2
    "")

  (function valign
    "")

  (function vcartesian
    "")

  (function vpolar
    "")

  (function vlerp
    "")

  (function vrand
    "")

  (function vapply
    "")

  (function vapplyf
    "")

  (function vorder
    "")

  (function vunit
    "")

  (function vunit*
    "")

  (function vrotv
    "")

  (function vscale
    "")

  (function vincf
    "")

  (function vdecf
    "")

  (function vlength
    "")

  (function v1+
    "")

  (function v1-
    "")

  (function vangle
    "")

  (function vclamp
    "")

  (variable +vx2+
    "")

  (variable +vy2+
    "")

  (variable +vx3+
    "")

  (variable +vy3+
    "")

  (variable +vz3+
    "")

  (variable +vx4+
    "")

  (variable +vy4+
    "")

  (variable +vz4+
    "")

  (variable +vw4+
    "")

  (variable +vx+
    "")

  (variable +vy+
    "")

  (variable +vz+
    ""))
