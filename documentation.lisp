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
    "Updates the members of the vector with the given values.

Values beyond the vector's arity are silently ignored.

The original vector is returned.

See *VEC (type)")

  (function v<-
    "Updates the members of the vector with values from the given other vector.

The vectors must match in arity.

The updated vector is returned.

See *VEC (type)")

  (function v=
    "Checks whether the passed vectors are equal in all elements.

The elements are checked \"in parallel\", meaning the X element
of each vector is only checked against the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)
See V/=")

  (function v/=
    "Checks whether any of the passed vectors are different in any of the elements.

The elements are checked \"in parallel\", meaning the X element
of each vector is only checked against the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)
See V=")

  (function v<
    "Checks whether the passed vectors' elements are in strictly ascending order.

The elements are checked \"in parallel\", meaning the X element
of each vector is only checked against the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)
See V<=")

  (function v<=
    "Checks whether the passed vectors' elements are in ascending order.

The elements are checked \"in parallel\", meaning the X element
of each vector is only checked against the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)
See V<")

  (function v>
    "Checks whether the passed vectors' elements are in strictly descending order.

The elements are checked \"in parallel\", meaning the X element
of each vector is only checked against the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)
See V>=")

  (function v>=
    "Checks whether the passed vectors' elements are in descending order.

The elements are checked \"in parallel\", meaning the X element
of each vector is only checked against the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)
See v>")

  (function v.
    "Returns the dot product of the two vectors.

The vectors must match in type.

See *VEC (type)")

  (function vdistance
    "Returns the euclidean distance between the two vectors.

The vectors must match in type.

See *VEC (type)")

  (function vsqrdistance
    "Returns the squared euclidean distance between the two vectors.

The vectors must match in type.

See *VEC (type)")

  (function v1norm
    "Returns the taxicab/1-norm of the vector.

See *VEC (type)")

  (function vinorm
    "Returns the maximum/infinity-norm of the vector.

See *VEC (type)")

  (function v2norm
    "Returns the euclidean/2-norm of the vector.

See *VEC (type)")

  (function vpnorm
    "Returns the p-norm of the vector.

See *VEC (type)")

  (function vsqrlength
    "Returns the squared length of the vector.

See *VEC (type)")

  (function vlike
    "Returns a vector of the same element-type and the specified arity.

The returned vector is zeroed out in all elements.

See *VEC (type)")

  (function v+
    "Adds the elements of each of the specified vectors up and returns a fresh vector with the result.

The elements are added \"in parallel\", meaning the X element
of each vector is only added to the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)")

  (function v-
    "Subtracts the elements of each of the specified vectors from the first vector and returns a fresh vector with the result.

The elements are subtracted \"in parallel\", meaning the X element
of each vector is only subtracted from the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)")

  (function v*
    "Multiplies the elements of each of the specified vectors up and returns a fresh vector with the result.

The elements are multiplied \"in parallel\", meaning the X element
of each vector is only multiplied with the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)")

  (function v/
    "Divides the elements of the first vector by each of the specified vectors and returns a fresh vector with the result.

The elements are divided \"in parallel\", meaning the X element
of each vector is only divided by the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)")

  (function vmin
    "Returns a fresh vector with each element being the minimum of all vectors.

The elements are compared \"in parallel\", meaning the X element
of each vector is only compared to the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)")

  (function vmax
    "Returns a fresh vector with each element being the maximum of all vectors.

The elements are compared \"in parallel\", meaning the X element
of each vector is only compared to the X element of others, and so
forth.

The vectors must match in arity.
You may also pass a REAL in place of a vector, in which case the REAL
is treated as if it were a vector with the same value in all
elements.

See *VEC (type)")

  (function vabs
    "Returns a fresh vector with each element being the absolute of the passed vector.

See *VEC (type)")

  (function vmod
    "Returns a fresh vector with each element being modulated by the divider.

See *VEC (type)")

  (function vfloor
    "Returns a fresh vector with each element being floored by the divider.

See *VEC (type)")

  (function vceiling
    "Returns a fresh vector with each element being ceilinged by the divider.

See *VEC (type)")

  (function vround
    "Returns a fresh vector with each element being rounded by the divider.

See *VEC (type)")

  (function vc
    "Returns the cross product of the two vectors.

The vectors must be 3-element vectors.

See *VEC3 (type)")

  (function vrot
    "Returns a fresh vector that is the rotation of the vector by the given axis and angle.

The vector must be a 3-element vector.

See *VEC3 (type)")

  (function vrot2
    "Returns a fresh vector that is the rotation of the vector by the given angle.

The vector must be a 2-element vector.

See *VEC2 (type)")

  (function valign
    "Returns a fresh vector with each element aligned to the specified grid.

The elements are rounded to the grid.

See *VEC (type)")

  (function vcartesian
    "Returns a fresh cartesian coordinate version of the given polar/spherical coordinate vector.

For a polar/spherical vector, the first element is the radius, the
second the theta inclination and the third the phi azimuth.
The vector must be a 2 or 3-element vector.

See *VEC2 (type)
See *VEC3 (type)
See VPOLAR")

  (function vpolar
    "Returns a fresh polar/spherical coordinate version of the given cartesian coordinate vector.

For a polar/spherical vector, the first element is the radius, the
second the theta inclination and the third the phi azimuth.
The vector must be a 2 or 3-element vector.

See *VEC2 (type)
See *VEC3 (type)
See VPOLAR")

  (function vlerp
    "Returns a fresh linear interpolation vector between the two given vectors.

Each element is interpolated independently.
The two vectors must match in type.

See *VEC (type)")

  (function vrand
    "Returns a fresh randomised vector around the given vector by the given variance.

The vectors must match in type.
The elements of the fresh vector are randomised as follows:

    (+ (X V) (- (RANDOM (X VAR)) (/ (X VAR) 2)))

Meaning the randomisation diameter is VAR around the origin V.

See *VEC (type)")

  (function vapply
    "Returns a fresh vector where each element is computed by calling the function on each element of the original vector

The function must accept one argument and return a REAL.

See *VEC (type)
See VAPPLYF")

  (function vapplyf
    "Returns the modified vector where each element is computed by calling the function on each element of the vector

The function must accept one argument and return a REAL.

See *VEC (type)
See VAPPLY")

  (function vorder
    "Return a fresh vector with the elements taken from the given vector in the specified order.

This can be understood as a load operation where the result vector's
elements are designated by the corresponding element in the order
symbol.

    (VORDER (VEC 1 2 3) :ZYX) ; => (VEC 3 2 1)
    (VORDER (VEC 1 2) :XYXY)  ; => (VEC 1 2 1 2)

You may also SETF this place:

    (SETF (VORDER (VEC3) :Z_W) (VEC 1 2 3 4)) ; => (VEC 3 0 4)

The symbol naming the fields may contain the characters X Y Z _
wherein _ denotes \"do not touch\".

See *VEC (type)")

  (function vunit
    "Return a fresh unit vector of the given vector.

This signals an error if the vector is zero.

See *VEC (type)
See VUNIT*")

  (function vunit*
    "Return a fresh unit vector of the given vector, ignoring zeroes.

If the vector is zero, the returned vector is also zero.

See *VEC (type)
See VUNIT")

  (function vrotv
    "Return a fresh vector which is the rotation of the given vector by rotating it around each axis.

The two vectors must be 3-element vectors.
The secondary vector's element specify the angle around each axis to
rotate by. Rotation happens in order of X, Y, Z.

See *VEC3 (type)
See VROT")

  (function vscale
    "Return a fresh vector which is the original vector but with the specified length.

This signals an error if the vector is zero.

See *VEC (type)
See V2NORM
See V*")
  
  (function vincf
    "Increases each element of the vector by the given delta.

Returns the same vector.

See *VEC (type)")

  (function vdecf
    "Decreases each element of the vector by the given delta.

Returns the same vector.

See *VEC (type)")

  (function vlength
    "Returns the euclidean norm or length of the vector.

See *VEC (type)
See V2NORM")

  (function v1+
    "Returns a fresh vector with each element increased by one from the original vector.

See *VEC (type)
See V+")

  (function v1-
    "Returns a fresh vector with each element decreased by one from the original vector.

See *VEC (type)
See V+")

  (function vangle
    "Returns the angle between the two vectors.

This is computed as: (ACOS (/ (V. A B) (V2NORM A) (V2NORM B)))

See *VEC (type)")

  (function vclamp
    "Returns a fresh vector with each element clamped by the specified lower and upper bounds.

See *VEC (type)")

  (variable +vx2+
    "Constant 2-element single-float vector with the following elements: [ 1 0 ]

See VEC2 (type)")

  (variable +vy2+
    "Constant 2-element single-float vector with the following elements: [ 0 1 ]

See VEC2 (type)")

  (variable +vx3+
    "Constant 3-element single-float vector with the following elements: [ 1 0 0 ]

See VEC3 (type)")

  (variable +vy3+
    "Constant 3-element single-float vector with the following elements: [ 0 1 0 ]

See VEC3 (type)")

  (variable +vz3+
    "Constant 3-element single-float vector with the following elements: [ 0 0 1 ]

See VEC3 (type)")

  (variable +vx4+
    "Constant 4-element single-float vector with the following elements: [ 1 0 0 0 ]

See VEC4 (type)")

  (variable +vy4+
    "Constant 4-element single-float vector with the following elements: [ 0 1 0 0 ]

See VEC4 (type)")

  (variable +vz4+
    "Constant 4-element single-float vector with the following elements: [ 0 0 1 0 ]

See VEC4 (type)")

  (variable +vw4+
    "Constant 4-element single-float vector with the following elements: [ 0 0 0 1 ]

See VEC4 (type)")

  (variable +vx+
    "Constant 3-element single-float vector with the following elements: [ 1 0 0 ]

See VEC3 (type)")

  (variable +vy+
    "Constant 3-element single-float vector with the following elements: [ 0 1 0 ]

See VEC3 (type)")

  (variable +vz+
    "Constant 3-element single-float vector with the following elements: [ 0 0 1 ]

See VEC3 (type)"))
