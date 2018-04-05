## About 3d-vectors
This is a library for vector math in 3D space. It contains most of the vector operations one would usually expect out of such a library and offers them both in non-modifying and modifying versions where applicable. It also tries to be efficient where plausible. Each vector is made up of `float`s, which by default are `single-float`s, as they do not require value boxing on most modern systems and compilers. Despite the name of this library, 2D and 4D vectors are supported as well.

## How To
Load it through ASDF or Quicklisp

    (ql:quickload :3d-vectors)
    (use-package :3d-vectors)

Create a vector:

    (vec 0 0 0)

Vectors always use `float`s. Where sensible, operations should accept `real` numbers for convenience. All vector operations are prefixed with a `v` to allow importing the package without conflicts. 

    (v+ (vec 1 2 3) 4 5 6)

3d-vectors implements pretty much all vector operations you might need, including comparators, dot and cross product, and rotation. There's also modifying variants of all operators, which have the same name, except they are prefixed by an `n`.

    (let ((v (vec 0 0 0)))
      (nv* (nv+ v (vec 1 2 3)) 3)
      v)

`vec`s are dumpable, meaning you can insert them as literals into your code and they will be properly saved to and restored from a FASL.

The type `vec` includes all three subtypes `vec2`, `vec3`, and `vec4`. Each of the three also has its own accessors that are suffixed with the dimension number. While the standard `vx`, `vy`, `vz`, and `vw` will result in the lower-level variants through an `etypecase`, it is usually a good idea to use `vx2` etc if the type is already known to avoid unnecessary dispatch or branch elimination.

While most of the operations work on all three variants, you cannot intermix them. For example, `(v+ (vec 1 2) (vec 1 2 3))` will signal an error. This is because it is often ambiguous and thus likely confusing as to what might happen in such a case. Should the result be upgraded to a `vec3` or downgraded to a `vec2`? In order to avoid this ambiguity, it is simply left up to you to ensure proper types.

One convenient way to switch around between the types and generally flip around the vector fields is swizzling: similar to the single-field accessors, there's multi-field readers that construct a new vector from the specified fields of the necessary length.

    (vxy (vec 1 2 3))    ; => (vec2 1 2)
    (vxy_ (vec 1 2))     ; => (vec3 1 2 0)
    (vwwx (vec 1 2 3 4)) ; => (vec3 4 4 1)

The `_` can be used anywhere within swizzle operators in order to pad the vector with a zero. You can also use the swizzle operators as accessors to set multiple fields of a vector at once.

If you require higher precision than `single-float`s ensure, you can add `:3d-vectors-double-floats` to `*features*` and recompile the library `(asdf:compile-system :3d-vectors :force T)`. Similarly, if you want to switch back to `single-float`s, you can remove the feature and recompile. Both at the same time is not supported as it would increase complexity in the library massively and make certain operations much slower.

## Also See

* [3d-matrices](https://shinmera.github.io/3d-matrices) for Matrix operations in conjunction with this library.
