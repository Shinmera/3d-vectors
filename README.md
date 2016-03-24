## About 3d-vectors
This is a simple library for 3D vectors. It contains most of the vector operations one would usually expect out of such a library and offers them both in non-modifying and modifying versions where applicable. It also tries to be efficient where plausible. Each vector is made up of a triplet of `float`s, which by default are `single-float`s, as they do not require value boxing on most modern systems and compilers.

## How To
Load it through ASDF or Quicklisp

    (ql:quickload :3d-vectors)
    (use-package :3d-vectors)

Create a vector:

    (vec 0 0 0)

Vectors always use a triplet of `float`s. All operations should accept `real` numbers though, for convenience. All vector operations will return a `vec` and are prefixed with a `v` to allow importing of the package. 

    (v+ 1 2 3 (vec 4 5 6))

3d-vectors implements pretty much all vector operations you might need, including comparators, dot and cross product, and rotation. There's also modifying variants of all operators, which have the same name, except they are prefixed by an `n`.

    (let ((v (vec 0 0 0)))
      (nv* (nv+ v (vec 1 2 3)) 3)
      v)

`vec`s are dumpable, meaning you can insert them as literals into your code and they will be properly saved to and restored from a FASL.

If you require higher precision than `single-float`s ensure, you can add `:3d-vectors-double-floats` to `*features*` and recompile the library `(asdf:compile-system :3d-vectors :force T)`. Similarly, if you want to switch back to `single-float`s, you can remove the feature and recompile. Both at the same time is not currently possible as it would increase complexity in the library and make certain operations much slower.
