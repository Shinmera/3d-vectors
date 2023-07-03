(in-package #:org.shirakumo.flare.vector)

;; ops.lisp
(docs:define-docs
  (with-vec2 "Binds each component of the vector (or real) to the specified variable.")
  (with-vec3 "Binds each component of the vector (or real) to the specified variable.")
  (with-vec4 "Binds each component of the vector (or real) to the specified variable.")
  (with-vec "Binds each component of the vector (or real) to the specified variable.
If the vector does not have a particular field, the variable is initialized to 0 in the proper float format.")

  (v= "This is the same as =, but testing all vector fields simultaneously.")
  (v/= "This is the same as /=, but testing all vector fields simultaneously.")
  (v< "This is the same as <, but testing all vector fields simultaneously.")
  (v<= "This is the same as <=, but testing all vector fields simultaneously.")
  (v> "This is the same as >, but testing all vector fields simultaneously.")
  (v>= "This is the same as >=, but testing all vector fields simultaneously.")

  (vmin "Same as MIN, but testing all vector fields simultaneously.")
  (vmax "Same as MAX, but testing all vector fields simultaneously.")

  (variable +vx2+ "Constant vector for the 2D unit in X direction.")
  (variable +vy2+ "Constant vector for the 2D unit in Y direction.")
  (variable +vx3+ "Constant vector for the 3D unit in X direction.")
  (variable +vy3+ "Constant vector for the 3D unit in Y direction.")
  (variable +vz3+ "Constant vector for the 3D unit in Z direction.")
  (variable +vx4+ "Constant vector for the 4D unit in X direction.")
  (variable +vy4+ "Constant vector for the 4D unit in Y direction.")
  (variable +vz4+ "Constant vector for the 4D unit in Z direction.")
  (variable +vw4+ "Constant vector for the 4D unit in W direction.")
  (variable +vx+ "Constant vector for the 3D unit in X direction.")
  (variable +vy+ "Constant vector for the 3D unit in Y direction.")
  (variable +vz+ "Constant vector for the 3D unit in Z direction.")
  
  (vdistance "Returns the distance from A to B.")
  (vsqrdistance "Returns the squared distance from A to B.")
  (vlength "Returns the euclidean norm of the vector.")
  (vlength "Returns the squared euclidean norm of the vector.")
  (v2norm "Returns the euclidean/2-norm of the vector.")
  (v1norm "Returns the taxicab/1-norm of the vector.")
  (vinorm "Returns the maximum-norm of the vector.")
  (vpnorm "Returns the p-norm of the vector.")

  (vsetf "Similar to SETF, but requires as many values as the given vector has fields. Returns the modified vector.")

  (vapply "Applies OP to each applicable field of the vector plus the optional argument for each respective dimension, if given. Returns a new vector of the same type with the results in its fields.")
  (vapplyf "Applies OP to each applicable field of the vector plus the optional argument for each respective dimension, if given. Returns the same vector with the results stored in its fields.")

  (v+ "Same as +, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (v- "Same as -, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (v* "Same as *, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (v/ "Same as /, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv+ "Same as +, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv- "Same as -, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv* "Same as *, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv/ "Same as /, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv+* "Same as nv+, but scales the added vector by a scalar.")

  (v1+ "Same as 1+, but returns a new vector with each component increased by 1.")
  (v1- "Same as 1-, but returns a new vector with each component decreased by 1.")

  (vincf "Increases each field in the vector by DELTA.")
  (vdecf "Decreases each field in the vector by DELTA.")

  (v. "Returns the dot product of the two vectors.")
  (vc "Returns the cross product of the two 3D vectors.
This operation does not work with 2D or 4D vectors.")
  (vangle "Returns the angle between two vectors.")

  (vabs "Returns a vector with each component being the absolute value of the given vector's.")
  (nvabs "Performs ABS on each component of the vector and stores back the results.")

  (vmod "Returns a vector with each component being the modulus of the given vector's against the divisor.")
  (nvmod "Performs MOD on each component of the vector and stores back the results.")

  (vunit "Returns the unit vector form of the given vector by the 2-norm. If the vector is zero, an error is signalled.
See VUNIT*")
  (nvunit "Normalizes the vector into its unit form by the 2-norm. If the vector is zero, an error is signalled.
See NVUNIT*")

  (vunit* "Returns the unit vector form of the given vector by the 2-norm. If the vector is zero, returns a zero vector.
See VUNIT")
  (nvunit* "Normalizes the vector into its unit form by the 2-norm. If the vector is zero, returns it unmodified.
See NVUNIT")

  (vscale "Returns a scaled vector of the specified length.")
  (nvscale "Scales the vector to be of the specified length.")

  (vfloor "Returns a vector with all components floored.")
  (vfloor "Floors all components of the vector.")
  (vceiling "Returns a vector with all components ceilinged.")
  (vceiling "Ceilings all components of the vector.")
  (vround "Returns a vector with all components rounded.")
  (vround "Rounds all components of the vector.")
  
  (vclamp "Returns a clamped vector where each field is within [LOWER, UPPER]. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nvclamp "Clamps the vector such that each field is within [LOWER, UPPER]. Accepts REALs or VECs as limits, where REALs are used for each component of the vector.")
  
  (vlimit "Returns a limited vector where each field is within [-LIMIT, LIMIT]. Accepts REALs or VEcs as arguments, where REALs are used for each component of the vector.")
  (nvlimit "Limits the vector such that each field is within [-LIMIT, LIMIT]. Accepts a REAL or VEc for the limit, where a REAL is used for each component of the vector.")

  (vlerp "Returns a vector where each field is linearly interpolated from the corresponding field in FROM to TO by N. Accepts a REAL or VEC for N, where REALs are used for each component of the vector.")

  (vrot "Returns a 3D vector rotated around AXIS by PHI rads. The axis has to be a unit vector.
This operation does not work with 2D or 4D vectors.

See VROT2")
  (nvrot "Rotates the 3D vector around AXIS by PHI rads. The axis has to be a unit vector.
This operation does not work with 2D or 4D vectors.

See NVROT2")
  
  (vrotv "Returns a 3D vector of A rotated around each axis by the amount in B. The rotations are performed in the order of X, Y, Z.
Note that rotation in 3D space is not commutative, so this function might not perform the rotation as you expected if you need the rotation to happen in a different order.
This operation does not work with 2D or 4D vectors.

See VROT.")
  (nvrotv "Rotates the 3D vector A around each axis by the amount in B. The rotations are performed in the order of X, Y, Z.
Note that rotation in 3D space is not commutative, so this function might not perform the rotation as you expected if you need the rotation to happen in a different order.
This operation does not work with 2D or 4D vectors.

See NVROT.")
  (vrot2 "Returns a 2D vector rotated around zero by PHI rads.")
  (nvrot2 "Rotates the 2D vector A around zero by PHI rads.")

  (v<- "Copies the fields from SOURCE into TARGET.")
  
  (vrand "Returns a vector with each of the fields having a value in [x-var, x+var].
Either X or VAR must be a vector. If both are vectors, they must match in type.")

  (valign "Returns a vector aligned to the given grid size.")
  (nvalign "Aligns the vector to the given grid size.")

  (vpolar "Returns the polar/spherical coordinate translation of the given cartesian vector.
For 2D vectors it will be R,PHI, for 3D, R,PHI,THETA")
  (vcartesian "Returns the cartesian coordinate translation of the given polar/spherical vector.
For 2D vectors it has to be R,PHI, for 3D R,PHI,THETA")

  (vorder "Allows you to handily create a new vector with reordered components.
Each X/Y/Z argument can be one of 'X,'Y,'Z,'VX,'VY,'VZ,:X,:Y,:Z indicating the respective component, or NIL for 0.")
  (vorder "Allows you to handily modify a vector by reordering its components.
Each X/Y/Z argument can be one of 'X,'Y,'Z,'VX,'VY,'VZ,:X,:Y,:Z indicating the respective component, or NIL for 0."))

;; struct.lisp
(docs:define-docs
  (type vec2
    "A two-dimensional vector with X and Y fields.")

  (vx2 "Returns the X component of a 2D vector.")
  (vy2 "Returns the Y component of a 2D vector.")

  (vcopy2 "Creates a copy of a 2D vector.")
  (vec2-p "Returns T if the argument is of type vec2.")
  (vec2 "Constructs a 2D vector.")
  (vec2-random "Constructs a 2D vector with random values according to the given bounds.")

  (type vec3
    "A three-dimensional vector with X, Y, and Z fields.")

  (vx3 "Returns the X component of a 3D vector.")
  (vy3 "Returns the Y component of a 3D vector.")
  (vz3 "Returns the Z component of a 3D vector.")

  (vcopy3 "Creates a copy of a 3D vector.")
  (vec3-p "Returns T if the argument is of type vec3.")
  (vec3 "Constructs a 3D vector.")
  (vec3-random "Constructs a 3D vector with random values according to the given bounds.")

  (type vec4
    "A four-dimensional vector with X, Y, Z, and W fields.")

  (vx4 "Returns the X component of a 4D vector.")
  (vy4 "Returns the Y component of a 4D vector.")
  (vz4 "Returns the Z component of a 4D vector.")
  (vw4 "Returns the W component of a 4D vector.")

  (vcopy4 "Creates a copy of a 4D vector.")
  (vec4-p "Returns T if the argument is of type vec4.")
  (vec4 "Constructs a 3D vector.")
  (vec4-random "Constructs a 4D vector with random values according to the given bounds.")

  (type vec
    "Either a vec2, vec3, or vec4.")

  (vx "Returns the X component of the vector.")
  (vy "Returns the Y component of the vector.")
  (vz "Returns the Z component of the vector.")
  (vw "Returns the W component of the vector.")
  
  (vcopy "Creates a copy of the vector.")
  (vec-p "Returns T if the argument is a vector.")

  (vec "Creates a new vector of the appropriate size.")

  (vec-from-vector "Create a vector from a lisp array vector. The array must be 2-4 elements long."))
