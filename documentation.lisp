#|
This file is a part of 3d-vectors
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  ((vec type)
   "Vector struct with three DOUBLE-FLOAT fields: VX, VY, VZ.")

  (vx "Returns a double-float for the X component of the vector.")
  (vy "Returns a double-float for the Y component of the vector.")
  (vz "Returns a double-float for the Z component of the vector.")

  (vcopy "Creates a copy of the vector.")
  (vec-p "Returns T if the argument is a vector.")

  (vec "Creates a new vector. The arguments passed must be REALs and will be coerced into DOUBLE-FLOATs.")

  (with-vec "Binds each component of the vector to the specified variable.")

  (v= "This is the same as =, but testing all vector fields simultaneously.")
  (v/= "This is the same as /=, but testing all vector fields simultaneously.")
  (v< "This is the same as <, but testing all vector fields simultaneously.")
  (v<= "This is the same as <=, but testing all vector fields simultaneously.")
  (v> "This is the same as >, but testing all vector fields simultaneously.")
  (v>= "This is the same as >=, but testing all vector fields simultaneously.")

  (vmin "Same as MIN, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (vmax "Same as MAX, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")

  ((+vx+ variable) "Constant vector for the unit in X direction.")
  ((+vy+ variable) "Constant vector for the unit in Y direction.")
  ((+vz+ variable) "Constant vector for the unit in Z direction.")

  (vlength "Returns the euclidean norm of the vector.")

  (vsetf "Same as SETF but for vectors. Requires three values per vector.
Example: (setf v x y z 
               w a b c)")

  (vmodf "Modifies the fields in VEC by passing each field to OP as the first argument, and optionally the X, Y, and Z values as second arguments.")

  (v+ "Same as +, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (v- "Same as -, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (v* "Same as *, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (v/ "Same as /, but always returns a vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv+ "Same as +, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv- "Same as -, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv* "Same as *, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nv/ "Same as /, but modifies the first vector. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")

  (v1+ "Same as 1+, but returns a new vector with each component increased by 1.")
  (v1- "Same as 1-, but returns a new vector with each component decreased by 1.")

  (vincf "Increases each field in the vector by DELTA.")
  (vdecf "Decreases each field in the vector by DELTA.")

  (v. "Returns the dot product of the two vectors.")
  (vc "Returns the cross product of the two vectors.")

  (vabs "Returns a vector with each component having the ABS value.")
  (nvabs "Performs ABS on each component of the vector.")

  (vunit "Returns the unit vector form of the given vector.")
  (nvunit "Normalizes the vector into its unit form.")

  (vscale "Returns a scaled vector of the specified length.")
  (nvscale "Scales the vector to be of the specified length.")
  
  (vclamp "Returns a clamped vector where each field is within [LOWER, UPPER]. Accepts REALs or VECs as arguments, where REALs are used for each component of the vector.")
  (nvclamp "Clamps the vector such that each field is within [LOWER, UPPER]. Accepts REALs or VECs as limits, where REALs are used for each component of the vector.")
  
  (vlimit "Returns a limited vector where each field is within [-LIMIT, LIMIT]. Accepts REALs or VEcs as arguments, where REALs are used for each component of the vector.")
  (nvlimit "Limits the vector such that each field is within [-LIMIT, LIMIT]. Accepts a REAL or VEc for the limit, where a REAL is used for each component of the vector.")

  (vrot "Returns a vector rotated around AXIS by PHI rads. The axis has to be a unit vector.")
  (nvrot "Rotates the vector around AXIS by PHI rads. The axis has to be a unit vector.")
  
  (vrotv "Returns a vector of A rotated around each axis by the amount in B. The rotations are performed in the order of X, Y, Z.
Note that rotation in 3D space is not commutative, so this function might not perform the rotation as you expected if you need the rotation to happen in a different order.

See VROT.")
  (nvrotv "Rotates the vector A around each axis by the amount in B. The rotations are performed in the order of X, Y, Z.
Note that rotation in 3D space is not commutative, so this function might not perform the rotation as you expected if you need the rotation to happen in a different order.

See NVROT."))
