(asdf:defsystem 3d-vectors
  :version "3.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A utility library implementing 2D, 3D, and 4D vector functionality."
  :homepage "https://Shinmera.github.io/3d-vectors/"
  :bug-tracker "https://github.com/Shinmera/3d-vectors/issues"
  :source-control (:git "https://github.com/Shinmera/3d-vectors.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               ;;(:file "ssa")
               (:file "struct")
               (:file "ops")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-vectors-test))))
