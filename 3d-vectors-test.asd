(asdf:defsystem 3d-vectors-test
  :version "3.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-vectors system."
  :homepage "https://Shinmera.github.io/3d-vectors/"
  :bug-tracker "https://github.com/Shinmera/3d-vectors/issues"
  :source-control (:git "https://github.com/Shinmera/3d-vectors.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:3d-vectors :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :3d-vectors-test)))
