#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem 3d-vectors-test
  :version "3.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-vectors system."
  :homepage "https://github.com/Shinmera/3d-vectors"
  :serial T
  :components ((:file "test"))
  :depends-on (:3d-vectors :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :3d-vectors-test)))
