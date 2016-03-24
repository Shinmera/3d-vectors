#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem 3d-vectors
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A small utility library implementing basic 3d vector functionality."
  :homepage "https://github.com/Shinmera/3d-vectors"
  :serial T
  :components ((:file "package")
               (:file "struct")
               (:file "ops")
               (:file "documentation"))
  :depends-on ())
