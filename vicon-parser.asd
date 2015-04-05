(in-package #:cl-user)
(asdf:defsystem vicon-parser
  :name "Vicon-Parser"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A parser for VICON CSV files."
  :serial T
  :components ((:file "package")
               (:file "vicon-parser"))
  :depends-on (:cl-ppcre
               :parse-float))
