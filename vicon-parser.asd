(in-package #:cl-user)
(asdf:defsystem vicon-parser
  :name "Vicon-Parser"
  :version "1.1.0"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A parser for VICON CSV files."
  :serial T
  :components ((:file "parser-package")
               (:file "vicon-parser"))
  :depends-on (:alexandria
               :cl-ppcre
               :parse-float
               :zip))
