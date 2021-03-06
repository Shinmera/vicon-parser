(in-package #:cl-user)
(asdf:defsystem vicon-bag-translator
  :name "Vicon-Bag-Translator"
  :version "1.1.0"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Translating from the parsed VICON file into the RSBag format."
  :homepage "https://Shinmera.github.io/vicon-parser//"
  :bug-tracker "https://github.com/Shinmera/vicon-parser//issues"
  :source-control (:git "https://github.com/Shinmera/vicon-parser/.git")
  :serial T
  :components ((:file "translator-package")
               (:file "bag-translator"))
  :depends-on (:cl-rsbag
               :cl-rsb-common
               :rsbag-tidelog
               :rsb-converter-protocol-buffer
               :rsbag-helper
               :vicon-parser)
  :build-operation asdf:program-op
  :build-pathname "vicon-bag-translator"
  :entry-point "vicon-bag-translator:main")
