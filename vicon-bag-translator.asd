(in-package #:cl-user)
(asdf:defsystem vicon-bag-translator
  :name "Vicon-Bag-Translator"
  :version "1.1.0"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Translating from the parsed VICON file into the RSBag format."
  :serial T
  :components ((:file "translator-package")
               (:file "bag-translator")
               (:file "translator-main"))
  :depends-on (:cl-rsbag
               :cl-rsb-common
               :rsbag-tidelog
               :rsb-converter-protocol-buffer
               :vicon-parser)
  :build-operation asdf:program-op
  :build-pathname "vicon-bag-translator"
  :entry-point "vicon-bag-translator:main")
