all: vicon-bag-translator

vicon-bag-translator: vicon-bag-translator.asd *.lisp
	cl -Q                                                                           \
	  -S "(:source-registry (:directory \""$$(pwd)"\") :inherit-configuration)"     \
	  '(asdf:oos (quote asdf:program-op) :vicon-bag-translator)'

.PHONY: all
