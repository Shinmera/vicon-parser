all: vicon-bag-translator

vicon-bag-translator: vicon-bag-translator.asd *.lisp
	cl                                                                              \
	  -Q                                                                            \
	  -S "(:source-registry (:directory \""$$(pwd)"\") :inherit-configuration)"     \
	  -s vicon-bag-translator                                                       \
	  --dump ! --output $@ -r "vicon-bag-translator:main"

.PHONY: all
