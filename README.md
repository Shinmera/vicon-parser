# Vicon Bag Converter #

Converts textfiles exported from the Vicon motion capturing system
into [RSBag](https://code.cor-lab.org/projects/rsbag) log files.

# Building #

In Lisp

```cl
(sb-posix:setenv "RST" "PATH_TO_RST_SUPERPROJECT" 1) ; or equivalent
(asdf:oos 'asdf:program-op :vicon-bag-translator)
```

Via ``Makefile``

```bash
export RST=PATH_TO_RST_SUPERPROJECT
make
```

# Usage #

    vicon-bag-translator OUTPUT INPUT₁ [INPUT₂ ...]

Where ``OUTPUT`` is the name of the TIDE log file into which the
merged content of ``INPUT₁``, ``INPUT₂``, … should be written.

``OUTPUT`` can be any filename ending with the ``.tide``
extension. The filenames of ``INPUT₁``, ``INPUT₂``, … have to be of
the form

    vicon_YYYYMMDD_hhmmss.txt

where ``YYYYMMDD_hhmmss`` corresponds to the point in time at which
the first Vicon frame in the file has been recorded.

If multiple input files are specified, their data is concatenated
(assuming the files correspond to the individual segments of a single
recorded trial) and stored into a single channel in the output log
file.

# License #

LGPLv2.1
