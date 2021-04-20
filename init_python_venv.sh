#!/bin/sh

# a sample initialization of a virtualenv for use with
# this project. a local virtualenv is created, the python
# dependencies are installed into it, and an environment
# variable is set to hint reticulate to use this virtualenv.

# this assumes that a python3 command is available on the path
# with a python version >= 3.6

# Sample usage:
# $source init_python_venv.sh

python3 -m venv venv
. venv/bin/activate

pip install -r requirements.txt

export RETICULATE_PYTHON=venv/bin/python

