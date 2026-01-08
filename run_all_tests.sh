set -e

clisp run-all-tests.lisp

python3 cl.py run-all-tests.lisp
