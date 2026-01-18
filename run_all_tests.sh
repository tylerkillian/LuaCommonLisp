set -e

clisp run-all-tests.lisp

python3 run_all_tests.py
python3 cl.py run-all-tests.lisp
