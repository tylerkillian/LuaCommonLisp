source test_utilities.sh

echo "test_sum"

referenceOutput=`clisp test_sum.lisp`
testOutput=`python3 main.py test_sum.lisp`
assertEquals "$testOutput" "$referenceOutput"

