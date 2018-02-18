source test_utilities.sh

echo "test_test1"

referenceOutput=`clisp test_test1.lisp`
testOutput=`python3 main.py test_test1.lisp`
assertEquals "$testOutput" "$referenceOutput"

