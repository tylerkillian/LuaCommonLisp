source test_utilities.sh

echo "test_let"

referenceOutput=`clisp test_let.lisp`
testOutput=`python3 main.py test_let.lisp`
assertEquals "$testOutput" "$referenceOutput"

