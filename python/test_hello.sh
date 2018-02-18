source test_utilities.sh

echo "test_hello"

referenceOutput=`clisp test_hello.lisp`
testOutput=`python3 main.py test_hello.lisp`
assertEquals "$testOutput" "$referenceOutput"

