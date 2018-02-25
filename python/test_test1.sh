source test_utilities.sh

echo "test_test1"

referenceCommand="clisp test_test1.lisp"
testCommand="python3 main.py test_test1.lisp"
assertEquals "$testCommand" "$referenceCommand"

