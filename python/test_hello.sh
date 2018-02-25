source test_utilities.sh

echo "test_hello"

referenceCommand="clisp test_hello.lisp"
testCommand="python3 main.py test_hello.lisp"
assertEquals "$testCommand" "$referenceCommand"

