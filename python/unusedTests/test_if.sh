source test_utilities.sh

echo "test_if"

referenceCommand="clisp test_if.lisp"
testCommand="python3 main.py test_if.lisp"
assertEquals "$testCommand" "$referenceCommand"

