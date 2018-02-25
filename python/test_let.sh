source test_utilities.sh

echo "test_let"

referenceCommand="clisp test_let.lisp"
testCommand="python3 main.py test_let.lisp"
assertEquals "$testCommand" "$referenceCommand"

