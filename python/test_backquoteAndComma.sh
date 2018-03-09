source test_utilities.sh

echo "test_backquoteAndComma"

referenceCommand="clisp test_backquoteAndComma.lisp"
testCommand="python3 main.py test_backquoteAndComma.lisp"
assertEquals "$testCommand" "$referenceCommand"

